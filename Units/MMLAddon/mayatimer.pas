unit MayaTimer;

interface

uses
  Windows, Classes;

const
  DEFAULT_INTERVAL = 300000;

type
  TMayaTimer = class;

  TTimerThread = class(TThread)
  private
    FOwner: TMayaTimer;
    FInterval: Cardinal;
    FStop: THandle;
  protected
    procedure Execute; override;
  end;

  TMayaTimer = class(TComponent)
  private
    FOnTimer: TNotifyEvent;
    FTimerThread: TTimerThread;
    FEnabled: Boolean;

    procedure DoTimer;

    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read GetInterval write SetInterval default DEFAULT_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property ThreadPriority: TThreadPriority read GetThreadPriority write SetThreadPriority default tpNormal;
  end;

implementation

{ TTimerThread }

procedure TTimerThread.Execute;
begin
  repeat
    if((WaitForSingleObject(FStop, FInterval) = WAIT_TIMEOUT)) then
      Synchronize(@FOwner.DoTimer);
  Until(Terminated);
end;

{ TMayaTimer }

constructor TMayaTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimerThread := TTimerThread.Create(True);
  with FTimerThread do
  begin
    FOwner := Self;
    FEnabled := False;
    FInterval := DEFAULT_INTERVAL;
    Priority := tpNormal;
	
    FStop := CreateEvent(nil, False, False, nil);
  end;
end;

destructor TMayaTimer.Destroy;
begin
  with FTimerThread do
  begin
    FEnabled := False;

    Terminate;

    SetEvent(FStop);
    if(Suspended) then
      Suspended := False;
    WaitFor;
    CloseHandle(FStop);  
    Free;
  end;

  inherited Destroy;
end;

procedure TMayaTimer.DoTimer;
begin
  if((FEnabled) and (Assigned(FOnTimer)) and (not(csDestroying in ComponentState))) then
  begin
    try
      FOnTimer(Self);
    except
    end;
  end;
end;

procedure TMayaTimer.SetEnabled(Value: Boolean);
begin
  if(Value <> FEnabled) then
  begin
    FEnabled := Value;

    if(FEnabled) then
    begin
      SetEvent(FTimerThread.FStop);
      FTimerThread.Suspended := False;
    end
    else
      FTimerThread.Suspended := True;
  end;
end;

function TMayaTimer.GetInterval: Cardinal;
begin
  Result := FTimerThread.FInterval;
end;

procedure TMayaTimer.SetInterval(Value: Cardinal);
var
  PrevEnabled: Boolean;
begin
  if(Value <> FTimerThread.FInterval) then
  begin
    PrevEnabled := FEnabled;
    Enabled := False;
    FTimerThread.FInterval := Value;
    Enabled := PrevEnabled;
  end;
end;

function TMayaTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FTimerThread.Priority;
end;

procedure TMayaTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FTimerThread.Priority := Value;
end;

end.
