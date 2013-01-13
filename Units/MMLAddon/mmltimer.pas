unit mmltimer;
//cross-platform threaded timer
//by Cynic
{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TMMLTimer = class; // forward declaration


  TTimerThread = class(TThread)
  private
    FTimer: TMMLTimer;
  protected
    procedure   DoExecute;
    procedure   Execute; override;
  public
    constructor CreateTimerThread(Timer: TMMLTimer);
  end;


  { TMMLTimer }

  TMMLTimer = class(TObject)
  private
    FInterval: Integer;
    FPriority: TThreadPriority;
    FOnTimer: TNotifyEvent;
    FContinue: Boolean;
    FRunning: Boolean;
    FEnabled: Boolean;
    procedure   SetEnabled(Value: Boolean );
    function GetInterval: integer;
    procedure SetInterval(Value: integer);
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
  protected
    procedure   StartTimer;
    procedure   StopTimer;
    property    Continue: Boolean read FContinue write FContinue;
  public
    constructor Create;
    destructor  Destroy;
    procedure   On;
    procedure   Off;
  published
    property    Enabled: Boolean read FEnabled write SetEnabled;
    property    Interval: Integer read FInterval write FInterval;
    property    ThreadPriority: TThreadPriority read FPriority write FPriority default tpNormal;
    property    OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;


implementation

uses
  SysUtils;

function _GetTickCount: Cardinal;
begin
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;


{ TTimerThread }

constructor TTimerThread.CreateTimerThread(Timer: TMMLTimer);
begin
  inherited Create(True);
  FTimer := Timer;
  FreeOnTerminate := True;
end;

procedure TTimerThread.Execute;
var
  SleepTime: Integer;
  Last: Cardinal;
begin
  while FTimer.Continue do
  begin
    Last := _GetTickCount;
    Synchronize(@DoExecute);
    SleepTime := FTimer.FInterval - (_GetTickCount - Last);
    if SleepTime < 10 then
      SleepTime := 10;
    Sleep(SleepTime);
  end;
end;

procedure TTimerThread.DoExecute;
begin
  if Assigned(FTimer.OnTimer) then FTimer.OnTimer(FTimer);
end;


{ TMMLTimer }

constructor TMMLTimer.Create;
begin
  FPriority := tpNormal;
  FInterval:=1000;
end;

destructor TMMLTimer.Destroy;
begin
  StopTimer;
end;

procedure TMMLTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
      StartTimer
    else
      StopTimer;
  end;
end;

function TMMLTimer.GetInterval: integer;
begin
  Result := Self.FInterval;
end;

procedure TMMLTimer.SetInterval(Value: integer);
var
  tmp: Boolean;
begin
  if(Value <> FInterval) then
  begin
    tmp := FEnabled;
    Enabled := False;
    FInterval := Value;
    Enabled := tmp;
  end;
end;

function TMMLTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FPriority;
end;

procedure TMMLTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FPriority:=value;
end;

procedure TMMLTimer.StartTimer;
begin
  if FRunning then
    Exit; //==>
  FContinue := True;
    with TTimerThread.CreateTimerThread(Self) do
    begin
      Priority := FPriority;
      Resume;
    end;
  FRunning := True;
end;

procedure TMMLTimer.StopTimer;
begin
  FContinue := False;
  FRunning  := False;
end;

procedure TMMLTimer.On;
begin
  StartTimer;
end;

procedure TMMLTimer.Off;
begin
  StopTimer;
end;


end.

