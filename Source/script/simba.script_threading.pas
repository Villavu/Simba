{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils, syncobjs,
  lptypes, lpmessages, lpvartypes, lpinterpreter,
  simba.base, simba.baseclass, simba.threading;

type
  TPointerArray = array of Pointer;

  TSimbaLock = class(TSimbaBaseClass)
  protected
    FCriticalSection: TCriticalSection;
  public
    constructor Create(AFreeOnTerminate: Boolean = False); reintroduce;
    destructor Destroy; override;

    function TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
  end;

  TSimbaThread = class(TSimbaBaseThread)
  protected
    FCodeRunner: TLapeCodeRunner;
    FMethod: TMethod;
    FTerminateMethod: TMethod;

    procedure Invoke(Method: TMethod; Params: array of Pointer);

    procedure DoMethod; virtual;
    procedure DoTerminateMethod; virtual;

    procedure DoTerminate; override;
    procedure Execute; override;

    function GetName: String;
    procedure SetName(Value: String);
  public
    constructor Create(Emitter: TLapeCodeEmitter; Method, TerminateMethod: TMethod); reintroduce;
    constructor Create(Emitter: TLapeCodeEmitter; Method: TMethod); reintroduce;
    destructor Destroy; override;

    function WaitForTerminate(Timeout: Int32): Boolean;

    property Name: String read GetName write SetName;
    property Terminated;
  end;

  TSimbaThreadEx = class(TSimbaThread)
  protected
    FParams: TPointerArray;

    function getParamsAsParam: Pointer;

    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  public
    property Params: TPointerArray read FParams write FParams;
  end;

  TSimbaThreadSchedule = class(TSimbaThread)
  protected
    FInterval: Integer;
    FTerminateLock: TWaitableLock;

    procedure TerminatedSet; override;

    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  public
    property Interval: Integer read FInterval write FInterval;
  end;

  TSimbaThreadScheduleEx = class(TSimbaThread)
  protected
    FInterval: Integer;
    FTerminateLock: TWaitableLock;
    FParams: TPointerArray;

    function getParamsAsParam: Pointer;
    procedure TerminatedSet; override;
    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  public
    property Interval: Integer read FInterval write FInterval;
    property Params: TPointerArray read FParams write FParams;
  end;

implementation

constructor TSimbaLock.Create(AFreeOnTerminate: Boolean);
begin
  inherited Create();

  FCriticalSection := TCriticalSection.Create();
  FFreeOnTerminate := AFreeOnTerminate;
end;

destructor TSimbaLock.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy();
end;

function TSimbaLock.TryEnter: Boolean;
begin
  Result := FCriticalSection.TryEnter();
end;

procedure TSimbaLock.Enter;
begin
  FCriticalSection.Enter();
end;

procedure TSimbaLock.Leave;
begin
  FCriticalSection.Leave();
end;

function TSimbaThreadScheduleEx.getParamsAsParam: Pointer;
begin
  Result := Pointer(FParams);
  // inc ref count
  Inc(PSizeInt(Result - SizeOf(SizeInt) * 2)^);
end;

procedure TSimbaThreadScheduleEx.TerminatedSet;
begin
  inherited TerminatedSet;

  FTerminateLock.Unlock();
end;

procedure TSimbaThreadScheduleEx.DoMethod;
begin
  FTerminateLock.Lock();

  while not Terminated do
  begin
    Invoke(FMethod, [getParamsAsParam()]);

    FTerminateLock.WaitLocked(FInterval);
  end;
end;

procedure TSimbaThreadScheduleEx.DoTerminateMethod;
begin
  { nothing }
end;

procedure TSimbaThreadSchedule.TerminatedSet;
begin
  inherited TerminatedSet();

  FTerminateLock.Unlock();
end;

procedure TSimbaThreadSchedule.DoMethod;
begin
  FTerminateLock.Lock();

  while not Terminated do
  begin
    Invoke(FMethod, []);

    FTerminateLock.WaitLocked(FInterval);
  end;
end;

procedure TSimbaThreadSchedule.DoTerminateMethod;
begin
  { nothing }
end;

function TSimbaThreadEx.getParamsAsParam: Pointer;
begin
  Result := Pointer(FParams);
  // inc ref count
  Inc(PSizeInt(Result - SizeOf(SizeInt) * 2)^);
end;

procedure TSimbaThreadEx.DoMethod;
begin
  Invoke(FMethod, [getParamsAsParam()]);
end;

procedure TSimbaThreadEx.DoTerminateMethod;
begin
  Invoke(FTerminateMethod, [Self, getParamsAsParam()]);
end;

procedure TSimbaThread.Invoke(Method: TMethod; Params: array of Pointer);
var
  VarStack: TByteArray = nil;
  I: Integer;
begin
  SetLength(VarStack, SizeOf(Pointer) + (Length(Params) * SizeOf(Pointer)));
  PPointer(@VarStack[0])^ := Method.Data;
  for I := 0 to High(Params) do
    PPointer(@VarStack[SizeOf(Pointer) * (I + 1)])^ := Params[I];

  FCodeRunner.Run(TCodePos(Method.Code), VarStack);
end;

procedure TSimbaThread.DoMethod;
begin
  Invoke(FMethod, []);
end;

procedure TSimbaThread.DoTerminateMethod;
begin
  Invoke(FTerminateMethod, [Self]);
end;

procedure TSimbaThread.DoTerminate;
begin
  if Assigned(FTerminateMethod.Code) then
    DoTerminateMethod();
end;

procedure TSimbaThread.Execute;
begin
  if Assigned(FMethod.Code) then
    DoMethod();
end;

function TSimbaThread.GetName: String;
begin
  Result := FName;
end;

procedure TSimbaThread.SetName(Value: String);
begin
  FName := Value;
  NameThreadForDebugging(FName, ThreadID);
end;

constructor TSimbaThread.Create(Emitter: TLapeCodeEmitter; Method, TerminateMethod: TMethod);
begin
  inherited Create();

  FCodeRunner := TLapeCodeRunner.Create(Emitter);
  FMethod := Method;
  FTerminateMethod := TerminateMethod;
end;

constructor TSimbaThread.Create(Emitter: TLapeCodeEmitter; Method: TMethod);
begin
  Create(Emitter, Method, Default(TMethod));
end;

destructor TSimbaThread.Destroy;
begin
  FreeAndNil(FCodeRunner);

  inherited Destroy();
end;

function TSimbaThread.WaitForTerminate(Timeout: Int32): Boolean;
begin
  Result := WaitForThreadTerminate(ThreadID, Timeout) = 0;
end;

end.

