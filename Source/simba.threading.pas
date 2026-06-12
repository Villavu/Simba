{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, syncobjs,
  simba.base;

type
  TThreadProc = procedure;
  TThreadNestedProc = procedure is nested;

  function IsMainThread: Boolean;
  procedure CheckMainThread(const Method: String);

  procedure QueueOnMainThread(Proc: TThreadProc); overload;
  procedure QueueOnMainThread(Method: TThreadMethod); overload;

  procedure RunInMainThread(Method: TThreadMethod); overload;
  procedure RunInMainThread(NestedMethod: TThreadNestedProc); overload;

  function RunInThread(Proc: TThreadProc; FreeOnTerminate: Boolean = False): TThread; overload;
  function RunInThread(Method: TThreadMethod; FreeOnTerminate: Boolean = False): TThread; overload;

type
  TWaitableLock = record
  private
    FLock: TSimpleEvent;
  public
    procedure Lock;
    procedure Unlock;
    procedure WaitLocked; overload;
    function WaitLocked(Timeout: Integer): Boolean; overload; // returns True if unlocked!

    function IsLocked: Boolean;

    class operator Initialize(var Self: TWaitableLock);
    class operator Finalize(var Self: TWaitableLock);
  end;

  TEnterableLock = record
  private
    FLock: TCriticalSection;
  public
    function TryEnter: Boolean; inline;
    procedure Enter; inline;
    procedure Leave; inline;

    class operator Initialize(var Self: TEnterableLock);
    class operator Finalize(var Self: TEnterableLock);
  end;

  TLimit = record
  private
    FCount: Integer;
    FLimit: Integer;
  public
    procedure Inc; inline;
    function Reached: Boolean; inline;
    property Count: Integer read FCount;

    class function Create(Limit: Integer): TLimit; static;
  end;

  // Thread is idle until Wake() is called which then invokes FMethod
  // and then returns to idleness.
  TIdleThread = class(TThread)
  protected
    FLock: TWaitableLock;
    FMethod: TThreadMethod;

    procedure Execute; override;
    function GetIsIdle: Boolean;
  public
    constructor Create(Method: TThreadMethod); reintroduce;
    procedure Wake;
    property IsIdle: Boolean read GetIsIdle;
  end;

var
  SimbaCPUInfo: record
    CoreCount: Integer;
    ThreadCount: Integer;
    PhysicalMemory: Integer;
  end;

implementation

uses
  NumCPULib;

procedure TLimit.Inc;
begin
  InterlockedIncrement(FCount);
end;

function TLimit.Reached: Boolean;
begin
  Result := (FLimit > 0) and (InterlockedCompareExchange(FCount, FLimit, FLimit) >= FLimit);
end;

class function TLimit.Create(Limit: Integer): TLimit;
begin
  Result.FCount := 0;
  Result.FLimit := Limit;
end;

function TEnterableLock.TryEnter: Boolean;
begin
  Result := FLock.TryEnter();
end;

procedure TEnterableLock.Enter;
begin
  FLock.Enter();
end;

procedure TEnterableLock.Leave;
begin
  FLock.Leave();
end;

class operator TEnterableLock.Initialize(var Self: TEnterableLock);
begin
  Self.FLock := TCriticalSection.Create();
end;

class operator TEnterableLock.Finalize(var Self: TEnterableLock);
begin
  if (Self.FLock <> nil) then
    FreeAndNil(Self.FLock);
end;

procedure TWaitableLock.Lock;
begin
  FLock.ResetEvent();
end;

procedure TWaitableLock.Unlock;
begin
  FLock.SetEvent();
end;

procedure TWaitableLock.WaitLocked;
begin
  FLock.WaitFor(INFINITE);
end;

function TWaitableLock.WaitLocked(Timeout: Integer): Boolean;
begin
  Result := FLock.WaitFor(Timeout) = wrSignaled;
end;

function TWaitableLock.IsLocked: Boolean;
begin
  Result := FLock.WaitFor(0) = wrTimeout;
end;

class operator TWaitableLock.Initialize(var Self: TWaitableLock);
begin
  Self.FLock := TSimpleEvent.Create();
  Self.FLock.SetEvent();
end;

class operator TWaitableLock.Finalize(var Self: TWaitableLock);
begin
  FreeAndNil(Self.FLock);
end;

function IsMainThread: Boolean;
begin
  Result := GetCurrentThreadID() = MainThreadID;
end;

procedure CheckMainThread(const Method: String);
begin
  if (not IsMainThread()) then
    SimbaException('Not called on main thread: ' + Method);
end;

type
  TSyncObject = object
    Proc: TThreadProc;
    NestedProc: TThreadNestedProc;
    Method: TThreadMethod;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
var
  I: Integer;
  Frames: PPointer;
begin
  try
    if Assigned(Method)     then Method()      else
    if Assigned(Proc)       then Proc()        else
    if Assigned(NestedProc) then NestedProc();
  except
    on E: Exception do
    begin
      DebugLn('RunOnMainThread exception: ' + E.Message);

      {$IFDEF SIMBA_HAS_DEBUGINFO}
      DebugLn(BackTraceStrFunc(ExceptAddr));
      Frames := ExceptFrames;
      for I := 0 to ExceptFrameCount - 1 do
        DebugLn(BackTraceStrFunc(Frames[I]));
      {$ENDIF}
    end;
  end;
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TThreadProc;
    FMethod: TThreadMethod;

    procedure Execute; override;
  public
    constructor Create(Proc: TThreadProc; AFreeOnTerminate: Boolean); reintroduce;
    constructor Create(Method: TThreadMethod; AFreeOnTerminate: Boolean); reintroduce;
  end;

procedure TThreaded.Execute;
begin
  try
    if Assigned(FMethod)     then FMethod() else
    if Assigned(FProc)       then FProc();
  except
    on E: Exception do
      DebugLn('RunInThread exception: ' + E.Message);
  end;
end;

constructor TThreaded.Create(Proc: TThreadProc; AFreeOnTerminate: Boolean);
begin
  inherited Create(False, DefaultStackSize div 2);

  FProc := Proc;

  FreeOnTerminate := AFreeOnTerminate;
end;

constructor TThreaded.Create(Method: TThreadMethod; AFreeOnTerminate: Boolean);
begin
  inherited Create(False, DefaultStackSize div 2);

  FMethod := Method;
  FreeOnTerminate := AFreeOnTerminate;
end;

type
  TQueueObject = class
  public
    Proc: TThreadProc;
    Method: TThreadMethod;

    procedure Execute(Data: PtrInt);
  end;

procedure TQueueObject.Execute(Data: PtrInt);
begin
  try
    if Assigned(Method) then Method() else
    if Assigned(Proc)   then Proc();
  except
    on E: Exception do
      DebugLn('QueueOnMainThread exception: ' + E.Message);
  end;

  Free();
end;

function RunInThread(Proc: TThreadProc; FreeOnTerminate: Boolean): TThread;
begin
  Result := TThreaded.Create(Proc, FreeOnTerminate);
end;

function RunInThread(Method: TThreadMethod; FreeOnTerminate: Boolean): TThread;
begin
  Result := TThreaded.Create(Method, FreeOnTerminate);
end;

procedure RunInMainThread(Method: TThreadMethod);
var
  {%H-}SyncObject: TSyncObject;
begin
  if (not IsMainThread()) then
  begin
    SyncObject := Default(TSyncObject);
    SyncObject.Method := Method;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

procedure RunInMainThread(NestedMethod: TThreadNestedProc);
var
  {%H-}SyncObject: TSyncObject;
begin
  if (not IsMainThread()) then
  begin
    SyncObject := Default(TSyncObject);
    SyncObject.NestedProc := NestedMethod;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    NestedMethod();
end;

procedure QueueOnMainThread(Proc: TThreadProc);
var
  Queue: TQueueObject;
begin
  Queue := TQueueObject.Create();
  Queue.Proc := Proc;

  Application.QueueAsyncCall(@Queue.Execute, 0);
end;

procedure QueueOnMainThread(Method: TThreadMethod);
var
  Queue: TQueueObject;
begin
  Queue := TQueueObject.Create();
  Queue.Method := Method;

  Application.QueueAsyncCall(@Queue.Execute, 0);
end;

procedure TIdleThread.Execute;
begin
  while (not Terminated) do
  begin
    if FLock.WaitLocked(1000) then
    try
      FMethod();
    finally
      FLock.Lock();
    end;
  end;
end;

function TIdleThread.GetIsIdle: Boolean;
begin
  Result := FLock.IsLocked;
end;

constructor TIdleThread.Create(Method: TThreadMethod);
begin
  inherited Create(True, 512*512);

  FMethod := Method;
  FLock.Lock(); // default to idle
end;

procedure TIdleThread.Wake;
begin
  if Suspended then
    Start();
  FLock.Unlock();
end;

initialization
  SimbaCPUInfo.ThreadCount    := TNumCPULib.GetLogicalCPUCount();
  SimbaCPUInfo.CoreCount      := TNumCPULib.GetPhysicalCPUCount();
  SimbaCPUInfo.PhysicalMemory := TNumCPULib.GetTotalPhysicalMemory();

end.

