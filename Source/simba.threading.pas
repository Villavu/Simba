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
  TThreadPoolMethod = procedure(const Index, Lo, Hi: Integer) is nested;

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
    function WaitLocked(Timeout: Integer): Boolean; overload; // Returns True if unlocked!

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

  TSimbaThreadPool = class
  protected
  type
    TPoolThread = class(TThread)
    protected
      procedure Execute; override;
    public
      IdleLock: TWaitableLock; // Locked = Thread is being used right now.
      MethodLock: TWaitableLock; // Locked = Waiting for a method to call

      Index: Integer;
      Lo: Integer;
      Hi: Integer;

      Method: TThreadPoolMethod;

      constructor Create; reintroduce;
      destructor Destroy; override;
    end;
    TPoolThreadArray = array of TPoolThread;
  protected
    FThreadCount: Integer;
    FThreads: TPoolThreadArray;
    FLock: TEnterableLock;

    function GetIdleThreads(MaxThreads: Integer): TPoolThreadArray;
  public
    constructor Create(AThreadCount: Integer);
    destructor Destroy; override;

    property ThreadCount: Integer read FThreadCount;

    function RunParallel(MaxThreads: Integer; Lo, Hi: Integer; Method: TThreadPoolMethod): Integer;
  end;

var
  SimbaThreadPool: TSimbaThreadPool = nil;

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
begin
  try
    if Assigned(Method)     then Method()      else
    if Assigned(Proc)       then Proc()        else
    if Assigned(NestedProc) then NestedProc();
  except
    on E: Exception do
      DebugLn('RunOnMainThread exception: ' + E.Message);
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

function TSimbaThreadPool.GetIdleThreads(MaxThreads: Integer): TPoolThreadArray;
var
  I, Count: Integer;
begin
  SetLength(Result, MaxThreads);
  Count := 0;

  FLock.Enter();
  try
    for I := 0 to High(FThreads) do
    begin
      if FThreads[I].IdleLock.IsLocked() then
        Continue;

      Result[Count] := FThreads[I];
      Result[Count].IdleLock.Lock();
      Inc(Count);

      if (Count = MaxThreads) then
        Break;
    end;
  finally
    FLock.Leave();
  end;

  SetLength(Result, Count);
end;

constructor TSimbaThreadPool.Create(AThreadCount: Integer);
var
  I: Integer;
begin
  inherited Create();

  FThreadCount := AThreadCount;
  SetLength(FThreads, FThreadCount);
  for I := 0 to High(FThreads) do
    FThreads[I] := TPoolThread.Create();
end;

destructor TSimbaThreadPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FThreads) do
    FThreads[I].Free();
  FThreads := nil;

  inherited Destroy();
end;

function TSimbaThreadPool.RunParallel(MaxThreads: Integer; Lo, Hi: Integer; Method: TThreadPoolMethod): Integer;
var
  Threads: TPoolThreadArray;
  I, Size: Integer;
begin
  if (MaxThreads > 1) then
    Threads := GetIdleThreads(Min(FThreadCount, MaxThreads))
  else
    Threads := [];

  Result := Max(1, Length(Threads));

  if (Length(Threads) > 1) then
  begin
    Size := ((Hi - Lo) + 1) div Result;

    for I := 0 to High(Threads) do
    begin
      Threads[I].Index := I;
      Threads[I].Method := Method;

      if (I = 0) then
      begin
        Threads[I].Lo := 0;
        Threads[I].Hi := Size;
      end else
      begin
        Threads[I].Lo := Threads[I-1].Hi + 1;
        Threads[I].Hi := Threads[I-1].Hi + Size;
      end;

      if (I = High(Threads)) then
        Threads[I].Hi := Hi;

      Threads[I].MethodLock.Unlock();
      if Threads[I].Suspended then
        Threads[I].Start();
    end;

    for I := 0 to High(Threads) do
      Threads[I].IdleLock.WaitLocked();
  end else
    Method(0, Lo, Hi);
end;

procedure TSimbaThreadPool.TPoolThread.Execute;
begin
  while True do
  begin
    MethodLock.WaitLocked();
    if Terminated then
      Break;

    if Assigned(Method) then
    try
      Method(Index, Lo, Hi);
    except
      on E: Exception do
      begin
        DebugLn('[SimbaThreadPool]: Exception occurred while executing a method: ' + E.Message);
        {$IFDEF SIMBA_HAS_DEBUGINFO}
        DumpExceptionBacktrace(Output);
        {$ENDIF}
      end;
    end;

    Method := nil;
    MethodLock.Lock();
    IdleLock.Unlock();
  end;
end;

constructor TSimbaThreadPool.TPoolThread.Create;
begin
  inherited Create(True, 512 * 512); // default = 4MiB, we set 256KiB
                                     // also start suspended until we need it.

  MethodLock.Lock();
end;

destructor TSimbaThreadPool.TPoolThread.Destroy;
begin
  IdleLock.WaitLocked(); // Wait if running something
  MethodLock.Unlock();   // Wake `Execute` loop if not running

  if (not Suspended) then
  begin
    Terminate();
    WaitFor();
  end;

  inherited Destroy();
end;

initialization
  SimbaCPUInfo.ThreadCount    := TNumCPULib.GetLogicalCPUCount();
  SimbaCPUInfo.CoreCount      := TNumCPULib.GetPhysicalCPUCount();
  SimbaCPUInfo.PhysicalMemory := TNumCPULib.GetTotalPhysicalMemory();

  SimbaThreadPool := TSimbaThreadPool.Create(SimbaCPUInfo.CoreCount);

finalization
  FreeAndNil(SimbaThreadPool);

end.

