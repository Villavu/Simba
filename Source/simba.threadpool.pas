{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.threadpool;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.baseclass, simba.mufasatypes, simba.simplelock;

type
  TSimbaThreadPoolMethod_Nested = procedure(const Index, Lo, Hi: Integer) is nested;

  TSimbaThreadPool_Thread = class(TThread)
  protected
    procedure Execute; override;
  public
    IdleLock: TSimpleWaitableLock; // Locked = Thread is being used right now.
    MethodLock: TSimpleWaitableLock; // Locked = Waiting for a method to call

    Index: Integer;
    Lo: Integer;
    Hi: Integer;

    Method: TSimbaThreadPoolMethod_Nested;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TSimbaThreadPool = class
  protected
  type
    TThreadArray = array of TSimbaThreadPool_Thread;
  protected
    FThreadCount: Integer;
    FThreads: TThreadArray;
    FLock: TSimpleEnterableLock;

    function GetIdleThreads(MaxThreads: Integer): TThreadArray;
  public
    constructor Create(AThreadCount: Integer);
    destructor Destroy; override;

    property ThreadCount: Integer read FThreadCount;

    function RunParallel(MaxThreads: Integer; Lo, Hi: Integer; Method: TSimbaThreadPoolMethod_Nested): Integer;
  end;

var
  SimbaThreadPool: TSimbaThreadPool;

implementation

procedure TSimbaThreadPool_Thread.Execute;
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
        DebugLn('Exception whilst invoking method in thread pool: ' + E.Message);
    end;

    Method := nil;
    MethodLock.Lock();
    IdleLock.Unlock();
  end;
end;

constructor TSimbaThreadPool_Thread.Create;
begin
  inherited Create(True, 512 * 512); // default = 4MiB, we set 256KiB
                                     // also start suspended until we need it.

  MethodLock.Lock();
end;

destructor TSimbaThreadPool_Thread.Destroy;
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

function TSimbaThreadPool.GetIdleThreads(MaxThreads: Integer): TThreadArray;
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
    FThreads[I] := TSimbaThreadPool_Thread.Create();
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

function TSimbaThreadPool.RunParallel(MaxThreads: Integer; Lo, Hi: Integer; Method: TSimbaThreadPoolMethod_Nested): Integer;
var
  Threads: TThreadArray;
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

initialization
  SimbaThreadPool := TSimbaThreadPool.Create(TThread.ProcessorCount);

finalization
  if (SimbaThreadPool <> nil) then
    FreeAndNil(SimbaThreadPool);

end.

