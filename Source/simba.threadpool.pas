{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.threadpool;

{$i simba.inc}

interface

uses
  classes, sysutils, syncobjs;

type
  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

  TSimbaThreadPoolMethod = procedure(const Params: PParamArray; const Result: Pointer);
  TSimbaThreadPoolTask = record
    Method: TSimbaThreadPoolMethod;
    Params: TParamArray;
    Result: Pointer;

    class function Create(AMethod: TSimbaThreadPoolMethod; AParams: array of Pointer; AResult: Pointer): TSimbaThreadPoolTask; static;
  end;
  TSimbaThreadPoolTasks = array of TSimbaThreadPoolTask;

  TSimbaThreadPool_Thread = class(TThread)
  protected
    FEvent: TSimpleEvent;
    FIdleEvent: TSimpleEvent;
    FMethod: TSimbaThreadPoolMethod;
    FParams: TParamArray;
    FResult: Pointer;

    procedure Execute; override;

    procedure SetIdle(Value: Boolean);
    function GetIdle: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Run(Task: TSimbaThreadPoolTask);
    procedure WaitForIdle;

    property Idle: Boolean read GetIdle write SetIdle;
  end;

  TSimbaThreadPool = class
  protected
  type
    TThreadArray = array of TSimbaThreadPool_Thread;
  protected
    FThreads: TThreadArray;
    FLock: TCriticalSection;

    function GetIdleThreads(Count: Integer; out IdleThreads: TThreadArray): Boolean;
  public
    procedure RunParallel(Tasks: TSimbaThreadPoolTasks);

    constructor Create(ThreadCount: Int32);
    destructor Destroy; override;
  end;

var
  SimbaThreadPool: TSimbaThreadPool;

implementation

class function TSimbaThreadPoolTask.Create(AMethod: TSimbaThreadPoolMethod; AParams: array of Pointer; AResult: Pointer): TSimbaThreadPoolTask;
begin
  Result.Method := AMethod;
  Result.Params := AParams;
  Result.Result := AResult;
end;

procedure TSimbaThreadPool_Thread.Execute;
begin
  while True do
  begin
    FEvent.WaitFor(INFINITE);
    if Terminated then
      Exit;

    FMethod(@FParams, FResult);

    FEvent.ResetEvent();
    FIdleEvent.SetEvent();
  end;
end;

procedure TSimbaThreadPool_Thread.Run(Task: TSimbaThreadPoolTask);
begin
  FMethod := Task.Method;
  FParams := Task.Params;
  FResult := Task.Result;

  FEvent.SetEvent(); // begin execution
end;

procedure TSimbaThreadPool_Thread.WaitForIdle;
begin
  FIdleEvent.WaitFor(INFINITE);
end;

function TSimbaThreadPool_Thread.GetIdle: Boolean;
begin
  Result := FIdleEvent.WaitFor(0) = wrSignaled;
end;

procedure TSimbaThreadPool_Thread.SetIdle(Value: Boolean);
begin
  if Suspended then
    Start();

  if Value then
    FIdleEvent.SetEvent()
  else
    FIdleEvent.ResetEvent();
end;

constructor TSimbaThreadPool_Thread.Create;
begin
  inherited Create(True, 1024 * 512); // default = 4MiB, we set 512KiB

  FreeOnTerminate := False;

  FEvent := TSimpleEvent.Create();

  FIdleEvent := TSimpleEvent.Create();
  FIdleEvent.SetEvent();
end;

destructor TSimbaThreadPool_Thread.Destroy;
begin
  FEvent.SetEvent(); // call event so execute loop can execute.

  if (not Suspended) then
  begin
    Terminate();
    WaitFor();
  end;

  FEvent.Free();
  FIdleEvent.Free();

  inherited Destroy();
end;

function TSimbaThreadPool.GetIdleThreads(Count: Integer; out IdleThreads: TThreadArray): Boolean;
var
  I, J: Integer;
begin
  FLock.Enter();

  try
    for I := 0 to High(FThreads) do
      if FThreads[i].Idle then
      begin
        IdleThreads := IdleThreads + [FThreads[I]];

        if (Length(IdleThreads) = Count) then
        begin
          for J := 0 to High(IdleThreads) do
            IdleThreads[J].Idle := False;

          Result := True;
          Exit;
        end;
      end;
  finally
    FLock.Leave();
  end;

  Result := False;
end;

constructor TSimbaThreadPool.Create(ThreadCount: Int32);
var
  I: Integer;
begin
  inherited Create();

  FLock := TCriticalSection.Create();

  SetLength(FThreads, ThreadCount);
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

  if (FLock <> nil) then
    FreeAndNil(FLock);

  inherited Destroy();
end;

procedure TSimbaThreadPool.RunParallel(Tasks: TSimbaThreadPoolTasks);
var
  Threads: TThreadArray;
  I: Integer;
begin
  if GetIdleThreads(Length(Tasks), Threads) then
  begin
    for I := 0 to High(Tasks) do
      Threads[I].Run(Tasks[I]);

    for I := 0 to High(Threads) do
      Threads[I].WaitForIdle();
  end else
  begin
    // Not enough threads - no multithreading.
    for I := 0 to High(Tasks) do
      Tasks[I].Method(@Tasks[I].Params, Tasks[I].Result);
  end;
end;

initialization
  if (TThread.ProcessorCount >= 4) then
    SimbaThreadPool := TSimbaThreadPool.Create(4)
  else
    SimbaThreadPool := TSimbaThreadPool.Create(TThread.ProcessorCount);

finalization
  if (SimbaThreadPool <> nil) then
    FreeAndNil(SimbaThreadPool);

end.

