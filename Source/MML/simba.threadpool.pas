unit simba.threadpool;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, syncobjs;

type
  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

  TSimbaThreadPool_Method = procedure(Params: PParamArray; Lo, Hi: Int32);

  TSimbaThreadPool_Thread = class;
  TSimbaThreadPool_ThreadArray = array of TSimbaThreadPool_Thread;

  TSimbaThreadPool_Thread = class(TThread)
  protected
    FEvent: TSimpleEvent;
    FMethod: TSimbaThreadPool_Method;
    FParameters: TParamArray;
    FTemporary: Boolean;
    FLo, FHi: Int32;
    FFinished: TSimpleEvent;

    procedure Execute; override;
  public
    property Temporary: Boolean read FTemporary;

    procedure Run(Method: TSimbaThreadPool_Method; Parameters: array of Pointer; Lo, Hi: Int32);
    procedure Terminate;

    constructor Create(IsTemporary: Boolean); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaThreadPool = class
  protected
    FThreads: TSimbaThreadPool_ThreadArray;
    FLock: TCriticalSection;
  public
    function GetAvailableThread: TSimbaThreadPool_Thread;

    procedure RunParallel(Method: TSimbaThreadPool_Method; Args: array of Pointer; Lo, Hi: Int32; Fallback: Boolean; ThreadCount: Int32 = -1);

    constructor Create(ThreadCount: Int32);
    destructor Destroy; override;
  end;

var
  SimbaThreadPool: TSimbaThreadPool;

implementation

uses
  math;

procedure TSimbaThreadPool_Thread.Execute;
begin
  while (not Terminated) do
  begin
    FEvent.WaitFor(INFINITE);

    if (FMethod <> nil) then
    begin
      FMethod(@FParameters, FLo, FHi);
      FMethod := nil;

      if FTemporary then
        Terminate();
    end;

    FEvent.ResetEvent();
    FFinished.SetEvent();
  end;
end;

procedure TSimbaThreadPool_Thread.Run(Method: TSimbaThreadPool_Method; Parameters: array of Pointer; Lo, Hi: Int32);
var
  i: Int32;
begin
  FLo := Lo;
  FHi := Hi;
  FMethod := Method;
  for i := 0 to High(Parameters) do
    FParameters[i] := Parameters[i];

  FEvent.SetEvent(); // begin execution
end;

procedure TSimbaThreadPool_Thread.Terminate;
begin
  inherited Terminate();

  FEvent.SetEvent(); // call event so execute loop can execute.
end;

constructor TSimbaThreadPool_Thread.Create(IsTemporary: Boolean);
begin
  FTemporary := IsTemporary;
  FEvent := TSimpleEvent.Create();
  FFinished := TSimpleEvent.Create();
  FFinished.SetEvent();

  inherited Create(False, 1024 * 512); // default = 4MiB, we set 512KiB
end;

destructor TSimbaThreadPool_Thread.Destroy;
begin
  FEvent.Free();
  FFinished.Free();

  inherited Destroy;
end;

function TSimbaThreadPool.GetAvailableThread: TSimbaThreadPool_Thread;
var
  i: Int32;
begin
  FLock.Enter();

  try
    for i := 0 to High(FThreads) do
      if (FThreads[i].FFinished.WaitFor(0) = wrSignaled) then
      begin
        Result := FThreads[i];
        Result.FFinished.ResetEvent();

        Exit;
      end;

    Result := TSimbaThreadPool_Thread.Create(True);
  finally
    FLock.Leave();
  end;
end;

constructor TSimbaThreadPool.Create(ThreadCount: Int32);
var
  i: Int32;
begin
  inherited Create();

  FLock := TCriticalSection.Create();

  SetLength(FThreads, ThreadCount);
  for i := 0 to High(FThreads) do
    FThreads[i] := TSimbaThreadPool_Thread.Create(False);
end;

destructor TSimbaThreadPool.Destroy;
var
  i: Int32;
begin
  FLock.Enter();

  try
    for i := 0 to High(FThreads) do
    begin
      FThreads[i].Terminate();
      FThreads[i].WaitFor();
      FThreads[i].Free();
    end;
  finally
    FLock.Leave();
  end;

  FLock.Free();

  inherited Destroy();
end;

procedure TSimbaThreadPool.RunParallel(Method: TSimbaThreadPool_Method; Args: array of Pointer; Lo, Hi: Int32; Fallback: Boolean; ThreadCount: Int32);
var
  i, Step, A, B: Int32;
  Threads: TSimbaThreadPool_ThreadArray;
begin
  if Fallback or (ThreadCount = 1) then
  begin
    Method(@Args, Lo, Hi);
    Exit;
  end;

  if (ThreadCount = -1) or (ThreadCount > Length(FThreads)) then
    ThreadCount := Length(FThreads);

  A := Lo;
  B := Lo;
  Step := Max(1, (Hi + 1) div ThreadCount);

  while (B <> Hi) do
  begin
    B := Min(Hi, A + Step);

    SetLength(Threads, Length(Threads) + 1);

    Threads[High(Threads)] := Self.GetAvailableThread();
    Threads[High(Threads)].Run(Method, Args, A, B);

    A := B + 1;
  end;

  for i := 0 to High(Threads) do
  begin
    Threads[i].FFinished.WaitFor(INFINITE);
    if Threads[i].Temporary then
      Threads[i].Free();
  end;
end;

initialization
  if TThread.ProcessorCount < 4 then
    SimbaThreadPool := TSimbaThreadPool.Create(TThread.ProcessorCount)
  else
    SimbaThreadPool := TSimbaThreadPool.Create(4);

finalization
  SimbaThreadPool.Free();

end.

