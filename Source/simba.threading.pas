unit simba.threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms,
  simba.mufasatypes;

type
  TProcedureOfObject      = procedure of object;
  TProcedureOfObjectArray = array of TProcedureOfObject;

  TProcedureNested   = procedure is nested;
  TProcedureNestedEx = procedure(Params: TVariantArray) is nested;

procedure ExecuteOnMainThread(Method: TProcedureOfObject); overload;
procedure ExecuteOnMainThread(Method: TProcedureNested); overload;

procedure QueueOnMainThread(Proc: TProcedureOfObject); overload;
procedure QueueOnMainThread(Proc: TProcedureNested); overload;
procedure QueueOnMainThread(Proc: TProcedureNestedEx; Params: TVariantArray); overload;

function Threaded(Method: TProcedureNested): TThread; overload;
function Threaded(Method: TProcedureOfObject): TThread; overload;
procedure Threaded(Methods: TProcedureOfObjectArray; Interval: Integer = 0); overload;

procedure ThreadedAndForget(Method: TProcedureNested);

implementation

type
  TSyncObject = object
    Proc: TProcedureOfObject;
    NestedProc: TProcedureNested;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
begin
  try
    if Assigned(Proc)       then Proc();
    if Assigned(NestedProc) then NestedProc();
  except
    on E: Exception do
      DebugLn('Sync :: ' + E.Message);
  end;
end;

procedure ExecuteOnMainThread(Method: TProcedureOfObject);
var
  SyncObject: TSyncObject;
begin
  if (GetCurrentThreadID() <> MainThreadID) then
  begin
    SyncObject.Proc       := Method;
    SyncObject.NestedProc := nil;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

procedure ExecuteOnMainThread(Method: TProcedureNested);
var
  SyncObject: TSyncObject;
begin
  if (GetCurrentThreadID() <> MainThreadID) then
  begin
    SyncObject.Proc       := nil;
    SyncObject.NestedProc := Method;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TProcedureOfObject;
    FNestedProc: TProcedureNested;

    procedure DoTerminated(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create(Proc: TProcedureNested; Forget: Boolean); reintroduce;
    constructor Create(Proc: TProcedureOfObject; Forget: Boolean); reintroduce;
  end;

procedure TThreaded.DoTerminated(Sender: TObject);
begin
  Flush(Output);
end;

procedure TThreaded.Execute;
begin
  if Assigned(FNestedProc) then FNestedProc();
  if Assigned(FProc)       then FProc();
end;

constructor TThreaded.Create(Proc: TProcedureNested; Forget: Boolean);
begin
  inherited Create(False, 512*512);

  if Forget then
  begin
    FreeOnTerminate := True;
    OnTerminate := @DoTerminated;
  end;

  FNestedProc := Proc;
end;

constructor TThreaded.Create(Proc: TProcedureOfObject; Forget: Boolean);
begin
  inherited Create(False, 512*512);

  if Forget then
  begin
    FreeOnTerminate := True;
    OnTerminate := @DoTerminated;
  end;

  FProc := Proc;
end;

function Threaded(Method: TProcedureNested): TThread;
begin
  Result := TThreaded.Create(Method, False);
end;

function Threaded(Method: TProcedureOfObject): TThread;
begin
  Result := TThreaded.Create(Method, False);
end;

procedure Threaded(Methods: TProcedureOfObjectArray; Interval: Integer);
var
  Threads: array of TThread;
  I: Integer;
begin
  SetLength(Threads, Length(Methods));
  for I := 0 to High(Threads) do
  begin
    Threads[I] := TThreaded.Create(Methods[I], False);
    if (Interval > 0) then
      Sleep(Interval);
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor();
    Threads[I].Free();
  end;
end;

procedure ThreadedAndForget(Method: TProcedureNested);
begin
  TThreaded.Create(Method, True);
end;

type
  TQueueOnMainThread = class(TObject)
  public
    FParams: TVariantArray;
    FProcNested: TProcedureNested;
    FProcNestedEx: TProcedureNestedEx;
    FProcObject: TProcedureOfObject;

    procedure Execute(Data: PtrInt);
  end;

procedure TQueueOnMainThread.Execute(Data: PtrInt);
begin
  try
    if Assigned(FProcNestedEx) then FProcNestedEx(FParams) else
    if Assigned(FProcNested)   then FProcNested()          else
    if Assigned(FProcObject)   then FProcObject();
  finally
    Free();
  end;
end;

procedure QueueOnMainThread(Proc: TProcedureOfObject);
var
  Queue: TQueueOnMainThread;
begin
  Queue := TQueueOnMainThread.Create();
  Queue.FProcObject := Proc;

  Application.QueueAsyncCall(@Queue.Execute, 0);
end;

procedure QueueOnMainThread(Proc: TProcedureNested);
var
  Queue: TQueueOnMainThread;
begin
  Queue := TQueueOnMainThread.Create();
  Queue.FProcNested := Proc;

  Application.QueueAsyncCall(@Queue.Execute, 0);
end;

procedure QueueOnMainThread(Proc: TProcedureNestedEx; Params: TVariantArray);
var
  Queue: TQueueOnMainThread;
begin
  Queue := TQueueOnMainThread.Create();
  Queue.FParams := Copy(Params);
  Queue.FProcNestedEx := Proc;

  Application.QueueAsyncCall(@Queue.Execute, 0);
end;

end.

