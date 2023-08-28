unit simba.threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms,
  simba.mufasatypes;

type
  TThreadNestedMethod = procedure is nested;

function IsMainThread: Boolean;

procedure QueueOnMainThread(Proc: TThreadMethod);

procedure RunInMainThread(Method: TThreadMethod); overload;
procedure RunInMainThread(NestedMethod: TThreadNestedMethod); overload;

function RunInThread(Method: TThreadMethod; FreeOnTerminate: Boolean = False): TThread; overload;
function RunInThread(NestedMethod: TThreadNestedMethod; FreeOnTerminate: Boolean = False): TThread; overload;

implementation

type
  TSyncObject = object
    Proc: TThreadMethod;
    NestedProc: TThreadNestedMethod;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
begin
  try
    if Assigned(Proc)       then Proc();
    if Assigned(NestedProc) then NestedProc();
  except
    on E: Exception do
      DebugLn('RunOnMainThread exception: ' + E.Message);
  end;
end;

function IsMainThread: Boolean;
begin
  Result := GetCurrentThreadID() = MainThreadID;
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TThreadMethod;
    FNestedProc: TThreadNestedMethod;

    procedure Execute; override;
  public
    constructor Create(Proc: TThreadNestedMethod; AFreeOnTerminate: Boolean); reintroduce;
    constructor Create(Proc: TThreadMethod; AFreeOnTerminate: Boolean); reintroduce;
  end;

procedure TThreaded.Execute;
begin
  if Assigned(FNestedProc) then FNestedProc();
  if Assigned(FProc)       then FProc();
end;

constructor TThreaded.Create(Proc: TThreadNestedMethod; AFreeOnTerminate: Boolean);
begin
  inherited Create(False, DefaultStackSize div 2);

  FNestedProc := Proc;

  FreeOnTerminate := AFreeOnTerminate;
end;

constructor TThreaded.Create(Proc: TThreadMethod; AFreeOnTerminate: Boolean);
begin
  inherited Create(False, DefaultStackSize div 2);

  FProc := Proc;

  FreeOnTerminate := AFreeOnTerminate;
end;

type
  TQueueObject = class
  public
    Proc: TThreadMethod;

    procedure Execute(Data: PtrInt);
  end;

procedure TQueueObject.Execute(Data: PtrInt);
begin
  try
    Proc();
  except
    on E: Exception do
      DebugLn('QueueOnMainThread exception: ' + E.Message);
  end;

  Free();
end;

function RunInThread(Method: TThreadMethod; FreeOnTerminate: Boolean): TThread;
begin
  Result := TThreaded.Create(Method, FreeOnTerminate);
end;

function RunInThread(NestedMethod: TThreadNestedMethod; FreeOnTerminate: Boolean): TThread;
begin
  Result := TThreaded.Create(NestedMethod, FreeOnTerminate);
end;

procedure RunInMainThread(Method: TThreadMethod);
var
  SyncObject: TSyncObject;
begin
  if (not IsMainThread()) then
  begin
    SyncObject.Proc       := Method;
    SyncObject.NestedProc := nil;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    Method();
end;

procedure RunInMainThread(NestedMethod: TThreadNestedMethod);
var
  SyncObject: TSyncObject;
begin
  if (not IsMainThread()) then
  begin
    SyncObject.Proc       := nil;
    SyncObject.NestedProc := NestedMethod;

    TThread.Synchronize(nil, @SyncObject.Execute);
  end else
    NestedMethod();
end;

procedure QueueOnMainThread(Proc: TThreadMethod);
var
  Queue: TQueueObject;
begin
  Queue := TQueueObject.Create();
  Queue.Proc := Proc;

  Application.QueueAsyncCall(@Queue.Execute, 0);
end;

end.

