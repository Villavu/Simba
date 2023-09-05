unit simba.threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms,
  simba.mufasatypes;

type
  TThreadProc = procedure;
  TThreadNestedProc = procedure is nested;
  // TThreadMethod = procedure of object;

function IsMainThread: Boolean;

procedure QueueOnMainThread(Proc: TThreadProc); overload;
procedure QueueOnMainThread(Method: TThreadMethod); overload;

procedure RunInMainThread(Method: TThreadMethod); overload;
procedure RunInMainThread(NestedMethod: TThreadNestedProc); overload;

function RunInThread(Proc: TThreadProc; FreeOnTerminate: Boolean = False): TThread; overload;
function RunInThread(Method: TThreadMethod; FreeOnTerminate: Boolean = False): TThread; overload;
function RunInThread(NestedMethod: TThreadNestedProc; FreeOnTerminate: Boolean = False): TThread; overload;

implementation

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

function IsMainThread: Boolean;
begin
  Result := GetCurrentThreadID() = MainThreadID;
end;

type
  TThreaded = class(TThread)
  protected
    FProc: TThreadProc;
    FNestedProc: TThreadNestedProc;
    FMethod: TThreadMethod;

    procedure Execute; override;
  public
    constructor Create(Proc: TThreadProc; AFreeOnTerminate: Boolean); reintroduce;
    constructor Create(NestedProc: TThreadNestedProc; AFreeOnTerminate: Boolean); reintroduce;
    constructor Create(Method: TThreadMethod; AFreeOnTerminate: Boolean); reintroduce;
  end;

procedure TThreaded.Execute;
begin
  try
    if Assigned(FMethod)     then FMethod()      else
    if Assigned(FProc)       then FProc()        else
    if Assigned(FNestedProc) then FNestedProc();
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

constructor TThreaded.Create(NestedProc: TThreadNestedProc; AFreeOnTerminate: Boolean);
begin
  inherited Create(False, DefaultStackSize div 2);

  FNestedProc := NestedProc;

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

function RunInThread(NestedMethod: TThreadNestedProc; FreeOnTerminate: Boolean): TThread;
begin
  Result := TThreaded.Create(NestedMethod, FreeOnTerminate);
end;

procedure RunInMainThread(Method: TThreadMethod);
var
  SyncObject: TSyncObject;
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
  SyncObject: TSyncObject;
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

end.

