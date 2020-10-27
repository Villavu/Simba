unit simba.script_debugger;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, syncobjs,
  simba.script, simba.script_communication;

type
  TSimbaScript_Debugger = class(TThread)
  protected
    FStream: TMemoryStream;
    FLock: TCriticalSection;
    FWarned: Boolean;
    FScript: TSimbaScript;
    FDepth: Int32;

    procedure EnterMethod(Sender: TObject; Index: Int32);
    procedure LeaveMethod(Sender: TObject; Index: Int32; Exception: Boolean);

    procedure Execute; override;
  public
    procedure Queue(constref Event: TSimbaScriptDebuggerEvent);

    constructor Create(Script: TSimbaScript); reintroduce;
    destructor Destroy; override;
  end;

implementation

procedure TSimbaScript_Debugger.EnterMethod(Sender: TObject; Index: Int32);
var
  Event: TSimbaScriptDebuggerEvent;
begin
  if FScript.Terminated then
    Exit;

  Inc(FDepth);

  Event.Depth := FDepth;
  Event.Method := Index;
  Event.Exception := False;

  Queue(Event);
end;

procedure TSimbaScript_Debugger.LeaveMethod(Sender: TObject; Index: Int32; Exception: Boolean);
var
  Event: TSimbaScriptDebuggerEvent;
begin
  if FScript.Terminated then
    Exit;

  Event.Depth := FDepth;
  Event.Method := Index;
  Event.Exception := Exception;

  Queue(Event);

  Dec(FDepth);
end;

procedure TSimbaScript_Debugger.Execute;
var
  Method: TSimbaMethod;
begin
  while (not Terminated) or (FStream.Position > 0) do
  begin
    FLock.Enter();

    try
      if (FStream.Position > 0) then
      begin
        Method := TSimbaMethod_DebuggerEvents.Create(FStream);

        try
          FScript.Invoke(Method);
        finally
          Method.Free();
        end;

        FStream.Position := 0;
      end;
    finally
      FLock.Leave();
    end;

    Sleep(500);
  end;
end;

procedure TSimbaScript_Debugger.Queue(constref Event: TSimbaScriptDebuggerEvent);
begin
  if FStream.Position >= 1024 * 1024 then
  begin
    if not FWarned then
    begin
      WriteLn('Debugger cannot keep up with the scripts function calling!');
      WriteLn('Script will be paused until data has been processed.');

      FWarned := True;
    end;

    while FStream.Position > 0 do
      Sleep(100);
  end;

  FLock.Enter();

  try
    FStream.Write(Event, SizeOf(TSimbaScriptDebuggerEvent));
  finally
    FLock.Leave();
  end;
end;

constructor TSimbaScript_Debugger.Create(Script: TSimbaScript);
var
  I: Int32;
  Method: TSimbaMethod;
begin
  inherited Create(False);

  FDepth := -1;
  FStream := TMemoryStream.Create();
  FLock := TCriticalSection.Create();

  FScript := Script;
  FScript.Compiler.OnEnterMethod := @EnterMethod;
  FScript.Compiler.OnLeaveMethod := @LeaveMethod;

  for I := 0 to High(FScript.Compiler.DebuggingMethods) do
  begin
    Method := TSimbaMethod_DebuggingMethod.Create(FScript.Compiler.DebuggingMethods[I]);

    try
      FScript.Invoke(Method);
    finally
      Method.Free();
    end;
  end;
end;

destructor TSimbaScript_Debugger.Destroy;
begin
  Self.Terminate();
  Self.WaitFor();

  FStream.Free();
  FLock.Free();

  inherited Destroy();
end;

end.

