{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_debugger;

{$i simba.inc}

interface

uses
  classes, sysutils, syncobjs,
  simba.script_communication;

type
  TSimbaScript_Debugger = class(TThread)
  protected
    FScript: TObject;
    FStream: TMemoryStream;
    FLock: TCriticalSection;
    FWarned: Boolean;
    FDepth: Integer;
    FMethods: TStringArray;
    FStarted: Boolean;

    procedure Write(const Event: TSimbaScriptDebuggerEvent);

    procedure EnterMethod(const Index: Integer);
    procedure LeaveMethod(const Index: Integer; const Exception: Boolean);

    procedure Execute; override;
  public
    procedure Compile;
    procedure Run;

    constructor Create(Script: TObject); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  lazloggerbase,
  simba.script, simba.script_compiler_debugger;

procedure TSimbaScript_Debugger.EnterMethod(const Index: Integer);
var
  Event: TSimbaScriptDebuggerEvent;
begin
  Inc(FDepth);

  Event.Depth := FDepth;
  Event.Method := Index;
  Event.Exception := False;

  Write(Event);
end;

procedure TSimbaScript_Debugger.LeaveMethod(const Index: Integer; const Exception: Boolean);
var
  Event: TSimbaScriptDebuggerEvent;
begin
  Event.Depth := FDepth;
  Event.Method := Index;
  Event.Exception := Exception;

  Write(Event);

  Dec(FDepth);
end;

procedure TSimbaScript_Debugger.Compile;
begin
  with TSimbaScript(FScript) do
    InitializeDebugger(Compiler, @FMethods, @Self.EnterMethod, @Self.LeaveMethod);
end;

procedure TSimbaScript_Debugger.Run;
begin
  Start();
  while not FStarted do
    Sleep(100);
end;

procedure TSimbaScript_Debugger.Execute;
var
  I: Integer;
begin
  // Send method names
  for I := 0 to High(FMethods) do
    with TSimbaScript(FScript) do
      Invoke(TSimbaMethod_DebuggingMethod.Create(FMethods[I]), True);

  FStarted := True;
  repeat
    FLock.Enter();

    try
      if (FStream.Position > 0) then
      begin
        with TSimbaScript(FScript) do
          Invoke(TSimbaMethod_DebuggerEvents.Create(FStream), True);

        FStream.Position := 0;
      end;
    finally
      FLock.Leave();
    end;

    Sleep(500);
  until Terminated;

  // Empty stream. Shouldn't need lock.
  if (FStream.Position > 0) then
    with TSimbaScript(FScript) do
      Invoke(TSimbaMethod_DebuggerEvents.Create(FStream), True);
end;

procedure TSimbaScript_Debugger.Write(const Event: TSimbaScriptDebuggerEvent);
begin
  if FStream.Position >= 1024 * 1024 then
  begin
    if not FWarned then
    begin
      DebugLn('Debugger cannot keep up with function calling.');
      DebugLn('Script will be paused until data has been processed.');

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

constructor TSimbaScript_Debugger.Create(Script: TObject);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FDepth := -1;
  FStream := TMemoryStream.Create();
  FLock := TCriticalSection.Create();
  FScript := Script;
end;

destructor TSimbaScript_Debugger.Destroy;
begin
  if (FStream <> nil) then
    FreeAndNil(FStream);
  if (FLock <> nil) then
    FreeAndNil(FLock);

  inherited Destroy();
end;

end.

