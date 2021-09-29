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
    FDepth: Int32;
    FMethods: TStringArray;
    FStartEvent: TSimpleEvent;

    procedure Write(constref Event: TSimbaScriptDebuggerEvent);

    procedure EnterMethod(Index: Int32);
    procedure LeaveMethod(Index: Int32; Exception: Boolean);

    procedure Execute; override;
  public
    procedure Start;

    constructor Create(Script: TObject); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  simba.script, simba.script_compiler_debugger;

procedure TSimbaScript_Debugger.EnterMethod(Index: Int32);
var
  Event: TSimbaScriptDebuggerEvent;
begin
  Inc(FDepth);

  Event.Depth := FDepth;
  Event.Method := Index;
  Event.Exception := False;

  Write(Event);
end;

procedure TSimbaScript_Debugger.LeaveMethod(Index: Int32; Exception: Boolean);
var
  Event: TSimbaScriptDebuggerEvent;
begin
  Event.Depth := FDepth;
  Event.Method := Index;
  Event.Exception := Exception;

  Write(Event);

  Dec(FDepth);
end;

procedure TSimbaScript_Debugger.Execute;
var
  I: Int32;
begin
  for I := 0 to High(FMethods) do
    with TSimbaScript(FScript) do
      Invoke(TSimbaMethod_DebuggingMethod.Create(FMethods[I]), True);

  FStartEvent.SetEvent();

  while (FStream.Position > 0) or (not Terminated) do
  begin
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
  end;
end;

procedure TSimbaScript_Debugger.Start;
begin
  inherited Start();

  FStartEvent.WaitFor(INFINITE);
end;

procedure TSimbaScript_Debugger.Write(constref Event: TSimbaScriptDebuggerEvent);
begin
  if FStream.Position >= 1024 * 1024 then
  begin
    if not FWarned then
    begin
      WriteLn('Debugger cannot keep up with function calling.');
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

constructor TSimbaScript_Debugger.Create(Script: TObject);
begin
  inherited Create(True);

  FDepth := -1;
  FStream := TMemoryStream.Create();
  FLock := TCriticalSection.Create();
  FScript := Script;
  FStartEvent := TSimpleEvent.Create();

  with TSimbaScript(FScript) do
    InitializeDebugger(Compiler, @FMethods, @Self.EnterMethod, @Self.LeaveMethod);
end;

destructor TSimbaScript_Debugger.Destroy;
begin
  FStream.Free();
  FLock.Free();
  FStartEvent.Free();

  inherited Destroy();
end;

end.

