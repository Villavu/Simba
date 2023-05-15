{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scriptthread;

{$i simba.inc}

{$IFDEF DARWIN}
  {$DEFINE COCOA_TERMINATE_FIX} // https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39496
{$ENDIF}

{$IFDEF COCOA_TERMINATE_FIX}
  {$MODESWITCH objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes,
  simba.script, simba.mufasatypes;

type
  TSimbaScriptRunner = class(TThread)
  protected
    FScript: TSimbaScript;
    FCompileOnly: Boolean;

    procedure DebugLnGreen(S: String);
    procedure DebugLnRed(S: String);
    procedure DebugLnYellow(S: String);

    procedure DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);

    procedure HandleTerminate(Sender: TObject);
    procedure HandleInput;
    procedure HandleException(E: Exception);

    procedure Execute; override;
  public
    property Script: TSimbaScript read FScript;

    constructor Create(FileName: String;
      SimbaCommunication, TargetWindow: String;
      CompileOnly: Boolean
    ); reintroduce;
  end;

var
  SimbaScriptThread: TSimbaScriptRunner;

implementation

uses
  {$IFDEF COCOA_TERMINATE_FIX}
  cocoaall, cocoaint, cocoautils,
  {$ENDIF}
  forms, fileutil, lpmessages,
  simba.files, simba.datetime, simba.script_communication;

procedure TSimbaScriptRunner.DebugLnGreen(S: String);
begin
  if (SimbaProcessType = ESimbaProcessType.SCRIPT_WITH_COMMUNICATION) then
    SimbaDebugLn([EDebugLn.GREEN], S)
  else
    DebugLn(S);
end;

procedure TSimbaScriptRunner.DebugLnRed(S: String);
begin
  if (SimbaProcessType = ESimbaProcessType.SCRIPT_WITH_COMMUNICATION) then
    SimbaDebugLn([EDebugLn.RED], S)
  else
    DebugLn(S);
end;

procedure TSimbaScriptRunner.DebugLnYellow(S: String);
begin
  if (SimbaProcessType = ESimbaProcessType.SCRIPT_WITH_COMMUNICATION) then
    SimbaDebugLn([EDebugLn.YELLOW], S)
  else
    DebugLn(S);
end;

procedure TSimbaScriptRunner.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  DebugLnYellow(Hint);
end;

procedure TSimbaScriptRunner.HandleTerminate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  if (StartupConsoleMode <> 0) then
  begin
    DebugLn('Press enter to exit');

    ReadLn();
  end;
  {$ENDIF}

  Application.Terminate();
  while (not Application.Terminated) do
    Application.ProcessMessages();

  {$IFDEF COCOA_TERMINATE_FIX}
  // https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39496
  NSApplication.sharedApplication.postEvent_AtStart(
    nsEvent.otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2(
      NSApplicationDefined, GetNSPoint(0, 0), 0, NSTimeIntervalSince1970, 0, nil, 0, 0, 0
    ),
    True
  );
  {$ENDIF}
end;

procedure TSimbaScriptRunner.HandleInput;
var
  Stream: THandleStream;
  State: ESimbaScriptState;
begin
  Stream := THandleStream.Create(StdInputHandle);
  while Stream.Read(State, SizeOf(ESimbaScriptState)) = SizeOf(ESimbaScriptState) do
    FScript.State := State;

  Stream.Free();
end;

procedure TSimbaScriptRunner.HandleException(E: Exception);
var
  Line: String;
begin
  DebugLnRed(E.Message);

  if (E is lpException) then
  begin
    with lpException(E) do
    begin
      for Line in lpException(E).StackTrace.Split(LineEnding) do
        if (Line <> '') then
          DebugLnRed(Line);

      if (FScript.SimbaCommunication <> nil) then
        FScript.SimbaCommunication.ScriptError(E.Message, DocPos.Line, DocPos.Col, DocPos.FileName)
      else
        FScript.SimbaCommunication.ScriptError(E.Message, 0, 0, '');
    end;
  end;

  if (FScript.SimbaCommunication = nil) then
    ExitCode := 1;
end;

procedure TSimbaScriptRunner.Execute;
{$IFDEF SIMBA_HAS_DEBUGINFO}
var
  I: Integer;
{$ENDIF}
begin
  try
    ExecuteInThread(@HandleInput);

    if FScript.Compile() then
    begin
      DebugLnGreen('Succesfully compiled in %.2f milliseconds.'.Format([FScript.CompileTime]));

      if (not FCompileOnly) then
      begin
        FScript.Run();

        if (Script.RunningTime < 10000) then
          DebugLnGreen('Succesfully executed in %.2f milliseconds.'.Format([Script.RunningTime]))
        else
          DebugLnGreen('Succesfully executed in %s.'.Format([FormatMilliseconds(Script.RunningTime, '\[hh:mm:ss\]')]));
      end;
    end;
  except
    on E: Exception do
      HandleException(E);
  end;

  // Free the script in the thread so it (hopefully) doesn't mess up the rest of the process if something goes wrong.
  try
    FScript.Free();
  except
    on E: Exception do
    begin
      DebugLn('Exception occurred while cleaning up the script: ' + E.Message);

      {$IFDEF SIMBA_HAS_DEBUGINFO}
      DebugLn('Stack trace:');
      DebugLn(BackTraceStrFunc(ExceptAddr));
      for I := 0 to ExceptFrameCount - 1 do
        DebugLn(BackTraceStrFunc(ExceptFrames[I]));
      {$ENDIF}
    end;
  end;
end;

constructor TSimbaScriptRunner.Create(FileName: String; SimbaCommunication, TargetWindow: String; CompileOnly: Boolean);
begin
  inherited Create(False);

  FreeOnTerminate := True;
  OnTerminate := @HandleTerminate;

  FCompileOnly := CompileOnly;

  FScript := TSimbaScript.Create();
  FScript.Script := ReadFile(FileName);
  FScript.ScriptFileName := FileName;
  FScript.SimbaCommunicationServer := SimbaCommunication;
  FScript.TargetWindow := TargetWindow;

  // Simba created a temp file. Most likely default script.
  if FileIsInDirectory(FileName, GetDataPath()) then
  begin
    FScript.ScriptFileName := ChangeFileExt(ExtractFileName(FileName), '');

    // temp file, so delete.
    if FileExists(FileName) then
      DeleteFile(FileName);
  end;
end;

end.

