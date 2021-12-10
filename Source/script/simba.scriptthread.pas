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
  simba.script;

type
  TSimbaScriptThread = class(TThread)
  protected
    FScript: TSimbaScript;
    FCompileOnly: Boolean;

    procedure HandleTerminate(Sender: TObject);
    procedure HandleInput;
    procedure HandleException(const E: Exception);

    procedure Execute; override;
  public
    property Script: TSimbaScript read FScript;

    constructor Create(FileName: String;
      SimbaCommunication, TargetWindow: String;
      CompileOnly, Debugging: Boolean
    ); reintroduce;
  end;

var
  SimbaScriptThread: TSimbaScriptThread;

implementation

uses
  {$IFDEF COCOA_TERMINATE_FIX}
  cocoaall, cocoaint, cocoautils,
  {$ENDIF}
  forms, fileutil, lazloggerbase, lpmessages,
  simba.files, simba.datetime, simba.script_communication, simba.script_debugger;

procedure TSimbaScriptThread.HandleTerminate(Sender: TObject);
begin
  if (FatalException is Exception) then
    DebugLn('Note: Script thread did not exit cleanly: ', Exception(FatalException).Message);

  {$IFDEF WINDOWS}
  if (StartupConsoleMode <> 0) then
  begin
    DebugLn('Press enter to exit');

    ReadLn();
  end;
  {$ENDIF}

  Application.Terminate();
  while not Application.Terminated do
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

procedure TSimbaScriptThread.HandleInput;
var
  Stream: THandleStream;
  Value: Integer;
begin
  Stream := THandleStream.Create(StdInputHandle);
  while Stream.Read(Value, SizeOf(Integer)) = SizeOf(Integer) do
  begin
    FScript.State := TSimbaScriptState(Value);

    // Stop button was clicked
    if (FScript.State = SCRIPT_STATE_STOP) then
      PBoolean(FScript.Compiler['IsTerminatedByUser'].Ptr)^ := True;
  end;

  Stream.Free();
end;

procedure TSimbaScriptThread.HandleException(const E: Exception);
begin
  if (TextRec(Output).Mode <> fmClosed) then // Can WriteLn
    WriteLn(E.Message);

  if FScript.CanInvoke() then
  begin
    if (E is lpException) then
      with E as lpException do
        FScript.Invoke(TSimbaMethod_ScriptError.Create(DocPos.Line, DocPos.Col, DocPos.FileName), True);
  end else
    ExitCode := 1;
end;

procedure TSimbaScriptThread.Execute;
begin
  try
    ExecuteInThread(@HandleInput);

    if FScript.Compile() then
    begin
      DebugLn('Succesfully compiled in %.2f milliseconds.', [FScript.CompileTime]);
      if FCompileOnly then
        Exit;

      FScript.Run();

      if (Script.RunningTime < 10000) then
        DebugLn('Succesfully executed in %.2f milliseconds.', [Script.RunningTime])
      else
        DebugLn('Succesfully executed in %s.', [TimeStamp(Round(Script.RunningTime))]);
    end;
  except
    on E: Exception do
      HandleException(E);
  end;

  if (FScript <> nil) then
    FreeAndNil(FScript);
end;

constructor TSimbaScriptThread.Create(FileName: String; SimbaCommunication, TargetWindow: String; CompileOnly, Debugging: Boolean);
begin
  inherited Create(False);

  FreeOnTerminate := True;
  OnTerminate := @HandleTerminate;

  FCompileOnly := CompileOnly;

  FScript := TSimbaScript.Create();
  FScript.Script := ReadFile(FileName);
  FScript.ScriptFileName := FileName;
  FScript.SimbaCommunication := SimbaCommunication;
  FScript.TargetWindow := TargetWindow;

  if Debugging then
    FScript.Debugger := TSimbaScript_Debugger.Create(FScript);

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

