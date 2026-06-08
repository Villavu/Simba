{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_runner;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.baseclass,
  simba.script;

type
  TSimbaScriptRunner = class(TThread)
  protected
    FScript: TSimbaScript;
    FCompileOnly: Boolean;

    procedure DoApplicationTerminate(Sender: TObject);
    procedure DoInputThread;
    procedure DoError(E: Exception);

    procedure Execute; override;
  public
    constructor Create;
    constructor Create(FileName: String; SimbaCommunication, TargetWindow: String; CompileOnly, Hints: Boolean); reintroduce; overload;
    constructor Create(SimbaCommunication, TargetWindow: String; CompileOnly, Hints: Boolean); reintroduce; overload;
  end;

implementation

uses
  Forms,
  lpmessages,
  simba.fs,
  simba.datetime,
  simba.script_communication;

procedure TSimbaScriptRunner.DoApplicationTerminate(Sender: TObject);
begin
  Application.Terminate();
  while (not Application.Terminated) do
    Application.ProcessMessages();
end;

procedure TSimbaScriptRunner.DoInputThread;
var
  Stream: THandleStream;
  State: ESimbaScriptState;
begin
  Stream := THandleStream.Create(StdInputHandle);
  while Stream.Read(State{%H-}, SizeOf(ESimbaScriptState)) = SizeOf(ESimbaScriptState) do
    FScript.State := State;
  Stream.Free();
end;

procedure TSimbaScriptRunner.DoError(E: Exception);
begin
  ExitCode := 1;

  DebugLn(DEBUG_RED + E.Message + DEBUG_RESET_EOL);
  if (E is lpException) then
    with lpException(E) do
    begin
      if (StackTrace <> '') then
        DebugLn(DEBUG_RED + StackTrace + DEBUG_RESET_EOL);
      if (Hint <> '') then
        DebugLn(DEBUG_YELLOW + Hint + DEBUG_RESET_EOL);

      if (FScript.SimbaCommunication <> nil) then
        FScript.SimbaCommunication.ScriptError(Message, DocPos.Line, DocPos.Col, DocPos.FileName);
    end;
  DebugLn(DEBUG_FOCUS);
end;

procedure TSimbaScriptRunner.Execute;
begin
  try
    ExecuteInThread(@DoInputThread);

    try
      if FScript.Compile() then
        DebugLn(DEBUG_GREEN + 'Succesfully compiled in %.2f milliseconds.' + DEBUG_RESET_EOL, [FScript.CompileTime]);
    except
      on E: Exception do
      begin
        DoError(E);
        Exit;
      end;
    end;

    if not FCompileOnly then
    try
      FScript.Run();

      if (FScript.RunningTime < 10000) then
        DebugLn(DEBUG_GREEN + 'Succesfully executed in %.2f milliseconds.' + DEBUG_RESET_EOL, [FScript.RunningTime])
      else
        DebugLn(DEBUG_GREEN + 'Succesfully executed in %s.' + DEBUG_RESET_EOL, [FormatMilliseconds(Round(FScript.RunningTime), '\[hh:mm:ss\]')]);
    except
      on E: Exception do
        DoError(E);
    end;

    PrintUnfinishedThreads();
    PrintUnfreedThreads();
    PrintUnfreedObjects();
  finally
    FScript.Free(); // Free the script in thread so it hopefully doesn't nuke the process
  end;
end;

constructor TSimbaScriptRunner.Create;
begin
  inherited Create(False);

  FreeOnTerminate := True;
  OnTerminate := @DoApplicationTerminate;
end;

constructor TSimbaScriptRunner.Create(FileName: String; SimbaCommunication, TargetWindow: String; CompileOnly, Hints: Boolean);
begin
  Create();

  FCompileOnly := CompileOnly;

  if (SimbaCommunication <> '') then
    FScript := TSimbaScript.Create(FileName, TSimbaScriptCommunication.Create(SimbaCommunication))
  else
    FScript := TSimbaScript.Create(FileName);

  FScript.Script := TSimbaFile.FileRead(FileName);
  FScript.ScriptFileName := FileName;
  FScript.TargetWindow := TargetWindow;
  FScript.Hints := Hints;
end;

constructor TSimbaScriptRunner.Create(SimbaCommunication, TargetWindow: String; CompileOnly, Hints: Boolean);
begin
  Create();

  FCompileOnly := CompileOnly;

  FScript := TSimbaScript.Create(TSimbaScriptCommunication.Create(SimbaCommunication));
  FScript.TargetWindow := TargetWindow;
  FScript.Hints := Hints;
end;

end.
