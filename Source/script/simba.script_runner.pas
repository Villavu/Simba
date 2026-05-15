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
  lptypes, lpvartypes, lpmessages,
  simba.base, simba.script;

type
  TSimbaScriptRunner = class(TThread)
  protected
    FScript: TSimbaScript;
    FCompileOnly: Boolean;

    //procedure DoDebugLn(Flags: EDebugLnFlags; Text: String);
    procedure DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);

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
  Forms, FileUtil,
  simba.env, simba.fs, simba.datetime, simba.script_communication,
  simba.baseclass;

//procedure TSimbaScriptRunner.DoDebugLn(Flags: EDebugLnFlags; Text: String);
//begin
//  if (SimbaProcessType = ESimbaProcessType.SCRIPT_WITH_COMMUNICATION) then // Only add flags if we have communication with simba to use them
//    DebugLn(Flags, Text)
//  else
//  begin
//    if Application.HasOption('silent') and (Flags * [EDebugLn.YELLOW, EDebugLn.GREEN] <> []) then
//      Exit;
//
//    DebugLn(Text);
//  end;
//end;

procedure TSimbaScriptRunner.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  DebugLn(DEBUG_YELLOW + Hint);
  Debug(DEBUG_CLEAR);
end;

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

  DebugLn(DEBUG_RED + E.Message);
  if (E is lpException) then
    with lpException(E) do
    begin
      if (StackTrace <> '') then
        DebugLn(DEBUG_RED + StackTrace);
      if (Hint <> '') then
        DebugLn(DEBUG_YELLOW + Hint);

      if (FScript.SimbaCommunication <> nil) then
        FScript.SimbaCommunication.ScriptError(Message, DocPos.Line, DocPos.Col, DocPos.FileName);
    end;
  Debug(DEBUG_RESET);
end;

procedure TSimbaScriptRunner.Execute;
begin
  try
    ExecuteInThread(@DoInputThread);

    try
      if FScript.Compile() then
      begin
        DebugLn(DEBUG_GREEN + 'Succesfully compiled in %.2f milliseconds.', [FScript.CompileTime]);
        Debug(DEBUG_RESET);
      end;
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

      Debug(DEBUG_GREEN);
      if (FScript.RunningTime < 10000) then
        DebugLn('Succesfully executed in %.2f milliseconds.', [FScript.RunningTime])
      else
        DebugLn('Succesfully executed in %s.', [FormatMilliseconds(Round(FScript.RunningTime), '\[hh:mm:ss\]')]);
      Debug(DEBUG_RESET);
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
