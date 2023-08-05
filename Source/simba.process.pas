{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.process;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

type
  PProcessID = ^TProcessID;
  PProcessExitStatus = ^TProcessExitStatus;

  TProcessID = type Integer;
  TProcessExitStatus = type Integer;

  TSimbaProcess = record
    function GetScriptPID: TProcessID;
    function GetScriptParameters: TStringArray;
    function GetScriptParameter(Name: String): String;

    function IsProcess64Bit(PID: TProcessID): Boolean;
    function IsProcessRunning(PID: TProcessID): Boolean;
    function GetProcessPath(PID: TProcessID): String;
    function GetProcessMemUsage(PID: TProcessID): Int64;
    function GetProcessStartTime(PID: TProcessID): TDateTime;
    function GetProcessRunnningTime(PID: TProcessID): UInt64;
    procedure TerminateProcess(PID: TProcessID);

    function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload;
    function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload;

    function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload;
    function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload;

    function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;

    function RunDump(FileName: String; Commands: TStringArray): TStringList;

    function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus; overload;
    function RunScript(Script: String; Parameters: TStringArray): TProcessID; overload;
    function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID;
  end;

var
  SimbaProcess: TSimbaProcess;

implementation

uses
  Forms, Process, DateUtils,
  simba.env, simba.files, simba.nativeinterface;

type
  TProcessTimeout = class(TProcess)
  public
    Timeout: UInt64;

    procedure Idle(Sender, Context: TObject; Status: TRunCommandEventCode; const Message: string);
  end;

procedure TProcessTimeout.Idle(Sender, Context: TObject; Status: TRunCommandEventCode; const Message: string);
begin
  if (Status = RunCommandIdle) then
  begin
    if (GetTickCount64() > Timeout) then
      Terminate(255);

    Sleep(RunCommandSleepTime);
  end;
end;

function RunCommandTimeout(Executable: TProcessString; Commands: array of TProcessString; out OutputString: String; Timeout: Int32): Boolean;
Var
  Process: TProcessTimeout;
  ExitStatus: Int32;
  ErrorString: String;
  Command: TProcessString;
begin
  Process := TProcessTimeout.Create(nil);
  Process.OnRunCommandEvent := @Process.Idle;
  Process.Timeout := GetTickCount64() + Timeout;
  Process.Options := Process.Options + [poRunIdle, poStderrToOutPut];
  Process.Executable := Executable;

  for Command in Commands do
    Process.Parameters.Add(Command);

  try
    Result := (Process.RunCommandLoop(OutputString, ErrorString, ExitStatus) = 0) and (ExitStatus = 0) and (GetTickCount64() < Process.Timeout);
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.GetScriptPID: TProcessID;
begin
  Result := GetProcessID();
end;

function TSimbaProcess.GetScriptParameters: TStringArray;
var
  I: Integer;
begin
  SetLength(Result, ParamCount + 1);
  for I := 0 to ParamCount do
    Result[I] := ParamStr(I);
end;

function TSimbaProcess.GetScriptParameter(Name: String): String;
begin
  Result := Application.GetOptionValue(Name);
end;

function TSimbaProcess.IsProcess64Bit(PID: TProcessID): Boolean;
begin
  Result := SimbaNativeInterface.IsProcess64Bit(PID);
end;

function TSimbaProcess.IsProcessRunning(PID: TProcessID): Boolean;
begin
  Result := SimbaNativeInterface.IsProcessRunning(PID);
end;

function TSimbaProcess.GetProcessPath(PID: TProcessID): String;
begin
  Result := SimbaNativeInterface.GetProcessPath(PID);
end;

function TSimbaProcess.GetProcessMemUsage(PID: TProcessID): Int64;
begin
  Result := SimbaNativeInterface.GetProcessMemUsage(PID);
end;

function TSimbaProcess.GetProcessStartTime(PID: TProcessID): TDateTime;
begin
  Result := SimbaNativeInterface.GetProcessStartTime(PID);
end;

function TSimbaProcess.GetProcessRunnningTime(PID: TProcessID): UInt64;
begin
  Result := MillisecondsBetween(GetProcessStartTime(PID), Now());
end;

procedure TSimbaProcess.TerminateProcess(PID: TProcessID);
begin
  SimbaNativeInterface.TerminateProcess(PID);
end;

function TSimbaProcess.RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus;
begin
  Process.RunCommandInDir(Directory, Executable, Commands, Output, Result, [poStderrToOutPut]);
end;

function TSimbaProcess.RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID;
var
  Process: TProcess;
begin
  Result := 0;

  Process := TProcess.Create(nil);
  try
    Process.CurrentDirectory := Directory;
    Process.Executable := Executable;
    Process.Options := Process.Options + [poStderrToOutPut];
    Process.Parameters.AddStrings(Commands);
    Process.Execute();

    Result := Process.ProcessID;
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus;
begin
  Process.RunCommandInDir(Application.Location, Executable, Commands, Output, Result, [poStderrToOutPut]);
end;

function TSimbaProcess.RunCommand(Executable: String; Commands: TStringArray): TProcessID;
var
  Process: TProcess;
begin
  Result := 0;

  Process := TProcess.Create(nil);
  try
    Process.CurrentDirectory := Application.Location;
    Process.Executable := Executable;
    Process.Options := Process.Options + [poStderrToOutPut];
    Process.Parameters.AddStrings(Commands);
    Process.Execute();

    Result := Process.ProcessID;
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;
var
  Process: TProcessTimeout;
  ExitStatus: Int32;
  ErrorOutput: String;
  Command: TProcessString;
begin
  Process := TProcessTimeout.Create(nil);
  Process.OnRunCommandEvent := @Process.Idle;
  Process.Timeout := GetTickCount64() + Timeout;
  Process.Options := Process.Options + [poRunIdle, poStderrToOutPut];
  Process.Executable := Executable;

  for Command in Commands do
    Process.Parameters.Add(Command);

  try
    Result := (Process.RunCommandLoop(Output, ErrorOutput, ExitStatus) = 0) and (ExitStatus = 0) and (GetTickCount64() < Process.Timeout);
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.RunDump(FileName: String; Commands: TStringArray): TStringList;
var
  DumpFileName, ProcessOutput: String;
begin
  DumpFileName := SimbaEnv.DumpsPath + TSimbaFile.FileHash(FileName);

  Result := TStringList.Create();
  Result.LineBreak := #0;

  if FileExists(DumpFileName) then
  begin
    Result.LoadFromFile(DumpFileName);

    Exit;
  end;

  try
    if not SimbaProcess.RunCommandTimeout(Application.ExeName, Commands + [DumpFileName], ProcessOutput, 5000) then
      raise Exception.Create('Timed out');

    Result.LoadFromFile(DumpFileName);
  except
    on E: Exception do
      raise Exception.Create(E.Message + ' :: ' + ProcessOutput);
  end;
end;

function TSimbaProcess.RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus;
var
  I: Integer;
begin
  for I := 0 to High(Parameters) do
    if (Parameters[I] <> '') then
    begin
      if (Length(Parameters[I].Split('=')) <> 2) then
        raise Exception.Create('TSimbaProcess.RunScript: Invalid parameter "' + Parameters[I] + '". Expected "name=value"');

      while (not Parameters[I].StartsWith('--')) do
        Parameters[I] := '-' + Parameters[I]
    end;

  Result := Self.RunCommandInDir(Application.Location, Application.ExeName, Parameters + ['--run', Script], Output);
end;

function TSimbaProcess.RunScript(Script: String; Parameters: TStringArray): TProcessID;
var
  I: Integer;
begin
  for I := 0 to High(Parameters) do
    if (Parameters[I] <> '') then
    begin
      if (Length(Parameters[I].Split('=')) <> 2) then
        raise Exception.Create('TSimbaProcess.RunScript: Invalid parameter "' + Parameters[I] + '". Expected "name=value"');

      while (not Parameters[I].StartsWith('--')) do
        Parameters[I] := '-' + Parameters[I]
    end;

  Result := Self.RunCommandInDir(Application.Location, Application.ExeName, Parameters + ['--run', Script]);
end;

function TSimbaProcess.RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID;
var
  I: Integer;
begin
  for I := 0 to High(Parameters) do
    if (Parameters[I] <> '') then
    begin
      if (Length(Parameters[I].Split('=')) <> 2) then
        raise Exception.Create('TSimbaProcess.RunScript: Invalid parameter "' + Parameters[I] + '". Expected "name=value"');

      while (not Parameters[I].StartsWith('--')) do
        Parameters[I] := '-' + Parameters[I];
    end;

  {$IFDEF UNIX}
  Result := Self.RunCommandInDir(Application.Location, GetEnvironmentVariable('SHELL'), ['-c', Application.ExeName + ' ' + ' '.Join(Parameters) + ' --run ' + Script + ' > ' + OutputFileName]);
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := Self.RunCommandInDir(Application.Location, GetEnvironmentVariable('COMSPEC'), ['/c', Application.ExeName + ' ' + ' '.Join(Parameters) + ' --run ' + Script + ' > ' + OutputFileName]);
  {$ENDIF}
end;

end.

