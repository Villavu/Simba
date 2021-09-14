unit simba.process;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils;

type
  PProcessID = ^TProcessID;
  PProcessExitStatus = ^TProcessExitStatus;

  TProcessID = type SizeUInt;
  TProcessExitStatus = type Integer;

  TSimbaProcess = record
    function GetScriptPID: TProcessID;
    function GetScriptParameters: TStringArray;
    function GetScriptParameter(Name: String): String;

    function IsProcess64Bit(PID: TProcessID): Boolean;
    function IsProcessRunning(PID: TProcessID): Boolean;
    function GetProcessPath(PID: TProcessID): String;
    procedure TerminateProcess(PID: TProcessID);

    function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload;
    function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload;

    function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload;
    function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload;

    function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;

    function RunDump(Commands: TStringArray): TStringList;

    function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus; overload;
    function RunScript(Script: String; Parameters: TStringArray): TProcessID; overload;
    function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID;
  end;

var
  SimbaProcess: TSimbaProcess;

implementation

uses
  forms, process, lazloggerbase,
  simba.files,
  {$IFDEF WINDOWS}
  simba.process_helpers_windows
  {$ENDIF}
  {$IFDEF LINUX}
  simba.process_helpers_linux
  {$ENDIF}
  {$IFDEF DARWIN}
  simba.process_helpers_darwin
  {$ENDIF};

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
  Process.Options := Process.Options + [poRunIdle];
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
  Result := SimbaProcessHelpers.IsProcess64Bit(PID);
end;

function TSimbaProcess.IsProcessRunning(PID: TProcessID): Boolean;
begin
  Result := SimbaProcessHelpers.IsProcessRunning(PID);
end;

function TSimbaProcess.GetProcessPath(PID: TProcessID): String;
begin
  Result := SimbaProcessHelpers.GetProcessPath(PID);
end;

procedure TSimbaProcess.TerminateProcess(PID: TProcessID);
begin
  SimbaProcessHelpers.TerminateProcess(PID);
end;

function TSimbaProcess.RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus;
begin
  Process.RunCommandInDir(Directory, Executable, Commands, Output, Result);
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
    Process.Parameters.AddStrings(Commands);
    Process.Execute();

    Result := Process.ProcessID;
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus;
begin
  Process.RunCommandInDir(Application.Location, Executable, Commands, Output, Result);
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
  Process.Options := Process.Options + [poRunIdle];
  Process.Executable := Executable;

  for Command in Commands do
    Process.Parameters.Add(Command);

  try
    Result := (Process.RunCommandLoop(Output, ErrorOutput, ExitStatus) = 0) and (ExitStatus = 0) and (GetTickCount64() < Process.Timeout);
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.RunDump(Commands: TStringArray): TStringList;
var
  ProcessOutput, FileName: String;
begin
  Result := nil;

  ForceDirectories(GetDumpPath());
  FileName := GetTempFileName(GetDumpPath(), 'dump');

  try
    if not SimbaProcess.RunCommandTimeout(Application.ExeName, Commands + [FileName], ProcessOutput, 5000) then
      raise Exception.Create('Timed out');

    Result := TStringList.Create();
    Result.LineBreak := #0;
    Result.LoadFromFile(FileName);
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message + ' :: ' + ProcessOutput);

      if (Result <> nil) then
        FreeAndNil(Result);
    end;
  end;

  DeleteFile(FileName);
end;

function TSimbaProcess.RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus;
var
  I: Integer;
begin
  for I := 0 to High(Parameters) do
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
  begin
    if (Length(Parameters[I].Split('=')) <> 2) then
      raise Exception.Create('TSimbaProcess.RunScript: Invalid parameter "' + Parameters[I] + '". Expected "name=value"');

    while (not Parameters[I].StartsWith('--')) do
      Parameters[I] := '-' + Parameters[I];
  end;

  {$IFDEF UNIX}
  Result := Self.RunCommandInDir(Application.Location, GetEnvironmentVariable('SHELL'), ['-c', Application.ExeName + ' ' + String.Join(' ', Parameters) + ' --run ' + Script + ' > ' + OutputFileName]);
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := Self.RunCommandInDir(Application.Location, GetEnvironmentVariable('COMSPEC'), ['/c', Application.ExeName + ' ' + String.Join(' ', Parameters) + ' --run ' + Script + ' > ' + OutputFileName]);
  {$ENDIF}
end;

end.

