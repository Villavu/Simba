unit simba.process;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils;

type
  TSimbaProcess = record
    function GetProcessID: SizeUInt;
    function GetProcessParameters: TStringArray;
    function GetProcessParameter(Name: String): String;

    function IsProcessRunning(PID: SizeUInt): Boolean;

    function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): Int32; overload;
    function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): Boolean; overload;

    function RunCommand(Executable: String; Commands: TStringArray; out Output: String): Int32; overload;
    function RunCommand(Executable: String; Commands: TStringArray): Boolean; overload;

    function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;

    procedure RunScript(Script: String; Parameters: TStringArray);
    procedure RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String);
  end;

var
  SimbaProcess: TSimbaProcess;

implementation

uses
  forms, process
  {$IFDEF WINDOWS},
  Windows
  {$ENDIF}
  {$IFDEF UNIX},
  BaseUnix
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

// TSimbaProcess
function TSimbaProcess.GetProcessID: SizeUInt;
begin
  Result := System.GetProcessID();
end;

function TSimbaProcess.GetProcessParameters: TStringArray;
var
  I: Integer;
begin
  SetLength(Result, ParamCount + 1);
  for I := 0 to ParamCount do
    Result[I] := ParamStr(I);
end;

function TSimbaProcess.GetProcessParameter(Name: String): String;
begin
  Result := Application.GetOptionValue(Name);
end;

function TSimbaProcess.IsProcessRunning(PID: SizeUInt): Boolean;
{$IF DEFINED(UNIX)}
begin
  Result := fpkill(PID, 0) <> 0;
end;
{$ELSEIF DEFINED(WINDOWS)}
var
  Handle: THandle;
  ExitCode: UInt32 = 0;
begin
  Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);

  if (Handle > 0) then
  try
    Result := GetExitCodeProcess(Handle, ExitCode) and (ExitCode = STILL_ACTIVE);
  finally
    CloseHandle(Handle);
  end;
end;
{$ELSE}
begin
  raise Exception.Create('TSimbaProcess.IsProcessRunning not supported on this system');
end;
{$ENDIF}

function TSimbaProcess.RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): Int32;
begin
  Process.RunCommandInDir(Directory, Executable, Commands, Output, Result);
end;

function TSimbaProcess.RunCommandInDir(Directory, Executable: String; Commands: TStringArray): Boolean;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.CurrentDirectory := Directory;
    Process.Executable := Executable;
    Process.Parameters.AddStrings(Commands);
    Process.Execute();
  finally
    Process.Free();
  end;
end;

function TSimbaProcess.RunCommand(Executable: String; Commands: TStringArray; out Output: String): Int32;
begin
  Process.RunCommandInDir(Application.Location, Executable, Commands, Output, Result);
end;

function TSimbaProcess.RunCommand(Executable: String; Commands: TStringArray): Boolean;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.CurrentDirectory := Application.Location;
    Process.Executable := Executable;
    Process.Parameters.AddStrings(Commands);
    Process.Execute();
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

procedure TSimbaProcess.RunScript(Script: String; Parameters: TStringArray);
var
  I: Integer;
begin
  for I := 0 to High(Parameters) do
  begin
    if (Length(Parameters[I].Split('=')) <> 2) then
      raise Exception.Create('TSimbaProcess.RunScript: Invalid parameter "' + Parameters[I] + '". Expected "name=value"');

    if not Parameters[I].StartsWith('--') then
      Parameters[I] := '--' + Parameters[I];
  end;

  Self.RunCommandInDir(Application.Location, Application.ExeName, Parameters + ['--run', Script]);
end;

procedure TSimbaProcess.RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String);
begin
  Self.RunScript(Script, Parameters + ['log=' + OutputFileName]);
end;

end.

