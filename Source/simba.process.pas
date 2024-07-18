{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.process;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Process,
  simba.base, simba.baseclass;

type
  TProcessID = type Integer;
  TProcessExitCode = type Integer;

  TRunningProcess = class(TSimbaBaseClass)
  protected
    FProcess: TProcess;
    FPID: TProcessID;

    function GetExitCode: TProcessExitCode;
    function GetRunning: Boolean;
  public
    constructor Create(Process: TProcess); reintroduce;
    destructor Destroy; override;

    function WaitOnExit: Boolean; overload;
    function WaitOnExit(Timeout: Integer): Boolean; overload;
    function Terminate(AExitCode: Integer): Boolean;

    procedure WaitUntilFinishedOrTimeout(Timeout: Integer = 0);

    property Running: Boolean read GetRunning;
    property PID: TProcessID read FPID;
    property ExitCode: TProcessExitCode read GetExitCode;
  end;

  TRunningProcessPiped = class(TRunningProcess)
  protected
    function GetReadBytesAvailable: Integer;
  public
    // read from stdout
    function Read(Buf: PByte; Count: Integer): Integer;
    function ReadString: String;

    // write to stdin
    function Write(Buf: PByte; Count: Integer): Integer;
    function WriteString(Str: String): Integer;

    function WaitUntilFinishedOrTimeout(Timeout: Integer = 0): String; reintroduce;

    property ReadBytesAvailable: Integer read GetReadBytesAvailable;
  end;

  function StartProcess(Executable: String; Params: TStringArray): TRunningProcess; overload;
  function StartProcess(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray): TRunningProcess; overload;

  function StartProcessPiped(Executable: String; Params: TStringArray): TRunningProcessPiped; overload;
  function StartProcessPiped(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray): TRunningProcessPiped; overload;

  function RunProcess(Executable: String; Params: TStringArray): TProcessExitCode;
  function RunProcess(Executable: String; Params: TStringArray; out Output: String): TProcessExitCode;

  function RunProcessInDir(Directory, Executable: String; Params: TStringArray): TProcessExitCode; overload;
  function RunProcessInDir(Directory, Executable: String; Params: TStringArray; out Output: String): TProcessExitCode; overload;

  function RunProcessTimeout(Executable: String; Params: TStringArray; Timeout: Integer): Boolean; overload;
  function RunProcessTimeout(Executable: String; Params: TStringArray; Timeout: Integer; out Output: String): Boolean; overload;

  function IsProcess64Bit(PID: TProcessID): Boolean;
  function IsProcessRunning(PID: TProcessID): Boolean;
  function GetProcessPath(PID: TProcessID): String;
  function GetProcessMemUsage(PID: TProcessID): Int64;
  function GetProcessStartTime(PID: TProcessID): TDateTime;
  function GetProcessAge(PID: TProcessID): UInt64;
  procedure TerminateProcess(PID: TProcessID);

  function RunScript(Script: String; Params: TStringArray): Boolean; overload;
  function RunScript(Script: String; Params: TStringArray; out Output: String): Boolean; overload;

  function StartScript(Script: String; Params: TStringArray): TRunningProcess;
  function StartScriptPiped(Script: String; Params: TStringArray): TRunningProcessPiped;

implementation

uses
  Forms, DateUtils,
  simba.env, simba.nativeinterface, simba.vartype_string;

function IsProcess64Bit(PID: TProcessID): Boolean;
begin
  Result := SimbaNativeInterface.IsProcess64Bit(PID);
end;

function IsProcessRunning(PID: TProcessID): Boolean;
begin
  Result := SimbaNativeInterface.IsProcessRunning(PID);
end;

function GetProcessPath(PID: TProcessID): String;
begin
  Result := SimbaNativeInterface.GetProcessPath(PID);
end;

function GetProcessMemUsage(PID: TProcessID): Int64;
begin
  Result := SimbaNativeInterface.GetProcessMemUsage(PID);
end;

function GetProcessStartTime(PID: TProcessID): TDateTime;
begin
  Result := SimbaNativeInterface.GetProcessStartTime(PID);
end;

function GetProcessAge(PID: TProcessID): UInt64;
begin
  Result := MillisecondsBetween(GetProcessStartTime(PID), Now());
end;

procedure TerminateProcess(PID: TProcessID);
begin
  SimbaNativeInterface.TerminateProcess(PID);
end;

procedure CheckScriptParams(var Params: TStringArray);
var
  I: Integer;
begin
  for I := 0 to High(Params) do
    if (Params[I] <> '') then
    begin
      if (Length(Params[I].Split('=')) <> 2) then
        raise Exception.Create('RunScript: Invalid parameter "' + Params[I] + '". Expected "name=value"');

      while (not Params[I].StartsWith('--')) do
        Params[I] := '-' + Params[I]
    end;
end;

function RunScript(Script: String; Params: TStringArray): Boolean;
begin
  CheckScriptParams(Params);

  Result := RunProcessInDir(Application.Location, Application.ExeName, Params + ['--run', Script]) = 0;
end;

function RunScript(Script: String; Params: TStringArray; out Output: String): Boolean;
begin
  CheckScriptParams(Params);

  Result := RunProcessInDir(Application.Location, Application.ExeName, Params + ['--run', Script], Output) = 0;
end;

function StartScript(Script: String; Params: TStringArray): TRunningProcess;
begin
  Result := StartProcess(Application.ExeName, Params + ['--run', Script], Application.Location, []);
end;

function StartScriptPiped(Script: String; Params: TStringArray): TRunningProcessPiped;
begin
  Result := StartProcessPiped(Application.ExeName, Params + ['--run', Script], Application.Location, []);
end;

function TRunningProcess.GetRunning: Boolean;
begin
  Result := FProcess.Running;
end;

function TRunningProcess.GetExitCode: TProcessExitCode;
begin
  Result := FProcess.ExitCode;
end;

constructor TRunningProcess.Create(Process: TProcess);
begin
  inherited Create();

  FProcess := Process;
  FPID := FProcess.ProcessID;
end;

destructor TRunningProcess.Destroy;
begin
  FreeAndNil(FProcess);

  inherited Destroy();
end;

function TRunningProcess.WaitOnExit: Boolean;
begin
  Result := FProcess.WaitOnExit();
end;

function TRunningProcess.WaitOnExit(Timeout: Integer): Boolean;
begin
  Result := FProcess.WaitOnExit(Timeout);
end;

function TRunningProcess.Terminate(AExitCode: Integer): Boolean;
begin
  Result := FProcess.Terminate(AExitCode);
end;

procedure TRunningProcess.WaitUntilFinishedOrTimeout(Timeout: Integer);
var
  T: UInt64;
begin
  T := GetTickCount64() + Timeout;
  while ((Timeout = 0) or (T > GetTickCount64())) and (not WaitOnExit(1000)) do
    { nothing };
end;

function TRunningProcessPiped.GetReadBytesAvailable: Integer;
begin
  Result := FProcess.Output.NumBytesAvailable;
end;

function TRunningProcessPiped.Read(Buf: PByte; Count: Integer): Integer;
begin
  if (Count > 0) then
    Result := FProcess.Output.Read(Buf^, Count)
  else
    Result := 0;
end;

function TRunningProcessPiped.ReadString: String;

  function MaybeRead: String;
  begin
    SetLength(Result, 1024);
    SetLength(Result, FProcess.Output.Read(Result[1], 1024));
  end;

var
  Str: String;
begin
  Result := '';
  repeat
    Str := MaybeRead();
    Result := Result + Str;
  until (Length(Str) = 0);
end;

function TRunningProcessPiped.Write(Buf: PByte; Count: Integer): Integer;
begin
  if (Count > 0) then
    Result := FProcess.Input.Write(Buf^, Count)
  else
    Result := 0;
end;

function TRunningProcessPiped.WriteString(Str: String): Integer;
begin
  if (Length(Str) > 0) then
    Result := FProcess.Input.Write(Str[1], Length(Str))
  else
    Result := 0;
end;

function TRunningProcessPiped.WaitUntilFinishedOrTimeout(Timeout: Integer): String;
var
  T: UInt64;
begin
  T := GetTickCount64() + Timeout;
  Result := '';
  while ((Timeout = 0) or (T > GetTickCount64())) and (not WaitOnExit(1000)) do
    Result := Result + ReadString();
  Result := Result + ReadString();
end;

function StartProcess(Executable: String; Params: TStringArray): TRunningProcess;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poStderrToOutPut, poNoConsole];
  Proc.Executable := Executable;
  Proc.Parameters.AddStrings(Params);
  Proc.Execute();

  Result := TRunningProcess.Create(Proc);
end;

function StartProcess(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray): TRunningProcess;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poStderrToOutPut, poNoConsole];
  Proc.Executable := Executable;
  Proc.CurrentDirectory := Cwd;
  Proc.Environment.AddStrings(Env);
  Proc.Parameters.AddStrings(Params);
  Proc.Execute();

  Result := TRunningProcess.Create(Proc);
end;

function StartProcessPiped(Executable: String; Params: TStringArray): TRunningProcessPiped;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poStderrToOutPut, poUsePipes, poNoConsole];
  Proc.Executable := Executable;
  Proc.Parameters.AddStrings(Params);
  Proc.Execute();

  Result := TRunningProcessPiped.Create(Proc);
end;

function StartProcessPiped(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray): TRunningProcessPiped;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poStderrToOutPut, poUsePipes, poNoConsole];
  Proc.Executable := Executable;
  Proc.CurrentDirectory := Cwd;
  Proc.Environment.AddStrings(Env);
  Proc.Parameters.AddStrings(Params);
  Proc.Execute();

  Result := TRunningProcessPiped.Create(Proc);
end;

function RunProcess(Executable: String; Params: TStringArray): TProcessExitCode;
var
  Proc: TRunningProcess;
begin
  Proc := StartProcess(Executable, Params);
  Proc.WaitOnExit();

  Result := Proc.ExitCode;

  Proc.Free();
end;

function RunProcess(Executable: String; Params: TStringArray; out Output: String): TProcessExitCode;
var
  Proc: TRunningProcessPiped;
begin
  Proc := StartProcessPiped(Executable, Params);
  Output := Proc.WaitUntilFinishedOrTimeout();
  Result := Proc.ExitCode;

  Proc.Free();
end;

function RunProcessInDir(Directory, Executable: String; Params: TStringArray): TProcessExitCode;
var
  Proc: TRunningProcess;
begin
  Proc := StartProcess(Executable, Params, Directory, []);
  Proc.WaitOnExit();

  Result := Proc.ExitCode;

  Proc.Free();
end;

function RunProcessInDir(Directory, Executable: String; Params: TStringArray; out Output: String): TProcessExitCode;
var
  Proc: TRunningProcessPiped;
begin
  Proc := StartProcessPiped(Executable, Params, Directory, []);
  Output := Proc.WaitUntilFinishedOrTimeout();
  Result := Proc.ExitCode;

  Proc.Free();
end;

function RunProcessTimeout(Executable: String; Params: TStringArray; Timeout: Integer): Boolean;
var
  Proc: TRunningProcess;
begin
  Proc := StartProcess(Executable, Params);
  Proc.WaitUntilFinishedOrTimeout(Timeout);

  Result := (not Proc.Running) and (Proc.ExitCode = 0);
  if Proc.Running then
    Proc.Terminate(255);

  Proc.Free();
end;

function RunProcessTimeout(Executable: String; Params: TStringArray; Timeout: Integer; out Output: String): Boolean;
var
  Proc: TRunningProcessPiped;
begin
  Proc := StartProcessPiped(Executable, Params);
  Output := Proc.WaitUntilFinishedOrTimeout(Timeout);
  Result := (not Proc.Running) and (Proc.ExitCode = 0);
  if Proc.Running then
    Proc.Terminate(255);

  Proc.Free();
end;

end.


