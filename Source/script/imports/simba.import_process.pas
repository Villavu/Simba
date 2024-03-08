unit simba.import_process;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportProcess(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.process;

(*
Process
=======
Process related things.
*)

(*
GetScriptPID
------------
> function GetScriptPID: TProcessID;

Returns the process ID of the running script.
*)
procedure _LapeGetScriptPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := GetProcessID();
end;

(*
GetScriptParameters
-------------------
> function GetScriptParameters: TStringArray;

Returns all command line parameters passed to the script.
*)
procedure _LapeGetScriptParameters(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := SimbaProcess.GetScriptParameters();
end;

(*
GetScriptParameter
------------------
> function GetScriptParameter(Name: String): String;

Returns a command line parameter value passed to the script.
Parameters should be passed as a key-pair value: `Name=Value`
*)
procedure _LapeGetScriptParameter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SimbaProcess.GetScriptParameter(PString(Params^[0])^);
end;

(*
RunScript
---------
> function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus;

Runs a simba script and **will wait** until the script has finished.

 - The script output will be returned in the `Output` parameter.
 - Returns the exit status of the scripts process.
*)
procedure _LapeRunScript(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

(*
RunScript
---------
> function RunScript(Script: String; Parameters: TStringArray): TProcessID;

Runs a simba script and instantly returns the scripts PID.

- The script output will be printed normally.
- The script PID can be used with process methods.

Example:

```
  PID := RunScript('script.simba', []);
  while IsProcessRunning(PID) do
    Sleep(100);
  WriteLn('Script finished!');
```
*)
procedure _LapeRunScriptEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

(*
RunScriptOutputToFile
---------------------
> function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID;

- The script output will be redirected to the file `OutputFileName`
- The script PID can be used with process methods.

Example:

```
  PID := RunScriptOutputToFile('script.simba', [], 'output.txt');
  while IsProcessRunning(PID) do
    Sleep(100);
  WriteLn('Script finished!');
```
*)
procedure _LapeRunScriptOutputToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := SimbaProcess.RunScriptOutputToFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

(*
RunCommandInDir
---------------
> function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID;
*)
procedure _LapeRunCommandInDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

(*
RunCommandInDir
---------------
> function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus;
*)
procedure _LapeRunCommandInDirOutput(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, PString(Params^[3])^);
end;

(*
RunCommand
----------
> function RunCommand(Executable: String; Commands: TStringArray): TProcessID;
*)
procedure _LapeRunCommand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

(*
RunCommand
----------
> function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus;
*)
procedure _LapeRunCommandOutput(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

(*
RunCommandTimeout
-----------------
> function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Integer): Boolean;
*)
procedure _LapeRunCommandTimeout(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaProcess.RunCommandTimeout(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

(*
IsProcessRunning
----------------
> function IsProcessRunning(PID: TProcessID): Boolean;
*)
procedure _LapeIsProcessRunning(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaProcess.IsProcessRunning(PProcessID(Params^[0])^);
end;

(*
IsProcess64Bit
--------------
> function IsProcess64Bit(PID: TProcessID): Boolean;
*)
procedure _LapeIsProcess64Bit(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaProcess.IsProcess64Bit(PProcessID(Params^[0])^);
end;

(*
GetProcessPath
--------------
> function GetProcessPath(PID: TProcessID): String;
*)
procedure _LapeGetProcessPath(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SimbaProcess.GetProcessPath(PProcessID(Params^[0])^);
end;

(*
GetProcessMemUsage
------------------
> function GetProcessMemUsage(PID: TProcessID): Int64;
*)
procedure _LapeGetProcessMemUsage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := SimbaProcess.GetProcessMemUsage(PProcessID(Params^[0])^);
end;

(*
GetProcessStartTime
-------------------
> function GetProcessStartTime(PID: TProcessID): TDateTime;
*)
procedure _LapeGetProcessStartTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := SimbaProcess.GetProcessStartTime(PProcessID(Params^[0])^);
end;

(*
GetProcessRunnningTime
----------------------
> function GetProcessRunnningTime(PID: TProcessID): UInt64;
*)
procedure _LapeGetProcessRunningTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := SimbaProcess.GetProcessRunnningTime(PProcessID(Params^[0])^);
end;

(*
TerminateProcess
----------------
> procedure TerminateProcess(PID: TProcessID);
*)
procedure _LapeTerminateProcess(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaProcess.TerminateProcess(PProcessID(Params^[0])^);
end;

(*
GetEnvVar
---------
> function GetEnvVar(Name: String): String
*)
procedure _LapeGetEnvVar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetEnvironmentVariable(PString(Params^[0])^);
end;

(*
GetEnvVars
----------
> function GetEnvVars: TStringArray
*)
procedure _LapeGetEnvVars(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function _GetEnvVars: TStringArray;
  var
    Count, I: Integer;
  begin
    Count := 0;

    SetLength(Result, GetEnvironmentVariableCount() + 1);
    for I := 1 to GetEnvironmentVariableCount() do
      if (GetEnvironmentString(I) <> '') then
      begin
        Result[Count] := GetEnvironmentString(I);
        Inc(Count);
      end;
    SetLength(Result, Count);
  end;

begin
  PStringArray(Result)^ := _GetEnvVars();
end;

procedure ImportProcess(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Process';

    addGlobalType('type Integer', 'TProcessID');
    addGlobalType('type Integer', 'TProcessExitStatus');

    addGlobalFunc('function GetProcessID: TProcessID', @_LapeGetScriptPID);
    addGlobalFunc('function GetProcessParameters: TStringArray', @_LapeGetScriptParameters);
    addGlobalFunc('function GetProcessParameter(Name: String): String', @_LapeGetScriptParameter);

    addGlobalFunc('function IsProcessRunning(PID: TProcessID): Boolean', @_LapeIsProcessRunning);
    addGlobalFunc('function IsProcess64Bit(PID: TProcessID): Boolean', @_LapeIsProcess64Bit);
    addGlobalFunc('function GetProcessPath(PID: TProcessID): String', @_LapeGetProcessPath);
    addGlobalFunc('function GetProcessMemUsage(PID: TProcessID): Int64', @_LapeGetProcessMemUsage);
    addGlobalFunc('function GetProcessStartTime(PID: TProcessID): TDateTime', @_LapeGetProcessStartTime);
    addGlobalFunc('function GetProcessRunnningTime(PID: TProcessID): UInt64', @_LapeGetProcessRunningTime);
    addGlobalFunc('procedure TerminateProcess(PID: TProcessID)', @_LapeTerminateProcess);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandInDirOutput);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommandInDir);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandOutput);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommand);
    addGlobalFunc('function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Integer): Boolean', @_LapeRunCommandTimeout);

    addGlobalFunc('function GetEnvVar(Name: String): String', @_LapeGetEnvVar);
    addGlobalFunc('function GetEnvVars: TStringArray', @_LapeGetEnvVars);

    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunScript);
    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray): TProcessID; overload', @_LapeRunScriptEx);
    addGlobalFunc('function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID', @_LapeRunScriptOutputToFile);

    ImportingSection := '';
  end;
end;

end.
