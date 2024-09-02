unit simba.import_process;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms,
  simba.base, simba.script_compiler;

procedure ImportProcess(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.process;

type
  PProcessID = ^TProcessID;
  PProcessExitCode = ^TProcessExitCode;
  PRunningProcess = ^TRunningProcess;
  PRunningProcessPiped = ^TRunningProcessPiped;

(*
Process
=======
Process related things.
*)

(*
GetProcessID
------------
```
function GetProcessID: TProcessID;
```

Returns the process ID of the running script.
*)
procedure _LapeGetProcessID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := GetProcessID();
end;

(*
GetProcessArgs
--------------
```
function GetProcessArgs: TStringArray;
```

Returns all process arguments.
*)
procedure _LapeGetProcessArgs(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function _GetProcessArgs: TStringArray;
  var
    I: Integer;
  begin
    SetLength(Result, Application.ParamCount);
    for I := 0 to High(Result) do
      Result[I] := Application.Params[I];
  end;

begin
  PStringArray(Result)^ := _GetProcessArgs();
end;

(*
GetProcessArg
-------------
```
function GetProcessArg(Key: String): String;
```

Argument should be passed as a key-pair value: `Name=Value`
*)
procedure _LapeGetProcessArg(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Application.GetOptionValue(PString(Params^[0])^);
end;

(*
IsProcessRunning
----------------
```
function IsProcessRunning(PID: TProcessID): Boolean;
```
*)
procedure _LapeIsProcessRunning(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := IsProcessRunning(PProcessID(Params^[0])^);
end;

(*
IsProcess64Bit
--------------
```
function IsProcess64Bit(PID: TProcessID): Boolean;
```
*)
procedure _LapeIsProcess64Bit(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := IsProcess64Bit(PProcessID(Params^[0])^);
end;

(*
GetProcessPath
--------------
```
function GetProcessPath(PID: TProcessID): String;
```
*)
procedure _LapeGetProcessPath(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetProcessPath(PProcessID(Params^[0])^);
end;

(*
GetProcessMemUsage
------------------
```
function GetProcessMemUsage(PID: TProcessID): Int64;
```
*)
procedure _LapeGetProcessMemUsage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := GetProcessMemUsage(PProcessID(Params^[0])^);
end;

(*
GetProcessStartTime
-------------------
```
function GetProcessStartTime(PID: TProcessID): TDateTime;
```
*)
procedure _LapeGetProcessStartTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := GetProcessStartTime(PProcessID(Params^[0])^);
end;

(*
GetProcessAge
-------------
```
function GetProcessAge(PID: TProcessID): UInt64;
```
*)
procedure _LapeGetProcessAge(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := GetProcessAge(PProcessID(Params^[0])^);
end;

(*
TerminateProcess
----------------
```
procedure TerminateProcess(PID: TProcessID);
```
*)
procedure _LapeTerminateProcess(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TerminateProcess(PProcessID(Params^[0])^);
end;

(*
GetEnvVar
---------
```
function GetEnvVar(Name: String): String
```
*)
procedure _LapeGetEnvVar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetEnvironmentVariable(PString(Params^[0])^);
end;

(*
GetEnvVars
----------
```
function GetEnvVars: TStringArray
```
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

(*
TRunningProcess.Free
--------------------
*)
procedure _LapeRunningProcess_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcess(Params^[0])^.Free();
end;

(*
TRunningProcess.WaitOnExit
--------------------------
```
function WaitOnExit: Boolean; overload;
```
```
function WaitOnExit(Timeout: Integer): Boolean; overload;
```
*)
procedure _LapeRunningProcess_WaitOnExit1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PRunningProcess(Params^[0])^.WaitOnExit();
end;

procedure _LapeRunningProcess_WaitOnExit2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PRunningProcess(Params^[0])^.WaitOnExit(PInteger(Params^[1])^);
end;

(*
TRunningProcess.Terminate
-------------------------
```
function Terminate(AExitCode: Integer): Boolean;
```
*)
procedure _LapeRunningProcess_Terminate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PRunningProcess(Params^[0])^.Terminate(PInteger(Params^[1])^);
end;

(*
TRunningProcess.Running
------------------------
```
property TRunningProcess.Running: Boolean
```
*)
procedure _LapeRunningProcess_Running_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PRunningProcess(Params^[0])^.Running;
end;

(*
TRunningProcess.PID
-------------------
```
property TRunningProcess.PID: TProcessID
```
*)
procedure _LapeRunningProcess_PID_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessID(Result)^ := PRunningProcess(Params^[0])^.PID;
end;

(*
TRunningProcess.ExitCode
------------------------
```
property TRunningProcess.ExitCode: TProcessExitCode
```
*)
procedure _LapeRunningProcess_ExitCode_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessExitCode(Result)^ := PRunningProcess(Params^[0])^.ExitCode;
end;

(*
TRunningProcessPiped.Read
-------------------------
```
function TRunningProcessPiped.Read(Buf: PByte; Count: Integer): Integer;
```
*)
procedure _LapeRunningProcessPiped_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PRunningProcessPiped(Params^[0])^.Read(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TRunningProcessPiped.ReadString
-------------------------------
```
function TRunningProcessPiped.ReadString: String;
```
*)
procedure _LapeRunningProcessPiped_ReadString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PRunningProcessPiped(Params^[0])^.ReadString();
end;

(*
TRunningProcessPiped.Write
--------------------------
```
function TRunningProcessPiped.Write(Buf: PByte; Count: Integer): Integer;
```
*)
procedure _LapeRunningProcessPiped_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PRunningProcessPiped(Params^[0])^.Write(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TRunningProcessPiped.WriteString
--------------------------------
```
function TRunningProcessPiped.WriteString(Str: String): Integer;
```
*)
procedure _LapeRunningProcessPiped_WriteString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PRunningProcessPiped(Params^[0])^.WriteString(PString(Params^[1])^);
end;

(*
TRunningProcessPiped.ReadBytesAvailable
---------------------------------------
```
property TRunningProcessPiped.ReadBytesAvailable: Integer
```
*)
procedure _LapeRunningProcessPiped_ReadBytesAvailable_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PRunningProcessPiped(Params^[0])^.ReadBytesAvailable;
end;

(*
StartProcess
------------
```
function StartProcess(Executable: String; Params: TStringArray): TRunningProcess;
```
```
function StartProcess(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray = []): TRunningProcess;
```
*)
procedure _LapeStartProcess1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcess(Result)^ := StartProcess(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeStartProcess2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcess(Result)^ := StartProcess(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PStringArray(Params^[3])^);
end;

(*
StartProcessPiped
-----------------
```
function StartProcessPiped(Executable: String; Params: TStringArray): TRunningProcessPiped;
```
```
function StartProcessPiped(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray = []): TRunningProcessPiped;
```
*)
procedure _LapeStartProcessPiped1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcessPiped(Result)^ := StartProcessPiped(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeStartProcessPiped2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcessPiped(Result)^ := StartProcessPiped(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PStringArray(Params^[3])^);
end;

(*
RunProcess
------------
```
function RunProcess(Executable: String; Params: TStringArray): TProcessExitCode;
```
```
function RunProcess(Executable: String; Params: TStringArray; out Output: String): TProcessExitCode;
```
*)
procedure _LapeRunProcess1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := RunProcess(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRunProcess2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := RunProcess(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

(*
RunSimbaScript
--------------
```
function RunSimbaScript(Script: String; Parameters: TStringArray): TProcessExitStatus;
```
```
function RunSimbaScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus;
```

Runs a simba script and **will wait** until the script has finished.
*)
procedure _LapeRunSimbaScript1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRunSimbaScript2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := RunScript(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

(*
StartSimbaScript
----------------
```
function StartSimbaScript(Script: String; Params: TStringArray): TRunningProcess;
```

Start a simba script.
*)
procedure _LapeStartSimbaScript(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcess(Result)^ := StartScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

(*
StartSimbaScriptPiped
---------------------
```
function StartSimbaScriptPiped(Script: String; Params: TStringArray): TRunningProcessPiped;
```

Start a simba script (with piped input & output)
*)
procedure _LapeStartSimbaScriptPiped(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRunningProcessPiped(Result)^ := StartScriptPiped(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure ImportProcess(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Process';

    addGlobalType('type Integer', 'TProcessID');
    addGlobalType('type Integer', 'TProcessExitCode');

    addGlobalFunc('function GetProcessID: TProcessID', @_LapeGetProcessID);
    addGlobalFunc('function GetProcessArgs: TStringArray', @_LapeGetProcessArgs);
    addGlobalFunc('function GetProcessArg(Key: String): String', @_LapeGetProcessArg);

    addGlobalFunc('function IsProcessRunning(PID: TProcessID): Boolean', @_LapeIsProcessRunning);
    addGlobalFunc('function IsProcess64Bit(PID: TProcessID): Boolean', @_LapeIsProcess64Bit);
    addGlobalFunc('function GetProcessPath(PID: TProcessID): String', @_LapeGetProcessPath);
    addGlobalFunc('function GetProcessMemUsage(PID: TProcessID): Int64', @_LapeGetProcessMemUsage);
    addGlobalFunc('function GetProcessStartTime(PID: TProcessID): TDateTime', @_LapeGetProcessStartTime);
    addGlobalFunc('function GetProcessAge(PID: TProcessID): UInt64', @_LapeGetProcessAge);
    addGlobalFunc('procedure TerminateProcess(PID: TProcessID)', @_LapeTerminateProcess);

    addGlobalFunc('function GetEnvVar(Name: String): String', @_LapeGetEnvVar);
    addGlobalFunc('function GetEnvVars: TStringArray', @_LapeGetEnvVars);

    addClass('TRunningProcess', 'TBaseClass');
    addGlobalFunc('procedure TRunningProcess.Free;', @_LapeRunningProcess_Free);
    addGlobalFunc('function TRunningProcess.WaitOnExit: Boolean; overload;', @_LapeRunningProcess_WaitOnExit1);
    addGlobalFunc('function TRunningProcess.WaitOnExit(Timeout: Integer): Boolean; overload;', @_LapeRunningProcess_WaitOnExit2);
    addGlobalFunc('function TRunningProcess.Terminate(AExitCode: Integer): Boolean;', @_LapeRunningProcess_Terminate);
    addProperty('TRunningProcess', 'Running', 'Boolean', @_LapeRunningProcess_Running_Read);
    addProperty('TRunningProcess', 'PID', 'TProcessID', @_LapeRunningProcess_PID_Read);
    addProperty('TRunningProcess', 'ExitCode', 'TProcessExitCode', @_LapeRunningProcess_ExitCode_Read);

    addClass('TRunningProcessPiped', 'TRunningProcess');
    addGlobalFunc('function TRunningProcess.Read(Buf: Pointer; Count: Integer): Integer;', @_LapeRunningProcessPiped_Read);
    addGlobalFunc('function TRunningProcess.ReadString: String;', @_LapeRunningProcessPiped_ReadString);
    addGlobalFunc('function TRunningProcess.Write(Buf: Pointer; Count: Integer): Integer;', @_LapeRunningProcessPiped_Write);
    addGlobalFunc('function TRunningProcess.WriteString(Str: String): Integer;', @_LapeRunningProcessPiped_WriteString);
    addProperty('TRunningProcessPiped', 'ReadBytesAvailable', 'Integer', @_LapeRunningProcessPiped_ReadBytesAvailable_Read);

    addGlobalFunc('function StartProcess(Executable: String; Params: TStringArray): TRunningProcess; overload', @_LapeStartProcess1);
    addGlobalFunc('function StartProcess(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray = []): TRunningProcess; overload', @_LapeStartProcess2);
    addGlobalFunc('function StartProcessPiped(Executable: String; Params: TStringArray): TRunningProcessPiped; overload', @_LapeStartProcessPiped1);
    addGlobalFunc('function StartProcessPiped(Executable: String; Params: TStringArray; Cwd: String; Env: TStringArray = []): TRunningProcessPiped; overload', @_LapeStartProcessPiped2);

    addGlobalFunc('function RunProcess(Executable: String; Params: TStringArray): TProcessExitCode; overload', @_LapeRunProcess1);
    addGlobalFunc('function RunProcess(Executable: String; Params: TStringArray; out Output: String): TProcessExitCode; overload', @_LapeRunProcess2);

    addGlobalFunc('function RunSimbaScript(Script: String; Params: TStringArray): Boolean; overload', @_LapeRunSimbaScript1);
    addGlobalFunc('function RunSimbaScript(Script: String; Params: TStringArray; out Output: String): Boolean; overload', @_LapeRunSimbaScript2);

    addGlobalFunc('function StartSimbaScript(Script: String; Params: TStringArray): TRunningProcess', @_LapeStartSimbaScript);
    addGlobalFunc('function StartSimbaScriptPiped(Script: String; Params: TStringArray): TRunningProcessPiped', @_LapeStartSimbaScriptPiped);

    ImportingSection := '';
  end;
end;

end.
