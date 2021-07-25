unit simba.script_import_process;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Process(Compiler: TSimbaScript_Compiler);

implementation

uses
  simba.process;

// function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): Boolean; overload;
procedure Lape_RunCommandInDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

// function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): Int32; overload;
procedure Lape_RunCommandInDirOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, PString(Params^[3])^);
end;

// function RunCommand(Executable: String; Commands: TStringArray; out Output: String): Int32; overload;
procedure Lape_RunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

// function RunCommand(Executable: String; Commands: TStringArray): Boolean; overload;
procedure Lape_RunCommandOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

// function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;
procedure Lape_RunCommandTimeout(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.RunCommandTimeout(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

// function GetProcessParameters: TStringArray;
procedure Lape_GetScriptParameters(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := SimbaProcess.GetScriptParameters();
end;

// function GetProcessParameter(Name: String): String;
procedure Lape_GetScriptParameter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetScriptParameter(PString(Params^[0])^);
end;

// function IsProcessRunning(PID: UInt32): Boolean;
procedure Lape_IsProcessRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcessRunning(PProcessID(Params^[0])^);
end;

procedure Lape_IsProcess64Bit(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcess64Bit(PProcessID(Params^[0])^);
end;

procedure Lape_GetProcessPath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetProcessPath(PProcessID(Params^[0])^);
end;

procedure Lape_TerminateProcess(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaProcess.TerminateProcess(PProcessID(Params^[0])^);
end;

// procedure RunScript(Script: String; Parameters: TStringArray);
procedure Lape_RunScript(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

// procedure RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String);
procedure Lape_RunScriptOutputToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScriptOutputToFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

// function GetProcessID: TProcessID;
procedure Lape_GetScriptPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := GetProcessID();
end;

procedure Lape_Import_Process(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'Process';

    addGlobalType('type SizeUInt', 'TProcessID');
    addGlobalType('type Integer', 'TProcessExitStatus');

    addGlobalFunc('function GetScriptPID: TProcessID;', @Lape_GetScriptPID);
    addGlobalFunc('function GetScriptParameters: TStringArray;', @Lape_GetScriptParameters);
    addGlobalFunc('function GetScriptParameter(Name: String): String;', @Lape_GetScriptParameter);

    addGlobalFunc('function IsProcessRunning(PID: TProcessID): Boolean;', @Lape_IsProcessRunning);
    addGlobalFunc('function IsProcess64Bit(PID: TProcessID): Boolean;', @Lape_IsProcess64Bit);
    addGlobalFunc('function GetProcessPath(PID: TProcessID): String;', @Lape_GetProcessPath);
    addGlobalFunc('procedure TerminateProcess(PID: TProcessID);', @Lape_TerminateProcess);

    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload;', @Lape_RunCommandInDirOutput);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload;', @Lape_RunCommand);

    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload;', @Lape_RunCommandOutput);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload;', @Lape_RunCommand);

    addGlobalFunc('function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;', @Lape_RunCommandTimeout);

    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray): TProcessID;', @Lape_RunScript);
    addGlobalFunc('function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID;', @Lape_RunScriptOutputToFile);
  end;
end;

end.


