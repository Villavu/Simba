unit simba.import_process;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.process;

procedure _LapeRunCommandInDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure _LapeRunCommandInDirOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeRunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRunCommandOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeRunCommandTimeout(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.RunCommandTimeout(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeGetScriptParameters(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := SimbaProcess.GetScriptParameters();
end;

procedure _LapeGetScriptParameter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetScriptParameter(PString(Params^[0])^);
end;

procedure _LapeIsProcessRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcessRunning(PProcessID(Params^[0])^);
end;

procedure _LapeIsProcess64Bit(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcess64Bit(PProcessID(Params^[0])^);
end;

procedure _LapeGetProcessPath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetProcessPath(PProcessID(Params^[0])^);
end;

procedure _LapeTerminateProcess(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaProcess.TerminateProcess(PProcessID(Params^[0])^);
end;

procedure _LapeRunScript(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeRunScriptEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeRunScriptOutputToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScriptOutputToFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeGetScriptPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := GetProcessID();
end;

procedure ImportProcess(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Process');

    addGlobalType('type SizeUInt', 'TProcessID');
    addGlobalType('type Integer', 'TProcessExitStatus');
    addGlobalFunc('function GetScriptPID: TProcessID', @_LapeGetScriptPID);
    addGlobalFunc('function GetScriptParameters: TStringArray', @_LapeGetScriptParameters);
    addGlobalFunc('function GetScriptParameter(Name: String): String', @_LapeGetScriptParameter);
    addGlobalFunc('function IsProcessRunning(PID: TProcessID): Boolean', @_LapeIsProcessRunning);
    addGlobalFunc('function IsProcess64Bit(PID: TProcessID): Boolean', @_LapeIsProcess64Bit);
    addGlobalFunc('function GetProcessPath(PID: TProcessID): String', @_LapeGetProcessPath);
    addGlobalFunc('procedure TerminateProcess(PID: TProcessID)', @_LapeTerminateProcess);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandInDirOutput);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommandInDir);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandOutput);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommand);
    addGlobalFunc('function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean', @_LapeRunCommandTimeout);
    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunScript);
    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray): TProcessID; overload', @_LapeRunScriptEx);
    addGlobalFunc('function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID', @_LapeRunScriptOutputToFile);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportProcess);

end.

