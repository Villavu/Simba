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

procedure _LapeGetProcessMemUsage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := SimbaProcess.GetProcessMemUsage(PProcessID(Params^[0])^);
end;

procedure _LapeTerminateProcess(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaProcess.TerminateProcess(PProcessID(Params^[0])^);
end;

procedure ImportProcess(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Process';

    addGlobalType('type SizeUInt', 'TProcessID');
    addGlobalType('type Integer', 'TProcessExitStatus');
    addGlobalFunc('function IsProcessRunning(PID: TProcessID): Boolean', @_LapeIsProcessRunning);
    addGlobalFunc('function IsProcess64Bit(PID: TProcessID): Boolean', @_LapeIsProcess64Bit);
    addGlobalFunc('function GetProcessPath(PID: TProcessID): String', @_LapeGetProcessPath);
    addGlobalFunc('function GetProcessMemUsage(PID: TProcessID): Int64', @_LapeGetProcessMemUsage);
    addGlobalFunc('procedure TerminateProcess(PID: TProcessID)', @_LapeTerminateProcess);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandInDirOutput);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommandInDir);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandOutput);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommand);
    addGlobalFunc('function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Integer): Boolean', @_LapeRunCommandTimeout);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportProcess);

end.

