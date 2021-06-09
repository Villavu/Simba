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
  PBoolean(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

// function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): Int32; overload;
procedure Lape_RunCommandInDirOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaProcess.RunCommandInDir(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, PString(Params^[3])^);
end;

// function RunCommand(Executable: String; Commands: TStringArray; out Output: String): Int32; overload;
procedure Lape_RunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

// function RunCommand(Executable: String; Commands: TStringArray): Boolean; overload;
procedure Lape_RunCommandOutput(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaProcess.RunCommand(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

// function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;
procedure Lape_RunCommandTimeout(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.RunCommandTimeout(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

// function GetProcessParameters: TStringArray;
procedure Lape_GetProcessParameters(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := SimbaProcess.GetProcessParameters();
end;

// function GetProcessParameter(Name: String): String;
procedure Lape_GetProcessParameter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetProcessParameter(PString(Params^[0])^);
end;

// function IsProcessRunning(PID: UInt32): Boolean;
procedure Lape_IsProcessRunning(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaProcess.IsProcessRunning(PSizeUInt(Params^[0])^);
end;

// procedure RunScript(Script: String; Parameters: TStringArray);
procedure Lape_RunScript(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

// procedure RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String);
procedure Lape_RunScriptOutputToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaProcess.RunScriptOutputToFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

// function GetProcessID: SizeUInt;
procedure Lape_GetProcessID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeUInt(Result)^ := GetProcessID();
end;

procedure Lape_Import_Process(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'Process';

    addGlobalFunc('function GetProcessID: SizeUInt;', @Lape_GetProcessID);
    addGlobalFunc('function GetProcessParameters: TStringArray;', @Lape_GetProcessParameters);
    addGlobalFunc('function GetProcessParameter(Name: String): String;', @Lape_GetProcessParameter);

    addGlobalFunc('function IsProcessRunning(PID: SizeUInt): Boolean;', @Lape_IsProcessRunning);

    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): Int32; overload;', @Lape_RunCommandInDirOutput);
    addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): Boolean; overload;', @Lape_RunCommand);

    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray; out Output: String): Int32; overload;', @Lape_RunCommandOutput);
    addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray): Boolean; overload;', @Lape_RunCommand);

    addGlobalFunc('function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean;', @Lape_RunCommandTimeout);

    addGlobalFunc('procedure RunScript(Script: String; Parameters: TStringArray);', @Lape_RunScript);
    addGlobalFunc('procedure RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String);', @Lape_RunScriptOutputToFile);
  end;
end;

end.


