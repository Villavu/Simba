unit simbascript.import_process;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

implementation

uses
  simba.misc;

procedure Lape_RunCommandInDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RunCommand(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_RunCommandInDirEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RunCommand(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_RunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RunCommand(GetCurrentDir(), PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_RunCommandEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RunCommand(GetCurrentDir(), PString(Params^[0])^);
end;

procedure Lape_GetCommandLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetCommandLine();
end;

procedure Lape_Import_Proccess(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    Section := 'Process';

    addGlobalFunc('function GetCommandLine: TStringArray;', @Lape_GetCommandLine);

    addGlobalFunc('function RunCommandInDir(CurrentDirectory: String; CommandLine: String; out Output: String): Int32; overload;', @Lape_RunCommandInDir);
    addGlobalFunc('procedure RunCommandInDir(CurrentDirectory: String; CommandLine: String); overload;', @Lape_RunCommandInDirEx);

    addGlobalFunc('function RunCommand(CommandLine: String; out Output: String): Int32; overload;', @Lape_RunCommand);
    addGlobalFunc('procedure RunCommand(CommandLine: String); overload;', @Lape_RunCommandEx);
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_Proccess);

end.

