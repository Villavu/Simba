unit simbascript.import_process;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Process(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.misc;

procedure Lape_RunCommandInDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RunCommandInDir(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_RunCommandInDirEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RunCommandInDir(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_RunCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RunCommand(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_RunCommandEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  RunCommand(PString(Params^[1])^);
end;

procedure Lape_GetCommandLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetCommandLine();
end;

procedure Lape_Import_Process(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Process';

    addGlobalMethod('function GetCommandLine: TStringArray;', @Lape_GetCommandLine, Data);

    addGlobalMethod('function RunCommandInDir(CurrentDirectory: String; CommandLine: String; out Output: String): Int32; overload;', @Lape_RunCommandInDir, Data);
    addGlobalMethod('procedure RunCommandInDir(CurrentDirectory: String; CommandLine: String); overload;', @Lape_RunCommandInDirEx, Data);

    addGlobalMethod('function RunCommand(CommandLine: String; out Output: String): Int32; overload;', @Lape_RunCommand, Data);
    addGlobalMethod('procedure RunCommand(CommandLine: String); overload;', @Lape_RunCommandEx, Data);
  end;
end;

end.

