unit simba.import_file;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.files;

procedure _LapeWriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  WriteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := ReadINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeDeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DeleteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[1])^);
end;

procedure _LapeUnZipFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UnZipFile(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeUnZipOneFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := UnZipOneFile(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeZipFiles(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure ImportFile(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'File';

    addGlobalVar(GetIncludePath(), 'IncludePath').isConstant := True;
    addGlobalVar(GetPluginPath(), 'PluginPath').isConstant := True;
    addGlobalVar(GetSimbaPath(), 'SimbaPath').isConstant := True;
    addGlobalVar(GetScriptPath(), 'ScriptPath').isConstant := True;
    addGlobalVar(GetDataPath(), 'DataPath').isConstant := True;
    addGlobalVar(GetScreenshotPath(), 'ScreenshotPath').isConstant := True;

    addGlobalFunc('procedure WriteINI(Section, KeyName, NewString, FileName: String)', @_LapeWriteINI);
    addGlobalFunc('function ReadINI(Section, KeyName, FileName: String): String', @_LapeReadINI);
    addGlobalFunc('procedure DeleteINI(Section, KeyName, FileName: String)', @_LapeDeleteINI);
    addGlobalFunc('procedure ZipFiles(ArchiveFileName: String; const Files: TStringArray)', @_LapeZipFiles);
    addGlobalFunc('procedure UnZipFile(ArchiveFileName, OutputDirectory: String)', @_LapeUnZipFile);
    addGlobalFunc('function UnZipOneFile(ArchiveFileName, FileName, OutputDirectory: String): Boolean', @_LapeUnZipOneFile);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFile);

end.

