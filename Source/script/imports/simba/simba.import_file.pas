unit simba.import_file;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, fileutil, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.files;

procedure _LapeCreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.CreateFile(PString(Params^[0])^);
end;

procedure _LapeOpenFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.OpenFile(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeRewriteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.RewriteFile(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeAppendFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.AppendFile(PString(Params^[0])^);
end;

procedure _LapeCloseFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFiles.CloseFile(PInt32(Params^[0])^);
end;

procedure _LapeEndOfFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFiles.EndOfFile(PInt32(Params^[0])^);
end;

procedure _LapeFileSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.FileSizeMuf(PInt32(Params^[0])^);
end;

procedure _LapeReadFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFiles.ReadFileString(PInt32(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeWriteFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PBoolean(Result)^ := MFiles.WriteFileString(PInt32(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeSetFileCharPointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.SetFileCharPointer(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeFilePointerPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MFiles.FilePointerPos(PInt32(Params^[0])^);
end;

procedure _LapeGetFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetFiles(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeGetDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetDirectories(PString(Params^[0])^);
end;

procedure _LapeWriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFiles.WriteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PString(Result)^ := MFiles.ReadINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeDeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    MFiles.DeleteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
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

procedure _LapeFindFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  List: TStringList;
  Files: ^TStringArray absolute Result;
  i: Int32;
begin
  List := TStringList.Create();

  try
    with TListFileSearcher.Create(List) do
    try
      Search(PString(Params^[0])^, PString(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
    finally
      Free();
    end;

    SetLength(Files^, List.Count);
    for i := 0 to List.Count - 1 do
      Files^[i] := List[i];
  finally
    List.Free();
  end;
end;

procedure ImportFile(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('File');

    addGlobalFunc('function CreateFile(Path: String): Integer', @_LapeCreateFile);
    addGlobalFunc('function OpenFile(Path: String; Shared: Boolean): Integer', @_LapeOpenFile);
    addGlobalFunc('function RewriteFile(Path: String; Shared: Boolean): Integer', @_LapeRewriteFile);
    addGlobalFunc('function AppendFile(Path: String): Integer', @_LapeAppendFile);
    addGlobalFunc('procedure CloseFile(FileNum: Integer)', @_LapeCloseFile);
    addGlobalFunc('function EndOfFile(FileNum: Integer): Boolean', @_LapeEndOfFile);
    addGlobalFunc('function FileSize(FileNum: Integer): Integer', @_LapeFileSize);
    addGlobalFunc('function ReadFileString(FileNum: Integer; var s: String; x: Integer): Boolean', @_LapeReadFileString);
    addGlobalFunc('function WriteFileString(FileNum: Integer; s: String): Boolean', @_LapeWriteFileString);
    addGlobalFunc('function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer', @_LapeSetFileCharPointer);
    addGlobalFunc('function FilePointerPos(FileNum: Integer): Integer', @_LapeFilePointerPos);
    addGlobalFunc('function GetFiles(Path, Ext: String): TStringArray', @_LapeGetFiles);
    addGlobalFunc('function GetDirectories(Path: String): TStringArray', @_LapeGetDirectories);
    addGlobalFunc('procedure WriteINI(Section, KeyName, NewString, FileName: String)', @_LapeWriteINI);
    addGlobalFunc('function ReadINI(Section, KeyName, FileName: String): String', @_LapeReadINI);
    addGlobalFunc('procedure DeleteINI(Section, KeyName, FileName: String)', @_LapeDeleteINI);
    addGlobalFunc('procedure ZipFiles(const ArchiveFileName: String; const Files: TStringArray)', @_LapeZipFiles);
    addGlobalFunc('procedure UnZipFile(const ArchiveFileName, OutputDirectory: String)', @_LapeUnZipFile);
    addGlobalFunc('function UnZipOneFile(const ArchiveFileName, FileName, OutputDirectory: String): Boolean', @_LapeUnZipOneFile);
    addGlobalFunc('function FindFile(Path, Mask: String; SearchSubDirs: Boolean = True; CaseSenstive: Boolean = False): TStringArray', @_LapeFindFile);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFile);

end.

