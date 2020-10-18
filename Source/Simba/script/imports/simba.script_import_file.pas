unit simba.script_import_file;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_File(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.files, fileutil;

procedure Lape_CreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.CreateFile(PString(Params^[0])^);
end;

procedure Lape_OpenFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.OpenFile(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_RewriteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.RewriteFile(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_AppendFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.AppendFile(PString(Params^[0])^);
end;

procedure Lape_CloseFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MFiles.CloseFile(PInt32(Params^[0])^);
end;

procedure Lape_EndOfFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.EndOfFile(PInt32(Params^[0])^);
end;

procedure Lape_FileSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.FileSizeMuf(PInt32(Params^[0])^);
end;

procedure Lape_ReadFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.ReadFileString(PInt32(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_WriteFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.WriteFileString(PInt32(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_SetFileCharPointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.SetFileCharPointer(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_FilePointerPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.FilePointerPos(PInt32(Params^[0])^);
end;

procedure Lape_FileExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FileExists(PString(Params^[0])^);
end;

procedure Lape_DirectoryExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DirectoryExists(PString(Params^[0])^);
end;

procedure Lape_CreateDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CreateDir(PString(Params^[0])^);
end;

procedure Lape_ForceDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := ForceDirectories(PString(Params^[0])^);
end;

procedure Lape_GetFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetFiles(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_GetDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetDirectories(PString(Params^[0])^);
end;

procedure Lape_DeleteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.DeleteFile(PString(Params^[0])^);
end;

procedure Lape_RenameFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.RenameFile(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_WriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MFiles.WriteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_ReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PString(Result)^ := MFiles.ReadINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_DeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MFiles.DeleteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_DeleteDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DeleteDirectory(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_UnZipFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UnZipFile(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_UnZipOneFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UnZipOneFile(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_ZipFiles(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure Lape_FindFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

procedure Lape_Import_File(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'File';

    addGlobalFunc('function CreateFile(Path: String): Int32', @Lape_CreateFile);
    addGlobalFunc('function OpenFile(Path: String; Shared: Boolean): Int32', @Lape_OpenFile);
    addGlobalFunc('function RewriteFile(Path: String; Shared: Boolean): Int32', @Lape_RewriteFile);
    addGlobalFunc('function AppendFile(Path: String): Int32', @Lape_AppendFile);
    addGlobalFunc('procedure CloseFile(FileNum: Int32);', @Lape_CloseFile);
    addGlobalFunc('function EndOfFile(FileNum: Int32): Boolean', @Lape_EndOfFile);
    addGlobalFunc('function FileSize(FileNum: Int32): LongInt', @Lape_FileSize);
    addGlobalFunc('function ReadFileString(FileNum: Int32; var s: String; x: Int32): Boolean', @Lape_ReadFileString);
    addGlobalFunc('function WriteFileString(FileNum: Int32; s: String): Boolean', @Lape_WriteFileString);
    addGlobalFunc('function SetFileCharPointer(FileNum, cChars, Origin: Int32): Int32', @Lape_SetFileCharPointer);
    addGlobalFunc('function FilePointerPos(FileNum: Int32): Int32', @Lape_FilePointerPos);
    addGlobalFunc('function FileExists(FileName: String): Boolean', @Lape_FileExists);
    addGlobalFunc('function DirectoryExists(Directory: String): Boolean', @Lape_DirectoryExists);
    addGlobalFunc('function CreateDirectory(Directory: String): Boolean', @Lape_CreateDirectory);
    addGlobalFunc('function ForceDirectories(Directory: String): Boolean', @Lape_ForceDirectories);
    addGlobalFunc('function GetFiles(Path, Ext: String): TStringArray', @Lape_GetFiles);
    addGlobalFunc('function GetDirectories(Path: String): TStringArray', @Lape_GetDirectories);
    addGlobalFunc('function DeleteFile(Filename: String): Boolean',  @Lape_DeleteFile);
    addGlobalFunc('function RenameFile(Oldname, NewName: String): Boolean;', @Lape_RenameFile);
    addGlobalFunc('procedure WriteINI(Section, KeyName, NewString, FileName: String);', @Lape_WriteINI);
    addGlobalFunc('function ReadINI(Section, KeyName, FileName: String): String', @Lape_ReadINI);
    addGlobalFunc('procedure DeleteINI(Section, KeyName, FileName: String);', @Lape_DeleteINI);
    addGlobalFunc('function DeleteDirectory(Dir: String; Empty: Boolean): Boolean;', @Lape_DeleteDirectory);
    addGlobalFunc('procedure ZipFiles(ToFolder: String; Files: TStringArray);', @Lape_ZipFiles);
    addGlobalFunc('procedure UnZipFile(FilePath, TargetPath: String);', @Lape_UnZipFile);
    addGlobalFunc('procedure UnZipOneFile(const ArchiveFileName, FileName, OutputPath: String);', @Lape_UnZipOneFile);
    addGlobalFunc('function FindFile(Path, Mask: String; SearchSubDirs: Boolean = True; CaseSenstive: Boolean = False): TStringArray;', @Lape_FindFile);
  end;
end;

end.

