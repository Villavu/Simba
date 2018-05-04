unit script_import_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, mufasatypes, files;

procedure Lape_CreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.CreateFile(PString(Params^[1])^);
end;

procedure Lape_OpenFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.OpenFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_RewriteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.RewriteFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_AppendFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.AppendFile(PString(Params^[1])^);
end;

procedure Lape_CloseFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFiles.CloseFile(PInt32(Params^[1])^);
end;

procedure Lape_EndOfFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFiles.EndOfFile(PInt32(Params^[1])^);
end;

procedure Lape_FileSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.FileSizeMuf(PInt32(Params^[1])^);
end;

procedure Lape_ReadFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFiles.ReadFileString(PInt32(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_WriteFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFiles.WriteFileString(PInt32(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_SetFileCharPointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.SetFileCharPointer(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_FilePointerPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MFiles.FilePointerPos(PInt32(Params^[1])^);
end;

procedure Lape_FileExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FileExists(PString(Params^[1])^);
end;

procedure Lape_DirectoryExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DirectoryExists(PString(Params^[1])^);
end;

procedure Lape_CreateDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CreateDir(PString(Params^[1])^);
end;

procedure Lape_ForceDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := ForceDirectories(PString(Params^[1])^);
end;

procedure Lape_GetFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetFiles(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_GetDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetDirectories(PString(Params^[1])^);
end;

procedure Lape_DeleteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFiles.DeleteFile(PString(Params^[1])^);
end;

procedure Lape_RenameFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MFiles.RenameFile(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_WriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFiles.WriteINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^, PString(Params^[4])^);
end;

procedure Lape_ReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PString(Result)^ := MFiles.ReadINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_DeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MFiles.DeleteINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_ExtractFileExt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := ExtractFileExt(PString(Params^[1])^);
end;

procedure Lape_DeleteDirectory(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DeleteDirectoryEx(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure Lape_UnZipFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UnZipFile(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_ZipFiles(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ZipFiles(PString(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure Lape_Import_File(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function ExtractFileExt(FileName: String): String;', @Lape_ExtractFileExt, Data);
    addGlobalMethod('function CreateFile(Path: String): Int32', @Lape_CreateFile, Data);
    addGlobalMethod('function OpenFile(Path: String; Shared: Boolean): Int32', @Lape_OpenFile, Data);
    addGlobalMethod('function RewriteFile(Path: String; Shared: Boolean): Int32', @Lape_RewriteFile, Data);
    addGlobalMethod('function AppendFile(Path: String): Int32', @Lape_AppendFile, Data);
    addGlobalMethod('procedure CloseFile(FileNum: Int32);', @Lape_CloseFile, Data);
    addGlobalMethod('function EndOfFile(FileNum: Int32): Boolean', @Lape_EndOfFile, Data);
    addGlobalMethod('function FileSize(FileNum: Int32): LongInt', @Lape_FileSize, Data);
    addGlobalMethod('function ReadFileString(FileNum: Int32; var s: String; x: Int32): Boolean', @Lape_ReadFileString, Data);
    addGlobalMethod('function WriteFileString(FileNum: Int32; s: String): Boolean', @Lape_WriteFileString, Data);
    addGlobalMethod('function SetFileCharPointer(FileNum, cChars, Origin: Int32): Int32', @Lape_SetFileCharPointer, Data);
    addGlobalMethod('function FilePointerPos(FileNum: Int32): Int32', @Lape_FilePointerPos, Data);
    addGlobalMethod('function FileExists(FileName: String): Boolean', @Lape_FileExists, Data);
    addGlobalMethod('function DirectoryExists(Directory: String): Boolean', @Lape_DirectoryExists, Data);
    addGlobalMethod('function CreateDirectory(Directory: String): Boolean', @Lape_CreateDirectory, Data);
    addGlobalMethod('function ForceDirectories(Directory: String): Boolean', @Lape_ForceDirectories, Data);
    addGlobalMethod('function GetFiles(Path, Ext: String): TStringArray', @Lape_GetFiles, Data);
    addGlobalMethod('function GetDirectories(Path: String): TStringArray', @Lape_GetDirectories, Data);
    addGlobalMethod('function DeleteFile(Filename: String): Boolean',  @Lape_DeleteFile, Data);
    addGlobalMethod('function RenameFile(Oldname, NewName: String): Boolean;', @Lape_RenameFile, Data);
    addGlobalMethod('procedure WriteINI(Section, KeyName, NewString, FileName: String);', @Lape_WriteINI, Data);
    addGlobalMethod('function ReadINI(Section, KeyName, FileName: String): String', @Lape_ReadINI, Data);
    addGlobalMethod('procedure DeleteINI(Section, KeyName, FileName: String);', @Lape_DeleteINI, Data);
    addGlobalMethod('function DeleteDirectory(Dir: String; Empty: Boolean): Boolean;', @Lape_DeleteDirectory, Data);
    addGlobalMethod('procedure ZipFiles(ToFolder: String; Files: TStringArray);', @Lape_ZipFiles, Data);
    addGlobalMethod('procedure UnZipFile(FilePath, TargetPath: String);', @Lape_UnZipFile, Data);
  end;
end;

initialization
  ScriptImports.Add('File', @Lape_Import_File);

end.

