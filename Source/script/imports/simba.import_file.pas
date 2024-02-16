{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_file;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportFile(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.files;

(*
File
====
File, Path, Directory related methods.
*)

(*
INIFileWrite
~~~~~~~~~~~~~
> function INIFileWrite(FileName: String; Section, Key, Value: String): Boolean
*)
procedure _LapeINIFileWrite(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := INIFileWrite(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
INIFileRead
~~~~~~~~~~~~~
> function INIFileRead(FileName: String; Section, Key, Value: String): String
*)
procedure _LapeINIFileRead(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := INIFileRead(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
INIFileDelete
~~~~~~~~~~~~~
> function INIFileDelete(FileName: String; Section, Key: String): Boolean
*)
procedure _LapeINIFileDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := INIFileDelete(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

(*
INIFileKeys
~~~~~~~~~~~
> function INIFileKeys(FileName: String; Section: String): TStringArray
*)
procedure _LapeINIFileKeys(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := INIFileKeys(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
INIFileSections
~~~~~~~~~~~~~~~
> function INIFileSections(FileName: String): TStringArray
*)
procedure _LapeINIFileSections(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := INIFileSections(PString(Params^[0])^);
end;

(*
ZipExtractAll
~~~~~~~~~~~~~
> function ZipExtractAll(ZipFileName, OutputDir: String): Boolean;
*)
procedure _LapeZipExtractAll(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipExtractAll(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
ZipExtractOne
~~~~~~~~~~~~~
> function ZipExtractOne(ZipFileName, FileName, OutputDir: String): Boolean;
*)
procedure _LapeZipExtractOne(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipExtractOne(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

(*
ZipFiles
~~~~~~~~
> function ZipFiles(ZipFileName: String; Files: TStringArray): Boolean;
*)
procedure _LapeZipFiles(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

(*
ZipEntries
~~~~~~~~~~
> function ZipEntries(ZipFileName: String): TStringArray;
*)
procedure _LapeZipEntries(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := ZipEntries(PString(Params^[0])^);
end;

(*
FileAppend
~~~~~~~~~~
> function FileAppend(FileName: String; Text: String): Boolean;
*)
procedure _LapeFileAppend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileAppend(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileWrite
~~~~~~~~~
> function FileWrite(FileName: String; Text: String): Boolean;
*)
procedure _LapeFileWrite(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileWrite(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileCopy
~~~~~~~~
> function FileCopy(SourceFileName, DestFileName: String; OverwriteIfExists: Boolean = True): Boolean;
*)
procedure _LapeFileCopy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileCopy(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileRename
~~~~~~~~~~
> function FileRename(SourceFileName, DestFileName: String): Boolean;
*)
procedure _LapeFileRename(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileRename(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileDelete
~~~~~~~~~~
> function FileDelete(FileName: String): Boolean;
*)
procedure _LapeFileDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileDelete(PString(Params^[0])^);
end;

(*
FileRead
~~~~~~~~
> function FileRead(FileName: String): String;
*)
procedure _LapeFileRead(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileRead(PString(Params^[0])^);
end;

(*
FileReadEx
~~~~~~~~~~
> function FileReadEx(FileName: String; Offset: Integer): String;
*)
procedure _LapeFileReadEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileReadEx(PString(Params^[0])^, PInteger(Params^[1])^);
end;

(*
FileReadLines
~~~~~~~~~~~~~
> function FileReadLines(FileName: String): TStringArray;
*)
procedure _LapeFileReadLines(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaFile.FileReadLines(PString(Params^[0])^);
end;

(*
FileReadBytes
~~~~~~~~~~~~~
> function FileReadBytes(FileName: String): TByteArray;
*)
procedure _LapeFileReadBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := TSimbaFile.FileReadBytes(PString(Params^[0])^);
end;

(*
FileReadBytesEx
~~~~~~~~~~~~~~~
> function FileReadBytesEx(FileName: String; Offset: Integer): TByteArray;
*)
procedure _LapeFileReadBytesEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := TSimbaFile.FileReadBytesEx(PString(Params^[0])^, PInteger(Params^[1])^);
end;

(*
FileWriteBytes
~~~~~~~~~~~~~~
> function FileWriteBytes(FileName: String; Bytes: TByteArray): Boolean;
*)
procedure _LapeFileWriteBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileWriteBytes(PString(Params^[0])^, PByteArray(Params^[1])^);
end;

(*
FileAppendBytes
~~~~~~~~~~~~~~~
> function FileAppendBytes(FileName: String; Bytes: TByteArray): Boolean;
*)
procedure _LapeFileAppendBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileAppendBytes(PString(Params^[0])^, PByteArray(Params^[1])^);
end;

(*
FileCreate
~~~~~~~~~~
> function FileCreate(FileName: String): Boolean;
*)
procedure _LapeFileCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileCreate(PString(Params^[0])^);
end;

(*
FileExists
~~~~~~~~~~
> function FileExists(FileName: String): Boolean;
*)
procedure _LapeFileExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileExists(PString(Params^[0])^);
end;

(*
FileCreationTime
~~~~~~~~~~~~~~~~
> function FileCreationTime(FileName: String): TDateTime;
*)
procedure _LapeFileCreationTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TSimbaFile.FileCreationTime(PString(Params^[0])^);
end;

(*
FileLastWriteTime
~~~~~~~~~~~~~~~~~
> function FileLastWriteTime(FileName: String): TDateTime;
*)
procedure _LapeFileLastWriteTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TSimbaFile.FileLastWriteTime(PString(Params^[0])^);
end;

(*
FileSize
~~~~~~~~
> function FileSize(FileName: String): Int64;
*)
procedure _LapeFileSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaFile.FileSize(PString(Params^[0])^);
end;

(*
FileSizeInMegaBytes
~~~~~~~~~~~~~~~~~~~
> function FileSizeInMegaBytes(FileName: String): Single;
*)
procedure _LapeFileSizeInMegaBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := TSimbaFile.FileSizeInMegaBytes(PString(Params^[0])^);
end;

(*
PathExists
~~~~~~~~~~
> function PathExists(Path: String): Boolean;
*)
procedure _LapePathExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathExists(PString(Params^[0])^);
end;

(*
PathNormalize
~~~~~~~~~~~~~
> function PathNormalize(Path: String): String;
*)
procedure _LapePathNormalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathNormalize(PString(Params^[0])^);
end;

(*
PathIsFile
~~~~~~~~~~
> function PathIsFile(Path: String): Boolean;
*)
procedure _LapePathIsFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsFile(PString(Params^[0])^);
end;

(*
PathIsDirectory
~~~~~~~~~~~~~~~
> function PathIsDirectory(Path: String): Boolean;
*)
procedure _LapePathIsDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsDirectory(PString(Params^[0])^);
end;

(*
PathExtractName
~~~~~~~~~~~~~~~
> function PathExtractName(Path: String): String;
*)
procedure _LapePathExtractName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractName(PString(Params^[0])^);
end;

(*
PathExtractNameWithoutExt
~~~~~~~~~~~~~~~~~~~~~~~~~
> function PathExtractNameWithoutExt(Path: String): String;
*)
procedure _LapePathExtractNameWithoutExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractNameWithoutExt(PString(Params^[0])^);
end;

(*
PathExtractExt
~~~~~~~~~~~~~~
> function PathExtractExt(Path: String): String;
*)
procedure _LapePathExtractExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractExt(PString(Params^[0])^);
end;

(*
PathExtractDir
~~~~~~~~~~~~~~
> function PathExtractDir(Path: String): String;
*)
procedure _LapePathExtractDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractDir(PString(Params^[0])^);
end;

(*
PathJoin
~~~~~~~~
> function PathJoin(Paths: TStringArray): String;
*)
procedure _LapePathJoin(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathJoin(PStringArray(Params^[0])^);
end;

(*
PathSetSeperators
~~~~~~~~~~~~~~~~~
> function PathSetSeperators(Path: String): String;
*)
procedure _LapePathSetSeperators(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathSetSeperators(PString(Params^[0])^);
end;

(*
PathExcludeTrailingSep
~~~~~~~~~~~~~~~~~~~~~~
> function PathExcludeTrailingSep(Path: String): String;
*)
procedure _LapePathExcludeTrailingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExcludeTrailingSep(PString(Params^[0])^);
end;

(*
PathIncludeTrailingSep
~~~~~~~~~~~~~~~~~~~~~~
> function PathIncludeTrailingSep(Path: String): String;
*)
procedure _LapePathIncludeTrailingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathIncludeTrailingSep(PString(Params^[0])^);
end;

(*
PathExcludeLeadingSep
~~~~~~~~~~~~~~~~~~~~~
> function PathExcludeLeadingSep(Path: String): String;
*)
procedure _LapePathExcludeLeadingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExcludeLeadingSep(PString(Params^[0])^);
end;

(*
PathIncludeLeadingSep
~~~~~~~~~~~~~~~~~~~~~
> function PathIncludeLeadingSep(Path: String): String;
*)
procedure _LapePathIncludeLeadingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathIncludeLeadingSep(PString(Params^[0])^);
end;

(*
PathExtractRelative
~~~~~~~~~~~~~~~~~~~
> function PathExtractRelative(BasePath, DestPath: String): String;
*)
procedure _LapePathExtractRelative(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractRelative(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
PathChangeExt
~~~~~~~~~~~~~
> function PathChangeExt(Path, NewExt: String): String;
*)
procedure _LapePathChangeExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathChangeExt(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
DirList
~~~~~~~
> function DirList(Path: String; Recursive: Boolean = False): TStringArray;
*)
procedure _LapeDirList(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaDir.DirList(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
DirSearch
~~~~~~~~~
> function DirSearch(Path: String; Mask: String; Recursive: Boolean = False): TStringArray;
*)
procedure _LapeDirSearch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaDir.DirSearch(PString(Params^[0])^, PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
DirCreate
~~~~~~~~~
> function DirCreate(Path: String): Boolean;
*)
procedure _LapeDirCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirCreate(PString(Params^[0])^);
end;

(*
DirDelete
~~~~~~~~~
> function DirDelete(Path: String; OnlyChildren: Boolean): Boolean;
*)
procedure _LapeDirDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirDelete(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
DirExists
~~~~~~~~~
> function DirExists(Path: String): Boolean;
*)
procedure _LapeDirExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirExists(PString(Params^[0])^);
end;

(*
DirParent
~~~~~~~~~
> function DirParent(Path: String): String;
*)
procedure _LapeDirParent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaDir.DirParent(PString(Params^[0])^);
end;

(*
DirIsEmpty
~~~~~~~~~~
> function DirIsEmpty(Path: String): Boolean;
*)
procedure _LapeDirIsEmpty(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirIsEmpty(PString(Params^[0])^);
end;

(*
DirSize
~~~~~~~
> function DirSize(Path: String): Int64;
*)
procedure _LapeDirSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaDir.DirSize(PString(Params^[0])^);
end;

(*
DirSizeInMegaBytes
~~~~~~~~~~~~~~~~~~
> function DirSizeInMegaBytes(Path: String): Single;
*)
procedure _LapeDirSizeInMegaBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := TSimbaDir.DirSizeInMegaBytes(PString(Params^[0])^);
end;

(*
GetUserDir
~~~~~~~~~~
> function GetUserDir: String;
*)
procedure _LapeGetUserDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetUserDir();
end;

(*
GetTempDir
~~~~~~~~~~
> function GetTempDir: String;
*)
procedure _LapeGetTempDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetTempDir();
end;

(*
GetTempFileName
~~~~~~~~~~~~~~~
> function GetTempFileName: String;
*)
procedure _LapeGetTempFileName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetTempFileName();
end;

procedure ImportFile(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'File';

    addGlobalVar(PATH_SEP, 'PATH_SEP').isConstant := True;
    addGlobalVar(LINE_SEP, 'LINE_SEP').isConstant := True;

    addGlobalFunc('function INIFileWrite(FileName: String; Section, Key, Value: String): Boolean', @_LapeINIFileWrite);
    addGlobalFunc('function INIFileRead(FileName: String; Section, Key, Value: String): String', @_LapeINIFileRead);
    addGlobalFunc('function INIFileDelete(FileName: String; Section, Key: String): Boolean', @_LapeINIFileDelete);
    addGlobalFunc('function INIFileKeys(FileName: String; Section: String): TStringArray', @_LapeINIFileKeys);
    addGlobalFunc('function INIFileSections(FileName: String): TStringArray', @_LapeINIFileSections);

    addGlobalFunc('function ZipExtractAll(ZipFileName, OutputDir: String): Boolean', @_LapeZipExtractAll);
    addGlobalFunc('function ZipExtractOne(ZipFileName, FileName, OutputDir: String): Boolean', @_LapeZipExtractOne);
    addGlobalFunc('function ZipFiles(ZipFileName: String; Files: TStringArray): Boolean', @_LapeZipFiles);
    addGlobalFunc('function ZipEntries(ZipFileName: String): TStringArray', @_LapeZipEntries);

    addGlobalFunc('function GetUserDir: String', @_LapeGetUserDir);
    addGlobalFunc('function GetTempDir: String', @_LapeGetTempDir);
    addGlobalFunc('function GetTempFileName: String', @_LapeGetTempFileName);

    addGlobalFunc('function FileRead(FileName: String): String', @_LapeFileRead);
    addGlobalFunc('function FileReadEx(FileName: String; Offset: Integer): String', @_LapeFileReadEx);
    addGlobalFunc('function FileWrite(FileName: String; Text: String): Boolean', @_LapeFileWrite);
    addGlobalFunc('function FileAppend(FileName: String; Text: String): Boolean', @_LapeFileAppend);
    addGlobalFunc('function FileReadLines(FileName: String): TStringArray', @_LapeFileReadLines);

    addGlobalFunc('function FileReadBytes(FileName: String): TByteArray', @_LapeFileReadBytes);
    addGlobalFunc('function FileReadBytesEx(FileName: String; Offset: Integer): TByteArray', @_LapeFileReadBytesEx);
    addGlobalFunc('function FileWriteBytes(FileName: String; Bytes: TByteArray): Boolean', @_LapeFileWriteBytes);
    addGlobalFunc('function FileAppendBytes(FileName: String; Bytes: TByteArray): Boolean', @_LapeFileAppendBytes);

    addGlobalFunc('function FileCopy(SourceFileName, DestFileName: String; OverwriteIfExists: Boolean = True): Boolean', @_LapeFileCopy);
    addGlobalFunc('function FileRename(SourceFileName, DestFileName: String): Boolean', @_LapeFileRename);
    addGlobalFunc('function FileDelete(FileName: String): Boolean', @_LapeFileDelete);
    addGlobalFunc('function FileCreate(FileName: String): Boolean', @_LapeFileCreate);
    addGlobalFunc('function FileExists(FileName: String): Boolean', @_LapeFileExists);
    addGlobalFunc('function FileCreationTime(FileName: String): TDateTime', @_LapeFileCreationTime);
    addGlobalFunc('function FileLastWriteTime(FileName: String): TDateTime', @_LapeFileLastWriteTime);
    addGlobalFunc('function FileSize(FileName: String): Int64', @_LapeFileSize);
    addGlobalFunc('function FileSizeInMegaBytes(FileName: String): Single', @_LapeFileSizeInMegaBytes);

    addGlobalFunc('function PathExists(Path: String): Boolean', @_LapePathExists);
    addGlobalFunc('function PathNormalize(Path: String): String', @_LapePathNormalize);
    addGlobalFunc('function PathIsFile(Path: String): Boolean', @_LapePathIsFile);
    addGlobalFunc('function PathIsDirectory(Path: String): Boolean', @_LapePathIsDirectory);
    addGlobalFunc('function PathExtractName(Path: String): String', @_LapePathExtractName);
    addGlobalFunc('function PathExtractNameWithoutExt(Path: String): String', @_LapePathExtractNameWithoutExt);
    addGlobalFunc('function PathExtractExt(Path: String): String', @_LapePathExtractExt);
    addGlobalFunc('function PathExtractDir(Path: String): String', @_LapePathExtractDir);
    addGlobalFunc('function PathJoin(Paths: TStringArray): String', @_LapePathJoin);
    addGlobalFunc('function PathSetSeperators(Path: String): String', @_LapePathSetSeperators);
    addGlobalFunc('function PathExcludeTrailingSep(Path: String): String', @_LapePathExcludeTrailingSep);
    addGlobalFunc('function PathIncludeTrailingSep(Path: String): String', @_LapePathIncludeTrailingSep);
    addGlobalFunc('function PathExcludeLeadingSep(Path: String): String', @_LapePathExcludeLeadingSep);
    addGlobalFunc('function PathIncludeLeadingSep(Path: String): String', @_LapePathIncludeLeadingSep);
    addGlobalFunc('function PathExtractRelative(BasePath, DestPath: String): String', @_LapePathExtractRelative);
    addGlobalFunc('function PathChangeExt(Path, NewExt: String): String', @_LapePathChangeExt);

    addGlobalFunc('function DirList(Path: String; Recursive: Boolean = False): TStringArray', @_LapeDirList);
    addGlobalFunc('function DirSearch(Path: String; Mask: String; Recursive: Boolean = False): TStringArray', @_LapeDirSearch);
    addGlobalFunc('function DirDelete(Path: String; OnlyChildren: Boolean): Boolean', @_LapeDirDelete);
    addGlobalFunc('function DirCreate(Path: String): Boolean', @_LapeDirCreate);
    addGlobalFunc('function DirExists(Path: String): Boolean', @_LapeDirExists);
    addGlobalFunc('function DirParent(Path: String): String', @_LapeDirParent);
    addGlobalFunc('function DirIsEmpty(Path: String): Boolean', @_LapeDirIsEmpty);
    addGlobalFunc('function DirSize(Path: String): Int64', @_LapeDirSize);
    addGlobalFunc('function DirSizeInMegaBytes(Path: String): Single', @_LapeDirSizeInMegaBytes);

    ImportingSection := '';
  end;
end;

end.
