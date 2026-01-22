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
  simba.base, simba.script;

procedure ImportFile(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.fs, simba.zip;

(*
File
====
File, Path, Directory related methods.
*)

(*
FileAppend
----------
```
function FileAppend(FileName: String; Text: String): Boolean;
```
*)
procedure _LapeFileAppend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileAppend(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileWrite
---------
```
function FileWrite(FileName: String; Text: String): Boolean;
```
*)
procedure _LapeFileWrite(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileWrite(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileCopy
--------
```
function FileCopy(SourceFileName, DestFileName: String; OverwriteIfExists: Boolean = True): Boolean;
```
*)
procedure _LapeFileCopy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileCopy(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileRename
----------
```
function FileRename(SourceFileName, DestFileName: String): Boolean;
```
*)
procedure _LapeFileRename(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileRename(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
FileDelete
----------
```
function FileDelete(FileName: String): Boolean;
```
*)
procedure _LapeFileDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileDelete(PString(Params^[0])^);
end;

(*
FileRead
--------
```
function FileRead(FileName: String): String;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileRead(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileRead(PString(Params^[0])^);
end;

(*
FileReadEx
----------
```
function FileReadEx(FileName: String; Start, Stop: Integer): String;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileReadEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileReadEx(PString(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
FileReadLines
-------------
```
function FileReadLines(FileName: String): TStringArray;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileReadLines(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaFile.FileReadLines(PString(Params^[0])^);
end;

(*
FileReadBytes
-------------
```
function FileReadBytes(FileName: String): TByteArray;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileReadBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := TSimbaFile.FileReadBytes(PString(Params^[0])^);
end;

(*
FileReadBytesEx
---------------
```
function FileReadBytesEx(FileName: String; Start, Stop: Integer): TByteArray;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileReadBytesEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := TSimbaFile.FileReadBytesEx(PString(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
FileWriteBytes
--------------
```
function FileWriteBytes(FileName: String; Bytes: TByteArray): Boolean;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileWriteBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileWriteBytes(PString(Params^[0])^, PByteArray(Params^[1])^);
end;

(*
FileAppendBytes
---------------
```
function FileAppendBytes(FileName: String; Bytes: TByteArray): Boolean;
```

```{note}
Other processes could modify the file while this function is being called.<br>
Use `LockFile` if you need to ensure this can't happen.
```
*)
procedure _LapeFileAppendBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileAppendBytes(PString(Params^[0])^, PByteArray(Params^[1])^);
end;

(*
FileCreate
----------
```
function FileCreate(FileName: String): Boolean;
```
*)
procedure _LapeFileCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileCreate(PString(Params^[0])^);
end;

(*
FileExists
----------
```
function FileExists(FileName: String): Boolean;
```
*)
procedure _LapeFileExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileExists(PString(Params^[0])^);
end;

(*
FileCreationTime
----------------
```
function FileCreationTime(FileName: String): TDateTime;
```
*)
procedure _LapeFileCreationTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TSimbaFile.FileCreationTime(PString(Params^[0])^);
end;

(*
FileLastWriteTime
-----------------
```
function FileLastWriteTime(FileName: String): TDateTime;
```
*)
procedure _LapeFileLastWriteTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TSimbaFile.FileLastWriteTime(PString(Params^[0])^);
end;

(*
FileSize
--------
```
function FileSize(FileName: String): Int64;
```
*)
procedure _LapeFileSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaFile.FileSize(PString(Params^[0])^);
end;

(*
FileSizeInMegaBytes
-------------------
```
function FileSizeInMegaBytes(FileName: String): Single;
```
*)
procedure _LapeFileSizeInMegaBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := TSimbaFile.FileSizeInMegaBytes(PString(Params^[0])^);
end;

(*
FileLock
--------
```
function FileLock(FileName: String): Pointer;
```
Lock a file from another process accessing it.
- Returns nil not succesfull.
- This is released when the script ends or when `FileUnlock` is called with the returned pointer.
*)
procedure _LapeFileLock(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := TSimbaFile.FileLock(PString(Params^[0])^);
end;

(*
FileUnlock
----------
```
procedure FileUnlock(Lock: Pointer);
```
Unlock a file locked by `FileLock`.
*)
procedure _LapeFileUnLock(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaFile.FileUnlock(PPointer(Params^[0])^);
end;

(*
PathExists
----------
```
function PathExists(Path: String): Boolean;
```
*)
procedure _LapePathExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathExists(PString(Params^[0])^);
end;

(*
PathNormalize
-------------
```
function PathNormalize(Path: String): String;
```
*)
procedure _LapePathNormalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathNormalize(PString(Params^[0])^);
end;

(*
PathIsFile
----------
```
function PathIsFile(Path: String): Boolean;
```
*)
procedure _LapePathIsFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsFile(PString(Params^[0])^);
end;

(*
PathIsDirectory
---------------
```
function PathIsDirectory(Path: String): Boolean;
```
*)
procedure _LapePathIsDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsDirectory(PString(Params^[0])^);
end;

(*
PathExtractName
---------------
```
function PathExtractName(Path: String): String;
```
*)
procedure _LapePathExtractName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractName(PString(Params^[0])^);
end;

(*
PathExtractNameWithoutExt
-------------------------
```
function PathExtractNameWithoutExt(Path: String): String;
```
*)
procedure _LapePathExtractNameWithoutExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractNameWithoutExt(PString(Params^[0])^);
end;

(*
PathExtractExt
--------------
```
function PathExtractExt(Path: String): String;
```
*)
procedure _LapePathExtractExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractExt(PString(Params^[0])^);
end;

(*
PathExtractDir
--------------
```
function PathExtractDir(Path: String): String;
```
*)
procedure _LapePathExtractDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractDir(PString(Params^[0])^);
end;

(*
PathJoin
--------
```
function PathJoin(Paths: TStringArray): String;
```
*)
procedure _LapePathJoin(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathJoin(PStringArray(Params^[0])^);
end;

(*
PathSetSeperators
-----------------
```
function PathSetSeperators(Path: String): String;
```
*)
procedure _LapePathSetSeperators(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathSetSeperators(PString(Params^[0])^);
end;

(*
PathExcludeTrailingSep
----------------------
```
function PathExcludeTrailingSep(Path: String): String;
```
*)
procedure _LapePathExcludeTrailingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExcludeTrailingSep(PString(Params^[0])^);
end;

(*
PathIncludeTrailingSep
----------------------
```
function PathIncludeTrailingSep(Path: String): String;
```
*)
procedure _LapePathIncludeTrailingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathIncludeTrailingSep(PString(Params^[0])^);
end;

(*
PathExcludeLeadingSep
---------------------
```
function PathExcludeLeadingSep(Path: String): String;
```
*)
procedure _LapePathExcludeLeadingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExcludeLeadingSep(PString(Params^[0])^);
end;

(*
PathIncludeLeadingSep
---------------------
```
function PathIncludeLeadingSep(Path: String): String;
```
*)
procedure _LapePathIncludeLeadingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathIncludeLeadingSep(PString(Params^[0])^);
end;

(*
PathExtractRelative
-------------------
```
function PathExtractRelative(BasePath, DestPath: String): String;
```
*)
procedure _LapePathExtractRelative(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractRelative(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
PathChangeExt
-------------
```
function PathChangeExt(Path, NewExt: String): String;
```
*)
procedure _LapePathChangeExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathChangeExt(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
PathIsInDir
-------------
```
function PathIsInDir(Path, Directory: String): Boolean;
```
*)
procedure _LapePathIsInDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsInDir(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
DirList
-------
```
function DirList(Path: String; Recursive: Boolean = False): TStringArray;
```
*)
procedure _LapeDirList(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaDir.DirList(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
DirSearch
---------
```
function DirSearch(Path: String; Mask: String; Recursive: Boolean = False): TStringArray;
```
*)
procedure _LapeDirSearch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaDir.DirSearch(PString(Params^[0])^, PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
DirCreate
---------
```
function DirCreate(Path: String): Boolean;
```
*)
procedure _LapeDirCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirCreate(PString(Params^[0])^);
end;

(*
DirDelete
---------
```
function DirDelete(Path: String; OnlyChildren: Boolean): Boolean;
```
*)
procedure _LapeDirDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirDelete(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
DirExists
---------
```
function DirExists(Path: String): Boolean;
```
*)
procedure _LapeDirExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirExists(PString(Params^[0])^);
end;

(*
DirParent
---------
```
function DirParent(Path: String): String;
```
*)
procedure _LapeDirParent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaDir.DirParent(PString(Params^[0])^);
end;

(*
DirIsEmpty
----------
```
function DirIsEmpty(Path: String): Boolean;
```
*)
procedure _LapeDirIsEmpty(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirIsEmpty(PString(Params^[0])^);
end;

(*
DirSize
-------
```
function DirSize(Path: String): Int64;
```
*)
procedure _LapeDirSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaDir.DirSize(PString(Params^[0])^);
end;

(*
DirSizeInMegaBytes
------------------
```
function DirSizeInMegaBytes(Path: String): Single;
```
*)
procedure _LapeDirSizeInMegaBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := TSimbaDir.DirSizeInMegaBytes(PString(Params^[0])^);
end;

(*
INIFileWrite
------------
```
function INIFileWrite(FileName: String; Section, Key, Value: String): Boolean
```
*)
procedure _LapeINIFileWrite(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := INIFileWrite(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
INIFileRead
-----------
```
function INIFileDelete(FileName: String; Section, Key: String): Boolean
```
*)
procedure _LapeINIFileRead(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := INIFileRead(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
INIFileDelete
-------------
```
function INIFileDelete(FileName: String; Section, Key: String): Boolean
```
*)
procedure _LapeINIFileDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := INIFileDelete(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

(*
INIFileKeys
-----------
```
function INIFileKeys(FileName: String; Section: String): TStringArray
```
*)
procedure _LapeINIFileKeys(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := INIFileKeys(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
INIFileSections
---------------
```
function INIFileSections(FileName: String): TStringArray
```
*)
procedure _LapeINIFileSections(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := INIFileSections(PString(Params^[0])^);
end;

(*
ZipExtract
----------
```
function ZipExtract(ZipFile, DestDir: String): Boolean;
```
Extract an entire zip to the destination directory.

Example:
```
ZipExtract('myzip.zip', './path/to/unzipped');
```
*)
procedure _LapeZipExtract1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipExtract(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
ZipExtract
----------
```
function ZipExtract(ZipFile, DestDir: String; Entries: TStringArray): Integer;
```
Extract entries (select files) from a zipfile.

Example:
```
ZipExtract('myzip.zip', './path/to/unzipped', ['somethinginzip', 'somethingelse/in/zip']);
```
*)
procedure _LapeZipExtract2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := ZipExtractEntries(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
end;

(*
ZipFiles
--------
```
function ZipFiles(ZipFile: String; Files: TStringArray): Boolean;
```
Creates a ZIP archive from the provided list of files.

The archive structure is "flattened": only the filenames are stored,
removing any hierarchy, they will all appear in the root of the resulting ZIP file.
*)
procedure _LapeZipFiles1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

(*
ZipFiles
--------
```
function ZipFiles(ZipFile: String; Files, Names: TStringArray): Boolean;
```
ZipFiles but allows you control the filenames within the resulting ZIP file.
*)
procedure _LapeZipFiles2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^, PStringArray(Params^[2])^);
end;

(*
ZipDirectory
------------
```
function ZipDirectory(ZipFileName: String; Dir: String): Boolean;
```
Creates a ZIP archive from all the files in the provided directory.
All files inside the archive are stored relative to the `Dir` parameter.
*)
procedure _LapeZipDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipDirectory(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
ZipReadEntries
--------------
```
function ZipReadEntries(ZipFile: String): TStringArray;
```
*)
procedure _LapeZipReadEntries(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := ZipReadEntries(PString(Params^[0])^);
end;


(*
GetUserDir
----------
```
function GetUserDir: String;
```
*)
procedure _LapeGetUserDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetUserDir();
end;

(*
GetTempDir
----------
```
function GetTempDir: String;
```
*)
procedure _LapeGetTempDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetTempDir();
end;

(*
GetTempFileName
---------------
```
function GetTempFileName: String;
```
*)
procedure _LapeGetTempFileName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetTempFileName();
end;

procedure ImportFile(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'File';

    addGlobalVar(PATH_SEP, 'PATH_SEP').isConstant := True;
    addGlobalVar(LINE_SEP, 'LINE_SEP').isConstant := True;

    addGlobalFunc('function FileRead(FileName: String): String', @_LapeFileRead);
    addGlobalFunc('function FileReadEx(FileName: String; Start, Stop: Integer): String', @_LapeFileReadEx);
    addGlobalFunc('function FileWrite(FileName: String; Text: String): Boolean', @_LapeFileWrite);
    addGlobalFunc('function FileAppend(FileName: String; Text: String): Boolean', @_LapeFileAppend);
    addGlobalFunc('function FileReadLines(FileName: String): TStringArray', @_LapeFileReadLines);

    addGlobalFunc('function FileReadBytes(FileName: String): TByteArray', @_LapeFileReadBytes);
    addGlobalFunc('function FileReadBytesEx(FileName: String; Start, Stop: Integer): TByteArray', @_LapeFileReadBytesEx);
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

    addGlobalFunc('function FileLock(FileName: String): Pointer', @_LapeFileLock);
    addGlobalFunc('procedure FileUnlock(Lock: Pointer);', @_LapeFileUnLock);

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
    addGlobalFunc('function PathIsInDir(Path, Directory: String): Boolean;', @_LapePathIsInDir);

    addGlobalFunc('function DirList(Path: String; Recursive: Boolean = False): TStringArray', @_LapeDirList);
    addGlobalFunc('function DirSearch(Path: String; Mask: String; Recursive: Boolean = False): TStringArray', @_LapeDirSearch);
    addGlobalFunc('function DirDelete(Path: String; OnlyChildren: Boolean): Boolean', @_LapeDirDelete);
    addGlobalFunc('function DirCreate(Path: String): Boolean', @_LapeDirCreate);
    addGlobalFunc('function DirExists(Path: String): Boolean', @_LapeDirExists);
    addGlobalFunc('function DirParent(Path: String): String', @_LapeDirParent);
    addGlobalFunc('function DirIsEmpty(Path: String): Boolean', @_LapeDirIsEmpty);
    addGlobalFunc('function DirSize(Path: String): Int64', @_LapeDirSize);
    addGlobalFunc('function DirSizeInMegaBytes(Path: String): Single', @_LapeDirSizeInMegaBytes);

    addGlobalFunc('function INIFileWrite(FileName: String; Section, Key, Value: String): Boolean', @_LapeINIFileWrite);
    addGlobalFunc('function INIFileRead(FileName: String; Section, Key, Value: String): String', @_LapeINIFileRead);
    addGlobalFunc('function INIFileDelete(FileName: String; Section, Key: String): Boolean', @_LapeINIFileDelete);
    addGlobalFunc('function INIFileKeys(FileName: String; Section: String): TStringArray', @_LapeINIFileKeys);
    addGlobalFunc('function INIFileSections(FileName: String): TStringArray', @_LapeINIFileSections);

    addGlobalFunc('function ZipExtract(ZipFile, DestDir: String): Boolean; overload', @_LapeZipExtract1);
    addGlobalFunc('function ZipExtract(ZipFile, DestDir: String; Entries: TStringArray): Boolean; overload', @_LapeZipExtract2);
    addGlobalFunc('function ZipFiles(ZipFile: String; Files: TStringArray): Boolean; overload', @_LapeZipFiles1);
    addGlobalFunc('function ZipFiles(ZipFile: String; Files, Names: TStringArray): Boolean; overload', @_LapeZipFiles2);
    addGlobalFunc('function ZipDirectory(ZipFile, Dir: String): Boolean', @_LapeZipDirectory);
    addGlobalFunc('function ZipReadEntries(ZipFile: String): TStringArray', @_LapeZipReadEntries);

    addGlobalFunc('function GetUserDir: String', @_LapeGetUserDir);
    addGlobalFunc('function GetTempDir: String', @_LapeGetTempDir);
    addGlobalFunc('function GetTempFileName: String', @_LapeGetTempFileName);

    DumpSection := '';
  end;
end;

end.
