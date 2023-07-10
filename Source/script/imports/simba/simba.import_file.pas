{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_file;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.env, simba.files;

type
  PByteArray = ^TByteArray;

procedure _LapeWriteINI(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  WriteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeReadINI(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := ReadINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeDeleteINI(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  DeleteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[1])^);
end;

procedure _LapeZipExtractAll(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipExtractAll(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeZipExtractOne(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipExtractOne(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeZipFiles(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeZipEntries(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := ZipEntries(PString(Params^[0])^);
end;

procedure _LapeFileAppend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileAppend(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFileWrite(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileWrite(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFileCopy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileCopy(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFileRename(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileRename(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFileDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileDelete(PString(Params^[0])^);
end;

procedure _LapeFileRead(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileRead(PString(Params^[0])^);
end;

procedure _LapeFileReadEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileReadEx(PString(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFileReadLines(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaFile.FileReadLines(PString(Params^[0])^);
end;

procedure _LapeFileReadBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := TSimbaFile.FileReadBytes(PString(Params^[0])^);
end;

procedure _LapeFileReadBytesEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := TSimbaFile.FileReadBytesEx(PString(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFileWriteBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileWriteBytes(PString(Params^[0])^, PByteArray(Params^[1])^);
end;

procedure _LapeFileAppendBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileAppendBytes(PString(Params^[0])^, PByteArray(Params^[1])^);
end;

procedure _LapeFileCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileCreate(PString(Params^[0])^);
end;

procedure _LapeFileExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaFile.FileExists(PString(Params^[0])^);
end;

procedure _LapeFileCreationTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TSimbaFile.FileCreationTime(PString(Params^[0])^);
end;

procedure _LapeFileLastWriteTime(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := TSimbaFile.FileLastWriteTime(PString(Params^[0])^);
end;

procedure _LapeFileSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaFile.FileSize(PString(Params^[0])^);
end;

procedure _LapeFileSizeInMegaBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := TSimbaFile.FileSizeInMegaBytes(PString(Params^[0])^);
end;

procedure _LapeFileHash(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaFile.FileHash(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapePathExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathExists(PString(Params^[0])^);
end;

procedure _LapePathNormalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathNormalize(PString(Params^[0])^);
end;

procedure _LapePathIsFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsFile(PString(Params^[0])^);
end;

procedure _LapePathIsDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaPath.PathIsDirectory(PString(Params^[0])^);
end;

procedure _LapePathExtractName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractName(PString(Params^[0])^);
end;

procedure _LapePathExtractNameWithoutExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractNameWithoutExt(PString(Params^[0])^);
end;

procedure _LapePathExtractExt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractExt(PString(Params^[0])^);
end;

procedure _LapePathExtractDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractDir(PString(Params^[0])^);
end;

procedure _LapePathJoin(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathJoin(PStringArray(Params^[0])^);
end;

procedure _LapePathSetSeperators(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathSetSeperators(PString(Params^[0])^);
end;

procedure _LapePathExcludeTrailingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExcludeTrailingSep(PString(Params^[0])^);
end;

procedure _LapePathIncludeTrailingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathIncludeTrailingSep(PString(Params^[0])^);
end;

procedure _LapePathExcludeLeadingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExcludeLeadingSep(PString(Params^[0])^);
end;

procedure _LapePathIncludeLeadingSep(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathIncludeLeadingSep(PString(Params^[0])^);
end;

procedure _LapePathExtractRelative(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaPath.PathExtractRelative(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeDirList(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaDir.DirList(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeDirSearch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaDir.DirSearch(PString(Params^[0])^, PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeDirCreate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirCreate(PString(Params^[0])^);
end;

procedure _LapeDirDelete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirDelete(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeDirExists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirExists(PString(Params^[0])^);
end;

procedure _LapeDirParent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaDir.DirParent(PString(Params^[0])^);
end;

procedure _LapeDirIsEmpty(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaDir.DirIsEmpty(PString(Params^[0])^);
end;

procedure _LapeDirSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaDir.DirSize(PString(Params^[0])^);
end;

procedure _LapeDirSizeInMegaBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := TSimbaDir.DirSizeInMegaBytes(PString(Params^[0])^);
end;

procedure _LapeGetUserDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetUserDir();
end;

procedure _LapeGetTempDir(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetTempDir();
end;

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

    addGlobalVar(GetIncludePath(), 'IncludePath').isConstant := True;
    addGlobalVar(GetPluginPath(), 'PluginPath').isConstant := True;
    addGlobalVar(GetSimbaPath(), 'SimbaPath').isConstant := True;
    addGlobalVar(GetScriptPath(), 'ScriptPath').isConstant := True;
    addGlobalVar(GetDataPath(), 'DataPath').isConstant := True;
    addGlobalVar(GetScreenshotPath(), 'ScreenshotPath').isConstant := True;

    addGlobalFunc('procedure WriteINI(Section, KeyName, NewString, FileName: String)', @_LapeWriteINI);
    addGlobalFunc('function ReadINI(Section, KeyName, FileName: String): String', @_LapeReadINI);
    addGlobalFunc('procedure DeleteINI(Section, KeyName, FileName: String)', @_LapeDeleteINI);

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
    addGlobalFunc('function FileHash(FileName: String; HashType: String = "SHA1"): String', @_LapeFileHash);

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

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFile);

end.


