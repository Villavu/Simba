{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.fs;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.hash;

const
  PATH_SEP = DirectorySeparator;
  LINE_SEP = LineEnding;

type
  TSimbaFile = class
  private
    class function DoFileRead(const FileName: String; Len, Offset: Int64; out Bytes: TByteArray): Boolean;
    class function DoFileWrite(const FileName: String; const Bytes: TByteArray; Seek: TSeekOrigin; Offset: Integer): Boolean;
  public
    // Read/Write String
    class function FileRead(FileName: String): String;
    class function FileReadEx(FileName: String; Start, Stop: Integer): String;
    class function FileReadLines(FileName: String): TStringArray;
    class function FileWrite(FileName: String; Text: String): Boolean;
    class function FileAppend(FileName: String; Text: String): Boolean;

    // Read/Write Byte
    class function FileReadBytes(FileName: String): TByteArray;
    class function FileReadBytesEx(FileName: String; Start, Stop: Integer): TByteArray;
    class function FileWriteBytes(FileName: String; Bytes: TByteArray): Boolean;
    class function FileAppendBytes(FileName: String; Bytes: TByteArray): Boolean;

    class function FileCopy(SourceFileName, DestFileName: String; OverwriteIfExists: Boolean = True): Boolean;
    class function FileRename(SourceFileName, DestFileName: String): Boolean;
    class function FileDelete(FileName: String): Boolean;
    class function FileCreate(FileName: String): Boolean;
    class function FileExists(FileName: String): Boolean;
    class function FileCreationTime(FileName: String): TDateTime;
    class function FileLastWriteTime(FileName: String): TDateTime;
    class function FileSize(FileName: String): Int64;
    class function FileSizeInMegaBytes(FileName: String): Single;
    class function FileHash(FileName: String; Algo: EHashAlgo = EHashAlgo.SHA1): String;
    class function FileIsText(FileName: String): Boolean;

    class function FileLock(FileName: String): Pointer;
    class procedure FileUnlock(Lock: Pointer);
  end;

  TSimbaPath = class
  public
    class function PathExists(Path: String): Boolean;
    class function PathNormalize(Path: String): String;
    class function PathIsFile(Path: String): Boolean;
    class function PathIsDirectory(Path: String): Boolean;
    class function PathExtractName(Path: String): String;
    class function PathExtractNameWithoutExt(Path: String): String;
    class function PathExtractExt(Path: String): String;
    class function PathExtractDir(Path: String): String;
    class function PathJoin(Paths: TStringArray): String;
    class function PathSetSeperators(Path: String): String;
    class function PathExcludeTrailingSep(Path: String): String;
    class function PathIncludeTrailingSep(Path: String): String;
    class function PathExcludeLeadingSep(Path: String): String;
    class function PathIncludeLeadingSep(Path: String): String;
    class function PathExtractRelative(BasePath, DestPath: String): String; overload;
    class function PathExtractRelative(DestPath: String): String; overload;
    class function PathChangeExt(Path, NewExt: String): String;
    class function PathIsInDir(Path, Directory: String): Boolean;
    class function PathHasExt(Path: String; Extensions: array of String): Boolean;
  end;

  TSimbaDir = class
  public
    class function DirList(Path: String; Recursive: Boolean = False): TStringArray;
    class function DirListFiles(Path: String; Recursive: Boolean = False): TStringArray;
    class function DirListDirectories(Path: String; Recursive: Boolean = False): TStringArray;
    class function DirSearch(Path: String; Mask: String; Recursive: Boolean = False): TStringArray;
    class function DirDelete(Path: String; OnlyChildren: Boolean): Boolean;
    class function DirCreate(Path: String): Boolean;
    class function DirExists(Path: String): Boolean;
    class function DirParent(Path: String): String;
    class function DirIsEmpty(Path: String): Boolean;
    class function DirSize(Path: String): Int64;
    class function DirSizeInMegaBytes(Path: String): Single;
    class function DirCopy(SourceDir, DestDir: String): Boolean;
  end;

  function INIFileWrite(FileName: String; Section, Key, Value: String): Boolean;
  function INIFileRead(FileName: String; Section, Key, Value: String): String;
  function INIFileDelete(FileName: String; Section, Key: String): Boolean;
  function INIFileKeys(FileName: String; Section: String): TStringArray;
  function INIFileSections(FileName: String): TStringArray;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Forms, FileUtil, LazFileUtils, IniFiles,
  simba.containers,
  simba.vartype_string,
  simba.vartype_ordarray;

class function TSimbaDir.DirList(Path: String; Recursive: Boolean): TStringArray;
var
  Buffer: TSimbaStringBuffer;

  procedure Get(Path: String);
  var
    SearchRec: TSearchRec;
  begin
    Path := CleanAndExpandDirectory(Path);

    if (FindFirst(Path + '*', faAnyFile and faDirectory, SearchRec) = 0) then
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr = faDirectory) and Recursive then
          Get(Path + SearchRec.Name);

        Buffer.Add(Path + SearchRec.Name);
      end;
    until (FindNext(SearchRec) <> 0);

    SysUtils.FindClose(SearchRec);
  end;

begin
  Get(Path);

  Result := Buffer.ToArray(False);
end;

class function TSimbaDir.DirListFiles(Path: String; Recursive: Boolean): TStringArray;
var
  Buffer: TSimbaStringBuffer;

  procedure Get(Path: String);
  var
    SearchRec: TSearchRec;
  begin
    Path := CleanAndExpandDirectory(Path);

    if (FindFirst(Path + AllFilesMask, faAnyFile and faDirectory, SearchRec) = 0) then
    try
      repeat
        if (SearchRec.Name <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory <> 0) and Recursive then // is directory
            Get(Path + SearchRec.Name);

          if (SearchRec.Attr and faDirectory = 0) then // is file
            Buffer.Add(Path + SearchRec.Name);
        end;
      until (FindNext(SearchRec) <> 0);
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;

begin
  Get(Path);

  Result := Buffer.ToArray(False);
end;

class function TSimbaDir.DirListDirectories(Path: String; Recursive: Boolean): TStringArray;
var
  Buffer: TSimbaStringBuffer;

  procedure Get(Path: String);
  var
    SearchRec: TSearchRec;
  begin
    Path := CleanAndExpandDirectory(Path);

    if (FindFirst(Path + AllDirectoryEntriesMask, faDirectory, SearchRec) = 0) then
    try
      repeat
        if (SearchRec.Name <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
           (SearchRec.Attr and faDirectory <> 0) then // is directory
        begin
          if Recursive then
            Get(Path + SearchRec.Name);

          Buffer.Add(Path + SearchRec.Name);
        end;
      until (FindNext(SearchRec) <> 0);
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;

begin
  Get(Path);

  Result := Buffer.ToArray(False);
end;

class function TSimbaDir.DirSearch(Path: String; Mask: String; Recursive: Boolean): TStringArray;
begin
  with FindAllFiles(Path, Mask, Recursive) do
  try
    Result := ToStringArray();
  finally
    Free();
  end;
end;

class function TSimbaDir.DirDelete(Path: String; OnlyChildren: Boolean): Boolean;
begin
  Result := DeleteDirectory(Path, OnlyChildren);
end;

class function TSimbaDir.DirCreate(Path: String): Boolean;
begin
  Result := DirectoryExists(Path) or ForceDirectory(Path);
end;

class function TSimbaDir.DirExists(Path: String): Boolean;
begin
  Result := DirectoryExists(Path);
end;

class function TSimbaDir.DirParent(Path: String): String;
begin
  Result := ExpandFileName(IncludeTrailingPathDelimiter(Path) + '..');
end;

class function TSimbaDir.DirIsEmpty(Path: String): Boolean;
var
  SearchRec: TSearchRec;
begin
  Path := CleanAndExpandDirectory(Path);

  if (FindFirst(Path + AllFilesMask, faAnyFile and faDirectory, SearchRec) = 0) then
  try
    repeat
      if (SearchRec.Name <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        Exit(False);
    until (FindNext(SearchRec) <> 0);
  finally
    SysUtils.FindClose(SearchRec);
  end;

  Result := True;
end;

class function TSimbaDir.DirSize(Path: String): Int64;
var
  FileName: String;
begin
  Result := 0;
  for FileName in DirList(Path, True) do
    Result := Result + TSimbaFile.FileSize(FileName);
end;

class function TSimbaDir.DirSizeInMegaBytes(Path: String): Single;
begin
  Result := DirSize(Path) / (1024 * 1024);
end;

class function TSimbaDir.DirCopy(SourceDir, DestDir: String): Boolean;
begin
  Result := CopyDirTree(SourceDir, DestDir);
end;

class function TSimbaPath.PathExists(Path: String): Boolean;
begin
  Result := FileExists(Path) or DirectoryExists(Path);
end;

class function TSimbaPath.PathNormalize(Path: String): String;
begin
  Result := CleanAndExpandFilename(Path);
end;

class function TSimbaPath.PathIsFile(Path: String): Boolean;
begin
  Result := FileExists(Path) and (not DirectoryExists(Path));
end;

class function TSimbaPath.PathIsDirectory(Path: String): Boolean;
begin
  Result := DirectoryExists(Path);
end;

class function TSimbaPath.PathExtractName(Path: String): String;
begin
  Result := ExtractFileName(Path);
end;

class function TSimbaPath.PathExtractNameWithoutExt(Path: String): String;
begin
  Result := ExtractFileName(Path);
  if '.' in Result then
    Result := Result.Before(ExtractFileExt(Path));
end;

class function TSimbaPath.PathExtractExt(Path: String): String;
begin
  Result := ExtractFileExt(Path);
end;

class function TSimbaPath.PathExtractDir(Path: String): String;
begin
  Result := ExtractFileDir(ExcludeTrailingPathDelimiter(Path));
end;

class function TSimbaPath.PathJoin(Paths: TStringArray): String;
begin
  Result := ConcatPaths(Paths);
end;

class function TSimbaPath.PathSetSeperators(Path: String): String;
begin
  Result := GetForcedPathDelims(Path);
end;

class function TSimbaPath.PathExcludeTrailingSep(Path: String): String;
begin
  Result := ExcludeTrailingPathDelimiter(Path);
end;

class function TSimbaPath.PathIncludeTrailingSep(Path: String): String;
begin
  Result := IncludeTrailingPathDelimiter(Path);
end;

class function TSimbaPath.PathExcludeLeadingSep(Path: String): String;
begin
  Result := ExcludeLeadingPathDelimiter(Path);
end;

class function TSimbaPath.PathIncludeLeadingSep(Path: String): String;
begin
  Result := IncludeLeadingPathDelimiter(Path);
end;

class function TSimbaPath.PathExtractRelative(BasePath, DestPath: String): String;
begin
  Result := ExtractRelativePath(BasePath, DestPath);
end;

class function TSimbaPath.PathExtractRelative(DestPath: String): String;
begin
  Result := ExtractRelativePath(Application.Location, DestPath);
end;

class function TSimbaPath.PathChangeExt(Path, NewExt: String): String;
begin
  Result := ChangeFileExt(Path, NewExt);
end;

class function TSimbaPath.PathIsInDir(Path, Directory: String): Boolean;
begin
  Result := PathIsInPath(Path, Directory);
end;

class function TSimbaPath.PathHasExt(Path: String; Extensions: array of String): Boolean;
var
  Ext: String;
  I: Integer;
begin
  Ext := PathExtractExt(Path);
  for I := 0 to High(Extensions) do
    if (Ext = Extensions[I]) then
      Exit(True);

  Result := False;
end;

class function TSimbaFile.DoFileRead(const FileName: String; Len, Offset: Int64; out Bytes: TByteArray): Boolean;
var
  Stream: TFileStream;
  BytesToRead: SizeInt;
begin
  Result := False;

  Bytes := [];
  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    if (Offset >= 0) and (Offset < Stream.Size) then
    begin
      BytesToRead := Stream.Size - Offset;
      if (Len >= 0) and (Len < BytesToRead) then
        BytesToRead := Len;
      SetLength(Bytes, BytesToRead);

      if (Length(Bytes) > 0) then
      begin
        Stream.Seek(Offset, soBeginning);
        Stream.ReadBuffer(Bytes[0], Length(Bytes));

        Result := True;
      end;
    end;
  except
  end;
  if (Stream <> nil) then
    Stream.Free();
end;

class function TSimbaFile.DoFileWrite(const FileName: String; const Bytes: TByteArray; Seek: TSeekOrigin; Offset: Integer): Boolean;
var
  Stream: TFileStream;
  Written: Int64;
begin
  Result := False;

  Stream := nil;
  try
    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
    else
      Stream := TFileStream.Create(FileName, fmCreate);
    Stream.Seek(Offset, Seek);
    if (Seek = soBeginning) then
      Stream.Size := 0;

    Written := 0;
    if (Length(Bytes) > 0) then
      Written := Stream.Write(Bytes[0], Length(Bytes));

    Result := (Written = Length(Bytes));
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

class function TSimbaFile.FileRead(FileName: String): String;
var
  Bytes: TByteArray;
begin
  if DoFileRead(FileName, -1, 0, Bytes) then
    Result := Bytes.ToString()
  else
    Result := '';
end;

class function TSimbaFile.FileReadEx(FileName: String; Start, Stop: Integer): String;
var
  Bytes: TByteArray;
begin
  if DoFileRead(FileName, (Stop-Start)+1, Start, Bytes) then
    Result := Bytes.ToString()
  else
    Result := '';
end;

class function TSimbaFile.FileReadLines(FileName: String): TStringArray;
begin
  Result := FileRead(FileName).SplitLines();
end;

class function TSimbaFile.FileWrite(FileName: String; Text: String): Boolean;
begin
  Result := DoFileWrite(FileName, Text.ToBytes, soBeginning, 0);
end;

class function TSimbaFile.FileAppend(FileName: String; Text: String): Boolean;
begin
  Result := DoFileWrite(FileName, Text.ToBytes, soEnd, 0);
end;

class function TSimbaFile.FileReadBytes(FileName: String): TByteArray;
var
  Bytes: TByteArray;
begin
  if DoFileRead(FileName, -1, 0, Bytes) then
    Result := Bytes
  else
    Result := [];
end;

class function TSimbaFile.FileReadBytesEx(FileName: String; Start, Stop: Integer): TByteArray;
var
  Bytes: TByteArray;
begin
  if DoFileRead(FileName, (Stop-Start)+1, Start, Bytes) then
    Result := Bytes
  else
    Result := [];
end;

class function TSimbaFile.FileWriteBytes(FileName: String; Bytes: TByteArray): Boolean;
begin
  Result := DoFileWrite(FileName, Bytes, soBeginning, 0);
end;

class function TSimbaFile.FileAppendBytes(FileName: String; Bytes: TByteArray): Boolean;
begin
  Result := DoFileWrite(FileName, Bytes, soEnd, 0);
end;

class function TSimbaFile.FileCopy(SourceFileName, DestFileName: String; OverwriteIfExists: Boolean): Boolean;
begin
  if OverwriteIfExists then
    Result := FileUtil.CopyFile(ExpandFileName(SourceFileName), ExpandFileName(DestFileName), [cffOverwriteFile, cffCreateDestDirectory])
  else
    Result := FileUtil.CopyFile(ExpandFileName(SourceFileName), ExpandFileName(DestFileName), [cffCreateDestDirectory])
end;

class function TSimbaFile.FileRename(SourceFileName, DestFileName: String): Boolean;
begin
  Result := RenameFile(SourceFileName, DestFileName);
end;

class function TSimbaFile.FileDelete(FileName: String): Boolean;
begin
  Result := SysUtils.DeleteFile(FileName);
end;

class function TSimbaFile.FileCreate(FileName: String): Boolean;
begin
  if SysUtils.FileExists(FileName) then
    Exit(True);

  try
    FileClose(SysUtils.FileCreate(FileName));
  except
  end;

  Result := SysUtils.FileExists(FileName);
end;

class function TSimbaFile.FileExists(FileName: String): Boolean;
begin
  Result := SysUtils.FileExists(FileName);
end;

class function TSimbaFile.FileCreationTime(FileName: String): TDateTime;
{$IFDEF UNIX}
var
  Info: stat;
begin
  Result := 0;

  if (fpstat(FileName, Info) = 0) then
    Result := FileDateToDateTime(Info.st_ctime);
end;
{$ENDIF}
{$IFDEF WINDOWS}
var
  FileAttribute: TWin32FileAttributeData;
  SystemTime: TSystemTime;
  FileTime: TFileTime;
begin
  Result := 0;

  if GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @FileAttribute) then
    if FileTimeToLocalFileTime(FileAttribute.ftCreationTime, FileTime) and FileTimeToSystemTime(FileTime, SystemTime) then
      Result := SystemTimeToDateTime(SystemTime);
end;
{$ENDIF}

class function TSimbaFile.FileLastWriteTime(FileName: String): TDateTime;
{$IFDEF UNIX}
var
  Info: stat;
begin
  Result := 0;

  if (fpstat(FileName, Info) = 0) then
    Result := FileDateToDateTime(Info.st_mtime);
end;
{$ENDIF}
{$IFDEF WINDOWS}
var
  FileAttribute: TWin32FileAttributeData;
  SystemTime: TSystemTime;
  FileTime: TFileTime;
begin
  Result := 0;

  if GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @FileAttribute) then
    if FileTimeToLocalFileTime(FileAttribute.ftLastWriteTime, FileTime) and FileTimeToSystemTime(FileTime, SystemTime) then
      Result := SystemTimeToDateTime(SystemTime);
end;
{$ENDIF}

class function TSimbaFile.FileSize(FileName: String): Int64;
begin
  Result := FileUtil.FileSize(FileName);
end;

class function TSimbaFile.FileSizeInMegaBytes(FileName: String): Single;
begin
  Result := FileUtil.FileSize(FileName) / (1024 * 1024);
end;

class function TSimbaFile.FileHash(FileName: String; Algo: EHashAlgo): String;
begin
  Result := HashFile(Algo, FileName);
end;

class function TSimbaFile.FileIsText(FileName: String): Boolean;
begin
  Result := LazFileUtils.FileIsText(FileName);
end;

class function TSimbaFile.FileLock(FileName: String): Pointer;
begin
  try
    Result := TFileStream.Create(FileName, fmShareExclusive);
  except
    Result := nil;
  end;
end;

class procedure TSimbaFile.FileUnlock(Lock: Pointer);
begin
  if (Lock <> nil) then
    TFileStream(Lock).Free();
end;

function INIFileWrite(FileName: String; Section, Key, Value: String): Boolean;
begin
  Result := True;

  try
    with TINIFile.Create(FileName) do
    try
	    WriteString(Section, Key, Value);
    finally
	    Free();
    end;
  except
    Result := False;
  end;
end;

function INIFileRead(FileName: String; Section, Key, Value: String): String;
begin
  Result := '';

  try
    with TINIFile.Create(FileName) do
    try
	    Result := ReadString(Section, Key, Value);
    finally
	    Free();
    end;
  except
  end;
end;

function INIFileDelete(FileName: String; Section, Key: String): Boolean;
begin
  Result := True;

  try
    with TIniFile.Create(FileName) do
    try
      if (Key = '') then
	      EraseSection(Section)
	    else
	      DeleteKey(Section, Key);
    finally
      Free();
    end;
  except
    Result := False;
  end;
end;

function INIFileKeys(FileName: String; Section: String): TStringArray;
var
  List: TStringList;
begin
  Result := [];

  List := nil;
  try
    with TIniFile.Create(FileName) do
    try
      List := TStringList.Create();
      ReadSection(Section, List);

      Result := List.ToStringArray();
    finally
      Free();
    end;
  except
  end;
  if Assigned(List) then
    List.Free();
end;

function INIFileSections(FileName: String): TStringArray;
var
  List: TStringList;
begin
  Result := [];

  List := nil;
  try
    with TIniFile.Create(FileName) do
    try
      List := TStringList.Create();
      ReadSections(List);

      Result := List.ToStringArray();
    finally
      Free();
    end;
  except
  end;
  if Assigned(List) then
    List.Free();
end;

end.

