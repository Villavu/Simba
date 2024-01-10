{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.files;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

const
  PATH_SEP = DirectorySeparator;
  LINE_SEP = LineEnding;

type
  TSimbaFile = class
  protected
    class function DoFileRead(const FileName: String; var Buffer; const Len: Integer; Offset: Integer = 0): Boolean;
    class function DoFileWrite(const FileName: String; const Data; const Len: Integer): Boolean;
    class function DoFileAppend(const FileName: String; const Data; const Len: Integer): Boolean;
  public
    // Read/Write String
    class function FileRead(FileName: String): String;
    class function FileReadEx(FileName: String; Offset: Integer): String;
    class function FileWrite(FileName: String; Text: String): Boolean;
    class function FileAppend(FileName: String; Text: String): Boolean;
    class function FileReadLines(FileName: String): TStringArray;

    // Read/Write Byte
    class function FileReadBytes(FileName: String): TByteArray;
    class function FileReadBytesEx(FileName: String; Offset: Integer): TByteArray;
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

    class function FileHash(FileName: String; HashType: String = 'SHA1'): String;
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
    class function PathExtractRelative(BasePath, DestPath: String): String;
    class function PathChangeExt(Path, NewExt: String): String;
  end;

  TSimbaDir = class
  public
    class function DirList(Path: String; Recursive: Boolean = False): TStringArray;
    class function DirListFiles(Path: String; Recursive: Boolean = False): TStringArray;
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

  function ZipExtractAll(ZipFileName, OutputDir: String): Boolean;
  function ZipExtractOne(ZipFileName, FileName, OutputDir: String): Boolean; overload;
  function ZipExtractOne(ZipFileName, FileName: String; out Stream: TMemoryStream): Boolean; overload;
  function ZipFiles(ZipFileName: String; Files: TStringArray): Boolean;
  function ZipEntries(ZipFileName: String): TStringArray;
  function ZipHasEntryCrc(ZipFileName: String; Crc32: UInt32): Boolean;
  function ZipAppend(ZipFileName: String; FileName, FileContents: String): Boolean;

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
  FileUtil, LazFileUtils, Zipper, IniFiles, md5, sha1,
  simba.encoding, simba.arraybuffer;

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

    if (FindFirst(Path + '*', faAnyFile and faDirectory, SearchRec) = 0) then
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr = faDirectory) and Recursive then
          Get(Path + SearchRec.Name);

        if (SearchRec.Attr <> faDirectory) then
          Buffer.Add(Path + SearchRec.Name);
      end;
    until (FindNext(SearchRec) <> 0);

    SysUtils.FindClose(SearchRec);
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
  Result := True;
  if (FindFirst(Path, faAnyFile, SearchRec) = 0) then
  repeat
    Result := (SearchRec.Name = '.') or (SearchRec.Name = '..');
  until Result and (FindNext(SearchRec) = 0);
  SysUtils.FindClose(SearchRec);
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
  Result := ExpandFileName(Path);
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
    Result := Result.Before('.');
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

class function TSimbaPath.PathChangeExt(Path, NewExt: String): String;
begin
  Result := ChangeFileExt(Path, NewExt);
end;

class function TSimbaFile.DoFileRead(const FileName: String; var Buffer; const Len: Integer; Offset: Integer): Boolean;
var
  Stream: TFileStream;
begin
  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Stream.Seek(Offset, soBeginning);
    Stream.ReadBuffer(Buffer, Len);

    Result := True;
  except
    Result := False;
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

class function TSimbaFile.DoFileWrite(const FileName: String; const Data; const Len: Integer): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;
  if (Len = 0) then
    Exit;

  Stream := nil;
  try
    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    Stream.Seek(0, soBeginning);
    Stream.Size := 0;

    Result := Stream.Write(Data, Len) = Len;
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

class function TSimbaFile.DoFileAppend(const FileName: String; const Data; const Len: Integer): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;
  if (Len = 0) then
    Exit;

  Stream := nil;
  try
    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    Stream.Seek(0, soEnd);

    Result := Stream.Write(Data, Len) = Len;
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

class function TSimbaFile.FileRead(FileName: String): String;
begin
  SetLength(Result, FileSize(FileName));

  if (Length(Result) > 0) and (not DoFileRead(FileName, Result[1], Length(Result))) then
    Result := '';
end;

class function TSimbaFile.FileReadEx(FileName: String; Offset: Integer): String;
begin
  SetLength(Result, FileSize(FileName) - Offset);
  if (Length(Result) > 0) and (not DoFileRead(FileName, Result[1], Length(Result), Offset)) then
    Result := '';
end;

class function TSimbaFile.FileWrite(FileName: String; Text: String): Boolean;
begin
  Result := (Length(Text) > 0) and DoFileWrite(FileName, Text[1], Length(Text));
end;

class function TSimbaFile.FileAppend(FileName: String; Text: String): Boolean;
begin
  Result := (Length(Text) > 0) and DoFileAppend(FileName, Text[1], Length(Text));
end;

class function TSimbaFile.FileReadLines(FileName: String): TStringArray;
begin
  Result := FileRead(FileName).Split(LineEnding);
end;

class function TSimbaFile.FileReadBytes(FileName: String): TByteArray;
begin
  SetLength(Result, FileSize(FileName));

  if (Length(Result) > 0) and (not DoFileRead(FileName, Result[1], Length(Result))) then
    Result := [];
end;

class function TSimbaFile.FileReadBytesEx(FileName: String; Offset: Integer): TByteArray;
begin
  SetLength(Result, FileSize(FileName) - Offset);
  if not DoFileRead(FileName, Result[0], Length(Result), Offset) then
    Result := [];
end;

class function TSimbaFile.FileWriteBytes(FileName: String; Bytes: TByteArray): Boolean;
begin
  Result := (Length(Bytes) > 0) and DoFileWrite(FileName, Bytes[0], Length(Bytes));
end;

class function TSimbaFile.FileAppendBytes(FileName: String; Bytes: TByteArray): Boolean;
begin
  Result := (Length(Bytes) > 0) and DoFileAppend(FileName, Bytes[0], Length(Bytes));
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

class function TSimbaFile.FileHash(FileName: String; HashType: String): String;
begin
  case HashType.ToUpper() of
    'MD5':    Result := MD5Print(MD5File(FileName));
    'SHA1':   Result := SHA1Print(SHA1File(FileName));
    'SHA256': Result := SHA256File(FileName);
    'SHA512': Result := SHA512File(FileName);
    else
      SimbaException('Invalid hashtype. Expected: SHA1,SHA256,SHA512,MD5');
  end;
end;

function ZipExtractAll(ZipFileName, OutputDir: String): Boolean;
var
  UnZipper: TUnZipper;
begin
  Result := False;

  if FileExists(ZipFileName) then
  begin
    UnZipper := TUnZipper.Create();

    try
      UnZipper.FileName := ZipFileName;
      UnZipper.OutputPath := OutputDir;
      UnZipper.UnZipAllFiles();

      Result := True;
    except
    end;

    UnZipper.Free();
  end;
end;

function ZipExtractOne(ZipFileName, FileName, OutputDir: String): Boolean;
var
  UnZipper: TUnZipper;
  I: Integer;
begin
  Result := False;

  if FileExists(ZipFileName) then
  begin
    UnZipper := TUnZipper.Create();

    try
      UnZipper.Files.Add(FileName);
      UnZipper.FileName := ZipFileName;
      UnZipper.OutputPath := OutputDir;
      UnZipper.Examine();

      for I := 0 to UnZipper.Entries.Count - 1 do
        if (UnZipper.Entries[I].ArchiveFileName = FileName) then
        begin
          UnZipper.UnZipAllFiles();

          Result := True;
          Break;
        end;
    except
    end;

    UnZipper.Free();
  end;
end;

type
  TZipExtractor = class(TUnZipper)
  protected
    procedure DoCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure DoDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  public
    Stream: TMemoryStream;

    constructor Create;
  end;

procedure TZipExtractor.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create();
end;

procedure TZipExtractor.DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  Stream := TMemoryStream(AStream);

  AStream := nil;
end;

constructor TZipExtractor.Create;
begin
  inherited Create;

  OnCreateStream := @DoCreateStream;
  OnDoneStream := @DoDoneStream;
end;

function ZipExtractOne(ZipFileName, FileName: String; out Stream: TMemoryStream): Boolean;
var
  UnZipper: TZipExtractor;
  I: Integer;
begin
  Result := False;

  if FileExists(ZipFileName) then
  begin
    UnZipper := TZipExtractor.Create();

    try
      UnZipper.Files.Add(FileName);
      UnZipper.FileName := ZipFileName;
      UnZipper.Examine();

      for I := 0 to UnZipper.Entries.Count - 1 do
        if (UnZipper.Entries[I].ArchiveFileName = FileName) then
        begin
          UnZipper.UnZipAllFiles();

          Stream := UnZipper.Stream;
          Stream.Position := 0;

          Result := True;
          Break;
        end;
    except
    end;

    UnZipper.Free();
  end;
end;

function ZipFiles(ZipFileName: String; Files: TStringArray): Boolean;
var
  Zipper: TZipper;
  I: Integer;
begin
  Result := False;

  if (Length(Files) > 0) then
  begin
    Zipper := TZipper.Create();

    try
      Zipper.FileName := ZipFileName;
      for I := 0 to High(Files) do
        Zipper.Entries.AddFileEntry(Files[I], ExtractFileName(Files[I]));

      Zipper.ZipAllFiles();

      Result := True;
    except
    end;

    Zipper.Free();
  end;
end;

function ZipEntries(ZipFileName: String): TStringArray;
var
  UnZipper: TUnZipper;
  I: Integer;
begin
  Result := [];

  if FileExists(ZipFileName) then
  begin
    UnZipper := TUnZipper.Create();
    try
      UnZipper.FileName := ZipFileName;
      UnZipper.Examine();

      SetLength(Result, UnZipper.Entries.Count);
      for I := 0 to UnZipper.Entries.Count - 1 do
        Result[I] := UnZipper.Entries[I].ArchiveFileName;
    except
    end;
    UnZipper.Free();
  end;
end;

function ZipHasEntryCrc(ZipFileName: String; Crc32: UInt32): Boolean;
var
  UnZipper: TUnZipper;
  I: Integer;
begin
  Result := False;

  if FileExists(ZipFileName) then
  begin
    UnZipper := TUnZipper.Create();
    try
      UnZipper.FileName := ZipFileName;
      UnZipper.Examine();

      for I := 0 to UnZipper.Entries.Count - 1 do
        if (UnZipper.Entries[I].CRC32 = Crc32) then
        begin
          Result := True;
          Break;
        end;
    except
    end;
    UnZipper.Free();
  end;
end;

type
  TZipAppender = class(TObject)
  protected
    FUnZip: TUnZipper;
    FZip: TZipper;

    procedure DoCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure DoDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;

    procedure Add(FileName, FileContents: String);
  end;

procedure TZipAppender.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create();
end;

procedure TZipAppender.DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  with FZip.Entries.Add() as TZipFileEntry do
  begin
    Assign(AItem);

    Stream := AStream;
    Stream.Position := 0;
  end;
end;

constructor TZipAppender.Create(AFileName: String);
begin
  inherited Create();

  if FileExists(AFileName) then
  begin
    FUnZip := TUnZipper.Create();
    FUnZip.FileName := AFileName;
    FUnZip.OnCreateStream := @DoCreateStream;
    FUnZip.OnDoneStream := @DoDoneStream;
  end;

  FZip := TZipper.Create();
  FZip.FileName := AFileName;
end;

destructor TZipAppender.Destroy;
begin
  if Assigned(FUnZip) then
    FreeAndNil(FUnZip);
  if Assigned(FZip) then
    FreeAndNil(FZip);

  inherited Destroy();
end;

procedure TZipAppender.Add(FileName, FileContents: String);
var
  I: Integer;
begin
  if Assigned(FUnZip) then
    FUnZip.UnZipAllFiles();

  FZip.Entries.AddFileEntry(TStringStream.Create(FileContents), IfThen(FileName = '', FZip.Entries.Count.ToString(), FileName));
  FZip.ZipAllFiles();

  for I := 0 to FZip.Entries.Count - 1 do
    if (FZip.Entries[I].Stream <> nil) then
    begin
      FZip.Entries[I].Stream.Free();
      FZip.Entries[I].Stream := nil;
    end;
end;

function ZipAppend(ZipFileName: String; FileName, FileContents: String): Boolean;
begin
  Result := True;

  with TZipAppender.Create(ZipFileName) do
  try
    Add(FileName, FileContents);
  finally
    Free();
  end;
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

