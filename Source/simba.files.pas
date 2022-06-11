{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.files;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

const
  File_AccesError = -1;
  File_EventError = -2;

type
  PMFiles = ^TMFiles;
  TMFiles = class(TObject)
  public
    function CreateFile(Path: string): Integer;
    function OpenFile(Path: string; Shared: Boolean): Integer;
    function RewriteFile(Path: string; Shared: Boolean): Integer;
    function AppendFile(Path: string): Integer;
    function DeleteFile(Filename: string): Boolean;
    function RenameFile(OldName, NewName: string): Boolean;
    procedure CloseFile(FileNum: Integer);
    procedure WriteINI(const Section, KeyName, NewString : string; FileName : string);
    function ReadINI(const Section, KeyName : string; FileName : string) : string;
    procedure DeleteINI(const Section, KeyName : string; FileName : string);
    function EndOfFile(FileNum: Integer): Boolean;
    function FileSizeMuf(FileNum: Integer): LongInt;
    function ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
    function WriteFileString(FileNum: Integer;const s: string): Boolean;
    function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
    function FilePointerPos(FileNum: Integer): Integer;
    constructor Create(Owner : TObject);
    destructor Destroy; override;
  private
    MFiles: TMufasaFilesArray;
    FreeSpots: Array Of Integer;
    Client : TObject;
    procedure CheckFileNum(FileNum : integer);
    procedure FreeFileList;
    function AddFileToManagedList(Path: string; FS: TFileStream; Mode: Integer): Integer;
  end;

  function GetFiles(Path, Ext: string): TStringArray;
  function GetDirectories(Path: string): TStringArray;
  function FindFile(var FileName: string; Extension: String; const Directories: array of String): Boolean;
  function FindFiles(Directories: TStringArray; WildCard: String; Recursive: Boolean = False): TStringArray;
  function FindPlugin(var FileName: String; const Directories: array of String): Boolean;
  procedure CopyPlugin(var FileName: String);
  procedure ZipFiles(const ArchiveFileName: String; const Files: TStringArray);
  procedure UnZipFile(const ArchiveFileName, OutputDirectory: String);
  function UnZipOneFile(const ArchiveFileName, FileName, OutputDirectory: String): Boolean;

  function HashFile(const FileName: String): String;
  function ReadFile(const FileName: String): String;
  function WriteFile(const FileName, Contents: String): Boolean;
  function CreateTempFile(const Contents, Prefix: String): String;

  function GetSimbaPath: String;
  function GetDataPath: String;
  function GetIncludePath: String;
  function GetPluginPath: String;
  function GetPluginCopyPath: String;
  function GetFontPath: String;
  function GetScriptPath: String;
  function GetPackagePath: String;
  function GetDumpPath: String;

  procedure CreateBaseDirectories;

implementation

uses
  forms, lazloggerbase, inifiles, fileutil, lazfileutils, zipper, dynlibs, sha1;

function FindFile(var FileName: string; Extension: String; const Directories: array of String): Boolean;
var
  I: Int32;
begin
  Result := False;

  if FileExists(FileName) then
  begin
    FileName := ExpandFileName(FileName);

    Exit(True);
  end;

  for I := 0 to High(Directories) do
    if FileExists(IncludeTrailingPathDelimiter(Directories[I]) + FileName + Extension) then
    begin
      FileName := ExpandFileName(IncludeTrailingPathDelimiter(Directories[I]) + FileName + Extension);

      Exit(True);
    end;
end;

function FindFiles(Directories: TStringArray; WildCard: String; Recursive: Boolean): TStringArray;
var
  I: Integer;
  Path: String;
begin
  Result := Default(TStringArray);
  if Length(Directories) = 0 then
    Exit;

  I := Pos('*', WildCard) - 1;
  if (I > 0) then
  begin
    Path := Copy(Wildcard, 1, I);
    Wildcard := Copy(WildCard, I+1);
  end else
    Path := '';

  for I := 0 to High(Directories) do
    if DirectoryExists(Directories[I]) then
    begin
      Directories[I] := ExpandFileName(ConcatPaths([Directories[I], Path]));

      with FindAllFiles(Directories[I], WildCard, Recursive) do
      try
        Sort(); // sort, else it's dependant on how filesystem orders

        Result := ToStringArray();
        if Length(Result) > 0 then
          Exit;
      finally
        Free();
      end;
    end;
end;

function FindPlugin(var FileName: String; const Directories: array of String): Boolean;
begin
  Result := FindFile(FileName, '', Directories) or
            {$IFDEF CPUAARCH64} 
            FindFile(FileName, '.' + SharedSuffix + '.aarch64', Directories) or
            FindFile(FileName, {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix + '.aarch64', Directories) or
            {$ENDIF}
            FindFile(FileName, '.' + SharedSuffix, Directories) or
            FindFile(FileName, {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix, Directories);
end;

function GetFiles(Path, Ext: string): TStringArray;
var
  SearchRec : TSearchRec;
  c : integer;
begin
  c := 0;
  Path := IncludeTrailingPathDelimiter(Path);
  Ext := Ext.TrimLeft(['.']);

  if FindFirst(Path + '*.' + ext, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Continue;
      inc(c);
      SetLength(Result,c);
      Result[c-1] := SearchRec.Name;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function GetDirectories(Path: string): TStringArray;
var
  SearchRec : TSearchRec;
  c : integer;
begin
  c := 0;
  if FindFirst(Path + '*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name[1] = '.') or ((SearchRec.Attr and faDirectory) <> faDirectory) then
        continue;
      inc(c);
      SetLength(Result,c);
       Result[c-1] := SearchRec.Name;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure UnZipFile(const ArchiveFileName, OutputDirectory: String);
var
  UnZipper: TUnZipper;
begin
  if (not FileExistsUTF8(ArchiveFileName)) then
    raise Exception.CreateFmt('UnZipFile: Archive "%s" does not exist', [ArchiveFileName]);

  UnZipper := TUnZipper.Create();
  try
    UnZipper.FileName := ArchiveFileName;
    UnZipper.OutputPath := OutputDirectory;
    UnZipper.Examine();
    UnZipper.UnZipAllFiles();
  finally
    UnZipper.Free();
  end;
end;

function UnZipOneFile(const ArchiveFileName, FileName, OutputDirectory: String): Boolean;
var
  UnZipper: TUnZipper;
  I: Int32;
begin
  Result := False;

  UnZipper := TUnZipper.Create();
  UnZipper.Files.Add(FileName);

  try
    UnZipper.FileName := ArchiveFileName;
    UnZipper.OutputPath := OutputDirectory;
    UnZipper.Examine();

    for I := 0 to UnZipper.Entries.Count - 1 do
      if (UnZipper.Entries[I].ArchiveFileName = FileName) then
      begin
        UnZipper.UnZipAllFiles();

        Result := True;
        Break;
      end;
  finally
    UnZipper.Free();
  end;
end;

// Make a copy of the plugin to data/plugins/ so we can delete/update if it's loaded
procedure CopyPlugin(var FileName: String);
var
  Hash: String;
begin
  Hash := ConcatPaths([GetPluginCopyPath(), HashFile(FileName) + ExtractFileExt(FileName)]);
  if not FileExists(Hash) then
    CopyFile(FileName, Hash);

  FileName := Hash;
end;

procedure ZipFiles(const ArchiveFileName: String; const Files: TStringArray);
var
  Zipper: TZipper;
  I: Integer;
begin
  if (Length(Files) = 0) then
    raise Exception.Create('ZipFiles: No files to zip');

  Zipper := TZipper.Create;
  try
    Zipper.FileName := ArchiveFileName;
    for I := 0 to High(Files) do
      Zipper.Entries.AddFileEntry(Files[I], Files[I]);

    Zipper.ZipAllFiles();
  finally
    Zipper.Free;
  end;
end;

function HashFile(const FileName: String): String;
begin
  try
    Result := SHA1Print(SHA1File(FileName, 256*256));
  except
    Result := '';
  end;
end;

function ReadFile(const FileName: String): String;
var
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.Read(Result[1], Length(Result));
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

function WriteFile(const FileName, Contents: String): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;

  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    if Stream.Write(Contents[1], Length(Contents)) = Length(Contents) then
      Result := True;
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

function CreateTempFile(const Contents, Prefix: String): String;
var
  Number: Integer = 0;
begin
  Result := Format('%s%s.%d', [GetDataPath(), Prefix, Number]);
  while FileExists(Result) do
  begin
    Inc(Number);

    Result := Format('%s%s.%d', [GetDataPath(), Prefix, Number]);
  end;

  with TStringList.Create() do
  try
    Text := Contents;

    SaveToFile(Result);
  finally
    Free();
  end;
end;

function GetSimbaPath: String;
begin
  Result := IncludeTrailingPathDelimiter(Application.Location);
end;

function GetDataPath: String;
begin
  Result := GetSimbaPath() + 'Data' + DirectorySeparator;
end;

function GetIncludePath: String;
begin
  Result := GetSimbaPath() + 'Includes' + DirectorySeparator;
end;

function GetPluginPath: String;
begin
  Result := GetSimbaPath() + 'Plugins' + DirectorySeparator;
end;

function GetPluginCopyPath: String;
begin
  Result := GetDataPath() + 'plugins' + DirectorySeparator;
end;

function GetFontPath: String;
begin
  Result := GetSimbaPath() + 'Fonts' + DirectorySeparator;
end;

function GetScriptPath: String;
begin
  Result := GetSimbaPath() + 'Scripts' + DirectorySeparator;
end;

function GetPackagePath: String;
begin
  Result := GetDataPath() + 'packages' + DirectorySeparator;
end;

function GetDumpPath: String;
begin
  Result := GetDataPath() + 'dumps' + DirectorySeparator;
end;

procedure CreateBaseDirectories;
var
  Directory: String;
begin
  for Directory in [GetDumpPath(), GetPackagePath(), GetIncludePath(), GetFontPath(), GetScriptPath(), GetPluginPath(), GetPluginCopyPath()] do
  begin
    if DirectoryExists(Directory) then
      Continue;

    ForceDirectories(Directory);
  end;
end;

constructor TMFiles.Create(Owner : TObject);
begin
  inherited Create;
  self.Client := Owner;
  SetLength(Self.MFiles, 0);
  SetLength(Self.FreeSpots, 0);
end;

procedure TMFiles.FreeFileList;
var
  I : integer;
begin
  For I := 0 To High(MFiles) Do
    if MFiles[i].FS <> nil then
    begin
      Writeln(Format('File[%s] has not been freed in the script, freeing it now.',[MFiles[i].Path]));
      try
        MFiles[I].FS.Free;
      except
        Writeln('FreeFileList - Exception when freeing FileStream');
      end;
    end;
  SetLength(MFiles, 0);
  SetLength(FreeSpots, 0);
end;

destructor TMFiles.Destroy;
begin
  FreeFileList;
  inherited;
end;

procedure TMFiles.CheckFileNum(FileNum: integer);
begin
  if(FileNum < 0) or (FileNum >= Length(MFiles)) then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);
end;

function TMFiles.AddFileToManagedList(Path: String; FS: TFileStream; Mode: Integer): Integer;
var
  tFile: TMufasaFile;
begin
  tFile.Path := Path;
  tFile.FS := FS;
  tFile.Mode := Mode;
  tFile.BytesRead := 0;
  if Length(FreeSpots) > 0 then
  begin
    MFiles[FreeSpots[High(FreeSpots)]] := tFile;
    Result := FreeSpots[High(FreeSpots)];
    SetLength(FreeSpots, High(FreeSpots));
  end else
  begin
    SetLength(MFiles, Length(MFiles) + 1);
    MFiles[High(MFiles)] := tFile;
    Result := High(MFiles);
  end;
end;

function TMFiles.SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
begin
  CheckFileNum(FileNum);
  case Origin of
    fsFromBeginning:
                      if(cChars < 0) then
                        raise Exception.CreateFmt('fsFromBeginning takes no negative cChars. (%d)',[cChars]);
    fsFromCurrent:
                  ;
    fsFromEnd:
                  if(cChars > 0) then
                    raise Exception.CreateFmt('fsFromEnd takes no positive cChars. (%d)',[cChars]);
    else
      raise Exception.CreateFmt('Invalid Origin: %d',[Origin]);
  end;

  try
    Result := MFiles[FileNum].FS.Seek(cChars, Origin);
  except
    Writeln('SetFileCharPointer - Exception Occured.');
    Result := File_AccesError;
  end;
  //Result := FileSeek(Files[FileNum].Handle, cChars, Origin);
end;

{/\
  Creates a file for reading/writing.
  Returns the handle (index) to the File Array.
  Returns File_AccesError if unsuccesfull.
/\}

function TMFiles.CreateFile(Path: string): Integer;
var
  FS: TFileStream;
begin
  try
    FS := TFileStream.Create(Path, fmCreate);
    Result := AddFileToManagedList(Path, FS, fmCreate);
  except
    Result := File_AccesError;
    Writeln(Format('CreateFile - Exception. Could not create file: %s',[path]));
  end;
end;

{/\
  Opens a file for reading.
  Returns the handle (index) to the File Array.
  Returns File_AccesError if unsuccesfull.
/\}

function TMFiles.OpenFile(Path: string; Shared: Boolean): Integer;
var
  FS: TFileStream;
  fMode: Integer;
begin
  if Shared then
    fMode := fmOpenRead or fmShareDenyNone
  else
    fMode := fmOpenRead or fmShareExclusive;
  try
      FS := TFileStream.Create(Path, fMode)
  except
    Result := File_AccesError;
    Writeln(Format('OpenFile - Exception. Could not open file: %s',[path]));
    Exit;
  end;
  Result := AddFileToManagedList(Path, FS, fMode);
end;

function TMFiles.AppendFile(Path: string): Integer;
var
  FS: TFileStream;
  fMode: Integer;
begin
  fMode := fmOpenReadWrite;
  if not FileExists(Path) then
    fMode := fMode or fmCreate;
  try
    FS := TFileStream.Create(Path, fMode);
    FS.Seek(0, fsFromEnd);
    Result := AddFileToManagedList(Path, FS, fMode);
  except
    Result := File_AccesError;
    Writeln(Format('AppendFile - Exception. Could not create file: %s',[path]));
  end;
end;

{/\
  Reads key from INI file
/\}

function TMFiles.ReadINI(const Section, KeyName: string; FileName: string): string;
begin
  FileName := ExpandFileNameUTF8(FileName);

  with TINIFile.Create(FileName, True) do
    try
      Result := ReadString(Section, KeyName, '');
    finally
      Free;
  end;
end;

{/\
  Deletes a key from INI file
/\}

procedure TMFiles.DeleteINI(const Section, KeyName : string; FileName : string);
begin 
  FileName := ExpandFileNameUTF8(FileName);

  with TIniFile.Create(FileName, True) do
    try
      if KeyName = '' then
	    EraseSection(Section)
	  else
		DeleteKey(Section, KeyName);
    finally
      Free;
  end;
end;

{/\
  Writes a key to INI file
/\}

procedure TMFiles.WriteINI(const Section, KeyName, NewString : string; FileName : string);
begin
  FileName := ExpandFileNameUTF8(FileName);

  with TINIFile.Create(FileName, True) do
    try
	  WriteString(Section, KeyName, NewString);
    finally
	  Free;
  end;
end;

{/\
  Opens a file for writing. And deletes the contents.
  Returns the handle (index) to the File Array.
  Returns File_AccesError if unsuccesfull.
/\}

function TMFiles.RewriteFile(Path: string; Shared: Boolean): Integer;
var
  FS: TFileStream;
  fMode: Integer;
begin
  if Shared then
    fMode := fmOpenReadWrite or fmShareDenyNone  or fmCreate
  else
    fMode := fmOpenReadWrite or fmShareDenyWrite or fmShareDenyRead or fmCreate;
  try
    FS := TFileStream.Create(Path, fMode);
    FS.Size:=0;
    Result := AddFileToManagedList(Path, FS, fMode);
  except
    Result := File_AccesError;
    WriteLn(Format('ReWriteFile - Exception. Could not create file: %s',[path]));
  end;
end;

function TMFiles.DeleteFile(Filename: string): Boolean;
begin
  Result := DeleteFileUTF8(Filename);
end;

function TMFiles.RenameFile(OldName, NewName: string): Boolean;
begin
  Result := RenameFileUTF8(OldName, NewName);
end;

{/\
  Free's the given File at the given index.
/\}
procedure TMFiles.CloseFile(FileNum: Integer);
begin
  CheckFileNum(filenum);
  try
    MFiles[FileNum].FS.Free;
    MFiles[FileNum].FS := nil;
    SetLength(FreeSpots, Length(FreeSpots) + 1);
    FreeSpots[High(FreeSpots)] := FileNum;
  except
    WriteLn(Format('CloseFile, exception when freeing the file: %d',[filenum]));
  end;
end;

{/\
  Returns true if the BytesRead of the given FileNum (Index) has been reached.
  Also returns true if the FileNum is not valid.
/\}

function TMFiles.EndOfFile(FileNum: Integer): Boolean;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    WriteLn(format('EndOfFile: Invalid Internal Handle of File: %d',[filenum]));
    Result := True;
    Exit;
  end;
  Result := FilePointerPos(FileNum) >= FileSizeMuf(FileNum);
end;

{/\
  Returns the FileSize of the given index (FileNum)
/\}

function TMFiles.FileSizeMuf(FileNum: Integer): LongInt;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    WriteLn(format('FileSize: Invalid Internal Handle of File: %d',[filenum]));
    Result := File_AccesError;
    Exit;
  end;

  Result := MFiles[FileNum].FS.Size;
end;

function TMFiles.FilePointerPos(FileNum: Integer): Integer;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    WriteLn(format('FilePointerPos: Invalid Internal Handle of File: %d',[filenum]));
    Result := File_AccesError;
    Exit;
  end;
  try
    Result := MFiles[FileNum].FS.Seek(0, fsFromCurrent);
  except
    WriteLn('Exception in FilePointerPos');
  end;
end;

{/\
  Reads x numbers of characters from a file, and stores it into s.
/\}

function TMFiles.ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    WriteLn(format('ReadFileString: Invalid Internal Handle of File: %d',[filenum]));
    Exit;
  end;

  SetLength(S, X);
  Result := MFiles[FileNum].FS.Read(S[1], x) = x;
  {Files[FileNum].BytesRead := Files[FileNum].BytesRead + X;
  FileRead(Files[FileNum].Handle, S[1], X);
  SetLength(S, X); }
end;

{/\
  Writes s in the given File.
/\}

function TMFiles.WriteFileString(FileNum: Integer;const  s: string): Boolean;
begin
  result := false;
  CheckFileNum(filenum);
  if(MFiles[FileNum].FS = nil) then
  begin
    WriteLn(format('WriteFileString: Invalid Internal Handle of File: %d',[filenum]));
    Exit;
  end;
  if (MFiles[FileNum].Mode and (fmOpenWrite or fmOpenReadWrite)) = 0 then //Checks if we have write rights..
    exit;
  try
    Result := MFiles[FileNum].FS.Write(S[1], Length(S)) <> 1;
  except
    WriteLn('Exception - WriteFileString.');
    Result := False;
  end;
end;

end.

