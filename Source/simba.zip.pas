{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.zip;

{$i simba.inc}

interface

uses
  classes, sysutils, zipper;

type
  TZipProgressEvent = procedure(Sender: TObject; FileName: String; Percent: Double) of object;

  TSimbaZipExtractor = class
  protected
    FInputStream: TStream;
    FOutputPath: String;
    FFlat: Boolean;
    FOnProgress: TZipProgressEvent;
    FOnCopying: TZipProgressEvent;
    FExtractingFinished: TNotifyEvent;
    FCopyingFinished: TNotifyEvent;
    FIgnoreList: TStringList;
    FCurrentFile: String;

    procedure DoCreateInputStream(Sender: TObject; var Stream: TStream);
    procedure DoProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
    procedure DoStartFile(Sender: TObject; const AFileName: String);
  public
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputPath: String read FOutputPath write FOutputPath;
    property Flat: Boolean read FFlat write FFlat;
    property OnProgress: TZipProgressEvent read FOnProgress write FOnProgress;
    property OnCopying: TZipProgressEvent read FOnCopying write FOnCopying;
    property OnCopyingFinished: TNotifyEvent read FCopyingFinished write FCopyingFinished;
    property OnExtractingFinished: TNotifyEvent read FExtractingFinished write FExtractingFinished;
    property IgnoreList: TStringList read FIgnoreList;

    procedure Extract;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  fileutil, lazfileutils, strutils,
  simba.files;

type
  TCopyDirectory = class
  protected
    FSearcher: TFileSearcher;

    FSourceDir: String;
    FTargetDir: String;
    FIgnoreList: TStringList;

    FFileIndex: Integer;
    FTotalFileCount: Integer;
    FCopyFailedCount: Integer;

    FOnProgress: TZipProgressEvent;

    procedure DoDirectoryFound(Sender: TFileIterator);
    procedure DoFileFound(Sender: TFileIterator);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(SourceDir, TargetDir: String);

    property IgnoreList: TStringList read FIgnoreList;
    property TotalFileCount: Integer read FTotalFileCount write FTotalFileCount;
    property OnProgress: TZipProgressEvent read FOnProgress write FOnProgress;
  end;

procedure TCopyDirectory.DoDirectoryFound(Sender: TFileIterator);
begin
  Inc(FFileIndex);
end;

procedure TCopyDirectory.DoFileFound(Sender: TFileIterator);

  function isIgnore(FileName: String): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to FIgnoreList.Count - 1 do
      if (FIgnoreList[I] = FileName) or FileIsInPath(FileName, FIgnoreList[I]) then
        Exit(True);

    Result := False;
  end;

var
  DestPath, RelativePath: string;
begin
  Inc(FFileIndex);

  RelativePath := Copy(Sender.FileName, Length(FSourceDir) + 1);
  if (not isIgnore(RelativePath)) then
  begin
    DestPath := StringReplace(Sender.FileName, FSourceDir, FTargetDir, []);
    if not CopyFile(Sender.FileName, DestPath, [cffCreateDestDirectory]) then
      Inc(FCopyFailedCount);
  end;

  if (FOnProgress <> nil) then
  begin
    if (FTotalFileCount > 0) then
      FOnProgress(Self, Sender.FileName, FFileIndex / FTotalFileCount * 100.0)
    else
      FOnProgress(Self, Sender.FileName, FFileIndex)
  end;
end;

constructor TCopyDirectory.Create;
begin
  inherited Create();

  FSearcher := TFileSearcher.Create();
  FSearcher.OnDirectoryFound := @DoDirectoryFound;
  FSearcher.OnFileFound := @DoFileFound;

  FIgnoreList := TStringList.Create();
end;

destructor TCopyDirectory.Destroy;
begin
  if (FSearcher <> nil) then
    FreeAndNil(FSearcher);
  if (FIgnoreList <> nil) then
    FreeAndNil(FIgnoreList);

  inherited Destroy();
end;

procedure TCopyDirectory.Execute(SourceDir, TargetDir: String);
var
  I: Integer;
begin
  FFileIndex := 0;
  FSourceDir := CleanAndExpandFilename(SetDirSeparators(SourceDir));
  FTargetDir := CleanAndExpandFilename(SetDirSeparators(TargetDir));

  // Don't even try to copy to a subdirectory of SourceDir.
  // append a pathedelim, otherwise CopyDirTree('/home/user/foo','/home/user/foobar') will fail at this point. Issue #0038644
  {$ifdef CaseInsensitiveFilenames}
  if AnsiStartsText(AppendPathDelim(FSourceDir), AppendPathDelim(FTargetDir)) then Exit;
  {$ELSE}
  if AnsiStartsStr(AppendPathDelim(FSourceDir), AppendPathDelim(FTargetDir)) then Exit;
  {$ENDIF}

  ForceDirectories(FTargetDir);
  for I := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[I] := IncludeLeadingPathDelimiter(SetDirSeparators(FIgnoreList[I]));

  FSearcher.Search(FSourceDir);
end;

procedure TSimbaZipExtractor.DoProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
begin
  if (FOnProgress <> nil) then
    FOnProgress(Self, FCurrentFile, ATotPos / ATotSize * 100.0);
end;

procedure TSimbaZipExtractor.DoStartFile(Sender: TObject; const AFileName: String);
begin
  FCurrentFile := AFileName;
end;

procedure TSimbaZipExtractor.Extract;

  function GetTopDirectory(Dir: String): String;
  var
    SearchRec: TSearchRec;
  begin
    Result := '';

    if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        if (Result <> '') then
        begin
          Result := Dir;
          Exit;
        end;

        Result := SearchRec.Name;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;

    Result := ConcatPaths([Dir, Result]);
  end;

var
  UnZipper: TUnZipper;
begin
  UnZipper := TUnZipper.Create();
  UnZipper.OutputPath := GetTempFileName(GetDataPath(), 'zip');
  UnZipper.OnOpenInputStream := @DoCreateInputStream;
  UnZipper.OnProgressEx := @DoProgress;
  UnZipper.OnStartFile := @DoStartFile;
  UnZipper.Flat := FFlat;

  try
    UnZipper.UnZipAllFiles();
    if Assigned(FOnProgress)         then FOnProgress(Self, '', 100);
    if Assigned(FExtractingFinished) then FExtractingFinished(Self);

    with TCopyDirectory.Create() do
    try
      OnProgress := FOnCopying;

      TotalFileCount := Unzipper.Entries.Count - 1;
      IgnoreList.AddStrings(Self.IgnoreList, True);

      Execute(GetTopDirectory(UnZipper.OutputPath), FOutputPath);

      DeleteDirectory(UnZipper.OutputPath, False);
    finally
      Free();
    end;

    if Assigned(FCopyingFinished) then FCopyingFinished(Self);
  finally
    UnZipper.Free();
  end;
end;

constructor TSimbaZipExtractor.Create;
begin
  inherited Create();

  FIgnoreList := TStringList.Create();
end;

destructor TSimbaZipExtractor.Destroy;
begin
  if (FIgnoreList <> nil) then
    FreeAndNil(FIgnoreList);

  inherited Destroy();
end;

procedure TSimbaZipExtractor.DoCreateInputStream(Sender: TObject; var Stream: TStream);
begin
  FInputStream.Position := 0;

  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FInputStream, 0);
end;

end.

