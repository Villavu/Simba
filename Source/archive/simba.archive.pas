{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.archive;

{$i simba.inc}

interface

uses
  classes, sysutils;

type
  TSimbaArchiveExtractor = class
  public
  type
    TProgressEvent = procedure(Sender: TObject; FileName: String; Position, Size: Int64) of object;
  protected
    FInputStream: TStream;
    FOutputPath: String;
    FFlat: Boolean;
    FOnProgress: TProgressEvent;
    FIgnoreList: TStringList;

    procedure CopyToOutputPath(SourceDir: String; RemoveSourceDir: Boolean = True);
  public
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputPath: String read FOutputPath write FOutputPath;
    property Flat: Boolean read FFlat write FFlat;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property IgnoreList: TStringList read FIgnoreList;

    procedure Extract; virtual; abstract;

    constructor Create;
    destructor Destroy; override;
  end;

  TSimbaArchiveExtractorClass = class of TSimbaArchiveExtractor;

implementation

uses
  fileutil, lazfileutils, strutils;

type
  TCopyDirTree = class(TFileSearcher)
  protected
    procedure DoFileFound; override;
  public
    FIgnoreList: TStringList;
    FSourceDir: String;
    FTargetDir: String;
    FFlags: TCopyFileFlags;
    FCopyFailedCount: Integer;

    constructor Create;
    destructor Destroy; override;
  end;

procedure TCopyDirTree.DoFileFound;
var
  DestPath, RelativePath: string;
begin
  RelativePath := Copy(FileName, Length(FSourceDir) + 1);
  if (FIgnoreList.IndexOf(RelativePath) > -1) then
    Exit;

  DestPath := StringReplace(FileName, FSourceDir, FTargetDir, []);
  if not CopyFile(FileName, DestPath, FFlags) then
    Inc(FCopyFailedCount);
end;

constructor TCopyDirTree.Create;
begin
  inherited Create();

  FIgnoreList := TStringList.Create();
end;

destructor TCopyDirTree.Destroy;
begin
  FreeAndNil(FIgnoreList);

  inherited Destroy();
end;

function CopyDir(IgnoreList: TStringList; const SourceDir, TargetDir: string): Boolean;
var
  Searcher: TCopyDirTree;
  I: Integer;
begin
  Result := False;

  ForceDirectories(TargetDir);

  Searcher := TCopyDirTree.Create();
  Searcher.FIgnoreList.AddStrings(IgnoreList);
  for I := 0 to Searcher.FIgnoreList.Count - 1 do
    Searcher.FIgnoreList[I] := IncludeLeadingPathDelimiter(SwitchPathDelims(Searcher.FIgnoreList[I], True));
  Searcher.FIgnoreList.Sorted := True;

  try
    // Destination directories are always created. User setting has no effect!
    Searcher.FFlags := [cffCreateDestDirectory];
    Searcher.FSourceDir := TrimFilename(SetDirSeparators(SourceDir));
    Searcher.FTargetDir := TrimFilename(SetDirSeparators(TargetDir));

    // Don't even try to copy to a subdirectory of SourceDir.
    //append a pathedelim, otherwise CopyDirTree('/home/user/foo','/home/user/foobar') will fail at this point. Issue #0038644
    {$ifdef CaseInsensitiveFilenames}
      if AnsiStartsText(AppendPathDelim(Searcher.FSourceDir), AppendPathDelim(Searcher.FTargetDir)) then Exit;
    {$ELSE}
      if AnsiStartsStr(AppendPathDelim(Searcher.FSourceDir), AppendPathDelim(Searcher.FTargetDir)) then Exit;
    {$ENDIF}
    Searcher.Search(SourceDir);

    Result := Searcher.FCopyFailedCount = 0;
  finally
    Searcher.Free;
  end;
end;

procedure TSimbaArchiveExtractor.CopyToOutputPath(SourceDir: String; RemoveSourceDir: Boolean);

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

begin
  FOutputPath := IncludeTrailingPathDelimiter(ExpandFileName(FOutputPath));
  if (FOnProgress <> nil) then
    FOnProgress(Self, 'Copying...', -1, -1);

  CopyDir(FIgnoreList, GetTopDirectory(SourceDir), FOutputPath);
  if RemoveSourceDir then
    DeleteDirectory(SourceDir, False);
end;

constructor TSimbaArchiveExtractor.Create;
begin
  FIgnoreList := TStringList.Create();
end;

destructor TSimbaArchiveExtractor.Destroy;
begin
  if (FIgnoreList <> nil) then
    FIgnoreList.Free();

  inherited Destroy();
end;

end.

