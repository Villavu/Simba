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
    FExtractingFinished: TNotifyEvent;
    FIgnoreList: TStringList;
    FCurrentFile: String;

    FDirectoryZipped: Boolean;

    procedure DoCreateInputStream(Sender: TObject; var Stream: TStream);
    procedure DoCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure DoProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
    procedure DoStartFile(Sender: TObject; const AFileName: String);
  public
    property InputStream: TStream read FInputStream write FInputStream;
    property OutputPath: String read FOutputPath write FOutputPath;
    property Flat: Boolean read FFlat write FFlat;
    property OnProgress: TZipProgressEvent read FOnProgress write FOnProgress;
    property OnExtractingFinished: TNotifyEvent read FExtractingFinished write FExtractingFinished;
    property IgnoreList: TStringList read FIgnoreList;

    procedure Extract;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  fileutil, lazfileutils,
  simba.mufasatypes;

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
var
  UnZipper: TUnZipper;
begin
  FOutputPath := CleanAndExpandDirectory(FOutputPath);

  UnZipper := TUnZipper.Create();
  UnZipper.OutputPath := FOutputPath;
  UnZipper.OnOpenInputStream := @DoCreateInputStream;
  UnZipper.OnCreateStream := @DoCreateStream;
  UnZipper.OnProgressEx := @DoProgress;
  UnZipper.OnStartFile := @DoStartFile;
  UnZipper.Flat := FFlat;
  UnZipper.Examine();

  FDirectoryZipped := (UnZipper.Entries.Count > 1) and UnZipper.Entries[0].IsDirectory() and UnZipper.Entries[1].ArchiveFileName.StartsWith(UnZipper.Entries[0].ArchiveFileName);

  UnZipper.UnZipAllFiles();
  if Assigned(FExtractingFinished) then
    FExtractingFinished(Self);

  UnZipper.Free();
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

procedure TSimbaZipExtractor.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

  function IsIgnoreFile(const FileName: String): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to FIgnoreList.Count - 1 do
      if (FIgnoreList[I] = FileName.TrimRight(['/'])) or (FIgnoreList[I] = FileName) or FileIsInPath(FileName, FIgnoreList[I]) then
        Exit(True);

    Result := False;
  end;

var
  FileName: String;
begin
  if FDirectoryZipped then
    FileName := AItem.ArchiveFileName.After('/')
  else
    FileName := AItem.ArchiveFileName;

  if IsIgnoreFile(AItem.ArchiveFileName) then
  begin
    AStream := TMemoryStream.Create();
    Exit;
  end;

  if FFlat then
    FileName := CleanAndExpandFilename(FOutputPath + ExtractFileName(FileName))
  else
    FileName := CleanAndExpandFilename(FOutputPath + FileName);

  ForceDirectories(ExtractFileDir(FileName));
  if (not AItem.IsDirectory) then
    AStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
end;

end.

