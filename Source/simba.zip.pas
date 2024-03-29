{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.zip;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Zipper,
  simba.base;

type
  TSimbaZipExtractor = class
  public
  type
    TProgressEvent = procedure(Sender: TObject; FileName: String; Percent: Double) of object;
  protected
    FInputStream: TStream;
    FOutputPath: String;
    FFlat: Boolean;
    FOnProgress: TProgressEvent;
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
    property IgnoreList: TStringList read FIgnoreList;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnExtractingFinished: TNotifyEvent read FExtractingFinished write FExtractingFinished;

    procedure Extract;

    constructor Create;
    destructor Destroy; override;
  end;

function ZipFiles(ZipFileName: String; Files: TStringArray): Boolean;
function ZipReadEntries(FileName: String): TStringArray;
function ZipAppend(FileName: String; Entry, FileContents: String): Boolean;

function ZipExtractEntries(FileName, OutputDir: String; Entries: TStringArray): Integer;
function ZipExtractEntry(FileName, Entry, OutputDir: String): Boolean; overload;
function ZipExtractEntry(FileName: String; Entry: String): TMemoryStream; overload;
function ZipExtract(ZipFileName, OutputDir: String): Boolean;

function ZipHasEntryCrc(ZipFileName: String; Crc32: UInt32): Boolean;

implementation

uses
  fileutil, lazfileutils,
  simba.vartype_string;

// Zip Extractor
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

// Zip Files
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

// ZipReadEntries
function ZipReadEntries(FileName: String): TStringArray;
var
  UnZipper: TUnZipper;
  I: Integer;
begin
  Result := [];

  if FileExists(FileName) then
  begin
    UnZipper := TUnZipper.Create();
    try
      UnZipper.FileName := FileName;
      UnZipper.Examine();

      SetLength(Result, UnZipper.Entries.Count);
      for I := 0 to UnZipper.Entries.Count - 1 do
        Result[I] := UnZipper.Entries[I].ArchiveFileName;
    except
    end;
    UnZipper.Free();
  end;
end;

// ZipAppend
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

function ZipAppend(FileName: String; Entry, FileContents: String): Boolean;
begin
  Result := True;

  with TZipAppender.Create(FileName) do
  try
    Add(Entry, FileContents);
  finally
    Free();
  end;
end;

// Zip Entries
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

// ZipExtractEntries
type
  TSimpleZipExtract = class(TUnZipper)
  protected
    FExtractCount: Integer;

    procedure DoStartFile(Sender: TObject; const AFileName: String);
  public
    constructor Create;

    property ExtractCount: Integer read FExtractCount;
  end;

procedure TSimpleZipExtract.DoStartFile(Sender: TObject; const AFileName: String);
begin
  Inc(FExtractCount);
end;

constructor TSimpleZipExtract.Create;
begin
  inherited Create();

  OnStartFile := @DoStartFile;
end;

function ZipExtractEntries(FileName, OutputDir: String; Entries: TStringArray): Integer;
var
  UnZipper: TSimpleZipExtract;
begin
  Result := 0;

  if FileExists(FileName) then
  begin
    UnZipper := TSimpleZipExtract.Create();
    try
      UnZipper.Files.AddStrings(Entries);
      UnZipper.FileName := FileName;
      UnZipper.OutputPath := OutputDir;
      UnZipper.UnZipAllFiles();

      Result := UnZipper.ExtractCount;
    finally
      UnZipper.Free();
    end;
  end;
end;

function ZipExtractEntry(FileName, Entry, OutputDir: String): Boolean;
begin
  Result := ZipExtractEntries(FileName, OutputDir, [Entry]) = 1;
end;

// ZipExtractEntry
type
  TSimpleZipExtractStream = class(TUnZipper)
  protected
    procedure DoCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure DoDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  public
    Stream: TMemoryStream;

    constructor Create;
  end;

procedure TSimpleZipExtractStream.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create();
end;

procedure TSimpleZipExtractStream.DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  Stream := TMemoryStream(AStream);
  Stream.Position := 0;

  AStream := nil;
end;

constructor TSimpleZipExtractStream.Create;
begin
  inherited Create();

  OnCreateStream := @DoCreateStream;
  OnDoneStream := @DoDoneStream;
end;

function ZipExtractEntry(FileName: String; Entry: String): TMemoryStream;
var
  UnZipper: TSimpleZipExtractStream;
begin
  Result := nil;

  if FileExists(FileName) then
  begin
    UnZipper := TSimpleZipExtractStream.Create();
    try
      UnZipper.Files.Add(Entry);
      UnZipper.FileName := FileName;
      UnZipper.UnZipAllFiles();

      Result := UnZipper.Stream;

    finally
      UnZipper.Free();
    end;

    if (Result = nil) then
      SimbaException('Entry "%s" not found', [Entry]);
  end else
    SimbaException('Zip "%s" not found', [FileName]);
end;

// ZipExtract
function ZipExtract(ZipFileName, OutputDir: String): Boolean;
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

// ZipHasEntryCrc
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

end.

