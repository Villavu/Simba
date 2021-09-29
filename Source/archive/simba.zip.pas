{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.zip;

{$i simba.inc}

interface

uses
  classes, sysutils, zipper,
  simba.archive;

type
  TSimbaZipExtractor = class(TSimbaArchiveExtractor)
  protected
    FRoot: String;

    procedure DoProgress(Sender: TObject; const Position, Size: Int64);
    procedure DoCreateInputStream(Sender: TObject; var Stream: TStream);
    procedure DoCreateFileStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
  public
    procedure Extract; override;
  end;

implementation

uses
  LazFileUtils;

procedure TSimbaZipExtractor.DoProgress(Sender: TObject; const Position, Size: Int64);
begin
  if (FOnProgress <> nil) then
    FOnProgress(Self, '', Position, Size);
end;

procedure TSimbaZipExtractor.DoCreateInputStream(Sender: TObject; var Stream: TStream);
begin
  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FInputStream, 0);
end;

procedure TSimbaZipExtractor.DoCreateFileStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
var
  I: Int32;
  ArchiveFileName, DiskFileName: String;
begin
  if (Item.Index = 0) then
    with Sender as TUnZipper do
    begin
      FRoot := Entries[0].ArchiveFileName;

      for I := 0 to Entries.Count - 1 do
        if not Entries[I].ArchiveFileName.StartsWith(FRoot) then
          FRoot := '';
    end;

  ArchiveFileName := Copy(Item.ArchiveFileName, Length(FRoot) + 1);
  if Flat then
    DiskFileName := ExpandFileName(OutputPath + ExtractFileName(ArchiveFileName))
  else
    DiskFileName := ExpandFileName(OutputPath + ArchiveFileName);

  if (ArchiveFileName = FRoot) then
  begin
    Stream := TMemoryStream.Create();

    Exit;
  end;

  for I := 0 to FIgnoreList.Count - 1 do
    if (FIgnoreList[I] = ArchiveFileName) or FileIsInPath(ArchiveFileName, FIgnoreList[I]) then
    begin
      Stream := TMemoryStream.Create();

      Exit;
    end;

  if Item.IsDirectory then
  begin
    ForceDirectories(DiskFileName);

    Exit;
  end;

  Stream := TFileStream.Create(DiskFileName, fmCreate);
end;

procedure TSimbaZipExtractor.Extract;
var
  UnZipper: TUnZipper;
begin
  inherited Extract();

  UnZipper := TUnZipper.Create();
  UnZipper.OutputPath := FOutputPath;
  UnZipper.OnOpenInputStream := @DoCreateInputStream;
  UnZipper.OnCreateStream := @DoCreateFileStream;
  UnZipper.OnProgressEx := @DoProgress;
  UnZipper.Flat := FFlat;

  try
    UnZipper.UnZipAllFiles();
  finally
    UnZipper.Free();
  end;
end;

end.

