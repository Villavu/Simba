unit simba.zip;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, zipper,
  simba.archive;

type
  TSimbaZipExtractor = class(TSimbaArchiveExtractor)
  protected
    FDirectory: String;
    FIsDirectory: Boolean;

    procedure CreateFileStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
    procedure CreateInputStream(Sender: TObject; var Stream: TStream);
  public
    procedure Extract; override;
  end;

implementation

procedure TSimbaZipExtractor.CreateFileStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
begin
  if FFlat then
    Item.DiskFileName := FOutputPath + ExtractFileName(Item.ArchiveFileName)
  else
  if FIsDirectory then
    Item.DiskFileName := FOutputPath + Item.ArchiveFileName.SubString(Length(FDirectory))
  else
    Item.DiskFileName := FOutputPath + Item.ArchiveFileName;

  ForceDirectories(ExtractFilePath(Item.DiskFileName));

  if Item.IsDirectory then
    Stream := TStringStream.Create('')
  else
    Stream := TFileStream.Create(Item.DiskFileName, fmCreate);

  FPosition := FPosition + Item.Size;

  if (FOnProgress <> nil) then
    FOnProgress(Self, Item.DiskFileName, FPosition, FSize);
end;

procedure TSimbaZipExtractor.CreateInputStream(Sender: TObject; var Stream: TStream);
begin
  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FInputStream, 0);
end;

procedure TSimbaZipExtractor.Extract;
var
  Zipper: TUnZipper;
  i: Int32;
begin
  inherited Extract();

  FOutputPath := SetDirSeparators(IncludeTrailingPathDelimiter(FOutputPath));
  FIsDirectory := False;
  FDirectory := '';

  Zipper := TUnZipper.Create();
  Zipper.OnCreateStream := @CreateFileStream;
  Zipper.OnOpenInputStream := @CreateInputStream;

  try
    Zipper.Examine();

    if Zipper.Entries.Count > 0 then
    begin
      FDirectory := Zipper.Entries[0].ArchiveFileName;

      for i := 0 to Zipper.Entries.Count - 1 do
      begin
        FSize := FSize + Zipper.Entries[i].Size;
        if Copy(Zipper.Entries[i].ArchiveFileName, 1, Length(FDirectory)) <> FDirectory then
          FDirectory := '';
      end;

      FIsDirectory := FDirectory <> '';
    end;

    Zipper.UnZipAllFiles();
  finally
    Zipper.Free();
  end;
end;

end.

