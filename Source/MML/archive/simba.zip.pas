unit simba.zip;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, zipper,
  simba.archive;

type
  TSimbaZipExtractor = class(TSimbaArchiveExtractor)
  protected
    FRootIsDirectory: Int32;

    procedure DoProgress(Sender : TObject; const Position, Size: Int64);
    procedure DoCreateInputStream(Sender: TObject; var Stream: TStream);
    procedure DoCreateFileStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
  public
    procedure Extract; override;
  end;

implementation

procedure TSimbaZipExtractor.DoProgress(Sender: TObject; const Position, Size: Int64);
begin
  if (OnProgress <> nil) then
    OnProgress(Self, '', Position, Size);
end;

procedure TSimbaZipExtractor.DoCreateInputStream(Sender: TObject; var Stream: TStream);
begin
  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FInputStream, 0);
end;

procedure TSimbaZipExtractor.DoCreateFileStream(Sender: TObject; var Stream: TStream; Item: TFullZipFileEntry);
var
  FileName: String;
begin
  if FFlat then
  begin
    if not Item.IsDirectory then
      Stream := TFileStream.Create(FOutputPath + ExtractFileName(Item.DiskFileName), fmCreate);
  end else
  begin
    FileName := FOutputPath + Copy(Item.DiskFileName, FRootIsDirectory, Length(Item.DiskFileName));
    if Item.IsDirectory then
      ForceDirectories(FileName)
    else
      Stream := TFileStream.Create(FileName, fmCreate);
  end;
end;

procedure TSimbaZipExtractor.Extract;
var
  Zipper: TUnZipper;
  I: Int32;
begin
  inherited Extract();

  Zipper := TUnZipper.Create();
  Zipper.OutputPath := FOutputPath;
  Zipper.OnOpenInputStream := @DoCreateInputStream;
  Zipper.OnCreateStream := @DoCreateFileStream;
  Zipper.OnProgressEx := @DoProgress;

  ForceDirectories(FOutputPath);

  try
    Zipper.Examine();

    if (Zipper.Entries.Count > 0) then
    begin
      FRootIsDirectory := Length(Zipper.Entries[0].ArchiveFileName) + 1;

      for I := 1 to Zipper.Entries.Count - 1 do
        if not Zipper.Entries[I].ArchiveFileName.StartsWith(Zipper.Entries[0].ArchiveFileName) then
        begin
          FRootIsDirectory := 0;

          Break;
        end;
    end;

    Zipper.UnZipAllFiles();
  finally
    Zipper.Free();
  end;
end;

end.

