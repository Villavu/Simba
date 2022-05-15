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
  simba.archive, simba.files;

type
  TSimbaZipExtractor = class(TSimbaArchiveExtractor)
  protected
    procedure DoCreateInputStream(Sender: TObject; var Stream: TStream);
    procedure DoProgress(Sender: TObject; const Position, Size: Int64);
  public
    procedure Extract; override;
  end;

implementation

procedure TSimbaZipExtractor.DoProgress(Sender: TObject; const Position, Size: Int64);
begin
  if (FOnProgress <> nil) then
    FOnProgress(Self, '', Position, Size);
end;

procedure TSimbaZipExtractor.Extract;
var
  UnZipper: TUnZipper;
begin
  UnZipper := TUnZipper.Create();
  UnZipper.OutputPath := GetTempFileName(GetDataPath(), ExtractFileName(ExcludeTrailingPathDelimiter(FOutputPath)));
  UnZipper.OnOpenInputStream := @DoCreateInputStream;
  UnZipper.OnProgressEx := @DoProgress;
  UnZipper.Flat := FFlat;

  try
    UnZipper.UnZipAllFiles();

    CopyToOutputPath(UnZipper.OutputPath);
  finally
    UnZipper.Free();
  end;
end;

procedure TSimbaZipExtractor.DoCreateInputStream(Sender: TObject; var Stream: TStream);
begin
  FInputStream.Position := 0;

  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FInputStream, 0);
end;

end.

