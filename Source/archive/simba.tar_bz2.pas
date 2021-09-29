{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.tar_bz2;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.tar;

type
  TSimbaTarBZ2Extractor = class(TSimbaTarExtractor)
  protected
    function Decompress: TMemoryStream; override;
  end;

implementation

uses
  bzip2stream;

function TSimbaTarBZ2Extractor.Decompress: TMemoryStream;
var
  Stream: TDecompressBzip2Stream;
  Count: Int32;
  Buffer: array[0..4095] of Byte;
begin
  Result := TMemoryStream.Create();

  Stream := TDecompressBzip2Stream.Create(FInputStream);
  Stream.SourceOwner := False;

  try
    repeat
      Count := Stream.Read(Buffer[0], Length(Buffer));
      if Count > 0 then
        Result.Write(Buffer[0], Count);

      if FOnProgress <> nil then
        FOnProgress(Self, 'Decompressing', Result.Position, 0);
    until Count = 0;
  finally
    Stream.Free();
  end;
end;

end.

