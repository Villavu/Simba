unit simba.tar_gz;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.tar;

type
  TSimbaTarGZExtractor = class(TSimbaTarExtractor)
  protected
    function Decompress: TMemoryStream; override;
  end;

implementation

uses
  simba.gz_stream;

function TSimbaTarGZExtractor.Decompress: TMemoryStream;
var
  Stream: TGZFileStream;
  Count: Int32;
  Buffer: array[0..4095] of Byte;
begin
  Result := TMemoryStream.Create();

  Stream := TGZFileStream.Create(FInputStream, False);
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

