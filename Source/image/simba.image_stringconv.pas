{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Save/Load bitmaps from/to a base64 string.

  Channels (B,G,R,A) are split and compressed with zlib individually for better compression.
}
unit simba.image_stringconv;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image;

const
  HeaderPrefix = 'IMG:';

type
  TImageStringHeader = packed record
    Version: Integer;
    Width, Height: Integer;
    ChannelSize: record
      B,G,R,A: Integer;
    end;
    Name: String[128];
  end;

procedure SimbaImage_FromString(Image: TSimbaImage; Str: String);
function SimbaImage_ToString(Image: TSimbaImage): String;

implementation

uses
  ZStream,
  simba.encoding;

procedure SimbaImage_FromString(Image: TSimbaImage; Str: String);

  procedure DecompressChannel(SourceStream: TStream; SourceSize: Integer; ChannelOffset: Integer);
  var
    Stream: TMemoryStream;
    DecompressStream: Tdecompressionstream;
    Buffer: array[0..	16384-1] of Byte;
    I, Count, DataIndex: Integer;
  begin
    Stream := nil;
    DecompressStream := nil;
    try
      Stream := TMemoryStream.Create();
      Stream.CopyFrom(SourceStream, SourceSize);
      Stream.Position := 0;

      DecompressStream := Tdecompressionstream.create(Stream);

      DataIndex := 0;
      repeat
        Count := DecompressStream.Read(Buffer[0], Length(Buffer));
        for I := 0 to Count - 1 do
          PByte(@Image.Data[DataIndex + I].B + ChannelOffset)^ := Buffer[I];
        Inc(DataIndex, Count);
      until (Count = 0);
    finally
      Stream.Free();
      DecompressStream.Free();
    end;
  end;

var
  Stream: TStringStream;
  Header: TImageStringHeader;
begin
  if not Str.StartsWith(HeaderPrefix, True) then
    SimbaException('TImage.LoadFromString: Invalid string, it should begin with "IMG:"');
  Str := Str.After(HeaderPrefix);

  Stream := nil;
  try
    Stream := TStringStream.Create(BaseDecode(BaseEncoding.b64, Str));
    Stream.Read(Header, SizeOf(TImageStringHeader));

    Image.SetSize(Header.Width, Header.Height);
    Image.Name := Header.Name;

    DecompressChannel(Stream, Header.ChannelSize.B, 0);
    DecompressChannel(Stream, Header.ChannelSize.G, 1);
    DecompressChannel(Stream, Header.ChannelSize.R, 2);
    DecompressChannel(Stream, Header.ChannelSize.A, 3);
  finally
    if (Stream <> nil) then
      FreeAndNil(Stream);
  end;
end;

function SimbaImage_ToString(Image: TSimbaImage): String;

  procedure CompressChannel(ChannelOffset: Byte; DestStream: TStream; out Count: Integer);
  var
    Channel: TByteArray;
    I: Integer;
    CompressionStream: Tcompressionstream;
  begin
    CompressionStream := nil;

    Count := DestStream.Position;
    try
      CompressionStream := Tcompressionstream.create(clDefault, DestStream);

      SetLength(Channel, Image.Width * Image.Height);
      for I := 0 to High(Channel) do
        Channel[I] := PByte(@Image.Data[I].B + ChannelOffset)^;

      CompressionStream.Write(Channel[0], Length(Channel));
      CompressionStream.Flush();
    finally
      CompressionStream.Free();
    end;

    Count := DestStream.Position - Count;
  end;

var
  Stream: TStringStream;
  Header: TImageStringHeader;
begin
  Stream := nil;
  try
    Stream := TStringStream.Create();
    Stream.Write(Header{%H-}, SizeOf(TImageStringHeader)); // write header last, but reserve space

    CompressChannel(0, Stream, Header.ChannelSize.B);
    CompressChannel(1, Stream, Header.ChannelSize.G);
    CompressChannel(2, Stream, Header.ChannelSize.R);
    CompressChannel(3, Stream, Header.ChannelSize.A);

    Header.Version := 1;
    Header.Width   := Image.Width;
    Header.Height  := Image.Height;
    Header.Name    := Image.Name;

    Stream.Position := 0;
    Stream.Write(Header, SizeOf(TImageStringHeader));

    Result := HeaderPrefix + BaseEncode(BaseEncoding.b64, Stream.DataString);
  finally
    Stream.Free();
  end;
end;

end.

