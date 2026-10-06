{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Save/Load images from/to a base64 string.

  A string is "IMG:" + base64(header + name + LZMA-compressed pixels).
  The pixels are BGRA, or a bit a pixel when the image has two colors.
  Version 1 stored a PNG which is still supported for reading
}
unit simba.image_string;

{$I simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.image;

procedure SimbaImage_FromString(Image: TSimbaImage; Str: String);
function SimbaImage_ToString(Image: TSimbaImage): String;

implementation

uses
  FPReadPNG,
  simba.math,
  simba.encoding,
  simba.compress,
  simba.image_file,
  simba.image_utils,
  simba.vartype_string;

const
  HeaderPrefix = 'IMG:';

  VERSION_PNG        = 1; // legacy: header with fixed name String[128] then PNG data
  VERSION_LZMA       = 2; // header + LZMA(interleaved BGRA)
  VERSION_LZMA_SPLIT = 3; // header + LZMA(channel-split BGRA)
  VERSION_LZMA_BITS  = 4; // header + LZMA(two colors + a bit a pixel)

type
  THeaderLegacy = packed record
    Version: Int32;
    Width, Height: Int32;
    Name: String[128];
  end;

  THeader = packed record
    Version: Int32;
    Width, Height: Int32;
    NameLen: Int32; // length of the name that follows the header
  end;

function ReadName(Stream: TStream; Len: SizeInt): String;
begin
  Result := '';
  if (Len > 0) and (Stream.Position + Len <= Stream.Size) then
  begin
    SetLength(Result, Len);
    Stream.ReadBuffer(Result[1], Len);
  end;
end;

// the four channels one after another, each PixelCount bytes
function SplitBGRA(Src: PColorBGRA; PixelCount: SizeInt): TByteArray;
var
  Channels: PByte;
begin
  SetLength(Result, PixelCount * 4);
  Channels := PByte(Result);
  SplitChannels(Src, PixelCount, Channels, Channels + PixelCount, Channels + PixelCount * 2, Channels + PixelCount * 3);
end;

procedure UnsplitBGRA(Channels: PByte; Dest: PColorBGRA; PixelCount: SizeInt);
begin
  MergeChannels(Dest, PixelCount, Channels, Channels + PixelCount, Channels + PixelCount * 2, Channels + PixelCount * 3, 0);
end;

// The two colors of an isDualColor image, then a bit a pixel with each row starting on a byte.
// A set bit is the second color.
procedure PackBits(Src: PColorBGRA; Width, Height: Integer; Color1, Color2: TColorBGRA; out Bits: TByteArray);
var
  Row: PByte;
  X, Y, RowBytes: Integer;
begin
  RowBytes := (Width + 7) div 8;
  SetLength(Bits, 2 * SizeOf(TColorBGRA) + RowBytes * Height);

  PColorBGRA(Bits)[0] := Color1;
  PColorBGRA(Bits)[1] := Color2;

  Row := PByte(Bits) + 2 * SizeOf(TColorBGRA);
  for Y := 0 to Height - 1 do
  begin
    for X := 0 to Width - 1 do
    begin
      if (Src^.AsInteger <> Color1.AsInteger) then
        SetBit(Row, X);

      Inc(Src);
    end;

    Inc(Row, RowBytes);
  end;
end;

procedure UnpackBits(Bits: PByte; Dest: PColorBGRA; Width, Height: Integer);
var
  Colors: PColorBGRA;
  X, Y: Integer;
begin
  Colors := PColorBGRA(Bits);
  Inc(Bits, 2 * SizeOf(TColorBGRA));

  for Y := 0 to Height - 1 do
  begin
    for X := 0 to Width - 1 do
    begin
      if IsBitSet(Bits, X) then
        Dest^ := Colors[1]
      else
        Dest^ := Colors[0];
      Inc(Dest);
    end;

    Inc(Bits, (Width + 7) div 8);
  end;
end;

procedure SimbaImage_FromString(Image: TSimbaImage; Str: String);
var
  Stream: TBytesStream;
  DecompressedData: TByteArray;
  Header: THeader;
  HeaderLegacy: THeaderLegacy;
begin
  if not Str.StartsWith(HeaderPrefix, True) then
    SimbaException('TImage.FromString: Invalid string. Must start with "IMG:"');
  Str.DeleteRange(1, Length(HeaderPrefix));

  Stream := TBytesStream.Create();
  try
    BaseDecode(EBaseEncoding.b64, Str, Stream);
    if (Stream.Size < SizeOf(THeader)) then
      SimbaException('TImage.FromString: Invalid string. Too small');

    Stream.Position := 0;
    Stream.Read(Header, SizeOf(THeader));

    case Header.Version of
      VERSION_LZMA,
      VERSION_LZMA_SPLIT,
      VERSION_LZMA_BITS:
        begin
          Image.Name := ReadName(Stream, Header.NameLen);
          Image.SetSize(Header.Width, Header.Height);

          DecompressedData := DecompressStream(ESimbaCompressAlgo.LZMA, Stream);
          case Header.Version of
            VERSION_LZMA:       MoveData(Image.Data, PColorBGRA(DecompressedData), Header.Width * Header.Height);
            VERSION_LZMA_SPLIT: UnsplitBGRA(PByte(DecompressedData), Image.Data, Header.Width * Header.Height);
            VERSION_LZMA_BITS:  UnpackBits(PByte(DecompressedData), Image.Data, Header.Width, Header.Height);
          end;
        end;

      VERSION_PNG:
        begin
          Stream.Position := 0;
          Stream.Read(HeaderLegacy, SizeOf(THeaderLegacy));
          Image.Name := HeaderLegacy.Name;
          SimbaImage_LoadFPImage(Image, TFPReaderPNG, Stream);
        end;
    else
      SimbaException('TImage.FromString: Unsupported version (%d)', [Header.Version]);
    end;
  finally
    Stream.Free();
  end;
end;

function SimbaImage_ToString(Image: TSimbaImage): String;
var
  Interleaved, Split, Bits, CompressedData: TByteArray;
  Color1, Color2: TColorBGRA;
  Buffer: TBytesStream;
  Header: THeader;
  PixelCount, DataSize: SizeInt;
begin
  PixelCount := Image.PixelCount;
  DataSize   := PixelCount * SizeOf(TColorBGRA);

  if Image.isDualColor(Color1, Color2) then
  begin
    // two colors: nothing else is as small
    PackBits(Image.Data, Image.Width, Image.Height, Color1, Color2, Bits);

    Header.Version := VERSION_LZMA_BITS;
    CompressedData := CompressBytes(ESimbaCompressAlgo.LZMA, Bits);
  end else
  begin
    // pick the smaller output, and store in version field
    Interleaved := CompressData(ESimbaCompressAlgo.LZMA, PByte(Image.Data), DataSize);
    Split       := CompressBytes(ESimbaCompressAlgo.LZMA, SplitBGRA(Image.Data, PixelCount));

    if (Length(Split) < Length(Interleaved)) then
    begin
      Header.Version := VERSION_LZMA_SPLIT;
      CompressedData := Split;
    end else
    begin
      Header.Version := VERSION_LZMA;
      CompressedData := Interleaved;
    end;
  end;

  Header.Width   := Image.Width;
  Header.Height  := Image.Height;
  Header.NameLen := Length(Image.Name);

  Buffer := TBytesStream.Create();
  try
    Buffer.Write(Header, SizeOf(Header));
    if (Length(Image.Name) > 0) then
      Buffer.Write(Image.Name[1], Length(Image.Name));
    if (Length(CompressedData) > 0) then
      Buffer.Write(CompressedData[0], Length(CompressedData));

    Buffer.Position := 0;
    Result := HeaderPrefix + BaseEncode(EBaseEncoding.b64, Buffer);
  finally
    Buffer.Free();
  end;
end;

end.
