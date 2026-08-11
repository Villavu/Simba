{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Save/Load images from/to a base64 string.

  A string is "IMG:" + base64(header + name + LZMA-compressed BGRA pixels).
  Version 1 stored a PNG which is still supported for reading
}
unit simba.image_stringconv;

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
  simba.encoding,
  simba.compress,
  simba.image_lazbridge,
  simba.vartype_string;

const
  HeaderPrefix = 'IMG:';

  VERSION_PNG        = 1; // legacy: header with fixed name String[128] then PNG data
  VERSION_LZMA       = 2; // header + LZMA(interleaved BGRA)
  VERSION_LZMA_SPLIT = 3; // header + LZMA(channel-split BGRA)

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

function SplitBGRA(Src: PByte; PixelCount: SizeInt): TByteArray;
var
  I: SizeInt;
  ptrB, ptrG, ptrR, ptrA: PByte;
begin
  SetLength(Result, PixelCount * 4);

  ptrB := Pointer(Result);
  ptrG := ptrB + PixelCount;
  ptrR := ptrG + PixelCount;
  ptrA := ptrR + PixelCount;

  for I := 1 to PixelCount do
  begin
    ptrB^ := Src[0];
    ptrG^ := Src[1];
    ptrR^ := Src[2];
    ptrA^ := Src[3];

    Inc(ptrB);
    Inc(ptrG);
    Inc(ptrR);
    Inc(ptrA);
    Inc(Src, 4);
  end;
end;

procedure UnsplitBGRA(Src, Dst: PByte; PixelCount: SizeInt);
var
  I: SizeInt;
  ptrB, ptrG, ptrR, ptrA: PByte;
begin
  ptrB := Src;
  ptrG := ptrB + PixelCount;
  ptrR := ptrG + PixelCount;
  ptrA := ptrR + PixelCount;

  for I := 1 to PixelCount do
  begin
    Dst[0] := ptrB^;
    Dst[1] := ptrG^;
    Dst[2] := ptrR^;
    Dst[3] := ptrA^;

    Inc(ptrB);
    Inc(ptrG);
    Inc(ptrR);
    Inc(ptrA);
    Inc(Dst, 4);
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
      VERSION_LZMA_SPLIT:
        begin
          Image.Name := ReadName(Stream, Header.NameLen);
          Image.SetSize(Header.Width, Header.Height);

          DecompressedData := DecompressStream(ESimbaCompressAlgo.LZMA, Stream);
          if (Header.Version = VERSION_LZMA_SPLIT) then
            UnsplitBGRA(@DecompressedData[0], PByte(Image.Data), Header.Width * Header.Height)
          else
            Move(DecompressedData[0], Image.Data^, (Header.Width * Header.Height) * SizeOf(TColorBGRA));
        end;

      VERSION_PNG:
        begin
          Stream.Position := 0;
          Stream.Read(HeaderLegacy, SizeOf(THeaderLegacy));
          Image.Name := HeaderLegacy.Name;
          SimbaImage_FromFPImageReader(Image, TFPReaderPNG, Stream);
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
  Interleaved, Split, CompressedData: TByteArray;
  Buffer: TBytesStream;
  Header: THeader;
  PixelCount, DataSize: SizeInt;
begin
  PixelCount := Image.Width * Image.Height;
  DataSize   := PixelCount * SizeOf(TColorBGRA);

  // pick the smaller output, and store in version field
  Interleaved := CompressData(ESimbaCompressAlgo.LZMA, PByte(Image.Data), DataSize);
  Split       := CompressBytes(ESimbaCompressAlgo.LZMA, SplitBGRA(PByte(Image.Data), PixelCount));

  if (Length(Split) < Length(Interleaved)) then
  begin
    Header.Version := VERSION_LZMA_SPLIT;
    CompressedData := Split;
  end else
  begin
    Header.Version := VERSION_LZMA;
    CompressedData := Interleaved;
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
