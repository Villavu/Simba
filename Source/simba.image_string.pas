{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Save/Load images from/to a base64 string.

  A string is "IMG:" + base64(header + name + LZMA-compressed BGRA pixels).
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
  Planes: PByte;
begin
  SetLength(Result, PixelCount * 4);
  Planes := PByte(Result);
  SplitChannels(Src, PixelCount, Planes, Planes + PixelCount, Planes + PixelCount * 2, Planes + PixelCount * 3);
end;

procedure UnsplitBGRA(Planes: PByte; Dest: PColorBGRA; PixelCount: SizeInt);
begin
  MergeChannels(Dest, PixelCount, Planes, Planes + PixelCount, Planes + PixelCount * 2, Planes + PixelCount * 3, 0);
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
            UnsplitBGRA(PByte(DecompressedData), Image.Data, Header.Width * Header.Height)
          else
            Move(DecompressedData[0], Image.Data^, (Header.Width * Header.Height) * SizeOf(TColorBGRA));
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
  Interleaved, Split, CompressedData: TByteArray;
  Buffer: TBytesStream;
  Header: THeader;
  PixelCount, DataSize: SizeInt;
begin
  PixelCount := Image.PixelCount;
  DataSize   := PixelCount * SizeOf(TColorBGRA);

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
