{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  RFC 1952 https://www.rfc-editor.org/rfc/rfc1952
}
unit simba.compress_gz;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.compress_codec;

type
  Gz = class(TCompressCodec)
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
  end;

implementation

uses
  simba.crc, simba.compress_zlib_deflate;

const
  HEADER_SIZE = 10;
  FOOTER_SIZE = 8;
  OS_UNKNOWN  = 255; // so the output does not vary by the platform that wrote it

  FLAG_TEXT    = $01;
  FLAG_HCRC    = $02;
  FLAG_EXTRA   = $04;
  FLAG_NAME    = $08;
  FLAG_COMMENT = $10;

class procedure Gz.Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Needed: PtrUInt;
begin
  Needed := DeflateBound(InSize) + HEADER_SIZE + FOOTER_SIZE;
  if (OutData = nil) or (MemSize(OutData) < Needed) then
    ReAllocMem(OutData, Needed);

  OutData[0] := $1F;
  OutData[1] := $8B;
  OutData[2] := 8;               // deflate
  OutData[3] := 0;               // no optional fields
  PUInt32(OutData + 4)^ := 0;    // no modification time
  OutData[8] := 0;               // no extra flags
  OutData[9] := OS_UNKNOWN;

  OutSize := HEADER_SIZE + Deflate(InData, InSize, OutData + HEADER_SIZE);

  PUInt32(OutData + OutSize)^ := NtoLE(CRC32(InData, InSize));
  PUInt32(OutData + OutSize + 4)^ := NtoLE(UInt32(InSize));
  Inc(OutSize, FOOTER_SIZE);
end;

class procedure Gz.Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Pos, MemberStart, HeaderStart: Int64;
  Flags: Byte;
  Used: PtrUInt;
  Total: Int64;
begin
  Pos := 0;
  OutSize := 0;

  repeat
    if (InSize - Pos < HEADER_SIZE + FOOTER_SIZE) then
      CompressCodecException('gzip data is too small (%d bytes)', [InSize - Pos]);
    if (InData[Pos] <> $1F) or (InData[Pos+1] <> $8B) then
      CompressCodecException('not a gzip stream');
    if (InData[Pos+2] <> 8) then
      CompressCodecException('unsupported gzip compression method (%d)', [InData[Pos+2]]);

    Flags := InData[Pos+3];
    // bits 5..7 have no meaning yet, so a stream setting them is not one we know
    if (Flags and $E0 <> 0) then
      CompressCodecException('gzip header sets unknown flags');
    HeaderStart := Pos;
    Inc(Pos, HEADER_SIZE);

    if (Flags and FLAG_EXTRA <> 0) then
      Inc(Pos, 2 + Int64(InData[Pos]) + (Int64(InData[Pos+1]) shl 8));
    if (Flags and FLAG_NAME <> 0) then
    begin
      while (Pos < InSize) and (InData[Pos] <> 0) do
        Inc(Pos);
      Inc(Pos);
    end;
    if (Flags and FLAG_COMMENT <> 0) then
    begin
      while (Pos < InSize) and (InData[Pos] <> 0) do
        Inc(Pos);
      Inc(Pos);
    end;
    if (Flags and FLAG_HCRC <> 0) then
    begin
      if (Pos + 2 > InSize) then
        CompressCodecException('gzip header is truncated');
      if (LEtoN(PUInt16(InData + Pos)^) <> UInt16(CRC32(InData + HeaderStart, Pos - HeaderStart) and $FFFF)) then
        CompressCodecException('gzip header checksum mismatch');
      Inc(Pos, 2);
    end;

    if (Pos + FOOTER_SIZE > InSize) then
      CompressCodecException('gzip header is truncated');

    MemberStart := OutSize;
    Inflate(InData + Pos, InSize - Pos, OutData, OutSize, Total, Used);
    Inc(Pos, Used);

    if (Pos + FOOTER_SIZE > InSize) then
      CompressCodecException('gzip footer is missing');

    if (LEtoN(PUInt32(InData + Pos)^) <> CRC32(OutData + MemberStart, Total - MemberStart)) then
      CompressCodecException('gzip checksum mismatch');
    if (LEtoN(PUInt32(InData + Pos + 4)^) <> UInt32(Total - MemberStart)) then
      CompressCodecException('gzip size mismatch');

    Inc(Pos, FOOTER_SIZE);
    OutSize := Total;
  until (Pos + HEADER_SIZE + FOOTER_SIZE > InSize) or (InData[Pos] <> $1F) or (InData[Pos+1] <> $8B);
end;

end.
