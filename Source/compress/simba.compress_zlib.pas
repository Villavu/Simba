{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  RFC 1950 https://www.rfc-editor.org/rfc/rfc1950
}
unit simba.compress_zlib;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.compress_codec;

type
  ZLib = class(TCompressCodec)
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
  end;

implementation

uses
  simba.compress_zlib_deflate;

function Adler32(Data: PByte; Len: PtrUInt): UInt32;
const
  BASE = 65521;
  NMAX = 5552; // most bytes that can be summed before the 32 bit sums could overflow
var
  S1, S2, Chunk: UInt32;
begin
  S1 := 1;
  S2 := 0;

  while (Len > 0) do
  begin
    Chunk := NMAX;
    if (Chunk > Len) then
      Chunk := Len;
    Dec(Len, Chunk);

    while (Chunk > 0) do
    begin
      Inc(S1, Data^);
      Inc(S2, S1);
      Inc(Data);
      Dec(Chunk);
    end;

    S1 := S1 mod BASE;
    S2 := S2 mod BASE;
  end;

  Result := (S2 shl 16) or S1;
end;

class procedure ZLib.Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Needed: PtrUInt;
begin
  Needed := DeflateBound(InSize) + 6;
  if (OutData = nil) or (MemSize(OutData) < Needed) then
    ReAllocMem(OutData, Needed);

  // CM 8 (deflate) with a 32K window, FLEVEL 2, and check bits that make the
  // first two bytes a multiple of 31
  OutData[0] := $78;
  OutData[1] := $9C;

  OutSize := 2 + Deflate(InData, InSize, OutData + 2);

  PUInt32(OutData + OutSize)^ := NtoBE(Adler32(InData, InSize));
  Inc(OutSize, 4);
end;

class procedure ZLib.Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Used: PtrUInt;
begin
  if (InSize < 6) then
    CompressCodecException('zlib data is too small (%d bytes)', [InSize]);
  if (InData[0] and $0F <> 8) or (((InData[0] shl 8) + InData[1]) mod 31 <> 0) then
    CompressCodecException('not a zlib stream');
  // CINFO above 7 declares a window deflate cannot address
  if ((InData[0] shr 4) > 7) then
    CompressCodecException('invalid zlib window size');
  if (InData[1] and $20 <> 0) then
    CompressCodecException('zlib preset dictionaries are not supported');

  Inflate(InData + 2, InSize - 2 - 4, OutData, 0, OutSize, Used);

  if (BEtoN(PUInt32(InData + InSize - 4)^) <> Adler32(OutData, OutSize)) then
    CompressCodecException('zlib checksum mismatch');
end;

end.
