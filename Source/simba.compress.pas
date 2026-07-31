{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Rough ratios:
    SYNLZ  ~1.7x, but hundreds of MB/s in both directions
    LZ4    ~2.6x, nearly as fast, and a format other tools read
    ZLIB   ~5.8x, the same as ZLIB, in a container other tools expect
    BZIP2  ~6.0x, several times slower to compress
    LZMA   ~7.4x, slower again to compress but quick to decompress
}
unit simba.compress;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.encoding;

{$PUSH}
{$SCOPEDENUMS ON}
type
  ESimbaCompressAlgo = (
    ZLIB,
    SYNLZ,
    GZ,
    BZIP2,
    LZ4,
    LZMA
  );
{$POP}

procedure CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; Truncate: Boolean = True);
procedure DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; Truncate: Boolean = True);

function CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64): TByteArray; overload;
function DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64): TByteArray; overload;

function CompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
function DecompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;

function CompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
function DecompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;

procedure CompressStream(Algo: ESimbaCompressAlgo; Source, Dest: TStream; Count: Int64 = -1);
procedure DecompressStream(Algo: ESimbaCompressAlgo; Source, Dest: TStream; Count: Int64 = -1);

function CompressStream(Algo: ESimbaCompressAlgo; Source: TStream; Count: Int64 = -1): TByteArray; overload;
function DecompressStream(Algo: ESimbaCompressAlgo; Source: TStream; Count: Int64 = -1): TByteArray; overload;

implementation

uses
  simba.compress_codec,
  simba.compress_zlib,
  simba.compress_synlz,
  simba.compress_gz,
  simba.compress_bzip2,
  simba.compress_lz4,
  simba.compress_lzma;

const
  // in ESimbaCompressAlgo order
  CODECS: array[ESimbaCompressAlgo] of TCompressCodecClass = (
    ZLib, SynLZ, Gz, BZip2, LZ4, LZMA
  );

procedure CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; Truncate: Boolean);
begin
  OutSize := 0;
  if (InSize < 0) or (InSize > High(Int32)) then
    SimbaException('InSize %d is out of range (0 .. %d)', [InSize, High(Int32)]);

  CODECS[Algo].Compress(InData, InSize, OutData, OutSize);

  if Truncate then
    ReAllocMem(OutData, OutSize);
end;

procedure DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; Truncate: Boolean);
begin
  OutSize := 0;
  if (InSize < 0) or (InSize > High(Int32)) then
    SimbaException('InSize %d is out of range (0 .. %d)', [InSize, High(Int32)]);

  CODECS[Algo].Decompress(InData, InSize, OutData, OutSize);

  if Truncate then
    ReAllocMem(OutData, OutSize);
end;

function CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
  try
    CompressData(Algo, InData, InSize, OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[0], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
end;

function DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
  try
    DecompressData(Algo, InData, InSize, OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[0], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
end;

function CompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
begin
  Result := CompressData(Algo, Pointer(Bytes), Length(Bytes));
end;

function DecompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
begin
  Result := DecompressData(Algo, Pointer(Bytes), Length(Bytes));
end;

function CompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
  try
    CompressData(Algo, Pointer(Str), Length(Str), OutData, OutSize);
    SetLength(Str, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Str[1], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;

  Result := BaseEncode(Encoding, Str);
end;

function ReadStream(Source: TStream; Count: Int64): TByteArray;
begin
  if (Count < 0) then
    Count := Source.Size - Source.Position;
  SetLength(Result, Count);
  if (Count > 0) then
    Source.ReadBuffer(Result[0], Count);
end;

function CompressStream(Algo: ESimbaCompressAlgo; Source: TStream; Count: Int64): TByteArray;
begin
  Result := CompressBytes(Algo, ReadStream(Source, Count));
end;

function DecompressStream(Algo: ESimbaCompressAlgo; Source: TStream; Count: Int64): TByteArray;
begin
  Result := DecompressBytes(Algo, ReadStream(Source, Count));
end;

procedure CompressStream(Algo: ESimbaCompressAlgo; Source, Dest: TStream; Count: Int64);
var
  Bytes: TByteArray;
begin
  Bytes := CompressStream(Algo, Source, Count);
  if (Length(Bytes) > 0) then
    Dest.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure DecompressStream(Algo: ESimbaCompressAlgo; Source, Dest: TStream; Count: Int64);
var
  Bytes: TByteArray;
begin
  Bytes := DecompressStream(Algo, Source, Count);
  if (Length(Bytes) > 0) then
    Dest.WriteBuffer(Bytes[0], Length(Bytes));
end;

function DecompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
  Str := BaseDecode(Encoding, Str);
  try
    DecompressData(Algo, Pointer(Str), Length(Str), OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[1], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
end;

end.
