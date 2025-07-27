{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
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
    SYNLZ
  );
{$POP}

procedure CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; out OutData: PByte; out OutSize: Int64);
procedure DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; out OutData: PByte; out OutSize: Int64);

function CompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
function DecompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;

function CompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
function DecompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;

implementation

uses
  ZStream,
  mormot2_synlz,
  mormot2_rle;

procedure CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; out OutData: PByte; out OutSize: Int64);

  procedure CompressWithZLib;
  var
    InStream: TCompressionstream;
    OutStream: TMemoryStream;
  begin
    OutStream := TMemoryStream.Create();
    InStream := TCompressionStream.Create(clDefault, OutStream);
    try
      InStream.Write(InData^, InSize);
      InStream.Flush();

      OutSize := OutStream.Position;
      OutData := GetMem(OutSize);

      Move(OutStream.Memory^, OutData^, OutSize);
    finally
      InStream.Free();
      OutStream.Free();
    end;
  end;

  procedure CompressWithSynLZ;
  var
    RleSize: Int64;
    RleData: PByte;
  begin
    // first byte stores if RLE or not
    // see if RLE is even worth it
    RleData := GetMem(InSize - InSize shr 3);
    RleSize := RleCompress(InData, RleData, InSize, InSize - InSize shr 3);

    if (RleSize < 0) then
    begin
      OutData := GetMem(SynLZcompressdestlen(InSize) + SizeOf(Int64));
      OutSize := SynLZcompress(InData, InSize, OutData + SizeOf(Int64)) + SizeOf(Int64);
      PInt64(OutData)^ := 0;
    end
    else
    begin
      OutData := GetMem(SynLZcompressdestlen(RleSize) + SizeOf(Int64));
      OutSize := SynLZcompress(RleData, RleSize, OutData + SizeOf(Int64)) + SizeOf(Int64);
      PInt64(OutData)^ := InSize;
    end;

    FreeMem(RleData);

    OutData := ReAllocMem(OutData, OutSize);
  end;

begin
  OutSize := 0;
  OutData := nil;

  case Algo of
    ESimbaCompressAlgo.ZLIB:  CompressWithZLib();
    ESimbaCompressAlgo.SYNLZ: CompressWithSynLZ();
  end;
end;

procedure DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; out OutData: PByte; out OutSize: Int64);

  procedure DecompressWithZLib;
  var
    InStream, OutStream: TMemoryStream;
    DecompressStream: Tdecompressionstream;
    Count: Integer;
    Chunk: array[0..4095] of Byte;
  begin
    InStream := TMemoryStream.Create();
    InStream.Write(InData^, InSize);
    InStream.Position := 0;
    OutStream := TMemoryStream.Create();
    DecompressStream := TDeCompressionStream.Create(InStream);
    try
      repeat
        Count := DecompressStream.Read(Chunk[0], Length(Chunk));
        if (Count > 0) then
          OutStream.Write(Chunk[0], Count);
      until (Count = 0);
      OutSize := OutStream.Position;
      OutData := GetMem(OutSize);

      Move(OutStream.Memory^, OutData^, OutSize);
    finally
      InStream.Free();
      OutStream.Free();
      DecompressStream.Free();
    end;
  end;

  procedure DecompressWithSynLZ;
  var
    RleData: PByte;
    RleSize: Int64;
  begin
    if (PInt64(InData)^ > 0) then // data has been rle compressed
    begin
      RleSize := SynLZdecompressdestlen(InData + SizeOf(Int64));
      RleData := GetMem(RleSize);
      if (SynLZdecompress(InData + SizeOf(Int64), InSize - SizeOf(Int64), RleData) <> RleSize) then
        SimbaException('Error SynLZDecompress');

      OutSize := PInt64(InData)^;
      OutData := GetMem(OutSize);

      if (RleUnCompress(RleData, OutData, RleSize) <> OutSize) then
        SimbaException('Error RleUnCompress');

      FreeMem(RleData);
    end else
    begin
      OutData := GetMem(SynLZdecompressdestlen(InData + SizeOf(Int64)));
      OutSize := SynLZdecompress(InData + SizeOf(Int64), InSize - SizeOf(Int64), OutData);
    end;
  end;

begin
  OutData := nil;
  OutSize := 0;

  case Algo of
    ESimbaCompressAlgo.ZLIB:  DecompressWithZLib();
    ESimbaCompressAlgo.SYNLZ: DecompressWithSynLZ();
  end;
end;

function CompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  CompressData(Algo, @Bytes[0], Length(Bytes), OutData, OutSize);
  SetLength(Result, OutSize);
  Move(OutData^, Result[0], OutSize);
  FreeMem(OutData);
end;

function DecompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  DecompressData(Algo, @Bytes[0], Length(Bytes), OutData, OutSize);
  SetLength(Result, OutSize);
  Move(OutData^, Result[0], OutSize);
  FreeMem(OutData);
end;

function CompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
var
  OutData: PByte;
  OutSize: Int64;
begin
  CompressData(Algo, @Str[1], Length(Str), OutData, OutSize);
  SetLength(Str, OutSize);
  Move(OutData^, Str[1], OutSize);
  FreeMem(OutData);

  Result := BaseEncode(Encoding, Str);
end;

function DecompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
var
  OutData: PByte;
  OutSize: Int64;
begin
  Str := BaseDecode(Encoding, Str);
  DecompressData(Algo, @Str[1], Length(Str), OutData, OutSize);

  SetLength(Result, OutSize);
  Move(OutData^, Result[1], OutSize);
  FreeMem(OutData);
end;

end.

