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
    SYNLZ,
    GZ,
    RLE
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

implementation

uses
  ZStream,
  castle_gz,
  mormot2_synlz,
  mormot2_rle;

type
  TOutStream = class(TStream)
  protected
    FData: Pointer;
    FDataSize: UInt32;
    FPosition: Int32;
    FSize: Int32;
  public
    constructor Create(Data: Pointer); reintroduce;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property Data: Pointer read FData;
  end;

constructor TOutStream.Create(Data: Pointer);
begin
  inherited Create();

  FData := Data;
  if (FData <> nil) then
    FDataSize := MemSize(FData);
end;

function TOutStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Word(Origin) of
    soFromBeginning: FPosition := Offset;
    soFromEnd      : FPosition := FSize + Offset;
    soFromCurrent  : FPosition := FPosition + Offset;
  end;
  Result := FPosition;
end;

function TOutStream.Write(const Buffer; Count: Longint): Longint;
begin
  if (FPosition + Count > FDataSize) then
  begin
    FDataSize := Max(4096, (FPosition + Count) * 2);
    ReAllocMem(FData, FDataSize);
  end;

  Move(Buffer, (FData + FPosition)^, Count);
  Inc(FPosition, Count);
  if (FPosition > FSize) then
    FSize := FPosition;

  Result := Count;
end;

procedure AllocOrGrowMemory(var P: Pointer; Needed: PtrUInt);
begin
  if (P = nil) or (MemSize(P) < Needed) then
    ReAllocMem(P, Needed);
end;

procedure CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; Truncate: Boolean);

  procedure CompressWithZLib;
  var
    InStream: TCompressionstream;
    OutStream: TOutStream;
  begin
    OutStream := TOutStream.Create(OutData);
    InStream := TCompressionStream.Create(clDefault, OutStream);
    InStream.SourceOwner := False;
    try
      InStream.Write(InData^, InSize);
      InStream.Flush();

      OutSize := OutStream.Position;
      OutData := OutStream.Data;
    finally
      InStream.Free();
      OutStream.Free();
    end;
  end;

  procedure CompressWithSynLZ;
  begin
    AllocOrGrowMemory(OutData, SynLZcompressdestlen(InSize));
    OutSize := SynLZcompress(InData, InSize, OutData);
  end;

  procedure CompressWithGz;
  var
    InStream: TGZFileStream;
    OutStream: TOutStream;
  begin
    OutStream := TOutStream.Create(OutData);
    try
      InStream := TGZFileStream.Create(OutStream, True);
      InStream.SourceOwner := False;
      try
        InStream.Write(InData^, InSize);
      finally
        InStream.Free(); // flushes on free
      end;

      OutSize := OutStream.Position;
      OutData := OutStream.Data;
    finally
      OutStream.Free();
    end;
  end;

  procedure CompressWithRle();
  begin
    OutSize := InSize + SizeOf(Int32);
    AllocOrGrowMemory(OutData, OutSize);
    OutSize := RleCompress(InData, @OutData[SizeOf(Int32)], InSize, OutSize - SizeOf(Int32));
    if (OutSize = -1) then
    begin
      OutSize := InSize + SizeOf(Int32);
      Move(InData^, OutData[SizeOf(Int32)], InSize);
      PInt32(OutData)^ := 0;
    end else
    begin
      OutSize := OutSize + SizeOf(Int32);
      PInt32(OutData)^ := InSize;
    end;
  end;

begin
  OutSize := 0;
  case Algo of
    ESimbaCompressAlgo.ZLIB:  CompressWithZLib();
    ESimbaCompressAlgo.SYNLZ: CompressWithSynLZ();
    ESimbaCompressAlgo.GZ:    CompressWithGZ();
    ESimbaCompressAlgo.RLE:   CompressWithRle();
  end;
  if Truncate then
    ReAllocMem(OutData, OutSize);
end;

procedure DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; Truncate: Boolean);

  procedure DecompressWithZLib;
  var
    InStream: TMemoryStream;
    OutStream: TOutStream;
    DecompressStream: Tdecompressionstream;
    Count: Integer;
    Chunk: array[0..4095] of Byte;
  begin
    InStream := TMemoryStream.Create();
    InStream.Write(InData^, InSize);
    InStream.Position := 0;
    OutStream := TOutStream.Create(OutData);
    DecompressStream := TDeCompressionStream.Create(InStream);
    DecompressStream.SourceOwner := False;
    try
      repeat
        Count := DecompressStream.Read(Chunk[0], Length(Chunk));
        if (Count > 0) then
          OutStream.Write(Chunk[0], Count);
      until (Count = 0);
      OutSize := OutStream.Position;
      OutData := OutStream.Data;
    finally
      InStream.Free();
      OutStream.Free();
      DecompressStream.Free();
    end;
  end;

  procedure DecompressWithSynLZ;
  begin
    AllocOrGrowMemory(OutData, SynLZdecompressdestlen(InData));
    OutSize := SynLZdecompress(InData, InSize, OutData);
  end;

  procedure DecompressWithGZ;
  var
    InStream: TMemoryStream;
    OutStream: TOutStream;
    GzStream: TGZFileStream;
    Count: Integer;
    Chunk: array[0..4095] of Byte;
  begin
    InStream := TMemoryStream.Create();
    InStream.Write(InData^, InSize);
    InStream.Position := 0;
    OutStream := TOutStream.Create(OutData);
    GzStream := TGZFileStream.Create(InStream, False);
    GzStream.SourceOwner := False;
    try
      repeat
        Count := GzStream.Read(Chunk[0], Length(Chunk));
        if (Count > 0) then
          OutStream.Write(Chunk[0], Count);
      until (Count = 0);
      OutSize := OutStream.Position;
      OutData := OutStream.Data;
    finally
      InStream.Free();
      OutStream.Free();
      GzStream.Free();
    end;
  end;

  procedure DecompressWithRLE;
  begin
    OutSize := PInt32(InData)^;

    // no rle
    if (OutSize = 0) then
    begin
      OutSize := InSize - SizeOf(Int32);
      AllocOrGrowMemory(OutData, OutSize);
      Move(InData[SizeOf(Int32)], OutData^, OutSize);
    end else
    begin
      AllocOrGrowMemory(OutData, OutSize);
      OutSize := RleUnCompress(@InData[SizeOf(Int32)], OutData, InSize - SizeOf(Int32));
    end;
  end;

begin
  OutSize := 0;
  case Algo of
    ESimbaCompressAlgo.ZLIB:  DecompressWithZLib();
    ESimbaCompressAlgo.SYNLZ: DecompressWithSynLZ();
    ESimbaCompressAlgo.GZ:    DecompressWithGZ();
    ESimbaCompressAlgo.RLE:   DecompressWithRLE();
  end;
  if Truncate then
    ReAllocMem(OutData, OutSize);
end;

function CompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64): TByteArray;
var
  OutData: Pointer;
  OutSize: Int64;
begin
  OutData := nil;
  CompressData(Algo, InData, InSize, OutData, OutSize);
  SetLength(Result, OutSize);
  Move(OutData^, Result[0], Length(Result));
  FreeMem(OutData);
end;

function DecompressData(Algo: ESimbaCompressAlgo; InData: PByte; InSize: Int64): TByteArray;
var
  OutData: Pointer;
  OutSize: Int64;
begin
  OutData := nil;
  DecompressData(Algo, InData, InSize, OutData, OutSize);
  SetLength(Result, OutSize);
  Move(OutData^, Result[0], Length(Result));
  FreeMem(OutData);
end;

function CompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
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
  OutData := nil;
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
  OutData := nil;
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
  OutData := nil;
  Str := BaseDecode(Encoding, Str);
  DecompressData(Algo, @Str[1], Length(Str), OutData, OutSize);

  SetLength(Result, OutSize);
  Move(OutData^, Result[1], OutSize);
  FreeMem(OutData);
end;

end.

