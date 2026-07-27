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
    RLE,
    BZIP2
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
  mormot2_rle,
  bzip2stream;

type
  TOutStream = class(TStream)
  protected
    FData: Pointer;
    FDataSize: PtrInt;
    FPosition: PtrInt;
    FSize: PtrInt;
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
    OutSize := RleCompressDestLen(InSize) + SizeOf(Int32);
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
  if (InSize < 0) or (InSize > High(Int32)) then
    SimbaException('CompressData: InSize %d is out of range (0 .. %d)', [InSize, High(Int32)]);

  case Algo of
    ESimbaCompressAlgo.ZLIB:  CompressWithZLib();
    ESimbaCompressAlgo.SYNLZ: CompressWithSynLZ();
    ESimbaCompressAlgo.GZ:    CompressWithGZ();
    ESimbaCompressAlgo.RLE:   CompressWithRle();
    ESimbaCompressAlgo.BZIP2: SimbaException('BZip2 compression is not supported.');
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
    if (InSize < SizeOf(Int32)) then
      SimbaException('DecompressData: RLE data is too small (%d bytes)', [InSize]);

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
      OutSize := RleUnCompressPartial(@InData[SizeOf(Int32)], OutData, InSize - SizeOf(Int32), OutSize);
    end;
  end;

  procedure DecompressWithBZip2;
  var
    InStream: TMemoryStream;
    OutStream: TOutStream;
    BZStream: TDecompressBzip2Stream;
    Count: Integer;
    Chunk: array[0..4095] of Byte;
  begin
    InStream := TMemoryStream.Create();
    InStream.Write(InData^, InSize);
    InStream.Position := 0;
    OutStream := TOutStream.Create(OutData);
    BZStream := TDecompressBzip2Stream.Create(InStream);
    try
      repeat
        Count := BZStream.Read(Chunk[0], Length(Chunk));
        if (Count > 0) then
          OutStream.Write(Chunk[0], Count);
      until (Count = 0);
      OutSize := OutStream.Position;
      OutData := OutStream.Data;
    finally
      InStream.Free();
      OutStream.Free();
      BZStream.Free();
    end;
  end;

begin
  OutSize := 0;
  if (InSize < 0) or (InSize > High(Int32)) then
    SimbaException('DecompressData: InSize %d is out of range (0 .. %d)', [InSize, High(Int32)]);

  case Algo of
    ESimbaCompressAlgo.ZLIB:  DecompressWithZLib();
    ESimbaCompressAlgo.SYNLZ: DecompressWithSynLZ();
    ESimbaCompressAlgo.GZ:    DecompressWithGZ();
    ESimbaCompressAlgo.RLE:   DecompressWithRLE();
    ESimbaCompressAlgo.BZIP2: DecompressWithBZip2();
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
  OutData: Pointer;
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
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
  try
    CompressData(Algo, Pointer(Bytes), Length(Bytes), OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[0], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
end;

function DecompressBytes(Algo: ESimbaCompressAlgo; Bytes: TByteArray): TByteArray;
var
  OutData: PByte;
  OutSize: Int64;
begin
  OutData := nil;
  try
    DecompressData(Algo, Pointer(Bytes), Length(Bytes), OutData, OutSize);
    SetLength(Result, OutSize);
    if (OutSize > 0) then
      Move(OutData^, Result[0], OutSize);
  finally
    if (OutData <> nil) then
      FreeMem(OutData);
  end;
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

