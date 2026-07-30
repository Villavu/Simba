{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  SynLZ by Arnaud Bouchez https://synopse.info
  https://github.com/synopse/mORMot2 (src/core/mormot.core.base.pas)
}
unit simba.compress_synlz;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.compress_codec;

type
  SynLZ = class(TCompressCodec)
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;

    // Decode at most MaxOut bytes, however much the stream declares. Upstream's
    // SynLZdecompress1partial, which stops cleanly at the destination bound.
    class procedure DecompressPartial(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; MaxOut: Int64);
  end;

implementation

{$R-}{$Q-}

const
  // the wrapper allocates from the header before decoding, so the claimed size
  // cannot be taken entirely on trust: 100 bytes in can become 8736 out
  MAX_EXPANSION   = 88;
  EXPANSION_SLACK = 512;
  // the asm full decompressor stores 8 bytes at a time, so give it room past the
  // last real byte to overshoot into
  DECOMPRESS_SLACK = 16;

type
  TOffsets = array[0..4095] of PByte;

procedure MoveByOne(Source, Dest: Pointer; Count: PtrUInt);
var
  B: Byte; // better code generation on FPC
begin
  Inc(PtrUInt(Source), Count);
  Inc(PtrUInt(Dest), Count);
  PtrInt(Count) := -PtrInt(Count);
  repeat
    B := PByte(Source)[Count];
    PByte(Dest)[Count] := B;
    Inc(Count);
  until Count = 0;
end;

function SynLZCompressBound(InSize: Int32): Int32;
begin
  // maximum possible (worst case) compressed size
  Result := InSize + (InSize shr 3) + 16;
end;

function SynLZDecompressedSize(Src: PByte): Int32;
begin
  // uncompressed size from the lz-compressed buffer (to reserve memory, e.g.)
  Result := PUInt16(Src)^;
  if (Result and $8000 <> 0) then
    Result := (Result and $7FFF) or (Int32(PUInt16(Src + 2)^) shl 15);
end;

{$IF DEFINED(SYNLZ_ASM)}
  {$I ../asm/synlzcompress_x64.inc}
  {$I ../asm/synlzdecompress_x64.inc}
{$ENDIF}

function SynLZCompressPas(Src: PByte; Size: Int32; Dst: PByte): Int32;
var
  DstBeg,          // initial Dst value
  SrcEnd,          // real last byte available in Src
  SrcEndMatch,     // last byte to try for hashing
  O: PByte;
  CWBit: Byte;
  CWPoint: PUInt32;
  V, H, Cached, T, TMax: PtrUInt;
  Offset: TOffsets;
  Cache: array[0..4095] of UInt32; // 16KB+16KB=32KB on stack (48KB for cpu64)
begin
  DstBeg := Dst;
  // 1. store the size
  if (Size >= $8000) then
  begin
    // size in 32KB..2GB -> stored as an integer
    PUInt32(Dst)^ := UInt32($8000 or (Size and $7FFF) or ((Size shr 15) shl 16));
    Inc(Dst, 4);
  end
  else
  begin
    PUInt32(Dst)^ := UInt32(Size); // size < 32768 -> stored as a word
    if (Size = 0) then
      Exit(2);
    Inc(Dst, 2);
  end;
  // 2. compress
  SrcEnd := Src + Size;
  SrcEndMatch := SrcEnd - (6 + 5);
  CWBit := 0;
  CWPoint := Pointer(Dst);
  PUInt32(Dst)^ := 0;
  Inc(Dst, SizeOf(CWPoint^));
  FillChar(Offset, SizeOf(Offset), 0);
  // 1. main loop to search using hash[]
  if (Src <= SrcEndMatch) then
    repeat
      V := PUInt32(Src)^;
      H := ((V shr 12) xor V) and 4095;
      O := Offset[H];
      Offset[H] := Src;
      Cached := V xor {%H-}Cache[H]; // O=nil if Cache[H] is uninitialized
      Cache[H] := V;
      if (Cached and $00FFFFFF = 0) and
         (O <> nil) and
         (Src - O > 2) then
      begin
        CWPoint^ := CWPoint^ or (UInt32(1) shl CWBit);
        Inc(Src, 2);
        Inc(O, 2);
        T := 1;
        TMax := SrcEnd - Src - 1;
        if (TMax >= (255 + 16)) then
          TMax := (255 + 16);
        while (O[T] = Src[T]) and
              (T < TMax) do
          Inc(T);
        Inc(Src, T);
        H := H shl 4;
        // here we always have T>0
        if (T <= 15) then
        begin
          // mark 2 to 17 bytes -> size=1..15
          PUInt16(Dst)^ := UInt16(T or H);
          Inc(Dst, 2);
        end
        else
        begin
          // mark 18 to (255+16) bytes -> size=0, next byte=T
          Dec(T, 16);
          PUInt16(Dst)^ := UInt16(H); // size=0
          Dst[2] := Byte(T);
          Inc(Dst, 3);
        end;
      end
      else
      begin
        Dst^ := Src^;
        Inc(Src);
        Inc(Dst);
      end;
      if (CWBit < 31) then
      begin
        Inc(CWBit);
        if (Src <= SrcEndMatch) then
          Continue
        else
          Break;
      end
      else
      begin
        CWPoint := Pointer(Dst);
        PUInt32(Dst)^ := 0;
        Inc(Dst, SizeOf(CWPoint^));
        CWBit := 0;
        if (Src <= SrcEndMatch) then
          Continue
        else
          Break;
      end;
    until False;
  // 2. store the remaining bytes
  if (Src < SrcEnd) then
    repeat
      Dst^ := Src^;
      Inc(Src);
      Inc(Dst);
      if (CWBit < 31) then
      begin
        Inc(CWBit);
        if (Src < SrcEnd) then
          Continue
        else
          Break;
      end
      else
      begin
        PUInt32(Dst)^ := 0;
        Inc(Dst, 4);
        CWBit := 0;
        if (Src < SrcEnd) then
          Continue
        else
          Break;
      end;
    until False;
  Result := Dst - DstBeg;
end;

// better code generation with sub-functions for raw decoding
procedure SynLZDecompressPasSub(Src, SrcEnd, Dst: PByte; var Offset: TOffsets);
var
  LastHashed: PByte; // initial Src and Dst value
  {$IFDEF CPU64}
  O: PByte;
  {$ENDIF CPU64}
  CW, CWBit: UInt32;
  V, T, H: PtrUInt;
label
  NextCW;
begin
  LastHashed := Dst - 1;
NextCW:
  CW := PUInt32(Src)^;
  Inc(Src, 4);
  CWBit := 1;
  if (Src < SrcEnd) then
    repeat
      if (CW and CWBit = 0) then
      begin
        Dst^ := Src^;
        Inc(Src);
        Inc(Dst);
        if (Src >= SrcEnd) then
          Break;
        if (LastHashed < Dst - 3) then
        begin
          Inc(LastHashed);
          V := PUInt32(LastHashed)^;
          Offset[((V shr 12) xor V) and 4095] := LastHashed;
        end;
        CWBit := CWBit shl 1;
        if (CWBit <> 0) then
          Continue
        else
          goto NextCW;
      end
      else
      begin
        H := PUInt16(Src)^;
        Inc(Src, 2);
        T := (H and 15) + 2;
        if (T = 2) then
        begin
          T := Src^ + (16 + 2);
          Inc(Src);
        end;
        H := H shr 4;
        {$IFDEF CPU64}
        O := Offset[H];
        if (PtrUInt(Dst - O) < T) then // overlap -> move byte-by-byte
          MoveByOne(O, Dst, T)
        else if (T <= 8) then
          PInt64(Dst)^ := PInt64(O)^ // much faster in practice
        else
          Move(O^, Dst^, T);         // safe since SrcEndMatch = SrcEnd-(6+5)
        {$ELSE}
        if (PtrUInt(Dst - Offset[H]) < T) then
          MoveByOne(Offset[H], Dst, T)
        else if (T > 8) then
          Move(Offset[H]^, Dst^, T)
        else
          PInt64(Dst)^ := PInt64(Offset[H])^;
        {$ENDIF CPU64}
        if (Src >= SrcEnd) then
          Break;
        if (LastHashed < Dst) then
          repeat // decompressed bytes should update the hash table
            Inc(LastHashed);
            V := PUInt32(LastHashed)^;
            Offset[((V shr 12) xor V) and 4095] := LastHashed;
          until (LastHashed >= Dst);
        Inc(Dst, T);
        LastHashed := Dst - 1;
        CWBit := CWBit shl 1;
        if (CWBit <> 0) then
          Continue
        else
          goto NextCW;
      end;
    until False;
end;

function SynLZDecompressPas(Src: PByte; Size: Int32; Dst: PByte): Int32;
var
  Offset: TOffsets;
  SrcEnd: PByte;
begin
  SrcEnd := Src + Size;
  Result := PUInt16(Src)^;
  if (Result = 0) then
    Exit;
  Inc(Src, 2);
  if (Result and $8000 <> 0) then
  begin
    Result := (Result and $7FFF) or (Int32(PUInt16(Src)^) shl 15);
    Inc(Src, 2);
  end;
  SynLZDecompressPasSub(Src, SrcEnd, Dst, Offset);
end;

procedure SynLZDecompressPartialSub(Src, Dst, SrcEnd, DstEnd: PByte; var Offset: TOffsets);
var
  LastHashed: PByte; // initial Src and Dst value
  CWBit, CW: UInt32;
  V, T, H: PtrUInt;
  {$IFDEF CPU64}
  O: PByte;
  {$ENDIF CPU64}
label
  NextCW;
begin
  LastHashed := Dst - 1;
NextCW:
  CW := PUInt32(Src)^;
  Inc(Src, 4);
  CWBit := 1;
  if (Src < SrcEnd) then
    repeat
      if (CW and CWBit = 0) then
      begin
        Dst^ := Src^;
        Inc(Src);
        Inc(Dst);
        if (Src >= SrcEnd) or
           (Dst >= DstEnd) then
          Break;
        if (LastHashed < Dst - 3) then
        begin
          Inc(LastHashed);
          V := PUInt32(LastHashed)^;
          Offset[((V shr 12) xor V) and 4095] := LastHashed;
        end;
        CWBit := CWBit shl 1;
        if (CWBit <> 0) then
          Continue
        else
          goto NextCW;
      end
      else
      begin
        H := PUInt16(Src)^;
        Inc(Src, 2);
        T := (H and 15) + 2;
        H := H shr 4;
        if (T = 2) then
        begin
          T := Src^ + (16 + 2);
          Inc(Src);
        end;
        if (Dst + T >= DstEnd) then
        begin
          // avoid buffer overflow by all means
          MoveByOne(Offset[H], Dst, DstEnd - Dst);
          Break;
        end;
        {$IFDEF CPU64}
        O := Offset[H];
        if (T <= 8) or
           (PtrUInt(Dst - O) < T) then
          MoveByOne(O, Dst, T)
        else
          Move(O^, Dst^, T);
        {$ELSE}
        if (T <= 8) or
           (PtrUInt(Dst - Offset[H]) < T) then
          MoveByOne(Offset[H], Dst, T)
        else
          Move(Offset[H]^, Dst^, T);
        {$ENDIF CPU64}
        if (Src >= SrcEnd) then
          Break;
        if (LastHashed < Dst) then
          repeat
            Inc(LastHashed);
            V := PUInt32(LastHashed)^;
            Offset[((V shr 12) xor V) and 4095] := LastHashed;
          until (LastHashed >= Dst);
        Inc(Dst, T);
        LastHashed := Dst - 1;
        CWBit := CWBit shl 1;
        if (CWBit <> 0) then
          Continue
        else
          goto NextCW;
      end;
    until False;
end;

function SynLZDecompressPartial(Src: PByte; Size: Int32; Dst: PByte; MaxDst: Int32): Int32;
var
  Offset: TOffsets;
  SrcEnd: PByte;
begin
  SrcEnd := Src + Size;
  Result := PUInt16(Src)^;
  if (Result = 0) then
    Exit;
  Inc(Src, 2);
  if (Result and $8000 <> 0) then
  begin
    Result := (Result and $7FFF) or (Int32(PUInt16(Src)^) shl 15);
    Inc(Src, 2);
  end;
  if (MaxDst < Result) then
    Result := MaxDst;
  if (Result > 0) then
    SynLZDecompressPartialSub(Src, Dst, SrcEnd, Dst + Result, Offset);
end;

function SynLZCompress(Src: PByte; Size: Int32; Dst: PByte): Int32; inline;
begin
  {$IFDEF SYNLZ_ASM}
  Result := SynLZCompressAsm(Src, Size, Dst);
  {$ELSE}
  Result := SynLZCompressPas(Src, Size, Dst);
  {$ENDIF}
end;

function SynLZDecompress(Src: PByte; Size: Int32; Dst: PByte): Int32; inline;
begin
  {$IFDEF SYNLZ_ASM}
  Result := SynLZDecompressAsm(Src, Size, Dst);
  {$ELSE}
  Result := SynLZDecompressPas(Src, Size, Dst);
  {$ENDIF}
end;

// the header decides how much gets allocated, so it cannot be taken on trust
function GuardedDestLen(InData: PByte; InSize: Int64): Int32;
begin
  if (InSize < 2) or ((InData[1] and $80 <> 0) and (InSize < 4)) then
    CompressCodecException('SynLZ data is too small (%d bytes)', [InSize]);

  Result := SynLZDecompressedSize(InData);
  if (Result > InSize * MAX_EXPANSION + EXPANSION_SLACK) then
    CompressCodecException('SynLZ header claims %d bytes from %d', [Result, InSize]);
end;

class procedure SynLZ.Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Needed: PtrUInt;
begin
  Needed := SynLZCompressBound(InSize);
  if (OutData = nil) or (MemSize(OutData) < Needed) then
    ReAllocMem(OutData, Needed);

  OutSize := SynLZCompress(InData, InSize, OutData);
end;

class procedure SynLZ.Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Declared: Int32;
begin
  Declared := GuardedDestLen(InData, InSize);

  if (OutData = nil) or (MemSize(OutData) < PtrUInt(Declared) + DECOMPRESS_SLACK) then
    ReAllocMem(OutData, Declared + DECOMPRESS_SLACK);

  OutSize := SynLZDecompress(InData, InSize, OutData);
  if (OutSize <> Declared) then
    CompressCodecException('SynLZ produced %d bytes, header says %d', [OutSize, Declared]);
end;

class procedure SynLZ.DecompressPartial(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64; MaxOut: Int64);
var
  Declared, Want: Int32;
begin
  Declared := GuardedDestLen(InData, InSize);

  Want := Declared;
  if (MaxOut < Want) then
    Want := MaxOut;
  if (Want < 0) then
    Want := 0;

  if (OutData = nil) or (MemSize(OutData) < PtrUInt(Want) + 1) then
    ReAllocMem(OutData, Want + 1);

  OutSize := SynLZDecompressPartial(InData, InSize, OutData, Want);
end;

end.
