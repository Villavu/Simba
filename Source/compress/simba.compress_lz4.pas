{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  LZ4 by Yann Collet
    https://github.com/lz4/lz4
    https://github.com/Cyan4973/xxHash

    CompressBlock     LZ4_compress_generic    lz4.c
    DecompressBlock   LZ4_decompress_generic  lz4.c
    HashAt            LZ4_hashPosition        lz4.c
    XXHash            XXH32                   xxhash.h
}
unit simba.compress_lz4;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.compress_codec;

type
  LZ4 = class(TCompressCodec)
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
  end;

implementation

{$R-}{$Q-}

const
  FRAME_MAGIC = $184D2204;
  SKIP_MAGIC_LO = $184D2A50; // skippable frames, which carry no data
  SKIP_MAGIC_HI = $184D2A5F;

  FLG_CONTENT_CHECKSUM = $04;
  FLG_CONTENT_SIZE     = $08;
  FLG_BLOCK_CHECKSUM   = $10;
  FLG_BLOCK_INDEP      = $20;
  FLG_VERSION          = $40;
  FLG_DICT_ID          = $01;

  BLOCK_SIZE   = 4 * 1024 * 1024; // block max size id 7
  UNCOMPRESSED = UInt32($80000000); // set in a block's size to mean "stored"

  MIN_MATCH     = 4;
  MAX_DISTANCE  = 65535;
  LAST_LITERALS = 5;  // a block always ends with at least this many literals
  MF_LIMIT      = 12; // and no match may start within this much of the end
  ML_BITS       = 4;
  ML_MASK       = 15;
  RUN_MASK      = 15;
  LIT_CONTINUE  = 255; // a count that fills its nibble carries on in 255s

  // lz4's LZ4_HASHLOG, which is LZ4_MEMORY_USAGE - 2 at the default 14
  HASH_LOG   = 12;
  // byU16 hashes to HASH_LOG+1 bits; lz4 fits both in one buffer by making that
  // table 16 bit wide, we just size for the larger of the two
  HASH_SIZE  = 1 shl (HASH_LOG + 1);
  // below this, lz4 switches to the byU16 table and a 4 byte hash
  LIMIT_64K  = 65536 + (MF_LIMIT - 1);
  MIN_LENGTH = MF_LIMIT + 1;
  HASH5_PRIME = UInt64(889523592379);
  SKIP_TRIGGER = 6; // how fast the search step grows over incompressible data

  // what a frame costs on top of the blocks themselves
  FRAME_HEADER_SIZE = 15; // magic, FLG, BD, content size, header checksum
  FRAME_FOOTER_SIZE = 8;  // end mark and content checksum
  BLOCK_HEADER_SIZE = 4;  // each block is prefixed with its compressed size
  BLOCK_SLACK       = 4;  // the token and length bytes an all-literal block adds
  BOUND_SLACK       = 32; // headroom over the worst case, which is never reached
  GROW_MIN          = 65536; // first size a growing output buffer takes

  OS_UNKNOWN = 255;

type
  THashTable = array[0..HASH_SIZE-1] of Int32;

{ xxHash32, which is what the frame format checksums with }

const
  XXH_PRIME1 = UInt32(2654435761);
  XXH_PRIME2 = UInt32(2246822519);
  XXH_PRIME3 = UInt32(3266489917);
  XXH_PRIME4 = UInt32(668265263);
  XXH_PRIME5 = UInt32(374761393);

function RotL32(V: UInt32; Bits: Int32): UInt32; inline;
begin
  Result := (V shl Bits) or (V shr (32 - Bits));
end;

function XXHash32(Data: PByte; Len: PtrUInt; Seed: UInt32): UInt32;
var
  V1, V2, V3, V4, H: UInt32;
  Left: PtrUInt;
begin
  Left := Len;

  if (Left >= 16) then
  begin
    V1 := Seed + XXH_PRIME1 + XXH_PRIME2;
    V2 := Seed + XXH_PRIME2;
    V3 := Seed;
    V4 := Seed - XXH_PRIME1;

    repeat
      V1 := RotL32(V1 + LEtoN(PUInt32(Data)^) * XXH_PRIME2, 13) * XXH_PRIME1; Inc(Data, 4);
      V2 := RotL32(V2 + LEtoN(PUInt32(Data)^) * XXH_PRIME2, 13) * XXH_PRIME1; Inc(Data, 4);
      V3 := RotL32(V3 + LEtoN(PUInt32(Data)^) * XXH_PRIME2, 13) * XXH_PRIME1; Inc(Data, 4);
      V4 := RotL32(V4 + LEtoN(PUInt32(Data)^) * XXH_PRIME2, 13) * XXH_PRIME1; Inc(Data, 4);
      Dec(Left, 16);
    until (Left < 16);

    H := RotL32(V1, 1) + RotL32(V2, 7) + RotL32(V3, 12) + RotL32(V4, 18);
  end else
    H := Seed + XXH_PRIME5;

  Inc(H, UInt32(Len));

  while (Left >= 4) do
  begin
    H := RotL32(H + LEtoN(PUInt32(Data)^) * XXH_PRIME3, 17) * XXH_PRIME4;
    Inc(Data, 4);
    Dec(Left, 4);
  end;

  while (Left > 0) do
  begin
    H := RotL32(H + Data^ * XXH_PRIME5, 11) * XXH_PRIME1;
    Inc(Data);
    Dec(Left);
  end;

  H := H xor (H shr 15);
  H := H * XXH_PRIME2;
  H := H xor (H shr 13);
  H := H * XXH_PRIME3;
  H := H xor (H shr 16);

  Result := H;
end;

{ block format }

// LZ4_hashPosition: hash5 over eight bytes for the byU32 table, hash4 over four
// for byU16, which is what a 64 bit build of lz4 does
function HashAt(Src: PByte; Pos: Int32; ByU16: Boolean): UInt32; inline;
var
  V: UInt32;
begin
  if ByU16 then
  begin
    // through a UInt32 first: the constant is wider than Int32, so the multiply
    // would otherwise be done at 64 bits and never wrap
    V := LEtoN(PUInt32(Src + Pos)^) * XXH_PRIME1;
    Result := V shr (32 - (HASH_LOG + 1));
  end else
    Result := UInt32(((LEtoN(PUInt64(Src + Pos)^) shl 24) * HASH5_PRIME) shr (64 - HASH_LOG));
end;

function CompressBlock(Src: PByte; SrcSize: Int32; Dst: PByte; var Hash: THashTable): Int32;
var
  Pos, Anchor, Ref, DstPos, Token, Step, Search, Forward_, Cur: Int32;
  MFLimitPlusOne, MatchLimit, LitLen, MatchLen, Left, MatchIndex: Int32;
  H, ForwardH: UInt32;
  ByU16, Found, Chained, Done: Boolean;
begin
  // an empty slot reads as index 0, exactly as lz4 leaves it - a candidate the
  // distance test and the four byte compare filter out
  FillDWord(Hash, HASH_SIZE, 0);

  ByU16 := SrcSize < LIMIT_64K;
  DstPos := 0;
  Anchor := 0;
  Pos := 0;
  MFLimitPlusOne := SrcSize - MF_LIMIT + 1;
  MatchLimit := SrcSize - LAST_LITERALS;
  Done := False;
  Ref := 0;

  if (SrcSize >= MIN_LENGTH) then
  begin
    Hash[HashAt(Src, 0, ByU16)] := 0;
    Pos := 1;
    ForwardH := HashAt(Src, 1, ByU16);

    while not Done do
    begin
      Step := 1;
      Search := 1 shl SKIP_TRIGGER;
      Forward_ := Pos;
      Found := False;
      repeat
        H := ForwardH;
        Cur := Forward_;
        MatchIndex := Hash[H];
        Pos := Forward_;
        Inc(Forward_, Step);
        Step := Search shr SKIP_TRIGGER;
        Inc(Search);
        if (Forward_ > MFLimitPlusOne) then
          Break;
        Ref := MatchIndex;
        Hash[H] := Cur;
        ForwardH := HashAt(Src, Forward_, ByU16);
        if (Cur - MatchIndex <= MAX_DISTANCE) and (PUInt32(Src + Ref)^ = PUInt32(Src + Pos)^) then
        begin
          Found := True;
          Break;
        end;
      until False;
      if not Found then
        Break;

      // a match often starts a byte or two before where the hash found it
      while (Pos > Anchor) and (Ref > 0) and (Src[Pos-1] = Src[Ref-1]) do
      begin
        Dec(Pos);
        Dec(Ref);
      end;

      LitLen := Pos - Anchor;
      Token := DstPos;
      Inc(DstPos);
      if (LitLen >= RUN_MASK) then
      begin
        Dst[Token] := RUN_MASK shl ML_BITS;
        Left := LitLen - RUN_MASK;
        while (Left >= LIT_CONTINUE) do
        begin
          Dst[DstPos] := LIT_CONTINUE;
          Inc(DstPos);
          Dec(Left, LIT_CONTINUE);
        end;
        Dst[DstPos] := Byte(Left);
        Inc(DstPos);
      end else
        Dst[Token] := Byte(LitLen shl ML_BITS);

      if (LitLen > 0) then
      begin
        Move(Src[Anchor], Dst[DstPos], LitLen);
        Inc(DstPos, LitLen);
      end;

      // lz4's _next_match: re-entered with a zero literal token when the
      // position right after a match is itself a match
      repeat
        Chained := False;

        PUInt16(Dst + DstPos)^ := NtoLE(UInt16(Pos - Ref));
        Inc(DstPos, 2);

        Inc(Pos, MIN_MATCH);
        Inc(Ref, MIN_MATCH);
        MatchLen := 0;
        while (Pos + MatchLen < MatchLimit) and (Src[Ref + MatchLen] = Src[Pos + MatchLen]) do
          Inc(MatchLen);
        Inc(Pos, MatchLen);

        if (MatchLen >= ML_MASK) then
        begin
          Dst[Token] := Dst[Token] or ML_MASK;
          Left := MatchLen - ML_MASK;
          while (Left >= LIT_CONTINUE) do
          begin
            Dst[DstPos] := LIT_CONTINUE;
            Inc(DstPos);
            Dec(Left, LIT_CONTINUE);
          end;
          Dst[DstPos] := Byte(Left);
          Inc(DstPos);
        end else
          Dst[Token] := Dst[Token] or Byte(MatchLen);

        Anchor := Pos;
        if (Pos >= MFLimitPlusOne) then
        begin
          Done := True;
          Break;
        end;

        // index the position two back, then test the one we are on
        Hash[HashAt(Src, Pos - 2, ByU16)] := Pos - 2;

        H := HashAt(Src, Pos, ByU16);
        MatchIndex := Hash[H];
        Hash[H] := Pos;
        if (Pos - MatchIndex <= MAX_DISTANCE) and (PUInt32(Src + MatchIndex)^ = PUInt32(Src + Pos)^) then
        begin
          Ref := MatchIndex;
          Token := DstPos;
          Dst[Token] := 0;
          Inc(DstPos);
          Chained := True;
        end;
      until not Chained;

      if not Done then
      begin
        Inc(Pos);
        ForwardH := HashAt(Src, Pos, ByU16);
      end;
    end;
  end;

  // whatever is left over has to go out as literals
  LitLen := SrcSize - Anchor;
  Token := DstPos;
  Inc(DstPos);
  if (LitLen >= RUN_MASK) then
  begin
    Dst[Token] := RUN_MASK shl ML_BITS;
    Left := LitLen - RUN_MASK;
    while (Left >= LIT_CONTINUE) do
    begin
      Dst[DstPos] := LIT_CONTINUE;
      Inc(DstPos);
      Dec(Left, LIT_CONTINUE);
    end;
    Dst[DstPos] := Byte(Left);
    Inc(DstPos);
  end else
    Dst[Token] := Byte(LitLen shl ML_BITS);

  if (LitLen > 0) then
  begin
    Move(Src[Anchor], Dst[DstPos], LitLen);
    Inc(DstPos, LitLen);
  end;

  Result := DstPos;
end;

// LowLimit is as far back as a match may reach: the frame start for linked
// blocks, this block's own start for independent ones. PtrInt because a small
// frame can decode to over 2GB and the length accumulators would wrap.
function DecompressBlock(Src: PByte; SrcSize: Int32; Dst: PByte; DstPos, DstCapacity, LowLimit: PtrInt): PtrInt;
var
  SrcPos, Token, Offset: Int32;
  LitLen, MatchLen, I: PtrInt;
  B: Byte;
  EndedOnLiterals: Boolean;
begin
  SrcPos := 0;
  EndedOnLiterals := False;

  while (SrcPos < SrcSize) do
  begin
    Token := Src[SrcPos];
    Inc(SrcPos);

    LitLen := Token shr ML_BITS;
    if (LitLen = RUN_MASK) then
      repeat
        if (SrcPos >= SrcSize) then
          CompressCodecException('truncated lz4 literal length');
        B := Src[SrcPos];
        Inc(SrcPos);
        Inc(LitLen, B);
      until (B <> LIT_CONTINUE);

    if (SrcPos + LitLen > SrcSize) or (DstPos + LitLen > DstCapacity) then
      CompressCodecException('lz4 literals run past the end of the block');
    if (LitLen > 0) then
    begin
      Move(Src[SrcPos], Dst[DstPos], LitLen);
      Inc(SrcPos, LitLen);
      Inc(DstPos, LitLen);
    end;

    if (SrcPos = SrcSize) then
    begin
      EndedOnLiterals := True;
      Break; // the last sequence is literals only
    end;

    if (SrcPos + 2 > SrcSize) then
      CompressCodecException('truncated lz4 match offset');
    Offset := LEtoN(PUInt16(Src + SrcPos)^);
    Inc(SrcPos, 2);
    if (Offset = 0) or (Offset > DstPos - LowLimit) then
      CompressCodecException('lz4 match reaches back before what this block may see');

    MatchLen := Token and ML_MASK;
    if (MatchLen = ML_MASK) then
      repeat
        if (SrcPos >= SrcSize) then
          CompressCodecException('truncated lz4 match length');
        B := Src[SrcPos];
        Inc(SrcPos);
        Inc(MatchLen, B);
      until (B <> LIT_CONTINUE);
    Inc(MatchLen, MIN_MATCH);

    if (DstPos + MatchLen > DstCapacity) then
      CompressCodecException('lz4 match runs past the end of the block');

    if (Offset >= MatchLen) then
      Move(Dst[DstPos - Offset], Dst[DstPos], MatchLen)
    else
      for I := 0 to MatchLen - 1 do // overlapping, so it has to go a byte at a time
        Dst[DstPos + I] := Dst[DstPos - Offset + I];
    Inc(DstPos, MatchLen);
  end;

  // the block format ends every block on a literal run, never on a match
  if (SrcSize > 0) and (not EndedOnLiterals) then
    CompressCodecException('lz4 block ends on a match rather than literals');

  Result := DstPos;
end;

{ frame format }

class procedure LZ4.Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Hash: ^THashTable;
  Pos, Chunk: Int64;
  Needed, Compressed: PtrUInt;
  Header: Int32;
begin
  // Worst case a block is one all-literal sequence: every byte survives, plus a
  // continuation byte per 255 of them, plus the per block and per frame headers.
  Needed := InSize + (InSize div LIT_CONTINUE) +
            FRAME_HEADER_SIZE + FRAME_FOOTER_SIZE +
            ((InSize div BLOCK_SIZE) + 1) * (BLOCK_HEADER_SIZE + BLOCK_SLACK) +
            BOUND_SLACK;
  if (OutData = nil) or (MemSize(OutData) < Needed) then
    ReAllocMem(OutData, Needed);

  PUInt32(OutData)^ := NtoLE(UInt32(FRAME_MAGIC));
  OutData[4] := FLG_VERSION or FLG_BLOCK_INDEP or FLG_CONTENT_SIZE or FLG_CONTENT_CHECKSUM;
  OutData[5] := $70; // block max size 4MB
  PUInt64(OutData + 6)^ := NtoLE(UInt64(InSize));
  OutData[14] := Byte((XXHash32(OutData + 4, 10, 0) shr 8) and $FF);
  OutSize := 15;

  New(Hash);
  try
    Pos := 0;
    while (Pos < InSize) do
    begin
      Chunk := InSize - Pos;
      if (Chunk > BLOCK_SIZE) then
        Chunk := BLOCK_SIZE;

      Header := OutSize;
      Inc(OutSize, 4);
      Compressed := CompressBlock(InData + Pos, Chunk, OutData + OutSize, Hash^);

      if (Compressed >= PtrUInt(Chunk)) then
      begin
        // compressing made it bigger, so store it as it came in
        Move((InData + Pos)^, (OutData + OutSize)^, Chunk);
        PUInt32(OutData + Header)^ := NtoLE(UInt32(Chunk) or UNCOMPRESSED);
        Inc(OutSize, Chunk);
      end else
      begin
        PUInt32(OutData + Header)^ := NtoLE(UInt32(Compressed));
        Inc(OutSize, Compressed);
      end;

      Inc(Pos, Chunk);
    end;
  finally
    Dispose(Hash);
  end;

  PUInt32(OutData + OutSize)^ := 0; // end mark
  Inc(OutSize, 4);
  PUInt32(OutData + OutSize)^ := NtoLE(XXHash32(InData, InSize, 0));
  Inc(OutSize, 4);
end;

class procedure LZ4.Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  Pos, FrameStart, ContentStart, BlockStart: Int64;
  Magic, BlockSize, Stored, SkipLen: UInt32;
  Flags, BD: Byte;
  MaxBlock: Int32;
  Produced, BlockCap, LowLimit: PtrInt;
  Capacity: PtrUInt;

  procedure Reserve(Extra: PtrUInt);
  begin
    if (PtrUInt(OutSize) + Extra > Capacity) then
    begin
      if (Capacity < GROW_MIN) then
        Capacity := GROW_MIN;
      while (Capacity < PtrUInt(OutSize) + Extra) do
        Capacity := Capacity * 2;
      ReAllocMem(OutData, Capacity);
    end;
  end;

begin
  OutSize := 0;
  if (OutData = nil) then
    Capacity := 0
  else
    Capacity := MemSize(OutData);

  Pos := 0;
  while (Pos + 4 <= InSize) do
  begin
    Magic := LEtoN(PUInt32(InData + Pos)^);

    if (Magic >= SKIP_MAGIC_LO) and (Magic <= SKIP_MAGIC_HI) then
    begin
      if (Pos + 8 > InSize) then
        CompressCodecException('truncated lz4 skippable frame');
      SkipLen := LEtoN(PUInt32(InData + Pos + 4)^);
      if (SkipLen > UInt32(InSize - (Pos + 8))) then
        CompressCodecException('truncated lz4 skippable frame');
      Pos := Pos + 8 + SkipLen;
      Continue;
    end;

    if (Magic <> FRAME_MAGIC) then
    begin
      if (Pos = 0) then
        CompressCodecException('not an lz4 frame');
      Break; // trailing data after a complete frame
    end;

    FrameStart := Pos + 4;
    if (FrameStart + 3 > InSize) then
      CompressCodecException('truncated lz4 frame descriptor');

    Flags := InData[FrameStart];
    BD := InData[FrameStart + 1];
    if (Flags and $C0 <> FLG_VERSION) then
      CompressCodecException('unsupported lz4 frame version');
    // reserved bits must be clear, or this is a format revision we cannot read
    if (Flags and $02 <> 0) then
      CompressCodecException('lz4 frame sets a reserved flag bit');
    if (BD and $8F <> 0) then
      CompressCodecException('lz4 frame sets a reserved block descriptor bit');

    Pos := FrameStart + 2;
    if (Flags and FLG_CONTENT_SIZE <> 0) then
      Inc(Pos, 8);
    if (Flags and FLG_DICT_ID <> 0) then
      Inc(Pos, 4);
    if (Pos + 1 > InSize) then
      CompressCodecException('truncated lz4 frame descriptor');
    if (InData[Pos] <> Byte((XXHash32(InData + FrameStart, Pos - FrameStart, 0) shr 8) and $FF)) then
      CompressCodecException('lz4 frame header checksum mismatch');
    Inc(Pos);

    case (BD shr 4) and 7 of
      4: MaxBlock := 64 * 1024;
      5: MaxBlock := 256 * 1024;
      6: MaxBlock := 1024 * 1024;
      7: MaxBlock := 4 * 1024 * 1024;
    else
      CompressCodecException('invalid lz4 block maximum size');
    end;

    ContentStart := OutSize;
    while True do
    begin
      if (Pos + 4 > InSize) then
        CompressCodecException('truncated lz4 block header');
      BlockSize := LEtoN(PUInt32(InData + Pos)^);
      Inc(Pos, 4);
      if (BlockSize = 0) then
        Break;

      Stored := BlockSize and UNCOMPRESSED;
      BlockSize := BlockSize and not UNCOMPRESSED;
      if (BlockSize > UInt32(MaxBlock)) or (Pos + BlockSize > InSize) then
        CompressCodecException('lz4 block runs past the end of the data');
      BlockStart := Pos;

      if (Stored <> 0) then
      begin
        Reserve(BlockSize);
        Move((InData + Pos)^, (OutData + OutSize)^, BlockSize);
        Inc(OutSize, BlockSize);
      end else
      begin
        Reserve(MaxBlock);

        // no block may decode to more than the frame's declared maximum
        BlockCap := (OutSize - ContentStart) + MaxBlock;
        if (BlockCap > PtrInt(Capacity) - (ContentStart)) then
          BlockCap := PtrInt(Capacity) - ContentStart;

        // and a match may only reach into earlier blocks when the frame says
        // its blocks are linked - independent ones start from nothing
        if (Flags and FLG_BLOCK_INDEP <> 0) then
          LowLimit := OutSize - ContentStart
        else
          LowLimit := 0;

        Produced := DecompressBlock(InData + Pos, BlockSize, OutData + ContentStart,
                                    OutSize - ContentStart, BlockCap, LowLimit);
        OutSize := ContentStart + Produced;
      end;
      Inc(Pos, BlockSize);

      if (Flags and FLG_BLOCK_CHECKSUM <> 0) then
      begin
        if (Pos + 4 > InSize) then
          CompressCodecException('lz4 block checksum is missing');
        if (LEtoN(PUInt32(InData + Pos)^) <> XXHash32(InData + BlockStart, BlockSize, 0)) then
          CompressCodecException('lz4 block checksum mismatch');
        Inc(Pos, 4);
      end;
    end;

    if (Flags and FLG_CONTENT_CHECKSUM <> 0) then
    begin
      if (Pos + 4 > InSize) then
        CompressCodecException('lz4 content checksum is missing');
      if (LEtoN(PUInt32(InData + Pos)^) <> XXHash32(OutData + ContentStart, OutSize - ContentStart, 0)) then
        CompressCodecException('lz4 content checksum mismatch');
      Inc(Pos, 4);
    end;

    if (Flags and FLG_CONTENT_SIZE <> 0) then
      if (LEtoN(PUInt64(InData + FrameStart + 2)^) <> UInt64(OutSize - ContentStart)) then
        CompressCodecException('lz4 content size mismatch');
  end;
end;

end.
