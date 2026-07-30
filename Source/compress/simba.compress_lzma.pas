{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The LZMA specification by Igor Pavlov https://www.7-zip.org/sdk.html
  The encoder is a port of liblzma `xz --format=lzma`
    https://github.com/tukaani-project/xz (src/liblzma)

  OptimumNormal          lzma_lzma_optimum_normal   lzma_encoder_optimum_normal.c
  Helper1, Helper2       helper1, helper2
  Backward               backward
  MfFind, MfSkip         lzma_mf_find, mf_skip      lz_encoder_mf.c
  Bt4Find                lzma_mf_bt4_find
  BtFindFunc, BtSkipFunc bt_find_func, bt_skip_func
  Hash4Calc              hash_4_calc                lz_encoder_hash.h
  FillDistPrices         fill_dist_prices           lzma_encoder_optimum_normal.c
  FillAlignPrices        fill_align_prices
  LengthUpdatePrices     length_update_prices       lzma_encoder.c
  EncodeSymbol           encode_symbol
  EmitLiteral            literal, literal_matched
  EmitMatch              match
  EmitRep, EmitShortRep  rep_match
  EncodeLen              length
  ShiftLow               rc_shift_low               range_encoder.h
  BitPrice               rc_bit_price               price.h
}
unit simba.compress_lzma;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.crc, simba.compress_codec;

type
  LZMA = class(TCompressCodec)
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
  end;

implementation

{$R-}{$Q-}

const
  kNumBitModelTotalBits = 11;
  kBitModelTotal        = 1 shl kNumBitModelTotalBits;
  kNumMoveBits          = 5;
  kTopValue             = UInt32(1) shl 24;

  kNumPosBitsMax   = 4;
  kNumPosStatesMax = 1 shl kNumPosBitsMax;
  kNumStates       = 12;
  kNumLenToPosStates = 4;
  kNumPosSlotBits  = 6;
  kNumAlignBits    = 4;
  kAlignTableSize  = 1 shl kNumAlignBits;
  kEndPosModelIndex = 14;
  kNumFullDistances = 1 shl (kEndPosModelIndex shr 1);

  kMatchMinLen = 2;
  kMatchMaxLen = 273;

  kLenNumLowBits     = 3;
  kLenNumLowSymbols  = 1 shl kLenNumLowBits;
  kLenNumMidBits     = 3;
  kLenNumMidSymbols  = 1 shl kLenNumMidBits;
  kLenNumHighBits    = 8;
  kLenNumHighSymbols = 1 shl kLenNumHighBits;

  // a length coder, relative to its own base
  LenChoice  = 0;
  LenChoice2 = 1;
  LenLow     = 2;
  LenMid     = LenLow + (kNumPosStatesMax shl kLenNumLowBits);
  LenHigh    = LenMid + (kNumPosStatesMax shl kLenNumMidBits);
  kNumLenProbs = LenHigh + kLenNumHighSymbols;

  // where each group of probabilities starts
  pIsMatch     = 0;
  pIsRep       = pIsMatch + (kNumStates shl kNumPosBitsMax);
  pIsRepG0     = pIsRep + kNumStates;
  pIsRepG1     = pIsRepG0 + kNumStates;
  pIsRepG2     = pIsRepG1 + kNumStates;
  pIsRep0Long  = pIsRepG2 + kNumStates;
  pPosSlot     = pIsRep0Long + (kNumStates shl kNumPosBitsMax);
  pSpecPos     = pPosSlot + (kNumLenToPosStates shl kNumPosSlotBits);
  pAlign       = pSpecPos + (1 + kNumFullDistances - kEndPosModelIndex);
  pLenCoder    = pAlign + kAlignTableSize;
  pRepLenCoder = pLenCoder + kNumLenProbs;
  pLiteral     = pRepLenCoder + kNumLenProbs;

  // encoder settings - liblzma preset 6, and what it derives from them
  ENC_LC = 3;
  ENC_LP = 0;
  ENC_PB = 2;
  ENC_PB_MASK = (1 shl ENC_PB) - 1;

  DICT_SIZE   = 1 shl 23;
  CYCLIC_SIZE = DICT_SIZE + 1;
  NICE_LEN    = 64;
  DEPTH       = 16 + NICE_LEN div 2;
  DIST_TABLE_SIZE = 46;                          // 2 * ceil(log2(DICT_SIZE))
  LEN_TABLE_SIZE  = NICE_LEN + 1 - kMatchMinLen; // priced lengths, 2..NICE_LEN

  HASH_2_SIZE = 1 shl 10;
  HASH_3_SIZE = 1 shl 16;
  HASH_MASK   = $3FFFFF;                         // from the dictionary size
  FIX_3_HASH  = HASH_2_SIZE;
  FIX_4_HASH  = HASH_2_SIZE + HASH_3_SIZE;
  HASH_COUNT  = HASH_MASK + 1 + FIX_4_HASH;

  BUF_SLACK   = 16; // the match finder compares a little past the end of input

  HEADER_SIZE = 13;    // props byte, dictionary size, uncompressed size
  GROW_MIN    = 65536; // first size a growing output buffer takes

  // sixteenths of a bit, so a cheap decision still compares as an integer
  kNumBitPriceShiftBits = 4;
  kNumMoveReducingBits  = 4;
  PRICE_TABLE_SIZE = kBitModelTotal shr kNumMoveReducingBits;

  NUM_OPTS      = 4096; // how far ahead the parser will look
  MARK_LIT  = UInt32($FFFFFFFF);
  INF_PRICE = UInt32(1) shl 30;

  MATCH_PRICE_REFRESH = 128; // matches encoded before the distance prices are rebuilt

  // the SDK decodes into the caller's buffer and errors when it fills; we buffer
  // everything, so these stand in for that bound
  MAX_OUTPUT      = High(Int32);
  PREALLOC_RATIO  = 64;

  // What one binary decision costs, by probability. liblzma's lzma_rc_prices,
  // verbatim. Generated by integer squaring, which is not the same function as
  // rounding -log2(p): 67 of the 128 differ by one.
  LZMA_RC_PRICES: array[0..PRICE_TABLE_SIZE-1] of Byte = (
    128, 103,  91,  84,  78,  73,  69,  66,
     63,  61,  58,  56,  54,  52,  51,  49,
     48,  46,  45,  44,  43,  42,  41,  40,
     39,  38,  37,  36,  35,  34,  34,  33,
     32,  31,  31,  30,  29,  29,  28,  28,
     27,  26,  26,  25,  25,  24,  24,  23,
     23,  22,  22,  22,  21,  21,  20,  20,
     19,  19,  19,  18,  18,  17,  17,  17,
     16,  16,  16,  15,  15,  15,  14,  14,
     14,  13,  13,  13,  12,  12,  12,  11,
     11,  11,  11,  10,  10,  10,  10,   9,
      9,   9,   9,   8,   8,   8,   8,   7,
      7,   7,   7,   6,   6,   6,   6,   5,
      5,   5,   5,   5,   4,   4,   4,   4,
      3,   3,   3,   3,   3,   2,   2,   2,
      2,   2,   2,   1,   1,   1,   1,   1
  );

function LiteralProbCount(Lc, Lp: Int32): Int32; inline;
begin
  Result := $300 shl (Lc + Lp);
end;

function BitPrice(Prob: UInt16; Symbol: Int32): UInt32; inline;
begin
  if (Symbol = 0) then
    Result := LZMA_RC_PRICES[Prob shr kNumMoveReducingBits]
  else
    Result := LZMA_RC_PRICES[(Prob xor (kBitModelTotal - 1)) shr kNumMoveReducingBits];
end;

function GetPosSlot(Dist: UInt32): Int32;
var
  N: Int32;
begin
  if (Dist < 4) then
    Exit(Dist);
  N := 31;
  while ((Dist shr N) = 0) do
    Dec(N);
  Result := (N shl 1) or Int32((Dist shr (N - 1)) and 1);
end;

{ the twelve state machine }

function IsLiteralState(S: Int32): Boolean; inline;
begin
  Result := (S < 7);
end;

procedure UpdateLiteral(var S: Int32); inline;
begin
  if (S < 4) then
    S := 0
  else if (S < 10) then
    Dec(S, 3)
  else
    Dec(S, 6);
end;

procedure UpdateMatch(var S: Int32); inline;
begin
  if (S < 7) then S := 7 else S := 10;
end;

procedure UpdateLongRep(var S: Int32); inline;
begin
  if (S < 7) then S := 8 else S := 11;
end;

procedure UpdateShortRep(var S: Int32); inline;
begin
  if (S < 7) then S := 9 else S := 11;
end;

{ compression }

type
  TRangeEncoder = record
    Data: PByte;
    Pos, Cap: PtrUInt;
    Low: UInt64;
    Range: UInt32;
    CacheSize: UInt64;
    Cache: Byte;

    procedure Init(AData: PByte);
    procedure PutByte(B: Byte);
    procedure ShiftLow;
    procedure EncodeBit(var Prob: UInt16; Symbol: Int32);
    procedure EncodeDirect(Value: UInt32; NumBits: Int32);
    procedure Flush;
  end;

  TMatch = record
    Len, Dist: UInt32;
  end;

  TReps = array[0..3] of UInt32;

  // cheapest route to this position, and the coder state it implies
  TOptimal = record
    Price: UInt32;
    State: Int32;
    PosPrev: Int32;
    BackPrev: UInt32;   // MARK_LIT, 0..3 for a rep, else dist + 4
    // a literal, and possibly a whole symbol before it, folded into this step
    Prev1IsLiteral: Boolean;
    Prev2: Boolean;
    PosPrev2: Int32;
    BackPrev2: UInt32;
    Backs: TReps;
  end;

  PLZMAEncoder = ^TLZMAEncoder;
  TLZMAEncoder = record
    RC: TRangeEncoder;
    Probs: PUInt16;

    State: Int32;
    Reps: TReps;
    UncompSize: Int32;

    // the input window. The match finder compares a few bytes past what is
    // available, so this is our own copy with a zeroed tail
    Buf: PByte;
    BufSize: Int32;
    ReadPos, ReadAhead, CyclicPos, Offset: UInt32;

    Hash, Son: PUInt32;

    Matches: array[0..kMatchMaxLen] of TMatch;
    MatchesCount: Int32;
    LongestMatchLength: Int32;

    Opts: array[0..NUM_OPTS-1] of TOptimal;
    OptsEndIndex, OptsCurrentIndex: Int32;

    // snapshotted prices, on three refresh cadences
    MatchPriceCount, AlignPriceCount: UInt32;
    LenCounters: array[0..1, 0..kNumPosStatesMax-1] of Int32;
    LenPrices: array[0..1, 0..kNumPosStatesMax-1, 0..LEN_TABLE_SIZE-1] of UInt32;
    DistSlotPrices: array[0..kNumLenToPosStates-1, 0..DIST_TABLE_SIZE-1] of UInt32;
    DistPrices: array[0..kNumLenToPosStates-1, 0..kNumFullDistances-1] of UInt32;
    AlignPrices: array[0..kAlignTableSize-1] of UInt32;

    function MemCmpLen(P1, P2, Len, Limit: Int32): Int32;
    procedure Hash4Calc(Cur: Int32; out H2, H3, HV: UInt32);
    procedure MovePos;
    function BtFindFunc(LenLimit, Cur: Int32; Pos, CurMatch: UInt32; Count, LenBest: Int32): Int32;
    procedure BtSkipFunc(LenLimit, Cur: Int32; Pos, CurMatch: UInt32);
    function Bt4Find: Int32;
    function MfFind: Int32;
    procedure MfSkip(Amount: Int32);

    procedure BitTreeEncode(Base, NumBits, Symbol: Int32);
    procedure BitTreeReverseEncode(Base, NumBits, Symbol: Int32);
    procedure EncodeLen(CoderIdx, PosState, Len: Int32);
    procedure EncodeDistance(Len: Int32; Dist: UInt32);

    function TreePrice(Base, NumBits, Symbol: Int32): UInt32;
    function TreeReversePrice(Base, NumBits, Symbol: Int32): UInt32;
    function LiteralPrice(Position, PrevByte: Int32; MatchMode: Boolean; MatchByte, Symbol: Int32): UInt32;
    function ShortRepPrice(AState, PosState: Int32): UInt32;
    function PureRepPrice(Index, AState, PosState: Int32): UInt32;
    function RepPrice(Index, Len, AState, PosState: Int32): UInt32;
    function DistLenPrice(Dist: UInt32; Len, PosState: Int32): UInt32;
    procedure LengthUpdatePrices(CoderIdx, PosState: Int32);
    procedure FillDistPrices;
    procedure FillAlignPrices;

    procedure EmitLiteral(Position, PosState: Int32);
    procedure EmitMatch(PosState, Len: Int32; Dist: UInt32);
    procedure EmitRep(PosState, Index, Len: Int32);
    procedure EmitShortRep(PosState: Int32);
    procedure EncodeSymbol(Back: UInt32; Len, Position: Int32);

    procedure MakeLiteral(Idx: Int32); inline;
    procedure MakeShortRep(Idx: Int32); inline;
    function Helper1(Position: Int32; out BackRes: UInt32; out LenRes: Int32): Int32;
    function Helper2(var R: TReps; LenEnd, Position, Cur, BufAvailFull: Int32): Int32;
    procedure Backward(Cur: Int32; out BackRes: UInt32; out LenRes: Int32);
    procedure OptimumNormal(Position: Int32; out BackRes: UInt32; out LenRes: Int32);
    procedure Run;
  end;

procedure TRangeEncoder.Init(AData: PByte);
begin
  Data := AData;
  Pos := 0;
  if (Data = nil) then
    Cap := 0
  else
    Cap := MemSize(Data);
  Low := 0;
  Range := $FFFFFFFF;
  CacheSize := 1;
  Cache := 0;
end;

procedure TRangeEncoder.PutByte(B: Byte);
begin
  if (Pos >= Cap) then
  begin
    if (Cap < GROW_MIN) then
      Cap := GROW_MIN
    else
      Cap := Cap * 2;
    ReAllocMem(Data, Cap);
  end;
  Data[Pos] := B;
  Inc(Pos);
end;

// Low can carry into bytes already produced, so $FF runs are held as a count
// until the next byte settles them
procedure TRangeEncoder.ShiftLow;
var
  Temp: Byte;
begin
  if (UInt32(Low) < UInt32($FF000000)) or ((Low shr 32) <> 0) then
  begin
    Temp := Cache;
    repeat
      PutByte(Byte(Temp + Byte(Low shr 32)));
      Temp := $FF;
      Dec(CacheSize);
    until (CacheSize = 0);
    Cache := Byte(UInt32(Low) shr 24);
  end;
  Inc(CacheSize);
  Low := (Low shl 8) and $FFFFFFFF;
end;

procedure TRangeEncoder.EncodeBit(var Prob: UInt16; Symbol: Int32);
var
  Bound: UInt32;
begin
  Bound := (Range shr kNumBitModelTotalBits) * Prob;
  if (Symbol = 0) then
  begin
    Range := Bound;
    Inc(Prob, (kBitModelTotal - Prob) shr kNumMoveBits);
  end else
  begin
    Inc(Low, Bound);
    Dec(Range, Bound);
    Dec(Prob, Prob shr kNumMoveBits);
  end;

  while (Range < kTopValue) do
  begin
    Range := Range shl 8;
    ShiftLow();
  end;
end;

procedure TRangeEncoder.EncodeDirect(Value: UInt32; NumBits: Int32);
begin
  while (NumBits > 0) do
  begin
    Dec(NumBits);
    Range := Range shr 1;
    if ((Value shr NumBits) and 1) <> 0 then
      Inc(Low, Range);
    while (Range < kTopValue) do
    begin
      Range := Range shl 8;
      ShiftLow();
    end;
  end;
end;

procedure TRangeEncoder.Flush;
var
  I: Int32;
begin
  for I := 1 to 5 do
    ShiftLow();
end;

{ the match finder - bt4, a binary search tree over a four byte hash }

function TLZMAEncoder.MemCmpLen(P1, P2, Len, Limit: Int32): Int32;
begin
  Result := Len;
  while (Result < Limit) and (Buf[P1 + Result] = Buf[P2 + Result]) do
    Inc(Result);
end;

// liblzma hashes the first and fourth byte through the CRC-32 table, which is
// the same table simba.crc checksums with - lzma_lz_hash_table is that table
procedure TLZMAEncoder.Hash4Calc(Cur: Int32; out H2, H3, HV: UInt32);
var
  Temp: UInt32;
begin
  Temp := CRC32_TABLE[Buf[Cur]] xor UInt32(Buf[Cur + 1]);
  H2 := Temp and (HASH_2_SIZE - 1);
  Temp := Temp xor (UInt32(Buf[Cur + 2]) shl 8);
  H3 := Temp and (HASH_3_SIZE - 1);
  HV := (Temp xor (CRC32_TABLE[Buf[Cur + 3]] shl 5)) and HASH_MASK;
end;

// Offset starts at CYCLIC_SIZE so an empty hash slot reads as too far back.
// Int32 input means it cannot wrap, so liblzma's normalize() has nothing to do.
procedure TLZMAEncoder.MovePos;
begin
  Inc(CyclicPos);
  if (CyclicPos = CYCLIC_SIZE) then
    CyclicPos := 0;
  Inc(ReadPos);
end;

// walks the tree at Cur, splitting it into the two subtrees rooted at the
// children of Cur as it goes. Matches come out in descent order, with strictly
// increasing lengths - not nearest first.
function TLZMAEncoder.BtFindFunc(LenLimit, Cur: Int32; Pos, CurMatch: UInt32; Count, LenBest: Int32): Int32;
var
  Ptr0, Ptr1, Pair, Delta: UInt32;
  Len0, Len1, Len, Left, PB: Int32;
begin
  Ptr0 := (CyclicPos shl 1) + 1;
  Ptr1 := (CyclicPos shl 1);
  Len0 := 0;
  Len1 := 0;
  Left := DEPTH;
  Result := Count;

  while True do
  begin
    Delta := Pos - CurMatch;
    if (Left = 0) or (Delta >= CYCLIC_SIZE) then
    begin
      Son[Ptr0] := 0;
      Son[Ptr1] := 0;
      Exit;
    end;
    Dec(Left);

    if (Delta > CyclicPos) then
      Pair := (CyclicPos - Delta + CYCLIC_SIZE) shl 1
    else
      Pair := (CyclicPos - Delta) shl 1;

    PB := Cur - Int32(Delta);
    if (Len0 < Len1) then Len := Len0 else Len := Len1;

    if (Buf[PB + Len] = Buf[Cur + Len]) then
    begin
      Len := MemCmpLen(PB, Cur, Len + 1, LenLimit);
      if (LenBest < Len) then
      begin
        LenBest := Len;
        Matches[Result].Len := Len;
        Matches[Result].Dist := Delta - 1;
        Inc(Result);
        if (Len = LenLimit) then
        begin
          Son[Ptr1] := Son[Pair];
          Son[Ptr0] := Son[Pair + 1];
          Exit;
        end;
      end;
    end;

    if (Buf[PB + Len] < Buf[Cur + Len]) then
    begin
      Son[Ptr1] := CurMatch;
      Ptr1 := Pair + 1;
      CurMatch := Son[Ptr1];
      Len1 := Len;
    end else
    begin
      Son[Ptr0] := CurMatch;
      Ptr0 := Pair;
      CurMatch := Son[Ptr0];
      Len0 := Len;
    end;
  end;
end;

procedure TLZMAEncoder.BtSkipFunc(LenLimit, Cur: Int32; Pos, CurMatch: UInt32);
var
  Ptr0, Ptr1, Pair, Delta: UInt32;
  Len0, Len1, Len, Left, PB: Int32;
begin
  Ptr0 := (CyclicPos shl 1) + 1;
  Ptr1 := (CyclicPos shl 1);
  Len0 := 0;
  Len1 := 0;
  Left := DEPTH;

  while True do
  begin
    Delta := Pos - CurMatch;
    if (Left = 0) or (Delta >= CYCLIC_SIZE) then
    begin
      Son[Ptr0] := 0;
      Son[Ptr1] := 0;
      Exit;
    end;
    Dec(Left);

    if (Delta > CyclicPos) then
      Pair := (CyclicPos - Delta + CYCLIC_SIZE) shl 1
    else
      Pair := (CyclicPos - Delta) shl 1;

    PB := Cur - Int32(Delta);
    if (Len0 < Len1) then Len := Len0 else Len := Len1;

    if (Buf[PB + Len] = Buf[Cur + Len]) then
    begin
      Len := MemCmpLen(PB, Cur, Len + 1, LenLimit);
      if (Len = LenLimit) then
      begin
        Son[Ptr1] := Son[Pair];
        Son[Ptr0] := Son[Pair + 1];
        Exit;
      end;
    end;

    if (Buf[PB + Len] < Buf[Cur + Len]) then
    begin
      Son[Ptr1] := CurMatch;
      Ptr1 := Pair + 1;
      CurMatch := Son[Ptr1];
      Len1 := Len;
    end else
    begin
      Son[Ptr0] := CurMatch;
      Ptr0 := Pair;
      CurMatch := Son[Ptr0];
      Len0 := Len;
    end;
  end;
end;

// fills Matches, returns how many. The two and three byte hashes are checked
// first: one byte compared equal plus an equal hash pins the rest of them.
function TLZMAEncoder.Bt4Find: Int32;
var
  LenLimit, Cur, LenBest, Count: Int32;
  Pos, H2, H3, HV, Delta2, Delta3, CurMatch: UInt32;
begin
  LenLimit := BufSize - Int32(ReadPos);
  if (NICE_LEN <= LenLimit) then
    LenLimit := NICE_LEN
  else if (LenLimit < 4) then
  begin
    Inc(ReadPos); // too little input left to hash; nothing enters the tree
    Exit(0);
  end;

  Cur := ReadPos;
  Pos := ReadPos + Offset;
  Count := 0;

  Hash4Calc(Cur, H2, H3, HV);
  Delta2 := Pos - Hash[H2];
  Delta3 := Pos - Hash[FIX_3_HASH + H3];
  CurMatch := Hash[FIX_4_HASH + HV];

  Hash[H2] := Pos;
  Hash[FIX_3_HASH + H3] := Pos;
  Hash[FIX_4_HASH + HV] := Pos;

  LenBest := 1;
  if (Delta2 < CYCLIC_SIZE) and (Buf[Cur - Int32(Delta2)] = Buf[Cur]) then
  begin
    LenBest := 2;
    Matches[0].Len := 2;
    Matches[0].Dist := Delta2 - 1;
    Count := 1;
  end;

  if (Delta2 <> Delta3) and (Delta3 < CYCLIC_SIZE) and (Buf[Cur - Int32(Delta3)] = Buf[Cur]) then
  begin
    LenBest := 3;
    Matches[Count].Dist := Delta3 - 1;
    Inc(Count);
    Delta2 := Delta3;
  end;

  if (Count <> 0) then
  begin
    LenBest := MemCmpLen(Cur, Cur - Int32(Delta2), LenBest, LenLimit);
    Matches[Count - 1].Len := LenBest;
    if (LenBest = LenLimit) then
    begin
      BtSkipFunc(LenLimit, Cur, Pos, CurMatch);
      MovePos();
      Exit(Count);
    end;
  end;

  if (LenBest < 3) then
    LenBest := 3;
  Result := BtFindFunc(LenLimit, Cur, Pos, CurMatch, Count, LenBest);
  MovePos();
end;

// a match that reached nice_len is extended here, but the entry in Matches
// keeps the shorter length. The parser wants both, and uses each in one place.
function TLZMAEncoder.MfFind: Int32;
var
  Count, Limit, P1, P2: Int32;
begin
  Count := Bt4Find();
  Result := 0;

  if (Count > 0) then
  begin
    Result := Matches[Count - 1].Len;
    if (Result = NICE_LEN) then
    begin
      Limit := BufSize - Int32(ReadPos) + 1;
      if (Limit > kMatchMaxLen) then
        Limit := kMatchMaxLen;
      P1 := Int32(ReadPos) - 1;
      P2 := P1 - Int32(Matches[Count - 1].Dist) - 1;
      Result := MemCmpLen(P1, P2, Result, Limit);
    end;
  end;

  MatchesCount := Count;
  Inc(ReadAhead);
end;

procedure TLZMAEncoder.MfSkip(Amount: Int32);
var
  LenLimit, Cur, Left: Int32;
  Pos, H2, H3, HV, CurMatch: UInt32;
begin
  if (Amount = 0) then
    Exit;

  Left := Amount;
  repeat
    LenLimit := BufSize - Int32(ReadPos);
    if (NICE_LEN <= LenLimit) then
      LenLimit := NICE_LEN
    else if (LenLimit < 4) then
    begin
      Inc(ReadPos);
      Dec(Left);
      Continue;
    end;

    Cur := ReadPos;
    Pos := ReadPos + Offset;

    Hash4Calc(Cur, H2, H3, HV);
    CurMatch := Hash[FIX_4_HASH + HV];
    Hash[H2] := Pos;
    Hash[FIX_3_HASH + H3] := Pos;
    Hash[FIX_4_HASH + HV] := Pos;

    BtSkipFunc(LenLimit, Cur, Pos, CurMatch);
    MovePos();
    Dec(Left);
  until (Left = 0);

  Inc(ReadAhead, UInt32(Amount));
end;

{ symbol emission }

procedure TLZMAEncoder.BitTreeEncode(Base, NumBits, Symbol: Int32);
var
  M, I, Bit: Int32;
begin
  M := 1;
  for I := NumBits - 1 downto 0 do
  begin
    Bit := (Symbol shr I) and 1;
    RC.EncodeBit(Probs[Base + M], Bit);
    M := (M shl 1) or Bit;
  end;
end;

procedure TLZMAEncoder.BitTreeReverseEncode(Base, NumBits, Symbol: Int32);
var
  M, I, Bit: Int32;
begin
  M := 1;
  for I := 0 to NumBits - 1 do
  begin
    Bit := Symbol and 1;
    Symbol := Symbol shr 1;
    RC.EncodeBit(Probs[Base + M], Bit);
    M := (M shl 1) or Bit;
  end;
end;

procedure TLZMAEncoder.EncodeLen(CoderIdx, PosState, Len: Int32);
var
  Base: Int32;
begin
  if (CoderIdx = 0) then
    Base := pLenCoder
  else
    Base := pRepLenCoder;
  Dec(Len, kMatchMinLen);

  // liblzma applies probability updates only once a whole symbol is out, so
  // this refresh must see the probabilities from before the length below
  Dec(LenCounters[CoderIdx][PosState]);
  if (LenCounters[CoderIdx][PosState] = 0) then
    LengthUpdatePrices(CoderIdx, PosState);

  if (Len < kLenNumLowSymbols) then
  begin
    RC.EncodeBit(Probs[Base + LenChoice], 0);
    BitTreeEncode(Base + LenLow + (PosState shl kLenNumLowBits), kLenNumLowBits, Len);
  end else
  begin
    RC.EncodeBit(Probs[Base + LenChoice], 1);
    Dec(Len, kLenNumLowSymbols);
    if (Len < kLenNumMidSymbols) then
    begin
      RC.EncodeBit(Probs[Base + LenChoice2], 0);
      BitTreeEncode(Base + LenMid + (PosState shl kLenNumMidBits), kLenNumMidBits, Len);
    end else
    begin
      RC.EncodeBit(Probs[Base + LenChoice2], 1);
      BitTreeEncode(Base + LenHigh, kLenNumHighBits, Len - kLenNumMidSymbols);
    end;
  end;
end;

procedure TLZMAEncoder.EncodeDistance(Len: Int32; Dist: UInt32);
var
  PosSlot, LenToPosState, FooterBits: Int32;
  Base: UInt32;
begin
  LenToPosState := Len - kMatchMinLen;
  if (LenToPosState >= kNumLenToPosStates) then
    LenToPosState := kNumLenToPosStates - 1;

  PosSlot := GetPosSlot(Dist);
  BitTreeEncode(pPosSlot + (LenToPosState shl kNumPosSlotBits), kNumPosSlotBits, PosSlot);
  if (PosSlot < 4) then
    Exit;

  FooterBits := (PosSlot shr 1) - 1;
  Base := UInt32(2 or (PosSlot and 1)) shl FooterBits;

  if (PosSlot < kEndPosModelIndex) then
    BitTreeReverseEncode(pSpecPos + Int32(Base) - PosSlot, FooterBits, Dist - Base)
  else
  begin
    // the high bits are so close to uniform that modelling them is not worth it
    RC.EncodeDirect((Dist - Base) shr kNumAlignBits, FooterBits - kNumAlignBits);
    BitTreeReverseEncode(pAlign, kNumAlignBits, Dist and (kAlignTableSize - 1));
    Inc(AlignPriceCount);
  end;
end;

{ prices - what each of the choices above would have cost, without paying it }

function TLZMAEncoder.TreePrice(Base, NumBits, Symbol: Int32): UInt32;
var
  M, I, Bit: Int32;
begin
  Result := 0;
  M := 1;
  for I := NumBits - 1 downto 0 do
  begin
    Bit := (Symbol shr I) and 1;
    Inc(Result, BitPrice(Probs[Base + M], Bit));
    M := (M shl 1) or Bit;
  end;
end;

function TLZMAEncoder.TreeReversePrice(Base, NumBits, Symbol: Int32): UInt32;
var
  M, I, Bit: Int32;
begin
  Result := 0;
  M := 1;
  for I := 0 to NumBits - 1 do
  begin
    Bit := Symbol and 1;
    Symbol := Symbol shr 1;
    Inc(Result, BitPrice(Probs[Base + M], Bit));
    M := (M shl 1) or Bit;
  end;
end;

// mirrors EmitLiteral exactly, but adds up prices instead of writing bits
function TLZMAEncoder.LiteralPrice(Position, PrevByte: Int32; MatchMode: Boolean; MatchByte, Symbol: Int32): UInt32;
var
  Base, Offs: Int32;
begin
  Result := 0;
  Base := pLiteral + $300 * (((Position and ((1 shl ENC_LP) - 1)) shl ENC_LC) + (PrevByte shr (8 - ENC_LC)));
  Symbol := Symbol or $100;

  if not MatchMode then
  begin
    repeat
      Inc(Result, BitPrice(Probs[Base + (Symbol shr 8)], (Symbol shr 7) and 1));
      Symbol := Symbol shl 1;
    until (Symbol >= $10000);
  end else
  begin
    Offs := $100;
    repeat
      MatchByte := MatchByte shl 1;
      Inc(Result, BitPrice(Probs[Base + Offs + (MatchByte and Offs) + (Symbol shr 8)], (Symbol shr 7) and 1));
      Symbol := Symbol shl 1;
      Offs := Offs and (not (MatchByte xor Symbol));
    until (Symbol >= $10000);
  end;
end;

function TLZMAEncoder.ShortRepPrice(AState, PosState: Int32): UInt32;
begin
  Result := BitPrice(Probs[pIsRepG0 + AState], 0) +
            BitPrice(Probs[pIsRep0Long + (AState shl kNumPosBitsMax) + PosState], 0);
end;

// the bits that say which of the four remembered distances is meant
function TLZMAEncoder.PureRepPrice(Index, AState, PosState: Int32): UInt32;
begin
  if (Index = 0) then
  begin
    Result := BitPrice(Probs[pIsRepG0 + AState], 0) +
              BitPrice(Probs[pIsRep0Long + (AState shl kNumPosBitsMax) + PosState], 1);
    Exit;
  end;

  Result := BitPrice(Probs[pIsRepG0 + AState], 1);
  if (Index = 1) then
    Inc(Result, BitPrice(Probs[pIsRepG1 + AState], 0))
  else
    Inc(Result, BitPrice(Probs[pIsRepG1 + AState], 1) +
                BitPrice(Probs[pIsRepG2 + AState], Index - 2));
end;

function TLZMAEncoder.RepPrice(Index, Len, AState, PosState: Int32): UInt32;
begin
  Result := LenPrices[1][PosState][Len - kMatchMinLen] + PureRepPrice(Index, AState, PosState);
end;

function TLZMAEncoder.DistLenPrice(Dist: UInt32; Len, PosState: Int32): UInt32;
var
  DistState, DistSlot: Int32;
begin
  DistState := Len - kMatchMinLen;
  if (DistState >= kNumLenToPosStates) then
    DistState := kNumLenToPosStates - 1;

  if (Dist < kNumFullDistances) then
    Result := DistPrices[DistState][Dist]
  else
  begin
    DistSlot := GetPosSlot(Dist);
    Result := DistSlotPrices[DistState][DistSlot] + AlignPrices[Dist and (kAlignTableSize - 1)];
  end;

  Inc(Result, LenPrices[0][PosState][Len - kMatchMinLen]);
end;

procedure TLZMAEncoder.LengthUpdatePrices(CoderIdx, PosState: Int32);
var
  Base, I: Int32;
  A0, A1, B0, B1: UInt32;
begin
  if (CoderIdx = 0) then
    Base := pLenCoder
  else
    Base := pRepLenCoder;
  LenCounters[CoderIdx][PosState] := LEN_TABLE_SIZE;

  A0 := BitPrice(Probs[Base + LenChoice], 0);
  A1 := BitPrice(Probs[Base + LenChoice], 1);
  B0 := A1 + BitPrice(Probs[Base + LenChoice2], 0);
  B1 := A1 + BitPrice(Probs[Base + LenChoice2], 1);

  for I := 0 to LEN_TABLE_SIZE - 1 do
    if (I < kLenNumLowSymbols) then
      LenPrices[CoderIdx][PosState][I] :=
        A0 + TreePrice(Base + LenLow + (PosState shl kLenNumLowBits), kLenNumLowBits, I)
    else if (I < kLenNumLowSymbols + kLenNumMidSymbols) then
      LenPrices[CoderIdx][PosState][I] :=
        B0 + TreePrice(Base + LenMid + (PosState shl kLenNumMidBits), kLenNumMidBits, I - kLenNumLowSymbols)
    else
      LenPrices[CoderIdx][PosState][I] :=
        B1 + TreePrice(Base + LenHigh, kLenNumHighBits, I - kLenNumLowSymbols - kLenNumMidSymbols);
end;

procedure TLZMAEncoder.FillDistPrices;
var
  DistState, DistSlot, I, FooterBits: Int32;
  Base, Price: UInt32;
begin
  for DistState := 0 to kNumLenToPosStates - 1 do
  begin
    for DistSlot := 0 to DIST_TABLE_SIZE - 1 do
      DistSlotPrices[DistState][DistSlot] :=
        TreePrice(pPosSlot + (DistState shl kNumPosSlotBits), kNumPosSlotBits, DistSlot);

    // above this slot the low bits are coded straight, one bit each, so fold
    // that flat cost into the slot itself
    for DistSlot := kEndPosModelIndex to DIST_TABLE_SIZE - 1 do
      Inc(DistSlotPrices[DistState][DistSlot],
          UInt32(((DistSlot shr 1) - 1) - kNumAlignBits) shl kNumBitPriceShiftBits);

    for I := 0 to 3 do
      DistPrices[DistState][I] := DistSlotPrices[DistState][I];
  end;

  for I := 4 to kNumFullDistances - 1 do
  begin
    DistSlot := GetPosSlot(I);
    FooterBits := (DistSlot shr 1) - 1;
    Base := UInt32(2 or (DistSlot and 1)) shl FooterBits;
    Price := TreeReversePrice(pSpecPos + Int32(Base) - DistSlot, FooterBits, I - Int32(Base));
    for DistState := 0 to kNumLenToPosStates - 1 do
      DistPrices[DistState][I] := Price + DistSlotPrices[DistState][DistSlot];
  end;

  MatchPriceCount := 0;
end;

procedure TLZMAEncoder.FillAlignPrices;
var
  I: Int32;
begin
  for I := 0 to kAlignTableSize - 1 do
    AlignPrices[I] := TreeReversePrice(pAlign, kNumAlignBits, I);
  AlignPriceCount := 0;
end;

procedure TLZMAEncoder.EmitLiteral(Position, PosState: Int32);
var
  Base, PrevByte, Symbol, MatchByte, Offs: Int32;
begin
  RC.EncodeBit(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState], 0);

  if (Position = 0) then
    PrevByte := 0
  else
    PrevByte := Buf[Position - 1];
  Base := pLiteral + $300 * (((Position and ((1 shl ENC_LP) - 1)) shl ENC_LC) + (PrevByte shr (8 - ENC_LC)));

  Symbol := Buf[Position] or $100;
  if (State < 7) then
  begin
    repeat
      RC.EncodeBit(Probs[Base + (Symbol shr 8)], (Symbol shr 7) and 1);
      Symbol := Symbol shl 1;
    until (Symbol >= $10000);
  end else
  begin
    // after a match, code against the byte it would have run into
    MatchByte := Buf[Position - Int32(Reps[0]) - 1];
    Offs := $100;
    repeat
      MatchByte := MatchByte shl 1;
      RC.EncodeBit(Probs[Base + Offs + (MatchByte and Offs) + (Symbol shr 8)], (Symbol shr 7) and 1);
      Symbol := Symbol shl 1;
      Offs := Offs and (not (MatchByte xor Symbol));
    until (Symbol >= $10000);
  end;

  UpdateLiteral(State);
end;

procedure TLZMAEncoder.EmitMatch(PosState, Len: Int32; Dist: UInt32);
begin
  RC.EncodeBit(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState], 1);
  RC.EncodeBit(Probs[pIsRep + State], 0);

  UpdateMatch(State);

  EncodeLen(0, PosState, Len);
  EncodeDistance(Len, Dist);

  Reps[3] := Reps[2];
  Reps[2] := Reps[1];
  Reps[1] := Reps[0];
  Reps[0] := Dist;
  Inc(MatchPriceCount);
end;

procedure TLZMAEncoder.EmitRep(PosState, Index, Len: Int32);
var
  Dist: UInt32;
begin
  RC.EncodeBit(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState], 1);
  RC.EncodeBit(Probs[pIsRep + State], 1);

  if (Index = 0) then
  begin
    RC.EncodeBit(Probs[pIsRepG0 + State], 0);
    RC.EncodeBit(Probs[pIsRep0Long + (State shl kNumPosBitsMax) + PosState], 1);
  end else
  begin
    Dist := Reps[Index];
    RC.EncodeBit(Probs[pIsRepG0 + State], 1);
    if (Index = 1) then
      RC.EncodeBit(Probs[pIsRepG1 + State], 0)
    else
    begin
      RC.EncodeBit(Probs[pIsRepG1 + State], 1);
      RC.EncodeBit(Probs[pIsRepG2 + State], Index - 2);
      if (Index = 3) then
        Reps[3] := Reps[2];
      Reps[2] := Reps[1];
    end;
    Reps[1] := Reps[0];
    Reps[0] := Dist;
  end;

  EncodeLen(1, PosState, Len);
  UpdateLongRep(State);
end;

procedure TLZMAEncoder.EmitShortRep(PosState: Int32);
begin
  RC.EncodeBit(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState], 1);
  RC.EncodeBit(Probs[pIsRep + State], 1);
  RC.EncodeBit(Probs[pIsRepG0 + State], 0);
  RC.EncodeBit(Probs[pIsRep0Long + (State shl kNumPosBitsMax) + PosState], 0);

  UpdateShortRep(State);
end;

procedure TLZMAEncoder.EncodeSymbol(Back: UInt32; Len, Position: Int32);
var
  PosState: Int32;
begin
  PosState := Position and ENC_PB_MASK;

  if (Back = MARK_LIT) then
    EmitLiteral(Position, PosState)
  else if (Back < 4) then
  begin
    if (Len = 1) then
      EmitShortRep(PosState)
    else
      EmitRep(PosState, Int32(Back), Len);
  end else
    EmitMatch(PosState, Len, Back - 4);

  Dec(ReadAhead, UInt32(Len));
end;

{ the parser }

procedure TLZMAEncoder.MakeLiteral(Idx: Int32);
begin
  Opts[Idx].BackPrev := MARK_LIT;
  Opts[Idx].Prev1IsLiteral := False;
end;

procedure TLZMAEncoder.MakeShortRep(Idx: Int32);
begin
  Opts[Idx].BackPrev := 0;
  Opts[Idx].Prev1IsLiteral := False;
end;

// prices everything reachable in one step from the current position, and seeds
// the window. Returns len_end, or -1 when it already knows the answer.
function TLZMAEncoder.Helper1(Position: Int32; out BackRes: UInt32; out LenRes: Int32): Int32;
var
  LenMain, BufAvail, RepMaxIndex, I, LenEnd, Len, PosState: Int32;
  BufBack, CurByte, MatchByte, RepLen: Int32;
  RepLens: array[0..3] of Int32;
  MatchPrice, RepMatchPrice, NormalMatchPrice, Price, CurAndLenPrice: UInt32;
  Dist: UInt32;
begin
  if (ReadAhead = 0) then
    LenMain := MfFind()
  else
    LenMain := LongestMatchLength;

  BufAvail := BufSize - Position;
  if (BufAvail > kMatchMaxLen) then
    BufAvail := kMatchMaxLen;
  if (BufAvail < 2) then
  begin
    BackRes := MARK_LIT;
    LenRes := 1;
    Exit(-1);
  end;

  RepMaxIndex := 0;
  for I := 0 to 3 do
  begin
    BufBack := Position - Int32(Reps[I]) - 1;
    if (Buf[Position] <> Buf[BufBack]) or (Buf[Position + 1] <> Buf[BufBack + 1]) then
    begin
      RepLens[I] := 0;
      Continue;
    end;
    RepLens[I] := MemCmpLen(Position, BufBack, 2, BufAvail);
    if (RepLens[I] > RepLens[RepMaxIndex]) then
      RepMaxIndex := I;
  end;

  // a rep this long is taken before anything else is even looked at
  if (RepLens[RepMaxIndex] >= NICE_LEN) then
  begin
    BackRes := RepMaxIndex;
    LenRes := RepLens[RepMaxIndex];
    MfSkip(LenRes - 1);
    Exit(-1);
  end;

  if (LenMain >= NICE_LEN) then
  begin
    BackRes := Matches[MatchesCount - 1].Dist + 4;
    LenRes := LenMain;
    MfSkip(LenMain - 1);
    Exit(-1);
  end;

  CurByte := Buf[Position];
  MatchByte := Buf[Position - Int32(Reps[0]) - 1];

  if (LenMain < 2) and (CurByte <> MatchByte) and (RepLens[RepMaxIndex] < 2) then
  begin
    BackRes := MARK_LIT;
    LenRes := 1;
    Exit(-1);
  end;

  Opts[0].State := State;
  PosState := Position and ENC_PB_MASK;

  Opts[1].Price := BitPrice(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState], 0) +
                   LiteralPrice(Position, Buf[Position - 1], not IsLiteralState(State), MatchByte, CurByte);
  MakeLiteral(1);

  MatchPrice := BitPrice(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState], 1);
  RepMatchPrice := MatchPrice + BitPrice(Probs[pIsRep + State], 1);

  if (MatchByte = CurByte) then
  begin
    Price := RepMatchPrice + ShortRepPrice(State, PosState);
    if (Price < Opts[1].Price) then
    begin
      Opts[1].Price := Price;
      MakeShortRep(1);
    end;
  end;

  LenEnd := LenMain;
  if (RepLens[RepMaxIndex] > LenEnd) then
    LenEnd := RepLens[RepMaxIndex];

  if (LenEnd < 2) then
  begin
    BackRes := Opts[1].BackPrev;
    LenRes := 1;
    Exit(-1);
  end;

  Opts[1].PosPrev := 0;
  for I := 0 to 3 do
    Opts[0].Backs[I] := Reps[I];

  Len := LenEnd;
  repeat
    Opts[Len].Price := INF_PRICE;
    Dec(Len);
  until (Len < 2);

  for I := 0 to 3 do
  begin
    RepLen := RepLens[I];
    if (RepLen < 2) then
      Continue;
    Price := RepMatchPrice + PureRepPrice(I, State, PosState);
    repeat
      CurAndLenPrice := Price + LenPrices[1][PosState][RepLen - kMatchMinLen];
      if (CurAndLenPrice < Opts[RepLen].Price) then
      begin
        Opts[RepLen].Price := CurAndLenPrice;
        Opts[RepLen].PosPrev := 0;
        Opts[RepLen].BackPrev := I;
        Opts[RepLen].Prev1IsLiteral := False;
      end;
      Dec(RepLen);
    until (RepLen < 2);
  end;

  NormalMatchPrice := MatchPrice + BitPrice(Probs[pIsRep + State], 0);

  if (RepLens[0] >= 2) then
    Len := RepLens[0] + 1
  else
    Len := 2;

  if (Len <= LenMain) then
  begin
    I := 0;
    while (Len > Int32(Matches[I].Len)) do
      Inc(I);
    while True do
    begin
      Dist := Matches[I].Dist;
      CurAndLenPrice := NormalMatchPrice + DistLenPrice(Dist, Len, PosState);
      if (CurAndLenPrice < Opts[Len].Price) then
      begin
        Opts[Len].Price := CurAndLenPrice;
        Opts[Len].PosPrev := 0;
        Opts[Len].BackPrev := Dist + 4;
        Opts[Len].Prev1IsLiteral := False;
      end;
      if (Len = Int32(Matches[I].Len)) then
      begin
        Inc(I);
        if (I = MatchesCount) then
          Break;
      end;
      Inc(Len);
    end;
  end;

  Result := LenEnd;
end;

// relaxes everything reachable from position Cur, including the three moves
// that fold a literal into the middle of a symbol pair
function TLZMAEncoder.Helper2(var R: TReps; LenEnd, Position, Cur, BufAvailFull: Int32): Int32;
var
  MatchesCnt, NewLen, PosPrev, St, St2, PosState, PosStateNext: Int32;
  StartLen, LenTest, LenTestTemp, LenTest2, Limit, Offs, I, RepIndex: Int32;
  BufAvail, BufBack, CurByte, MatchByte: Int32;
  Back, CurBack: UInt32;
  CurPrice, CurAnd1Price, MatchPrice, RepMatchPrice, NormalMatchPrice: UInt32;
  Price, CurAndLenPrice, CurAndLenLiteralPrice, NextRepMatchPrice: UInt32;
  NextIsLiteral: Boolean;
begin
  MatchesCnt := MatchesCount;
  NewLen := LongestMatchLength;
  PosPrev := Opts[Cur].PosPrev;

  if Opts[Cur].Prev1IsLiteral then
  begin
    Dec(PosPrev);
    if Opts[Cur].Prev2 then
    begin
      St := Opts[Opts[Cur].PosPrev2].State;
      if (Opts[Cur].BackPrev2 < 4) then
        UpdateLongRep(St)
      else
        UpdateMatch(St);
    end else
      St := Opts[PosPrev].State;
    UpdateLiteral(St);
  end else
    St := Opts[PosPrev].State;

  if (PosPrev = Cur - 1) then
  begin
    // one step back, so the remembered distances are the ones already in R
    if (Opts[Cur].BackPrev = 0) then
      UpdateShortRep(St)
    else
      UpdateLiteral(St);
  end else
  begin
    if Opts[Cur].Prev1IsLiteral and Opts[Cur].Prev2 then
    begin
      PosPrev := Opts[Cur].PosPrev2;
      Back := Opts[Cur].BackPrev2;
      UpdateLongRep(St);
    end else
    begin
      Back := Opts[Cur].BackPrev;
      if (Back < 4) then
        UpdateLongRep(St)
      else
        UpdateMatch(St);
    end;

    if (Back < 4) then
    begin
      R[0] := Opts[PosPrev].Backs[Back];
      I := 1;
      while (I <= Int32(Back)) do
      begin
        R[I] := Opts[PosPrev].Backs[I - 1];
        Inc(I);
      end;
      while (I < 4) do
      begin
        R[I] := Opts[PosPrev].Backs[I];
        Inc(I);
      end;
    end else
    begin
      R[0] := Back - 4;
      for I := 1 to 3 do
        R[I] := Opts[PosPrev].Backs[I - 1];
    end;
  end;

  Opts[Cur].State := St;
  for I := 0 to 3 do
    Opts[Cur].Backs[I] := R[I];

  CurPrice := Opts[Cur].Price;
  CurByte := Buf[Position];
  MatchByte := Buf[Position - Int32(R[0]) - 1];
  PosState := Position and ENC_PB_MASK;

  CurAnd1Price := CurPrice + BitPrice(Probs[pIsMatch + (St shl kNumPosBitsMax) + PosState], 0) +
                  LiteralPrice(Position, Buf[Position - 1], not IsLiteralState(St), MatchByte, CurByte);

  NextIsLiteral := False;
  if (CurAnd1Price < Opts[Cur + 1].Price) then
  begin
    Opts[Cur + 1].Price := CurAnd1Price;
    Opts[Cur + 1].PosPrev := Cur;
    MakeLiteral(Cur + 1);
    NextIsLiteral := True;
  end;

  MatchPrice := CurPrice + BitPrice(Probs[pIsMatch + (St shl kNumPosBitsMax) + PosState], 1);
  RepMatchPrice := MatchPrice + BitPrice(Probs[pIsRep + St], 1);

  if (MatchByte = CurByte) and
     not ((Opts[Cur + 1].PosPrev < Cur) and (Opts[Cur + 1].BackPrev = 0)) then
  begin
    Price := RepMatchPrice + ShortRepPrice(St, PosState);
    // the one place a tie is taken, so a free byte beats paying for a literal
    if (Price <= Opts[Cur + 1].Price) then
    begin
      Opts[Cur + 1].Price := Price;
      Opts[Cur + 1].PosPrev := Cur;
      MakeShortRep(Cur + 1);
      NextIsLiteral := True;
    end;
  end;

  if (BufAvailFull < 2) then
    Exit(LenEnd);

  BufAvail := BufAvailFull;
  if (BufAvail > NICE_LEN) then
    BufAvail := NICE_LEN;

  if (not NextIsLiteral) and (MatchByte <> CurByte) then
  begin
    // literal + rep0
    BufBack := Position - Int32(R[0]) - 1;
    Limit := BufAvailFull;
    if (Limit > NICE_LEN + 1) then
      Limit := NICE_LEN + 1;
    LenTest := MemCmpLen(Position, BufBack, 1, Limit) - 1;

    if (LenTest >= 2) then
    begin
      St2 := St;
      UpdateLiteral(St2);
      PosStateNext := (Position + 1) and ENC_PB_MASK;
      NextRepMatchPrice := CurAnd1Price +
                           BitPrice(Probs[pIsMatch + (St2 shl kNumPosBitsMax) + PosStateNext], 1) +
                           BitPrice(Probs[pIsRep + St2], 1);

      Offs := Cur + 1 + LenTest;
      while (LenEnd < Offs) do
      begin
        Inc(LenEnd);
        Opts[LenEnd].Price := INF_PRICE;
      end;

      CurAndLenPrice := NextRepMatchPrice + RepPrice(0, LenTest, St2, PosStateNext);
      if (CurAndLenPrice < Opts[Offs].Price) then
      begin
        Opts[Offs].Price := CurAndLenPrice;
        Opts[Offs].PosPrev := Cur + 1;
        Opts[Offs].BackPrev := 0;
        Opts[Offs].Prev1IsLiteral := True;
        Opts[Offs].Prev2 := False;
      end;
    end;
  end;

  StartLen := 2;

  for RepIndex := 0 to 3 do
  begin
    BufBack := Position - Int32(R[RepIndex]) - 1;
    if (Buf[Position] <> Buf[BufBack]) or (Buf[Position + 1] <> Buf[BufBack + 1]) then
      Continue;

    LenTest := MemCmpLen(Position, BufBack, 2, BufAvail);
    while (LenEnd < Cur + LenTest) do
    begin
      Inc(LenEnd);
      Opts[LenEnd].Price := INF_PRICE;
    end;

    LenTestTemp := LenTest;
    Price := RepMatchPrice + PureRepPrice(RepIndex, St, PosState);
    repeat
      CurAndLenPrice := Price + LenPrices[1][PosState][LenTest - kMatchMinLen];
      if (CurAndLenPrice < Opts[Cur + LenTest].Price) then
      begin
        Opts[Cur + LenTest].Price := CurAndLenPrice;
        Opts[Cur + LenTest].PosPrev := Cur;
        Opts[Cur + LenTest].BackPrev := RepIndex;
        Opts[Cur + LenTest].Prev1IsLiteral := False;
      end;
      Dec(LenTest);
    until (LenTest < 2);

    LenTest := LenTestTemp;
    if (RepIndex = 0) then
      StartLen := LenTest + 1;

    // rep + literal + rep0, at the one length that reaches furthest
    LenTest2 := LenTest + 1;
    Limit := BufAvailFull;
    if (Limit > LenTest2 + NICE_LEN) then
      Limit := LenTest2 + NICE_LEN;
    if (LenTest2 < Limit) then
      LenTest2 := MemCmpLen(Position, BufBack, LenTest2, Limit);
    Dec(LenTest2, LenTest + 1);

    if (LenTest2 >= 2) then
    begin
      St2 := St;
      UpdateLongRep(St2);
      PosStateNext := (Position + LenTest) and ENC_PB_MASK;

      CurAndLenLiteralPrice := Price + LenPrices[1][PosState][LenTest - kMatchMinLen] +
        BitPrice(Probs[pIsMatch + (St2 shl kNumPosBitsMax) + PosStateNext], 0) +
        LiteralPrice(Position + LenTest, Buf[Position + LenTest - 1], True,
                     Buf[BufBack + LenTest], Buf[Position + LenTest]);

      UpdateLiteral(St2);
      PosStateNext := (Position + LenTest + 1) and ENC_PB_MASK;

      NextRepMatchPrice := CurAndLenLiteralPrice +
                           BitPrice(Probs[pIsMatch + (St2 shl kNumPosBitsMax) + PosStateNext], 1) +
                           BitPrice(Probs[pIsRep + St2], 1);

      Offs := Cur + LenTest + 1 + LenTest2;
      while (LenEnd < Offs) do
      begin
        Inc(LenEnd);
        Opts[LenEnd].Price := INF_PRICE;
      end;

      CurAndLenPrice := NextRepMatchPrice + RepPrice(0, LenTest2, St2, PosStateNext);
      if (CurAndLenPrice < Opts[Offs].Price) then
      begin
        Opts[Offs].Price := CurAndLenPrice;
        Opts[Offs].PosPrev := Cur + LenTest + 1;
        Opts[Offs].BackPrev := 0;
        Opts[Offs].Prev1IsLiteral := True;
        Opts[Offs].Prev2 := True;
        Opts[Offs].PosPrev2 := Cur;
        Opts[Offs].BackPrev2 := RepIndex;
      end;
    end;
  end;

  // nothing longer than what is left, and nothing above nice_len, gets priced
  if (NewLen > BufAvail) then
  begin
    NewLen := BufAvail;
    MatchesCnt := 0;
    while (NewLen > Int32(Matches[MatchesCnt].Len)) do
      Inc(MatchesCnt);
    Matches[MatchesCnt].Len := NewLen;
    Inc(MatchesCnt);
  end;

  if (NewLen >= StartLen) then
  begin
    NormalMatchPrice := MatchPrice + BitPrice(Probs[pIsRep + St], 0);

    while (LenEnd < Cur + NewLen) do
    begin
      Inc(LenEnd);
      Opts[LenEnd].Price := INF_PRICE;
    end;

    I := 0;
    while (StartLen > Int32(Matches[I].Len)) do
      Inc(I);

    LenTest := StartLen;
    while True do
    begin
      CurBack := Matches[I].Dist;
      CurAndLenPrice := NormalMatchPrice + DistLenPrice(CurBack, LenTest, PosState);
      if (CurAndLenPrice < Opts[Cur + LenTest].Price) then
      begin
        Opts[Cur + LenTest].Price := CurAndLenPrice;
        Opts[Cur + LenTest].PosPrev := Cur;
        Opts[Cur + LenTest].BackPrev := CurBack + 4;
        Opts[Cur + LenTest].Prev1IsLiteral := False;
      end;

      if (LenTest = Int32(Matches[I].Len)) then
      begin
        // match + literal + rep0
        BufBack := Position - Int32(CurBack) - 1;
        LenTest2 := LenTest + 1;
        Limit := BufAvailFull;
        if (Limit > LenTest2 + NICE_LEN) then
          Limit := LenTest2 + NICE_LEN;
        if (LenTest2 < Limit) then
          LenTest2 := MemCmpLen(Position, BufBack, LenTest2, Limit);
        Dec(LenTest2, LenTest + 1);

        if (LenTest2 >= 2) then
        begin
          St2 := St;
          UpdateMatch(St2);
          PosStateNext := (Position + LenTest) and ENC_PB_MASK;

          CurAndLenLiteralPrice := CurAndLenPrice +
            BitPrice(Probs[pIsMatch + (St2 shl kNumPosBitsMax) + PosStateNext], 0) +
            LiteralPrice(Position + LenTest, Buf[Position + LenTest - 1], True,
                         Buf[BufBack + LenTest], Buf[Position + LenTest]);

          UpdateLiteral(St2);
          PosStateNext := (PosStateNext + 1) and ENC_PB_MASK;

          NextRepMatchPrice := CurAndLenLiteralPrice +
                               BitPrice(Probs[pIsMatch + (St2 shl kNumPosBitsMax) + PosStateNext], 1) +
                               BitPrice(Probs[pIsRep + St2], 1);

          Offs := Cur + LenTest + 1 + LenTest2;
          while (LenEnd < Offs) do
          begin
            Inc(LenEnd);
            Opts[LenEnd].Price := INF_PRICE;
          end;

          CurAndLenPrice := NextRepMatchPrice + RepPrice(0, LenTest2, St2, PosStateNext);
          if (CurAndLenPrice < Opts[Offs].Price) then
          begin
            Opts[Offs].Price := CurAndLenPrice;
            Opts[Offs].PosPrev := Cur + LenTest + 1;
            Opts[Offs].BackPrev := 0;
            Opts[Offs].Prev1IsLiteral := True;
            Opts[Offs].Prev2 := True;
            Opts[Offs].PosPrev2 := Cur;
            Opts[Offs].BackPrev2 := CurBack + 4;
          end;
        end;

        Inc(I);
        if (I = MatchesCnt) then
          Break;
      end;

      Inc(LenTest);
    end;
  end;

  Result := LenEnd;
end;

// reverses the chain of links in place, expanding the folded literals into
// explicit steps. The caller then drains one symbol per call.
procedure TLZMAEncoder.Backward(Cur: Int32; out BackRes: UInt32; out LenRes: Int32);
var
  PosMem, PosPrev: Int32;
  BackMem, BackCur: UInt32;
begin
  OptsEndIndex := Cur;
  PosMem := Opts[Cur].PosPrev;
  BackMem := Opts[Cur].BackPrev;

  repeat
    if Opts[Cur].Prev1IsLiteral then
    begin
      MakeLiteral(PosMem);
      Opts[PosMem].PosPrev := PosMem - 1;

      if Opts[Cur].Prev2 then
      begin
        Opts[PosMem - 1].Prev1IsLiteral := False;
        Opts[PosMem - 1].PosPrev := Opts[Cur].PosPrev2;
        Opts[PosMem - 1].BackPrev := Opts[Cur].BackPrev2;
      end;
    end;

    PosPrev := PosMem;
    BackCur := BackMem;

    BackMem := Opts[PosPrev].BackPrev;
    PosMem := Opts[PosPrev].PosPrev;

    Opts[PosPrev].BackPrev := BackCur;
    Opts[PosPrev].PosPrev := Cur;
    Cur := PosPrev;
  until (Cur = 0);

  OptsCurrentIndex := Opts[0].PosPrev;
  LenRes := Opts[0].PosPrev;
  BackRes := Opts[0].BackPrev;
end;

procedure TLZMAEncoder.OptimumNormal(Position: Int32; out BackRes: UInt32; out LenRes: Int32);
var
  LenEnd, Cur, BufAvailFull: Int32;
  R: TReps;
begin
  // symbols still pending from the last search
  if (OptsEndIndex <> OptsCurrentIndex) then
  begin
    LenRes := Opts[OptsCurrentIndex].PosPrev - OptsCurrentIndex;
    BackRes := Opts[OptsCurrentIndex].BackPrev;
    OptsCurrentIndex := Opts[OptsCurrentIndex].PosPrev;
    Exit;
  end;

  if (ReadAhead = 0) then
  begin
    if (MatchPriceCount >= MATCH_PRICE_REFRESH) then
      FillDistPrices();
    if (AlignPriceCount >= kAlignTableSize) then
      FillAlignPrices();
  end;

  LenEnd := Helper1(Position, BackRes, LenRes);
  if (LenEnd = -1) then
    Exit;

  R := Reps;

  Cur := 1;
  while (Cur < LenEnd) do
  begin
    LongestMatchLength := MfFind();
    if (LongestMatchLength >= NICE_LEN) then
      Break;

    BufAvailFull := BufSize - Int32(ReadPos) + 1;
    if (BufAvailFull > NUM_OPTS - 1 - Cur) then
      BufAvailFull := NUM_OPTS - 1 - Cur;

    LenEnd := Helper2(R, LenEnd, Position + Cur, Cur, BufAvailFull);
    Inc(Cur);
  end;

  Backward(Cur, BackRes, LenRes);
end;

procedure TLZMAEncoder.Run;
var
  Len: Int32;
  Back: UInt32;
begin
  // the first symbol is always a plain literal
  if (BufSize > 0) then
  begin
    MfSkip(1);
    ReadAhead := 0;
    RC.EncodeBit(Probs[pIsMatch], 0);
    BitTreeEncode(pLiteral, 8, Buf[0]);
    UncompSize := 1;
  end;

  while True do
  begin
    if (Int32(ReadPos) >= BufSize) and (ReadAhead = 0) then
      Break;

    OptimumNormal(UncompSize, Back, Len);
    EncodeSymbol(Back, Len, UncompSize);
    Inc(UncompSize, Len);
  end;

  // the size in the header is always unknown, so the stream always ends with a
  // marker: a match at a distance nothing can reach
  EmitMatch(UncompSize and ENC_PB_MASK, kMatchMinLen, $FFFFFFFF);
  RC.Flush();
end;

class procedure LZMA.Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  E: PLZMAEncoder;
  NumProbs, I, SonCount: Int32;
begin
  New(E);
  FillChar(E^, SizeOf(TLZMAEncoder), 0);
  try
    E^.BufSize := InSize;
    E^.Buf := GetMem(E^.BufSize + BUF_SLACK);
    if (E^.BufSize > 0) then
      Move(InData^, E^.Buf^, E^.BufSize);
    FillChar(E^.Buf[E^.BufSize], BUF_SLACK, 0);

    E^.Offset := CYCLIC_SIZE;
    E^.Hash := GetMem(HASH_COUNT * SizeOf(UInt32));
    FillDWord(E^.Hash^, HASH_COUNT, 0);

    // the tree only ever reaches as far as the input does
    SonCount := E^.BufSize + 1;
    if (SonCount > CYCLIC_SIZE) then
      SonCount := CYCLIC_SIZE;
    E^.Son := GetMem(SonCount * 2 * SizeOf(UInt32));

    NumProbs := pLiteral + LiteralProbCount(ENC_LC, ENC_LP);
    E^.Probs := GetMem(NumProbs * SizeOf(UInt16));
    for I := 0 to NumProbs - 1 do
      E^.Probs[I] := kBitModelTotal div 2;

    for I := 0 to (1 shl ENC_PB) - 1 do
    begin
      E^.LengthUpdatePrices(0, I);
      E^.LengthUpdatePrices(1, I);
    end;
    // big enough that both tables are built before they are first read
    E^.MatchPriceCount := High(UInt32) div 2;
    E^.AlignPriceCount := High(UInt32) div 2;

    if (OutData = nil) or (MemSize(OutData) < HEADER_SIZE) then
      ReAllocMem(OutData, GROW_MIN);

    // the alone-format header, then the coded stream. liblzma always writes the
    // preset's dictionary size and an unknown uncompressed size.
    OutData[0] := (ENC_PB * 5 + ENC_LP) * 9 + ENC_LC;
    PUInt32(OutData + 1)^ := NtoLE(UInt32(DICT_SIZE));
    PUInt64(OutData + 5)^ := NtoLE(UInt64($FFFFFFFFFFFFFFFF));

    E^.RC.Init(OutData);
    E^.RC.Pos := HEADER_SIZE;
    try
      E^.Run();
    finally
      OutData := E^.RC.Data;
      OutSize := E^.RC.Pos;
    end;
  finally
    if (E^.Buf <> nil) then FreeMem(E^.Buf);
    if (E^.Hash <> nil) then FreeMem(E^.Hash);
    if (E^.Son <> nil) then FreeMem(E^.Son);
    if (E^.Probs <> nil) then FreeMem(E^.Probs);
    Dispose(E);
  end;
end;

{ decompression }

type
  TRangeDecoder = record
    Data: PByte;
    Pos, Size: PtrUInt;
    Range, Code: UInt32;

    function NextByte: Byte;
    procedure Init;
    procedure Normalize; inline;
    function DecodeBit(var Prob: UInt16): Int32;
    function DecodeDirect(NumBits: Int32): UInt32;
  end;

  PLZMADecoder = ^TLZMADecoder;
  TLZMADecoder = record
    RC: TRangeDecoder;
    Probs: PUInt16;

    Dst: PByte;
    DstPos, DstCap: PtrUInt;

    Lc, Lp, Pb: Int32;
    State: Int32;
    Reps: array[0..3] of UInt32;

    procedure Emit(B: Byte); inline;
    function BitTreeDecode(Base, NumBits: Int32): Int32;
    function BitTreeReverseDecode(Base, NumBits: Int32): Int32;
    function DecodeLen(Base, PosState: Int32): Int32;
    function DecodeDistance(Len: Int32): UInt32;
    procedure Run(Limit: Int64; HaveSize: Boolean);
  end;

function TRangeDecoder.NextByte: Byte;
begin
  if (Pos >= Size) then
    CompressCodecException('lzma stream is truncated');
  Result := Data[Pos];
  Inc(Pos);
end;

procedure TRangeDecoder.Init;
var
  I: Int32;
begin
  if (NextByte() <> 0) then
    CompressCodecException('corrupt lzma stream');
  Code := 0;
  Range := $FFFFFFFF;
  for I := 1 to 4 do
    Code := (Code shl 8) or NextByte();
end;

procedure TRangeDecoder.Normalize;
begin
  if (Range < kTopValue) then
  begin
    Range := Range shl 8;
    Code := (Code shl 8) or NextByte();
  end;
end;

function TRangeDecoder.DecodeBit(var Prob: UInt16): Int32;
var
  Bound: UInt32;
begin
  Bound := (Range shr kNumBitModelTotalBits) * Prob;
  if (Code < Bound) then
  begin
    Inc(Prob, (kBitModelTotal - Prob) shr kNumMoveBits);
    Range := Bound;
    Result := 0;
  end else
  begin
    Dec(Prob, Prob shr kNumMoveBits);
    Dec(Code, Bound);
    Dec(Range, Bound);
    Result := 1;
  end;
  Normalize();
end;

function TRangeDecoder.DecodeDirect(NumBits: Int32): UInt32;
var
  T: UInt32;
begin
  Result := 0;
  while (NumBits > 0) do
  begin
    Range := Range shr 1;
    Dec(Code, Range);
    T := 0 - (Code shr 31);
    Inc(Code, Range and T);
    Normalize();
    Result := (Result shl 1) + T + 1;
    Dec(NumBits);
  end;
end;

procedure TLZMADecoder.Emit(B: Byte);
begin
  if (DstPos >= DstCap) then
  begin
    if (DstCap < GROW_MIN) then
      DstCap := GROW_MIN
    else
      DstCap := DstCap * 2;
    ReAllocMem(Dst, DstCap);
  end;
  Dst[DstPos] := B;
  Inc(DstPos);
end;

function TLZMADecoder.BitTreeDecode(Base, NumBits: Int32): Int32;
var
  M, I: Int32;
begin
  M := 1;
  for I := 1 to NumBits do
    M := (M shl 1) or RC.DecodeBit(Probs[Base + M]);
  Result := M - (1 shl NumBits);
end;

function TLZMADecoder.BitTreeReverseDecode(Base, NumBits: Int32): Int32;
var
  M, I, Bit: Int32;
begin
  M := 1;
  Result := 0;
  for I := 0 to NumBits - 1 do
  begin
    Bit := RC.DecodeBit(Probs[Base + M]);
    M := (M shl 1) or Bit;
    Result := Result or (Bit shl I);
  end;
end;

function TLZMADecoder.DecodeLen(Base, PosState: Int32): Int32;
begin
  if (RC.DecodeBit(Probs[Base + LenChoice]) = 0) then
    Exit(BitTreeDecode(Base + LenLow + (PosState shl kLenNumLowBits), kLenNumLowBits));

  if (RC.DecodeBit(Probs[Base + LenChoice2]) = 0) then
    Exit(kLenNumLowSymbols + BitTreeDecode(Base + LenMid + (PosState shl kLenNumMidBits), kLenNumMidBits));

  Result := kLenNumLowSymbols + kLenNumMidSymbols + BitTreeDecode(Base + LenHigh, kLenNumHighBits);
end;

function TLZMADecoder.DecodeDistance(Len: Int32): UInt32;
var
  LenToPosState, PosSlot, DirectBits: Int32;
begin
  LenToPosState := Len - kMatchMinLen;
  if (LenToPosState >= kNumLenToPosStates) then
    LenToPosState := kNumLenToPosStates - 1;

  PosSlot := BitTreeDecode(pPosSlot + (LenToPosState shl kNumPosSlotBits), kNumPosSlotBits);
  if (PosSlot < 4) then
    Exit(PosSlot);

  DirectBits := (PosSlot shr 1) - 1;
  Result := UInt32(2 or (PosSlot and 1)) shl DirectBits;

  if (PosSlot < kEndPosModelIndex) then
    Inc(Result, BitTreeReverseDecode(pSpecPos + Int32(Result) - PosSlot, DirectBits))
  else
  begin
    Inc(Result, RC.DecodeDirect(DirectBits - kNumAlignBits) shl kNumAlignBits);
    Inc(Result, BitTreeReverseDecode(pAlign, kNumAlignBits));
  end;
end;

procedure TLZMADecoder.Run(Limit: Int64; HaveSize: Boolean);
var
  PosState, PbMask, LpMask, Base, PrevByte, Symbol, MatchByte, Offs, Bit, Len, I: Int32;
  Dist: UInt32;
begin
  PbMask := (1 shl Pb) - 1;
  LpMask := (1 shl Lp) - 1;

  State := 0;
  Reps[0] := 0;
  Reps[1] := 0;
  Reps[2] := 0;
  Reps[3] := 0;

  // Limit always bounds the output. With a declared size it is that size; with
  // the unknown-size sentinel it is the hard cap, because nothing else stops a
  // few hundred bytes of rep matches from expanding until the allocator gives up.
  while (Int64(DstPos) < Limit) do
  begin
    PosState := DstPos and PbMask;

    if (RC.DecodeBit(Probs[pIsMatch + (State shl kNumPosBitsMax) + PosState]) = 0) then
    begin
      if (DstPos = 0) then
        PrevByte := 0
      else
        PrevByte := Dst[DstPos - 1];
      Base := pLiteral + $300 * (((Int32(DstPos) and LpMask) shl Lc) + (PrevByte shr (8 - Lc)));

      Symbol := 1;
      if (State >= 7) then
      begin
        if (Reps[0] >= DstPos) then
          CompressCodecException('corrupt lzma stream');
        MatchByte := Dst[DstPos - Reps[0] - 1];
        Offs := $100;
        repeat
          MatchByte := MatchByte shl 1;
          Bit := Offs;
          Offs := Offs and MatchByte;
          I := RC.DecodeBit(Probs[Base + Offs + Bit + Symbol]);
          Symbol := (Symbol shl 1) or I;
          if (I = 0) then
            Offs := Offs xor Bit;
        until (Symbol >= $100);
      end else
        repeat
          Symbol := (Symbol shl 1) or RC.DecodeBit(Probs[Base + Symbol]);
        until (Symbol >= $100);

      Emit(Byte(Symbol));

      if (State < 4) then
        State := 0
      else if (State < 10) then
        Dec(State, 3)
      else
        Dec(State, 6);

      Continue;
    end;

    if (RC.DecodeBit(Probs[pIsRep + State]) <> 0) then
    begin
      if (DstPos = 0) then
        CompressCodecException('corrupt lzma stream');

      if (RC.DecodeBit(Probs[pIsRepG0 + State]) = 0) then
      begin
        if (RC.DecodeBit(Probs[pIsRep0Long + (State shl kNumPosBitsMax) + PosState]) = 0) then
        begin
          // one byte at the last distance used
          if (State < 7) then
            State := 9
          else
            State := 11;
          if (Reps[0] >= DstPos) then
            CompressCodecException('corrupt lzma stream');
          Emit(Dst[DstPos - Reps[0] - 1]);
          Continue;
        end;
      end else
      begin
        if (RC.DecodeBit(Probs[pIsRepG1 + State]) = 0) then
          Dist := Reps[1]
        else
        begin
          if (RC.DecodeBit(Probs[pIsRepG2 + State]) = 0) then
            Dist := Reps[2]
          else
          begin
            Dist := Reps[3];
            Reps[3] := Reps[2];
          end;
          Reps[2] := Reps[1];
        end;
        Reps[1] := Reps[0];
        Reps[0] := Dist;
      end;

      Len := DecodeLen(pRepLenCoder, PosState) + kMatchMinLen;
      if (State < 7) then
        State := 8
      else
        State := 11;
    end else
    begin
      Reps[3] := Reps[2];
      Reps[2] := Reps[1];
      Reps[1] := Reps[0];

      Len := DecodeLen(pLenCoder, PosState) + kMatchMinLen;
      if (State < 7) then
        State := 7
      else
        State := 10;

      Dist := DecodeDistance(Len);
      if (Dist = $FFFFFFFF) then
        Break; // end of stream marker
      Reps[0] := Dist;
    end;

    if (Reps[0] >= DstPos) then
      CompressCodecException('lzma match reaches before the start of the output');
    if (Int64(DstPos) + Len > Limit) then
      Len := Limit - Int64(DstPos);

    for I := 1 to Len do
      Emit(Dst[DstPos - Reps[0] - 1]);
  end;
end;

class procedure LZMA.Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  D: PLZMADecoder;
  Props, NumProbs, I: Int32;
  UnpackSize, Reserve: UInt64;
  Limit: Int64;
  HaveSize: Boolean;
begin
  if (InSize < 13) then
    CompressCodecException('lzma data is too small (%d bytes)', [InSize]);

  Props := InData[0];
  if (Props >= 9 * 5 * 5) then
    CompressCodecException('invalid lzma properties byte');

  New(D);
  FillChar(D^, SizeOf(TLZMADecoder), 0);
  try
    D^.Lc := Props mod 9;
    D^.Lp := (Props div 9) mod 5;
    D^.Pb := (Props div 9) div 5;

    UnpackSize := LEtoN(PUInt64(InData + 5)^);
    HaveSize := (UnpackSize <> UInt64($FFFFFFFFFFFFFFFF));
    if HaveSize and (UnpackSize > MAX_OUTPUT) then
      CompressCodecException('lzma output would be %d bytes, over the %d limit', [UnpackSize, MAX_OUTPUT]);

    if HaveSize then
      Limit := Int64(UnpackSize)
    else
      Limit := MAX_OUTPUT;

    NumProbs := pLiteral + LiteralProbCount(D^.Lc, D^.Lp);
    D^.Probs := GetMem(NumProbs * SizeOf(UInt16));
    for I := 0 to NumProbs - 1 do
      D^.Probs[I] := kBitModelTotal div 2;

    D^.Dst := OutData;
    D^.DstPos := 0;
    if (OutData = nil) then
      D^.DstCap := 0
    else
      D^.DstCap := MemSize(OutData);
    // The header's size is a claim, not a fact: 13 bytes can assert 2GB. Reserve
    // only what this much input could plausibly produce and let Emit grow the
    // rest as real bytes arrive.
    Reserve := UInt64(InSize) * PREALLOC_RATIO;
    if HaveSize and (UnpackSize < Reserve) then
      Reserve := UnpackSize;
    if (Reserve > D^.DstCap) then
    begin
      D^.DstCap := Reserve;
      ReAllocMem(D^.Dst, D^.DstCap);
    end;

    D^.RC.Data := InData;
    D^.RC.Size := InSize;
    D^.RC.Pos := 13;

    try
      D^.RC.Init();
      // when the header says the size is unknown it is all ones, which is not a
      // size to hand on - decoding then runs to the end marker instead
      if (Limit > 0) then
        D^.Run(Limit, HaveSize);
    finally
      OutData := D^.Dst;
      OutSize := D^.DstPos;
    end;

    if HaveSize then
    begin
      if (UInt64(OutSize) <> UnpackSize) then
        CompressCodecException('lzma produced %d bytes, expected %d', [OutSize, UnpackSize]);
      // the range coder ends on zero when the stream was consumed cleanly
      if (D^.RC.Code <> 0) then
        CompressCodecException('corrupt lzma stream');
    end else
      if (OutSize >= MAX_OUTPUT) then
        CompressCodecException('lzma stream reached the %d byte limit with no end marker', [MAX_OUTPUT]);
  finally
    if (D^.Probs <> nil) then FreeMem(D^.Probs);
    Dispose(D);
  end;
end;

end.
