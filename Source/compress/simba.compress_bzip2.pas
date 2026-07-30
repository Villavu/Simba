{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  bzip2 1.0.8 by Julian Seward https://gitlab.com/bzip2/bzip2

  MakeCodeLengths      BZ2_hbMakeCodeLengths     huffman.c
  AssignCodes          BZ2_hbAssignCodes         huffman.c
  CreateDecodeTables   BZ2_hbCreateDecodeTables  huffman.c
  GenerateMTFValues    generateMTFValues         compress.c
  SendMTFValues        sendMTFValues             compress.c
  FlushBlock           BZ2_compressBlock         compress.c
  UndoTransform        BZ2_decompress            decompress.c
  SortBlock            fallbackSort              blocksort.c
  FallbackQSort3       fallbackQSort3            blocksort.c
  FallbackSimpleSort   fallbackSimpleSort        blocksort.c

  note: Only fallbackSort is ported.
}
unit simba.compress_bzip2;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.compress_codec;

type
  BZip2 = class(TCompressCodec)
  public
    class procedure Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
    class procedure Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64); override; overload;
  end;

implementation

{$R-}{$Q-}

const
  BLOCK_100K = 9;                  // 900k blocks, the same as `bzip2 -9`
  BLOCK_SIZE = BLOCK_100K * 100000;
  // a run can append five bytes past the room check, so stop short
  BLOCK_MARGIN = 19;
  WORK_SLACK   = 32;               // headroom on every per block working buffer
  BHTAB_SLACK  = 96;               // the sort sets sentinel bits past the block end
  GROW_MIN     = 65536;            // first size a growing output buffer takes

  GROUP_SIZE    = 50;              // symbols coded by one table before it may change
  MAX_GROUPS    = 6;
  MAX_ALPHA     = 258;
  MAX_CODE_LEN  = 23;
  MAX_SELECTORS = 2 + (900000 div GROUP_SIZE);
  N_ITERS       = 4;               // passes refining the table assignment

  RUNA = 0;
  RUNB = 1;

  LESSER_ICOST  = 0;               // seed lengths for the initial table split
  GREATER_ICOST = 15;

  BLOCK_MAGIC_HI = $314159;
  BLOCK_MAGIC_LO = $265359;
  EOS_MAGIC_HI   = $177245;
  EOS_MAGIC_LO   = $385090;

  CRCTable: array[0..255] of UInt32 = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9,
    $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61,
    $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9,
    $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011,
    $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    $9823B6E0, $9CE2AB57, $91A18D8E, $95609039,
    $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5,
    $BE2B5B58, $BAEA46EF, $B7A96036, $B3687D81,
    $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49,
    $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1,
    $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D,
    $34867077, $30476DC0, $3D044B19, $39C556AE,
    $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16,
    $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE,
    $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066,
    $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E,
    $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692,
    $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6,
    $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E,
    $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686,
    $D5B88683, $D1799B34, $DC3ABDED, $D8FBA05A,
    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637,
    $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F,
    $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47,
    $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF,
    $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    $F12F560E, $F5EE4BB9, $F8AD6D60, $FC6C70D7,
    $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F,
    $C423CD6A, $C0E2D0DD, $CDA1F604, $C960EBB3,
    $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7,
    $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B,
    $9B3660C6, $9FF77D71, $92B45BA8, $9675461F,
    $8832161A, $8CF30BAD, $81B02D74, $857130C3,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640,
    $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8,
    $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30,
    $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088,
    $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0,
    $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C,
    $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18,
    $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0,
    $9ABC8BD5, $9E7D9662, $933EB0BB, $97FFAD0C,
    $AFB010B1, $AB710D06, $A6322BDF, $A2F33668,
    $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4
  );

{ bit io, most significant bit first }

type
  TBitWriter = record
    Data: PByte;
    Pos, Cap: PtrUInt;
    Buf: UInt32;
    Live: Int32;

    procedure Init(AData: PByte);
    procedure PutByte(B: Byte);
    procedure PutBits(Count: Int32; Value: UInt32);
    procedure PutUInt32(Value: UInt32);
    procedure Finish;
  end;

  TBitReader = record
    Data: PByte;
    Pos, Size: PtrUInt;
    Buf: UInt64;
    Live: Int32;

    function GetBits(Count: Int32): UInt32;
  end;

procedure TBitWriter.Init(AData: PByte);
begin
  Data := AData;
  Pos := 0;
  Buf := 0;
  Live := 0;
  if (Data = nil) then
    Cap := 0
  else
    Cap := MemSize(Data);
end;

procedure TBitWriter.PutByte(B: Byte);
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

procedure TBitWriter.PutBits(Count: Int32; Value: UInt32);
begin
  while (Live >= 8) do
  begin
    PutByte(Byte(Buf shr 24));
    Buf := Buf shl 8;
    Dec(Live, 8);
  end;
  Buf := Buf or (Value shl (32 - Live - Count));
  Inc(Live, Count);
end;

procedure TBitWriter.PutUInt32(Value: UInt32);
begin
  PutBits(8, (Value shr 24) and $FF);
  PutBits(8, (Value shr 16) and $FF);
  PutBits(8, (Value shr 8) and $FF);
  PutBits(8, Value and $FF);
end;

procedure TBitWriter.Finish;
begin
  while (Live > 0) do
  begin
    PutByte(Byte(Buf shr 24));
    Buf := Buf shl 8;
    Dec(Live, 8);
  end;
end;

function TBitReader.GetBits(Count: Int32): UInt32;
begin
  while (Live < Count) do
  begin
    if (Pos >= Size) then
      CompressCodecException('bzip2 stream is truncated');
    Buf := (Buf shl 8) or Data[Pos];
    Inc(Pos);
    Inc(Live, 8);
  end;
  Dec(Live, Count);
  Result := UInt32((Buf shr Live) and ((UInt64(1) shl Count) - 1));
end;

{ huffman }

// frequencies live in the high bits of a weight so the low byte carries depth and
// breaks ties; too deep and the frequencies are flattened and it retries
procedure MakeCodeLengths(Len: PByte; Freq: PInteger; AlphaSize, MaxLen: Int32);
var
  Heap: array[0..MAX_ALPHA+1] of Int32;
  Weight: array[0..MAX_ALPHA*2] of Int32;
  Parent: array[0..MAX_ALPHA*2] of Int32;
  NNodes, NHeap, N1, N2, I, J, K, ZZ, YY, Tmp: Int32;
  TooLong: Boolean;
begin
  for I := 0 to AlphaSize - 1 do
    if (Freq[I] = 0) then
      Weight[I+1] := 1 shl 8
    else
      Weight[I+1] := Freq[I] shl 8;

  repeat
    NNodes := AlphaSize;
    NHeap := 0;
    Heap[0] := 0;
    Weight[0] := 0;
    Parent[0] := -2;

    for I := 1 to AlphaSize do
    begin
      Parent[I] := -1;
      Inc(NHeap);
      Heap[NHeap] := I;

      ZZ := NHeap;
      Tmp := Heap[ZZ];
      while (Weight[Tmp] < Weight[Heap[ZZ shr 1]]) do
      begin
        Heap[ZZ] := Heap[ZZ shr 1];
        ZZ := ZZ shr 1;
      end;
      Heap[ZZ] := Tmp;
    end;

    while (NHeap > 1) do
    begin
      N1 := Heap[1];
      Heap[1] := Heap[NHeap];
      Dec(NHeap);

      ZZ := 1;
      Tmp := Heap[ZZ];
      repeat
        YY := ZZ shl 1;
        if (YY > NHeap) then
          Break;
        if (YY < NHeap) and (Weight[Heap[YY+1]] < Weight[Heap[YY]]) then
          Inc(YY);
        if (Weight[Tmp] < Weight[Heap[YY]]) then
          Break;
        Heap[ZZ] := Heap[YY];
        ZZ := YY;
      until False;
      Heap[ZZ] := Tmp;

      N2 := Heap[1];
      Heap[1] := Heap[NHeap];
      Dec(NHeap);

      ZZ := 1;
      Tmp := Heap[ZZ];
      repeat
        YY := ZZ shl 1;
        if (YY > NHeap) then
          Break;
        if (YY < NHeap) and (Weight[Heap[YY+1]] < Weight[Heap[YY]]) then
          Inc(YY);
        if (Weight[Tmp] < Weight[Heap[YY]]) then
          Break;
        Heap[ZZ] := Heap[YY];
        ZZ := YY;
      until False;
      Heap[ZZ] := Tmp;

      Inc(NNodes);
      Parent[N1] := NNodes;
      Parent[N2] := NNodes;

      // add the frequencies, and carry one more than the deeper of the two depths
      if ((Weight[N1] and $FF) > (Weight[N2] and $FF)) then
        K := Weight[N1] and $FF
      else
        K := Weight[N2] and $FF;
      Weight[NNodes] := ((Weight[N1] and $FFFFFF00) + (Weight[N2] and $FFFFFF00)) or (1 + K);

      Parent[NNodes] := -1;
      Inc(NHeap);
      Heap[NHeap] := NNodes;

      ZZ := NHeap;
      Tmp := Heap[ZZ];
      while (Weight[Tmp] < Weight[Heap[ZZ shr 1]]) do
      begin
        Heap[ZZ] := Heap[ZZ shr 1];
        ZZ := ZZ shr 1;
      end;
      Heap[ZZ] := Tmp;
    end;

    TooLong := False;
    for I := 1 to AlphaSize do
    begin
      J := 0;
      K := I;
      while (Parent[K] >= 0) do
      begin
        K := Parent[K];
        Inc(J);
      end;
      Len[I-1] := J;
      if (J > MaxLen) then
        TooLong := True;
    end;

    if not TooLong then
      Break;

    for I := 1 to AlphaSize do
    begin
      J := 1 + ((Weight[I] shr 8) div 2);
      Weight[I] := J shl 8;
    end;
  until False;
end;

procedure AssignCodes(Code: PInteger; Len: PByte; MinLen, MaxLen, AlphaSize: Int32);
var
  N, I, Vec: Int32;
begin
  Vec := 0;
  for N := MinLen to MaxLen do
  begin
    for I := 0 to AlphaSize - 1 do
      if (Len[I] = N) then
      begin
        Code[I] := Vec;
        Inc(Vec);
      end;
    Vec := Vec shl 1;
  end;
end;

{ compression }

type
  PBZip2Encoder = ^TBZip2Encoder;
  TBZip2Encoder = record
    W: TBitWriter;

    Block: PByte;                 // run length encoded input for one block
    NBlock, NBlockMax: Int32;
    Ptr: PInteger;                // sorted rotation starts
    OrigPtr: Int32;
    SortEClass: PInteger;         // block sort scratch, allocated once
    SortBhtab: PCardinal;         // one bit per position, marking bucket starts

    Mtfv: PUInt16;                  // the move-to-front symbol stream
    NMTF: Int32;

    InUse: array[0..255] of Boolean;
    SeqToUnseq: array[0..255] of Byte;
    UnseqToSeq: array[0..255] of Byte;
    NInUse, AlphaSize: Int32;

    MtfFreq: array[0..MAX_ALPHA-1] of Int32;
    RFreq: array[0..MAX_GROUPS-1, 0..MAX_ALPHA-1] of Int32;
    Len: array[0..MAX_GROUPS-1, 0..MAX_ALPHA-1] of Byte;
    Code: array[0..MAX_GROUPS-1, 0..MAX_ALPHA-1] of Int32;

    Selector, SelectorMtf: array[0..MAX_SELECTORS-1] of Byte;
    NSelectors, NGroups: Int32;

    BlockCRC, CombinedCRC: UInt32;

    procedure AddRun(Ch: Byte; RunLen: Int32);
    procedure SortBlock;
    procedure GenerateMTFValues;
    procedure SendMTFValues;
    procedure FlushBlock;
  end;

// four literal bytes then a count of how many more
procedure TBZip2Encoder.AddRun(Ch: Byte; RunLen: Int32);
var
  I: Int32;
begin
  for I := 1 to RunLen do
    BlockCRC := (BlockCRC shl 8) xor CRCTable[((BlockCRC shr 24) xor Ch) and $FF];

  InUse[Ch] := True;
  if (RunLen < 4) then
    for I := 1 to RunLen do
    begin
      Block[NBlock] := Ch;
      Inc(NBlock);
    end
  else
  begin
    InUse[RunLen - 4] := True; // the count byte is part of the alphabet too
    for I := 1 to 4 do
    begin
      Block[NBlock] := Ch;
      Inc(NBlock);
    end;
    Block[NBlock] := Byte(RunLen - 4);
    Inc(NBlock);
  end;
end;

// bzip2's fallbackSimpleSort, on eclass rank rather than block bytes
procedure FallbackSimpleSort(Fmap, EClass: PInteger; Lo, Hi: Int32);
var
  I, J, Tmp, EcTmp: Int32;
begin
  if (Lo = Hi) then
    Exit;

  if (Hi - Lo > 3) then
    for I := Hi - 4 downto Lo do
    begin
      Tmp := Fmap[I];
      EcTmp := EClass[Tmp];
      J := I + 4;
      while (J <= Hi) and (EcTmp > EClass[Fmap[J]]) do
      begin
        Fmap[J-4] := Fmap[J];
        Inc(J, 4);
      end;
      Fmap[J-4] := Tmp;
    end;

  for I := Hi - 1 downto Lo do
  begin
    Tmp := Fmap[I];
    EcTmp := EClass[Tmp];
    J := I + 1;
    while (J <= Hi) and (EcTmp > EClass[Fmap[J]]) do
    begin
      Fmap[J-1] := Fmap[J];
      Inc(J);
    end;
    Fmap[J-1] := Tmp;
  end;
end;

// bzip2's fallbackQSort3: three way partition on rank, with the cheap
// pseudo-random pivot it prefers over median of three
procedure FallbackQSort3(Fmap, EClass: PInteger; LoSt, HiSt: Int32);
const
  SMALL_THRESH = 10;
  STACK_SIZE   = 100;
var
  StackLo, StackHi: array[0..STACK_SIZE-1] of Int32;
  Sp, Lo, Hi, UnLo, UnHi, LtLo, GtHi, N, M, I: Int32;
  R, R3, Med: UInt32;

  procedure Push(L, H: Int32); inline;
  begin
    StackLo[Sp] := L;
    StackHi[Sp] := H;
    Inc(Sp);
  end;

  procedure Swap(var A, B: Int32); inline;
  var
    T: Int32;
  begin
    T := A; A := B; B := T;
  end;

  procedure VSwap(P1, P2, Count: Int32); inline;
  begin
    while (Count > 0) do
    begin
      Swap(Fmap[P1], Fmap[P2]);
      Inc(P1); Inc(P2); Dec(Count);
    end;
  end;

begin
  R := 0;
  Sp := 0;
  Push(LoSt, HiSt);

  while (Sp > 0) do
  begin
    Dec(Sp);
    Lo := StackLo[Sp];
    Hi := StackHi[Sp];

    if (Hi - Lo < SMALL_THRESH) then
    begin
      FallbackSimpleSort(Fmap, EClass, Lo, Hi);
      Continue;
    end;

    R := ((R * 7621) + 1) mod 32768;
    R3 := R mod 3;
    if (R3 = 0) then
      Med := UInt32(EClass[Fmap[Lo]])
    else if (R3 = 1) then
      Med := UInt32(EClass[Fmap[(Lo + Hi) shr 1]])
    else
      Med := UInt32(EClass[Fmap[Hi]]);

    UnLo := Lo; LtLo := Lo;
    UnHi := Hi; GtHi := Hi;

    while True do
    begin
      while True do
      begin
        if (UnLo > UnHi) then
          Break;
        N := EClass[Fmap[UnLo]] - Int32(Med);
        if (N = 0) then
        begin
          Swap(Fmap[UnLo], Fmap[LtLo]);
          Inc(LtLo); Inc(UnLo);
          Continue;
        end;
        if (N > 0) then
          Break;
        Inc(UnLo);
      end;
      while True do
      begin
        if (UnLo > UnHi) then
          Break;
        N := EClass[Fmap[UnHi]] - Int32(Med);
        if (N = 0) then
        begin
          Swap(Fmap[UnHi], Fmap[GtHi]);
          Dec(GtHi); Dec(UnHi);
          Continue;
        end;
        if (N < 0) then
          Break;
        Dec(UnHi);
      end;
      if (UnLo > UnHi) then
        Break;
      Swap(Fmap[UnLo], Fmap[UnHi]);
      Inc(UnLo); Dec(UnHi);
    end;

    if (GtHi < LtLo) then
      Continue;

    N := LtLo - Lo; I := UnLo - LtLo; if (I < N) then N := I;
    VSwap(Lo, UnLo - N, N);
    M := Hi - GtHi; I := GtHi - UnHi; if (I < M) then M := I;
    VSwap(UnLo, Hi - M + 1, M);

    N := Lo + UnLo - LtLo - 1;
    M := Hi - (GtHi - UnHi) + 1;

    if (N - Lo > Hi - M) then
    begin
      Push(Lo, N);
      Push(M, Hi);
    end else
    begin
      Push(M, Hi);
      Push(Lo, N);
    end;
  end;
end;

// bzip2's fallbackSort: prefix doubling, bucket bounds in a bit array so each
// pass only sorts what is still unresolved. Upstream overlays the block on the
// eclass array and rebuilds it after; Block is separate here, so neither is needed.
procedure TBZip2Encoder.SortBlock;
var
  Ftab: array[0..256] of Int32;
  H, I, J, K, L, R, Cc, Cc1, N, NNotDone, NBhtab: Int32;
  Fmap, EClass: PInteger;
  Bhtab: PCardinal;

  procedure SetBh(Z: Int32); inline;
  begin
    Bhtab[Z shr 5] := Bhtab[Z shr 5] or (UInt32(1) shl (Z and 31));
  end;

  procedure ClearBh(Z: Int32); inline;
  begin
    Bhtab[Z shr 5] := Bhtab[Z shr 5] and not (UInt32(1) shl (Z and 31));
  end;

  function IsSetBh(Z: Int32): Boolean; inline;
  begin
    Result := (Bhtab[Z shr 5] and (UInt32(1) shl (Z and 31))) <> 0;
  end;

begin
  N := NBlock;
  if (N < 1) then
    Exit;
  if (N = 1) then
  begin
    Ptr[0] := 0;
    Exit;
  end;

  Fmap := Ptr;
  EClass := SortEClass;
  Bhtab := SortBhtab;

  // one byte radix sort for the initial buckets and their boundary bits
  FillDWord(Ftab[0], 257, 0);
  for I := 0 to N - 1 do
    Inc(Ftab[Block[I]]);
  for I := 1 to 256 do
    Inc(Ftab[I], Ftab[I-1]);
  for I := 0 to N - 1 do
  begin
    J := Block[I];
    Dec(Ftab[J]);
    Fmap[Ftab[J]] := I;
  end;

  NBhtab := 2 + (N div 32);
  FillDWord(Bhtab^, NBhtab, 0);
  for I := 0 to 255 do
    SetBh(Ftab[I]);

  // sentinels, so the bucket scan below can run off the end safely
  for I := 0 to 31 do
  begin
    SetBh(N + 2*I);
    ClearBh(N + 2*I + 1);
  end;

  H := 1;
  while True do
  begin
    J := 0;
    for I := 0 to N - 1 do
    begin
      if IsSetBh(I) then
        J := I;
      K := Fmap[I] - H;
      if (K < 0) then
        Inc(K, N);
      EClass[K] := J;
    end;

    NNotDone := 0;
    R := -1;
    while True do
    begin
      // walk to the next bucket holding more than one entry
      K := R + 1;
      while IsSetBh(K) and ((K and 31) <> 0) do Inc(K);
      if IsSetBh(K) then
      begin
        while (Bhtab[K shr 5] = $FFFFFFFF) do Inc(K, 32);
        while IsSetBh(K) do Inc(K);
      end;
      L := K - 1;
      if (L >= N) then
        Break;
      while (not IsSetBh(K)) and ((K and 31) <> 0) do Inc(K);
      if not IsSetBh(K) then
      begin
        while (Bhtab[K shr 5] = 0) do Inc(K, 32);
        while not IsSetBh(K) do Inc(K);
      end;
      R := K - 1;
      if (R >= N) then
        Break;

      if (R > L) then
      begin
        Inc(NNotDone, R - L + 1);
        FallbackQSort3(Fmap, EClass, L, R);

        Cc := -1;
        for I := L to R do
        begin
          Cc1 := EClass[Fmap[I]];
          if (Cc <> Cc1) then
          begin
            SetBh(I);
            Cc := Cc1;
          end;
        end;
      end;
    end;

    H := H * 2;
    if (H > N) or (NNotDone = 0) then
      Break;
  end;
end;

procedure TBZip2Encoder.GenerateMTFValues;
var
  Yy: array[0..255] of Byte;
  I, J, WR, ZPend: Int32;
  LL, Tmp, Tmp2: Byte;
begin
  NInUse := 0;
  for I := 0 to 255 do
    if InUse[I] then
    begin
      SeqToUnseq[NInUse] := I;
      UnseqToSeq[I] := NInUse;
      Inc(NInUse);
    end;
  AlphaSize := NInUse + 2;

  FillDWord(MtfFreq[0], MAX_ALPHA, 0);
  for I := 0 to NInUse - 1 do
    Yy[I] := I;

  WR := 0;
  ZPend := 0;
  for I := 0 to NBlock - 1 do
  begin
    J := Ptr[I] - 1;
    if (J < 0) then
      Inc(J, NBlock);
    LL := UnseqToSeq[Block[J]];

    if (Yy[0] = LL) then
      Inc(ZPend)
    else
    begin
      if (ZPend > 0) then
      begin
        // a run of N zeros goes out as N+1 in bijective base 2
        Dec(ZPend);
        repeat
          if (ZPend and 1 <> 0) then
          begin
            Mtfv[WR] := RUNB;
            Inc(MtfFreq[RUNB]);
          end else
          begin
            Mtfv[WR] := RUNA;
            Inc(MtfFreq[RUNA]);
          end;
          Inc(WR);
          if (ZPend < 2) then
            Break;
          ZPend := (ZPend - 2) div 2;
        until False;
        ZPend := 0;
      end;

      // shift the list along as we search it, which is the move to front
      J := 1;
      Tmp := Yy[1];
      Yy[1] := Yy[0];
      while (LL <> Tmp) do
      begin
        Inc(J);
        Tmp2 := Tmp;
        Tmp := Yy[J];
        Yy[J] := Tmp2;
      end;
      Yy[0] := Tmp;

      Mtfv[WR] := J + 1;
      Inc(MtfFreq[J + 1]);
      Inc(WR);
    end;
  end;

  if (ZPend > 0) then
  begin
    Dec(ZPend);
    repeat
      if (ZPend and 1 <> 0) then
      begin
        Mtfv[WR] := RUNB;
        Inc(MtfFreq[RUNB]);
      end else
      begin
        Mtfv[WR] := RUNA;
        Inc(MtfFreq[RUNA]);
      end;
      Inc(WR);
      if (ZPend < 2) then
        Break;
      ZPend := (ZPend - 2) div 2;
    until False;
  end;

  Mtfv[WR] := AlphaSize - 1; // end of block
  Inc(MtfFreq[AlphaSize - 1]);
  Inc(WR);

  NMTF := WR;
end;

procedure TBZip2Encoder.SendMTFValues;
var
  Cost: array[0..MAX_GROUPS-1] of Int32;
  Pos: array[0..MAX_GROUPS-1] of Byte;
  InUse16: array[0..15] of Boolean;
  MinLens: array[0..MAX_GROUPS-1] of Int32;
  MaxLens: array[0..MAX_GROUPS-1] of Int32;
  T, V, I, J, GS, GE, Iter, BC, BT, NPart, RemF, TFreq, AFreq, Curr: Int32;
  Sel: Int32;
  LL, Tmp, Tmp2: Byte;
begin
  if (NMTF < 200) then
    NGroups := 2
  else if (NMTF < 600) then
    NGroups := 3
  else if (NMTF < 1200) then
    NGroups := 4
  else if (NMTF < 2400) then
    NGroups := 5
  else
    NGroups := 6;

  // seed: slice the alphabet into runs of roughly equal weight, one per table
  NPart := NGroups;
  RemF := NMTF;
  GS := 0;
  while (NPart > 0) do
  begin
    TFreq := RemF div NPart;
    GE := GS - 1;
    AFreq := 0;
    while (AFreq < TFreq) and (GE < AlphaSize - 1) do
    begin
      Inc(GE);
      Inc(AFreq, MtfFreq[GE]);
    end;

    if (GE > GS) and (NPart <> NGroups) and (NPart <> 1) and ((NGroups - NPart) mod 2 = 1) then
    begin
      Dec(AFreq, MtfFreq[GE]);
      Dec(GE);
    end;

    for V := 0 to AlphaSize - 1 do
      if (V >= GS) and (V <= GE) then
        Len[NPart-1][V] := LESSER_ICOST
      else
        Len[NPart-1][V] := GREATER_ICOST;

    Dec(NPart);
    GS := GE + 1;
    Dec(RemF, AFreq);
  end;

  // refine: each group of 50 to its cheapest table, rebuild, repeat
  for Iter := 1 to N_ITERS do
  begin
    for T := 0 to NGroups - 1 do
      FillDWord(RFreq[T][0], MAX_ALPHA, 0);

    NSelectors := 0;
    GS := 0;
    while (GS < NMTF) do
    begin
      GE := GS + GROUP_SIZE - 1;
      if (GE >= NMTF) then
        GE := NMTF - 1;

      for T := 0 to NGroups - 1 do
        Cost[T] := 0;
      for I := GS to GE do
        for T := 0 to NGroups - 1 do
          Inc(Cost[T], Len[T][Mtfv[I]]);

      BC := MaxInt;
      BT := 0;
      for T := 0 to NGroups - 1 do
        if (Cost[T] < BC) then
        begin
          BC := Cost[T];
          BT := T;
        end;

      Selector[NSelectors] := BT;
      Inc(NSelectors);
      for I := GS to GE do
        Inc(RFreq[BT][Mtfv[I]]);

      GS := GE + 1;
    end;

    for T := 0 to NGroups - 1 do
      MakeCodeLengths(@Len[T][0], @RFreq[T][0], AlphaSize, 17);
  end;

  for T := 0 to NGroups - 1 do
  begin
    MinLens[T] := 32;
    MaxLens[T] := 0;
    for I := 0 to AlphaSize - 1 do
    begin
      if (Len[T][I] > MaxLens[T]) then
        MaxLens[T] := Len[T][I];
      if (Len[T][I] < MinLens[T]) then
        MinLens[T] := Len[T][I];
    end;
    AssignCodes(@Code[T][0], @Len[T][0], MinLens[T], MaxLens[T], AlphaSize);
  end;

  // the selectors themselves are move-to-front coded
  for I := 0 to NGroups - 1 do
    Pos[I] := I;
  for I := 0 to NSelectors - 1 do
  begin
    LL := Selector[I];
    J := 0;
    Tmp := Pos[J];
    while (LL <> Tmp) do
    begin
      Inc(J);
      Tmp2 := Tmp;
      Tmp := Pos[J];
      Pos[J] := Tmp2;
    end;
    Pos[0] := Tmp;
    SelectorMtf[I] := J;
  end;

  // which bytes the block uses, as a 16 bit map of 16 bit maps
  for I := 0 to 15 do
  begin
    InUse16[I] := False;
    for J := 0 to 15 do
      if InUse[I*16 + J] then
        InUse16[I] := True;
  end;
  for I := 0 to 15 do
    W.PutBits(1, Ord(InUse16[I]));
  for I := 0 to 15 do
    if InUse16[I] then
      for J := 0 to 15 do
        W.PutBits(1, Ord(InUse[I*16 + J]));

  W.PutBits(3, NGroups);
  W.PutBits(15, NSelectors);
  for I := 0 to NSelectors - 1 do
  begin
    for J := 1 to SelectorMtf[I] do
      W.PutBits(1, 1);
    W.PutBits(1, 0);
  end;

  // code lengths, as a delta walk from the previous one
  for T := 0 to NGroups - 1 do
  begin
    Curr := Len[T][0];
    W.PutBits(5, Curr);
    for I := 0 to AlphaSize - 1 do
    begin
      while (Curr < Len[T][I]) do
      begin
        W.PutBits(2, 2);
        Inc(Curr);
      end;
      while (Curr > Len[T][I]) do
      begin
        W.PutBits(2, 3);
        Dec(Curr);
      end;
      W.PutBits(1, 0);
    end;
  end;

  Sel := 0;
  GS := 0;
  while (GS < NMTF) do
  begin
    GE := GS + GROUP_SIZE - 1;
    if (GE >= NMTF) then
      GE := NMTF - 1;
    T := Selector[Sel];
    for I := GS to GE do
      W.PutBits(Len[T][Mtfv[I]], Code[T][Mtfv[I]]);
    GS := GE + 1;
    Inc(Sel);
  end;
end;

procedure TBZip2Encoder.FlushBlock;
var
  I: Int32;
begin
  BlockCRC := not BlockCRC;
  CombinedCRC := ((CombinedCRC shl 1) or (CombinedCRC shr 31)) xor BlockCRC;

  SortBlock();
  OrigPtr := -1;
  for I := 0 to NBlock - 1 do
    if (Ptr[I] = 0) then
    begin
      OrigPtr := I;
      Break;
    end;
  // PutBits does not mask, and a negative here would overwrite the block CRC
  if (OrigPtr < 0) then
    CompressCodecException('bzip2 block sort produced no origin');

  W.PutBits(24, BLOCK_MAGIC_HI);
  W.PutBits(24, BLOCK_MAGIC_LO);
  W.PutUInt32(BlockCRC);
  W.PutBits(1, 0); // not randomised
  W.PutBits(24, OrigPtr);

  GenerateMTFValues();
  SendMTFValues();
end;

class procedure BZip2.Compress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  E: PBZip2Encoder;
  Pos: Int64;
  Ch: Byte;
  RunLen: Int32;
begin
  New(E);
  FillChar(E^, SizeOf(TBZip2Encoder), 0);
  try
    E^.NBlockMax := BLOCK_SIZE - BLOCK_MARGIN;
    E^.Block := GetMem(E^.NBlockMax + WORK_SLACK);
    E^.Ptr := GetMem((E^.NBlockMax + WORK_SLACK) * SizeOf(Int32));
    E^.Mtfv := GetMem((E^.NBlockMax + WORK_SLACK) * SizeOf(UInt16));
    E^.SortEClass := GetMem((E^.NBlockMax + WORK_SLACK) * SizeOf(Int32));
    // one bit per position, plus the sentinels the bucket scan runs into
    E^.SortBhtab := GetMem((2 + (E^.NBlockMax + BHTAB_SLACK) div 32) * SizeOf(UInt32));

    E^.W.Init(OutData);
    E^.CombinedCRC := 0;

    try
      E^.W.PutBits(8, Ord('B'));
      E^.W.PutBits(8, Ord('Z'));
      E^.W.PutBits(8, Ord('h'));
      E^.W.PutBits(8, Ord('0') + BLOCK_100K);

      Pos := 0;
      while (Pos < InSize) do
      begin
        E^.NBlock := 0;
        E^.BlockCRC := $FFFFFFFF;
        FillChar(E^.InUse, SizeOf(E^.InUse), 0);

        while (Pos < InSize) and (E^.NBlock < E^.NBlockMax) do
        begin
          Ch := InData[Pos];
          Inc(Pos);
          RunLen := 1;
          while (Pos < InSize) and (InData[Pos] = Ch) and (RunLen < 255) do
          begin
            Inc(RunLen);
            Inc(Pos);
          end;
          E^.AddRun(Ch, RunLen);
        end;

        E^.FlushBlock();
      end;

      E^.W.PutBits(24, EOS_MAGIC_HI);
      E^.W.PutBits(24, EOS_MAGIC_LO);
      E^.W.PutUInt32(E^.CombinedCRC);
      E^.W.Finish();
    finally
      // the writer reallocates, so refresh the caller's pointer even on failure
      OutData := E^.W.Data;
      OutSize := E^.W.Pos;
    end;
  finally
    if (E^.Block <> nil) then FreeMem(E^.Block);
    if (E^.Ptr <> nil) then FreeMem(E^.Ptr);
    if (E^.Mtfv <> nil) then FreeMem(E^.Mtfv);
    if (E^.SortEClass <> nil) then FreeMem(E^.SortEClass);
    if (E^.SortBhtab <> nil) then FreeMem(E^.SortBhtab);
    Dispose(E);
  end;
end;

{ decompression }

type
  PBZip2Decoder = ^TBZip2Decoder;
  TBZip2Decoder = record
    R: TBitReader;

    Dst: PByte;
    DstPos, DstCap: PtrUInt;

    TT: PUInt32;                  // the block, then the inverse transform vector
    NBlock, BlockMax: Int32;
    OrigPtr: Int32;

    SeqToUnseq: array[0..255] of Byte;
    NInUse, AlphaSize: Int32;
    Unzftab: array[0..255] of Int32;

    Len: array[0..MAX_GROUPS-1, 0..MAX_ALPHA-1] of Byte;
    Limit: array[0..MAX_GROUPS-1, 0..MAX_CODE_LEN] of Int32;
    Base: array[0..MAX_GROUPS-1, 0..MAX_CODE_LEN] of Int32;
    Perm: array[0..MAX_GROUPS-1, 0..MAX_ALPHA-1] of Int32;
    MinLens: array[0..MAX_GROUPS-1] of Int32;

    Selector: array[0..MAX_SELECTORS-1] of Byte;
    NSelectors, NGroups: Int32;

    BlockCRC, CombinedCRC: UInt32;

    procedure Emit(B: Byte);
    procedure CreateDecodeTables(T, MinLen, MaxLen: Int32);
    procedure ReadTrees;
    procedure ReadSymbols;
    procedure UndoTransform;
  end;

procedure TBZip2Decoder.Emit(B: Byte);
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
  BlockCRC := (BlockCRC shl 8) xor CRCTable[((BlockCRC shr 24) xor B) and $FF];
end;

// Limit[n] is the largest code of n bits; Base[n] turns one into a Perm index
procedure TBZip2Decoder.CreateDecodeTables(T, MinLen, MaxLen: Int32);
var
  PP, I, J, Vec: Int32;
begin
  PP := 0;
  for I := MinLen to MaxLen do
    for J := 0 to AlphaSize - 1 do
      if (Len[T][J] = I) then
      begin
        Perm[T][PP] := J;
        Inc(PP);
      end;

  for I := 0 to MAX_CODE_LEN do
    Base[T][I] := 0;
  for I := 0 to AlphaSize - 1 do
    Inc(Base[T][Len[T][I] + 1]);
  for I := 1 to MAX_CODE_LEN do
    Inc(Base[T][I], Base[T][I-1]);

  for I := 0 to MAX_CODE_LEN do
    Limit[T][I] := 0;
  Vec := 0;
  for I := MinLen to MaxLen do
  begin
    Inc(Vec, Base[T][I+1] - Base[T][I]);
    Limit[T][I] := Vec - 1;
    Vec := Vec shl 1;
  end;
  for I := MinLen + 1 to MaxLen do
    Base[T][I] := ((Limit[T][I-1] + 1) shl 1) - Base[T][I];
end;

procedure TBZip2Decoder.ReadTrees;
var
  InUse16, Bits: UInt32;
  Pos: array[0..MAX_GROUPS-1] of Byte;
  I, J, T, Curr, MinLen, MaxLen: Int32;
  Tmp: Byte;
begin
  InUse16 := R.GetBits(16);
  NInUse := 0;
  for I := 0 to 15 do
    if (InUse16 and (UInt32($8000) shr I) <> 0) then
    begin
      Bits := R.GetBits(16);
      for J := 0 to 15 do
        if (Bits and (UInt32($8000) shr J) <> 0) then
        begin
          SeqToUnseq[NInUse] := I*16 + J;
          Inc(NInUse);
        end;
    end;
  if (NInUse = 0) then
    CompressCodecException('bzip2 block uses no symbols');
  AlphaSize := NInUse + 2;

  NGroups := R.GetBits(3);
  NSelectors := R.GetBits(15);
  if (NGroups < 2) or (NGroups > MAX_GROUPS) then
    CompressCodecException('bzip2 block has %d huffman tables', [NGroups]);
  if (NSelectors < 1) or (NSelectors > MAX_SELECTORS) then
    CompressCodecException('bzip2 block has %d selectors', [NSelectors]);

  for I := 0 to NGroups - 1 do
    Pos[I] := I;
  for I := 0 to NSelectors - 1 do
  begin
    J := 0;
    while (R.GetBits(1) = 1) do
    begin
      Inc(J);
      if (J >= NGroups) then
        CompressCodecException('bzip2 selector is out of range');
    end;

    Tmp := Pos[J];
    while (J > 0) do
    begin
      Pos[J] := Pos[J-1];
      Dec(J);
    end;
    Pos[0] := Tmp;
    Selector[I] := Tmp;
  end;

  for T := 0 to NGroups - 1 do
  begin
    Curr := R.GetBits(5);
    for I := 0 to AlphaSize - 1 do
    begin
      repeat
        if (Curr < 1) or (Curr > 20) then
          CompressCodecException('bzip2 code length is out of range');
        if (R.GetBits(1) = 0) then
          Break;
        if (R.GetBits(1) = 0) then
          Inc(Curr)
        else
          Dec(Curr);
      until False;
      Len[T][I] := Curr;
    end;
  end;

  for T := 0 to NGroups - 1 do
  begin
    MinLen := 32;
    MaxLen := 0;
    for I := 0 to AlphaSize - 1 do
    begin
      if (Len[T][I] > MaxLen) then
        MaxLen := Len[T][I];
      if (Len[T][I] < MinLen) then
        MinLen := Len[T][I];
    end;
    CreateDecodeTables(T, MinLen, MaxLen);
    MinLens[T] := MinLen;
  end;
end;

procedure TBZip2Decoder.ReadSymbols;
var
  Mtf: array[0..255] of Byte;
  GroupNo, GroupPos, GSel, ZN, ZVec, Sym, EOB: Int32;
  I, J, N, ES: Int32;
  UC, Tmp, Tmp2: Byte;

  function NextSymbol: Int32;
  begin
    if (GroupPos = 0) then
    begin
      Inc(GroupNo);
      if (GroupNo >= NSelectors) then
        CompressCodecException('bzip2 block ran out of selectors');
      GroupPos := GROUP_SIZE;
      GSel := Selector[GroupNo];
    end;
    Dec(GroupPos);

    ZN := MinLens[GSel];
    ZVec := R.GetBits(ZN);
    while (ZVec > Limit[GSel][ZN]) do
    begin
      Inc(ZN);
      if (ZN > 20) then
        CompressCodecException('invalid bzip2 huffman code');
      ZVec := (ZVec shl 1) or Int32(R.GetBits(1));
    end;
    Result := ZVec - Base[GSel][ZN];
    if (Result < 0) or (Result >= AlphaSize) then
      CompressCodecException('invalid bzip2 huffman code');
    Result := Perm[GSel][Result];
  end;

begin
  EOB := AlphaSize - 1;
  NBlock := 0;
  GroupNo := -1;
  GroupPos := 0;

  FillDWord(Unzftab[0], 256, 0);
  for I := 0 to NInUse - 1 do
    Mtf[I] := I;

  Sym := NextSymbol();
  while (Sym <> EOB) do
  begin
    if (Sym = RUNA) or (Sym = RUNB) then
    begin
      // a bijective base 2 count of how many times the front of the list repeats
      ES := 0;
      N := 1;
      repeat
        if (N > BLOCK_SIZE) then
          CompressCodecException('bzip2 run length is out of range');
        if (Sym = RUNA) then
          Inc(ES, N)
        else
          Inc(ES, 2 * N);
        N := N * 2;
        Sym := NextSymbol();
      until (Sym <> RUNA) and (Sym <> RUNB);

      UC := SeqToUnseq[Mtf[0]];
      Inc(Unzftab[UC], ES);
      if (NBlock + ES > BlockMax) then
        CompressCodecException('bzip2 block is bigger than its header allows');
      for I := 1 to ES do
      begin
        TT[NBlock] := UC;
        Inc(NBlock);
      end;
    end else
    begin
      if (NBlock >= BlockMax) then
        CompressCodecException('bzip2 block is bigger than its header allows');

      J := Sym - 1;
      if (J >= NInUse) then
        CompressCodecException('bzip2 symbol is out of range');

      Tmp := Mtf[J];
      while (J > 0) do
      begin
        Tmp2 := Mtf[J-1];
        Mtf[J] := Tmp2;
        Dec(J);
      end;
      Mtf[0] := Tmp;

      UC := SeqToUnseq[Tmp];
      Inc(Unzftab[UC]);
      TT[NBlock] := UC;
      Inc(NBlock);

      Sym := NextSymbol();
    end;
  end;

  if (OrigPtr < 0) or (OrigPtr >= NBlock) then
    CompressCodecException('bzip2 block pointer is out of range');
end;

// bzip2's vector walk: the low byte of each TT entry is the block byte, the high
// bits point at the next one. RLE1 is undone on the way out.
procedure TBZip2Decoder.UndoTransform;
var
  CfTab: array[0..256] of Int32;
  I, K, RunCount, Last: Int32;
  TPos: UInt32;
  C: Byte;
begin
  CfTab[0] := 0;
  for I := 0 to 255 do
    CfTab[I+1] := CfTab[I] + Unzftab[I];

  for I := 0 to NBlock - 1 do
  begin
    C := Byte(TT[I]);
    TT[CfTab[C]] := TT[CfTab[C]] or (UInt32(I) shl 8);
    Inc(CfTab[C]);
  end;

  TPos := TT[OrigPtr] shr 8;

  RunCount := 0;
  Last := -1;
  K := 0;
  while (K < NBlock) do
  begin
    if (TPos >= UInt32(NBlock)) then
      CompressCodecException('corrupt bzip2 block');
    TPos := TT[TPos];
    C := TPos and $FF;
    TPos := TPos shr 8;
    Inc(K);

    if (RunCount = 4) then
    begin
      for I := 1 to C do
        Emit(Byte(Last));
      RunCount := 0;
      Last := -1;
    end else
    begin
      if (C = Last) then
        Inc(RunCount)
      else
      begin
        RunCount := 1;
        Last := C;
      end;
      Emit(C);
    end;
  end;
end;

class procedure BZip2.Decompress(InData: PByte; InSize: Int64; var OutData: PByte; out OutSize: Int64);
var
  D: PBZip2Decoder;
  Hi, Lo, Stored: UInt32;
  StreamPos: Int64;
  TTSize: Int32;
begin
  New(D);
  FillChar(D^, SizeOf(TBZip2Decoder), 0);
  TTSize := 0;
  try
    D^.R.Data := InData;
    D^.R.Size := InSize;

    D^.Dst := OutData;
    D^.DstPos := 0;
    if (OutData = nil) then
      D^.DstCap := 0
    else
      D^.DstCap := MemSize(OutData);

    StreamPos := 0;
    try
      // a .bz2 file may be several whole streams concatenated
      repeat
        if (InSize - StreamPos < 10) then
          CompressCodecException('bzip2 data is too small (%d bytes)', [InSize - StreamPos]);
        if (InData[StreamPos] <> Ord('B')) or (InData[StreamPos+1] <> Ord('Z')) or (InData[StreamPos+2] <> Ord('h')) then
          CompressCodecException('not a bzip2 stream');
        if (InData[StreamPos+3] < Ord('1')) or (InData[StreamPos+3] > Ord('9')) then
          CompressCodecException('invalid bzip2 block size');

        D^.BlockMax := (InData[StreamPos+3] - Ord('0')) * 100000;
        if (D^.BlockMax + WORK_SLACK > TTSize) then
        begin
          TTSize := D^.BlockMax + WORK_SLACK;
          ReAllocMem(D^.TT, PtrUInt(TTSize) * SizeOf(UInt32));
        end;

        D^.R.Pos := StreamPos + 4;
        D^.R.Buf := 0;
        D^.R.Live := 0;
        D^.CombinedCRC := 0;

        repeat
          Hi := D^.R.GetBits(24);
          Lo := D^.R.GetBits(24);

          if (Hi = EOS_MAGIC_HI) and (Lo = EOS_MAGIC_LO) then
          begin
            Stored := D^.R.GetBits(32);
            if (Stored <> D^.CombinedCRC) then
              CompressCodecException('bzip2 stream checksum mismatch');
            Break;
          end;

          if (Hi <> BLOCK_MAGIC_HI) or (Lo <> BLOCK_MAGIC_LO) then
            CompressCodecException('corrupt bzip2 block header');

          Stored := D^.R.GetBits(32);
          if (D^.R.GetBits(1) <> 0) then
            CompressCodecException('randomised bzip2 blocks are not supported');
          D^.OrigPtr := D^.R.GetBits(24);

          D^.ReadTrees();
          D^.ReadSymbols();

          D^.BlockCRC := $FFFFFFFF;
          D^.UndoTransform();
          D^.BlockCRC := not D^.BlockCRC;

          if (Stored <> D^.BlockCRC) then
            CompressCodecException('bzip2 block checksum mismatch');
          D^.CombinedCRC := ((D^.CombinedCRC shl 1) or (D^.CombinedCRC shr 31)) xor D^.BlockCRC;
        until False;

        // a following stream starts at the next byte boundary
        Dec(D^.R.Live, D^.R.Live mod 8);
        StreamPos := Int64(D^.R.Pos) - (D^.R.Live div 8);
      until (StreamPos + 10 > InSize) or (InData[StreamPos] <> Ord('B')) or (InData[StreamPos+1] <> Ord('Z'));
    finally
      OutData := D^.Dst;
      OutSize := D^.DstPos;
    end;
  finally
    if (D^.TT <> nil) then FreeMem(D^.TT);
    Dispose(D);
  end;
end;

end.
