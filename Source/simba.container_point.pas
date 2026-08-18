{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Point containers:

  "TPointBuffer" and "TPointArrayBuffer" are growable buffers; ToArray(False)
  hands out the internal array without copying.

  "TPointSet" and "TPointHashSet" is presence only (exists or doesnt).
  "TPointTallySet" and "TPointTallyHashSet" stores a additional integer per point.

  Algorithmically the differences are:
    - "TPointSet" and "TPointTallySet" uses scanline for extremely fast lookup
      however needs allocate an array the size of entire point bounds.
    - "TPointHashSet" and "TPointTallyHashSet" uses a lightweight dictonary.

  "TPointSpatialHash" is separate: buckets point-indices by cell for neighbourhood queries.
}
unit simba.container_point;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  // A growable buffer of points. ToArray(False) hands out the internal array without copying.
  TPointBuffer = record
  private
    FLength: Integer;
    FCount: Integer;
    FArr: TPointArray;

    procedure Grow(const Len: Integer = 1);
    function GetItem(const Index: Integer): TPoint; inline;
  public
    property Size: Integer read FLength;
    property Count: Integer read FCount;
    property Item[Index: Integer]: TPoint read GetItem; default;

    procedure Clear;
    procedure Init(const InitialSize: Integer = 1024);
    procedure InitWith(const Values: TPointArray);

    procedure Add(const Value: TPoint); overload; inline;
    procedure Add(const X, Y: Integer); overload; inline;
    procedure Add(const Values: TPointArray); overload; inline;

    function First: TPoint; inline;
    function Last: TPoint; inline;
    function Pop: TPoint; inline;

    function ToArray(Copy: Boolean = True): TPointArray;

    class operator Initialize(var Self: TPointBuffer);
  end;

  // A growable buffer of point-arrays. ToArray(False) hands out the internal array without copying.
  TPointArrayBuffer = record
  private
    FLength: Integer;
    FCount: Integer;
    FArr: T2DPointArray;

    procedure Grow(const Len: Integer = 1);
    function GetItem(const Index: Integer): TPointArray; inline;
  public
    property Size: Integer read FLength;
    property Count: Integer read FCount;
    property Item[Index: Integer]: TPointArray read GetItem; default;

    procedure Clear;
    procedure Init(const InitialSize: Integer = 1024);
    procedure InitWith(const Values: T2DPointArray);

    procedure Add(const Value: TPointArray); overload; inline;
    procedure Add(const Values: T2DPointArray); overload; inline;

    function First: TPointArray; inline;
    function Last: TPointArray; inline;
    function Pop: TPointArray; inline;

    function ToArray(Copy: Boolean = True): T2DPointArray;

    class operator Initialize(var Self: TPointArrayBuffer);
  end;

  // Presence only: uses scanline
  TPointSet = record
  private
    FBits: array of UInt64; // 1 bit per pixel (bit-packed presence grid)
    FBounds: TBox;
    FWidth, FHeight: Integer;
    FCount: Integer;

    function IndexOf(const P: TPoint): Integer; inline;
  public
    class operator Initialize(var Self: TPointSet);

    procedure Init(const ABounds: TBox);

    // Adds P. False if it was already there
    function Add(const P: TPoint): Boolean;
    // True if P has been added.
    function Contains(const P: TPoint): Boolean; inline;
    // Every point added
    function ToArray: TPointArray;

    property Count: Integer read FCount;
  end;

  // Presence only: uses hashing
  TPointHashSet = record
  private
    FKeys: array of Int64;   // open-addressing; 0 = empty slot (8-byte slots vs the old {Key; Used} = 16)
    FHasZero: Boolean;       // the point (0,0) packs to key 0 = the empty sentinel, so it's tracked here instead
    FCount: Integer;

    function SlotOf(const Key: Int64): Integer; inline;   // Key must be non-zero
    procedure Grow;
  public
    class operator Initialize(var Self: TPointHashSet);

    procedure Init(ACapacity: Integer = 64);

    function Add(const P: TPoint): Boolean;
    function Contains(const P: TPoint): Boolean; inline;
    function ToArray: TPointArray;

    property Count: Integer read FCount;
  end;

  // Each point stores a number with it: uses scanline
  TPointTallySet = record
  private
    FCells: TIntegerArray;
    FBounds: TBox;
    FWidth, FHeight: Integer;
    FCount: Integer;

    function IndexOf(const P: TPoint): Integer; inline;
  public
    class operator Initialize(var Self: TPointTallySet);

    procedure Init(const ABounds: TBox);
    // Store P with a number of zero. False if it was already stored.
    function Add(const P: TPoint): Boolean;
    // Add one to P's number, but only if it currently equals IfValue.
    procedure IncrementIf(const P: TPoint; IfValue: Integer); inline;
    // Every point whose number reached at least MinValue.
    function ToArray(const MinValue: Integer = 0): TPointArray;
  end;

  // Each point stores a number with it: uses hashing
  TPointTallyHashSet = record
  private
  type
    TSlot = record
      Key: Int64;
      Data: Integer; // Count + 1, so a zeroed slot is an empty one
    end;
    TSlots = array of TSlot;
  private
    FSlots: TSlots;
    FCount: Integer;

    function SlotOf(const Key: Int64): Integer; inline;
    procedure Grow;
  public
    class operator Initialize(var Self: TPointTallyHashSet);

    procedure Init(ACapacity: Integer = 64);
    // Store P with a number of zero. False if it was already stored.
    function Add(const P: TPoint): Boolean;
    // Add one to P's number, but only if it currently equals IfValue
    procedure IncrementIf(const P: TPoint; IfValue: Integer); inline;
    // Every point whose number reached at least MinValue.
    function ToArray(const MinValue: Integer = 0): TPointArray;
  end;

  // A spatial hash that buckets point indices by grid cell (a distance-sized tile) for neighbourhood queries.
  // MoveTo(P, dx, dy) = head point (index) of P's cell offset by (dx,dy), or -1 if empty
  // Next[i] steps a cell's chain.
  TPointSpatialHash = record
  private
  type
    TSlot = record
      Key: Int64;
      Head: Integer;
    end;
  private
    FSlots: array of TSlot;
    FNext: array of Integer;
    FMask: Integer;
    FMinX, FMinY: Int64;
    FCellWidth, FCellHeight: Integer;
    FCurPX, FCurPY: Integer;
    FCurCX, FCurCY: Int64;

    function SlotOf(CX, CY: Int64): Integer; inline;
    function GetNext(Index: Integer): Integer; inline;
  public
    procedure Init(const Points: TPointArray; DistX, DistY: Single);
    function MoveTo(const P: TPoint; DX, DY: Integer): Integer;
    property Next[Index: Integer]: Integer read GetNext;
  end;

  function ShouldHashSet(const ABounds: TBox; PointCount: Integer): Boolean;

implementation

uses
  simba.math;

const
  MAX_SPACE = 128 * 1024 * 1024; // scanline is 1 bit/location, so this cap is ~16 MB of bits
  SPACE_PER_POINT = 128; // bit-packed scanline beats hashing up to ~1 point per 128 locations

// Fibonacci hash of a point (aka Int64)
{$PUSH}
{$Q-}{$R-}
function HashPoint(const Key: Int64): Integer; inline;
begin
  Result := Integer((UInt64(Key) * UInt64($9E3779B97F4A7C15)) shr 32);
end;
{$POP}

function ShouldHashSet(const ABounds: TBox; PointCount: Integer): Boolean;
var
  W, H, Space: Int64;
begin
  W := (Int64(ABounds.X2) - ABounds.X1) + 1;
  H := (Int64(ABounds.Y2) - ABounds.Y1) + 1;
  Space := W * H;

  // hash when a scanline would be too big to allocate or too sparse to be worth it
  Result := (W < 1) or (H < 1) or (Space > MAX_SPACE) or
            (Int64(PointCount) * SPACE_PER_POINT < Space);
end;

procedure TPointBuffer.Grow(const Len: Integer);
begin
  FLength := FLength + Len;
  if (FLength < 32) then
    FLength := 32
  else
  if (FLength > 256000) then
    FLength := FLength * 4
  else
    FLength := FLength * 2;

  SetLength(FArr, FLength);
end;

function TPointBuffer.GetItem(const Index: Integer): TPoint;
begin
  Result := FArr[Index];
end;

procedure TPointBuffer.Clear;
begin
  FCount := 0;
end;

procedure TPointBuffer.Init(const InitialSize: Integer);
begin
  FLength := InitialSize;
  FCount := 0;

  if (FLength > 0) then
    SetLength(FArr, FLength);
end;

procedure TPointBuffer.InitWith(const Values: TPointArray);
begin
  FArr := Values;
  FLength := Length(FArr);
  FCount := FLength;
end;

procedure TPointBuffer.Add(const Value: TPoint);
begin
  if (FCount >= FLength) then
    Grow();

  FArr[FCount] := Value;
  Inc(FCount);
end;

procedure TPointBuffer.Add(const X, Y: Integer);
begin
  if (FCount >= FLength) then
    Grow();

  FArr[FCount].X := X;
  FArr[FCount].Y := Y;
  Inc(FCount);
end;

procedure TPointBuffer.Add(const Values: TPointArray);
var
  Len: Integer;
begin
  Len := Length(Values);
  if (Len > 0) then
  begin
    if (FCount + Len >= FLength) then
      Grow(Len);
    Move(Values[0], FArr[FCount], Len * SizeOf(TPoint));
    Inc(FCount, Len);
  end;
end;

function TPointBuffer.First: TPoint;
begin
  Result := FArr[0];
end;

function TPointBuffer.Last: TPoint;
begin
  Result := FArr[FCount - 1];
end;

function TPointBuffer.Pop: TPoint;
begin
  Result := FArr[FCount - 1];
  Dec(FCount);
end;

function TPointBuffer.ToArray(Copy: Boolean): TPointArray;
begin
  if Copy then
    Result := System.Copy(FArr, 0, FCount)
  else
  begin
    FLength := FCount;
    SetLength(FArr, FLength);

    Result := FArr;
  end;
end;

class operator TPointBuffer.Initialize(var Self: TPointBuffer);
begin
  Self := Default(TPointBuffer);
end;

procedure TPointArrayBuffer.Grow(const Len: Integer);
begin
  FLength := FLength + Len;
  if (FLength < 32) then
    FLength := 32
  else
  if (FLength > 256000) then
    FLength := FLength * 4
  else
    FLength := FLength * 2;

  SetLength(FArr, FLength);
end;

function TPointArrayBuffer.GetItem(const Index: Integer): TPointArray;
begin
  Result := FArr[Index];
end;

procedure TPointArrayBuffer.Clear;
begin
  FCount := 0;
end;

procedure TPointArrayBuffer.Init(const InitialSize: Integer);
begin
  FLength := InitialSize;
  FCount := 0;

  if (FLength > 0) then
    SetLength(FArr, FLength);
end;

procedure TPointArrayBuffer.InitWith(const Values: T2DPointArray);
begin
  FArr := Values;
  FLength := Length(FArr);
  FCount := FLength;
end;

procedure TPointArrayBuffer.Add(const Value: TPointArray);
begin
  if (FCount >= FLength) then
    Grow();

  FArr[FCount] := Value;
  Inc(FCount);
end;

procedure TPointArrayBuffer.Add(const Values: T2DPointArray);
var
  Len, I: Integer;
begin
  Len := Length(Values);
  if (Len > 0) then
  begin
    if (FCount + Len >= FLength) then
      Grow(Len);
    for I := 0 to Len - 1 do // element-wise: TPointArray is managed, Move would skip refcounts
      FArr[FCount + I] := Values[I];
    Inc(FCount, Len);
  end;
end;

function TPointArrayBuffer.First: TPointArray;
begin
  Result := FArr[0];
end;

function TPointArrayBuffer.Last: TPointArray;
begin
  Result := FArr[FCount - 1];
end;

function TPointArrayBuffer.Pop: TPointArray;
begin
  Result := FArr[FCount - 1];
  Dec(FCount);
end;

function TPointArrayBuffer.ToArray(Copy: Boolean): T2DPointArray;
begin
  if Copy then
    Result := System.Copy(FArr, 0, FCount)
  else
  begin
    FLength := FCount;
    SetLength(FArr, FLength);

    Result := FArr;
  end;
end;

class operator TPointArrayBuffer.Initialize(var Self: TPointArrayBuffer);
begin
  Self := Default(TPointArrayBuffer);
end;

class operator TPointSet.Initialize(var Self: TPointSet);
begin
  Self := Default(TPointSet);
end;

procedure TPointSet.Init(const ABounds: TBox);
var
  Space: Int64;
begin
  FBounds := ABounds;
  FWidth := (ABounds.X2 - ABounds.X1) + 1;
  FHeight := (ABounds.Y2 - ABounds.Y1) + 1;
  Space := Int64(FWidth) * FHeight;

  FBits := nil;
  if (Space > 0) then
    SetLength(FBits, (Space + 63) shr 6);

  FCount := 0;
end;

// Location of P in the bounds, or -1 when outside
function TPointSet.IndexOf(const P: TPoint): Integer;
var
  X, Y: Integer;
begin
  X := P.X - FBounds.X1;
  Y := P.Y - FBounds.Y1;
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    Exit(-1);

  Result := (Y * FWidth) + X;
end;

function TPointSet.Add(const P: TPoint): Boolean;
var
  Index: Integer;
  Mask: UInt64;
begin
  Index := IndexOf(P);
  if (Index < 0) then Exit(False);

  Mask := UInt64(1) shl (Index and 63);
  Result := (FBits[Index shr 6] and Mask) = 0;
  if Result then
  begin
    FBits[Index shr 6] := FBits[Index shr 6] or Mask;
    Inc(FCount);
  end;
end;

function TPointSet.Contains(const P: TPoint): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(P);
  Result := (Index >= 0) and ((FBits[Index shr 6] and (UInt64(1) shl (Index and 63))) <> 0);
end;

function TPointSet.ToArray: TPointArray;
var
  W, Index, Taken: Integer;
  Bits: UInt64;
begin
  SetLength(Result, FCount);
  Taken := 0;

  for W := 0 to High(FBits) do   // skip 64 empty pixels at a time; BSF out the set ones
  begin
    Bits := FBits[W];
    while (Bits <> 0) do
    begin
      Index := (W shl 6) + BsfQWord(Bits);
      Bits := Bits and (Bits - 1);   // clear the lowest set bit
      Result[Taken].X := FBounds.X1 + (Index mod FWidth);
      Result[Taken].Y := FBounds.Y1 + (Index div FWidth);
      Inc(Taken);
    end;
  end;

  SetLength(Result, Taken);
end;

class operator TPointHashSet.Initialize(var Self: TPointHashSet);
begin
  Self := Default(TPointHashSet);
end;

procedure TPointHashSet.Init(ACapacity: Integer);
begin
  if (ACapacity < 4) then
    ACapacity := 4;

  FKeys := nil;
  SetLength(FKeys, NextPower2(ACapacity * 2));
  FHasZero := False;
  FCount := 0;
end;

function TPointHashSet.SlotOf(const Key: Int64): Integer;
var
  Mask: Integer;
begin
  Mask := High(FKeys);
  Result := HashPoint(Key) and Mask;
  while (FKeys[Result] <> 0) and (FKeys[Result] <> Key) do
    Result := (Result + 1) and Mask;
end;

procedure TPointHashSet.Grow;
var
  Old: array of Int64;
  I: Integer;
  HadZero: Boolean;
begin
  Old := FKeys;
  HadZero := FHasZero;
  Init(Length(Old)); // Init doubles (and resets FHasZero/FCount)

  for I := 0 to High(Old) do
    if (Old[I] <> 0) then
    begin
      FKeys[SlotOf(Old[I])] := Old[I];
      Inc(FCount);
    end;

  FHasZero := HadZero;
  if HadZero then Inc(FCount);
end;

function TPointHashSet.Add(const P: TPoint): Boolean;
var
  Key: Int64;
  Slot: Integer;
begin
  if (FKeys = nil) then
    Init();

  Key := Int64(P);
  if (Key = 0) then // (0,0) collides with the empty so track it in a flag
  begin
    Result := not FHasZero;
    if Result then
    begin
      FHasZero := True;
      Inc(FCount);
    end;
    Exit;
  end;

  Slot := SlotOf(Key);
  Result := (FKeys[Slot] = 0);
  if Result then
  begin
    FKeys[Slot] := Key;
    Inc(FCount);

    if (FCount * 2 > Length(FKeys)) then
      Grow();
  end;
end;

function TPointHashSet.Contains(const P: TPoint): Boolean;
var
  Key: Int64;
begin
  if (FKeys = nil) then
    Exit(False);

  Key := Int64(P);
  if (Key = 0) then
    Exit(FHasZero);

  Result := (FKeys[SlotOf(Key)] = Key);
end;

function TPointHashSet.ToArray: TPointArray;
var
  I, Taken: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;

  for I := 0 to High(FKeys) do
    if (FKeys[I] <> 0) then
    begin
      Result[Taken] := TPoint(FKeys[I]);
      Inc(Taken);
    end;

  if FHasZero then
  begin
    Result[Taken].X := 0;
    Result[Taken].Y := 0;
    Inc(Taken);
  end;

  SetLength(Result, Taken);
end;

class operator TPointTallySet.Initialize(var Self: TPointTallySet);
begin
  Self := Default(TPointTallySet);
end;

procedure TPointTallySet.Init(const ABounds: TBox);
var
  Space: Int64;
begin
  FBounds := ABounds;
  FWidth := (ABounds.X2 - ABounds.X1) + 1;
  FHeight := (ABounds.Y2 - ABounds.Y1) + 1;
  Space := Int64(FWidth) * FHeight;

  FCells := nil;
  if (Space > 0) then
    SetLength(FCells, Space);

  FCount := 0;
end;

function TPointTallySet.IndexOf(const P: TPoint): Integer;
var
  X, Y: Integer;
begin
  X := P.X - FBounds.X1;
  Y := P.Y - FBounds.Y1;
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    Exit(-1);

  Result := (Y * FWidth) + X;
end;

function TPointTallySet.Add(const P: TPoint): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(P);

  Result := (Index >= 0) and (FCells[Index] = 0);
  if Result then
  begin
    FCells[Index] := 1; // a count of zero
    Inc(FCount);
  end;
end;

procedure TPointTallySet.IncrementIf(const P: TPoint; IfValue: Integer);
var
  Index: Integer;
begin
  Index := IndexOf(P);
  if (Index >= 0) and (FCells[Index] = IfValue + 1) then // stored is the number plus one
    Inc(FCells[Index]);
end;

function TPointTallySet.ToArray(const MinValue: Integer): TPointArray;
var
  I, Taken, Least: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;
  Least := MinValue + 1;

  for I := 0 to High(FCells) do
    if (FCells[I] >= Least) then
    begin
      Result[Taken].X := FBounds.X1 + (I mod FWidth);
      Result[Taken].Y := FBounds.Y1 + (I div FWidth);
      Inc(Taken);
    end;

  SetLength(Result, Taken);
end;

class operator TPointTallyHashSet.Initialize(var Self: TPointTallyHashSet);
begin
  Self := Default(TPointTallyHashSet);
end;

procedure TPointTallyHashSet.Init(ACapacity: Integer);
begin
  if (ACapacity < 4) then
    ACapacity := 4;

  FSlots := nil;
  SetLength(FSlots, NextPower2(ACapacity * 2));

  FCount := 0;
end;

function TPointTallyHashSet.SlotOf(const Key: Int64): Integer;
var
  Mask: Integer;
begin
  Mask := High(FSlots);
  Result := HashPoint(Key) and Mask;
  while (FSlots[Result].Data <> 0) and (FSlots[Result].Key <> Key) do
    Result := (Result + 1) and Mask;
end;

procedure TPointTallyHashSet.Grow;
var
  Old: TSlots;
  I: Integer;
begin
  Old := FSlots;
  Init(Length(Old)); // Init doubles, so this grows to twice the size

  for I := 0 to High(Old) do
    if (Old[I].Data <> 0) then
    begin
      FSlots[SlotOf(Old[I].Key)] := Old[I];

      Inc(FCount);
    end;
end;

function TPointTallyHashSet.Add(const P: TPoint): Boolean;
var
  Key: Int64;
  Slot: Integer;
begin
  if (FSlots = nil) then
    Init();

  Key := Int64(P);
  Slot := SlotOf(Key);

  Result := (FSlots[Slot].Data = 0);
  if Result then
  begin
    FSlots[Slot].Key := Key;
    FSlots[Slot].Data := 1; // a count of zero
    Inc(FCount);
    if (FCount * 2 > Length(FSlots)) then
      Grow();
  end;
end;

procedure TPointTallyHashSet.IncrementIf(const P: TPoint; IfValue: Integer);
var
  Slot: Integer;
begin
  if (FSlots = nil) then
    Exit;

  Slot := SlotOf(Int64(P));
  if (FSlots[Slot].Data = IfValue + 1) then // stored is the number plus one
    Inc(FSlots[Slot].Data);
end;

function TPointTallyHashSet.ToArray(const MinValue: Integer): TPointArray;
var
  I, Taken, Least: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;
  Least := MinValue + 1;

  for I := 0 to High(FSlots) do
    if (FSlots[I].Data >= Least) then
    begin
      Result[Taken] := TPoint(FSlots[I].Key);
      Inc(Taken);
    end;

  SetLength(Result, Taken);
end;

function TPointSpatialHash.SlotOf(CX, CY: Int64): Integer;
var
  Key: Int64;
begin
  Key := (CX shl 32) or (CY and $FFFFFFFF);
  Result := HashPoint(Key) and FMask;
  while (FSlots[Result].Head >= 0) and (FSlots[Result].Key <> Key) do
    Result := (Result + 1) and FMask;
end;

procedure TPointSpatialHash.Init(const Points: TPointArray; DistX, DistY: Single);
var
  Cap, I, Slot: Integer;
  CX, CY: Int64;
begin
  if (Length(Points) = 0) then
    Exit;

  FCellWidth := Trunc(DistX) + 1;
  FCellHeight := Trunc(DistY) + 1;

  Cap := NextPower2(Length(Points) * 2);
  if (Cap < 8) then
    Cap := 8;
  FMask := Cap - 1;

  FSlots := nil;
  SetLength(FSlots, Cap);
  for I := 0 to Cap - 1 do
    FSlots[I].Head := -1;
  SetLength(FNext, Length(Points));

  FMinX := Points[0].X;
  FMinY := Points[0].Y;
  for I := 1 to High(Points) do
  begin
    if (Points[I].X < FMinX) then
      FMinX := Points[I].X;
    if (Points[I].Y < FMinY) then
      FMinY := Points[I].Y;
  end;

  FCurPX := Points[0].X;
  FCurPY := Points[0].Y;
  FCurCX := (Points[0].X - FMinX) div FCellWidth;
  FCurCY := (Points[0].Y - FMinY) div FCellHeight;

  for I := 0 to High(Points) do
  begin
    CX := (Points[I].X - FMinX) div FCellWidth;
    CY := (Points[I].Y - FMinY) div FCellHeight;
    Slot := SlotOf(CX, CY);
    FNext[I] := FSlots[Slot].Head;
    FSlots[Slot].Key := (CX shl 32) or (CY and $FFFFFFFF);
    FSlots[Slot].Head := I;
  end;
end;

function TPointSpatialHash.MoveTo(const P: TPoint; DX, DY: Integer): Integer;
begin
  if (P.X <> FCurPX) or (P.Y <> FCurPY) then // cache P's cell
  begin
    FCurCX := (P.X - FMinX) div FCellWidth;
    FCurCY := (P.Y - FMinY) div FCellHeight;
    FCurPX := P.X;
    FCurPY := P.Y;
  end;
  Result := FSlots[SlotOf(FCurCX + DX, FCurCY + DY)].Head;
end;

function TPointSpatialHash.GetNext(Index: Integer): Integer;
begin
  Result := FNext[Index];
end;

end.
