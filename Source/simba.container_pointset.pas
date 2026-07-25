{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Collections of points that count rather than store.

  TPointHashSet      - lightweight hashmap
  TPointScanLineSet  - flat array with points marked via scanline [Y*Width+X]

  CanScanlineSet returns if a scanline is worth it for a given bounds and point
  count - small enough to allocate and dense enough not to waste it.
}
unit simba.container_pointset;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TPointHashSetSlot = record
    Key: Int64;
    Data: Integer; // a zeroed slot is an empty one
  end;
  TPointHashSetSlots = array of TPointHashSetSlot;

  TPointHashSet = record
  private
    FSlots: TPointHashSetSlots;
    FCount: Integer;

    function SlotOf(const Key: Int64): Integer; inline;
    procedure Grow;
  public
    class operator Initialize(var Self: TPointHashSet);

    procedure Init(ACapacity: Integer = 64);

    // Adds P with a count of zero. False if it was already there.
    function Add(const P: TPoint): Boolean;

    // Mark(P, Group) gives P stamp group, but only if it already holds every earlier one.
    // Miss a group or repeat one and P can never catch up.
    function Mark(const P: TPoint; const Group: Integer = 0): Boolean; inline;
    procedure MarkAll(const Arr: TPointArray; Group: Integer = 0);

    // Every point counted in at least MinCount group
    function ToArray(const MinCount: Integer = 0): TPointArray;

    property Count: Integer read FCount;
  end;

  TPointScanLineSet = record
  private
    FSpace: TIntegerArray;
    FBounds: TBox;
    FWidth, FHeight: Integer;
    FCount: Integer;

    function IndexOf(const P: TPoint): Integer; inline;
  public
    class operator Initialize(var Self: TPointScanLineSet);

    procedure Init(const ABounds: TBox);
    // Adds P with a count of zero. False if it was already there.
    function Add(const P: TPoint): Boolean;
    // Mark(P, Group) gives P stamp group, but only if it already holds every earlier one.
    // Miss a group or repeat one and P can never catch up.
    function Mark(const P: TPoint; const Group: Integer = 0): Boolean; inline;
    procedure MarkAll(const Arr: TPointArray; Group: Integer = 0);
    // Every point counted in at least MinCount group
    function ToArray(const MinCount: Integer = 0): TPointArray;

    property Count: Integer read FCount;
  end;

  function CanScanlineSet(const ABounds: TBox; PointCount: Integer): Boolean;

implementation

uses
  simba.math;

const
  HASH_MULTIPLIER = UInt64($9E3779B97F4A7C15); // fibonacci multiplier
  // memory limit on the scanline
  MAX_SPACE = 16 * 1024 * 1024;
  // scanline beats hashing while it holds one point per ~8 locations or denser
  SPACE_PER_POINT = 8;

function CanScanlineSet(const ABounds: TBox; PointCount: Integer): Boolean;
var
  W, H: Integer;
  Space: Int64;
begin
  W := (ABounds.X2 - ABounds.X1) + 1;
  H := (ABounds.Y2 - ABounds.Y1) + 1;
  Space := Int64(W) * H;

  Result := (W >= 1) and (H >= 1) and (Space <= MAX_SPACE) and
            (Int64(PointCount) * SPACE_PER_POINT >= Space);
end;

class operator TPointHashSet.Initialize(var Self: TPointHashSet);
begin
  Self := Default(TPointHashSet);
end;

procedure TPointHashSet.Init(ACapacity: Integer);
begin
  if (ACapacity < 4) then
    ACapacity := 4;

  FSlots := nil; // start fresh, so a caller still holding the old table (Grow) doesn't get it copied
  SetLength(FSlots, NextPower2(ACapacity * 2));
  FillChar(FSlots[0], Length(FSlots) * SizeOf(TPointHashSetSlot), 0);

  FCount := 0;
end;

{$PUSH}
{$Q-}{$R-}
function TPointHashSet.SlotOf(const Key: Int64): Integer;
var
  Mask: Integer;
begin
  Mask := High(FSlots);
  Result := Integer((UInt64(Key) * HASH_MULTIPLIER) shr 32) and Mask;
  while (FSlots[Result].Data <> 0) and (FSlots[Result].Key <> Key) do
    Result := (Result + 1) and Mask;
end;
{$POP}

procedure TPointHashSet.Grow;
var
  Old: TPointHashSetSlots;
  I: Integer;
begin
  Old := FSlots;
  Init(Length(Old)); // Init doubles, so this grows to twice the size

  // add back the old items
  for I := 0 to High(Old) do
    if (Old[I].Data <> 0) then
    begin
      FSlots[SlotOf(Old[I].Key)] := Old[I];

      Inc(FCount);
    end;
end;

function TPointHashSet.Add(const P: TPoint): Boolean;
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

function TPointHashSet.Mark(const P: TPoint; const Group: Integer): Boolean;
var
  Slot: Integer;
begin
  if (FSlots = nil) then
    Exit(False);

  Slot := SlotOf(Int64(P));

  Result := (FSlots[Slot].Data = Group + 1);
  if Result then
    FSlots[Slot].Data := Group + 2;
end;

procedure TPointHashSet.MarkAll(const Arr: TPointArray; Group: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    Mark(Arr[I], Group);
end;

function TPointHashSet.ToArray(const MinCount: Integer): TPointArray;
var
  I, Taken, Least: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;
  Least := MinCount + 1;

  for I := 0 to High(FSlots) do
    if (FSlots[I].Data >= Least) then
    begin
      Result[Taken] := TPoint(FSlots[I].Key);
      Inc(Taken);
    end;

  SetLength(Result, Taken);
end;

class operator TPointScanLineSet.Initialize(var Self: TPointScanLineSet);
begin
  Self := Default(TPointScanLineSet);
end;

procedure TPointScanLineSet.Init(const ABounds: TBox);
var
  Space: Int64;
begin
  FBounds := ABounds;
  FWidth := (ABounds.X2 - ABounds.X1) + 1;
  FHeight := (ABounds.Y2 - ABounds.Y1) + 1;
  Space := Int64(FWidth) * FHeight;

  FSpace := nil;
  if (Space > 0) then
  begin
    SetLength(FSpace, Space);
    FillChar(FSpace[0], Space * SizeOf(Integer), 0);
  end;

  FCount := 0;
end;

// Location of P in the space or -1 if outside bounds
function TPointScanLineSet.IndexOf(const P: TPoint): Integer;
var
  X, Y: Integer;
begin
  X := P.X - FBounds.X1;
  Y := P.Y - FBounds.Y1;
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    Exit(-1);

  Result := (Y * FWidth) + X;
end;

function TPointScanLineSet.Add(const P: TPoint): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(P);

  Result := (Index >= 0) and (FSpace[Index] = 0);
  if Result then
  begin
    FSpace[Index] := 1; // a count of zero
    Inc(FCount);
  end;
end;

function TPointScanLineSet.Mark(const P: TPoint; const Group: Integer): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(P);

  Result := (Index >= 0) and (FSpace[Index] = Group + 1);
  if Result then
    FSpace[Index] := Group + 2;
end;

procedure TPointScanLineSet.MarkAll(const Arr: TPointArray; Group: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    Mark(Arr[I], Group);
end;

function TPointScanLineSet.ToArray(const MinCount: Integer): TPointArray;
var
  I, Taken, Least: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;

  Least := MinCount + 1;

  for I := 0 to High(FSpace) do
    if (FSpace[I] >= Least) then
    begin
      Result[Taken].X := FBounds.X1 + (I mod FWidth);
      Result[Taken].Y := FBounds.Y1 + (I div FWidth);
      Inc(Taken);
    end;

  SetLength(Result, Taken);
end;

end.
