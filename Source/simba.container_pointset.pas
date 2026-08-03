{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  "TPointSet" and "TPointHashSet" is presence only (exists or doesnt).
  "TPointTallySet" and "TPointTallyHashSet" stores a additional integer per point.

  Algorithmically the differences are:
    - "TPointSet" and "TPointTallySet" uses scanline for extremely fast lookup
      however needs allocate an array the size of entire point bounds.
    - "TPointHashSet" and "TPointTallyHashSet" uses a lightweight dictonary.
}
unit simba.container_pointset;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  // Presence only: uses scanline
  TPointSet = record
  private
    FCells: TByteArray; // 0 = empty, 1 = present
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
  type
    TSlot = record
      Key: Int64;
      Used: Boolean;
    end;
    TSlots = array of TSlot;
  private
    FSlots: TSlots;
    FCount: Integer;

    function SlotOf(const Key: Int64): Integer; inline;
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

  function ShouldHashSet(const ABounds: TBox; PointCount: Integer): Boolean;

implementation

uses
  simba.math;

const
  MAX_SPACE = 16 * 1024 * 1024; // hard memory limit on a scanline
  SPACE_PER_POINT = 8; // a scanline beats hashing while it holds one point per ~8 locations or denser

// Fibonacci hash of a point (aka Int64)
{$PUSH}{$Q-}{$R-}
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

  FCells := nil;
  if (Space > 0) then
    SetLength(FCells, Space);

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
begin
  Index := IndexOf(P);

  Result := (Index >= 0) and (FCells[Index] = 0);
  if Result then
  begin
    FCells[Index] := 1;
    Inc(FCount);
  end;
end;

function TPointSet.Contains(const P: TPoint): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(P);
  Result := (Index >= 0) and (FCells[Index] <> 0);
end;

function TPointSet.ToArray: TPointArray;
var
  I, Taken: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;

  for I := 0 to High(FCells) do
    if (FCells[I] <> 0) then
    begin
      Result[Taken].X := FBounds.X1 + (I mod FWidth);
      Result[Taken].Y := FBounds.Y1 + (I div FWidth);
      Inc(Taken);
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

  FSlots := nil; 
  SetLength(FSlots, NextPower2(ACapacity * 2));

  FCount := 0;
end;

function TPointHashSet.SlotOf(const Key: Int64): Integer;
var
  Mask: Integer;
begin
  Mask := High(FSlots);
  Result := HashPoint(Key) and Mask;
  while FSlots[Result].Used and (FSlots[Result].Key <> Key) do
    Result := (Result + 1) and Mask;
end;

procedure TPointHashSet.Grow;
var
  Old: TSlots;
  I: Integer;
begin
  Old := FSlots;
  Init(Length(Old)); // Init doubles

  for I := 0 to High(Old) do
    if Old[I].Used then
    begin
      FSlots[SlotOf(Old[I].Key)] := Old[I];
      Inc(FCount);
    end;
end;

function TPointHashSet.Add(const P: TPoint): Boolean;
var
  Slot: Integer;
begin
  if (FSlots = nil) then
    Init();

  Slot := SlotOf(Int64(P));
  Result := not FSlots[Slot].Used;
  if Result then
  begin
    FSlots[Slot].Key := Int64(P);
    FSlots[Slot].Used := True;
    Inc(FCount);

    if (FCount * 2 > Length(FSlots)) then
      Grow();
  end;
end;

function TPointHashSet.Contains(const P: TPoint): Boolean;
begin
  Result := (FSlots <> nil) and FSlots[SlotOf(Int64(P))].Used;
end;

function TPointHashSet.ToArray: TPointArray;
var
  I, Taken: Integer;
begin
  SetLength(Result, FCount);
  Taken := 0;

  for I := 0 to High(FSlots) do
    if FSlots[I].Used then
    begin
      Result[Taken] := TPoint(FSlots[I].Key);
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

end.
