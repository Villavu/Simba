{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  TBox methods.
}
unit simba.vartype_box;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TBoxHelper = record helper for TBox
  private
    function GetTopRight: TPoint; inline;
    function GetBottomLeft: TPoint; inline;
    function GetCenter: TPoint; inline;
    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;
    function GetSize: TSize; inline;
    function GetArea: Int64; inline;
  public
    const ZERO: TBox = (X1: 0; Y1: 0; X2: 0; Y2: 0);

    class function Create(const X1, Y1, X2, Y2: Integer): TBox; static; overload;
    class function Create(const Center: TPoint; const XRad, YRad: Integer): TBox; static; overload;

    function RandomPoint: TPoint;
    function RandomPointCenter: TPoint;

    function ToQuad: TQuad;
    function EqualDimensions(Other: TBox): Boolean;
    function Expand(SizeMod: Integer): TBox; overload;
    function Expand(SizeMod: Integer; MaxBounds: TBox): TBox; overload;
    function Expand(WidMod, HeiMod: Integer): TBox; overload;
    function Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox; overload;
    function Contains(Other: TBox): Boolean; overload; inline;
    function Contains(Other: TPoint): Boolean; overload; inline;
    function Contains(Other: TQuad): Boolean; overload; inline;

    function Offset(P: TPoint): TBox;
    function Combine(Other: TBox): TBox;
    function Invert(Space: TBox): TBoxArray;
    function Partition(Rows, Cols: Integer): TBoxArray;
    function Extract(Points: TPointArray): TPointArray;
    function Exclude(Points: TPointArray): TPointArray;

    function NearestEdge(P: TPoint): TPoint;
    function Intersect(P: TPoint): TPoint;
    function Corners: TPointArray;

    function Clip(Other: TBox): TBox;
    function Normalize: TBox;

    property TopRight: TPoint read GetTopRight;
    property BottomLeft: TPoint read GetBottomLeft;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Center: TPoint read GetCenter;
    property Area: Int64 read GetArea;
    property Size: TSize read GetSize;
  end;

  TBoxArrayHelper = type helper for TBoxArray
  public
    class function Create(Start: TPoint; Columns, Rows, Width, Height: Int32; Spacing: TPoint): TBoxArray; static;

    function Difference(Other: TBoxArray): TBoxArray;
    function SymmetricDifference(Other: TBoxArray): TBoxArray;
    function Intersection(Other: TBoxArray): TBoxArray;
    function Unique: TBoxArray;
    function Pack: TBoxArray;

    function Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TBoxArray; overload;
    function Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TBoxArray; overload;
    function SortFrom(From: TPoint): TBoxArray;
    function SortByX(LowToHigh: Boolean): TBoxArray;
    function SortByY(LowToHigh: Boolean): TBoxArray;
    function SortByWidth(LowToHigh: Boolean): TBoxArray;
    function SortByHeight(LowToHigh: Boolean): TBoxArray;
    function SortByArea(LowToHigh: Boolean): TBoxArray;

    function ContainsPoint(P: TPoint; out Index: Integer): Boolean; overload;
    function ContainsPoint(P: TPoint): Boolean; overload;

    function Merge: TBox;
    function Centers: TPointArray;
    function Offset(P: TPoint): TBoxArray;

    function Expand(SizeMod: Integer): TBoxArray; overload;
    function Expand(WidMod, HeiMod: Integer): TBoxArray; overload;
  end;

  operator = (const Left, Right: TBox): Boolean;

implementation

uses
  Math,
  simba.random, simba.containers, simba.geometry, simba.math, simba.array_algorithm;

function TBoxHelper.GetTopRight: TPoint;
begin
  Result.X := X2;
  Result.Y := Y1;
end;

function TBoxHelper.GetBottomLeft: TPoint;
begin
  Result.X := X1;
  Result.Y := Y2;
end;

function TBoxHelper.GetCenter: TPoint;
begin
  Result.X := (Self.X2 + Self.X1 + 1) div 2;
  Result.Y := (Self.Y2 + Self.Y1 + 1) div 2;
end;

function TBoxHelper.GetWidth: Integer;
begin
  Result := (Self.X2 - Self.X1) + 1;
end;

function TBoxHelper.GetHeight: Integer;
begin
  Result := (Self.Y2 - Self.Y1) + 1;
end;

function TBoxHelper.GetSize: TSize;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

class function TBoxHelper.Create(const X1, Y1, X2, Y2: Integer): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

class function TBoxHelper.Create(const Center: TPoint; const XRad, YRad: Integer): TBox;
begin
  Result.X1 := Center.X - XRad;
  Result.Y1 := Center.Y - YRad;
  Result.X2 := Center.X + XRad;
  Result.Y2 := Center.Y + YRad;
end;

function TBoxHelper.RandomPoint: TPoint;
begin
  Result.X := RandomRange(Self.X1, Self.X2);
  Result.Y := RandomRange(Self.Y1, Self.Y2);
end;

function TBoxHelper.RandomPointCenter: TPoint;
begin
  Result.X := RandomMean(Self.X1, Self.X2);
  Result.Y := RandomMean(Self.Y1, Self.Y2);
end;

function TBoxHelper.ToQuad: TQuad;
begin
  Result.Top    := TPoint.Create(Self.X1, Self.Y1);
  Result.Right  := TPoint.Create(Self.X2, Self.Y1);
  Result.Bottom := TPoint.Create(Self.X2, Self.Y2);
  Result.Left   := TPoint.Create(Self.X1, Self.Y2);
end;

function TBoxHelper.EqualDimensions(Other: TBox): Boolean;
begin
  Result := (Self.Width = Other.Width) and (Self.Height = Other.Height);
end;

function TBoxHelper.GetArea: Int64;
begin
  Result := (Width * Height);
end;

function TBoxHelper.Expand(SizeMod: Integer): TBox;
begin
  Result.X1 := Self.X1 - SizeMod;
  Result.Y1 := Self.Y1 - SizeMod;
  Result.X2 := Self.X2 + SizeMod;
  Result.Y2 := Self.Y2 + SizeMod;
end;

function TBoxHelper.Expand(SizeMod: Integer; MaxBounds: TBox): TBox;
begin
  Result := Self.Expand(SizeMod);
  Result.Clip(MaxBounds);
end;

function TBoxHelper.Expand(WidMod, HeiMod: Integer): TBox;
begin
  Result.X1 := Self.X1 - WidMod;
  Result.Y1 := Self.Y1 - HeiMod;
  Result.X2 := Self.X2 + WidMod;
  Result.Y2 := Self.Y2 + HeiMod;
end;

function TBoxHelper.Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox;
begin
  Result := Self.Expand(WidMod, HeiMod);
  Result.Clip(MaxBounds);
end;

function TBoxHelper.Contains(Other: TBox): Boolean;
begin
  Result := (Other.X1 >= Self.X1) and (Other.Y1 >= Self.Y1) and (Other.X2 <= Self.X2) and (Other.Y2 <= Self.Y2);
end;

function TBoxHelper.Contains(Other: TPoint): Boolean;
begin
  Result := (Other.X >= Self.X1) and (Other.Y >= Self.Y1) and (Other.X <= Self.X2) and (Other.Y <= Self.Y2);
end;

function TBoxHelper.Contains(Other: TQuad): Boolean;
begin
  Result := Contains(Other.Left) and Contains(Other.Right) and Contains(Other.Top) and Contains(Other.Bottom);
end;

function TBoxHelper.Offset(P: TPoint): TBox;
begin
  Result.X1 := Self.X1 + P.X;
  Result.Y1 := Self.Y1 + P.Y;
  Result.X2 := Self.X2 + P.X;
  Result.Y2 := Self.Y2 + P.Y;
end;

function TBoxHelper.Combine(Other: TBox): TBox;
begin
  Result.X1 := Min(Min(Self.X1, Other.X2), Min(Other.X1, Self.X2));
  Result.Y1 := Min(Min(Self.Y1, Other.Y2), Min(Other.Y1, Self.Y2));
  Result.X2 := Max(Max(Self.X1, Other.X2), Max(Other.X1, Self.X2));
  Result.Y2 := Max(Max(Self.Y1, Other.Y2), Max(Other.Y1, Self.Y2));
end;

function TBoxHelper.Invert(Space: TBox): TBoxArray;
var
  MinX, MaxX, LowY, MaxY: Integer;
  I: Integer;
begin
  MinX := Max(Space.X1, Self.X1-1);
  MaxX := Min(Space.X2, Self.X2+1);
  LowY := Max(Space.Y1, Self.Y1-1);
  MaxY := Min(Space.Y2, Self.Y2+1);

  Result := [
    TBox.Create(Space.X1, Space.Y1, MinX,     LowY),
    TBox.Create(Space.X1, LowY,     MinX,     MaxY),
    TBox.Create(Space.X1, LowY,     MinX,     Space.Y2),
    TBox.Create(MinX,     Space.Y1, MaxX,     LowY),
    TBox.Create(MinX,     MaxY,     MaxX,     Space.Y2),
    TBox.Create(MaxX,     Space.Y1, Space.X2, LowY),
    TBox.Create(MaxX,     LowY,     Space.X2, MaxY),
    TBox.Create(MaxX,     LowY,     Space.X2, Space.Y2)
  ];

  for I := High(Result) downto 0 do
    if (Result[I].X2 <= Result[I].X1) or (Result[I].Y2 <= Result[I].Y1) then
      Delete(Result, I, 1);
end;

function TBoxHelper.Partition(Rows, Cols: Integer): TBoxArray;
var
  idx,x,y: Integer;
  BoxW, BoxH: Single;
begin
  SetLength(Result, Cols * Rows);
  BoxW := (Self.X2 - Self.X1 + 1) / Cols;
  BoxH := (Self.Y2 - Self.Y1 + 1) / Rows;
  for y:=0 to Rows-1 do
    for x:=0 to Cols-1 do
    begin
      idx := (Y * Cols) + X;
      Result[idx].X1 := Ceil(Self.X1 + (BoxW * x));
      Result[idx].Y1 := Ceil(Self.Y1 + (BoxH * y));
      Result[idx].X2 := Trunc(Self.X1 + (BoxW * x) + BoxW-1);
      Result[idx].Y2 := Trunc(Self.Y1 + (BoxH * y) + BoxH-1);
    end;
end;

function TBoxHelper.Extract(Points: TPointArray): TPointArray;
var
  I: Integer;
  Buffer: TSimbaPointBuffer;
begin
  Buffer.Init(Length(Points));
  for I := 0 to High(Points) do
    if Contains(Points[I]) then
      Buffer.Add(Points[I]);

  Result := Buffer.ToArray(False);
end;

function TBoxHelper.Exclude(Points: TPointArray): TPointArray;
var
  I: Integer;
  Buffer: TSimbaPointBuffer;
begin
  Buffer.Init(Length(Points));
  for I := 0 to High(Points) do
    if not Contains(Points[I]) then
      Buffer.Add(Points[I]);

  Result := Buffer.ToArray(False);
end;

function TBoxHelper.NearestEdge(P: TPoint): TPoint;
begin
  Result := P;

  if Self.Contains(Result) then
  begin
    if Min(Abs(Self.Y1 - P.Y), Abs(P.Y - Self.Y2)) > Min(Abs(Self.X1 - P.X), Abs(P.X - Self.X2)) then
    begin
      Result.X := Self.X1;
      if (P.X - Self.X1 > Self.X2 - P.X) then
        Result.X := Self.X2;
    end else
    begin
      Result.Y := Self.Y1;
      if (P.Y - Self.Y1 > Self.Y2 - P.Y) then
        Result.Y := Self.Y2;
    end;
  end else
  begin
    if (Result.X < Self.X1) then Result.X := Self.X1;
    if (Result.X > Self.X2) then Result.X := Self.X2;
    if (Result.Y < Self.Y1) then Result.Y := Self.Y1;
    if (Result.Y > Self.Y2) then Result.Y := Self.Y2;
  end;
end;

function TBoxHelper.Intersect(P: TPoint): TPoint;
var
  i: TPoint;
begin
  Result := P;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X1, Self.Y1), TPoint.Create(Self.X2, Self.Y1), i) then Result := i;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X2, Self.Y1), TPoint.Create(Self.X2, Self.Y2), i) then Result := i;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X2, Self.Y2), TPoint.Create(Self.X1, Self.Y2), i) then Result := i;
  if TSimbaGeometry.LinesIntersect(p, Self.Center, TPoint.Create(Self.X1, Self.Y2), TPoint.Create(Self.X1, Self.Y1), i) then Result := i;
end;

function TBoxHelper.Corners: TPointArray;
begin
  Result := [TPoint.Create(X1, Y1), TPoint.Create(X2, Y1), TPoint.Create(X2, Y2), TPoint.Create(X1, Y2)];
end;

function TBoxHelper.Clip(Other: TBox): TBox;
begin
  Result := Self;

  if (Result.X1 < Other.X1) then Result.X1 := Other.X1;
  if (Result.X1 > Other.X2) then Result.X1 := Other.X2;
  if (Result.X2 < Other.X1) then Result.X2 := Other.X1;
  if (Result.X2 > Other.X2) then Result.X2 := Other.X2;

  if (Result.Y1 < Other.Y1) then Result.Y1 := Other.Y1;
  if (Result.Y1 > Other.Y2) then Result.Y1 := Other.Y2;
  if (Result.Y2 < Other.Y1) then Result.Y2 := Other.Y1;
  if (Result.Y2 > Other.Y2) then Result.Y2 := Other.Y2;
end;

function TBoxHelper.Normalize: TBox;
begin
  Result := Self;

  if (Result.X1 > Result.X2) then
    Swap(Result.X1, Result.X2);
  if (Result.Y1 > Result.Y2) then
    Swap(Result.Y1, Result.Y2);
end;

operator = (const Left, Right: TBox): Boolean;
begin
  Result := (Int64(Left.TopLeft)     = Int64(Right.TopLeft)) and
            (Int64(Left.BottomRight) = Int64(Right.BottomRight));
end;

class function TBoxArrayHelper.Create(Start: TPoint; Columns, Rows, Width, Height: Int32; Spacing: TPoint): TBoxArray;
var
  I: Int32;
begin
  Start.X += (Width div 2);
  Start.Y += (Height div 2);

  Spacing.X += Width;
  Spacing.Y += Height;

  SetLength(Result, Columns * Rows);
  for I := 0 to High(Result) do
  begin
    Result[I].X1 := Start.X + I mod Columns * Spacing.X - Width div 2;
    Result[I].Y1 := Start.Y + I div Columns * Spacing.Y - Height div 2;
    Result[I].X2 := Result[I].X1 + Width;
    Result[I].Y2 := Result[I].Y1 + Height;
  end;
end;

function TBoxArrayHelper.Difference(Other: TBoxArray): TBoxArray;
begin
  Result := specialize TArrayRelationship<TBox>.Difference(Self, Other);
end;

function TBoxArrayHelper.SymmetricDifference(Other: TBoxArray): TBoxArray;
begin
  Result := specialize TArrayRelationship<TBox>.SymmetricDifference(Self, Other);
end;

function TBoxArrayHelper.Intersection(Other: TBoxArray): TBoxArray;
begin
  Result := specialize TArrayRelationship<TBox>.Intersection(Self, Other);
end;

function TBoxArrayHelper.Unique: TBoxArray;
begin
  Result := specialize TArrayUnique<TBox>.Unique(Self)
end;

function TBoxArrayHelper.SortFrom(From: TPoint): TBoxArray;
var
  Weights: TDoubleArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Distance(From, Self[I].Center);

  Result := Self.Sort(Weights);
end;

function TBoxArrayHelper.SortByX(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Center.X;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByY(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Center.Y;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByWidth(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Width;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByHeight(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Height;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByArea(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Area;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.ContainsPoint(P: TPoint; out Index: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Self) do
    if Self[I].Contains(P) then
    begin
      Index := I;

      Exit(True);
    end;

  Result := False;
end;

function TBoxArrayHelper.ContainsPoint(P: TPoint): Boolean;
var
  Index: Integer;
begin
  Result := ContainsPoint(P, Index);
end;

function TBoxArrayHelper.Merge: TBox;
var
  I: Integer;
begin
  if (Length(Self) = 0) then
    Exit(TBox.ZERO);

  Result := Self[0];
  for I := 1 to High(Self) do
    Result := Result.Combine(Self[I]);
end;

function TBoxArrayHelper.Centers: TPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Center;
end;

function TBoxArrayHelper.Offset(P: TPoint): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Offset(P);
end;

function TBoxArrayHelper.Expand(SizeMod: Integer): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Expand(SizeMod);
end;

function TBoxArrayHelper.Expand(WidMod, HeiMod: Integer): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Expand(WidMod, HeiMod);
end;

// https://github.com/mapbox/potpack
function TBoxArrayHelper.Pack: TBoxArray;
type
  TBlock = record X,Y,W,H: Integer; Index: Integer; end;
  TBlockArray = array of TBlock;

  function Block(X,Y,W,H: Integer; Index: Integer = -1): TBlock;
  begin
    Result.X := X;
    Result.Y := Y;
    Result.W := W;
    Result.H := H;
    Result.Index := Index;
  end;

var
  Weights: TIntegerArray;
  I, J: Integer;
  StartWidth, Area, MaxWidth: Integer;
  Width, Height: Integer;
  Blocks, Spaces: TBlockArray;
  Len: Integer;
begin
  Len := Length(Self);
  if (Len = 0) then
    Exit(Default(TBoxArray));

  Area := 0;
  MaxWidth := 0;

  SetLength(Result, Len);
  SetLength(Blocks, Len);
  SetLength(Weights, Len);

  for I := 0 to Len - 1 do
  begin
    Blocks[I] := Block(0, 0, Self[I].Width - 1, Self[I].Height - 1, I);
    Weights[I] := Blocks[I].H;

    Area += Blocks[I].W * Blocks[I].H;
    MaxWidth := Max(MaxWidth, Blocks[I].W);
  end;

  specialize TArraySortWeighted<TBlock, Integer>.QuickSort(Blocks, Weights, Low(Blocks), High(Blocks), False);

  StartWidth := Max(Ceil(Sqrt(Area / 0.95)), MaxWidth);
  Spaces := [Block(0, 0, StartWidth, $FFFFFF)];

  Width := 0;
  Height := 0;

  for I := 0 to Len - 1 do
    for J := High(Spaces) downto 0 do
    begin
      if (Blocks[I].W > Spaces[J].W) or (Blocks[I].H > Spaces[J].H) then
        Continue;

      Blocks[I].X := Spaces[J].X;
      Blocks[I].Y := Spaces[J].Y;

      Width  := Max(Width, Blocks[I].X + Blocks[I].W);
      Height := Max(Height, Blocks[I].Y + Blocks[I].H);

      if (Blocks[I].W = Spaces[J].W) and (Blocks[I].H = Spaces[J].H) then
        Delete(Spaces, J, 1)
      else
      if (Blocks[I].H = Spaces[J].H) then
      begin
        Spaces[J].X += Blocks[I].W;
        Spaces[J].W -= Blocks[I].W;
      end else
      if (Blocks[I].W = Spaces[J].W) then
      begin
        Spaces[J].Y += Blocks[I].H;
        Spaces[J].H -= Blocks[I].H;
      end else
      begin
        Spaces += [Block(
          Spaces[J].X + Blocks[I].W,
          Spaces[J].Y,
          Spaces[J].W - Blocks[I].W,
          Blocks[I].H
        )];
        Spaces[J].Y += Blocks[I].H;
        Spaces[J].H -= Blocks[I].H;
      end;

      Break;
    end;

  for I := 0 to Len - 1 do
    Result[Blocks[I].Index] := TBox.Create(Blocks[I].X, Blocks[I].Y, Blocks[I].X + Blocks[I].W, Blocks[I].Y + Blocks[I].H);
end;

function TBoxArrayHelper.Sort(Weights: TIntegerArray; LowToHigh: Boolean): TBoxArray;
begin
  Result := Copy(Self);
  Weights := Copy(Weights);

  specialize TArraySortWeighted<TBox, Integer>.QuickSort(Result, Weights, Low(Result), High(Result), LowToHigh);
end;

function TBoxArrayHelper.Sort(Weights: TDoubleArray; LowToHigh: Boolean): TBoxArray;
begin
  Result := Copy(Self);
  Weights := Copy(Weights);

  specialize TArraySortWeighted<TBox, Double>.QuickSort(Result, Weights, Low(Result), High(Result), LowToHigh);
end;

end.

