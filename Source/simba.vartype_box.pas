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

  operator = (const Left, Right: TBox): Boolean;

implementation

uses
  Math,
  simba.random, simba.containers, simba.geometry;

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

end.

