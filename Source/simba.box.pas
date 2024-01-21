{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  TBox methods.
}
unit simba.box;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TBoxHelper = record Helper(TBoxHelperBase) for TBox
  public
    function RandomPoint: TPoint;
    function RandomPointCenter: TPoint;

    function ToQuad: TQuad;
    function EqualDimensions(Other: TBox): Boolean;
    function Area: Integer;
    function Expand(SizeMod: Integer): TBox; overload;
    function Expand(SizeMod: Integer; MaxBounds: TBox): TBox; overload;
    function Expand(WidMod, HeiMod: Integer): TBox; overload;
    function Expand(WidMod, HeiMod: Integer; MaxBounds: TBox): TBox; overload;
    function Contains(Other: TBox): Boolean; overload; inline;
    function Contains(Other: TPoint): Boolean; overload; inline;
    function Contains(X, Y: Integer): Boolean; overload; inline;
    function Contains(Other: TQuad): Boolean; overload; inline;

    function Offset(X, Y: Integer): TBox; overload;
    function Offset(P: TPoint): TBox; overload;
    function Combine(Other: TBox): TBox;
    function Invert(AArea: TBox): TBoxArray;
    function Partition(Rows, Cols: Integer): TBoxArray;
    function Extract(Points: TPointArray): TPointArray;
    function Exclude(Points: TPointArray): TPointArray;

    function NearestEdge(P: TPoint): TPoint;
    function Intersect(P: TPoint): TPoint;
    function Corners: TPointArray;

    function Clip(Other: TBox): TBox;
    function Normalize: TBox;
  end;

implementation

uses
  Math,
  simba.random, simba.arraybuffer, simba.geometry;

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

function TBoxHelper.Area: Integer;
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

function TBoxHelper.Contains(X, Y: Integer): Boolean;
begin
  Result := (X >= Self.X1) and (Y >= Self.Y1) and (X <= Self.X2) and (Y <= Self.Y2);
end;

function TBoxHelper.Contains(Other: TQuad): Boolean;
begin
  Result := Contains(Other.Left) and Contains(Other.Right) and Contains(Other.Top) and Contains(Other.Bottom);
end;

function TBoxHelper.Offset(X, Y: Integer): TBox;
begin
  Result.X1 := Self.X1 + X;
  Result.Y1 := Self.Y1 + Y;
  Result.X2 := Self.X2 + X;
  Result.Y2 := Self.Y2 + Y;
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

function TBoxHelper.Invert(AArea: TBox): TBoxArray;
var
  lowX, maxX, lowY, maxY: Integer;
  I: Integer;
begin
  lowX := Max(AArea.X1, Self.X1-1);
  maxX := Min(AArea.X2, Self.X2+1);
  lowY := Max(AArea.Y1, Self.Y1-1);
  maxY := Min(AArea.Y2, Self.Y2+1);

  Result := [
    TBox.Create(AArea.X1, AArea.Y1, lowX,     lowY),
    TBox.Create(AArea.X1, lowY,     lowX,     maxY),
    TBox.Create(AArea.X1, lowY,     lowX,     AArea.Y2),
    TBox.Create(lowX,     AArea.Y1, maxX,     lowY),
    TBox.Create(lowX,     maxY,     maxX,     AArea.Y2),
    TBox.Create(maxX,     AArea.Y1, AArea.X2, lowY),
    TBox.Create(maxX,     lowY,     AArea.X2, maxY),
    TBox.Create(maxX,     lowY,     AArea.X2, AArea.Y2)
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

end.

