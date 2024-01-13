{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.quad;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TQuadHelper = type helper for TQuad
  const
    EMPTY: TQuad = (Top: (X:0; Y:0); Right: (X:0; Y:0); Bottom: (X:0; Y:0); Left: (X:0; Y:0));
  public
    class function Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad; static; overload;
    class function CreateFromBox(Box: TBox): TQuad; static; overload;
    class function CreateFromPoints(Points: TPointArray): TQuad; static; overload;

    function RandomPoint: TPoint;
    function RandomPointCenter: TPoint;

    function ToTPA: TPointArray;
    function Bounds: TBox;
    function ShortSideLen: Integer;
    function LongSideLen: Integer;
    function Mean: TPoint;
    function Rotate(Radians: Double): TQuad;
    function Contains(P: TPoint): Boolean; overload; inline;
    function Contains(X, Y: Integer): Boolean; overload; inline;
    function Offset(P: TPoint): TQuad; overload;
    function Offset(X, Y: Integer): TQuad; overload;
    function Extract(Points: TPointArray): TPointArray;
    function Exclude(Points: TPointArray): TPointArray;
    function Expand(Amount: Double): TQuad;
    function NearestEdge(P: TPoint): TPoint;
    function Area: Integer;
    function Normalize: TQuad;
  end;

  operator in(const P: TPoint; const Quad: TQuad): Boolean;

implementation

uses
  Math,
  simba.math, simba.array_point, simba.random, simba.geometry, simba.arraybuffer;

class function TQuadHelper.Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad;
begin
  Result.Top    := ATop;
  Result.Right  := ARight;
  Result.Bottom := ABottom;
  Result.Left   := ALeft;
end;

class function TQuadHelper.CreateFromBox(Box: TBox): TQuad;
begin
  Result.Top    := TPoint.Create(Box.X1, Box.Y1);
  Result.Right  := TPoint.Create(Box.X2, Box.Y1);
  Result.Bottom := TPoint.Create(Box.X2, Box.Y2);
  Result.Left   := TPoint.Create(Box.X1, Box.Y2);
end;

class function TQuadHelper.CreateFromPoints(Points: TPointArray): TQuad;
begin
  Result := Points.MinAreaRect();
end;

function TQuadHelper.RandomPoint: TPoint;
var
  a,x,y,x1,y1,x2,y2: Double;
begin
  a := ArcTan2(Left.Y-Top.Y, Left.X-Top.X);
  X := (Top.X + Right.X + Bottom.X + Left.X) / 4;
  Y := (Top.Y + Right.Y + Bottom.Y + Left.Y) / 4;
  x1 := x-Hypot(Left.y-Top.y, Left.x-Top.x) / 2;
  y1 := y-Hypot(Left.y-Bottom.y, Left.x-Bottom.x) / 2;
  x2 := x+Hypot(Left.y-Top.y, Left.x-Top.x) / 2;
  y2 := y+Hypot(Left.y-Bottom.y, Left.x-Bottom.x) / 2;

  X := (X2 + X1) / 2 + (Random()*2 - 1);
  Y := (Y2 + Y1) / 2 + (Random()*2 - 1);

  Result.X := RandomRange(Round(x1)+2, Round(x2)-2);
  Result.Y := RandomRange(Round(y1)+2, Round(y2)-2);
  Result := Result.Rotate(a, TPoint.Create(Round(X), Round(Y)));
end;

function TQuadHelper.RandomPointCenter: TPoint;
var
  a,x,y,x1,y1,x2,y2: Double;
begin
  a := ArcTan2(Left.Y-Top.Y, Left.X-Top.X);
  X := (Top.X + Right.X + Bottom.X + Left.X) / 4;
  Y := (Top.Y + Right.Y + Bottom.Y + Left.Y) / 4;
  x1 := x-Hypot(Left.y-Top.y, Left.x-Top.x) / 2;
  y1 := y-Hypot(Left.y-Bottom.y, Left.x-Bottom.x) / 2;
  x2 := x+Hypot(Left.y-Top.y, Left.x-Top.x) / 2;
  y2 := y+Hypot(Left.y-Bottom.y, Left.x-Bottom.x) / 2;

  X := (X2 + X1) / 2 + (Random()*2 - 1);
  Y := (Y2 + Y1) / 2 + (Random()*2 - 1);

  Result.X := Round(RandomMean(x1+1, x2-1));
  Result.Y := Round(RandomMean(y1+1, y2-1));
  Result := Result.Rotate(a, TPoint.Create(Round(X), Round(Y)));
end;

function TQuadHelper.ToTPA: TPointArray;
begin
  Result := [Top, Right, Bottom, Left];
end;

function TQuadHelper.Bounds: TBox;
begin
  Result := ToTPA().Bounds();
end;

function TQuadHelper.ShortSideLen: Integer;
begin
  if (Hypot(Left.Y-Top.Y, Left.X-Top.X) < Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X)) then
    Result := Round(Hypot(Left.Y-Top.Y, Left.X-Top.X) / 2)
  else
    Result := Round(Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X) / 2);
end;

function TQuadHelper.LongSideLen: Integer;
begin
  if (Hypot(Left.Y-Top.Y, Left.X-Top.X) > Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X)) then
    Result := Round(Hypot(Left.Y-Top.Y, Left.X-Top.X) / 2)
  else
    Result := Round(Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X) / 2);
end;

function TQuadHelper.Mean: TPoint;
begin
  Result.X := (Self.Top.X + Self.Right.X + Self.Bottom.X + Self.Left.X) div 4;
  Result.Y := (Self.Top.Y + Self.Right.Y + Self.Bottom.Y + Self.Left.Y) div 4;
end;

function TQuadHelper.Rotate(Radians: Double): TQuad;
begin
  with Self.Mean() do
  begin
    Result.Top    := TSimbaGeometry.RotatePoint(Self.Top,    Radians, X, Y);
    Result.Right  := TSimbaGeometry.RotatePoint(Self.Right,  Radians, X, Y);
    Result.Bottom := TSimbaGeometry.RotatePoint(Self.Bottom, Radians, X, Y);
    Result.Left   := TSimbaGeometry.RotatePoint(Self.Left,   Radians, X, Y);
  end;

  Result := Result.Normalize();
end;

function TQuadHelper.Contains(P: TPoint): Boolean;
begin
  Result := TSimbaGeometry.PointInQuad(P, Self.Top, Self.Right, Self.Bottom, Self.Left);
end;

function TQuadHelper.Contains(X, Y: Integer): Boolean;
begin
  Result := TSimbaGeometry.PointInQuad(TPoint.Create(X, Y), Self.Top, Self.Right, Self.Bottom, Self.Left);
end;

function TQuadHelper.Offset(P: TPoint): TQuad;
begin
  Result := TQuad.Create(Top.Offset(P), Right.Offset(P), Bottom.Offset(P), Left.Offset(P));
end;

function TQuadHelper.Offset(X, Y: Integer): TQuad;
begin
  Result := TQuad.Create(Top.Offset(X, Y), Right.Offset(X, Y), Bottom.Offset(X, Y), Left.Offset(X, Y));
end;

function TQuadHelper.Extract(Points: TPointArray): TPointArray;
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

function TQuadHelper.Exclude(Points: TPointArray): TPointArray;
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

function TQuadHelper.Expand(Amount: Double): TQuad;
var
  InTPA, OutTPA: array[0..3] of TPoint;
  Theta: Double;
  I, J: Integer;
begin
  InTPA[0] := Self.Top;
  InTPA[1] := Self.Right;
  InTPA[2] := Self.Bottom;
  InTPA[3] := Self.Left;

  Amount *= Sqrt(2);
  for I := 0 to 3 do
  begin
    J := (I+1) mod 4;
    Theta := ArcTan2(InTPA[I].Y - InTPA[J].Y, InTPA[I].X - InTPA[J].X) + PI/4;
    OutTPA[I].X := Round(InTPA[I].X + Amount * Cos(Theta));
    OutTPA[I].Y := Round(InTPA[I].Y + Amount * Sin(Theta));
  end;

  Result := TQuad.Create(OutTPA[0], OutTPA[1], OutTPA[2], OutTPA[3]);
end;

function TQuadHelper.NearestEdge(P: TPoint): TPoint;
var
  Dists: array[0..3] of Double;
  Points: array[0..3] of TPoint;
  I: Integer;
  Best: Double;
begin
  Dists[0] := TSimbaGeometry.DistToLine(P, Self.Top,    Self.Left,   Points[0]);
  Dists[1] := TSimbaGeometry.DistToLine(P, Self.Left,   Self.Bottom, Points[1]);
  Dists[2] := TSimbaGeometry.DistToLine(P, Self.Bottom, Self.Right,  Points[2]);
  Dists[3] := TSimbaGeometry.DistToLine(P, Self.Right,  Self.Top,    Points[3]);

  Best   := Dists[0];
  Result := Points[0];

  for I := 1 to 3 do
    if (Dists[I] < Best) then
    begin
      Best   := Dists[I];
      Result := Points[I];
    end;
end;

function TQuadHelper.Area: Integer;
begin
  Result := Round(Distance(Self.Bottom, Self.Right)) * Round(Distance(Self.Bottom, Self.Left));
end;

function TQuadHelper.Normalize: TQuad;
var
  I, T: Integer;
  Points: TPointArray;
begin
  T := 0;
  Points := Self.ToTPA();
  for I := 1 to High(Points) do
    if (Points[I].Y < Points[T].Y) or ((Points[I].Y = Points[T].Y) and (Points[I].X < Points[T].X)) then
      T := I;

  Result.Top    := Points[T];
  Result.Right  := Points[(T + 1) mod 4];
  Result.Bottom := Points[(T + 2) mod 4];
  Result.Left   := Points[(T + 3) mod 4];
end;

operator in(const P: TPoint; const Quad: TQuad): Boolean;
begin
  Result := Quad.Contains(P);
end;

end.

