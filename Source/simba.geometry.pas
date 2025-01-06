{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
 Jarl Holta - https://github.com/slackydev
  - ExpandPolygon
  - DistToLine
  - AngleBetween
  - DeltaAngle
}

{
 Arash Partow - https://github.com/ArashPartow/fastgeo
  - LinesIntersect
  - CrossProduct
  - PointInTriangle
  - PointInPolygon
}

unit simba.geometry;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base, simba.math;

type
  TSimbaGeometry = class
  public
    class function CrossProduct(const r, p, q: TPoint): Int64; static; overload; inline;
    class function CrossProduct(const rx,ry, px,py, qx,qy: Double): Double; static; overload; inline;
    class function LinesIntersect(const P1, P2, Q1, Q2: TPoint): Boolean; static; overload;
    class function LinesIntersect(const P1, P2, Q1, Q2: TPoint; out Where: TPoint): Boolean; static; overload;

    class function PointInTriangle(const P, P1, P2, P3: TPoint): Boolean; static;
    class function PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean; static;
    class function PointInCircle(const P, Center: TPoint; const Radius: Double): Boolean; static;
    class function PointInEllipse(const P, Center: TPoint; const YRadius, XRadius: Double): Boolean; static;

    class function RotatePoint(const P: TPoint; Radians, X, Y: Double): TPoint; static;
    class function RotatePoints(const Points: TPointArray; Radians, X, Y: Double): TPointArray; static;

    class function AngleBetween(const P1, P2: TPoint): Double; static;
    class function DeltaAngle(const DegreesA, DegreesB: Double; R: Double = 360): Double; static;
    class function DistToLine(const P, P1, P2: TPoint; out Nearest: TPoint): Double; static; overload;
    class function DistToLine(const P, P1, P2: TPoint): Double; static; overload;
  end;

implementation

class function TSimbaGeometry.RotatePoint(const P: TPoint; Radians, X, Y: Double): TPoint;
var
  CosValue, SinValue: Double;
begin
  SinCos(Radians, SinValue, CosValue);

  Result.X := Round(X + CosValue * (P.X - X) - SinValue * (P.Y - Y));
  Result.Y := Round(Y + SinValue * (P.X - X) + CosValue * (P.Y - Y));
end;

class function TSimbaGeometry.RotatePoints(const Points: TPointArray; Radians, X, Y: Double): TPointArray;
var
  I: Integer;
  CosValue, SinValue: Double;
begin
  SinCos(Radians, SinValue, CosValue);

  SetLength(Result, Length(Points));
  for I := 0 to High(Result) do
  begin
    Result[I].X := Round(X + CosValue * (Points[I].X - X) - SinValue * (Points[I].Y - Y));
    Result[I].Y := Round(Y + SinValue * (Points[I].X - X) + CosValue * (Points[I].Y - Y));
  end;
end;

class function TSimbaGeometry.AngleBetween(const P1, P2: TPoint): Double;
begin
  Result := Modulo(RadToDeg(ArcTan2(P2.Y - P1.Y, P2.X - P1.X)) - 90, 360);
end;

class function TSimbaGeometry.DeltaAngle(const DegreesA, DegreesB: Double; R: Double): Double;
begin
  Result := Modulo((DegreesA - DegreesB + R/2), R) - R/2;
end;

class function TSimbaGeometry.DistToLine(const P, P1, P2: TPoint; out Nearest: TPoint): Double;
var
  dx,dy,d:Integer;
  f: Single;
begin
  Nearest.X := P1.X;
  Nearest.Y := P1.Y;
  dx := P2.X - P1.X;
  dy := P2.Y - P1.Y;
  d := dx*dx + dy*dy;
  if (d = 0) then
    Exit(Hypot(P.X-P1.X, P.Y-P1.Y));
  f := ((P.X - P1.X) * (dx) + (P.Y - P1.Y) * (dy)) / d;
  if (f < 0) then
    Exit(Hypot(P.X-P1.X, P.Y-P1.Y));
  if (f > 1) then
  begin
    Nearest := P2;
    Exit(Hypot(P.X-P2.X, P.Y-P2.Y));
  end;
  Nearest.X := Round(P1.X + f * dx);
  Nearest.Y := Round(P1.Y + f * dy);
  Result := Hypot(P.X-Nearest.X, P.Y-Nearest.Y);
end;

class function TSimbaGeometry.DistToLine(const P, P1, P2: TPoint): Double;
var
  dx,dy,d:Integer;
  f: Single;
begin
  dx := P2.X - P1.X;
  dy := P2.Y - P1.Y;
  d := dx*dx + dy*dy;
  if (d = 0) then
    Exit(Hypot(P.X-P1.X, P.Y-P1.Y));
  f := ((P.X - P1.X) * (dx) + (P.Y - P1.Y) * (dy)) / d;
  if (f < 0) then
    Exit(Hypot(P.X-P1.X, P.Y-P1.Y));
  if (f > 1) then
    Exit(Hypot(P.X-P2.X, P.Y-P2.Y));

  Result := Hypot(P.X-(P1.X + f * dx), P.Y-(P1.Y + f * dy));
end;

class function TSimbaGeometry.PointInTriangle(const P, P1, P2, P3: TPoint): Boolean;

  function Orientation(const P1, P2, P: TPoint): Integer;
  var
    Orin: Double;
  begin
    Orin := (P2.X - P1.X) * (P.Y - P1.Y) - (P.X - P1.X) * (P2.Y - P1.Y);

    if (Orin > 0) then
      Result := 1      (* Orientaion is to the left-hand side  *)
    else
    if (Orin < 0) then (* Orientaion is to the right-hand side *)
      Result := -1
    else
      Result := 0;     (* Orientaion is neutral aka collinear  *)
  end;

var
  Or1, Or2, Or3: Integer;
begin
  Result := False;

  Or1 := Orientation(P1,P2,P);
  Or2 := Orientation(P2,P3,P);

  if (Or1 * Or2) <> -1 then
  begin
    Or3 := Orientation(P3,P1,P);

    if (Or1 = Or3) or (Or3 = 0) then
      Result := True
    else if Or1 = 0 then
      Result := (Or2 * Or3) >= 0
    else if Or2 = 0 then
      Result := (Or1 * Or3) >= 0;
  end;
end;

class function TSimbaGeometry.PointInCircle(const P, Center: TPoint; const Radius: Double): Boolean;
begin
  Result := Sqr(P.X - Center.X) + Sqr(P.Y - Center.Y) <= Sqr(Radius);
end;

class function TSimbaGeometry.PointInEllipse(const P, Center: TPoint; const YRadius, XRadius: Double): Boolean;
var
  X, Y: Integer;
begin
  X := P.X - Center.X;
  Y := P.Y - Center.Y;

  Result := (Sqr(X) * Sqr(YRadius)) + (Sqr(Y) * Sqr(XRadius)) <= (Sqr(YRadius) * Sqr(XRadius));
end;

class function TSimbaGeometry.CrossProduct(const r, p, q: TPoint): Int64;
begin
  Result := (Int64(p.x) - Int64(r.x)) * (Int64(q.y) - Int64(r.y)) - (Int64(p.y) - Int64(r.y)) * (Int64(q.x) - Int64(r.x));
end;

class function TSimbaGeometry.CrossProduct(const rx, ry, px, py, qx, qy: Double): Double;
begin
  Result := (px - rx) * (qy - ry) - (py - ry) * (qx - rx);
end;

class function TSimbaGeometry.LinesIntersect(const P1,P2,Q1,Q2: TPoint; out Where: TPoint): Boolean;
var
  UpperX, UpperY, LowerX, LowerY: Integer;
  ax, bx, cx: Integer;
  ay, by, cy: Integer;
  D, F, E: Integer;
  Ratio: Double;
begin
  ax := P2.X - P1.X;
  bx := Q1.X - Q2.X;

  if (ax < 0) then
  begin
    LowerX := P2.X;
    UpperX := P1.X;
  end else
  begin
    UpperX := P2.X;
    LowerX := P1.X;
  end;

  if ((bx > 0) and ((UpperX < Q2.X) or (Q1.X < LowerX))) or
     ((bx <= 0) and ((Upperx < Q1.X) or (Q2.X < LowerX))) then
    Exit(False);

  ay := P2.Y - P1.Y;
  by := Q1.Y - Q2.Y;

  if (ay < 0) then
  begin
    LowerY := P2.Y;
    UpperY := P1.Y;
  end else
  begin
    UpperY := P2.Y;
    LowerY := P1.Y;
  end;

  if ((by > 0) and ((UpperY < Q2.Y) or (Q1.Y < LowerY))) or
     ((by <= 0) and ((UpperY < Q1.Y) or (Q2.Y < LowerY))) then
    Exit(False);

  cx := P1.X - Q1.X;
  cy := P1.Y - Q1.Y;
  d  := (by * cx) - (bx * cy);
  f  := (ay * bx) - (ax * by);

  if ((f > 0) and ((d < 0) or (d > f))) or
     ((f <= 0) and ((d > 0) or (d < f))) then
    Exit(False);

  e := (ax * cy) - (ay * cx);

  if ((f > 0) and ((e < 0) or (e > f))) or
     ((f <= 0) and ((e > 0) or (e < f))) then
    Exit(False);

  Ratio := (ax * -by) - (ay * -bx);
  if (Ratio <> 0) then
  begin
    Ratio := ((cy * -bx) - (cx * -by)) / Ratio;

    Where.X := P1.X + Round(Ratio * ax);
    Where.Y := P1.Y + Round(Ratio * ay);
  end else if (ax * -cy) = (-cx * ay) then
  begin
    Where.X := Q1.X;
    Where.Y := Q1.Y;
  end else
  begin
    Where.X := Q2.X;
    Where.Y := Q2.Y;
  end;

  Result := True;
end;

class function TSimbaGeometry.LinesIntersect(const P1,P2,Q1,Q2: TPoint): Boolean;
var
  UpperX, UpperY, LowerX, LowerY: Integer;
  ax, bx, cx: Integer;
  ay, by, cy: Integer;
  D, F, E: Integer;
begin
  ax := P2.X - P1.X;
  bx := Q1.X - Q2.X;

  if (ax < 0) then
  begin
    LowerX := P2.X;
    UpperX := P1.X;
  end else
  begin
    UpperX := P2.X;
    LowerX := P1.X;
  end;

  if ((bx > 0) and ((UpperX < Q2.X) or (Q1.X < LowerX))) or
     ((bx <= 0) and ((Upperx < Q1.X) or (Q2.X < LowerX))) then
    Exit(False);

  ay := P2.Y - P1.Y;
  by := Q1.Y - Q2.Y;

  if (ay < 0) then
  begin
    LowerY := P2.Y;
    UpperY := P1.Y;
  end else
  begin
    UpperY := P2.Y;
    LowerY := P1.Y;
  end;

  if ((by > 0) and ((UpperY < Q2.Y) or (Q1.Y < LowerY))) or
     ((by <= 0) and ((UpperY < Q1.Y) or (Q2.Y < LowerY))) then
    Exit(False);

  cx := P1.X - Q1.X;
  cy := P1.Y - Q1.Y;
  d  := (by * cx) - (bx * cy);
  f  := (ay * bx) - (ax * by);

  if ((f > 0) and ((d < 0) or (d > f))) or
     ((f <= 0) and ((d > 0) or (d < f))) then
    Exit(False);

  e := (ax * cy) - (ay * cx);

  if ((f > 0) and ((e < 0) or (e > f))) or
     ((f <= 0) and ((e > 0) or (e < f))) then
    Exit(False);

  Result := True;
end;

class function TSimbaGeometry.PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean;
var
  I, J: Integer;
begin
  Result := False;

  if (Length(Polygon) >= 3) then
  begin
    J := Length(Polygon) - 1;
    for I := 0 to J do
    begin
      if ((Polygon[I].Y <= P.Y) and (P.Y < Polygon[J].Y)) or    // an upward crossing
         ((Polygon[J].Y <= P.Y) and (P.Y < Polygon[I].Y)) then  // a downward crossing
      begin
        (* compute the edge-ray intersect at the x-coordinate *)
        if (P.X - Polygon[I].X < ((Polygon[J].X - Polygon[I].X) * (P.Y - Polygon[I].Y) / (Polygon[J].Y - Polygon[I].Y))) then
          Result := not Result;
      end;
      J := I;
    end;
  end;
end;

end.
