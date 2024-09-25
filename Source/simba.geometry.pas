{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
 Jarl Holta - https://github.com/slackydev
  - ExpandPolygon
  - DistToLine
  - PointInEllipse
  - AngleBetween
  - DeltaAngle
  - IsConvexPolygon
  - LineInPolygon
  - TriangulatePolygon
}

{
 Arash Partow - https://github.com/ArashPartow/fastgeo
  - LinesIntersect
  - CrossProduct
  - PointInTriangle
  - PointInPolygon
  - FastRotate
  - PolygonArea
}

unit simba.geometry;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base, simba.math;

type
  TSimbaGeometry = class
  public
  class var
    CosTable: array[0..359] of Double;
    SinTable: array[0..359] of Double;
  public
    class constructor Create;

    class function IsConvexPolygon(const Polygon: TPointArray): Boolean;
    class function LineInPolygon(a1, a2: TPoint; const Polygon: TPointArray): Boolean;
    class function TriangulatePolygon(Polygon: TPointArray; MinArea: Single=0; MaxDepth: Int32=0): TTriangleArray;
    class function PolygonArea(const Polygon: TPointArray): Double; static; inline;
    class function ExpandPolygon(const Polygon: TPointArray; Amount: Integer): TPointArray; static;
    class function CrossProduct(const r, p, q: TPoint): Int64; static; overload; inline;
    class function CrossProduct(const rx,ry, px,py, qx,qy: Double): Double; static; overload; inline;
    class function LinesIntersect(const P1, P2, Q1, Q2: TPoint): Boolean; static; overload; inline;
    class function LinesIntersect(const P1, P2, Q1, Q2: TPoint; out Where: TPoint): Boolean; static; overload; inline;
    class function PointInTriangle(const P, P1, P2, P3: TPoint): Boolean; static; inline;
    class function PointInBox(const P: TPoint; const Box: TBox): Boolean; static; inline;
    class function PointInQuad(const P: TPoint; const A,B,C,D: TPoint): Boolean; static; overload; inline;
    class function PointInQuad(const X, Y: Integer; const A,B,C,D: TPoint): Boolean; static; overload; inline;
    class function PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean; static; overload; inline;
    class function PointInPolygon(const X, Y: Integer; const Polygon: TPointArray): Boolean; static; overload; inline;
    class function PointInCircle(const X, Y, CenterX, CenterY: Integer; const Radius: Double): Boolean; static; overload; inline;
    class function PointInCircle(const P, Center: TPoint; const Radius: Double): Boolean; static; overload; inline;
    class function PointInEllipse(const P, Center: TPoint; const YRadius, XRadius: Double): Boolean; static;

    class function RotatePointFast(const P: TPoint; Degrees: Integer; X, Y: Double): TPoint; static;
    class function RotatePointsFast(const Points: TPointArray; Degrees: Integer; X, Y: Double): TPointArray; static;

    class function RotatePoint(const P: TPoint; Radians, X, Y: Double): TPoint; static;
    class function RotatePoints(const Points: TPointArray; Radians, X, Y: Double): TPointArray; static;

    class function AngleBetween(const P1, P2: TPoint): Double; static; inline;
    class function DeltaAngle(const DegreesA, DegreesB: Double; R: Double = 360): Double; static;
    class function DistToLine(const P, P1, P2: TPoint; out Nearest: TPoint): Double; static; overload;
    class function DistToLine(const P, P1, P2: TPoint): Double; static; overload;
  end;

implementation

uses
  simba.vartype_pointarray;

class constructor TSimbaGeometry.Create;
var
  I: Integer;
begin
  for I := 0 to High(CosTable) do
  begin
    CosTable[I] := Cos((1.0 * I) * (PI / 180));
    SinTable[I] := Sin((1.0 * I) * (PI / 180));
  end;
end;

class function TSimbaGeometry.RotatePointFast(const P: TPoint; Degrees: Integer; X, Y: Double): TPoint;
var
  SinValue, CosValue: Double;
begin
  Degrees := Degrees mod 360;
  if (Degrees < 0) then
    Degrees := 360 + Degrees;

  SinValue := SinTable[Degrees];
  CosValue := CosTable[Degrees];

  Result.X := Round(X + CosValue * (P.X - X) - SinValue * (P.Y - Y));
  Result.Y := Round(Y + SinValue * (P.X - X) + CosValue * (P.Y - Y));
end;

class function TSimbaGeometry.RotatePointsFast(const Points: TPointArray; Degrees: Integer; X, Y: Double): TPointArray;
var
  I: Integer;
  CosValue, SinValue: Double;
begin
  SetLength(Result, Length(Points));

  Degrees := Degrees mod 360;
  if (Degrees < 0) then
    Degrees := 360 + Degrees;

  SinValue := SinTable[Degrees];
  CosValue := CosTable[Degrees];

  for I := 0 to High(Result) do
  begin
    Result[I].X := Round(X + CosValue * (Points[I].X - X) - SinValue * (Points[I].Y - Y));
    Result[I].Y := Round(Y + SinValue * (Points[I].X - X) + CosValue * (Points[I].Y - Y));
  end;
end;

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
  if (d = 0) then Exit(Hypot(P.X-P1.X, P.Y-P1.Y));
  f := ((P.X - P1.X) * (dx) + (P.Y - P1.Y) * (dy)) / d;
  if (f < 0) then Exit(Hypot(P.X-P1.X, P.Y-P1.Y));
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

class function TSimbaGeometry.ExpandPolygon(const Polygon: TPointArray; Amount: Integer): TPointArray;
type
  TPointF = record
    X, Y: Double;
  end;
  TPointFArray = array of TPointF;

  function PointF(const X, Y: Double): TPointF;
  begin
    Result.X := X;
    Result.Y := Y;
  end;

var
  i,k,Len: Integer;
  theta,det: Double;
  c1,c2: array[0..2] of Double;
  p1,q1,p2,q2: TPointF;
  tmp: TPointFArray;
  CosValue, SinValue: Double;
begin
  if (Length(Polygon) <= 1) then
    Exit(Polygon);

  SetLength(Result, Length(Polygon));
  SetLength(tmp, Length(Polygon) * 2);

  for i := 0 to High(Polygon) do
  begin
    k := (i+1) mod Length(Polygon);
    theta := ArcTan2(Polygon[i].Y - Polygon[k].Y, Polygon[i].X - Polygon[k].X) + HALF_PI;
    SinCos(theta, CosValue, SinValue);
    tmp[i*2]  := PointF(Amount*CosValue+Polygon[i].X, Amount*SinValue+Polygon[i].Y);
    tmp[i*2+1]:= PointF(Amount*CosValue+Polygon[k].X, Amount*SinValue+Polygon[k].Y);
  end;

  i := 0;
  Len := Length(tmp);
  while (i < Len) do
  begin
    p1 := tmp[i];
    p2 := tmp[(i+1) mod Len];
    q1 := tmp[(i+2) mod Len];
    q2 := tmp[(i+3) mod Len];

    c1[0] := p1.y-p2.y;
    c1[1] := p2.x-p1.x;
    c1[2] := -(p1.x*p2.y-p2.x*p1.y);

    c2[0] := q1.y-q2.y;
    c2[1] := q2.x-q1.x;
    c2[2] := -(q1.x*q2.y-q2.x*q1.y);

    det := c1[0] * c2[1] - c1[1] * c2[0];
    if (Abs(0 - det) > 0.001) then
    begin
      Result[i div 2].X := Round((c1[2] * c2[1] - c1[1] * c2[2]) / det);
      Result[i div 2].Y := Round((c1[0] * c2[2] - c1[2] * c2[0]) / det);
    end else
    begin
      Result[i div 2].X := Round(p2.x);
      Result[i div 2].Y := Round(p2.y);
    end;

    Inc(i, 2);
  end;
end;

class function TSimbaGeometry.PointInTriangle(const P, P1, P2, P3: TPoint): Boolean;

  function Orientation(const P1, P2, P: TPoint): Integer; inline;
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

class function TSimbaGeometry.PointInBox(const P: TPoint; const Box: TBox): Boolean;
begin
  Result := InRange(P.X, Box.X1, Box.X2) and InRange(P.Y, Box.Y1, Box.Y2);
end;

class function TSimbaGeometry.PointInQuad(const P: TPoint; const A, B, C, D: TPoint): Boolean;
begin
  Result := PointInTriangle(P, A,C,B) or PointInTriangle(P, A,D,C);
end;

class function TSimbaGeometry.PointInQuad(const X, Y: Integer; const A, B, C, D: TPoint): Boolean;
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;

  Result := PointInTriangle(P, A,C,B) or PointInTriangle(P, A,D,C);
end;

class function TSimbaGeometry.PointInCircle(const X, Y, CenterX, CenterY: Integer; const Radius: Double): Boolean;
begin
  Result := Sqr(X - CenterX) + Sqr(Y - CenterY) <= Sqr(Radius);
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

class function TSimbaGeometry.IsConvexPolygon(const Polygon: TPointArray): Boolean;
var
  i,d: Int32;
  a,b,c: TPoint;
begin
  if Length(Polygon) = 0 then Exit(False);

  d := CrossProduct(Polygon[i],Polygon[(i+1) mod Length(Polygon)],Polygon[(i+2) mod Length(Polygon)]);
  for i:=0 to High(Polygon) do
  begin
    A := Polygon[i];
    B := Polygon[(i+1) mod Length(Polygon)];
    C := Polygon[(i+2) mod Length(Polygon)];

    if CrossProduct(A,B,C)*d <= 0 then
      Exit(False);
  end;

  Result := True;
end;

class function TSimbaGeometry.LineInPolygon(a1, a2: TPoint; const Polygon: TPointArray): Boolean;
var
  i: Int32;
  p1, p2: TPoint;
begin
  for i:=0 to High(Polygon)-1 do
  begin
    p1 := Polygon[i];
    p2 := Polygon[i + 1];
    if LinesIntersect(a1, a2, p1, p2) and not ((a1 = p1) or (a1 = p2) or (a2 = p1) or (a2 = p2)) then
      Exit(False);
  end;
  
  p1 := Polygon[High(Polygon)];
  p2 := Polygon[0];
  if LinesIntersect(a1, a2, p1, p2) and not ((a1 = p1) or (a1 = p2) or (a2 = p1) or (a2 = p2)) then
    Exit(False);
  
  Result := True;
end; 

class function TSimbaGeometry.TriangulatePolygon(Polygon: TPointArray; MinArea: Single=0; MaxDepth: Int32=0): TTriangleArray;
var
  i,j: Int32;
  A,B,C: TPoint;
  tmp1,tmp2: TPointArray;
  valid: Boolean;
begin
  tmp1 := specialize Reversed<TPoint>(Polygon);
  SetLength(tmp2, Length(Polygon));

  while Length(tmp1) > 3 do
  begin
    Inc(j);
    valid := False;
    i := 0;
    while i < High(tmp1) do
    begin
      A := tmp1[i];
      B := tmp1[(i+1) mod Length(tmp1)];
      C := tmp1[(i+2) mod Length(tmp1)];

      if (CrossProduct(A,B,C) >= 0) and LineInPolygon(A,C, Polygon) then
      begin
        if (j >= MaxDepth) or (PolygonArea([A,B,C]) > MinArea) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)].A := A;
          Result[High(Result)].B := B;
          Result[High(Result)].C := C;
        end;
        
        tmp2[i]   := A;
        tmp2[i+1] := C;
        valid  := True;
        Inc(i,2);
      end else
      begin
        tmp2[i] := A;
        Inc(i);
      end;
    end;

    if not valid then Exit();
    //Remove all duplicates without changing order
    //This is actually not bad here.
    if (i) > Length(tmp1) then SetLength(tmp1, i);
    Move(tmp2[0], tmp1[0], i*SizeOf(TPoint));

    tmp1 := tmp1.Unique();
  end;
  
  if Length(tmp1) = 3 then
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)].A := tmp1[0];
    Result[High(Result)].B := tmp1[1];
    Result[High(Result)].C := tmp1[2];
  end;
end;


class function TSimbaGeometry.PolygonArea(const Polygon: TPointArray): Double;
var
  i, j: Integer;
begin
  Result := 0;

  j := Length(Polygon) - 1;
  for i := 0 to j do
  begin
    Result := Result + ((Polygon[J].X * Polygon[I].Y) - (Polygon[J].Y * Polygon[I].X));
    j := i;
  end;

  Result := Abs(Result) * 0.5;
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
  Ax, Bx, Cx: Integer;
  Ay, By, Cy: Integer;
  D, F, E: Integer;
  Ratio: Double;
begin
  Result := False;

  Ax := P2.X - P1.X;
  Bx := Q1.X - Q2.X;

  if (Ax < 0) then
  begin
    LowerX := P2.X;
    UpperX := P1.X;
  end else
  begin
    UpperX := P2.X;
    LowerX := P1.X;
  end;

  if (Bx > 0) then
  begin
    if (UpperX < Q2.X) or (Q1.X < LowerX) then
      Exit;
  end else
  if (Upperx < Q1.X) or (Q2.X < LowerX) then
    Exit;

  Ay := P2.Y - P1.Y;
  By := Q1.Y - Q2.Y;

  if (Ay < 0) then
  begin
    LowerY := P2.Y;
    UpperY := P1.Y;
  end else
  begin
    UpperY := P2.Y;
    LowerY := P1.Y;
  end;

  if (By > 0) then
  begin
    if (UpperY < Q2.Y) or (Q1.Y < LowerY) then
      Exit;
  end else
  if (UpperY < Q1.Y) or (Q2.Y < LowerY) then
    Exit;

  Cx := P1.X - Q1.X;
  Cy := P1.Y - Q1.Y;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if (f > 0) then
  begin
    if (d < 0) or (d > f) then
      Exit;
  end else
  if (d > 0) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if (f > 0) then
  begin
    if (e < 0) or (e > f) then
      Exit;
  end else
  if (e > 0) or (e < f) then
    Exit;

  Result := True;

  Ratio := (Ax * -By) - (Ay * -Bx);
  if (Ratio <> 0) then
  begin
    Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;

    Where.X := P1.X + Round(Ratio * Ax);
    Where.Y := P1.Y + Round(Ratio * Ay);
  end else
  begin
    if (Ax * -Cy) = (-Cx * Ay) then
    begin
      Where.X := Q1.X;
      Where.Y := Q1.Y;
    end else
    begin
      Where.X := Q2.X;
      Where.Y := Q2.Y;
    end;
  end;
end;

class function TSimbaGeometry.LinesIntersect(const P1,P2,Q1,Q2: TPoint): Boolean;
var
  UpperX, UpperY, LowerX, LowerY: Integer;
  Ax, Bx, Cx: Integer;
  Ay, By, Cy: Integer;
  D, F, E: Integer;
begin
  Result := False;

  Ax := P2.X - P1.X;
  Bx := Q1.X - Q2.X;

  if (Ax < 0) then
  begin
    LowerX := P2.X;
    UpperX := P1.X;
  end else
  begin
    UpperX := P2.X;
    LowerX := P1.X;
  end;

  if (Bx > 0) then
  begin
    if (UpperX < Q2.X) or (Q1.X < LowerX) then
      Exit;
  end else
  if (Upperx < Q1.X) or (Q2.X < LowerX) then
    Exit;

  Ay := P2.Y - P1.Y;
  By := Q1.Y - Q2.Y;

  if (Ay < 0) then
  begin
    LowerY := P2.Y;
    UpperY := P1.Y;
  end else
  begin
    UpperY := P2.Y;
    LowerY := P1.Y;
  end;

  if (By > 0) then
  begin
    if (UpperY < Q2.Y) or (Q1.Y < LowerY) then
      Exit;
  end else
  if (UpperY < Q1.Y) or (Q2.Y < LowerY) then
    Exit;

  Cx := P1.X - Q1.X;
  Cy := P1.Y - Q1.Y;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if (f > 0) then
  begin
    if (d < 0) or (d > f) then
      Exit;
  end else
  if (d > 0) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if (f > 0) then
  begin
    if (e < 0) or (e > f) then
      Exit;
  end else
  if (e > 0) or (e < f) then
    Exit;

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

class function TSimbaGeometry.PointInPolygon(const X, Y: Integer; const Polygon: TPointArray): Boolean;
var
  I, J: Integer;
begin
  Result := False;

  if (Length(Polygon) >= 3) then
  begin
    J := Length(Polygon) - 1;
    for I := 0 to J do
    begin
      if ((Polygon[I].Y <= Y) and (Y < Polygon[J].Y)) or    // an upward crossing
         ((Polygon[J].Y <= Y) and (Y < Polygon[I].Y)) then  // a downward crossing
      begin
        (* compute the edge-ray intersect at the x-coordinate *)
        if (X - Polygon[I].X < ((Polygon[J].X - Polygon[I].X) * (Y - Polygon[I].Y) / (Polygon[J].Y - Polygon[I].Y))) then
          Result := not Result;
      end;
      J := I;
    end;
  end;
end;

end.
