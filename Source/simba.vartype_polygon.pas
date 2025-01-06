{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
 Jarl Holta - https://github.com/slackydev
  - Expand
  - Triangulate
}

unit simba.vartype_polygon;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.vartype_triangle;

type
  TPolygon = array of TPoint;
  TPolygonArray = array of TPolygon;

  TPolygonHelper = type helper for TPolygon
    function Bounds: TBox;
    function Mean: TPoint;
    function Area: Double;
    function Contains(p: TPoint): Boolean;
    function ContainsLine(a1, a2: TPoint): Boolean;
    function NearestEdge(P: TPoint): TPoint;
    function IsConvex: Boolean;
    function Expand(Amount: Integer): TPolygon;
    function DouglasPeucker(Epsilon: Double): TPolygon;
    procedure FurthestPoints(out A,B: TPoint);
    function Triangulate(MinArea: Single = 0; MaxDepth: Int32 = 0): TTriangleArray;
  end;

  PPolygon = ^TPolygon;
  PPolygonArray = ^TPolygonArray;

  operator in(const P: TPoint; const Poly: TPolygon): Boolean;

implementation

uses
  Math,
  simba.math,
  simba.geometry,
  simba.vartype_point,
  simba.vartype_pointarray;

function TPolygonHelper.Bounds: TBox;
begin
  Result := TPointArray(Self).Bounds;
end;

function TPolygonHelper.Mean: TPoint;
begin
  Result := TPointArray(Self).Mean;
end;

function TPolygonHelper.Area: Double;
var
  I, J: Integer;
begin
  Result := 0;

  J := Length(Self) - 1;
  for I := 0 to J do
  begin
    Result := Result + ((Self[J].X * Self[I].Y) - (Self[J].Y * Self[I].X));
    J := I;
  end;

  Result := Abs(Result) * 0.5;
end;

function TPolygonHelper.Contains(p: TPoint): Boolean;
begin
  Result := TSimbaGeometry.PointInPolygon(p, Self);
end;

function TPolygonHelper.NearestEdge(P: TPoint): TPoint;
var
  dist, best: single;
  I: Integer;
  q: TPoint;
begin
  if (Length(Self) = 0) then
    Exit(TPoint.ZERO);

  Best   := $FFFFFF;
  Result := Self[0];
  for I := 0 to High(Self) do
  begin
    dist := TSimbaGeometry.DistToLine(p, Self[i], Self[(i+1) mod Length(Self)], q);
    if (dist < best) then
    begin
      Best   := dist;
      Result := q;
    end;
  end;
end;

function TPolygonHelper.IsConvex: Boolean;
var
  i,d: Int32;
  a,b,c: TPoint;
begin
  if Length(Self) < 3 then
    Exit(False);

  d := TSimbaGeometry.CrossProduct(Self[0], Self[1 mod Length(Self)], Self[2 mod Length(Self)]);
  for i:=0 to High(Self) do
  begin
    A := Self[i];
    B := Self[(i+1) mod Length(Self)];
    C := Self[(i+2) mod Length(Self)];

    if TSimbaGeometry.CrossProduct(A,B,C)*d <= 0 then
      Exit(False);
  end;

  Result := True;
end;

function TPolygonHelper.ContainsLine(a1, a2: TPoint): Boolean;
var
  i: Int32;
  p1, p2: TPoint;
begin
  if (Length(Self) = 0) then
    Exit(False);

  for i:=0 to High(Self) - 1 do
  begin
    p1 := Self[i];
    p2 := Self[i + 1];
    if TSimbaGeometry.LinesIntersect(a1, a2, p1, p2) and not ((a1 = p1) or (a1 = p2) or (a2 = p1) or (a2 = p2)) then
      Exit(False);
  end;

  p1 := Self[High(Self)];
  p2 := Self[0];
  if TSimbaGeometry.LinesIntersect(a1, a2, p1, p2) and not ((a1 = p1) or (a1 = p2) or (a2 = p1) or (a2 = p2)) then
    Exit(False);

  Result := True;
end;

function TPolygonHelper.Expand(Amount: Integer): TPolygon;
var
  i,k,Len: Integer;
  theta,det: Double;
  c1,c2: array[0..2] of Double;
  p1,q1,p2,q2: TPointF;
  tmp: TPointFArray;
  CosValue, SinValue: Double;
begin
  if (Length(Self) > 1) then
  begin
    SetLength(Result, Length(Self));
    SetLength(tmp, Length(Self) * 2);
    for i := 0 to High(Self) do
    begin
      k := (i+1) mod Length(Self);
      theta := ArcTan2(Self[i].Y - Self[k].Y, Self[i].X - Self[k].X) + HALF_PI;
      SinCos(theta, SinValue, CosValue);
      tmp[i*2].X := Amount*CosValue+Self[i].X;
      tmp[i*2].Y := Amount*SinValue+Self[i].Y;
      tmp[i*2+1].X := Amount*CosValue+Self[k].X;
      tmp[i*2+1].Y := Amount*SinValue+Self[k].Y;
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
      if (Abs(det) > 0.001) then
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
  end else
    Result := Copy(Self);
end;

function TPolygonHelper.DouglasPeucker(Epsilon: Double): TPolygon;
var
  H, i, index: Int32;
  dmax, d: Double;
begin
  H := High(Self);
  if (H < 0) then
    Exit([]);

  dmax := 0;
  index := 0;
  for i:=1 to H do
  begin
    d := TSimbaGeometry.DistToLine(Self[i], Self[0], Self[H]);
    if (d > dmax) then
    begin
      index := i;
      dmax  := d;
    end;
  end;

  if (dmax > Epsilon) then
    Result := Copy(Self, 0, index).DouglasPeucker(epsilon) + Copy(Self, index).DouglasPeucker(epsilon)
  else
    Result := [Self[0], Self[High(Self)]];
end;

procedure TPolygonHelper.FurthestPoints(out A, B: TPoint);
var
  i, j, n: Integer;
  maxDist, dist: Double;
begin
  if Length(Self) = 0 then
  begin
    A := TPoint.ZERO;
    B := TPoint.ZERO;
    Exit;
  end;

  if Length(Self) = 1 then
  begin
    A := Self[0];
    B := Self[0];
    Exit;
  end;

  n := Length(Self);
  j := 1;
  maxDist := 0;
  for i:=0 to n-1 do
  begin
    while Abs(TSimbaGeometry.CrossProduct(Self[i], Self[(i + 1) mod n], Self[(j + 1) mod n])) >
          Abs(TSimbaGeometry.CrossProduct(Self[i], Self[(i + 1) mod n], Self[j])) do
      j := (j + 1) mod n;

    dist := Sqr(Self[i].x - Self[j].x) + Sqr(Self[i].y - Self[j].y);
    if dist > maxDist then
    begin
      maxDist := dist;
      A := Self[i];
      B := Self[j];
    end;
  end;
end;

function TPolygonHelper.Triangulate(MinArea: Single; MaxDepth: Int32): TTriangleArray;
var
  i,j: Int32;
  A,B,C: TPoint;
  tmp1,tmp2: TPointArray;
  valid: Boolean;
begin
  Result := [];

  tmp1 := TPointArray(Self).Reversed;
  SetLength(tmp2, Length(Self));

  j := 0;
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

      if (TSimbaGeometry.CrossProduct(A,B,C) >= 0) and ContainsLine(A,C) then
      begin
        if (j >= MaxDepth) or (TPolygon([A,B,C]).Area > MinArea) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)].A := A;
          Result[High(Result)].B := B;
          Result[High(Result)].C := C;
        end;

        tmp2[i]   := A;
        tmp2[i+1] := C;
        Inc(i,2);
        valid := True;
      end else
      begin
        tmp2[i] := A;
        Inc(i);
      end;
    end;

    if not valid then Exit();
    //Remove all duplicates without changing order
    //This is actually not bad here.
    if (i > Length(tmp1)) then SetLength(tmp1, i);
    Move(tmp2[0], tmp1[0], i*SizeOf(TPoint));

    tmp1 := TPointArray(tmp1).Unique();
  end;

  if Length(tmp1) = 3 then
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)].A := tmp1[0];
    Result[High(Result)].B := tmp1[1];
    Result[High(Result)].C := tmp1[2];
  end;
end;

operator in(const P: TPoint; const Poly: TPolygon): Boolean;
begin
  Result := Poly.Contains(P);
end;

end.

