{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.math;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

const
  HALF_PI = PI / 2;

function AngleBetween(const P1, P2: TPoint): Double; inline;

function CrossProduct(const r, p, q: TPoint): Int64; inline; overload;
function CrossProduct(const rx, ry, px, py, qx, qy: Double): Double; inline; overload;

function PolygonArea(const Polygon: TPointArray): Double;

function PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean; inline;
function PointInCircle(const P, Center: TPoint; const Radius: Double): Boolean; inline;

function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray;
function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;

function GaussMatrix(N: Integer; sigma: Extended): TExtendedMatrix;
function FixRad(const Rad: Extended): Extended;
function FixD(const Degrees: Extended): Extended;
function MiddleBox(const B: TBox): TPoint;
function Distance(const X1, Y1, X2, Y2: Integer): Integer; inline; overload;
function Distance(const P1, P2: TPoint): Integer; inline; overload;
function Radians(const e: Extended): Extended;
function Degrees(const e: Extended): Extended;
function IntToBox(x1,y1,x2,y2 : Integer) : TBox;
function IntInBox(x, y: Integer; Box: TBox): Boolean;
function PointToBox(topLeft,bottomRight: TPoint): TBox;
function PointInBox(PT : TPoint; Box: TBox): Boolean;
function NextPower2(const n: Integer): Integer; inline;

function IsNumber(const n: Double): Boolean; inline; overload;
function IsNumber(const n: Single): Boolean; inline; overload;

function Modulo(const X, Y: Double): Double; inline; overload;
function Modulo(const X, Y: Single): Single; inline; overload;
function Modulo(const X, Y: Integer): Integer; inline; overload;

function TruncatedGauss(Left:Double=0; Right:Double=1; CUTOFF:Single=4): Double;

implementation

uses
  math;

function IsNumber(const n: Double): Boolean;
begin
  Result := (not IsNan(n)) and (not IsInfinite(n));
end;

function IsNumber(const n: Single): Boolean;
begin
  Result := (LongWord(n) and $7fffffff) < $7f800000; // Result := (not IsNan(n)) and (not IsInfinite(n));
end;

function Modulo(const X, Y: Double): Double;
begin
  Result := X - Floor(X / Y) * Y;
end;

function Modulo(const X, Y: Single): Single;
begin
  Result := X - Floor(X / Y) * Y;
end;

function Modulo(const X, Y: Integer): Integer;
begin
  Result := X - Floor(X / Y) * Y;
end;

function NextPower2(const n: Integer): Integer;
begin
  Result := n - 1;
  Result := Result or (Result shr 1);
  Result := Result or (Result shr 2);
  Result := Result or (Result shr 4);
  Result := Result or (Result shr 8);
  Result := Result or (Result shr 16);
  Result := Result or (Result shr 32);
  Result := Result + 1;
end;

function AngleBetween(const P1, P2: TPoint): Double;
begin
  Result := Modulo(Degrees(ArcTan2(P2.Y - P1.Y, P2.X - P1.X)) - 90, 360);
end;

function CrossProduct(const r, p, q: TPoint): Int64;
begin
  Result := (Int64(p.x) - Int64(r.x)) * (Int64(q.y) - Int64(r.y)) - (Int64(p.y) - Int64(r.y)) * (Int64(q.x) - Int64(r.x));
end;

function CrossProduct(const rx, ry, px, py, qx, qy: Double): Double;
begin
  Result := (px - rx) * (qy - ry) - (py - ry) * (qx - rx);
end;

function PolygonArea(const Polygon: TPointArray): Double;
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

  Result := Result * 0.5;
end;

function PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean;
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

function PointInCircle(const P, Center: TPoint; const Radius: Double): Boolean;
begin
  Result := Sqr(P.X - Center.X) + Sqr(P.Y - Center.Y) <= Sqr(Radius);
end;

{/\
  Rotate the given TPA with A radians.
/\}

function RotatePoints(const P: TPointArray; A, cx, cy: Extended): TPointArray;

Var
   I, L: Integer;
   CosA,SinA : extended;

Begin
  L := High(P);
  SetLength(Result, L + 1);
  CosA := Cos(a);
  SinA := Sin(a);
  For I := 0 To L Do
  Begin
    Result[I].X := Trunc(cx + CosA * (p[i].x - cx) - SinA * (p[i].y - cy));
    Result[I].Y := Trunc(cy + SinA * (p[i].x - cx) + CosA * (p[i].y - cy));
  End;
  // I recon it's faster than Point().
End;

{/\
  Rotate the given Point with A radians.
/\}

function RotatePoint(const p: TPoint; angle, mx, my: Extended): TPoint;
Begin
  Result.X := Trunc(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Trunc(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
End;


{/\
  Returns a GaussianMatrix with size of X*X, where X is Nth odd-number.
/\}
function GaussMatrix(N: Integer; sigma: Extended): TExtendedMatrix;
var
  hkernel: TExtendedArray;
  Size,i,x,y:Integer;
  sum:Extended;
begin
  Size := 2*N+1;
  SetLength(hkernel, Size);
  for i:=0 to Size-1 do
    hkernel[i] := Exp(-(Sqr((i-N) / Sigma)) / 2.0);

  SetLength(Result, Size, Size);
  sum:=0;
  for y:=0 to Size-1 do
    for x:=0 to Size-1 do
    begin
      Result[y][x] := hkernel[x]*hkernel[y];
      Sum := Sum + Result[y][x];
    end;

  for y := 0 to Size-1 do
    for x := 0 to Size-1 do
      Result[y][x] := Result[y][x] / sum;
end;

function FixRad(const Rad: Extended): Extended;
begin
  Result := DegToRad(DegNormalize(RadToDeg(Rad)));
end;

function MiddleBox(const B: TBox): TPoint;
begin
  Result.X := (B.X2 + B.X1) div 2;
  Result.Y := (B.Y2 + B.Y1) div 2;
end;

function Distance(const X1, Y1, X2, Y2: Integer): Integer;
begin
  Result := Round(Sqrt(Sqr(ValReal(X2) - ValReal(X1)) + Sqr(ValReal(Y2) - ValReal(Y1)))); // convert to ValReal to prevent integer overflows
end;

function Distance(const P1, P2: TPoint): Integer;
begin
  Result := Round(Sqrt(Sqr(ValReal(P2.X) - ValReal(P1.X)) + Sqr(ValReal(P2.Y) - ValReal(P1.Y)))); // convert to ValReal to prevent integer overflows
end;

function FixD(const Degrees: Extended): Extended;
begin
  Result := DegNormalize(Degrees);
end;

function Radians(const e: Extended): Extended;
begin
  Result := DegToRad(e);
end;

function Degrees(const e: Extended): Extended;
begin
  Result := RadToDeg(e);
end;

function IntToBox(x1,y1,x2,y2: Integer) : TBox;
begin
  result.x1 := x1;
  result.y1 := y1;
  result.x2 := x2;
  result.y2 := y2;
end;

function IntInBox(x, y: Integer; Box: TBox): Boolean;
begin
  result := (((x >= Box.x1) and(x <= Box.x2)) and ((y >= box.y1) and (y <= box.y2)));
end;

function PointToBox(topLeft,bottomRight: TPoint): TBox;
begin
  result.x1 := topLeft.x;
  result.y1 := topLeft.y;
  result.x2 := bottomRight.x;
  result.y2 := bottomRight.y;
end;

function PointInBox(PT : TPoint; Box: TBox): Boolean;
begin
  result := (((PT.x >= Box.x1) and(PT.x <= Box.x2)) and ((PT.y>= box.y1) and (PT.y <= box.y2)));
end;

function TruncatedGauss(Left:Double=0; Right:Double=1; CUTOFF:Single=4): Double;

  function nzRandom: Extended;
  begin
    {$IFDEF FPC_HAS_TYPE_EXTENDED}
    Result := Max(Random(), 1.0e-4900); //10^-4900 seems to be our safe limit
    {$ELSE}
    Result := Max(Random(), 1.0e-320);
    {$ENDIF}
  end;

begin
  Result := CUTOFF+1;
  while Result >= CUTOFF do
    Result := Abs(Sqrt(-2 * Ln(nzRandom())) * Cos(2 * PI * Random()));
  Result := Result / CUTOFF * (Right-Left) + Left;
end;

end.

