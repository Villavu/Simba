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

function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray;
function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;

function GaussMatrix(N: Integer; sigma: Extended): T2DExtendedArray;
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

function MaxA(const Arr: TIntegerArray): Integer; overload;
function MinA(const Arr: TIntegerArray): Integer; overload;

function MaxA(const Arr: TExtendedArray): Extended; overload;
function MinA(const Arr: TExtendedArray): Extended; overload;

function Sum(const Arr: TIntegerArray): Int64; overload;
function Sum(const Arr: TExtendedArray): Extended; overload;

function Average(const Arr: TIntegerArray): Int64; overload;
function Average(const Arr: TExtendedArray): Extended; overload;

function Mode(const Arr: TIntegerArray): Integer;

procedure Sort(var Arr: TIntegerArray);

function TruncatedGauss(Left:Double=0; Right:Double=1; CUTOFF:Single=4): Double;

implementation

uses
  simba.array_generics, simba.tpa,
  math;

function IsNumber(const n: Double): Boolean;
begin
  Result := (not IsNan(n)) and (not IsInfinite(n));
end;

function IsNumber(const n: Single): Boolean;
begin
  Result := (LongWord(n) and $7fffffff) < $7f800000; // Result := (not IsNan(b)) and (not IsInfinite(b));
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


{/\
  Rotate the given TPA with A radians.
/\}

Function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray;

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

Function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;
Begin
  Result.X := Trunc(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Trunc(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
End;


{/\
  Returns a GaussianMatrix with size of X*X, where X is Nth odd-number.
/\}
function GaussMatrix(N: Integer; sigma: Extended): T2DExtendedArray;
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

function MaxA(const Arr: TIntegerArray): Integer;
begin
  Result := specialize MaxA<Integer>(Arr);
end;

function MinA(const Arr: TIntegerArray): Integer;
begin
  Result := specialize MinA<Integer>(Arr);
end;

function MaxA(const Arr: TExtendedArray): Extended;
begin
  Result := specialize MaxA<Extended>(Arr);
end;

function MinA(const Arr: TExtendedArray): Extended;
begin
  Result := specialize MinA<Extended>(Arr);
end;

function Sum(const Arr: TIntegerArray): Int64;
begin
  Result := specialize Sum<Integer, Int64>(Arr);
end;

function Sum(const Arr: TExtendedArray): Extended;
begin
  Result := specialize Sum<Extended, Extended>(Arr);
end;

function Average(const Arr: TIntegerArray): Int64;
begin
  Result := Sum(Arr);
  if (Result <> 0) then
    Result := Result div Length(Arr);
end;

function Average(const Arr: TExtendedArray): Extended;
begin
  Result := Sum(Arr);
  if (Result <> 0) then
    Result := Result / Length(Arr);
end;

function Mode(const Arr: TIntegerArray): Integer;
var
  Self: TIntegerArray;
  I: Integer;
  Current, Hits: Integer;
  Best: Integer;
begin
  Result := 0;

  if Length(Arr) > 0 then
  begin
    Self := Copy(Arr);
    Sort(Self);

    Current := Self[0];
    Hits := 1;
    Best := 0;

    for I := 1 to High(Self) do
    begin
      if (Self[I] <> Current) then
      begin
        if (Hits > Best) then
        begin
          Best := Hits;
          Result := Current;
        end;

        Current := Self[I];
        Hits := 0;
      end;

      Inc(Hits);
    end;

    if (Hits > Best) then
      Result := Current;
  end;
end;

procedure Sort(var Arr: TIntegerArray);
begin
  specialize QuickSort<Integer>(Arr, Low(Arr), High(Arr));
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

