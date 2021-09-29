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

function RotatePoints(const P: TPointArray;const A, cx, cy: Extended): TPointArray;
function RotatePoint(const p: TPoint;const angle, mx, my: Extended): TPoint;
function ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;
function ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean;
function RiemannGauss(Xstart,StepSize,Sigma : extended; AmountSteps : integer) : extended;
function DiscreteGauss(Xstart,Xend : integer; sigma : extended) : TExtendedArray;
function GaussMatrix(N : integer; sigma : extended) : T2DExtendedArray;
function MinA(a: TIntegerArray): Integer;
function MaxA(a: TIntegerArray): Integer;
function fixRad(rad: Extended): Extended;
function MiddleBox(b : TBox): TPoint;
function Distance(x1,y1,x2,y2 : integer) : integer;
function Factorial(number: longword): Int64;
function BinCoe(a, b: LongInt): Extended;
function FixD(Degrees : extended) : Extended;
function radians(e: extended): extended;
function degrees(e: extended): extended;
function Sum64IntArr(const Arr: TIntegerArray): Int64;
function IntToBox(x1,y1,x2,y2 : integer) : TBox;
function IntInBox(x, y: Integer; Box: TBox): Boolean;
function PointToBox(topLeft,bottomRight: TPoint): TBox;
function PointInBox(PT : TPoint; Box: TBox): Boolean;
function DecRet(e: Extended): Extended;
function NextPow2(n: Integer): Integer;
function IsNumber(const n: Single): Boolean; inline; overload;
function IsNumber(const n: Double): Boolean; inline; overload;

implementation

uses
  math;

function NextPow2(n: Integer): Integer;
begin
  n := n - 1;
  n := n or (n shr 1);
  n := n or (n shr 2);
  n := n or (n shr 4);
  n := n or (n shr 8);
  n := n or (n shr 16);
  n := n or (n shr 32);
  Result := n + 1;
end;

function IsNumber(const n: Single): Boolean;
begin
  // Result := (not IsNan(b)) and (not IsInfinite(b));
  Result := (LongWord(n) and $7fffffff) < $7f800000; // faster
end;

function IsNumber(const n: Double): Boolean;
begin
  Result := (not IsNan(n)) and (not IsInfinite(n));
end;

{/\
  Returns a GaussianMatrix with size of X*X, where X is Nth odd-number.
/\}
function GaussMatrix(N:Integer; Sigma:Extended): T2DExtendedArray;
var
  hkernel:Array of Extended;
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

{/\
  Returns the discrete Gaussian values, uses RiemanGauss with 100 steps.
/\}
function DiscreteGauss(Xstart,Xend : integer; sigma : extended) : TExtendedArray;
var
  i : integer;
begin
  setlength(Result,Xend-xstart+1);
  for i := xstart to xend do
    result[i-xstart] :=  RiemannGauss(i-0.5,0.01,Sigma,100);
end;

{/\
  RiemannGauss integrates the Gaussian function using the Riemann method.
/\}
function RiemannGauss(Xstart,StepSize,Sigma : extended; AmountSteps : integer) : extended;
var
  i : integer;
  x : extended;
begin
  result := 0;
  x := xstart - 0.5 * stepsize;
  for i := 1 to AmountSteps do
  begin
    x := x + stepsize; //Get the middle value
    result := Result + exp(-x*x/(2*sigma*sigma)); //Better accuracy to do the sig^2 here?
  end;
  result := result * stepsize * 1 / (Sqrt(2 * pi) * sigma);
end;

{/\
  Rotates the given points (P) by A (in radians) around the point defined by cx, cy.
/\}
function RotatePoints(const P: TPointArray;const A, cx, cy: Extended): TPointArray;
var
   I, L: Integer;
begin
  L := High(P);
  SetLength(Result, L + 1);
  for I := 0 to L do
  begin
    Result[I].X := Round(cx + cos(A) * (p[i].x - cx) - sin(A) * (p[i].y - cy));
    Result[I].Y := Round(cy + sin(A) * (p[i].x - cx) + cos(A) * (p[i].y - cy));
  end;
end;

{/\
  Rotates the given point (p) by A (in radians) around the point defined by cx, cy.
/\}
function RotatePoint(const p: TPoint;const angle, mx, my: Extended): TPoint;
begin
  Result.X := Round(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Round(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
end;

function ChangeDistPT(const PT : TPoint; mx,my : integer; newdist : extended) : TPoint;
var
  angle : extended;
begin
  angle := ArcTan2(pt.y-my,pt.x-mx);
  result.x := round(cos(angle) * newdist) + mx;
  result.y := round(sin(angle) * newdist) + my;
end;

function ChangeDistTPA(var TPA : TPointArray; mx,my : integer; newdist : extended) : boolean;
var
  angle : extended;
  i : integer;
begin
  result := false;
  if length(TPA) < 1 then
    exit;
  result := true;
  try
    for i := high(TPA) downto 0 do
    begin
      angle := ArcTan2(TPA[i].y-my,TPA[i].x-mx);
      TPA[i].x := round(cos(angle) * newdist) + mx;
      TPA[i].y := round(sin(angle) * newdist) + my;
    end;
  except
    result := false;
  end;
end;

function MinA(a: TIntegerArray): Integer;
var
  L, i: Integer;
begin
  L := High(a);
  if (L < 0) then
    Exit(0);

  Result := a[0];

  for i := 1 to L do
    if (a[i] < Result) then
      Result := a[i];
end;

function MaxA(a: TIntegerArray): Integer;
var
  L, i: Integer;
begin
  L := High(a);
  if (L < 0) then
    exit(0);

  Result := a[0];

  for i := 1 to L do
    if (a[i] > Result) then
      Result := a[i];
end;

function FixRad(rad: Extended): Extended;
begin
  result := rad;

  while (result >= (3.14159265358979320 * 2.0)) do
    result := result - (3.14159265358979320 * 2.0);

  while (result < 0) do
    result := result + (3.14159265358979320 * 2.0);
end;

function MiddleBox(b : TBox): TPoint;
begin
  result := point((b.x2+b.x1) div 2,(b.y2+b.y1) div 2);
end;

function Distance(x1,y1,x2,y2 : integer) : integer;
begin
  Result := Round(Sqrt(Sqr(x2-x1) + Sqr(y2-y1)));
end;

function Factorial(number: longword): Int64;
var
  Loop : longword;
begin
  result := 1;
  for loop := number downto 2 do
    result := result * loop;
end;

function BinCoe(a, b: LongInt): Extended;
begin
  result := Factorial(a) / (Factorial(b) * Factorial(a-b));
end;

function FixD(Degrees : extended) : Extended;
begin
  Result := Degrees;
  while Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;

function radians(e: extended): extended;
begin
  result := e / 180.0 * pi;
end;

function degrees(e: extended): extended;
begin
  result := e * 180.0 / pi;
end;

function Sum64IntArr(const Arr: TIntegerArray): Int64;
var
  H, I: LongInt;
begin
  Result := 0;

  H := High(Arr);
  for I := 0 to H do
    Result += Arr[I];
end;

function IntToBox(x1,y1,x2,y2 : integer) : TBox;
begin
  result.x1 := x1;
  result.y1 := y1;
  result.x2 := x2;
  result.y2 := y2;
end;

function IntInBox(x, y: Integer; Box: TBox): Boolean;
begin;
  result := (((x >= Box.x1) and(x <= Box.x2)) and ((y >= box.y1) and (y <= box.y2)));
end;

function PointToBox(topLeft,bottomRight: TPoint): TBox;
begin;
  result.x1 := topLeft.x;
  result.y1 := topLeft.y;
  result.x2 := bottomRight.x;
  result.y2 := bottomRight.y;
end;

function PointInBox(PT : TPoint; Box: TBox): Boolean;
begin
  result := (((PT.x >= Box.x1) and(PT.x <= Box.x2)) and ((PT.y>= box.y1) and (PT.y <= box.y2)));
end;

function DecRet(e: Extended): Extended;
begin
  result := e - Trunc(e);
end;

end.

