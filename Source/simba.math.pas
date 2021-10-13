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
function ChangeDistPT(const PT : TPoint; mx, my: Integer; newdist : Extended): TPoint;
function ChangeDistTPA(var TPA : TPointArray; mx,my : Integer; newdist : Extended): boolean;
function RiemannGauss(Xstart,StepSize,Sigma : Extended; AmountSteps : Integer): Extended;
function DiscreteGauss(Xstart,Xend : Integer; sigma : Extended) : TExtendedArray;
function GaussMatrix(N : Integer; sigma : Extended) : T2DExtendedArray;
function FixRad(const Rad: Extended): Extended;
function FixD(const Degrees: Extended): Extended;
function MiddleBox(b : TBox): TPoint;
function Distance(x1,y1,x2,y2 : Integer): Integer;
function Factorial(number: longword): Int64;
function BinCoe(a, b: LongInt): Extended;
function Radians(e: Extended): Extended;
function Degrees(e: Extended): Extended;
function IntToBox(x1,y1,x2,y2 : Integer) : TBox;
function IntInBox(x, y: Integer; Box: TBox): Boolean;
function PointToBox(topLeft,bottomRight: TPoint): TBox;
function PointInBox(PT : TPoint; Box: TBox): Boolean;
function NextPow2(n: Integer): Integer; inline;
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
function GaussMatrix(N: Integer; sigma: Extended): T2DExtendedArray;
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
function DiscreteGauss(Xstart,Xend : Integer; sigma : Extended) : TExtendedArray;
var
  i : Integer;
begin
  setlength(Result,Xend-xstart+1);
  for i := xstart to xend do
    result[i-xstart] :=  RiemannGauss(i-0.5,0.01,Sigma,100);
end;

{/\
  RiemannGauss integrates the Gaussian function using the Riemann method.
/\}
function RiemannGauss(Xstart,StepSize,Sigma : Extended; AmountSteps : Integer) : Extended;
var
  i : Integer;
  x : Extended;
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

function ChangeDistPT(const PT : TPoint; mx,my : Integer; newdist : Extended) : TPoint;
var
  angle : Extended;
begin
  angle := ArcTan2(pt.y-my,pt.x-mx);
  result.x := round(cos(angle) * newdist) + mx;
  result.y := round(sin(angle) * newdist) + my;
end;

function ChangeDistTPA(var TPA : TPointArray; mx,my : Integer; newdist : Extended) : boolean;
var
  angle : Extended;
  i : Integer;
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

function FixRad(const Rad: Extended): Extended;
const
  PI_MUL_2 = PI*2;
begin
  if (Rad <> PI_MUL_2) then
    Result := Rad - Int(Rad / PI_MUL_2) * PI_MUL_2;
  if (Result < 0) then
    Result := Result + PI_MUL_2;
end;

function MiddleBox(b : TBox): TPoint;
begin
  result := Point((b.x2+b.x1) div 2,(b.y2+b.y1) div 2);
end;

function Distance(x1,y1,x2,y2 : Integer) : Integer;
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

function FixD(const Degrees: Extended): Extended;
begin
  Result := DegNormalize(Degrees);
end;

function Radians(e: Extended): Extended;
begin
  Result := DegToRad(e);
end;

function Degrees(e: Extended): Extended;
begin
  Result := RadToDeg(e);
end;

function IntToBox(x1,y1,x2,y2 : Integer) : TBox;
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

end.

