{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.math;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

const
  HALF_PI = Double(PI / 2);

  SQRT_2 = Double(1.4142135623731);
  SQRT_3 = Double(1.73205080756888);
  SQRT_5 = Double(2.23606797749979);

{$scopedenums on}
type
  EDistanceAlgo = (
    Euclidean,
    EuclideanSq,
    Manhattan,
    Chebyshev
  );
{$scopedenums off}

function Distance(const P1,P2: TPoint; Algo: EDistanceAlgo): Single; overload;
function Distance(const X1,Y1,X2,Y2: Double; Algo: EDistanceAlgo): Single; overload;

function DistEuclidean(const P1,P2: TPoint): Single; inline; overload;
function DistEuclidean(const X1,Y1,X2,Y2: Double): Single; inline; overload;

function DistEuclideanSq(const P1,P2: TPoint): Single; inline; overload;
function DistEuclideanSq(const X1,Y1,X2,Y2: Double): Single; inline; overload;

function DistManhattan(const P1,P2: TPoint): Single; inline; overload;
function DistManhattan(const X1,Y1,X2,Y2: Double): Single; inline; overload;

function DistChebyshev(const P1,P2: TPoint): Single; inline; overload;
function DistChebyshev(const X1,Y1,X2,Y2: Double): Single; inline; overload;

function NextPower2(const n: Integer): Integer;

function IsNumber(const n: Double): Boolean; inline; overload;
function IsNumber(const n: Single): Boolean; inline; overload;

function Modulo(const X, Y: Double): Double; inline; overload;
function Modulo(const X, Y: Single): Single; inline; overload;
function Modulo(const X, Y: Integer): Integer; inline; overload;

function CeilTo(const n: Double; const Precision: Int8 = 0): Double;

implementation

uses
  Math;

function Distance(const P1, P2: TPoint; Algo: EDistanceAlgo): Single;
begin
  case Algo of
    EDistanceAlgo.Euclidean:   Result := DistEuclidean(P1, P2);
    EDistanceAlgo.EuclideanSq: Result := DistEuclideanSq(P1, P2);
    EDistanceAlgo.Chebyshev:   Result := DistChebyshev(P1, P2);
    EDistanceAlgo.Manhattan:   Result := DistManhattan(P1, P2);
  end;
end;

function Distance(const X1, Y1, X2, Y2: Double; Algo: EDistanceAlgo): Single;
begin
  case Algo of
    EDistanceAlgo.Euclidean:   Result := DistEuclidean(X1, Y1, X2, Y2);
    EDistanceAlgo.EuclideanSq: Result := DistEuclideanSq(X1, Y1, X2, Y2);
    EDistanceAlgo.Chebyshev:   Result := DistChebyshev(X1, Y1, X2, Y2);
    EDistanceAlgo.Manhattan:   Result := DistManhattan(X1, Y1, X2, Y2);
  end;
end;

function DistEuclidean(const P1,P2: TPoint): Single;
begin
  // use Double to prevent integer overflows
  Result := Sqrt(Sqr(Double(P1.X) - Double(P2.X)) + Sqr(Double(P1.Y) - Double(P2.Y)));
end;

function DistEuclidean(const X1,Y1,X2,Y2: Double): Single;
begin
  Result := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));
end;

function DistEuclideanSq(const P1,P2: TPoint): Single;
begin
  Result := Sqr(Double(P1.X) - Double(P2.X)) + Sqr(Double(P1.Y) - Double(P2.Y));
end;

function DistEuclideanSq(const X1,Y1,X2,Y2: Double): Single;
begin
  Result := Sqr(X1-X2) + Sqr(Y1-Y2);
end;

function DistManhattan(const P1,P2: TPoint): Single;
begin
  Result := Abs(Double(P1.X) - Double(P2.X)) + Abs(Double(P1.Y) - Double(P2.Y));
end;

function DistManhattan(const X1,Y1,X2,Y2: Double): Single;
begin
  Result := Abs(X1-X2) + Abs(Y1-Y2);
end;

function DistChebyshev(const P1,P2: TPoint): Single;
begin
  Result := Max(Abs(Double(P1.X) - Double(P2.X)), Abs(Double(P1.Y) - Double(P2.Y)));
end;

function DistChebyshev(const X1,Y1,X2,Y2: Double): Single;
begin
  Result := Max(Abs(X1 - X2), Abs(Y1 - Y2));
end;

function IsNumber(const n: Double): Boolean;
begin
  Result := (not IsNan(n)) and (not IsInfinite(n));
end;

function IsNumber(const n: Single): Boolean;
begin
  Result := (LongWord(n) and $7FFFFFFF) < $7F800000; // Result := (not IsNan(n)) and (not IsInfinite(n));
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

function CeilTo(const n: Double; const Precision: Int8 = 0): Double;
begin
  if (Precision = 0) then
    Result := Ceil(n)
  else
    Result := RoundTo(n + 0.5 * 10**(-Double(Precision)), -Precision);
end;

end.
