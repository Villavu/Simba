{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.math;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils;

const
  HALF_PI = Double(PI / 2);

  SQRT_2 = Double(1.4142135623731);
  SQRT_3 = Double(1.73205080756888);
  SQRT_5 = Double(2.23606797749979);

function Distance(const P1,P2: TPoint): Double; overload; inline;
function Distance(const X1,Y1,X2,Y2: Double): Double; overload; inline;

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

function Distance(const P1, P2: TPoint): Double;
begin
  Result := Sqrt(Sqr(Double(P2.X) - Double(P1.X)) + Sqr(Double(P2.Y) - Double(P1.Y))); // convert to Double to prevent integer overflows
end;

function Distance(const X1, Y1, X2, Y2: Double): Double;
begin
  Result := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));
end;

function CeilTo(const n: Double; const Precision: Int8 = 0): Double;
begin
  if (Precision = 0) then
    Result := Ceil(n)
  else
    Result := RoundTo(n + 0.5 * 10**(-Double(Precision)), -Precision);
end;

end.

