{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.vartype_point;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TPointHelper = record helper for TPoint
  const
    ZERO: TPoint = (X: 0; Y: 0);
  public
    class function Create(const X, Y: Integer): TPoint; static; inline;

    function DistanceTo(Other: TPoint): Double;
    function Rotate(Radians: Double; Center: TPoint): TPoint;
    function Magnitude: Double;
    function AngleBetween(Other: TPoint): Double;
    function Offset(X, Y: Integer): TPoint; inline; overload;
    function Offset(P: TPoint): TPoint; inline; overload;
    function Random(Min, Max: Integer): TPoint; overload;
    function Random(Value: Integer): TPoint; overload;
  end;

  operator > (const Left, Right: TPoint): Boolean;
  operator < (const Left, Right: TPoint): Boolean;
  operator = (const Left, Right: TPoint): Boolean;
  operator + (const Left, Right: TPoint): TPoint;
  operator - (const Left, Right: TPoint): TPoint;
  operator * (const Left: TPoint; const Right: Double): TPoint;
  operator div (const Left: TPoint; const Right: Integer): TPoint;

implementation

uses
  simba.geometry;

class function TPointHelper.Create(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointHelper.DistanceTo(Other: TPoint): Double;
begin
  Result := Sqrt(Sqr(Double(Other.X) - Double(X)) + Sqr(Double(Other.Y) - Double(Y)));
end;

function TPointHelper.Rotate(Radians: Double; Center: TPoint): TPoint;
begin
  Result := TSimbaGeometry.RotatePoint(Self, Radians, Center.X, Center.Y);
end;

function TPointHelper.Magnitude: Double;
begin
  Result := Sqrt(Sqr(Self.X) + Sqr(Self.Y));
end;

function TPointHelper.AngleBetween(Other: TPoint): Double;
begin
  Result := TSimbaGeometry.AngleBetween(Self, Other);
end;

function TPointHelper.Offset(X, Y: Integer): TPoint;
begin
  Result.X := Self.X + X;
  Result.Y := Self.Y + Y;
end;

function TPointHelper.Offset(P: TPoint): TPoint;
begin
  Result.X := Self.X + P.X;
  Result.Y := Self.Y + P.Y;
end;

function TPointHelper.Random(Min, Max: Integer): TPoint;
begin
  Result.X := Self.X + Min + System.Random((Max - Min) + 1);
  Result.Y := Self.Y + Min + System.Random((Max - Min) + 1);
end;

function TPointHelper.Random(Value: Integer): TPoint;
begin
  Result := Random(-Value, Value);
end;

operator>(const Left, Right: TPoint): Boolean;
begin
  Result := Int64(Left) > Int64(Right);
end;

operator<(const Left, Right: TPoint): Boolean;
begin
  Result := Int64(Left) < Int64(Right);
end;

operator =(const Left, Right: TPoint): Boolean;
begin
  Result := Int64(Left) = Int64(Right);
end;

operator +(const Left, Right: TPoint): TPoint;
begin
  Result.X := Left.X + Right.X;
  Result.Y := Left.Y + Right.Y;
end;

operator -(const Left, Right: TPoint): TPoint;
begin
  Result.X := Left.X - Right.X;
  Result.Y := Left.Y - Right.Y;
end;

operator *(const Left: TPoint; const Right: Double): TPoint;
begin
  Result.X := Round(Left.X * Right);
  Result.Y := Round(Left.Y * Right);
end;

operator div(const Left: TPoint; const Right: Integer): TPoint;
begin
  Result.X := Left.X div Right;
  Result.Y := Left.Y div Right;
end;

end.

