{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.circle;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  PCircle = ^TCircle;
  TCircle = record
    Center: TPoint;
    Radius: Integer;
  end;

  TCircleHelper = type helper for TCircle
  public const
    DEFAULT_VALUE: TCircle = (Center: (X: 0; Y: 0); Radius: 0);
  public
    class function Create(ACenter: TPoint; ARadius: Integer): TCircle; static;
    class function CreateFromPoints(Points: TPointArray): TCircle; static;

    function Bounds: TBox;
    function ToTPA(Filled: Boolean): TPointArray;
    function Contains(const P: TPoint): Boolean; inline;
    function PointAtDegrees(Degrees: Double): TPoint;
    function Circumference: Double;
    function Area: Double;
    function Expand(Amount: Integer): TCircle;
    function Offset(P: TPoint): TCircle;
    function Extract(Points: TPointArray): TPointArray;
    function Exclude(Points: TPointArray): TPointArray;
    function RandomPoint: TPoint;
    function RandomPointCenter: TPoint;
  end;

  operator in(const P: TPoint; const Circle: TCircle): Boolean;

implementation

uses
  Math,
  simba.tpa, simba.random, simba.overallocatearray;

class function TCircleHelper.Create(ACenter: TPoint; ARadius: Integer): TCircle;
begin
  Result.Center := ACenter;
  Result.Radius := ARadius;
end;

class function TCircleHelper.CreateFromPoints(Points: TPointArray): TCircle;
begin
  Result := Points.MinAreaCircle();
end;

function TCircleHelper.Bounds: TBox;
begin
  Result := TBox.Create(Center, Radius, Radius);
end;

function TCircleHelper.ToTPA(Filled: Boolean): TPointArray;
begin
  Result := TPointArray.CreateFromCircle(Center, Radius, Filled);
end;

function TCircleHelper.Contains(const P: TPoint): Boolean;
begin
  Result := Hypot(P.X - Self.Center.X, P.Y - Self.Center.Y) <= Self.Radius;
end;

function TCircleHelper.PointAtDegrees(Degrees: Double): TPoint;
begin
  Result.X := Round(Self.Radius * Cos(DegToRad(Degrees) - PI/2)) + Self.Center.X;
  Result.Y := Round(Self.Radius * Sin(DegToRad(Degrees) - PI/2)) + Self.Center.Y;
end;

function TCircleHelper.Circumference: Double;
begin
  Result := 2 * PI * Self.Radius;
end;

function TCircleHelper.Area: Double;
begin
  Result := PI * Sqr(Self.Radius);
end;

function TCircleHelper.Expand(Amount: Integer): TCircle;
begin
  Result := Self;
  Result.Radius += Amount;
end;

function TCircleHelper.Offset(P: TPoint): TCircle;
begin
  Result := Self;
  Result.Center := Result.Center.Offset(P);
end;

function TCircleHelper.Extract(Points: TPointArray): TPointArray;
var
  I: Integer;
  Buffer: TSimbaPointBuffer;
begin
  Buffer.Init(Length(Points));
  for I := 0 to High(Points) do
    if Contains(Points[I]) then
      Buffer.Add(Points[I]);

  Result := Buffer.Trim();
end;

function TCircleHelper.Exclude(Points: TPointArray): TPointArray;
var
  I: Integer;
  Buffer: TSimbaPointBuffer;
begin
  Buffer.Init(Length(Points));
  for I := 0 to High(Points) do
    if not Contains(Points[I]) then
      Buffer.Add(Points[I]);

  Result := Buffer.Trim();
end;

function TCircleHelper.RandomPoint: TPoint;
var
  R, Theta, SinValue, CosValue: Double;
begin
  R := Radius * Sqrt(Random());
  Theta := Random() * 2 * PI;

  SinCos(Theta, SinValue, CosValue);

  Result.X := Center.X + Round(R * CosValue);
  Result.Y := Center.X + Round(R * SinValue);
end;

function TCircleHelper.RandomPointCenter: TPoint;
var
  R, Theta, SinValue, CosValue: Double;
begin
  R := Radius * Sqrt(RandomLeft(0.0, 1.0));
  Theta := Random() * 2 * PI;

  SinCos(Theta, SinValue, CosValue);

  Result.X := Center.X + Round(R * CosValue);
  Result.Y := Center.X + Round(R * SinValue);
end;

operator in(const P: TPoint; const Circle: TCircle): Boolean;
begin
  Result := Circle.Contains(P);
end;

end.

