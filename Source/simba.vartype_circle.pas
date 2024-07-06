{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.vartype_circle;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TCircleHelper = type helper for TCircle
  private
    function GetCenter: TPoint;
    function GetEdge: TPointArray;
    function GetFilled: TPointArray;
    function GetBounds: TBox;
    function GetArea: Double;
    function GetCircumference: Double;
  public const
    ZERO: TCircle = (X: 0; Y: 0; Radius: 0);
  public
    class function Create(AX, AY: Integer; ARadius: Integer): TCircle; static;
    class function CreateFromPoints(Points: TPointArray): TCircle; static;

    property Edge: TPointArray read GetEdge;
    property Filled: TPointArray read GetFilled;
    property Center: TPoint read GetCenter;
    property Bounds: TBox read GetBounds;
    property Area: Double read GetArea;
    property Circumference: Double read GetCircumference;

    function Contains(const P: TPoint): Boolean;
    function PointAtDegrees(Degrees: Double): TPoint;
    function Expand(Amount: Integer): TCircle;
    function Offset(P: TPoint): TCircle;
    function Extract(Points: TPointArray): TPointArray;
    function Exclude(Points: TPointArray): TPointArray;
    function RandomPoint: TPoint;
    function RandomPointCenter: TPoint;
    function Circularity(TPA: TPointArray): Double;
  end;

  operator in(const P: TPoint; const Circle: TCircle): Boolean;

implementation

uses
  Math,
  simba.math, simba.vartype_pointarray, simba.random, simba.containers, simba.geometry,
  simba.vartype_box;

function TCircleHelper.GetCenter: TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TCircleHelper.GetEdge: TPointArray;
begin
  Result := TPointArray.CreateFromCircle(Center, Radius, False);
end;

function TCircleHelper.GetFilled: TPointArray;
begin
  Result := TPointArray.CreateFromCircle(Center, Radius, True);
end;

function TCircleHelper.GetBounds: TBox;
begin
  Result := TBox.Create(Center, Radius, Radius);
end;

function TCircleHelper.GetCircumference: Double;
begin
  Result := 2 * PI * Self.Radius;
end;

function TCircleHelper.GetArea: Double;
begin
  Result := PI * Sqr(Self.Radius);
end;

class function TCircleHelper.Create(AX, AY: Integer; ARadius: Integer): TCircle;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Radius := ARadius;
end;

class function TCircleHelper.CreateFromPoints(Points: TPointArray): TCircle;
begin
  Result := Points.MinAreaCircle();
end;

function TCircleHelper.Contains(const P: TPoint): Boolean;
begin
  Result := Distance(X, Y, P.X, P.Y) <= Radius;
end;

function TCircleHelper.PointAtDegrees(Degrees: Double): TPoint;
begin
  Result.X := Round(Self.Radius * Cos(DegToRad(Degrees) - PI/2)) + Self.Center.X;
  Result.Y := Round(Self.Radius * Sin(DegToRad(Degrees) - PI/2)) + Self.Center.Y;
end;

function TCircleHelper.Expand(Amount: Integer): TCircle;
begin
  Result := Self;
  Result.Radius += Amount;
end;

function TCircleHelper.Offset(P: TPoint): TCircle;
begin
  Result := Self;
  Result.X += P.X;
  Result.Y += P.Y;
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

  Result := Buffer.ToArray(False);
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

  Result := Buffer.ToArray(False);
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

function TCircleHelper.Circularity(TPA: TPointArray): Double;
var
  I: Integer;
  Smallest, Test: Double;
  Hull: TPointArray;
begin
  Hull := TPA.ConvexHull();
  if Length(Hull) <= 1 then
    Exit(0);

  Smallest := $FFFFFF;
  for I := 0 to High(Hull) do
  begin
    Test := TSimbaGeometry.DistToLine(Self.Center, Hull[I], Hull[(I+1) mod Length(Hull)]);
    if (Test < Smallest) then
      Smallest := Test;
  end;
  Result := Sqr(Smallest) / Sqr(Self.Radius);
end;

operator in(const P: TPoint; const Circle: TCircle): Boolean;
begin
  Result := Circle.Contains(P);
end;

end.

