{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  TPointHelper = record Helper for TPoint
  public
    class function Create(X, Y: Integer): TPoint; static;

    function InTriangle(A, B, C: TPoint): Boolean;
    function InPolygon(Poly: TPointArray): Boolean;
    function InCircle(Center: TPoint; Radius: Double): Boolean;
    function InBox(Box: TBox): Boolean;
    function DistanceTo(Other: TPoint): Double;
    function RotateFast(Degrees: Integer; Center: TPoint): TPoint;
    function Rotate(Radians: Double; Center: TPoint): TPoint;
    function Magnitude: Double;
    function AngleBetween(Other: TPoint): Double;
    function Offset(X, Y: Integer): TPoint; overload;
    function Offset(P: TPoint): TPoint; overload;
    function Random(Min, Max: Integer): TPoint; overload;
    function Random(Value: Integer): TPoint; overload;
  end;

  function Point(X, Y: Integer): TPoint;

  operator = (const Left, Right: TPoint): Boolean;
  operator + (const Left, Right: TPoint): TPoint;
  operator - (const Left, Right: TPoint): TPoint;
  operator * (const Left: TPoint; const Right: Double): TPoint;
  operator in (const Left: TPoint; const Right: TBox): Boolean;
{$ENDIF}

{$IFDEF BODY}
class function TPointHelper.Create(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointHelper.InTriangle(A, B, C: TPoint): Boolean;
begin
  Result := TSimbaGeometry.PointInTriangle(Self, A, B, C);
end;

function TPointHelper.InPolygon(Poly: TPointArray): Boolean;
begin
  Result := TSimbaGeometry.PointInPolygon(Self, Poly);
end;

function TPointHelper.InCircle(Center: TPoint; Radius: Double): Boolean;
begin
  Result := TSimbaGeometry.PointInCircle(Self, Center, Radius);
end;

function TPointHelper.InBox(Box: TBox): Boolean;
begin
  Result := TSimbaGeometry.PointInBox(Self, Box);
end;

function TPointHelper.DistanceTo(Other: TPoint): Double;
begin
  Result := Sqrt(Sqr(Double(Other.X) - Double(X)) + Sqr(Double(Other.Y) - Double(Y)));
end;

function TPointHelper.RotateFast(Degrees: Integer; Center: TPoint): TPoint;
begin
  Result := TSimbaGeometry.RotatePointFast(Self, Degrees, Center.X, Center.Y);
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
  Result.X := Self.X + System.Random(Value);
  Result.Y := Self.Y + System.Random(Value);
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

operator in(const Left: TPoint; const Right: TBox): Boolean;
begin
  Result := Left.InBox(Right);
end;

function Point(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

{$ENDIF}


