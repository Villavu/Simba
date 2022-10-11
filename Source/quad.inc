{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
 type
  PQuad = ^TQuad;
  TQuad = record
    Top: TPoint;
    Right: TPoint;
    Bottom: TPoint;
    Left: TPoint;

    class function Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad; static; overload;
    class function CreateFromBox(Box: TBox): TQuad; static; overload;
    class function CreateFromPoints(Points: TPointArray): TQuad; static; overload;
    class operator in(const P: TPoint; const Quad: TQuad): Boolean;

    function ToTPA: TPointArray;
    function Bounds: TBox;
    function ShortSideLen: Integer;
    function LongSideLen: Integer;
    function Mean: TPoint;
    function Rotate(Angle: Double): TQuad;
    function Contains(P: TPoint): Boolean; overload; inline;
    function Contains(X, Y: Integer): Boolean; overload; inline;
    function Offset(P: TPoint): TQuad; overload;
    function Offset(X, Y: Integer): TQuad; overload;
    function Filter(Points: TPointArray): TPointArray;
    function Expand(Amount: Double): TQuad;
    function NearestEdge(P: TPoint): TPoint;
    function Area: Integer;
    function Normalize: TQuad;
  end;
{$ENDIF}

{$IFDEF BODY}
class function TQuad.Create(ATop, ARight, ABottom, ALeft: TPoint): TQuad;
begin
  Result.Top    := ATop;
  Result.Right  := ARight;
  Result.Bottom := ABottom;
  Result.Left   := ALeft;
end;

class function TQuad.CreateFromBox(Box: TBox): TQuad;
begin
  Result.Top    := TPoint.Create(Box.X1, Box.Y1);
  Result.Right  := TPoint.Create(Box.X2, Box.Y1);
  Result.Bottom := TPoint.Create(Box.X2, Box.Y2);
  Result.Left   := TPoint.Create(Box.X1, Box.Y2);
end;

class function TQuad.CreateFromPoints(Points: TPointArray): TQuad;
begin
  Result := Points.MinAreaRect();
end;

class operator TQuad.in(const P: TPoint; const Quad: TQuad): Boolean;
begin
  Result := Quad.Contains(P);
end;

function TQuad.ToTPA: TPointArray;
begin
  Result := [Top, Right, Bottom, Left];
end;

function TQuad.Bounds: TBox;
begin
  Result := ToTPA().Bounds();
end;

function TQuad.ShortSideLen: Integer;
begin
  if (Hypot(Left.Y-Top.Y, Left.X-Top.X) < Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X)) then
    Result := Round(Hypot(Left.Y-Top.Y, Left.X-Top.X) / 2)
  else
    Result := Round(Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X) / 2);
end;

function TQuad.LongSideLen: Integer;
begin
  if (Hypot(Left.Y-Top.Y, Left.X-Top.X) > Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X)) then
    Result := Round(Hypot(Left.Y-Top.Y, Left.X-Top.X) / 2)
  else
    Result := Round(Hypot(Left.Y-Bottom.Y, Left.X-Bottom.X) / 2);
end;

function TQuad.Mean: TPoint;
begin
  Result.X := (Self.Top.X + Self.Right.X + Self.Bottom.X + Self.Left.X) div 4;
  Result.Y := (Self.Top.Y + Self.Right.Y + Self.Bottom.Y + Self.Left.Y) div 4;
end;

function TQuad.Rotate(Angle: Double): TQuad;
begin
  with Self.Mean() do
  begin
    Result.Top    := TSimbaGeometry.RotatePoint(Self.Top,    Angle, X, Y);
    Result.Right  := TSimbaGeometry.RotatePoint(Self.Right,  Angle, X, Y);
    Result.Bottom := TSimbaGeometry.RotatePoint(Self.Bottom, Angle, X, Y);
    Result.Left   := TSimbaGeometry.RotatePoint(Self.Left,   Angle, X, Y);
  end;

  Result := Result.Normalize();
end;

function TQuad.Contains(P: TPoint): Boolean;
begin
  Result := TSimbaGeometry.PointInQuad(P, Self.Top, Self.Right, Self.Bottom, Self.Left);
end;

function TQuad.Contains(X, Y: Integer): Boolean;
begin
  Result := TSimbaGeometry.PointInQuad(TPoint.Create(X, Y), Self.Top, Self.Right, Self.Bottom, Self.Left);
end;

function TQuad.Offset(P: TPoint): TQuad;
begin
  Result := TQuad.Create(Top.Offset(P), Right.Offset(P), Bottom.Offset(P), Left.Offset(P));
end;

function TQuad.Offset(X, Y: Integer): TQuad;
begin
  Result := TQuad.Create(Top.Offset(X, Y), Right.Offset(X, Y), Bottom.Offset(X, Y), Left.Offset(X, Y));
end;

function TQuad.Filter(Points: TPointArray): TPointArray;
var
  I, Count: Integer;
begin
  SetLength(Result, Length(Points));
  Count := 0;
  for I := 0 to High(Points) do
    if Contains(Points[I]) then
    begin
      Result[Count] := Points[I];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function TQuad.Expand(Amount: Double): TQuad;
var
  InTPA, OutTPA: array[0..3] of TPoint;
  Theta: Double;
  I, J: Integer;
begin
  InTPA[0] := Self.Top;
  InTPA[1] := Self.Right;
  InTPA[2] := Self.Bottom;
  InTPA[3] := Self.Left;

  Amount *= Sqrt(2);
  for I := 0 to 3 do
  begin
    J := (I+1) mod 4;
    Theta := ArcTan2(InTPA[I].Y - InTPA[J].Y, InTPA[I].X - InTPA[J].X) + PI/4;
    OutTPA[I].X := Round(InTPA[I].X + Amount * Cos(Theta));
    OutTPA[I].Y := Round(InTPA[I].Y + Amount * Sin(Theta));
  end;

  Result := TQuad.Create(OutTPA[0], OutTPA[1], OutTPA[2], OutTPA[3]);
end;

function TQuad.NearestEdge(P: TPoint): TPoint;
var
  Dists: array[0..3] of Double;
  Points: array[0..3] of TPoint;
  I: Integer;
  Best: Double;
begin
  Dists[0] := TSimbaGeometry.DistToLine(P, Self.Top,    Self.Left,   Points[0]);
  Dists[1] := TSimbaGeometry.DistToLine(P, Self.Left,   Self.Bottom, Points[1]);
  Dists[2] := TSimbaGeometry.DistToLine(P, Self.Bottom, Self.Right,  Points[2]);
  Dists[3] := TSimbaGeometry.DistToLine(P, Self.Right,  Self.Top,    Points[3]);

  Best   := Dists[0];
  Result := Points[0];

  for I := 1 to 3 do
    if (Dists[I] < Best) then
    begin
      Best   := Dists[I];
      Result := Points[I];
    end;
end;

function TQuad.Area: Integer;
begin
  Result := Round(Distance(Self.Bottom, Self.Right)) * Round(Distance(Self.Bottom, Self.Left));
end;

function TQuad.Normalize: TQuad;
var
  I, T: Integer;
  Points: TPointArray;
begin
  T := 0;
  Points := Self.ToTPA();
  for I := 1 to High(Points) do
    if (Points[I].Y < Points[T].Y) or ((Points[I].Y = Points[T].Y) and (Points[I].X < Points[T].X)) then
      T := I;

  Result.Top    := Points[T];
  Result.Right  := Points[(T + 1) mod 4];
  Result.Bottom := Points[(T + 2) mod 4];
  Result.Left   := Points[(T + 3) mod 4];
end;
{$ENDIF}


