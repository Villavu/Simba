{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildPointsAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildPointsAA(const Points: TPointArray; Thickness, Feather: Single; const Clip: TBox);
var
  Grow, RampWidth, Radius, RadiusSq, CoreSq, AlphaScale: Single;
  RelX, RelYSq, DistSq: Single;
  I, X, Y, Size, Span, Row, Col, ToCol: Integer;
begin
  Assert(Thickness > 0);

  Grow := Max(Feather, 0) * Thickness * 0.5;
  RampWidth := 1 + Grow;
  Radius := (Thickness + 2 * Grow) * 0.5 + 0.5;
  if (Radius < 1.25) then
    Radius := 1.25;
  if (Radius > 16384) then
    Radius := 16384;
  RadiusSq := Sqr(Radius);
  AlphaScale := 255 / RampWidth;
  CoreSq := Radius - RampWidth;
  if (CoreSq < 0) then
    CoreSq := -1
  else
    CoreSq := Sqr(CoreSq);

  if (Radius = 1.25) and (RampWidth = 1) then
  begin
    Size := Round((Radius - 1) * AlphaScale);
    for I := 0 to High(Points) do
    begin
      X := Points[I].X;
      Y := Points[I].Y;
      if (Y >= Clip.Y1) and (Y <= Clip.Y2) then
      begin
        if (X >= Clip.X1) and (X <= Clip.X2) then
          _Pixel(X, Y, 255);
        if (X > Clip.X1) and (X - 1 <= Clip.X2) then
          _Pixel(X - 1, Y, Size);
        if (X + 1 >= Clip.X1) and (X < Clip.X2) then
          _Pixel(X + 1, Y, Size);
      end;
      if (X >= Clip.X1) and (X <= Clip.X2) then
      begin
        if (Y > Clip.Y1) and (Y - 1 <= Clip.Y2) then
          _Pixel(X, Y - 1, Size);
        if (Y + 1 >= Clip.Y1) and (Y < Clip.Y2) then
          _Pixel(X, Y + 1, Size);
      end;
    end;
  end else
  begin
    Size := Trunc(Radius);
    for I := 0 to High(Points) do
    begin
      X := Points[I].X;
      Y := Points[I].Y;

      for Row := Max(Int64(Y) - Size, Clip.Y1) to Min(Int64(Y) + Size, Clip.Y2) do
      begin
        RelYSq := Sqr(Single(Row - Y));
        Span   := Trunc(Sqrt(RadiusSq - RelYSq)) + 1;
        Col    := Max(Int64(X) - Span, Clip.X1);
        ToCol  := Min(Int64(X) + Span, Clip.X2);

        RelX := Col - X;
        while (Col <= ToCol) do
        begin
          DistSq := Sqr(RelX) + RelYSq;
          if (DistSq <= CoreSq) then
            _Pixel(Col, Row, 255)
          else if (DistSq < RadiusSq) then
          begin
            DistSq := Sqrt(DistSq);
            _Pixel(Col, Row, Round((Radius - DistSq) * AlphaScale));
          end;

          Col := Col + 1;
          RelX := RelX + 1.0;
        end;
      end;
    end;
  end;
end;

{$undef _Pixel}
