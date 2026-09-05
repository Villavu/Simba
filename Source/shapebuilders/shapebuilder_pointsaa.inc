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

  Grow      := Max(Feather, 0) * (Thickness * 0.5 + 0.5);
  RampWidth := 1 + Grow;

  Radius := (Thickness + 2 * Grow) * 0.5 + 0.5;
  if (Radius > 16384) then
    Radius := 16384;
  RadiusSq := Sqr(Radius);
  AlphaScale := 255 / RampWidth;
  CoreSq := Radius - RampWidth;
  if (CoreSq < 0) then
    CoreSq := -1
  else
    CoreSq := Sqr(CoreSq);

  if (Radius <= 1) then
  begin
    for I := 0 to High(Points) do
      if Clip.Contains(Points[I]) then
        _Pixel(Points[I].X, Points[I].Y, 255);
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
