{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildLineAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildLineAA(X1, Y1, X2, Y2, Thickness, Feather: Single; const Clip: TBox);

  function TruncClamp(V: Single; Lo, Hi: Integer): Integer;
  begin
    if not (V > Lo) then
      Result := Lo
    else if (V >= Hi) then
      Result := Hi
    else
      Result := Trunc(V);
  end;

var
  Grow, RampWidth: Single;
  DX, DY, LenSq, InvLenSq, Radius, RadiusSq, CoreSq, AlphaScale, Slope, HalfSpan, Centre: Single;
  RelX, RelY, Along, Perp, DistSq: Single;
  Row, Col, FromCol, ToCol, BaseFromCol, BaseToCol: Integer;
begin
  Assert(Thickness > 0);

  Grow      := Max(Feather, 0) * (Thickness * 0.5 + 0.5);   // how far the feather grows the stroke
  RampWidth := 1 + Grow;

  DX := X2 - X1;
  DY := Y2 - Y1;
  LenSq := Sqr(DX) + Sqr(DY);
  InvLenSq := 0;
  if (LenSq > 0) then
    InvLenSq := 1 / LenSq;

  Radius := (Thickness + 2 * Grow) * 0.5 + 0.5;   // how far the stroke reaches from the segment
  if (Radius > 16384) then                 // the widest stroke honoured: keeps the alpha finite. Kept as an if: as a Min
    Radius := 16384;                       // the immediate host measured 20% slower with an IDENTICAL loop (code layout)
  RadiusSq := Sqr(Radius);
  AlphaScale := 255 / RampWidth;
  CoreSq := Radius - RampWidth;            // within this of the segment the alpha is full
  if (CoreSq < 0) then
    CoreSq := -1
  else
    CoreSq := Sqr(CoreSq);

  Slope := 0;
  HalfSpan := 0;
  if (DY <> 0) then
  begin
    Slope    := DX / DY;
    HalfSpan := Radius * Sqrt(LenSq) / Abs(DY);
  end;

  // The capsule's bounds within Clip (an empty range when it misses Clip)
  BaseFromCol := TruncClamp(Min(X1, X2) - Radius,     Clip.X1,     Clip.X2 + 1);
  BaseToCol   := TruncClamp(Max(X1, X2) + Radius + 1, Clip.X1 - 1, Clip.X2);

  for Row := TruncClamp(Min(Y1, Y2) - Radius, Clip.Y1, Clip.Y2 + 1) to TruncClamp(Max(Y1, Y2) + Radius + 1, Clip.Y1 - 1, Clip.Y2) do
  begin
    if (DY <> 0) then
    begin
      Centre  := X1 + (Row - Y1) * Slope;
      FromCol := TruncClamp(Centre - HalfSpan,     BaseFromCol,     BaseToCol + 1);
      ToCol   := TruncClamp(Centre + HalfSpan + 1, BaseFromCol - 1, BaseToCol);
    end
    else
    begin
      FromCol := BaseFromCol;
      ToCol   := BaseToCol;
    end;
    Assert((Row >= Clip.Y1) and (Row <= Clip.Y2) and (FromCol >= Clip.X1) and (ToCol <= Clip.X2));

    RelY  := Row - Y1;
    RelX  := FromCol - X1;
    Along := RelX*DX + RelY*DY;
    Perp  := RelX*DY - RelY*DX;
    Col := FromCol;
    while (Col <= ToCol) do
    begin
      if (Along <= 0) then
        DistSq := Sqr(RelX) + Sqr(RelY)
      else if (Along >= LenSq) then
        DistSq := Sqr(RelX-DX) + Sqr(RelY-DY)
      else
        DistSq := Sqr(Perp)*InvLenSq;

      if (DistSq <= CoreSq) then
        _Pixel(Col, Row, 255)
      else if (DistSq < RadiusSq) then
        _Pixel(Col, Row, Round((Radius - Sqrt(DistSq)) * AlphaScale));

      Col := Col + 1;  RelX := RelX + 1.0;
      Along := Along + DX;
      Perp := Perp + DY;
    end;
  end;
end;

{$undef _Pixel}
