{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildBoxEdgeAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildBoxEdgeAA(Box: TBox; Thickness, Feather: Single; const Clip: TBox);
var
  Band: TBox;
  X, XEnd, Y, Side, Margin, HoleX1, HoleX2, InteriorX1, InteriorX2, InteriorY1, InteriorY2: Integer;
  Grow, RampWidth, StrokeRadius, AlphaScale, DistL, DistR, DistY: Single;
  RampValue: Double;
  Alpha: Byte;
begin
  Assert(Thickness > 0);
  Grow         := Max(Feather, 0) * (Thickness * 0.5 + 0.5);
  RampWidth    := 1 + Grow;
  StrokeRadius := (Thickness + 2 * Grow) * 0.5 + 0.5;
  AlphaScale   := 255 / RampWidth;
  Margin       := Ceil(StrokeRadius + 1);

  Band := TBox.Create(
    Max(Clip.X1, Int64(Box.X1) - Margin), Max(Clip.Y1, Int64(Box.Y1) - Margin),
    Min(Clip.X2, Int64(Box.X2) + Margin), Min(Clip.Y2, Int64(Box.Y2) + Margin)
  );
  if (Band.X1 > Band.X2) or (Band.Y1 > Band.Y2) then
    Exit;

  InteriorX1 := Max(Band.X1, Int64(Box.X1) + Margin);
  InteriorX2 := Min(Band.X2, Int64(Box.X2) - Margin);
  InteriorY1 := Max(Band.Y1, Int64(Box.Y1) + Margin);
  InteriorY2 := Min(Band.Y2, Int64(Box.Y2) - Margin);

  for Y := Band.Y1 to Band.Y2 do
  begin
    DistY := Max(Single(Box.Y1) - Y, Y - Single(Box.Y2));

    HoleX1 := Band.X2 + 1;
    HoleX2 := Band.X2;
    if (Y >= InteriorY1) and (Y <= InteriorY2) and (InteriorX1 <= InteriorX2) then
    begin
      HoleX1 := InteriorX1;
      HoleX2 := InteriorX2;
    end;
    Assert((HoleX1 >= Band.X1) and (HoleX2 <= Band.X2));

    for Side := 0 to 1 do
    begin
      if (Side = 0) then
      begin
        X    := Band.X1;
        XEnd := HoleX1 - 1;
      end
      else
      begin
        X    := HoleX2 + 1;
        XEnd := Band.X2;
      end;

      DistL := Single(Box.X1) - X;
      DistR := X - Single(Box.X2);
      while (X <= XEnd) do
      begin
        RampValue := (StrokeRadius - Abs(Max(Max(DistL, DistR), DistY))) * AlphaScale;
        if (RampValue >= 255) then
          Alpha := 255
        else if (RampValue > 0) then
          Alpha := Round(RampValue)
        else
          Alpha := 0;
        _Pixel(X, Y, Alpha);

        X := X + 1;
        DistL := DistL - 1.0;
        DistR := DistR + 1.0;
      end;
    end;
  end;
end;

{$undef _Pixel}
