{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildEllipseEdgeAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildEllipseEdgeAA(CenterXI, CenterYI, XRadius, YRadius: Integer; Thickness, Feather: Single; const Clip: TBox);
var
  Band: TBox;
  X, XEnd, Y, Side, Margin, OuterX1, OuterX2, InnerX1, InnerX2: Integer;
  Grow, StrokeRadius, RampWidth, AlphaScale: Single;
  CenterX, InvRadiusX, InvRadiusY, Dx, Dy, SignedDist, HalfChord: Single;
  InnerRadiusX, InnerRadiusY, OuterRadiusX, OuterRadiusY: Single;
  NormX, NormY, GradY: Single;
  RampValue: Double;
  Alpha: Byte;
begin
  Assert(Thickness > 0);
  if (XRadius < 1) or (YRadius < 1) then
    Exit;
  Grow := Max(Feather, 0) * (Thickness * 0.5 + 0.5);
  RampWidth := 1 + Grow;
  StrokeRadius := (Thickness + 2 * Grow) * 0.5 + 0.5;
  AlphaScale := 255 / RampWidth;
  Margin := Ceil(StrokeRadius + 1);

  Band := TBox.Create(
    Max(Clip.X1, Int64(CenterXI) - XRadius - Margin), Max(Clip.Y1, Int64(CenterYI) - YRadius - Margin),
    Min(Clip.X2, Int64(CenterXI) + XRadius + Margin), Min(Clip.Y2, Int64(CenterYI) + YRadius + Margin)
  );
  if (Band.X1 > Band.X2) or (Band.Y1 > Band.Y2) then
    Exit;
  CenterX := CenterXI;
  InvRadiusX := 1 / XRadius;  InvRadiusY := 1 / YRadius;
  InnerRadiusX := XRadius - (StrokeRadius + 1);  InnerRadiusY := YRadius - (StrokeRadius + 1);
  OuterRadiusX := XRadius + (StrokeRadius + 1);  OuterRadiusY := YRadius + (StrokeRadius + 1);

  for Y := Band.Y1 to Band.Y2 do
  begin
    Dy := Y - Single(CenterYI);

    OuterX1 := Band.X2 + 1;
    OuterX2 := Band.X2;
    if (Abs(Dy) < OuterRadiusY) then
    begin
      HalfChord := OuterRadiusX * Sqrt(1 - Sqr(Dy / OuterRadiusY));
      OuterX1 := FloorClamp(CenterX - HalfChord, Band.X1, Band.X2 + 1);
      OuterX2 := CeilClamp(CenterX + HalfChord, Band.X1 - 1, Band.X2);
    end;

    InnerX1 := OuterX2 + 1;
    InnerX2 := OuterX2;
    if (InnerRadiusX > 0) and (InnerRadiusY > 0) and (Abs(Dy) < InnerRadiusY) then
    begin
      HalfChord := InnerRadiusX * Sqrt(1 - Sqr(Dy / InnerRadiusY));
      InnerX1 := CeilClamp(CenterX - HalfChord, Band.X1, Band.X2 + 1);
      InnerX2 := FloorClamp(CenterX + HalfChord, Band.X1 - 1, Band.X2);
      if (InnerX1 > InnerX2) then
      begin
        InnerX1 := OuterX2 + 1;
        InnerX2 := OuterX2;
      end;
    end;
    Assert((OuterX1 >= Band.X1) and (OuterX2 <= Band.X2) and (OuterX1 <= InnerX1) and (InnerX2 <= OuterX2));

    NormY := Dy * InvRadiusY;
    GradY := Sqr(NormY * InvRadiusY);
    for Side := 0 to 1 do
    begin
      if (Side = 0) then
      begin
        X    := OuterX1;
        XEnd := InnerX1 - 1;
      end
      else
      begin
        X    := InnerX2 + 1;
        XEnd := OuterX2;
      end;

      Dx := X - CenterX;
      while (X <= XEnd) do
      begin
        NormX := Dx * InvRadiusX;
        SignedDist := NormX * NormX + NormY * NormY;
        if (SignedDist > 0) then
          SignedDist := (SignedDist - Sqrt(SignedDist)) / Sqrt(Sqr(NormX * InvRadiusX) + GradY)
        else
          SignedDist := -Min(XRadius, YRadius);
        RampValue := (StrokeRadius - Abs(SignedDist)) * AlphaScale;
        if (RampValue >= 255) then
          Alpha := 255
        else if (RampValue > 0) then
          Alpha := Round(RampValue)
        else
          Alpha := 0;
        _Pixel(X, Y, Alpha);
        X := X + 1;  Dx := Dx + 1.0;
      end;
    end;
  end;
end;

{$undef _Pixel}
