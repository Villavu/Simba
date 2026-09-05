{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildEllipseAA requires "_Row(Y, X1, X2: Integer; Alpha: Byte)"}
{$ENDIF}
{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildEllipseAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildEllipseAA(CenterXI, CenterYI, XRadius, YRadius: Integer; Feather: Single; const Inverted: Boolean; const Clip: TBox);
var
  Band: TBox;
  X, XEnd, Y, Side, Margin, OuterX1, OuterX2, InnerX1, InnerX2: Integer;
  RampWidth, InvRampWidth, OuterX, OuterY, InnerX, InnerY, CenterX, InvRadiusX, InvRadiusY, InvRadiusXSq: Single;
  Dx, Dy, HalfChord, NormXSq, NormYSq, GradY, Cov: Single;
begin
  RampWidth := 1 + Max(Feather, 0);
  InvRampWidth := 1 / RampWidth;
  Margin := Ceil(RampWidth * 0.5) + 1;


  Band := TBox.Create(Clip.X2 + 1, Clip.Y2 + 1, Clip.X2, Clip.Y2);
  if (XRadius >= 1) and (YRadius >= 1) then
    Band := TBox.Create(
      Max(Clip.X1, Int64(CenterXI) - XRadius - Margin), Max(Clip.Y1, Int64(CenterYI) - YRadius - Margin),
      Min(Clip.X2, Int64(CenterXI) + XRadius + Margin), Min(Clip.Y2, Int64(CenterYI) + YRadius + Margin)
    );
  if (Band.X1 > Band.X2) or (Band.Y1 > Band.Y2) then
  begin
    if Inverted then
      for Y := Clip.Y1 to Clip.Y2 do
        _Row(Y, Clip.X1, Clip.X2, 255);
    Exit;
  end;
  if Inverted then
  begin
    for Y := Clip.Y1 to Band.Y1 - 1 do
      _Row(Y, Clip.X1, Clip.X2, 255);
    for Y := Band.Y2 + 1 to Clip.Y2 do
      _Row(Y, Clip.X1, Clip.X2, 255);
  end;

  CenterX    := CenterXI;
  InvRadiusX := 1 / XRadius;
  InvRadiusY := 1 / YRadius;
  InvRadiusXSq := Sqr(InvRadiusX);
  OuterX := Single(XRadius) + Margin;  OuterY := Single(YRadius) + Margin;
  InnerX := XRadius - Margin;          InnerY := YRadius - Margin;

  for Y := Band.Y1 to Band.Y2 do
  begin
    Dy := Y - Single(CenterYI);

    OuterX1 := Band.X2 + 1;
    OuterX2 := Band.X2;
    if (Abs(Dy) < OuterY) then
    begin
      HalfChord := OuterX * Sqrt(1 - Sqr(Dy / OuterY));
      OuterX1 := FloorClamp(CenterX - HalfChord, Band.X1, Band.X2 + 1);
      OuterX2 := CeilClamp(CenterX + HalfChord, Band.X1 - 1, Band.X2);
    end;

    InnerX1 := OuterX2 + 1;
    InnerX2 := OuterX2;
    if (InnerX > 0) and (Abs(Dy) < InnerY) then
    begin
      HalfChord := InnerX * Sqrt(1 - Sqr(Dy / InnerY));
      InnerX1 := CeilClamp(CenterX - HalfChord, Band.X1, Band.X2 + 1);
      InnerX2 := FloorClamp(CenterX + HalfChord, Band.X1 - 1, Band.X2);
      if (InnerX1 > InnerX2) then
      begin
        InnerX1 := OuterX2 + 1;
        InnerX2 := OuterX2;
      end;
    end;
    Assert((OuterX1 >= Band.X1) and (OuterX2 <= Band.X2) and (OuterX1 <= InnerX1) and (InnerX2 <= OuterX2));

    if (not Inverted) then
    begin
      if (InnerX1 <= InnerX2) then
        _Row(Y, InnerX1, InnerX2, 255);
    end
    else
    begin
      _Row(Y, Clip.X1, OuterX1 - 1, 255);
      _Row(Y, OuterX2 + 1, Clip.X2, 255);
    end;

    NormYSq := Sqr(Dy * InvRadiusY);          // the row's share of the field and of its gradient
    GradY   := NormYSq * Sqr(InvRadiusY);
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
        NormXSq := Sqr(Dx * InvRadiusX);
        Cov := NormXSq + NormYSq;
        if (Cov > 0) then
          Cov := (Cov - Sqrt(Cov)) / Sqrt(NormXSq * InvRadiusXSq + GradY)
        else
          Cov := -Min(XRadius, YRadius);
        Cov := Min(Max(0.5 - Cov * InvRampWidth, 0), 1);
        if Inverted then
          Cov := 1 - Cov;
        _Pixel(X, Y, Round(Cov * 255));

        X := X + 1;
        Dx := Dx + 1.0;
      end;
    end;
  end;
end;

{$undef _Row}
{$undef _Pixel}
