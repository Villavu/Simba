{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildEllipseEdge requires "_Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}
{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildEllipseEdge requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildEllipseEdge(CenterX, CenterY, XRadius, YRadius, Thickness: Integer; const Clip: TBox);
const
  WalkLimit = 1 shl 20;
var
  HalfWidth, X1, X2, Y1, Y2, Y, YHi, HoleX1, HoleX2: Integer;
  XRadiusSq, YRadiusSq, Err, ErrX, ErrY: Int64;
  OuterRadiusX, OuterRadiusY, InnerRadiusX, InnerRadiusY, Dy, OuterX, InnerX, ClipXLo, ClipXHi: Double;
begin
  if (Int64(CenterX) + XRadius < Int64(Clip.X1) - 1) or (Int64(CenterX) - XRadius > Int64(Clip.X2) + 1) or
     (Int64(CenterY) + YRadius < Int64(Clip.Y1) - 1) or (Int64(CenterY) - YRadius > Int64(Clip.Y2) + 1) then
    if (Thickness <= 1) then
      Exit;

  if (Thickness <= 1) and (XRadius <= WalkLimit) and (YRadius <= WalkLimit) then
  begin
    HalfWidth := XRadius;
    X1 := CenterX - HalfWidth;
    X2 := CenterX + HalfWidth;
    Y1 := CenterY;
    Y2 := CenterY;

    XRadiusSq := Int64(HalfWidth) * HalfWidth;
    YRadiusSq := Int64(YRadius) * YRadius;
    Err  := Int64(HalfWidth) * YRadius * YRadius;
    ErrX := Err;
    ErrY := 0;

    _Pixel(X1, Y1);
    _Pixel(X2, Y1);
    _Pixel(CenterX, CenterY - YRadius);
    _Pixel(CenterX, CenterY + YRadius);

    while (HalfWidth > 1) do
    begin
      if (Err > 0) then
      begin
        Inc(Y1);
        Dec(Y2);
        Inc(ErrY, XRadiusSq);
        Dec(Err, ErrY);
      end;
      if (Err <= 0) then
      begin
        Dec(HalfWidth);
        Inc(X1);
        Dec(X2);
        Dec(ErrX, YRadiusSq);
        Inc(Err, ErrX);
      end;

      _Pixel(X1, Y1);
      _Pixel(X2, Y1);
      _Pixel(X1, Y2);
      _Pixel(X2, Y2);
    end;
    Exit;
  end;

  if (XRadius < 1) or (YRadius < 1) then
    Exit;
  Thickness := Max(Thickness, 1);
  OuterRadiusX := XRadius + (Thickness / 2);  OuterRadiusY := YRadius + (Thickness / 2);
  InnerRadiusX := XRadius - (Thickness / 2);  InnerRadiusY := YRadius - (Thickness / 2);
  if (CenterX + OuterRadiusX < Clip.X1) or (CenterX - OuterRadiusX > Clip.X2) then
    Exit;

  ClipXLo := Clip.X1 - 1.0;
  ClipXHi := Clip.X2 + 1.0;
  Y   := FloorInt(Max(CenterY - OuterRadiusY, Double(Clip.Y1)));
  YHi := CeilInt(Min(CenterY + OuterRadiusY, Double(Clip.Y2)));
  Dy  := Y - CenterY;
  while (Y <= YHi) do
  begin
    if (Abs(Dy) <= OuterRadiusY) then
    begin
      OuterX := OuterRadiusX * Sqrt(1 - (Dy * Dy) / (OuterRadiusY * OuterRadiusY));
      HoleX1 := 0;                      // the hole on this row, when the row has one
      HoleX2 := 0;
      if (InnerRadiusX > 0) and (InnerRadiusY > 0) and (Abs(Dy) < InnerRadiusY) then
      begin
        InnerX := InnerRadiusX * Sqrt(1 - (Dy * Dy) / (InnerRadiusY * InnerRadiusY));
        HoleX1 := RoundHalfDown(Max(CenterX - InnerX, ClipXLo));
        HoleX2 := RoundHalfUp(Min(CenterX + InnerX, ClipXHi));
      end;
      if (HoleX1 < HoleX2) then
      begin
        _Row(Y, RoundHalfUp(Max(CenterX - OuterX, ClipXLo)), HoleX1);
        _Row(Y, HoleX2, RoundHalfDown(Min(CenterX + OuterX, ClipXHi)));
      end
      else
        _Row(Y, RoundHalfUp(Max(CenterX - OuterX, ClipXLo)), RoundHalfDown(Min(CenterX + OuterX, ClipXHi)));
    end;
    Y := Y + 1;  Dy := Dy + 1.0;
  end;
end;

{$undef _Row}
{$undef _Pixel}
