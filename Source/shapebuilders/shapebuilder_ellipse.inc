{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildEllipse requires "_Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildEllipse(CenterX, CenterY, XRadius, YRadius: Integer; const Inverted: Boolean; const Clip: TBox);
const
  ScanLimit = 1 shl 20;
var
  HalfWidth, X1, X2, Y1, Y2, OldY, Y, YMax: Integer;
  XRadiusSq, YRadiusSq, Err, ErrX, ErrY: Int64;
  Dy, HalfChord: Double;
begin
  if (XRadius < 1) or (YRadius < 1) or (Int64(CenterX) + XRadius < Clip.X1) or (Int64(CenterX) - XRadius > Clip.X2) or
     (Int64(CenterY) + YRadius < Clip.Y1) or (Int64(CenterY) - YRadius > Clip.Y2) then
  begin
    if Inverted then
      for Y := Clip.Y1 to Clip.Y2 do
        _Row(Y, Clip.X1, Clip.X2);
    Exit;
  end;

  if (XRadius > ScanLimit) or (YRadius > ScanLimit) then
  begin
    for Y := Clip.Y1 to Clip.Y2 do
    begin
      Dy := Y - Double(CenterY);
      if (Abs(Dy) > YRadius) then
      begin
        if Inverted then
          _Row(Y, Clip.X1, Clip.X2);
        Continue;
      end;
      HalfChord := XRadius * Sqrt(1 - Sqr(Dy / YRadius));
      X1 := RoundHalfUp(Max(CenterX - HalfChord, Clip.X1 - 1.0));
      X2 := RoundHalfDown(Min(CenterX + HalfChord, Clip.X2 + 1.0));
      if (not Inverted) then
        _Row(Y, X1, X2)
      else
      begin
        _Row(Y, Clip.X1, X1 - 1);
        _Row(Y, X2 + 1, Clip.X2);
      end;
    end;
    Exit;
  end;

  YMax := CenterY + YRadius;
  if Inverted then
  begin
    for Y := Clip.Y1 to Min(CenterY - YRadius - 1, Clip.Y2) do
      _Row(Y, Clip.X1, Clip.X2);
    for Y := Max(YMax + 1, Clip.Y1) to Clip.Y2 do
      _Row(Y, Clip.X1, Clip.X2);
  end;

  HalfWidth := XRadius;
  X1 := CenterX - HalfWidth;
  X2 := CenterX + HalfWidth;
  Y1 := CenterY;
  Y2 := CenterY;
  OldY := Y1;

  XRadiusSq := Int64(HalfWidth) * HalfWidth;
  YRadiusSq := Int64(YRadius) * YRadius;
  Err  := Int64(HalfWidth) * YRadius * YRadius;
  ErrX := Err;
  ErrY := 0;

  if (not Inverted) then
    _Row(Y1, X1, X2)
  else
  begin
    _Row(Y1, Clip.X1, X1 - 1);
    _Row(Y1, X2 + 1, Clip.X2);
  end;

  while (HalfWidth > 0) do
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

    if (Y1 <> OldY) and (Y1 <= YMax) then
    begin
      if (not Inverted) then
      begin
        _Row(Y1, X1, X2);
        _Row(Y2, X1, X2);
      end
      else
      begin
        _Row(Y1, Clip.X1, X1 - 1);
        _Row(Y1, X2 + 1, Clip.X2);
        _Row(Y2, Clip.X1, X1 - 1);
        _Row(Y2, X2 + 1, Clip.X2);
      end;
      OldY := Y1;
    end;
  end;

  if Inverted then
    for Y := OldY + 1 to YMax do
    begin
      _Row(Y, Clip.X1, Clip.X2);
      _Row(2 * CenterY - Y, Clip.X1, Clip.X2);
    end;
end;

{$undef _Row}
