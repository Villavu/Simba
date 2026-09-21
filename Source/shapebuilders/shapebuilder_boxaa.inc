{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildBoxAA requires "_Row(Y, X1, X2: Integer; Alpha: Byte)"}
{$ENDIF}
{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildBoxAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildBoxAA(Box: TBox; Feather: Single; const Inverted: Boolean; const Clip: TBox);
var
  Band: TBox;
  X, XEnd, Y, Side, Margin, InteriorX1, InteriorX2: Integer;
  RampWidth, InvRampWidth, CenterX, CenterY, HalfWidth, HalfHeight, Dx, CovY, Cov: Single;
begin
  RampWidth := 1 + Max(Feather, 0);
  InvRampWidth := 1 / RampWidth;
  Margin := Ceil(RampWidth * 0.5) + 1;

  Band := TBox.Create(
    Max(Clip.X1, Int64(Box.X1) - Margin), Max(Clip.Y1, Int64(Box.Y1) - Margin),
    Min(Clip.X2, Int64(Box.X2) + Margin), Min(Clip.Y2, Int64(Box.Y2) + Margin)
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

  CenterX    := (Int64(Box.X1) + Box.X2) / 2;
  CenterY    := (Int64(Box.Y1) + Box.Y2) / 2;
  HalfWidth  := (Abs(Int64(Box.X2) - Box.X1) + 1) / 2;
  HalfHeight := (Abs(Int64(Box.Y2) - Box.Y1) + 1) / 2;

  InteriorX1 := Max(Band.X1, Int64(Box.X1) + Margin);
  InteriorX2 := Min(Band.X2, Int64(Box.X2) - Margin);
  if (InteriorX1 > InteriorX2) then
  begin
    InteriorX1 := Band.X2 + 1;
    InteriorX2 := Band.X2;
  end;

  for Y := Band.Y1 to Band.Y2 do
  begin
    CovY := Min(Max((HalfHeight - Abs(Y - CenterY)) * InvRampWidth + 0.5, 0), 1);
    if (CovY <= 0) then
    begin
      if Inverted then
        _Row(Y, Clip.X1, Clip.X2, 255);
      Continue;
    end;

    if Inverted then
    begin
      _Row(Y, Clip.X1, Band.X1 - 1, 255);
      _Row(Y, Band.X2 + 1, Clip.X2, 255);
    end;

    if (InteriorX1 <= InteriorX2) then
      if (not Inverted) then
        _Row(Y, InteriorX1, InteriorX2, Round(CovY * 255))
      else
        _Row(Y, InteriorX1, InteriorX2, Round((1 - CovY) * 255));

    for Side := 0 to 1 do
    begin
      if (Side = 0) then
      begin
        X    := Band.X1;
        XEnd := InteriorX1 - 1;
      end
      else
      begin
        X    := InteriorX2 + 1;
        XEnd := Band.X2;
      end;
      Assert((X >= Band.X1) and (XEnd <= Band.X2));

      Dx := X - CenterX;
      while (X <= XEnd) do
      begin
        Cov := Min(Max((HalfWidth - Abs(Dx)) * InvRampWidth + 0.5, 0), 1) * CovY;
        if Inverted then
          Cov := 1 - Cov;
        _Pixel(X, Y, Round(Cov * 255));

        X := X + 1;  Dx := Dx + 1.0;
      end;
    end;
  end;
end;

{$undef _Row}
{$undef _Pixel}
