{$IF NOT DECLARED(_Row)}
  {$ERROR _BuildCircleThick requires "_Row"}
{$ENDIF}

procedure _BuildCircleThick(CenterX, CenterY, Radius, Thickness, ClipW, ClipH: Integer);
var
  Ro, Ri, DY, OuterX, InnerX: Double;
  Y, YLo, YHi, LX, RX: Integer;

  procedure DrawSpan(RowY, SLo, SHi: Integer);
  begin
    if (SLo < 0) then SLo := 0;
    if (SHi > ClipW - 1) then SHi := ClipW - 1;
    if (SLo <= SHi) then
      _Row(RowY, SLo, SHi);
  end;

begin
  if (Radius < 1) or (Thickness < 1) then
    Exit;

  Ro := Radius + (Thickness / 2); // outer radius
  Ri := Radius - (Thickness / 2); // inner radius (a filled disc once this reaches 0)

  YLo := Floor(CenterY - Ro); if (YLo < 0) then YLo := 0;
  YHi := Ceil (CenterY + Ro); if (YHi > ClipH - 1) then YHi := ClipH - 1;

  for Y := YLo to YHi do
  begin
    DY := Y - CenterY;
    if (Abs(DY) > Ro) then
      Continue;
    OuterX := Sqrt((Ro * Ro) - (DY * DY));

    if (Ri > 0) and (Abs(DY) < Ri) then // this row crosses the hole -> left and right spans
    begin
      InnerX := Sqrt((Ri * Ri) - (DY * DY));
      LX := Round(CenterX - InnerX);
      RX := Round(CenterX + InnerX);
      if (LX < RX) then
      begin
        DrawSpan(Y, Round(CenterX - OuterX), LX);
        DrawSpan(Y, RX, Round(CenterX + OuterX));
        Continue;
      end;
    end;

    DrawSpan(Y, Round(CenterX - OuterX), Round(CenterX + OuterX));
  end;
end;
