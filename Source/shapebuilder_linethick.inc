{$IF NOT DECLARED(_Row)}
  {$ERROR _BuildLineThick requires "_Row"}
{$ENDIF}

procedure _BuildLineThick(Start, Stop: TPoint; Thickness, ClipW, ClipH: Integer);
var
  Ax, Ay, Bx, By, R, PerpX, PerpY, Len, SlopeXY: Double;
  S1x, S1y, S1yLo, S1yHi, S2x, S2y, S2yLo, S2yHi: Double;
  Have: Boolean;
  Y, YLo, YHi, Lo, Hi: Integer;
  XLo, XHi, D, Wc: Double;

  procedure AddX(V: Double);
  begin
    if (not Have) then
    begin
      XLo := V; XHi := V; Have := True;
    end else
    if (V < XLo) then XLo := V
    else if (V > XHi) then XHi := V;
  end;

begin
  if (Thickness < 1) then
    Exit;

  R  := Thickness / 2;
  Ax := Start.X; Ay := Start.Y;
  Bx := Stop.X;  By := Stop.Y;

  Len := Hypot(Bx - Ax, By - Ay);
  if (Len = 0) then
  begin
    PerpX := 0;
    PerpY := 0;
  end else
  begin
    PerpX := -(By - Ay) / Len * R;
    PerpY :=  (Bx - Ax) / Len * R;
  end;

  if (Start.Y = Stop.Y) then
  begin
    // Horizontal segment: the long sides are horizontal, so the caps alone span every row.
    // Empty side ranges (Lo > Hi) disable the side tests below without a special case.
    SlopeXY := 0;
    S1x := 0; S1y := 0; S1yLo := 1; S1yHi := 0;
    S2x := 0; S2y := 0; S2yLo := 1; S2yHi := 0;
  end else
  begin
    SlopeXY := (Bx - Ax) / (By - Ay);       // both sides are parallel to the segment
    S1x := Ax + PerpX; S1y := Ay + PerpY;   // side 1 (+perp) base point
    S2x := Ax - PerpX; S2y := Ay - PerpY;   // side 2 (-perp) base point
    S1yLo := Min(Ay + PerpY, By + PerpY); S1yHi := Max(Ay + PerpY, By + PerpY);
    S2yLo := Min(Ay - PerpY, By - PerpY); S2yHi := Max(Ay - PerpY, By - PerpY);
  end;

  YLo := Floor(Min(Ay, By) - R); if (YLo < 0) then YLo := 0;
  YHi := Ceil (Max(Ay, By) + R); if (YHi > ClipH - 1) then YHi := ClipH - 1;

  for Y := YLo to YHi do
  begin
    Have := False;
    XLo := 0; XHi := 0;

    if (Y >= S1yLo) and (Y <= S1yHi) then AddX(S1x + (Y - S1y) * SlopeXY); // side 1 crossing
    if (Y >= S2yLo) and (Y <= S2yHi) then AddX(S2x + (Y - S2y) * SlopeXY); // side 2 crossing

    D := Y - Ay; // round cap disc at A
    if (Abs(D) <= R) then begin Wc := Sqrt((R * R) - (D * D)); AddX(Ax - Wc); AddX(Ax + Wc); end;
    D := Y - By; // round cap disc at B
    if (Abs(D) <= R) then begin Wc := Sqrt((R * R) - (D * D)); AddX(Bx - Wc); AddX(Bx + Wc); end;

    if Have then
    begin
      Lo := Round(XLo);
      Hi := Round(XHi);
      if (Lo < 0) then Lo := 0;
      if (Hi > ClipW - 1) then Hi := ClipW - 1;
      if (Lo <= Hi) then
        _Row(Y, Lo, Hi);
    end;
  end;
end;
