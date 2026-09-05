{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildPolygonEdge requires "_Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}
{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildPolygonEdge requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildPolygonEdge(const Poly: TPointArray; Thickness: Integer; const Clip: TBox);

  procedure Narrow(C0, StepC: Double; Lo, Hi: Integer; var First, Last: Int64);
  var
    A, B: Double;
  begin
    if (StepC = 0) then
    begin
      if (C0 < Lo - 0.5) or (C0 > Hi + 0.5) then
        Last := First - 1;
      Exit;
    end;
    A := (Lo - 0.5 - C0) / StepC;
    B := (Hi + 0.5 - C0) / StepC;
    First := Max(First, Trunc(Max(Min(A, B), -1.0)));
    Last  := Min(Last,  Trunc(Min(Max(A, B), Last + 1.0)) + 1);
  end;

  procedure Edge(Start, Stop: TPoint);
  var
    DX, DY, Step, I, First, Last: Int64;
    StepX, StepY, Px, Py: Double;
  begin
    DX := Int64(Stop.X) - Start.X;
    DY := Int64(Stop.Y) - Start.Y;
    Step := Max(Abs(DX), Abs(DY));

    _Pixel(Start.X, Start.Y);
    if (Step = 0) then
      Exit;
    StepX := DX / Step;
    StepY := DY / Step;
    First := 1;
    Last  := Step;
    Px := Start.X;
    Py := Start.Y;
    if (Step > 4 * (Int64(Clip.X2) - Clip.X1 + Clip.Y2 - Clip.Y1 + 2)) then
    begin
      Narrow(Px, StepX, Clip.X1, Clip.X2, First, Last);
      Narrow(Py, StepY, Clip.Y1, Clip.Y2, First, Last);
      if (First > Last) then
        Exit;
      Px := Px + (First - 1) * StepX;
      Py := Py + (First - 1) * StepY;
    end;
    I := First;
    while (I <= Last) do
    begin
      Px := Px + StepX;
      Py := Py + StepY;
      _Pixel(Round(Px), Round(Py));
      Inc(I);
    end;
  end;

  procedure Capsule(Start, Stop: TPoint);
  var
    Ax, Ay, Bx, By, HalfThickness, PerpX, PerpY, Len, SlopeXY: Double;
    SideAX, SideAY, SideAYLo, SideAYHi, SideBX, SideBY, SideBYLo, SideBYHi: Double;
    Y, YHi: Integer;
    RowY, XLo, XHi, SideX, CapDy, CapHalf, ClipXLo, ClipXHi: Double;
  begin
    HalfThickness := Thickness / 2;
    Ax := Start.X; Ay := Start.Y;
    Bx := Stop.X;  By := Stop.Y;
    if (Max(Ax, Bx) + HalfThickness < Clip.X1) or (Min(Ax, Bx) - HalfThickness > Clip.X2) then
      Exit;

    Len := Sqrt(Sqr(Bx - Ax) + Sqr(By - Ay));
    PerpX := 0;
    PerpY := 0;
    if (Len > 0) then
    begin
      PerpX := -(By - Ay) / Len * HalfThickness;
      PerpY :=  (Bx - Ax) / Len * HalfThickness;
    end;

    SlopeXY := 0;
    SideAX := 0; SideAY := 0; SideAYLo := 1; SideAYHi := 0;
    SideBX := 0; SideBY := 0; SideBYLo := 1; SideBYHi := 0;
    if (Start.Y <> Stop.Y) then
    begin
      SlopeXY := (Bx - Ax) / (By - Ay);
      SideAX := Ax + PerpX;
      SideAY := Ay + PerpY;
      SideAYLo := Min(SideAY, By + PerpY);
      SideAYHi := Max(SideAY, By + PerpY);
      SideBX := Ax - PerpX;
      SideBY := Ay - PerpY;
      SideBYLo := Min(SideBY, By - PerpY);
      SideBYHi := Max(SideBY, By - PerpY);
    end;

    Y   := FloorInt(Max(Min(Ay, By) - HalfThickness, Double(Clip.Y1)));
    YHi := CeilInt(Min(Max(Ay, By) + HalfThickness, Double(Clip.Y2)));
    ClipXLo := Clip.X1 - 1.0;
    ClipXHi := Clip.X2 + 1.0;
    RowY := Y;
    while (Y <= YHi) do
    begin
      XLo := 1e300;
      XHi := -1e300;
      if (RowY >= SideAYLo) and (RowY <= SideAYHi) then
      begin
        SideX := SideAX + (RowY - SideAY) * SlopeXY;
        XLo := Min(XLo, SideX);  XHi := Max(XHi, SideX);
      end;
      if (RowY >= SideBYLo) and (RowY <= SideBYHi) then
      begin
        SideX := SideBX + (RowY - SideBY) * SlopeXY;
        XLo := Min(XLo, SideX);  XHi := Max(XHi, SideX);
      end;
      CapDy := RowY - Ay;
      if (Abs(CapDy) <= HalfThickness) then
      begin
        CapHalf := Sqrt((HalfThickness * HalfThickness) - (CapDy * CapDy));
        XLo := Min(XLo, Ax - CapHalf);  XHi := Max(XHi, Ax + CapHalf);
      end;
      CapDy := RowY - By;
      if (Abs(CapDy) <= HalfThickness) then
      begin
        CapHalf := Sqrt((HalfThickness * HalfThickness) - (CapDy * CapDy));
        XLo := Min(XLo, Bx - CapHalf);  XHi := Max(XHi, Bx + CapHalf);
      end;
      if (XLo <= XHi) then
        _Row(Y, RoundHalfUp(Max(XLo, ClipXLo)), RoundHalfDown(Min(XHi, ClipXHi)));

      Y := Y + 1;  RowY := RowY + 1.0;
    end;
  end;

var
  I: Integer;
begin
  if (Length(Poly) < 2) then
    Exit;
  if (Thickness <= 1) then
  begin
    for I := 0 to High(Poly) - 1 do
      Edge(Poly[I], Poly[I + 1]);
    if (Length(Poly) > 2) and (Poly[High(Poly)] <> Poly[0]) then
      Edge(Poly[High(Poly)], Poly[0]);
  end
  else
    for I := 0 to High(Poly) do
      Capsule(Poly[I], Poly[(I + 1) mod Length(Poly)]);
end;

{$undef _Row}
{$undef _Pixel}
