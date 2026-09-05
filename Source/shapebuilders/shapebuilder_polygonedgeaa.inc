{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildPolygonEdgeAA requires "_Pixel(X, Y: Integer; Alpha: Byte)"}
{$ENDIF}

procedure _BuildPolygonEdgeAA(const Poly: TPolygon; Thickness, Feather: Single; const Clip: TBox);
type
  TCapsule = record
    X1, Y1, DX, DY, LenSq, InvLenSq, Slope, HalfSpan: Single;
    XLo, XHi, YLo, YHi: Integer;
    FromCol, ToCol: Integer;
    RelX, RelY, Along, Perp: Single;
  end;
var
  Caps: array of TCapsule;
  ByRow, Active, Order: TIntegerArray;
  N, I, NextCap, First, ActiveIdx, Last, Count, ActiveCount, Row, Col, RunEnd, RowLo, RowHi: Integer;
  X2, Y2, Grow, RampWidth, Radius, RadiusSq, CoreSq, AlphaScale, Centre, DistSq, MinDistSq: Single;
begin
  Assert(Thickness > 0);
  N := Length(Poly);
  if (N < 2) then
    Exit;
  Grow       := Max(Feather, 0) * (Thickness * 0.5 + 0.5);
  RampWidth  := 1 + Grow;
  Radius     := Min((Thickness + 2 * Grow) * 0.5 + 0.5, 16384);
  RadiusSq   := Sqr(Radius);
  AlphaScale := 255 / RampWidth;
  CoreSq     := Radius - RampWidth;
  if (CoreSq < 0) then
    CoreSq := -1
  else
    CoreSq := Sqr(CoreSq);

  SetLength(Caps, N);
  SetLength(ByRow, N);
  SetLength(Active, N);
  SetLength(Order, N);
  RowLo := Clip.Y2 + 1;
  RowHi := Clip.Y1 - 1;
  for I := 0 to N - 1 do
    with Caps[I] do
    begin
      X1 := Poly[I].X;  Y1 := Poly[I].Y;
      X2 := Poly[(I + 1) mod N].X;  Y2 := Poly[(I + 1) mod N].Y;
      DX := X2 - X1;
      DY := Y2 - Y1;
      LenSq := Sqr(DX) + Sqr(DY);
      InvLenSq := 0;
      if (LenSq > 0) then
        InvLenSq := 1 / LenSq;
      Slope := 0;
      HalfSpan := 0;
      if (DY <> 0) then
      begin
        Slope    := DX / DY;
        HalfSpan := Radius * Sqrt(LenSq) / Abs(DY);
      end;
      XLo := TruncClamp(Min(X1, X2) - Radius,     Clip.X1,     Clip.X2 + 1);
      XHi := TruncClamp(Max(X1, X2) + Radius + 1, Clip.X1 - 1, Clip.X2);
      YLo := TruncClamp(Min(Y1, Y2) - Radius,     Clip.Y1,     Clip.Y2 + 1);
      YHi := TruncClamp(Max(Y1, Y2) + Radius + 1, Clip.Y1 - 1, Clip.Y2);
      RowLo := Min(RowLo, YLo);
      RowHi := Max(RowHi, YHi);
      First := I;
      while (First > 0) and (Caps[ByRow[First - 1]].YLo > YLo) do
      begin
        ByRow[First] := ByRow[First - 1];
        Dec(First);
      end;
      ByRow[First] := I;
    end;

  NextCap := 0;
  ActiveCount := 0;
  for Row := RowLo to RowHi do
  begin
    while (NextCap < N) and (Caps[ByRow[NextCap]].YLo <= Row) do
    begin
      Active[ActiveCount] := ByRow[NextCap];
      Inc(ActiveCount);
      Inc(NextCap);
    end;

    Count := 0;
    ActiveIdx := 0;
    while (ActiveIdx < ActiveCount) do
    begin
      I := Active[ActiveIdx];
      if (Row > Caps[I].YHi) then
      begin
        Dec(ActiveCount);
        Active[ActiveIdx] := Active[ActiveCount];
        Continue;
      end;
      Inc(ActiveIdx);
      with Caps[I] do
      begin
        if (DY <> 0) then
        begin
          Centre  := X1 + (Row - Y1) * Slope;
          FromCol := TruncClamp(Centre - HalfSpan,     XLo,     XHi + 1);
          ToCol   := TruncClamp(Centre + HalfSpan + 1, XLo - 1, XHi);
        end
        else
        begin
          FromCol := XLo;
          ToCol   := XHi;
        end;
        if (FromCol > ToCol) then
          Continue;
        RelY  := Row - Y1;
        RelX  := FromCol - X1;
        Along := RelX*DX + RelY*DY;
        Perp  := RelX*DY - RelY*DX;
        First := Count;
        while (First > 0) and (Caps[Order[First - 1]].FromCol > FromCol) do
        begin
          Order[First] := Order[First - 1];
          Dec(First);
        end;
        Order[First] := I;
        Inc(Count);
      end;
    end;

    First := 0;
    while (First < Count) do
    begin
      Col    := Caps[Order[First]].FromCol;
      RunEnd := Caps[Order[First]].ToCol;
      Last   := First;
      while (Last + 1 < Count) and (Caps[Order[Last + 1]].FromCol <= RunEnd + 1) do
      begin
        Inc(Last);
        RunEnd := Max(RunEnd, Caps[Order[Last]].ToCol);
      end;

      while (Col <= RunEnd) do
      begin
        MinDistSq := RadiusSq;
        for I := First to Last do
          with Caps[Order[I]] do
            if (Col >= FromCol) and (Col <= ToCol) then
            begin
              if (Along <= 0) then
                DistSq := Sqr(RelX) + Sqr(RelY)
              else if (Along >= LenSq) then
                DistSq := Sqr(RelX - DX) + Sqr(RelY - DY)
              else
                DistSq := Sqr(Perp) * InvLenSq;
              if (DistSq < MinDistSq) then
                MinDistSq := DistSq;
              RelX := RelX + 1.0;  Along := Along + DX;  Perp := Perp + DY;
            end;
        if (MinDistSq <= CoreSq) then
          _Pixel(Col, Row, 255)
        else if (MinDistSq < RadiusSq) then
          _Pixel(Col, Row, Round((Radius - Sqrt(MinDistSq)) * AlphaScale));
        Inc(Col);
      end;
      First := Last + 1;
    end;
  end;
end;

{$undef _Pixel}
