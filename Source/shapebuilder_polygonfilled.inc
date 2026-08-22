{$IF NOT DECLARED(_Row)}
  {$ERROR _BuildPolygonFilled requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

function GetScanlineCrossings(const ACoord: Integer; const APoints: TPointArray; var Crossings: TIntegerArray): Integer;
var
  I, J, D: Integer;
  dx, dy: Double;
begin
  Result := 0;
  for I := 0 to High(APoints) - 1 do
    if ((APoints[I].Y <= ACoord) and (ACoord < APoints[I+1].Y)) or
       ((APoints[I+1].Y <= ACoord) and (ACoord < APoints[I].Y)) then
    begin
      dy := APoints[I+1].Y - APoints[I].Y;
      dx := APoints[I+1].X - APoints[I].X;

      Crossings[Result] := Round(APoints[I].X + (ACoord - APoints[I].Y) * dx / dy);
      Inc(Result);
    end;

  // Crossings are almost always 2 else insertion sort.
  if (Result = 2) then
  begin
    if (Crossings[0] > Crossings[1]) then
    begin
      D := Crossings[0];
      Crossings[0] := Crossings[1];
      Crossings[1] := D;
    end;
  end
  else
    for I := 1 to Result - 1 do
    begin
      D := Crossings[I];
      J := I - 1;
      while (J >= 0) and (Crossings[J] > D) do
      begin
        Crossings[J + 1] := Crossings[J];
        Dec(J);
      end;
      Crossings[J + 1] := D;
    end;
end;

(*
  Fills a polygon with the current colour using the scan-line algorithm and the
  even-odd rule, so non-convex polygons and holes fill correctly.
  https://www.tutorialspoint.com/computer_graphics/polygon_filling_algorithm.html
*)
procedure _BuildPolygonFilled(APoints: TPointArray; ARect: TRect; AOffset: TPoint);
var
  scanlineY, scanLineY1, scanLineY2: Integer;
  Crossings: TIntegerArray;
  I, Count, x1, prevEnd: Integer;
  yMin, yMax: Integer;
begin
  if (Length(APoints) < 3) then // a polygon needs at least 3 vertices
    Exit;

  // Make sure not to modify the input
  APoints := Copy(APoints);

  if (APoints[0] <> APoints[High(APoints)]) then
  begin
    SetLength(APoints, Length(APoints) + 1);
    APoints[High(APoints)] := APoints[0];
  end;

  if (ARect.Top < ARect.Bottom) then
  begin
    scanLineY1 := ARect.Top;
    scanLineY2 := ARect.Bottom;
  end else
  begin
    scanLineY1 := ARect.Bottom;
    scanLineY2 := ARect.Top;
  end;

  // Apply the offset and track the polygon's Y extent
  yMin := APoints[0].Y + AOffset.Y;
  yMax := yMin;
  for I := 0 to High(APoints) do
  begin
    APoints[I].X := APoints[I].X + AOffset.X;
    APoints[I].Y := APoints[I].Y + AOffset.Y;
    if (APoints[I].Y < yMin) then yMin := APoints[I].Y;
    if (APoints[I].Y > yMax) then yMax := APoints[I].Y;
  end;

  if (yMin > scanLineY1) then scanLineY1 := yMin;
  if (yMax < scanLineY2) then scanLineY2 := yMax;

  SetLength(Crossings, Length(APoints));

  scanlineY := scanLineY1;
  while (scanlineY <= scanLineY2) do
  begin
    Count := GetScanlineCrossings(scanlineY, APoints, Crossings);

    // Fill between successive crossing pairs (even-odd rule). Where a span starts on
    // the previous span's end pixel, skip that shared pixel so it isn't drawn twice.
    I := 0;
    prevEnd := Low(Integer);
    while (I < Count - 1) do
    begin
      x1 := Crossings[I];
      if (x1 <= prevEnd) then
        x1 := prevEnd + 1;
      if (x1 <= Crossings[I+1]) then
        _Row(scanlineY, x1, Crossings[I+1]);

      prevEnd := Crossings[I+1];
      Inc(I, 2);
    end;

    Inc(scanlineY);
  end;
end;
