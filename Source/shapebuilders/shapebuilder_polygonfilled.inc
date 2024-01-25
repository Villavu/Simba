{$IF NOT DECLARED(_Row)}
  {$ERROR _PolygonFilled requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

(*
  Calculates the intersection points of a vertical (ACoordIsX = true) or horizontal
  (ACoordIsX = false) line with border of the polygon specified by APoints.
  Returns the coordinates of the intersection points.
*)
function GetLinePolygonIntersectionPoints(const ACoord: Double; const APoints: TPointFArray; const ACoordIsX: Boolean): TPointFArray;
var
  I, Count: Integer;
  Arr: TDoubleArray;
  dx, dy: Double;
begin
  SetLength(Arr, Length(APoints));
  Count := 0;

  if ACoordIsX then
  begin
    for I := 0 to High(APoints) - 1 do
      if ((APoints[I].X <= ACoord) and (ACoord < APoints[I+1].X)) or
         ((APoints[I+1].X <= ACoord) and (ACoord < APoints[I].X)) then
      begin
        dx := APoints[I+1].X - APoints[I].X;
        dy := APoints[I+1].Y - APoints[I].Y;

        Arr[Count] := APoints[I].Y + (ACoord - APoints[I].X) * dy / dx;
        Inc(Count);
      end;
  end else
  begin
    for I := 0 to High(APoints) - 1 do
      if ((APoints[I].Y <= ACoord) and (ACoord < APoints[I+1].Y)) or
         ((APoints[I+1].Y <= ACoord) and (ACoord < APoints[I].Y)) then
      begin
        dy := APoints[I+1].Y - APoints[I].Y;
        dx := APoints[I+1].X - APoints[I].X;

        Arr[Count] := APoints[I].X + (ACoord - APoints[I].Y) * dx / dy;
        Inc(Count);
      end;
  end;

  SetLength(Result, Count);
  if (Count = 0) then
    Exit;

  specialize QuickSort<Double>(Arr, 0, Count - 1);

  if ACoordIsX then
    for I := 0 to High(Result) do
    begin
      Result[I].X := ACoord;
      Result[I].Y := Arr[I];
    end
  else
    for I := 0 to High(Result) do
    begin
      Result[I].X := Arr[I];
      Result[I].Y := ACoord;
    end;
end;

(*
  Fills a polygon with the color of the current brush. The routine can handle
  non-contiguous polygons (holes!) correctly using the ScanLine algorithm and
  the even-odd rule
  http://www.tutorialspoint.com/computer_graphics/polygon_filling_algorithm
*)
procedure _BuildPolygonFilled(APoints: TPointArray; ARect: TRect);
var
  scanlineY, scanLineY1, scanLineY2: Integer;
  lPoints, pts: TPointFArray;
  I: Integer;
begin
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

  // Prepare points as needed by the GetLinePolygonIntersectionPoints procedure
  SetLength(pts, Length(APoints));
  for I := 0 to High(APoints) do
  begin
    pts[I].x := APoints[I].X;
    pts[I].y := APoints[I].Y;
  end;

  // Fill polygon by drawing horizontal line segments
  scanlineY := scanlineY1;
  while (scanlineY <= scanlineY2) do
  begin
    // Find intersection points of horizontal scan line with polygon
    // with polygon
    lPoints := GetLinePolygonIntersectionPoints(scanlineY, pts, false);
    if Length(lPoints) < 2 then
    begin
      Inc(scanlineY);
      Continue;
    end;
    // Draw lines between intersection points, skip every second pair
    I := 0;
    while I < High(lPoints) do
    begin
      _Row(Round(lPoints[I].Y), Round(lPoints[I].X), Round(lPoints[I+1].X));
      Inc(I, 2);
    end;
    // Proceed to next scan line
    Inc(scanlineY);
  end;
end;

