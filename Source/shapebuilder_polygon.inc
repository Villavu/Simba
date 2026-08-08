{$IF NOT DECLARED(_BuildLine)}
  {$ERROR _BuildPolygon requires "_BuildLine"}
{$ENDIF}

// Polygon outline: draw each edge as a line, closing the last vertex back to the first.
procedure _BuildPolygon(APoints: TPointArray);
var
  I: Integer;
begin
  if (Length(APoints) < 2) then
    Exit;

  for I := 0 to High(APoints) - 1 do
    _BuildLine(APoints[I], APoints[I + 1]);

  if (Length(APoints) > 2) and (APoints[High(APoints)] <> APoints[0]) then
    _BuildLine(APoints[High(APoints)], APoints[0]);
end;
