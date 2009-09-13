function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;
begin
  Result := CurrThread.Client.MFinder.FindColor(x, y, color, x1, y1, x2, y2);
end;

function findcolortolerance(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;
begin
  Result := CurrThread.Client.MFinder.FindColorTolerance(x, y, color, x1, y1, x2, y2, tol);
end;

procedure SetColorToleranceSpeed(cts: Integer);
begin
  CurrThread.Client.MFinder.SetToleranceSpeed(cts);
end;
