function GetColor(x,y : integer) : TColor;
begin;
  Result := CurrThread.Client.MWindow.GetColor(x,y);
end;

function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;
begin
  Result := CurrThread.Client.MFinder.FindColor(x, y, color, x1, y1, x2, y2);
end;

function findcolortolerance(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;
begin
  Result := CurrThread.Client.MFinder.FindColorTolerance(x, y, color, x1, y1, x2, y2, tol);
end;

function FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Integer): Boolean;
begin
  Result := CurrThread.Client.MFinder.FindColors(TPA, color, x1, y1, x2, y2);
end;

procedure SetColorToleranceSpeed(cts: Integer);
begin
  CurrThread.Client.MFinder.SetToleranceSpeed(cts);
end;

function SimilarColors(Col1,Col2,Tol : integer) : boolean;
begin;
  Result := CurrThread.Client.MFinder.SimilarColors(Col1,Col2,Tol);
end;

function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
begin;
  result := CurrThread.Client.MFinder.CountColorTolerance(color,xs,ys,xe,ye,tolerance);
end;

function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;
begin;
  result := CurrThread.Client.MFinder.FindColorsTolerance(points,color,xs,ys,xe,ye,tolerance);
end;

function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;
begin;
  result := CurrThread.Client.MFinder.FindColorSpiral(x,y,color,xs,ys,xe,ye);
end;

function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;
begin;
  result := CurrThread.Client.MFinder.FindColorsSpiralTolerance(x,y,Points,color,xs,ys,xe,ye,tolerance);
end;
