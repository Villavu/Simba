function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;
begin
  CurrThread.Client.MFinder.FindColor(x, y, color, x1, y1, x2, y2);
end;

procedure GetClientDimensions(var w, h: integer);
begin
  CurrThread.Client.MWindow.GetDimensions(w, h);
end;
