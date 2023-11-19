// https://zingl.github.io/bresenham.js
// Requires _SetPixelAntialias(const X, Y: Integer; const Alpha: Byte);

procedure _LineAntialias(x0, y0, x1, y1: Integer; Thickness: Single);
var
  dx, dy, err: Integer;
  e2, x2, y2: Integer;
  ed: Single;
  sx, sy: Integer;
begin
  dx := Abs(x1 - x0);
  dy := Abs(y1 - y0);

  if (x0 < x1) then sx := 1 else sx := -1;
  if (y0 < y1) then sy := 1 else sy := -1;

  err := dx-dy;
  if (dx+dy = 0) then
    ed := 1
  else
    ed := Sqrt(Double(dx*dx) + Double(dy*dy));

  Thickness := (Thickness + 1) / 2;
  while True do
  begin
    _SetPixelAntialias(x0, y0, Round(Max(0, 255 * (Abs(err-dx+dy)/ed-Thickness+1))));

    e2 := err;
    x2 := x0;
    if (2*e2 >= -dx) then
    begin
      e2 += dy;
      y2 := y0;
      while (e2 < ed*Thickness) and ((y1 <> y2) or (dx > dy)) do
      begin
        y2 += sy;
        _SetPixelAntialias(x0, y2, Round(Max(0, 255 * (Abs(e2)/ed-Thickness+1))));
        e2 += dx;
      end;
      if (x0 = x1) then
        Break;

      e2 := err;
      err -= dy;
      x0 += sx;
    end;

    if (2*e2 <= dy) then
    begin
      e2 := dx-e2;
      while (e2 < ed*Thickness) and ((x1 <> x2) or (dx < dy)) do
      begin
        x2 += sx;
        _SetPixelAntialias(x2, y0, Round(Max(0, 255 * (Abs(e2)/ed-Thickness+1))));
        e2 += dy;
      end;
      if (y0 = y1) then
        Break;

      err += dx;
      y0 += sy;
    end;
  end;
end;

