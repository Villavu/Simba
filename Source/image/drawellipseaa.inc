// https://zingl.github.io/bresenham.js

procedure DrawEllipseAA(x0, y0, x1, y1: Integer; Thickness: Single);
var
  a,b,b1: Integer;
  a2,b2: Single;
  dx,dy: Single;
  err: Single;
  dx2,dy2,e2,ed: Single;
  i: Single;
  Alpha: Byte;
begin
  a := Abs(x1 - x0);
  b := Abs(y1 - y0);
  b1 := b and 1;
  a2 := a-2*Thickness;
  b2 := b-2*Thickness;
  dx := 4*(a-1)*b*b;
  dy := 4*(b1-1)*a*a;

  i := a+b2;
  err := b1*a*a;

  if ((Thickness-1) * (2*b-Thickness) > a*a) then
    b2 := Sqrt(a*(b-a)*i*a2) / (a-Thickness);

  if ((Thickness-1) * (2*a-Thickness) > b*b) then
  begin
    a2 := Sqrt(b*(a-b)*i*b2) / (b-Thickness);
    Thickness := (a-a2) / 2;
  end;

  if (a = 0) or (b = 0) then
    Exit;

  if (x0 > x1) then
  begin
    x0 := x1;
    x1 += a;
  end;

  if (y0 > y1) then
    y0 := y1;

  if (b2 <= 0) then
    Thickness := a;

  e2 := Thickness - Floor(Thickness);
  Thickness := x0+Thickness-e2;
  dx2 := 4*(a2+2*e2-1)*b2*b2;
  dy2 := 4*(b1-1)*a2*a2;
  e2 := dx2*e2;
  y0 += (b+1) shr 1;
  y1 := y0-b1;
  a := 8*a*a;
  b1 := 8*b*b;
  a2 := 8*a2*a2;
  b2 := 8*b2*b2;

  repeat
    while True do
    begin
      if (err < 0) or (x0 > x1) then
      begin
        i := x0;
        Break;
      end;

      i := Min(dx,dy);
      ed := Max(dx,dy);

      if ((y0 = y1+1) and (2*err > dx) and (a > b1)) then
        ed := a/4
      else
        ed += 2*ed*i*i/(4*ed*ed+i*i+1)+1;
      i := 255*err/ed;

      Alpha := Byte(Round(i));
      PixelProc(x0,y0, Alpha);
      PixelProc(x0,y1, Alpha);
      PixelProc(x1,y0, Alpha);
      PixelProc(x1,y1, Alpha);

      if (err+dy+a < dx) then
      begin
        i := x0+1;
        Break;
      end;

      x0 += 1;
      x1 -= 1;
      err -= dx;
      dx -= b1;
    end;

    while (i < Thickness) and (2*i <= x0+x1) do
    begin
      PixelProc(Round(i), y0, 0);
      PixelProc(Round(x0+x1-i), y0, 0);
      PixelProc(Round(i), y1, 0);
      PixelProc(Round(x0+x1-i), y1, 0);

      i += 1.0;
    end;

    while ((e2 > 0) and (x0+x1 >= 2*Thickness)) do
    begin
       i := Min(dx2,dy2);
       ed := Max(dx2,dy2);

       if (y0 = y1+1) and (2*e2 > dx2) and (a2 > b2) then
         ed := a2/4
       else
         ed += 2*ed*i*i/(4*ed*ed+i*i);

       Alpha := Byte(Round(255-255*e2/ed));
       PixelProc(Round(Thickness), y0, Alpha);
       PixelProc(Round(x0+x1-Thickness), y0, Alpha);
       PixelProc(Round(Thickness), y1, Alpha);
       PixelProc(Round(x0+x1-Thickness), y1, Alpha);

       if (e2+dy2+a2 < dx2) then
         Break;

       Thickness += 1;
       e2 -= dx2;
       dx2 -= b2;
    end;

    dy2 += a2;
    e2 += dy2;
    y0 += 1;
    y1 -= 1;
    dy += a;
    err += dy;
  until (x0 >= x1);

  while (y0-y1 <= b) do
  begin
    Alpha := Byte(Round(255*4*err/b1));

    PixelProc(x0, y0, Alpha);
    PixelProc(x1, y0, Alpha);

    y0 += 1;

    PixelProc(x0, y1, Alpha);
    PixelProc(x1, y1, Alpha);

    y1 -= 1;
    dy += a;
    err += dy;
  end;
end;

