{$IF NOT DECLARED(_Pixel)}
  {$ERROR _BuildCircle requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

// https://zingl.github.io/bresenham.js
procedure _BuildCircle(CenterX, CenterY, Radius: Integer);
var
  X, Y, Err: Integer;
begin
  X := -Radius;
  Y := 0;

  Err := 2-2*Radius;

  while (X < 0) do
  begin
    _Pixel(CenterX - X, CenterY + Y);
    _Pixel(CenterX - Y, CenterY - X);
    _Pixel(CenterX + X, CenterY - Y);
    _Pixel(CenterX + Y, CenterY + X);

    Radius := Err;
    if (Radius <= Y) then
    begin
      Y += 1;
      Err += Y*2+1;
    end;

    if (Radius > X) or (Err > Y) then
    begin
      X += 1;
      Err += X*2+1;
    end;
  end;
end;
