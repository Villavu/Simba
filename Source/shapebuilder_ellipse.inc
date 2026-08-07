{$IF NOT DECLARED(_Pixel)}
  {$ERROR _BuildEllipse requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildEllipse(centerX, centerY, radiusX, radiusY: Integer);
var
 RadX, RadY: Integer;
 DeltaX, DeltaY, R, RX, RY: Int64;
 X1, X2, Y1, Y2: Integer;
begin
  X1 := CenterX - RadiusX;
  X2 := CenterX + RadiusX;
  Y1 := CenterY - RadiusY;
  Y2 := CenterY + RadiusY;

  RadX := (X2 - X1) div 2;
  RadY := (Y2 - Y1) div 2;

  Y1 := Y1 + RadY;
  Y2 := Y1;

  DeltaX := Int64(RadX) * RadX;
  DeltaY := Int64(RadY) * RadY;
  R  := Int64(RadX) * RadY * RadY;
  RX := R;
  RY := 0;

  _Pixel(X1, Y1);
  _Pixel(X2, Y1);
  _Pixel(X1 + RadX, CenterY - RadY);
  _Pixel(X1 + RadX, CenterY + RadY);

  while RadX > 1 do
  begin
    if R > 0 then
    begin
      Inc(Y1);
      Dec(Y2);
      Inc(RY, DeltaX);
      Dec(R, RY);
    end;
    if R <= 0 then
    begin
      Dec(RadX);
      Inc(X1);
      Dec(X2);
      Dec(RX, DeltaY);
      Inc(R, RX);
    end;

    _Pixel(X1, Y1);
    _Pixel(X2, Y1);
    _Pixel(X1, Y2);
    _Pixel(X2, Y2);
  end;
end;
