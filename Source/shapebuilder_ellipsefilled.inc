{$IF NOT DECLARED(_Row)}
  {$ERROR _BuildEllipseFilled requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildEllipseFilled(centerX, centerY, radiusX, radiusY: Integer);
var
  RadX, RadY, DeltaX, DeltaY, R, RX, RY: Integer;
  X1, X2, Y1, Y2, OldY: Integer;
begin
  X1 := CenterX - RadiusX;
  X2 := CenterX + RadiusX;
  Y1 := CenterY - RadiusY;
  Y2 := CenterY + RadiusY;

  RadX := (X2 - X1) div 2;
  RadY := (Y2 - Y1) div 2;

  Y1 := Y1 + RadY;
  Y2 := Y1;
  OldY := Y1;

  DeltaX := (RadX * RadX);
  DeltaY := (RadY * RadY);
  R  := RadX * RadY * RadY;
  RX := R;
  RY := 0;

  _Row(Y1, X1, X2);

  while RadX > 0 do
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

    if (Y1 <> OldY) then
    begin
      _Row(Y1, X1, X2);
      _Row(Y2, X1, X2);

      OldY := Y1;
    end;
  end;
end;
