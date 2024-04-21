{$IF NOT DECLARED(_Row)}
  {$ERROR _BuildCircleFilled requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildCircleFilled(CenterX, CenterY, Radius: Integer);
var
  X, Y, Err, PrevY: Integer;
begin
  X := -Radius;
  Y := 1;
  PrevY := 0;
  Err := 2-2*Radius;

  _Row(CenterY, CenterX + X, CenterX - X);

  while (X < 0) do
  begin
    if (Y <> PrevY) and (Y > 0) then
    begin
      _Row(CenterY + Y, CenterX + X, CenterX - X);
      _Row(CenterY - Y, CenterX + X, CenterX - X);
    end;

    PrevY := Y;
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
