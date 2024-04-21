{$IF NOT DECLARED(_Pixel)}
  {$ERROR _BuildLine requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildLine(Start, Stop: TPoint);
var
  dx,dy,step,I: Integer;
  rx,ry,x,y: Single;
begin
  dx := (Stop.X - Start.X);
  dy := (Stop.Y - Start.Y);
  if (Abs(dx) > Abs(dy)) then
    step := Abs(dx)
  else
    step := Abs(dy);

  if (step = 0) then
  begin
    rx := dx;
    ry := dy;
  end else
  begin
    rx := dx / step;
    ry := dy / step;
  end;
  x := Start.X;
  y := Start.Y;

  _Pixel(Round(x), Round(y));
  for I := 1 to step do
  begin
    x := x + rx;
    y := y + ry;

    _Pixel(Round(x), Round(y));
  end;
end;

