{$IF NOT DECLARED(_Pixel)}
  {$ERROR _BuildLineGap requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildLineGap(Start, Stop: TPoint; GapSize: Integer);
var
  dx,dy,step,I,Gap: Integer;
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
  Gap := 0;

  _Pixel(Round(x), Round(y));
  for I := 1 to step do
  begin
    x := x + rx;
    y := y + ry;

    Inc(Gap);
    if (Gap > 0) then
    begin
      _Pixel(Round(x), Round(y));
      if (Gap > GapSize) then
        Gap := -GapSize;
    end;
  end;
  _Pixel(Round(x), Round(y));
end;


