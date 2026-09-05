{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildBoxEdge requires "_Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}
{$IF NOT DEFINED(_Pixel)}
  {$ERROR _BuildBoxEdge requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildBoxEdge(Box: TBox; Thickness: Integer; const Clip: TBox);

  function HeldX(V: Int64): Integer;
  begin
    Result := Max(Min(V, Int64(Clip.X2) + 1), Int64(Clip.X1) - 1);
  end;

  function HeldY(V: Int64): Integer;
  begin
    Result := Max(Min(V, Int64(Clip.Y2) + 1), Int64(Clip.Y1) - 1);
  end;

var
  HalfThickness, Y, OuterX1, OuterY1, OuterX2, OuterY2, TopEnd, BotStart, LeftEnd, RightStart: Integer;
begin
  if (Thickness <= 1) then
  begin
    if (Box.Y1 >= Clip.Y1) and (Box.Y1 <= Clip.Y2) then
      _Row(Box.Y1, Box.X1, Box.X2);                                       // top
    if (Box.Y2 > Box.Y1) and (Box.Y2 >= Clip.Y1) and (Box.Y2 <= Clip.Y2) then
      _Row(Box.Y2, Box.X1, Box.X2);                                       // bottom
    for Y := HeldY(Int64(Box.Y1) + 1) to HeldY(Int64(Box.Y2) - 1) do
    begin
      _Pixel(Box.X1, Y);                                                  // left
      if (Box.X2 > Box.X1) then
        _Pixel(Box.X2, Y);                                                // right
    end;
    Exit;
  end;

  HalfThickness := Thickness div 2;
  OuterX1 := HeldX(Int64(Box.X1) - HalfThickness);  OuterY1 := HeldY(Int64(Box.Y1) - HalfThickness);
  OuterX2 := HeldX(Int64(Box.X2) + HalfThickness);  OuterY2 := HeldY(Int64(Box.Y2) + HalfThickness);
  if (OuterX2 < Clip.X1) or (OuterX1 > Clip.X2) then
    Exit;
  TopEnd     := HeldY(Int64(Box.Y1) + HalfThickness);
  BotStart   := HeldY(Int64(Box.Y2) - HalfThickness);
  LeftEnd    := HeldX(Int64(Box.X1) + HalfThickness);
  RightStart := HeldX(Int64(Box.X2) - HalfThickness);

  for Y := Max(Clip.Y1, OuterY1) to Min(Clip.Y2, OuterY2) do
    if (Y <= TopEnd) or (Y >= BotStart) or (LeftEnd >= RightStart) then
      _Row(Y, OuterX1, OuterX2)
    else
    begin
      _Row(Y, OuterX1, LeftEnd);
      _Row(Y, RightStart, OuterX2);
    end;
end;

{$undef _Row}
{$undef _Pixel}
