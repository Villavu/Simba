{$IF NOT DEFINED(_Row)}
  {$ERROR _BuildBox requires "_Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildBox(Box: TBox; const Inverted: Boolean; const Clip: TBox);
var
  Y: Integer;
begin
  Box.X1 := Max(Box.X1, Clip.X1);
  Box.Y1 := Max(Box.Y1, Clip.Y1);
  Box.X2 := Min(Box.X2, Clip.X2);
  Box.Y2 := Min(Box.Y2, Clip.Y2);
  if (Box.X1 > Box.X2) or (Box.Y1 > Box.Y2) then
  begin
    if Inverted then
      for Y := Clip.Y1 to Clip.Y2 do
        _Row(Y, Clip.X1, Clip.X2);
    Exit;
  end;

  if (not Inverted) then
    for Y := Box.Y1 to Box.Y2 do
      _Row(Y, Box.X1, Box.X2)
  else
  begin
    for Y := Clip.Y1 to Box.Y1 - 1 do            // above the box
      _Row(Y, Clip.X1, Clip.X2);
    for Y := Box.Y1 to Box.Y2 do                 // level with it: the strips either side
    begin
      _Row(Y, Clip.X1, Box.X1 - 1);
      _Row(Y, Box.X2 + 1, Clip.X2);
    end;
    for Y := Box.Y2 + 1 to Clip.Y2 do            // below it
      _Row(Y, Clip.X1, Clip.X2);
  end;
end;

{$undef _Row}
