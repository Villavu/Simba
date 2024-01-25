{$IF NOT DECLARED(_Row)}
  {$ERROR _BoxEdge requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildBoxEdge(Box: TBox);
var
  Y: Integer;
begin
  if (Box.X1 = Box.X2) and (Box.Y1 = Box.Y2) then
    _Row(Box.Y1, Box.X1, Box.X1)
  else
  begin
    _Row(Box.Y1, Box.X1, Box.X2); // top horz
    _Row(Box.Y2, Box.X1, Box.X2); // bottom horz

    for Y := Box.Y1 + 1 to Box.Y2 - 1 do
    begin
      _Row(Y, Box.X1, Box.X1); // left vert
      _Row(Y, Box.X2, Box.X2); // right vert
    end;
  end;
end;

