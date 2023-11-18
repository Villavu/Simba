// Requires method `procedure _Row(const Y: Integer; X1, X2: Integer);`

procedure _BoxFilled(Box: TBox);
var
  Y: Integer;
begin
  for Y := Box.Y1 to Box.Y2 do
    _Row(Y, Box.X1, Box.X2);
end;

