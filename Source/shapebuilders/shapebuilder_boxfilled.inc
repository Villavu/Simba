{$IF NOT DECLARED(_Row)}
  {$ERROR _BoxFilled requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildBoxFilled(Box: TBox);
var
  Y: Integer;
begin
  for Y := Box.Y1 to Box.Y2 do
    _Row(Y, Box.X1, Box.X2);
end;

