{$IF NOT DECLARED(_Row)}
  {$ERROR _CircleFilled requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}

procedure _BuildCircleFilled(CenterX, CenterY, Radius: Integer);
var
  X, Y: Integer;
  Radius2: Integer;
begin
  Radius2 := Radius * Radius;
  for Y := -Radius to Radius do
  begin
    X := Round(Sqrt(Radius2 - Y * Y) + 0.5);
    _Row(Y + CenterY, CenterX - X, CenterX + X);
  end;
end;
