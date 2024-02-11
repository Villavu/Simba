// Requires: "procedure _Row(Y: Integer; X1, X2: Integer)" and "procedure _Pixel(X, Y: Integer)"

{$IF NOT DECLARED(_Row)}
  {$ERROR _BuildBoxEdge requires "procedure _Row(Y: Integer; X1, X2: Integer)"}
{$ENDIF}
{$IF NOT DECLARED(_Pixel)}
  {$ERROR _BuildBoxEdge requires "_Pixel(X, Y: Integer)"}
{$ENDIF}

procedure _BuildBoxEdge(Box: TBox);
var
  Y: Integer;
begin
  if (Box.X1 = Box.X2) and (Box.Y1 = Box.Y2) then
    _Pixel(Box.X1, Box.Y1)
  else
  begin
    _Row(Box.Y1, Box.X1, Box.X2); // top horz
    _Row(Box.Y2, Box.X1, Box.X2); // bottom horz

    for Y := Box.Y1 + 1 to Box.Y2 - 1 do
    begin
      _Pixel(Box.X1, Y); // left vert
      _Pixel(Box.X2, Y); // right vert
    end;
  end;
end;

