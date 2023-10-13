procedure _SetPixel(const X, Y: Integer); inline;
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    FData[Y*FWidth+X] := BGRA;
end;

