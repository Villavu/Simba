procedure PixelProc(const X, Y: Integer; const Alpha: Byte); inline;
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
    FData[Y*FWidth+X].A := 0;
end;

