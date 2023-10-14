procedure _SetPixelA(const X, Y: Integer; const Alpha: Byte); inline;
var
  Pixel: PColorBGRA;
  APlus1, APlus1Inv: UInt32;
begin
  if (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight) then
  begin
    Pixel := @FData[Y*FWidth+X];

    if (Alpha > 0) then
    begin
      if (Pixel^.A > 0) then // dont blend again
        Exit;

      APlus1    := Alpha + 1;
      APlus1Inv := 255 - Alpha + 1;

      Pixel^.R := (BGRA.R * APlus1Inv) shr 8 + Pixel^.R * APlus1 shr 8;
      Pixel^.G := (BGRA.G * APlus1Inv) shr 8 + Pixel^.G * APlus1 shr 8;
      Pixel^.B := (BGRA.B * APlus1Inv) shr 8 + Pixel^.B * APlus1 shr 8;
      Pixel^.A := Alpha;
    end else
      Pixel^ := BGRA;
  end;
end;

