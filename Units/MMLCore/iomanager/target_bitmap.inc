{%MainUnit simba.target}

constructor TBitmapTarget.Create(Bitmap: TMufasaBitmap);
begin
  inherited Create();

  FBitmap := Bitmap;
end;

destructor TBitmapTarget.Destroy;
begin
  inherited Destroy();
end;

procedure TBitmapTarget.GetTargetDimensions(out Width, Height: Int32);
begin
  if FImageClientAreaSet then
  begin
    Width := FImageClientArea.X2 - FImageClientArea.X1;
    Height := FImageClientArea.Y2 - FImageClientArea.Y1;
  end else
  begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
  end;
end;

function TBitmapTarget.ReturnData(X, Y, Width, Height: Integer): TRetData;
begin
  Result.Ptr := nil;
  Result.RowLen := 0;
  Result.IncPtrWith := 0;

  X := X + FImageClientArea.X1;
  Y := Y + FImageClientArea.Y1;

  if (X < 0) or (Y < 0) or (Width < 0) or (Height < 0) or
     (X + Width > FBitmap.Width) or (Y + Height > FBitmap.Height) then
    raise Exception.CreateFmt('TBitmapTarget.ReturnData: The parameters passed (%d, %d, %d, %d) execeed the targets bounds (width: %d, height: %d)', [X, Y, Width, Height, FBitmap.Width, FBitmap.Height]);

  Result.Ptr := @FBitmap.FData[Y * FBitmap.Width + X];
  Result.RowLen := FBitmap.Width;
  Result.IncPtrWith := FBitmap.Width - Width;
end;

function TBitmapTarget.CopyData(X, Y, Width, Height: Integer): PRGB32;
var
  Loop: Int32;
begin
  Result := nil;

  X := X + FImageClientArea.X1;
  Y := Y + FImageClientArea.Y1;

  if (X < 0) or (Y < 0) or (Width < 0) or (Height < 0) or
     (X + Width > FBitmap.Width) or (Y + Height > FBitmap.Height) then
    raise Exception.CreateFmt('TBitmapTarget.CopyData: The parameters passed (%d, %d, %d, %d) execeed the targets bounds (width: %d, height: %d)', [X, Y, Width, Height, FBitmap.Width, FBitmap.Height]);

  Result := GetMem(Width * Height * SizeOf(TRGB32));
  for Loop := 0 to Height - 1 do
    Move(FBitmap.FData[(Y + Loop) * FBitmap.Width + X], Result[Loop * Width], Width * SizeOf(TRGB32));
end;
