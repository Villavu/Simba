{%MainUnit simba.target}

{
  Sets the image target to some pointer data. Can copy & manage the data.
}

constructor TRawTarget.Create(Data: PRGB32; Width, Height: Int32; Copy: Boolean);
begin
  inherited Create();

  FWidth := Width;
  FHeight := Height;
  FManageData := Copy;

  if Copy then
  begin
    FData := GetMem(FWidth * FHeight * SizeOf(TRGB32));

    Move(Data^, FData^, FWidth * FHeight * SizeOf(TRGB32));
  end else
    FData := Data;
end;

destructor TRawTarget.Destroy;
begin
  if FManageData then
    FreeMem(FData);

  inherited Destroy();
end;

procedure TRawTarget.GetTargetDimensions(out Width, Height: Int32);
begin
  if FImageClientAreaSet then
  begin
    Width := FImageClientArea.X2 - FImageClientArea.X1 + 1;
    Height := FImageClientArea.Y2 - FImageClientArea.Y1 + 1;
  end else
  begin
    Width := FWidth;
    Height := FHeight;
  end;
end;

function TRawTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
begin
  X := X + FImageClientArea.X1;
  Y := Y + FImageClientArea.Y1;

  if (X < 0) or (Y < 0) or (Width < 0) or (Height < 0) or
     (X + Width > FWidth) or (Y + Height > FHeight) then
    raise Exception.CreateFmt('TRawTarget.ReturnData: The parameters passed (%d, %d, %d, %d) execeed the targets bounds (width: %d, height: %d)', [X, Y, Width, Height, FWidth, FHeight]);

  Result.Ptr := @FData[Y * FHeight + X];
  Result.RowLen := FWidth;
  Result.IncPtrWith := FWidth - Width;
end;

function TRawTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Loop: Int32;
begin
  Result := GetMem(Width * Height * SizeOf(TRGB32));

  X := X + FImageClientArea.X1;
  Y := Y + FImageClientArea.Y1;

  if (X < 0) or (Y < 0) or (Width < 0) or (Height < 0) or
     (X + Width > FWidth) or (Y + Height > FHeight) then
    raise Exception.CreateFmt('TRawTarget.CopyData: The parameters passed (%d, %d, %d, %d) execeed the targets bounds (width: %d, height: %d)', [X, Y, Width, Height, FWidth, FHeight]);

  for Loop := 0 to Height - 1 do
    Move(FData[(Y + Loop) * FWidth + X], Result[Loop * Width], Width * SizeOf(TRGB32));
end;
