function TMATRIXHELPER.Width: Integer;
begin
  Result := 0;
  if Length(Self) > 0 then
    Result := Length(Self[0]);
end;

function TMATRIXHELPER.Height: Integer;
begin
  Result := Length(Self);
end;

procedure TMATRIXHELPER.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

function TMATRIXHELPER.GetValues(Indices: TPointArray): TMATRIXARRAYTYPE;
var
  I, W, H, Count: Integer;
begin
  Result := nil;

  W := Width;
  H := Height;
  if (W = 0) or (H = 0) then
    Exit;

  Count := 0;
  SetLength(Result, Length(Indices));
  for I := 0 to High(Indices) do
    if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
       (Indices[I].X < W)  and (Indices[I].Y < H) then
    begin
      Result[Count] := Self[Indices[I].Y][Indices[I].X];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

procedure TMATRIXHELPER.SetValues(Indices: TPointArray; Values: TMATRIXARRAYTYPE);
var
  W, H, I: Integer;
begin
  W := Width;
  H := Height;
  if (W = 0) or (H = 0) then
    Exit;

  if (Length(Values) <> Length(Indices)) then
    raise Exception.Create('SetValues: Values and Indices must be same length');

  for I := 0 to High(Indices) do
    if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
       (Indices[I].X < W)  and (Indices[I].Y < H) then
        Self[Indices[I].Y][Indices[I].X] := Values[I];
end;

procedure TMATRIXHELPER.SetValues(Indices: TPointArray; Value: TMATRIXVARTYPE);
var
  W, H, I: Integer;
begin
  W := Width;
  H := Height;
  if (W = 0) or (H = 0) then
    Exit;

  for I := 0 to High(Indices) do
    if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
       (Indices[I].X < W)  and (Indices[I].Y < H) then
        Self[Indices[I].Y][Indices[I].X] := Value;
end;

procedure TMATRIXHELPER.Fill(Area: TBox; Value: TMATRIXVARTYPE);
var
  X, Y: Integer;
begin
  Area.X1 := Max(0, Area.X1);
  Area.Y1 := Max(0, Area.Y1);
  Area.X2 := Min(Width - 1,  Area.X2);
  Area.Y2 := Min(Height - 1, Area.Y2);
  if (Area.X2 - Area.X1 <= 0) or (Area.Y2 - Area.Y1 <= 0) then
    Exit;

  for X := Area.X1 to Area.X2 do
    Self[Area.Y1][X] := Value;
  for Y := Area.Y1 + 1 to Area.Y2 do
    Move(Self[Area.Y1][Area.X1], Self[Y][Area.X1], (Area.X2 - Area.X1 + 1) * SizeOf(TMATRIXVARTYPE));
end;

procedure TMATRIXHELPER.Fill(Value: TMATRIXVARTYPE);
begin
  Self.Fill(Box(0, 0, Width-1, Height-1), Value);
end;

function TMATRIXHELPER.Flatten: TMATRIXARRAYTYPE;
var
  W, H, Y: Integer;
begin
  Result := nil;

  W := Self.Width;
  H := Self.Height;
  if (W = 0) or (H = 0) then
    Exit;

  SetLength(Result, Width * Height);

  for Y := 0 to H - 1 do
    Move(Self[Y][0], Result[Y*W], W * SizeOf(TMATRIXVARTYPE));
end;
