{%MainUnit simba.generics}

procedure TSimbaStack.CheckIndex(Index: Integer; GrowIfNeeded: Boolean = False);
var
  NeedGrow: Boolean;
begin
  NeedGrow := (Index >= FLen);
  if (NeedGrow and (not GrowIfNeeded)) or (Index < 0) then
    raise ESimbaGenericsException.Create('Out of stack range')
  else if NeedGrow then
    Grow(GrowSize);
end;

function TSimbaStack.getItem(Index: Integer): _T;
begin
  CheckIndex(Index);
  Result := FArr[Index];
end;

function TSimbaStack.getCurItem: _T;
begin
  if (FCur >= 0) then
    Result := FArr[FCur]
  else
    CheckIndex(FCur);
end;

procedure TSimbaStack.setCurItem(Item: _T);
begin
  if (FCur >= 0) then
    FArr[FCur] := Item
  else
    CheckIndex(FCur);
end;

function TSimbaStack.getCount: Integer;
begin
  if (FCur < 0) then
    Result := 0
  else
    Result := FCur + 1;
end;

constructor TSimbaStack.Create(StartBufLen: Cardinal = 32);
begin
  inherited Create();

  GrowSize := StartBufLen div 2;
  if (GrowSize < 1) then
    GrowSize := 1;

  Reset();
  Grow(StartBufLen);
end;

procedure TSimbaStack.Reset;
begin
  FArr := nil;
  FLen := 0;
  FCur := -1;
end;

function TSimbaStack.Pop: _T;
begin
  Result := getCurItem;
  Dec(FCur);
end;

function TSimbaStack.Push(Item: _T): Integer;
begin
  Inc(FCur);
  CheckIndex(FCur, True);
  FArr[FCur] := Item;
  Result := FCur;
end;

procedure TSimbaStack.ImportFromArray(Arr: TTArray);
begin
  FArr := Arr;
  FLen := Length(Arr);
  FCur := FLen - 1;
end;

function TSimbaStack.ExportToArray: TTArray;
var
  i: Integer;
begin
  SetLength(Result, FCur + 1);
  for i := 0 to FCur do
    Result[i] := FArr[i];
end;

procedure TSimbaStack.Grow(AGrowSize: Integer);
begin
  FLen := FLen + AGrowSize;
  if (FLen < 0) then
    FLen := 0;
  SetLength(FArr, FLen);
end;

