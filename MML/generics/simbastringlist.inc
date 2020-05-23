{%MainUnit simba.generics}

function TSimbaStringList.getSorted: Boolean;
begin
  Result := FHashList.Sorted;
end;

procedure TSimbaStringList.setSorted(Sort: Boolean; DoUpdate: Boolean = True);
begin
  FHashList.setSorted(Sort, DoUpdate);
  inherited;
  FSorted := False;
end;

procedure TSimbaStringList.setItem(Index: Integer; Item: String);
begin
  if (Index > -1) and (Index < FCount) then
  begin
    Delete(Index);
    Insert(Item, Index);
  end
  else
    inherited;
end;

constructor TSimbaStringList.Create(InvalidValue: String; ADuplicates: TDuplicates; ACaseSensitive, ASort: Boolean);
begin
  FHashList := TSimbaList_UInt32.Create(0, dupAccept, ASort);
  inherited Create(InvalidValue, ADuplicates, False);

  FCaseSensitive := ACaseSensitive;
  Sorted := ASort;
end;

destructor TSimbaStringList.Destroy;
begin
  inherited;
  FHashList.Free();
end;

procedure TSimbaStringList.Clear;
begin
  inherited;
  FHashList.Clear();
end;

function TSimbaStringList.CaseSens(const Item: String): String;
begin
  if FCaseSensitive then
    Result := Item
  else
    Result := UpperCase(Item);
end;

function TSimbaStringList.Add(Item: String): Integer;
var
  dup: TDuplicates;
begin
  if (FDuplicates in [dupIgnore, dupError]) and ExistsItem(Item) then
    if (FDuplicates = dupError) then
      raise ESimbaGenericsException.Create('Duplicate declaration')
    else
      Exit(-1);

  Assert(FHashList.Count = Count);

  Item := CaseSens(Item);
  Result := FHashList.Add(HashString(Item));

  if (Result > -1) then
  try
    dup := FDuplicates;
    FDuplicates := dupAccept;
    inherited MoveItem(inherited Add(Item), Result);
  finally
    FDuplicates := dup;
  end;
end;

function TSimbaStringList.Delete(Index: Integer): String;
begin
  Result := inherited;
  FHashList.Delete(FCount);
  Assert(FHashList.Count = Count);
end;

procedure TSimbaStringList.MoveItem(AFrom, ATo: Integer);
begin
  FHashList.MoveItem(AFrom, ATo);
  inherited;
end;

procedure TSimbaStringList.SwapItem(AFrom, ATo: Integer);
begin
  FHashList.SwapItem(AFrom, ATo);
  inherited;
end;

function TSimbaStringList.IndexOf(Item: String; Lo, Hi: Integer): Integer;
begin
  if (Lo < 0) or (Lo > Hi) or (Hi >= FCount) then
    Exit(-1);

  Item := CaseSens(Item);
  Result := FHashList.IndexOf(HashString(Item), Lo, Hi);

  if (Result > -1) and (FItems[Result] <> Item) then
    Result := IndexOf(Item, Result + 1, Hi);
end;

procedure TSimbaStringList.ImportFromArray(Arr: TSimbaList_String.TTArray);
var
  i: Integer;
begin
  Clear();
  for i := 0 to High(Arr) do
    Add(Arr[i]);
end;

function TSimbaStringList.Implode(ASep: String): String;
var
  i, Len: Integer;
begin
  Result := '';
  if (FCount < 1) then
    Exit;

  Len := Length(ASep) * (FCount - 1);
  for i := 0 to FCount - 1 do
    Len := Len + Length(FItems[i]);

  SetLength(Result, Len);
  Len := 1;

  for i := 0 to FCount - 1 do
  begin
    if (i > 0) and (ASep <> '') then
    begin
      Move(ASep[1], Result[Len], Length(ASep) * SizeOf(ASep[1]));
      Len := Len + Length(ASep);
    end;
    if (FItems[i] <> '') then
    begin
      Move(FItems[i][1], Result[Len], Length(FItems[i]) * SizeOf(FItems[i][1]));
      Len := Len + Length(FItems[i]);
    end;
  end;
end;

function TSimbaStringMap.getItem(Key: String): _T;
begin
  Result := FItems[IndexOfKey(Key)];
end;

procedure TSimbaStringMap.setItem(Key: String; Item: _T);
var
  Index: Integer;
begin
  Index := IndexOfKey(Key);
  if (Index > -1) and (Index < FCount) then
    FItems[Index] := Item
  else
    Add(Key, Item);
end;

function TSimbaStringMap.getItemI(Index: Integer): _T;
begin
  Result := FItems[Index];
end;

procedure TSimbaStringMap.setItemI(Index: Integer; Item: _T);
begin
  FItems[Index] := Item;
end;
