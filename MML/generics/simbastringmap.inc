{%MainUnit simba.generics}

function TSimbaStringMap.getKey(Index: Integer): String;
begin
  Result := FStringList[Index];
end;

function TSimbaStringMap.getSorted: Boolean;
begin
  Result := FStringList.Sorted;
end;

procedure TSimbaStringMap.setSorted(Sort: Boolean);
begin
  FStringList.Sorted := Sort;
end;

constructor TSimbaStringMap.Create(InvalidValue: _T; Duplicates: TDuplicates; Sort: Boolean; InvalidKey: String = ''; ACaseSensitive: Boolean = False);
begin
  inherited Create();

  FStringList := TSimbaStringList.Create(InvalidKey, Duplicates, ACaseSensitive, Sort);
  FItems := TTItems.Create(InvalidValue, dupAccept, False);

  Clear();
end;

destructor TSimbaStringMap.Destroy;
begin
  FStringList.Free();
  FItems.Free;

  inherited;
end;

procedure TSimbaStringMap.Clear;
begin
  FStringList.Clear();
  FItems.Clear;
  FCount := 0;
end;

function TSimbaStringMap.Insert(Key: String; Item: _T; Index: Integer): Integer;
begin
  Result := FStringList.Insert(Key, Index);
  if (Result > -1) then
    if (FItems.Insert(Item, Result) = Result) then
      Inc(FCount)
    else
      FStringList.Delete(Result);
end;

function TSimbaStringMap.Add(Key: String; Item: _T): Integer;
begin
  Result := Insert(Key, Item, FCount);
end;

function TSimbaStringMap.Delete(Key: String): _T;
var
  Index: Integer;
begin
  Index := IndexOfKey(Key);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := FItems.InvalidVal;
end;

function TSimbaStringMap.Delete(Index: Integer): _T;
begin
  Result := FItems.Delete(Index);
  FStringList.Delete(Index);
  Dec(FCount);
end;

function TSimbaStringMap.DeleteItem(Item: _T): _T;
var
  Index: Integer;
begin
  Index := IndexOfItemI(Item);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := FItems.InvalidVal;
end;

function TSimbaStringMap.IndicesOfItemI(Item: _T): TIntegerArray;
begin
  Result := FItems.IndicesOf(Item);
end;

function TSimbaStringMap.IndicesOfItem(Item: _T): TStringArray;
var
  Indices: TIntegerArray;
  i: Integer;
begin
  Indices := IndicesOfItemI(Item);

  SetLength(Result, Length(Indices));
  for i := 0 to High(Indices) do
    Result[i] := FStringList[Indices[i]];
end;

function TSimbaStringMap.IndicesOfKey(Key: String): TIntegerArray;
begin
  Result := FStringList.IndicesOf(Key);
end;

function TSimbaStringMap.ItemsOfKey(Key: String): TTArray;
var
  i: Int32;
  Indices: TIntegerArray;
begin
  Indices := IndicesOfKey(Key);

  SetLength(Result, Length(Indices));
  for i := 0 to High(Indices) do
    Result[i] := FItems[Indices[i]];
end;

function TSimbaStringMap.IndexOfItemI(Item: _T): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function TSimbaStringMap.IndexOfItem(Item: _T): String;
begin
  Result := FStringList[IndexOfItemI(Item)];
end;

function TSimbaStringMap.IndexOfKey(Key: String): Integer;
begin
  Result := FStringList.IndexOf(Key);
end;

function TSimbaStringMap.ExistsItem(Item: _T): Boolean;
begin
  Result := FItems.ExistsItem(Item);
end;

function TSimbaStringMap.ExistsKey(Key: String): Boolean;
begin
  Result := FStringList.ExistsItem(Key);
end;

procedure TSimbaStringMap.setKeyI(Index: Integer; NewKey: String);
begin
  if (Index > -1) and (Index < FCount) then
    Insert(NewKey, Delete(Index), Index);
end;

procedure TSimbaStringMap.setKey(Item: _T; NewKey: String);
begin
  setKeyI(IndexOfItemI(Item), NewKey);
end;

procedure TSimbaStringMap.ImportFromArrays(Arr: TTArrays);
begin
  FStringList.ImportFromArray(Arr.Keys);
  FItems.ImportFromArray(Arr.Items);

  FCount := FItems.Count;
  Assert(FStringList.Count = FCount);
end;

function TSimbaStringMap.ExportToArrays: TTArrays;
begin
  Result.Keys := FStringList.ExportToArray();
  Result.Items := FItems.ExportToArray;
end;
