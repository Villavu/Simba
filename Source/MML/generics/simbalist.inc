{%MainUnit simba.generics}

function TSimbaList.getSorted: Boolean;
begin
  Result := FSorted;
end;

procedure TSimbaList.setSorted(Sort: Boolean; DoUpdate: Boolean);
var
  a: TTArray;
  i: Integer;
begin
  Sort := Sort and (Byte(SizeOf(_T)) in [SizeOf(UInt8), SizeOf(UInt16), SizeOf(UInt32), SizeOf(UInt64)]);
  if (FSorted <> Sort) then
  begin
    FSorted := Sort;

    if DoUpdate and Sort and (FCount > 0) then
    begin
      a := ExportToArray();
      Clear();
      for i := 0 to High(a) do
        Add(a[i]);
    end;
  end;
end;

procedure TSimbaList.setSorted(Sort: Boolean);
begin
  setSorted(Sort, True);
end;

function TSimbaList.getItem(Index: Integer): _T;
begin
  if (Index > -1) and (Index < FCount) then
    Result := FItems[Index]
  else
    Result := InvalidVal;
end;

procedure TSimbaList.setItem(Index: Integer; Item: _T);
begin
  if (Index > -1) and (Index < FCount) then
  begin
    if (not FSorted) then
      FItems[Index] := Item
    else
    begin
      Delete(Index);
      Add(Item);
    end;
  end
  else
    raise ESimbaGenericsException.Create('Invalid index: ' + IntToStr(Index));
end;

constructor TSimbaList.Create(InvalidValue: _T; ADuplicates: TDuplicates; ASort: Boolean);
begin
  inherited Create();

  InvalidVal := InvalidValue;
  FDuplicates := ADuplicates;
  FSorted := False;

  Clear();
  Sorted := ASort;
end;

constructor TSimbaList.Create;
begin
  Create(Default(_T), dupAccept, False);
end;

procedure TSimbaList.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

procedure TSimbaList.Extend(Items: TTArray);
var
  i: Integer;
begin
  for i := 0 to High(Items) do
    Add(Items[i]);
end;

function TSimbaList.Add(Item: _T): Integer;
var
  Len: Integer;
begin
  if (FDuplicates in [dupIgnore, dupError]) and ExistsItem(Item) then
    if (FDuplicates = dupError) then
      raise ESimbaGenericsException.Create('Duplicate Declaration')
    else
      Exit(-1);

  Result := FCount;
  Inc(FCount);

  Len := Length(FItems);
  if (FCount > Len) then
  begin
    Len := Len + 2 + (Len div 2) + ((Len div 2) mod 2);
    SetLength(FItems, Len);
  end;

  FItems[Result] := Item;
  if FSorted then
    case SizeOf(_T) of
      SizeOf(UInt8):  _Insert8 (@FItems[0], Result);
      SizeOf(UInt16): _Insert16(@FItems[0], Result);
      SizeOf(UInt32): _Insert32(@FItems[0], Result);
      SizeOf(UInt64): _Insert64(@FItems[0], Result);
    end;
end;

function TSimbaList.Insert(Item: _T; Index: Integer): Integer;
begin
  Result := Add(Item);

  if (Result <> Index) and (Result > -1) and (not Sorted) then
  begin
    MoveItem(Result, Index);
    Result := Index;
  end;
end;

function TSimbaList.Delete(Index: Integer): _T;
var
  Sort: Boolean;
begin
  Result := InvalidVal;
  if (Index > -1) and (Index < FCount) then
  begin
    Result := FItems[Index];

    Sort := Sorted;
    try
      setSorted(False, False);
      MoveItem(Index, FCount - 1);
    finally
      setSorted(Sort, False);
      Dec(FCount);
    end;
  end
  else
    raise ESimbaGenericsException.Create('Invalid index: ' + IntToStr(Index));
end;

function TSimbaList.DeleteItem(Item: _T): _T;
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := InvalidVal;
end;

procedure TSimbaList.MoveItem(AFrom, ATo: Integer);
var
  Item: _T;
begin
  if (AFrom = ATo) or
     (ATo   < 0) or (ATo   >= FCount)  or
     (AFrom < 0) or (AFrom >= FCount)
  then
    Exit;

  if FSorted then
    raise ESimbaGenericsException.Create('Cannot move items on a sorted list');

  Item := FItems[AFrom];
  if (ATo > AFrom) then
    Move(FItems[AFrom + 1], FItems[AFrom], (ATo - AFrom) * SizeOf(_T))
  else
    Move(FItems[ATo], FItems[ATo + 1], (AFrom - ATo) * SizeOf(_T));
  Move(Item, FItems[ATo], SizeOf(_T));
end;

procedure TSimbaList.SwapItem(AFrom, ATo: Integer);
var
  c: _T;
begin
  if (AFrom = ATo) or
     (ATo   < 0) or (ATo   >= FCount)  or
     (AFrom < 0) or (AFrom >= FCount)
  then
    Exit;

  if FSorted then
    raise ESimbaGenericsException.Create('Cannot swap items on a sorted list');

  c := FItems[AFrom];
  FItems[AFrom] := FItems[ATo];
  FItems[ATo] := c;
end;

function TSimbaList.IndicesOf(Item: _T): TIntegerArray;
var
  Index, Lo, Hi: Integer;
begin
  Result := nil;
  Lo := 0;
  Hi := FCount - 1;

  Index := IndexOf(Item, Lo, Hi);
  while (Index > -1) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Index;

    Lo := Index + 1;
    Index := IndexOf(Item, Lo, Hi);
  end;
end;

function TSimbaList.IndexOf(Item: _T; Lo, Hi: Integer): Integer;
var
  i, ii: Integer;
  ItemA, ItemB: PByteArray;
  Match: Boolean;
begin
  if (Lo < 0) or (Lo > Hi) or (Hi >= FCount) then
    Exit(-1);

  if FSorted then
    case SizeOf(_T) of
      SizeOf(UInt8) : Result := _BSearch8 (@FItems[0], PUInt8 (@Item)^, Lo, Hi);
      SizeOf(UInt16): Result := _BSearch16(@FItems[0], PUInt16(@Item)^, Lo, Hi);
      SizeOf(UInt32): Result := _BSearch32(@FItems[0], PUInt32(@Item)^, Lo, Hi);
      SizeOf(UInt64): Result := _BSearch64(@FItems[0], PUInt64(@Item)^, Lo, Hi);
    end
  else
    case SizeOf(_T) of
      SizeOf(UInt8) : Result := _Compare8 (@FItems[Lo], PUInt8 (@Item)^, Lo, Hi);
      SizeOf(UInt16): Result := _Compare16(@FItems[Lo], PUInt16(@Item)^, Lo, Hi);
      SizeOf(UInt32): Result := _Compare32(@FItems[Lo], PUInt32(@Item)^, Lo, Hi);
      SizeOf(UInt64): Result := _Compare64(@FItems[Lo], PUInt64(@Item)^, Lo, Hi);
      else
      begin
        ItemB := PByteArray(@Item);
        for i := Lo to Hi do
        begin
          ItemA := PByteArray(@FItems[i]);
          Match := True;
          for ii := 0 to SizeOf(_T) - 1 do
            if (ItemA^[ii] <> ItemB^[ii]) then
            begin
              Match := False;
              Break;
            end;
          if Match then
            Exit(i);
        end;
        Result := -1;
      end;
    end;
end;

function TSimbaList.IndexOf(Item: _T): Integer;
begin
  Result := IndexOf(Item, 0, FCount - 1);
end;

function TSimbaList.ExistsItem(Item: _T): Boolean;
begin
  Result := (IndexOf(Item) > -1);
end;

procedure TSimbaList.ImportFromArray(Arr: TTArray);
begin
  FItems := Arr;
  FCount := Length(Arr);

  if Sorted then
  begin
    Sorted := False;
    Sorted := True;
  end;
end;

function TSimbaList.ExportToArray: TTArray;
var
  i: Integer;
begin
  SetLength(Result, FCount);
  for i := 0 to FCount - 1 do
    Result[i] := FItems[i];
end;
