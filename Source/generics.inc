{$IFDEF HEADER}
generic procedure Swap<T>(var A, B: T);
generic procedure MoveElement<T>(var Arr: array of T; AFrom, ATo: Integer);
generic function MinA<T>(const Arr: array of T): T;
generic function MaxA<T>(const Arr: array of T): T;
generic function Sum<T, R>(var AValues: array of T): R;
generic procedure Reverse<T>(var Arr: specialize TArray<T>);
generic function Reversed<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
generic function IndexOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
generic function IndexOf_SameValue<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
generic function IndicesOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;
generic function IndicesOf_SameValue<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;
generic function Mode<_T>(const Arr: specialize TArray<_T>): Integer;
{$ENDIF}

{$IFDEF BODY}
generic procedure Swap<T>(var A, B: T);
var
  C: T;
begin
  C := A;

  A := B;
  B := C;
end;

generic procedure MoveElement<T>(var Arr: array of T; AFrom, ATo: Integer);
var
  Item: T;
begin
  if (Length(Arr) = 0) then
    SimbaException('MoveElement: Empty array');
  AFrom := EnsureRange(AFrom, Low(Arr), High(Arr));
  ATo := EnsureRange(ATo, Low(Arr), High(Arr));

  Item := Arr[AFrom];
  if (ATo > AFrom) then
    Move(Arr[AFrom + 1], Arr[AFrom], (ATo - AFrom) * SizeOf(T))
  else
    Move(Arr[ATo], Arr[ATo + 1], (AFrom - ATo) * SizeOf(T));
  Move(Item, Arr[ATo], SizeOf(T));
end;

generic function Sum<T, R>(var AValues: array of T): R;
var
  I: Integer;
begin
  Result := Default(R);
  for I:=0 to High(AValues) do
    Result := Result + AValues[I];
end;

generic procedure Reverse<T>(var Arr: specialize TArray<T>);
var
  tmp:T;
  lo,hi:^T;
begin
  if Length(Arr) > 0 then
  begin
    lo := @Arr[0];
    hi := @Arr[High(Arr)];
    while (PtrUInt(lo)<PtrUInt(hi)) do
    begin
      tmp := hi^;
      hi^ := lo^;
      lo^ := tmp;
      dec(hi);
      inc(lo);
    end;
  end;
end;

generic function Reversed<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
var
  lo:PtrUInt;
  r, p:^T;
begin
  SetLength(Result, Length(Arr));

  if Length(Arr) > 0 then
  begin
    p := @Arr[High(Arr)];
    r := @Result[0];

    lo := PtrUInt(@Arr[0]);
    while (lo<=PtrUInt(p)) do
    begin
      r^ := p^;
      dec(p);
      inc(r);
    end;
  end;
end;

generic function MinA<T>(const Arr: array of T): T;
var
  I: Integer;
begin
  if (Length(Arr) = 0) then
    Result := Default(T)
  else
    Result := Arr[0];

  for I := 1 to High(Arr) do
    if (Arr[I] < Result) then
      Result := Arr[I];
end;

generic function MaxA<T>(const Arr: array of T): T;
var
  I: Integer;
begin
  if (Length(Arr) = 0) then
    Result := Default(T)
  else
    Result := Arr[0];

  for I := 1 to High(Arr) do
    if (Arr[I] > Result) then
      Result := Arr[I];
end;

generic function IndexOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    if (Item = Arr[I]) then
      Exit(I);

  Result := -1;
end;

generic function IndicesOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;
var
  I: Integer;
  Res: specialize TSimbaOverAllocateArray<Integer>;
begin
  Res.Init(4);

  for I := 0 to High(Arr) do
    if (Item = Arr[I]) then
      Res.Add(I);

  Result := Res.Trim();
end;

generic function Mode<_T>(const Arr: specialize TArray<_T>): Integer;
var
  SortedArray: specialize TArray<_T>;
  I, Current, Hits: Integer;
  Best: _T;
begin
  Result := 0;

  if Length(Arr) > 0 then
  begin
    SortedArray := specialize Sorted<Integer>(Arr);

    Current := SortedArray[0];
    Hits := 1;
    Best := 0;

    for I := 1 to High(SortedArray) do
    begin
      if (SortedArray[I] <> Current) then
      begin
        if (Hits > Best) then
        begin
          Best := Hits;
          Result := Current;
        end;

        Current := SortedArray[I];
        Hits := 0;
      end;

      Inc(Hits);
    end;

    if (Hits > Best) then
      Result := Current;
  end;
end;

generic function IndexOf_SameValue<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    if SameValue(Item, Arr[I]) then
      Exit(I);

  Result := -1;
end;

generic function IndicesOf_SameValue<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;
var
  I: Integer;
  Buffer: specialize TSimbaOverAllocateArray<Integer>;
begin
  Buffer.Init(4);

  for I := 0 to High(Arr) do
    if SameValue(Item, Arr[I]) then
      Buffer.Add(I);

  Result := Buffer.Trim();
end;
{$ENDIF}
