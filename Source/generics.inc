{$IFDEF HEADER}
generic procedure Swap<T>(var A, B: T);
generic procedure QuickSort<T>(var AValues: array of T; ALeft, ARight: SizeInt);
generic procedure QuickSortWeighted<_T, _W>(var Arr: specialize TArray<_T>; var Weights: specialize TArray<_W>; iLo, iHi: SizeInt; const SortUp: Boolean);
generic function MinA<T>(const Arr: array of T): T;
generic function MaxA<T>(const Arr: array of T): T;
generic function Sum<T, R>(var AValues: array of T): R;
generic function Unique<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
generic procedure Reverse<T>(var Arr: specialize TArray<T>);
generic function Reversed<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
generic function IndexOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): Integer;
generic function IndicesOf<_T>(const Item: _T; const Arr: specialize TArray<_T>): TIntegerArray;
generic function Mode<_T>(const Arr: specialize TArray<_T>): Integer;
generic procedure Sort<_T>(var Arr: specialize TArray<_T>);
generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
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

generic procedure QuickSort<T>(var AValues: array of T; ALeft, ARight: SizeInt);
var
  I, J: SizeInt;
  Q, P: T;
begin
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
      while AValues[I] < P do Inc(I);
      while AValues[J] > P do Dec(J);

      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;

    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        specialize QuickSort<T>(AValues, ALeft, J);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        specialize QuickSort<T>(AValues, I, ARight);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

generic procedure QuickSortWeighted<_T, _W>(var Arr: specialize TArray<_T>; var Weights: specialize TArray<_W>; iLo, iHi: SizeInt; const SortUp: Boolean);
var
  Lo, Hi: SizeInt;
  Mid, T: _W;
  TP: _T;
begin
  if (Length(Weights) = Length(Arr)) then
  repeat
    Lo := iLo;
    Hi := iHi;
    Mid := Weights[(Lo + Hi) shr 1];
    repeat
      if SortUp then
      begin
        while (Weights[Lo] < Mid) do Inc(Lo);
        while (Weights[Hi] > Mid) do Dec(Hi);
      end else
      begin
        while (Weights[Lo] > Mid) do Inc(Lo);
        while (Weights[Hi] < Mid) do Dec(Hi);
      end;
      if (Lo <= Hi) then
      begin
        if (Lo <> Hi) then
        begin
          T := Weights[Lo];
          Weights[Lo] := Weights[Hi];
          Weights[Hi] := T;
          TP := Arr[Lo];
          Arr[Lo] := Arr[Hi];
          Arr[Hi] := TP;
        end;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if Hi - iLo < iHi - Lo then
    begin
      if iLo < Hi then
        specialize QuickSortWeighted<_T, _W>(Arr, Weights, iLo, Hi, SortUp);
      iLo := Lo;
    end else
    begin
      if Lo < iHi then
        specialize QuickSortWeighted<_T, _W>(Arr, Weights, Lo, iHi, SortUp);
      iHi := Hi;
    end;
  until iLo >= iHi;
end;

generic function Sum<T, R>(var AValues: array of T): R;
var
  I: Integer;
begin
  Result := Default(R);
  for I:=0 to High(AValues) do
    Result := Result + AValues[I];
end;

generic function Unique<T>(const Arr: specialize TArray<T>): specialize TArray<T>;
var
  i,j,last:Integer;
begin
  Result := Copy(Arr);

  last := Length(Result);

  i:=0;
  while (i < last) do
  begin
    j := i+1;
    while (j < last) do
    begin
      if (Result[i] = Result[j]) then
      begin
        Result[j] := Result[last-1];
        dec(last);
        dec(j);
      end;

      Inc(j);
    end;
    Inc(i);
  end;

  SetLength(Result, last);
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

generic procedure Sort<_T>(var Arr: specialize TArray<_T>);
begin
  specialize QuickSort<_T>(Arr, Low(Arr), High(Arr));
end;

generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
begin
  Result := Copy(Arr);

  specialize QuickSort<_T>(Result, Low(Result), High(Result));
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
{$ENDIF}
