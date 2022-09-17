unit simba.algo_sort;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

generic procedure QuickSort<T>(var AValues: array of T; ALeft, ARight: SizeInt);
generic procedure QuickSortWeighted<_T, _W>(var Arr: specialize TArray<_T>; var Weights: specialize TArray<_W>; iLo, iHi: SizeInt; const SortUp: Boolean);

generic procedure Sort<_T>(var Arr: specialize TArray<_T>);
generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;

implementation

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

generic procedure Sort<_T>(var Arr: specialize TArray<_T>);
begin
  specialize QuickSort<_T>(Arr, Low(Arr), High(Arr));
end;

generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
begin
  Result := Copy(Arr);

  specialize QuickSort<_T>(Result, Low(Result), High(Result));
end;

end.

