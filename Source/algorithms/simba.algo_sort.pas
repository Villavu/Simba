{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.algo_sort;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  generic TCompareFunc<_T> = function(const A, B: _T): Integer;

generic procedure QuickSort<_T>(var AValues: specialize TArray<_T>; ALeft, ARight: SizeInt; CompareFunc: specialize TCompareFunc<_T>); overload;
generic procedure QuickSort<_T>(var AValues: specialize TArray<_T>; ALeft, ARight: SizeInt); overload;
generic procedure QuickSort<_T, _W>(var Arr: specialize TArray<_T>; var Weights: specialize TArray<_W>; iLo, iHi: SizeInt; SortUp: Boolean); overload;

generic procedure Sort<_T>(var Arr: specialize TArray<_T>); overload;
generic procedure Sort<_T, _W>(var Arr: specialize TArray<_T>; Weights: specialize TArray<_W>; SortUp: Boolean); overload;

generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>; overload;
generic function Sorted<_T, _W>(const Arr: specialize TArray<_T>; Weights: specialize TArray<_W>; SortUp: Boolean): specialize TArray<_T>; overload;

implementation

generic procedure QuickSort<_T>(var AValues: specialize TArray<_T>; ALeft, ARight: SizeInt; CompareFunc: specialize TCompareFunc<_T>);
var
  I, J: SizeInt;
  Q, P: _T;
begin
  if (Length(AValues) <= 1) or (ARight-ALeft < 1) then
    Exit;

  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
      while CompareFunc(AValues[I], P) < 0 do Inc(I);
      while CompareFunc(AValues[J], P) > 0 do Dec(J);

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
        specialize QuickSort<_T>(AValues, ALeft, J, CompareFunc);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        specialize QuickSort<_T>(AValues, I, ARight, CompareFunc);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

generic procedure QuickSort<_T>(var AValues: specialize TArray<_T>; ALeft, ARight: SizeInt);
var
  I, J: SizeInt;
  Q, P: _T;
begin
  if (Length(AValues) <= 1) then
    Exit;

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
        specialize QuickSort<_T>(AValues, ALeft, J);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        specialize QuickSort<_T>(AValues, I, ARight);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

generic procedure QuickSort<_T, _W>(var Arr: specialize TArray<_T>; var Weights: specialize TArray<_W>; iLo, iHi: SizeInt; SortUp: Boolean);
var
  Lo, Hi: SizeInt;
  Mid, T: _W;
  TP: _T;
begin
  if (Length(Arr) <= 1) or (Length(Weights) <> Length(Arr)) or (iHi-iLo < 1) then
    Exit;

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
        specialize QuickSort<_T, _W>(Arr, Weights, iLo, Hi, SortUp);
      iLo := Lo;
    end else
    begin
      if Lo < iHi then
        specialize QuickSort<_T, _W>(Arr, Weights, Lo, iHi, SortUp);
      iHi := Hi;
    end;
  until iLo >= iHi;
end;

generic procedure Sort<_T>(var Arr: specialize TArray<_T>);
begin
  specialize QuickSort<_T>(Arr, Low(Arr), High(Arr));
end;

generic procedure Sort<_T, _W>(var Arr: specialize TArray<_T>; Weights: specialize TArray<_W>; SortUp: Boolean);
begin
  Weights := Copy(Weights);

  specialize QuickSort<_T, _W>(Arr, Weights, Low(Arr), High(Arr), SortUp);
end;

generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
begin
  Result := Copy(Arr);

  specialize QuickSort<_T>(Result, Low(Result), High(Result));
end;

generic function Sorted<_T, _W>(const Arr: specialize TArray<_T>; Weights: specialize TArray<_W>; SortUp: Boolean): specialize TArray<_T>;
begin
  Weights := Copy(Weights);
  Result := Copy(Arr);

  specialize QuickSort<_T, _W>(Result, Weights, Low(Result), High(Result), SortUp);
end;

end.

