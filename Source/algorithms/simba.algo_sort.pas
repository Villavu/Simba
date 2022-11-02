{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.algo_sort;

{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

generic procedure QuickSort<_T>(var AValues: array of _T; ALeft, ARight: SizeInt);
generic procedure QuickSortWeighted<_T, _W>(var Arr: array of _T; var Weights: array of _W; iLo, iHi: SizeInt; SortUp: Boolean);

generic procedure Sort<_T>(var Arr: array of _T);
generic procedure Sort<_T>(var Arr: array of _T; Weights: TIntegerArray; SortUp: Boolean); overload;
generic procedure Sort<_T>(var Arr: array of _T; Weights: TDoubleArray; SortUp: Boolean); overload;

generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>; overload;
generic function Sorted<_T>(const Arr: specialize TArray<_T>; Weights: TIntegerArray; SortUp: Boolean): specialize TArray<_T>; overload;
generic function Sorted<_T>(const Arr: specialize TArray<_T>; Weights: TDoubleArray; SortUp: Boolean): specialize TArray<_T>; overload;

implementation

generic procedure QuickSort<_T>(var AValues: array of _T; ALeft, ARight: SizeInt);
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

generic procedure QuickSortWeighted<_T, _W>(var Arr: array of _T; var Weights: array of _W; iLo, iHi: SizeInt; SortUp: Boolean);
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

generic procedure Sort<_T>(var Arr: array of _T);
begin
  specialize QuickSort<_T>(Arr, Low(Arr), High(Arr));
end;

generic procedure Sort<_T>(var Arr: array of _T; Weights: TIntegerArray; SortUp: Boolean);
begin
  Weights := Copy(Weights);

  specialize QuickSortWeighted<_T, Integer>(Arr, Weights, Low(Arr), High(Arr), SortUp);
end;

generic procedure Sort<_T>(var Arr: array of _T; Weights: TDoubleArray; SortUp: Boolean);
begin
  Weights := Copy(Weights);

  specialize QuickSortWeighted<_T, Double>(Arr, Weights, Low(Arr), High(Arr), SortUp);
end;

generic function Sorted<_T>(const Arr: specialize TArray<_T>): specialize TArray<_T>;
begin
  Result := Copy(Arr);

  specialize QuickSort<_T>(Result, Low(Result), High(Result));
end;

generic function Sorted<_T>(const Arr: specialize TArray<_T>; Weights: TIntegerArray; SortUp: Boolean): specialize TArray<_T>;
begin
  Weights := Copy(Weights);
  Result := Copy(Arr);

  specialize QuickSortWeighted<_T, Integer>(Result, Weights, Low(Result), High(Result), SortUp);
end;

generic function Sorted<_T>(const Arr: specialize TArray<_T>; Weights: TDoubleArray; SortUp: Boolean): specialize TArray<_T>;
begin
  Weights := Copy(Weights);
  Result := Copy(Arr);

  specialize QuickSortWeighted<_T, Double>(Result, Weights, Low(Result), High(Result), SortUp);
end;

end.

