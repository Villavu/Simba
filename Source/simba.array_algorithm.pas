{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Generic array methods.
}
unit simba.array_algorithm;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TArrayRelationship<_T> = class
  public type
    TArr = array of _T;
  public
    class function Difference(x,y: TArr): TArr; static;
    class function SymmetricDifference(x,y: TArr): TArr; static;
    class function Intersection(x,y: TArr): TArr; static;
  end;

  generic TArrayUnique<_T> = class
  public type
    TArr = array of _T;
    TCompareFunc = function(const L, R: _T): Boolean is nested;
  public
    class function Unique(Arr: TArr): TArr; static; overload;
    class function Unique(Arr: TArr; CompareFunc: TCompareFunc): TArr; static; overload;
  end;

  generic TArraySort<_T> = class
  public type
    TArr = array of _T;
  public
    class procedure QuickSort(var AValues: TArr; ALeft, ARight: SizeInt); static;
  end;

  generic TArraySortFunc<_T> = class
  public type
    TArr = array of _T;
    TCompareFunc = function(const A, B: _T): Integer is nested;
  public
    class procedure QuickSort(var AValues: TArr; ALeft, ARight: SizeInt; CompareFunc: TCompareFunc); static;
  end;

  generic TArraySortWeighted<_T, _W> = class
  public type
    TArr = array of _T;
    TWeightArr = array of _W;
  public
    class procedure QuickSort(var Arr: TArr; var Weights: TWeightArr; iLo, iHi: SizeInt; SortUp: Boolean); static;
  end;

implementation

uses
  simba.container_dict;

class function TArrayRelationship.Difference(x, y: TArr): TArr;
type
  TDict = specialize TDictionary<_T, Boolean>;
var
  dict: TDict;
  i,c: Int32;
  tmp: Boolean;
begin
  Result := [];
  dict := TDict.Create();

  for i:=0 to High(x) do dict[x[i]] := True;
  for i:=0 to High(y) do dict[y[i]] := False;

  c := 0;
  SetLength(Result, Length(x));
  for i:=0 to High(x) do
    if dict.Get(x[i], tmp) and (tmp <> False) then
    begin
      dict[x[i]] := False;
      Result[c] := x[i];
      Inc(c);
    end;
  SetLength(Result, c);

  dict.Free();
end;

class function TArrayRelationship.SymmetricDifference(x, y: TArr): TArr;
type
  TDict = specialize TDictionary<_T, Int32>;
var
  dict, dupes: TDict;
  i,c: Int32;
begin
  Result := nil;
  dict := TDict.Create();
  dupes := TDict.Create();
  for i:=0 to High(x) do dict[x[i]] := 1;
  for i:=0 to High(y) do
  begin
    if dupes.GetDef(y[i], 0) = 1 then continue;
    dict[y[i]] := dict.GetDef(y[i], 0) + 1;
    dupes[y[i]] := 1;
  end;

  c := 0;
  SetLength(Result, Length(x)+Length(y));
  for i:=0 to High(x) do
    if dict.GetDef(x[i], 0) = 1 then
    begin
      dict[x[i]] := $FFFFFF;
      Result[c] := x[i];
      Inc(c);
    end;

  for i:=0 to High(y) do
    if dict.GetDef(y[i], 0) = 1 then
    begin
      dict[y[i]] := $FFFFFF;
      Result[c] := y[i];
      Inc(c);
    end;

  SetLength(Result, c);
  dict.Free();
  dupes.Free();
end;

class function TArrayRelationship.Intersection(x, y: TArr): TArr;
type
  TDict = specialize TDictionary<_T, Int32>;
var
  dict, dupes: TDict;
  i,c: Int32;
begin
  Result := nil;
  dict  := TDict.Create();
  dupes := TDict.Create();
  for i:=0 to High(x) do dict[x[i]] := 1;
  for i:=0 to High(y) do
  begin
    if dupes.GetDef(y[i], 0) = 1 then continue;
    dict[y[i]] := dict.GetDef(y[i], 0) + 1;
    dupes[y[i]] := 1;
  end;

  c := 0;
  SetLength(Result, Length(x)+Length(y));
  for i:=0 to High(x) do
    if dict.GetDef(x[i], 0) = 2 then
    begin
      dict[x[i]] := $FFFFFF;
      Result[c] := x[i];
      Inc(c);
    end;

  for i:=0 to High(y) do
    if dict.GetDef(y[i], 0) = 2 then
    begin
      dict[y[i]] := $FFFFFF;
      Result[c] := y[i];
      Inc(c);
    end;

  SetLength(Result, c);
  dict.Free();
  dupes.Free();
end;

class function TArrayUnique.Unique(Arr: TArr): TArr;
type
  TDict = specialize TDictionary<_T, Boolean>;
var
  dict: TDict;
  i,c: Int32;
begin
  dict := TDict.Create();
  c := 0;
  SetLength(Result, Length(Arr));
  for i:=0 to High(arr) do
    if not dict.Contains(arr[i]) then
    begin
      Result[c] := arr[i];
      Inc(c);

      dict[arr[i]] := True;
    end;
  SetLength(Result, c);
  dict.Free();
end;

class function TArrayUnique.Unique(Arr: TArr; CompareFunc: TCompareFunc): TArr;
var
  I, J, NewLen: Integer;
begin
  Result := Copy(Arr);

  NewLen := 0;
  for I := 0 to High(Result) do
  begin
    J := 0;
    while (J < NewLen) do
    begin
      if CompareFunc(Result[I], Result[J]) then
        Break;
      Inc(J);
    end;

    if (J = NewLen) then
    begin
      Result[NewLen] := Result[I];
      Inc(NewLen);
    end;
  end;
  SetLength(Result, NewLen);
end;

class procedure TArraySort.QuickSort(var AValues: TArr; ALeft, ARight: SizeInt);
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
       QuickSort(AValues, ALeft, J);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

class procedure TArraySortFunc.QuickSort(var AValues: TArr; ALeft, ARight: SizeInt; CompareFunc: TCompareFunc);
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
        QuickSort(AValues, ALeft, J, CompareFunc);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, CompareFunc);
      ARight := J;
    end;
  until ALeft >= ARight;
end;

class procedure TArraySortWeighted.QuickSort(var Arr: TArr; var Weights: TWeightArr; iLo, iHi: SizeInt; SortUp: Boolean);
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
        QuickSort(Arr, Weights, iLo, Hi, SortUp);
      iLo := Lo;
    end else
    begin
      if Lo < iHi then
        QuickSort(Arr, Weights, Lo, iHi, SortUp);
      iHi := Hi;
    end;
  until iLo >= iHi;
end;

end.

