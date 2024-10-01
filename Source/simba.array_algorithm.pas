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
  Classes, SysUtils,
  simba.base;

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

  generic TArrayEquals<_T> = class
  public type
    TArr = array of _T;
  public
    class function Equals(A, B: TArr): Boolean; static; reintroduce;
  end;

  generic TArrayEqualsFunc<_T> = class
  public type
    TArr = array of _T;
    TEqualFunc = function(const A, B: _T): Boolean is nested;
  public
    class function Equals(A, B: TArr; EqualFunc: TEqualFunc): Boolean; static; reintroduce;
  end;

  generic TArrayIndexOf<_T> = class
  public type
    TArr = array of _T;
  public
    class function IndexOf(Item: _T; Arr: TArr): Integer; static;
  end;

  generic TArrayIndexOfFunc<_T> = class
  public type
    TArr = array of _T;
    TEqualFunc = function(const A, B: _T): Boolean is nested;
  public
    class function IndexOf(Item: _T; Arr: TArr; EqualFunc: TEqualFunc): Integer; static;
  end;

  generic TArrayIndicesOf<_T> = class
  public type
    TArr = array of _T;
  public
    class function IndicesOf(Item: _T; Arr: TArr): TIntegerArray; static;
  end;

  generic TArrayIndicesOfFunc<_T> = class
  public type
    TArr = array of _T;
    TEqualFunc = function(const A, B: _T): Boolean is nested;
  public
    class function IndicesOf(Item: _T; Arr: TArr; EqualFunc: TEqualFunc): TIntegerArray; static;
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
  dict: TDict;
  tmp: TArr;
  i,c: Int32;
begin
  c      := 0;
  Result := nil;
  dict   := TDict.Create();
  if Length(x) > Length(y) then
  begin
    tmp := x;
    x   := y;
    y   := tmp;
  end;

  SetLength(Result, Min(Length(y),Length(x)));
  for i:=0 to High(x) do dict[x[i]] := 1;
  for i:=0 to High(y) do
    if dict.Contains(y[i]) then
    begin
      Result[c] := y[i];
      Inc(c);
    end;

  SetLength(Result, c);
  dict.Free();
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

class function TArrayEquals.Equals(A, B: TArr): Boolean;
var
  I: Integer;
begin
  if (Length(A) <> Length(B)) then
    Exit(False);
  if (Length(A) = 0) and (Length(B) = 0) then
    Exit(True);

  if (not IsManagedType(_T)) then
    Result := CompareMem(@A[0], @B[0], Length(A) * SizeOf(_T))
  else
  begin
    for I := 0 to High(A) do
      if (A[I] <> B[I]) then
        Exit(False);
    Exit(True);
  end;
end;

class function TArrayEqualsFunc.Equals(A, B: TArr; EqualFunc: TEqualFunc): Boolean;
var
  I: Integer;
begin
  if (Length(A) <> Length(B)) then
    Exit(False);
  if (Length(A) = 0) and (Length(B) = 0) then
    Exit(True);

  for I := 0 to High(A) do
    if not EqualFunc(A[I], B[I]) then
      Exit(False);

  Result := True;
end;

class function TArrayIndexOf.IndexOf(Item: _T; Arr: TArr): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Length(Arr) = 0) then
    Exit;

  // can use these for better code
  if (not IsManagedType(_T)) and ((SizeOf(_T) = 1) or (SizeOf(_T) = 2) or (SizeOf(_T) = 4) or (SizeOf(_T) = 8)) then
  begin
    case SizeOf(_T) of
      1: Result := IndexByte(Arr[0], Length(Arr), PByte(@Item)^);
      2: Result := IndexWord(Arr[0], Length(Arr), PWord(@Item)^);
      4: Result := IndexDWord(Arr[0], Length(Arr), PDWord(@Item)^);
      8: Result := IndexQWord(Arr[0], Length(Arr), PQWord(@Item)^);
    end;
    Exit;
  end;

  for I := 0 to High(Arr) do
    if (Arr[I] = Item) then
      Exit(I);
end;

class function TArrayIndexOfFunc.IndexOf(Item: _T; Arr: TArr; EqualFunc: TEqualFunc): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Arr) do
    if EqualFunc(Arr[I], Item) then
      Exit(I);
end;

class function TArrayIndicesOf.IndicesOf(Item: _T; Arr: TArr): TIntegerArray;
var
  I, Count: Integer;
begin
  SetLength(Result, 8);
  Count := 0;

  for I := 0 to High(Arr) do
    if (Arr[I] = Item) then
    begin
      if (Count >= Length(Result)) then
        SetLength(Result, Length(Result) * 2);
      Result[Count] := I;
      Inc(Count);
    end;

  SetLength(Result, Count);
end;

class function TArrayIndicesOfFunc.IndicesOf(Item: _T; Arr: TArr; EqualFunc: TEqualFunc): TIntegerArray;
var
  I, Count: Integer;
begin
  SetLength(Result, 8);
  Count := 0;

  for I := 0 to High(Arr) do
    if EqualFunc(Arr[I], Item) then
    begin
      if (Count >= Length(Result)) then
        SetLength(Result, Length(Result) * 2);
      Result[Count] := I;
      Inc(Count);
    end;

  SetLength(Result, Count);
end;

end.

