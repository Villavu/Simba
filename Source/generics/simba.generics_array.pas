{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Generic methods for arrays.
}
unit simba.generics_array;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

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

procedure Sort(var Arr: TIntegerArray); overload;
procedure Sort(var Arr: TSingleArray); overload;
procedure Sort(var Arr: TDoubleArray); overload;
procedure Sort(var Arr: TExtendedArray); overload;

procedure Sort(var Arr: TPointArray; var Weights: TIntegerArray; SortUp: Boolean); overload;
procedure Sort(var Arr: TPointArray; var Weights: TIntegerArray; Lo, Hi: Integer; SortUp: Boolean); overload;
procedure Sort(var Arr: T2DPointArray; var Weights: TIntegerArray; SortUp: Boolean); overload;
procedure Sort(var Arr: T2DPointArray; var Weights: TIntegerArray; Lo, Hi: Integer; SortUp: Boolean); overload;

function Min(const Arr: TIntegerArray): Integer; overload;
function Min(const Arr: TSingleArray): Single; overload;
function Min(const Arr: TDoubleArray): Double; overload;
function Min(const Arr: TExtendedArray): Extended; overload;

function Max(const Arr: TIntegerArray): Integer; overload;
function Max(const Arr: TSingleArray): Single; overload;
function Max(const Arr: TDoubleArray): Double; overload;
function Max(const Arr: TExtendedArray): Extended; overload;

function Sum(const Arr: TIntegerArray): Int64; overload;
function Sum(const Arr: TSingleArray): Extended; overload;
function Sum(const Arr: TDoubleArray): Extended; overload;
function Sum(const Arr: TExtendedArray): Extended; overload;
function Sum(const Arr: TPointArray): TPoint; overload;

function Mean(const Arr: TIntegerArray): Int64; overload;
function Mean(const Arr: TExtendedArray): Extended; overload;

function Mode(const Arr: TIntegerArray): Integer;

procedure Reverse(var Arr: TIntegerArray); overload;
procedure Reverse(var Arr: TPointArray); overload;
procedure Reverse(var Arr: T2DPointArray); overload;

function IndexOf(const Item: Integer; const Arr: TIntegerArray): Integer; overload;
function IndexOf(const Item: String; const Arr: TStringArray): Integer; overload;
function IndexOf(const Item: TPoint; const Arr: TPointArray): Integer; overload;

function IndicesOf(const Item: Integer; const Arr: TIntegerArray): TIntegerArray; overload;
function IndicesOf(const Item: String; const Arr: TStringArray): TIntegerArray; overload;
function IndicesOf(const Item: TPoint; const Arr: TPointArray): TIntegerArray; overload;

// TPoint: Use Matrix
function Unique(const Arr: TPointArray): TPointArray; overload;

// Integer: Use Hashing
function Unique(const Arr: TIntegerArray): TIntegerArray; overload;

// String: Use Hashing
function Unique(const Arr: TStringArray): TStringArray; overload;

// Floats: Use SameValue
function IndexOf(const Item: Single; const Arr: TSingleArray): Integer; overload;
function IndexOf(const Item: Double; const Arr: TDoubleArray): Integer; overload;
function IndexOf(const Item: Extended; const Arr: TExtendedArray): Integer; overload;

function IndicesOf(const Item: Single; const Arr: TSingleArray): TIntegerArray; overload;
function IndicesOf(const Item: Double; const Arr: TDoubleArray): TIntegerArray; overload;
function IndicesOf(const Item: Extended; const Arr: TExtendedArray): TIntegerArray; overload;

function Unique(const Arr: TSingleArray): TSingleArray; overload;
function Unique(const Arr: TDoubleArray): TDoubleArray; overload;
function Unique(const Arr: TExtendedArray): TExtendedArray; overload;

implementation

uses
  math,
  simba.math, simba.helpers_matrix, simba.tpa, simba.overallocatearray;

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

procedure Sort(var Arr: TPointArray; var Weights: TIntegerArray; SortUp: Boolean);
begin
  specialize QuickSortWeighted<TPoint, Integer>(Arr, Weights, Low(Arr), High(Arr), SortUp);
end;

procedure Sort(var Arr: TPointArray; var Weights: TIntegerArray; Lo, Hi: Integer; SortUp: Boolean);
begin
  specialize QuickSortWeighted<TPoint, Integer>(Arr, Weights, Lo, Hi, SortUp);
end;

procedure Sort(var Arr: T2DPointArray; var Weights: TIntegerArray; SortUp: Boolean);
begin
  specialize QuickSortWeighted<TPointArray, Integer>(Arr, Weights, Low(Arr), High(Arr), SortUp);
end;

procedure Sort(var Arr: T2DPointArray; var Weights: TIntegerArray; Lo, Hi: Integer; SortUp: Boolean);
begin
  specialize QuickSortWeighted<TPointArray, Integer>(Arr, Weights, Lo, Hi, SortUp);
end;

function Min(const Arr: TIntegerArray): Integer;
begin
  Result := specialize MinA<Integer>(Arr);
end;

function Min(const Arr: TSingleArray): Single;
begin
  Result := specialize MinA<Single>(Arr);
end;

function Min(const Arr: TDoubleArray): Double;
begin
  Result := specialize MinA<Double>(Arr);
end;

function Min(const Arr: TExtendedArray): Extended;
begin
  Result := specialize MinA<Extended>(Arr);
end;

function Max(const Arr: TIntegerArray): Integer;
begin
  Result := specialize MaxA<Integer>(Arr);
end;

function Max(const Arr: TSingleArray): Single;
begin
  Result := specialize MaxA<Single>(Arr);
end;

function Max(const Arr: TDoubleArray): Double;
begin
  Result := specialize MaxA<Double>(Arr);
end;

function Max(const Arr: TExtendedArray): Extended;
begin
  Result := specialize MaxA<Extended>(Arr);
end;

function Sum(const Arr: TIntegerArray): Int64;
begin
  Result := specialize Sum<Integer, Int64>(Arr);
end;

function Sum(const Arr: TSingleArray): Extended;
begin
  Result := specialize Sum<Single, Extended>(Arr);
end;

function Sum(const Arr: TDoubleArray): Extended;
begin
  Result := specialize Sum<Double, Extended>(Arr);
end;

function Sum(const Arr: TExtendedArray): Extended;
begin
  Result := specialize Sum<Extended, Extended>(Arr);
end;

function Sum(const Arr: TPointArray): TPoint;
begin
  Result := specialize Sum<TPoint, TPoint>(Arr);
end;

function Mean(const Arr: TIntegerArray): Int64;
begin
  Result := Sum(Arr);
  if (Result <> 0) then
    Result := Result div Length(Arr);
end;

function Mean(const Arr: TExtendedArray): Extended;
begin
  Result := Sum(Arr);
  if (Result <> 0) then
    Result := Result / Length(Arr);
end;

function Mode(const Arr: TIntegerArray): Integer;
begin
  Result := specialize Mode<Integer>(Arr);
end;

procedure Sort(var Arr: TIntegerArray);
begin
  specialize QuickSort<Integer>(Arr, Low(Arr), High(Arr));
end;

procedure Sort(var Arr: TSingleArray);
begin
  specialize QuickSort<Single>(Arr, Low(Arr), High(Arr));
end;

procedure Sort(var Arr: TDoubleArray);
begin
  specialize QuickSort<Double>(Arr, Low(Arr), High(Arr));
end;

procedure Sort(var Arr: TExtendedArray);
begin
  specialize QuickSort<Extended>(Arr, Low(Arr), High(Arr));
end;

procedure Reverse(var Arr: TIntegerArray);
begin
  specialize Reverse<Integer>(Arr);
end;

procedure Reverse(var Arr: TPointArray);
begin
  specialize Reverse<TPoint>(Arr);
end;

procedure Reverse(var Arr: T2DPointArray);
begin
  specialize Reverse<TPointArray>(Arr);
end;

function IndexOf(const Item: Integer; const Arr: TIntegerArray): Integer;
begin
  Result := specialize IndexOf<Integer>(Item, Arr);
end;

function IndexOf(const Item: String; const Arr: TStringArray): Integer;
begin
  Result := specialize IndexOf<String>(Item, Arr);
end;

function IndexOf(const Item: TPoint; const Arr: TPointArray): Integer;
begin
  Result := specialize IndexOf<TPoint>(Item, Arr);
end;

function IndicesOf(const Item: Integer; const Arr: TIntegerArray): TIntegerArray;
begin
  Result := specialize IndicesOf<Integer>(Item, Arr);
end;

function IndicesOf(const Item: String; const Arr: TStringArray): TIntegerArray;
begin
   Result := specialize IndicesOf<String>(Item, Arr);
end;

function IndicesOf(const Item: TPoint; const Arr: TPointArray): TIntegerArray;
begin
   Result := specialize IndicesOf<TPoint>(Item, Arr);
end;

function Unique(const Arr: TPointArray): TPointArray;
var
  Matrix: TBooleanMatrix;
  I, Count: Integer;
begin
  SetLength(Result, Length(Arr));
  if (Length(Arr) = 0) then
    Exit;

  Count := 0;

  with GetTPABounds(Arr) do
  begin
    Matrix.SetSize(Width, Height);

    for I := 0 to High(Arr) do
      if not Matrix[Arr[I].Y - Y1, Arr[I].X - X1] then
      begin
        Matrix[Arr[I].Y - Y1, Arr[I].X - X1] := True;
        Result[Count] := Arr[I];
        Inc(Count);
      end;
  end;

  SetLength(Result, Count);
end;

function Unique(const Arr: TIntegerArray): TIntegerArray;
var
  I, J, Size: Integer;
  Value: Integer;
  Table: array of record
    Bucket: TIntegerArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<Integer>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Value and Size] do
    begin
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

function Unique(const Arr: TStringArray): TStringArray;
var
  I, J, Size: Integer;
  Value: String;
  Table: array of record
    Bucket: TStringArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<String>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Hash(Value) and Size] do
    begin
      for J := 0 to Count - 1 do
        if (Value = Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

function IndexOf(const Item: Single; const Arr: TSingleArray): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    if SameValue(Item, Arr[I]) then
      Exit(I);

  Result := -1;
end;

function IndexOf(const Item: Double; const Arr: TDoubleArray): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    if SameValue(Item, Arr[I]) then
      Exit(I);

  Result := -1;
end;

function IndexOf(const Item: Extended; const Arr: TExtendedArray): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Arr) do
    if SameValue(Item, Arr[I]) then
      Exit(I);

  Result := -1;
end;

function IndicesOf(const Item: Single; const Arr: TSingleArray): TIntegerArray;
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

function IndicesOf(const Item: Double; const Arr: TDoubleArray): TIntegerArray;
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

function IndicesOf(const Item: Extended; const Arr: TExtendedArray): TIntegerArray;
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

function Unique(const Arr: TSingleArray): TSingleArray;
var
  I, J, Size: Integer;
  Value: Single;
  Table: array of record
    Bucket: TSingleArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<Single>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Round(Value) and Size] do
    begin
      for J := 0 to Count - 1 do
        if SameValue(Value, Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

function Unique(const Arr: TDoubleArray): TDoubleArray;
var
  I, J, Size: Integer;
  Value: Double;
  Table: array of record
    Bucket: TDoubleArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<Double>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Round(Value) and Size] do
    begin
      for J := 0 to Count - 1 do
        if SameValue(Value, Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

function Unique(const Arr: TExtendedArray): TExtendedArray;
var
  I, J, Size: Integer;
  Value: Extended;
  Table: array of record
    Bucket: TExtendedArray;
    Count: Integer;
  end;
  Buffer: specialize TSimbaOverAllocateArray<Extended>;
label
  Next;
begin
  Buffer.Init();

  SetLength(Table, NextPower2(Length(Arr)));
  Size := High(Table);

  for i := 0 to High(Arr) do
  begin
    Value := Arr[i];

    with Table[Round(Value) and Size] do
    begin
      for J := 0 to Count - 1 do
        if SameValue(Value, Bucket[J]) then
          goto Next;

      if (Count >= Length(Bucket)) then
        SetLength(Bucket, 4 + (Length(Bucket) * 2));

      Bucket[Count] := Value;
      Inc(Count);

      Buffer.Add(Value);
    end;

    Next:
  end;

  Result := Buffer.Trim();
end;

end.

