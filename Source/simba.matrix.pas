{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Basic matrix methods to be added to matrix types.
}
unit simba.matrix;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

type
  generic TMatrix<T> = array of array of T;

generic function MatrixWidth<_T>(const Matrix: specialize TMatrix<_T>): Integer;
generic function MatrixHeight<_T>(const Matrix: specialize TMatrix<_T>): Integer;
generic function MatrixArea<_T>(const Matrix: specialize TMatrix<_T>): Integer;
generic function MatrixSize<_T>(const Matrix: specialize TMatrix<_T>; out Width, Height: Integer): Boolean;
generic procedure MatrixSetSize<_T>(var Matrix: specialize TMatrix<_T>; Width, Height: Integer);
generic function MatrixGetValues<_T>(const Matrix: specialize TMatrix<_T>; const Indices: TPointArray): specialize TArray<_T>;
generic function MatrixFlatten<_T>(const Matrix: specialize TMatrix<_T>): specialize TArray<_T>;
generic procedure MatrixFill<_T>(const Matrix: specialize TMatrix<_T>; Area: TBox; const Value: _T); overload;
generic procedure MatrixFill<_T>(const Matrix: specialize TMatrix<_T>; const Value: _T); overload;
generic procedure MatrixSetValues<_T>(const Matrix: specialize TMatrix<_T>; const Indices: TPointArray; const Values: specialize TArray<_T>); overload;
generic procedure MatrixSetValues<_T>(const Matrix: specialize TMatrix<_T>; const Indices: TPointArray; const Value: _T); overload;
generic procedure MatrixMinMax<_T>(const Matrix: specialize TMatrix<_T>; out MinValue, MaxValue: _T);
generic function MatrixMin<_T>(const Matrix: specialize TMatrix<_T>): _T;
generic function MatrixMax<_T>(const Matrix: specialize TMatrix<_T>): _T;
generic function MatrixMean<_T>(const Matrix: specialize TMatrix<_T>): Double;
generic function MatrixArgMin<_T>(const Matrix: specialize TMatrix<_T>): TPoint;
generic function MatrixArgMax<_T>(const Matrix: specialize TMatrix<_T>): TPoint;
generic function MatrixIndices<_T>(const Matrix: specialize TMatrix<_T>; const Value: _T; const Comparator: EComparator): TPointArray;
generic procedure MatrixMeanStdev<_T>(const Matrix: specialize TMatrix<_T>; out Mean, Stdev: Double);
generic function MatrixNormMinMax<_T>(const Matrix: specialize TMatrix<_T>; const Alpha, Beta: _T): specialize TMatrix<_T>;
generic function MatrixArgMulti<_T>(const Matrix: specialize TMatrix<_T>; const Count: Integer; const HiLo: Boolean): TPointArray;
generic function MatrixRot90<_T>(const Matrix: specialize TMatrix<_T>): specialize TMatrix<_T>;

implementation

uses
  math,
  simba.math, simba.heaparray;

generic function MatrixWidth<_T>(const Matrix: specialize TMatrix<_T>): Integer;
begin
  if Length(Matrix) > 0 then
    Result := Length(Matrix[0])
  else
    Result := 0;
end;

generic function MatrixHeight<_T>(const Matrix: specialize TMatrix<_T>): Integer;
begin
  Result := Length(Matrix);
end;

generic function MatrixArea<_T>(const Matrix: specialize TMatrix<_T>): Integer;
begin
  Result := specialize MatrixWidth<_T>(Matrix) * specialize MatrixHeight<_T>(Matrix);
end;

generic function MatrixSize<_T>(const Matrix: specialize TMatrix<_T>; out Width, Height: Integer): Boolean;
begin
  Width := 0;
  Height := Length(Matrix);
  if (Height > 0) then
    Width := Length(Matrix[0]);

  Result := (Height > 0) and (Width > 0);
end;

generic procedure MatrixSetSize<_T>(var Matrix: specialize TMatrix<_T>; Width, Height: Integer);
begin
  SetLength(Matrix, Height, Width);
end;

generic function MatrixGetValues<_T>(const Matrix: specialize TMatrix<_T>; const Indices: TPointArray): specialize TArray<_T>;
var
  I, Width, Height, Count: Integer;
begin
  Result := nil;

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Count := 0;
    SetLength(Result, Length(Indices));

    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
         (Indices[I].X < Width) and (Indices[I].Y < Height) then
      begin
        Result[Count] := Matrix[Indices[I].Y][Indices[I].X];
        Inc(Count);
      end;

    SetLength(Result, Count);
  end;
end;

generic function MatrixFlatten<_T>(const Matrix: specialize TMatrix<_T>): specialize TArray<_T>;
var
  Width, Height, Y: Integer;
begin
  Result := nil;

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    SetLength(Result, Width * Height);
    for Y := 0 to Height - 1 do
      Move(Matrix[Y][0], Result[Y * Width], Width * SizeOf(_T));
  end;
end;

generic procedure MatrixFill<_T>(const Matrix: specialize TMatrix<_T>; Area: TBox; const Value: _T); overload;
var
  X, Y, Width, Height: Integer;
begin
  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Area.X1 := Max(0, Area.X1);
    Area.Y1 := Max(0, Area.Y1);
    Area.X2 := Min(Width - 1,  Area.X2);
    Area.Y2 := Min(Height - 1, Area.Y2);
    if (Area.X2 - Area.X1 <= 0) or (Area.Y2 - Area.Y1 <= 0) then
      Exit;

    for X := Area.X1 to Area.X2 do
      Matrix[Area.Y1][X] := Value;
    for Y := Area.Y1 + 1 to Area.Y2 do
      Move(Matrix[Area.Y1][Area.X1], Matrix[Y][Area.X1], (Area.X2 - Area.X1 + 1) * SizeOf(_T));
  end;
end;

generic procedure MatrixFill<_T>(const Matrix: specialize TMatrix<_T>; const Value: _T); overload;
var
  Area: TBox;
begin
  Area.X1 := 0;
  Area.Y1 := 0;
  Area.X2 := $FFFFFF;
  Area.Y2 := $FFFFFF;

  specialize MatrixFill<_T>(Matrix, Area, Value);
end;

generic procedure MatrixSetValues<_T>(const Matrix: specialize TMatrix<_T>; const Indices: TPointArray; const Values: specialize TArray<_T>); overload;
var
  Width, Height, I: Integer;
begin
  if (Length(Values) <> Length(Indices)) then
    raise Exception.Create('MatrixSetValues: `Values` and `Indices` must be equal lengths');

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
         (Indices[I].X < Width)  and (Indices[I].Y < Height) then
          Matrix[Indices[I].Y][Indices[I].X] := Values[I];
  end;
end;

generic procedure MatrixSetValues<_T>(const Matrix: specialize TMatrix<_T>; const Indices: TPointArray; const Value: _T); overload;
var
  Width, Height, I: Integer;
begin
  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
         (Indices[I].X < Width)  and (Indices[I].Y < Height) then
          Matrix[Indices[I].Y][Indices[I].X] := Value;
  end;
end;

generic procedure MatrixMinMax<_T>(const Matrix: specialize TMatrix<_T>; out MinValue, MaxValue: _T);
var
  X, Y, Width, Height: Integer;
  Value: _T;
begin
  MinValue := Default(_T);
  MaxValue := Default(_T);

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Width -= 1;
    Height -= 1;

    MinValue := Matrix[0,0];
    MaxValue := Matrix[0,0];

    for Y := 0 to Height do
      for X := 0 to Width do
      begin
        Value := Matrix[Y, X];
        if IsNumber(Value) then
        begin
          if (Value > MaxValue) then MaxValue := Value;
          if (Value < MinValue) then MinValue := Value;
        end;
      end;
  end;
end;

generic function MatrixMin<_T>(const Matrix: specialize TMatrix<_T>): _T;
var
  MinValue, MaxValue: _T;
begin
  specialize MatrixMinMax<_T>(Matrix, MinValue, MaxValue);

  Result := MinValue;
end;

generic function MatrixMax<_T>(const Matrix: specialize TMatrix<_T>): _T;
var
  MinValue, MaxValue: _T;
begin
  specialize MatrixMinMax<_T>(Matrix, MinValue, MaxValue);

  Result := MaxValue;
end;

generic function MatrixMean<_T>(const Matrix: specialize TMatrix<_T>): Double;
var
  Width, Height, X, Y: Integer;
  Sum: Double;
begin
  Result := Default(_T);

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Width -= 1;
    Height -= 1;

    Sum := 0;
    for Y := 0 to Height do
      for X := 0 to Width do
        if IsNumber(Matrix[Y,X]) then
          Sum += Matrix[Y,X];

    Result := Sum / ((Width + 1) * (Height + 1));
  end;
end;

generic function MatrixArgMin<_T>(const Matrix: specialize TMatrix<_T>): TPoint;
var
  Width, Height, X, Y: Integer;
begin
  Result := Default(TPoint);

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Width -= 1;
    Height -= 1;

    for Y := 0 to Height do
      for X := 0 to Width do
        if IsNumber(Matrix[Y, X]) and (Matrix[Y, X] < Matrix[Result.Y, Result.X]) then
        begin
          Result.X := X;
          Result.Y := Y;
        end;
  end;
end;

generic function MatrixArgMax<_T>(const Matrix: specialize TMatrix<_T>): TPoint;
var
  Width, Height, X, Y: Integer;
begin
  Result := Default(TPoint);

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Width -= 1;
    Height -= 1;

    for Y := 0 to Height do
      for X := 0 to Width do
        if IsNumber(Matrix[Y, X]) and (Matrix[Y, X] > Matrix[Result.Y, Result.X]) then
        begin
          Result.X := X;
          Result.Y := Y;
        end;
  end;
end;

generic function MatrixIndices<_T>(const Matrix: specialize TMatrix<_T>; const Value: _T; const Comparator: EComparator): TPointArray;
var
  Count: Integer;
  X, Y, Width, Height: Integer;

  procedure Match; inline;
  begin
    if Count = Length(Result) then
      SetLength(Result, Count*2);
    Result[Count].X := X;
    Result[Count].Y := Y;
    Inc(Count);
  end;

begin
  Result := nil;

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Width -= 1;
    Height -= 1;

    Count := 0;
    SetLength(Result, 512);

    for Y:=0 to Height do
      for X:=0 to Width do
        if IsNumber(Matrix[Y,X]) then
          case Comparator of
            __LT__: if Matrix[Y,X] < Value then Match();
            __GT__: if Matrix[Y,X] > Value then Match();
            __EQ__: if Matrix[Y,X] = Value then Match();
            __LE__: if Matrix[Y,X] <= Value then Match();
            __GE__: if Matrix[Y,X] >= Value then Match();
            __NE__: if Matrix[Y,X] <> Value then Match();
          end;

    SetLength(Result, Count);
  end;
end;

generic procedure MatrixMeanStdev<_T>(const Matrix: specialize TMatrix<_T>; out Mean, Stdev: Double);
var
  Width, Height, X, Y: Integer;
  Sum, Square: Double;
begin
  Mean := 0;
  Stdev := 0;

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    Width -= 1;
    Height -= 1;

    Sum := 0;
    for Y := 0 to Height do
      for X := 0 to Width do
        if IsNumber(Matrix[Y, X]) then
          Sum += Matrix[Y, X];
    Mean := Sum / ((Width+1)*(Height+1));

    Square := 0;
    for Y := 0 to Height do
      for X := 0 to Width do
        if IsNumber(Matrix[Y ,X]) then
          Square += Sqr(Matrix[Y, X] - Mean);

    Stdev := Sqrt(Square / ((Width+1)*(Height+1)));
  end;
end;

generic function MatrixNormMinMax<_T>(const Matrix: specialize TMatrix<_T>; const Alpha, Beta: _T): specialize TMatrix<_T>;
var
  Lo, Hi, OldRange, NewRange: _T;
  X, Y, Width, Height: Integer;
begin
  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    SetLength(Result, Height, Width);

    specialize MatrixMinMax<_T>(Matrix, Lo, Hi);

    OldRange := Hi - Lo;
    NewRange := Beta - Alpha;
    if (OldRange = 0) then
      Exit;

    Width -= 1;
    Height -= 1;

    for Y:=0 to Height do
      for X:=0 to Width do
        if IsNumber(Matrix[Y, X]) then
          Result[Y, X] := (Matrix[Y, X] - Lo) / OldRange * NewRange + Alpha;
  end;
end;

generic function MatrixArgMulti<_T>(const Matrix: specialize TMatrix<_T>; const Count: Integer; const HiLo: Boolean): TPointArray;
type
  THeapArray = specialize TSimbaHeapArray<_T>;
var
  Width, Height, I, Y, X: Integer;
  HeapArray: THeapArray;
begin
  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    HeapArray := THeapArray.Create();

    Width -= 1;
    Height -= 1;

    case HiLo of
      True:
        for Y := 0 to Height do
          for X := 0 to Width do
            if IsNumber(Matrix[Y, X]) and ((Length(HeapArray.Data) < Count) or (Matrix[Y, X] > HeapArray.Peek.Value)) then
            begin
              if (Length(HeapArray.Data) = Count) then
                HeapArray.Pop(True);

              HeapArray.Push(Matrix[Y, X], Y*(Width+1)+X, True);
            end;

      False:
        for Y := 0 to Height do
          for X := 0 to Width do
            if IsNumber(Matrix[Y, X]) and ((Length(HeapArray.Data) < Count) or (Matrix[Y, X] < HeapArray.Peek.Value)) then
            begin
              if (Length(HeapArray.Data) = Count) then
                HeapArray.Pop(False);

              HeapArray.Push(Matrix[Y, X], Y*(Width+1)+X, False);
            end;
    end;

    Width += 1;
    Height += 1;

    SetLength(Result, Length(HeapArray.Data));
    for I := 0 to High(HeapArray.Data) do
    begin
      Result[I].Y := HeapArray.Data[I].Index div Width;
      Result[I].X := HeapArray.Data[I].Index mod Width;
    end;

    HeapArray.Free();
  end;
end;

generic function MatrixRot90<_T>(const Matrix: specialize TMatrix<_T>): specialize TMatrix<_T>;
var
  Width, Height, X, Y: Integer;
begin
  Result := nil;

  if specialize MatrixSize<_T>(Matrix, Width, Height) then
  begin
    SetLength(Result, Width, Height);

    Width -= 1;
    Height -= 1;

    for Y := 0 to Height do
      for X := 0 to Width do
        Result[X, Y] := Matrix[Y, X];
  end;
end;

end.

