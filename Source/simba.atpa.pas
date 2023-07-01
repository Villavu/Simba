{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.atpa;

{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  T2DPointArrayHelper = type helper for T2DPointArray
  public
    function Offset(P: TPoint): T2DPointArray; overload;
    function Offset(X, Y: Integer): T2DPointArray; overload;

    function Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): T2DPointArray; overload;
    function Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): T2DPointArray; overload;
    function SortFromSize(Size: Integer): T2DPointArray;
    function SortFromIndex(From: TPoint; Index: Integer = 0): T2DPointArray;
    function SortFromFirstPoint(From: TPoint): T2DPointArray;
    function SortFromFirstPointX(From: TPoint): T2DPointArray;
    function SortFromFirstPointY(From: TPoint): T2DPointArray;

    function SortFrom(From: TPoint): T2DPointArray;

    function SortByArea(LowToHigh: Boolean): T2DPointArray;
    function SortBySize(LowToHigh: Boolean): T2DPointArray;
    function SortByDensity(LowToHigh: Boolean): T2DPointArray;

    function SortByX(LowToHigh: Boolean): T2DPointArray;
    function SortByY(LowToHigh: Boolean): T2DPointArray;

    function SortByShortSide(LowToHigh: Boolean): T2DPointArray;
    function SortByLongSide(LowToHigh: Boolean): T2DPointArray;

    function ExcludeSize(Len: Integer; KeepIf: EComparator): T2DPointArray; overload;
    function ExcludeSize(MinLen, MaxLen: Integer): T2DPointArray; overload;
    function ExcludeSizeEx(MaxLen: Integer): T2DPointArray;
    function ExcludeDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray;
    function ExcludeDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray;

    function Bounds: TBox;
    function BoundsArray: TBoxArray;

    function Mean: TPoint;
    function Means: TPointArray;

    function Merge: TPointArray;

    function Smallest: TPointArray;
    function Largest: TPointArray;

    function Intersection: TPointArray;
  end;

implementation

uses
  math,
  simba.tpa, simba.algo_sort, simba.overallocatearray, simba.integermatrix;

function T2DPointArrayHelper.Sort(Weights: TIntegerArray; LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Copy(Self[I]);

  specialize Sort<TPointArray>(Result, Weights, LowToHigh);
end;

function T2DPointArrayHelper.Sort(Weights: TDoubleArray; LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Copy(Self[I]);

  specialize Sort<TPointArray>(Result, Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortFromSize(Size: Integer): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Abs(Size - Length(Self[I]));

  Result := Self.Sort(Weights, True);
end;

function T2DPointArrayHelper.SortFromIndex(From: TPoint; Index: Integer): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
  begin
    if (Index >= Length(Self[I])) then
      raise Exception.CreateFmt('T2DPointArray.SortFromIndex: Index %d out of range', [Index]);

    Weights[I] := Sqr(From.X - Self[I][Index].X) + Sqr(From.Y - Self[I][Index].Y);
  end;

  Result := Self.Sort(Weights, True);
end;

function T2DPointArrayHelper.SortFromFirstPoint(From: TPoint): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  Result := Self.ExcludeSize(0, __EQ__);

  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Sqr(From.X - Self[I][0].X) + Sqr(From.Y - Self[I][0].Y);

  specialize Sort<TPointArray>(Result, Weights, True);
end;

function T2DPointArrayHelper.SortFromFirstPointX(From: TPoint): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  Result := Self.ExcludeSize(0, __EQ__);

  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Sqr(From.X - Self[I][0].X);

  specialize Sort<TPointArray>(Result, Weights, True);
end;

function T2DPointArrayHelper.SortFromFirstPointY(From: TPoint): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  Result := Self.ExcludeSize(0, __GT__);

  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Sqr(From.Y - Self[I][0].Y);

  specialize Sort<TPointArray>(Result, Weights, True);
end;

function T2DPointArrayHelper.SortFrom(From: TPoint): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    with Self[I].Mean() do
      Weights[I] := Sqr(From.X - X) + Sqr(From.Y - Y);

  Result := Sort(Weights, True);
end;

function T2DPointArrayHelper.Offset(P: TPoint): T2DPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Offset(P);
end;

function T2DPointArrayHelper.Offset(X, Y: Integer): T2DPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Offset(X, Y);
end;

function T2DPointArrayHelper.SortByArea(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TDoubleArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Self[I].Area();

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortBySize(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Length(Self[I]);

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortByDensity(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TDoubleArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Self[I].Density();

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortByX(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Self[I].Bounds.X1;

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortByY(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Self[I].Bounds.Y1;

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortByShortSide(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Self[I].MinAreaRect().ShortSideLen();

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.SortByLongSide(LowToHigh: Boolean): T2DPointArray;
var
  I: Integer;
  Weights: TIntegerArray;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Self) do
    Weights[I] := Self[I].MinAreaRect().LongSideLen();

  Result := Sort(Weights, LowToHigh);
end;

function T2DPointArrayHelper.ExcludeSize(Len: Integer; KeepIf: EComparator): T2DPointArray;
var
  I: Integer;
  Buffer: TSimbaPointArrayBuffer;
begin
  Buffer.Init(Length(Self));

  for I := 0 to High(Self) do
    case KeepIf of
      __LT__: if (Length(Self[I]) <  Len) then Buffer.Add(Copy(Self[I]));
      __GT__: if (Length(Self[I]) >  Len) then Buffer.Add(Copy(Self[I]));
      __EQ__: if (Length(Self[I]) =  Len) then Buffer.Add(Copy(Self[I]));
      __LE__: if (Length(Self[I]) <= Len) then Buffer.Add(Copy(Self[I]));
      __GE__: if (Length(Self[I]) >= Len) then Buffer.Add(Copy(Self[I]));
      __NE__: if (Length(Self[I]) <> Len) then Buffer.Add(Copy(Self[I]));
    end;

  Result := Buffer.Trim();
end;

function T2DPointArrayHelper.ExcludeSize(MinLen, MaxLen: Integer): T2DPointArray;
var
  I: Integer;
  Buffer: TSimbaPointArrayBuffer;
begin
  Buffer.Init(Length(Self));

  for I := 0 to High(Self) do
    if InRange(Length(Self[I]), MinLen, MaxLen) then
      Buffer.Add(Copy(Self[I]));

  Result := Buffer.Trim();
end;

function T2DPointArrayHelper.ExcludeSizeEx(MaxLen: Integer): T2DPointArray;
begin
  Result := Self.ExcludeSize(0, MaxLen);
end;

function T2DPointArrayHelper.ExcludeDimensions(MinShortSide, MinLongSide, MaxShortSide, MaxLongSide: Integer): T2DPointArray;
var
  I: Integer;
  Buffer: TSimbaPointArrayBuffer;
begin
  Buffer.Init(Length(Self));

  for I := 0 to High(Self) do
    with Self[I].MinAreaRect() do
      if InRange(ShortSideLen, MinShortSide, MaxShortSide) and InRange(LongSideLen, MinLongSide, MaxLongSide) then
        Buffer.Add(Copy(Self[I]));

  Result := Buffer.Trim();
end;

function T2DPointArrayHelper.ExcludeDimensionsEx(MinShortSide, MinLongSide: Integer): T2DPointArray;
begin
  Result := Self.ExcludeDimensions(MinShortSide, MinLongSide, Integer.MaxValue, Integer.MaxValue);
end;

function T2DPointArrayHelper.Bounds: TBox;
var
  I: Integer;
begin
  if (Length(Self) = 0) then
    Result := TBox.Default()
  else
  begin
    Result := Self[0].Bounds();
    for I := 1 to High(Self) do
      if (Length(Self[I]) > 0) then
        Result := Result.Combine(Self[I].Bounds());
  end;
end;

function T2DPointArrayHelper.BoundsArray: TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Self) do
    Result[I] := Self[I].Bounds();
end;

function T2DPointArrayHelper.Mean: TPoint;
begin
  Result := Self.Merge().Mean();
end;

function T2DPointArrayHelper.Means: TPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Self) do
    Result[I] := Self[I].Mean();
end;

function T2DPointArrayHelper.Merge: TPointArray;
var
  I, Len, Count: Integer;
begin
  Count := 0;
  Len := 0;
  for I := 0 to High(Self) do
    Len := Len + Length(Self[I]);

  SetLength(Result, Len);
  for I := 0 to High(Self) do
  begin
    Len := Length(Self[I]);
    if (Len > 0) then
      Move(Self[I, 0], Result[Count], Len * SizeOf(TPoint));

    Count := Count + Len;
  end;
end;

function T2DPointArrayHelper.Smallest: TPointArray;
var
  I: Integer;
begin
  if (Length(Self) = 0) then
    Exit(Default(TPointArray));

  Result := Self[0];
  for I := 1 to High(Self) do
    if (Length(Self[I]) < Length(Result)) Then
      Result := Self[I];
end;

function T2DPointArrayHelper.Largest: TPointArray;
var
  I: Integer;
begin
  if (Length(Self) = 0) then
    Exit(Default(TPointArray));

  Result := Self[0];
  for I := 1 to High(Self) do
    if (Length(Self[I]) > Length(Result)) Then
      Result := Self[I];
end;

function T2DPointArrayHelper.Intersection: TPointArray;
var
  Matrix, DupMatrix: TIntegerMatrix;
  I, J: Integer;
  X, Y, Target: Integer;
  Buffer: TSimbaPointBuffer;
begin
  with Self.Bounds() do
  begin
    Matrix.SetSize(Width, Height);
    DupMatrix.SetSize(Width, Height);

    Target := High(Self);

    for I := 0 to High(Self) do
      for J := 0 to High(Self[I]) do
      begin
        X := Self[I, J].X - X1;
        Y := Self[I, J].Y - Y1;

        if DupMatrix[Y, X] <> I then
        begin
          DupMatrix[Y,X] := I;

          Matrix[Y, X] += 1;
          if (Matrix[Y, X] = Target) then
            Buffer.Add(Self[I, J]);
        end;
      end;
  end;

  Result := Buffer.Trim();
end;


end.

