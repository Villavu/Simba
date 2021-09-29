{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.type_singlematrix;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.type_matrix, simba.mufasatypes;

type
  TSingleMatrixHelper = type helper(TSingleMatrix_BaseHelper) for TSingleMatrix
    function ToIntegerMatrix: TIntegerMatrix;
    function Mean: Single;
    procedure MeanStdev(out Mean, Stdev: Double);
    procedure MinMax(out MinValue, MaxValue: Single);
    function Min: Single;
    function Max: Single;
    function ArgMax: TPoint;
    function ArgMin: TPoint;
    function NormMinMax(Alpha, Beta: Single): TSingleMatrix;
    function Indices(Value: Single; Comparator: EComparator): TPointArray;
    function ArgMulti(Count: Integer; HiLo: Boolean): TPointArray;
  end;

implementation

uses
  simba.heaparray, simba.math;

function TSingleMatrixHelper.ToIntegerMatrix: TIntegerMatrix;
var
  X,Y,W,H: Integer;
begin
  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit(nil);

  SetLength(Result, H+1, W+1);
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Trunc(Self[Y, X]);
end;

function TSingleMatrixHelper.Mean: Single;
var
  W,H,X,Y: Integer;
  Sum: Double;
begin
  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit(0);

  Sum := 0;
  for Y := 0 to H do
    for X := 0 to W do
      if IsNumber(Self[Y,X]) then
        Sum += Self[Y,X];

  Result := Sum / ((W+1)*(H+1));
end;

procedure TSingleMatrixHelper.MeanStdev(out Mean, Stdev: Double);
var
  W,H,X,Y: Integer;
  Sum, Square: Double;
begin
  Mean := 0;
  Stdev := 0;

  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit;

  Sum := 0;
  for Y := 0 to H do
    for X := 0 to W do
      if IsNumber(Self[Y,X]) then
        Sum += Self[Y,X];
  Mean := Sum / ((W+1)*(H+1));

  Square := 0;
  for Y := 0 to H do
    for X := 0 to W do
      if IsNumber(Self[Y,X]) then
        Square += Sqr(Self[Y,X] - Mean);
  Stdev := Sqrt(Square / ((W+1)*(H+1)));
end;

procedure TSingleMatrixHelper.MinMax(out MinValue, MaxValue: Single);
var
  W,H,X,Y: Integer;
  Sum, Square: Double;
  Value: Single;
begin
  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit;

  MinValue := Self[0,0];
  MaxValue := Self[0,0];
  for Y := 0 to H do
    for X := 0 to W do
    begin
      Value := Self[Y,X];
      if IsNumber(Value) then
      begin
        if (Value > MaxValue) then MaxValue := Value;
        if (Value < MinValue) then MinValue := Value;
      end;
    end;
end;

function TSingleMatrixHelper.Min: Single;
var
  MinValue, MaxValue: Single;
begin
  Self.MinMax(MinValue, MaxValue);

  Result := MinValue;
end;

function TSingleMatrixHelper.Max: Single;
var
  MinValue, MaxValue: Single;
begin
  Self.MinMax(MinValue, MaxValue);

  Result := MaxValue;
end;

function TSingleMatrixHelper.ArgMax: TPoint;
var
  X,Y,W,H:Integer;
begin
  Result.X := 0;
  Result.Y := 0;

  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit;

  for Y:=0 to H do
    for X:=0 to W do
      if IsNumber(Self[Y,X]) and (Self[Y,X] > Self[Result.Y, Result.X]) then
      begin
        Result.X := X;
        Result.Y := Y;
      end;
end;

function TSingleMatrixHelper.ArgMin: TPoint;
var
  X,Y,W,H:Integer;
begin
  Result.X := 0;
  Result.Y := 0;

  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit;

  for Y:=0 to H do
    for X:=0 to W do
      if IsNumber(Self[Y,X]) and (Self[Y,X] < Self[Result.Y, Result.X]) then
      begin
        Result.X := X;
        Result.Y := Y;
      end;
end;

function TSingleMatrixHelper.NormMinMax(Alpha, Beta: Single): TSingleMatrix;
var
  Lo,Hi,OldRange,NewRange: Single;
  X,Y,W,H: Integer;
begin
  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit(nil);

  SetLength(Result, H+1, W+1);

  MinMax(Lo, Hi);

  OldRange := Hi-Lo;
  NewRange := Beta-Alpha;

  if (OldRange = 0) then
    Exit;

  for Y:=0 to H do
    for X:=0 to W do
      if IsNumber(Self[Y,X]) then
        Result[Y,X] := (Self[Y,X] - Lo) / OldRange * NewRange + Alpha;
end;

function TSingleMatrixHelper.Indices(Value: Single; Comparator: EComparator): TPointArray;
var
  Count: Integer;
  X,Y,W,H: Integer;

  procedure Match; inline;
  begin
    if Count = Length(Result) then
      SetLength(Result, Count*2);
    Result[Count].X := X;
    Result[Count].Y := Y;
    Inc(Count);
  end;

begin
  W := Self.Width - 1;
  H := Self.Height - 1;
  if (W < 0) or (H < 0) then
    Exit(nil);

  Count := 0;
  SetLength(Result, 512);
  for Y:=0 to H do
    for X:=0 to W do
      if IsNumber(Self[Y,X]) then
        case Comparator of
          __LT__: if Self[Y,X] < Value then Match();
          __GT__: if Self[Y,X] > Value then Match();
          __EQ__: if Self[Y,X] = Value then Match();
          __LE__: if Self[Y,X] <= Value then Match();
          __GE__: if Self[Y,X] >= Value then Match();
          __NE__: if Self[Y,X] <> Value then Match();
        end;

  SetLength(Result, Count);
end;

function TSingleMatrixHelper.ArgMulti(Count: Integer; HiLo: Boolean): TPointArray;
var
  W, H, I, Y, X: Integer;
  HeapArray: TSimbaHeapArrayF;
begin
  W := Width - 1;
  H := Height - 1;
  if (W < 0) or (H < 0) then
    Exit(nil);

  HeapArray := TSimbaHeapArrayF.Create();

  case HiLo of
    True:
      for Y := 0 to H do
        for X := 0 to W do
          if IsNumber(Self[Y, X]) and ((Length(HeapArray.Data) < Count) or (Self[Y, X] > HeapArray.Peek.Value)) then
          begin
            if (Length(HeapArray.Data) = Count) then
              HeapArray.Pop(True);

            HeapArray.Push(Self[Y, X], Y*(W+1)+X, True);
          end;

    False:
      for Y := 0 to H do
        for X := 0 to W do
          if IsNumber(Self[Y, X]) and ((Length(HeapArray.Data) < Count) or (Self[Y, X] < HeapArray.Peek.Value)) then
          begin
            if (Length(HeapArray.Data) = Count) then
              HeapArray.Pop(False);

            HeapArray.Push(Self[Y, X], Y*(W+1)+X, False);
          end;
  end;

  W += 1;
  H += 1;
  SetLength(Result, Length(HeapArray.Data));
  for I := 0 to High(HeapArray.Data) do
  begin
    Result[I].Y := HeapArray.Data[I].Index div W;
    Result[I].X := HeapArray.Data[I].Index mod W;
  end;

  HeapArray.Free();
end;

end.

