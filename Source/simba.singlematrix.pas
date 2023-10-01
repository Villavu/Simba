{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
 Jarl Holta - https://github.com/slackydev

  - ArgMulti
  - ArgExtrema

}

unit simba.singlematrix;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  TSingleSum = record
    Value: Double;

    class operator :=(const Right: Single): TSingleSum;

    class operator +(const Left: TSingleSum; const Right: Single): TSingleSum;
    class operator -(const Left: TSingleSum; const Right: Single): TSingleSum;

    class operator +(const Left, Right: TSingleSum): TSingleSum;
    class operator -(const Left, Right: TSingleSum): TSingleSum;
  end;
  TSingleSumTable = array of array of TSingleSum;

  TSingleSumTableHelper = type helper for TSingleSumTable
  public
    class function Create(Mat: TSingleMatrix): TSingleSumTable; static;

    function Query(Area: TBox): TSingleSum;
  end;

  TSingleMatrixHelper = type Helper(TSingleMatrixBaseHelper) for TSingleMatrix
  public
    function GetSizeMinusOne(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
    function Copy: TSingleMatrix; overload;
    function Copy(Y1, Y2: Integer): TSingleMatrix; overload;
    function GetValues(Indices: TPointArray): TSingleArray;
    procedure SetValues(Indices: TPointArray;  Values: TSingleArray);
    procedure SetValue(Indices: TPointArray;  Value: Single);
    procedure Fill(Box: TBox; Value: Single); overload;
    procedure Fill(Value: Single); overload;
    function Flatten: TSingleArray;
    function ToIntegerMatrix: TIntegerMatrix;
    function Mean: Double;
    procedure MeanStdev(out MeanValue, Stdev: Double);
    procedure MinMax(out MinValue, MaxValue: Single);
    function Min: Single;
    function Max: Single;
    function ArgMax: TPoint;
    function ArgMin: TPoint;
    function NormMinMax(Alpha, Beta: Single): TSingleMatrix;
    function Indices(Value: Single;  Comparator: EComparator): TPointArray;
    function ArgMulti(Count: Integer;  HiLo: Boolean): TPointArray;
    procedure Smoothen(Block: Integer);
    function Sum: Double;
    function Equals(Other: TSingleMatrix; Epsilon: Single = 0): Boolean;
    procedure ReplaceNaNAndInf(const ReplaceWith: Single);
    function Rot90: TSingleMatrix;
    function ArgExtrema(Count: Int32; HiLo: Boolean = True): TPointArray;
  end;

implementation

uses
  Math,
  simba.math, simba.arraybuffer, simba.heaparray, simba.tpa;

class operator TSingleSum.:=(const Right: Single): TSingleSum;
begin
  Result.Value := Right;
end;

class operator TSingleSum.+(const Left: TSingleSum; const Right: Single): TSingleSum;
begin
  Result.Value := Left.Value + Right;
end;

class operator TSingleSum.-(const Left: TSingleSum; const Right: Single): TSingleSum;
begin
  Result.Value := Left.Value - Right;
end;

class operator TSingleSum.+(const Left, Right: TSingleSum): TSingleSum;
begin
  Result.Value := Left.Value + Right.Value;
end;

class operator TSingleSum.-(const Left, Right: TSingleSum): TSingleSum;
begin
  Result.Value := Left.Value - Right.Value;
end;

class function TSingleSumTableHelper.Create(Mat: TSingleMatrix): TSingleSumTable;
var
  W, H, X, Y: Integer;
begin
  SetLength(Result, Mat.Height, Mat.Width);

  W := Mat.Width - 1;
  H := Mat.Height - 1;

  for Y := 0 to H do
    Result[Y, 0] := Mat[Y,0];

  for Y := 1 to H do
    for X := 0 to W do
      Result[Y, X] := Mat[Y,X] + Result[Y-1, X];

  for Y := 0 to H do
    for X := 1 to W do
      Result[Y, X] += Result[Y, X-1];
end;

function TSingleSumTableHelper.Query(Area: TBox): TSingleSum;
begin
  Result := Self[Area.Y2, Area.X2];

  if (Area.Y1 > 0) then
    Result.Value := Result.Value - Self[Area.Y1 - 1, Area.X2].Value;
  if (Area.X1 > 0) then
    Result.Value := Result.Value - Self[Area.Y2, Area.X1 - 1].Value;
  if (Area.Y1 > 0) and (Area.X1 > 0) then
    Result.Value := Result.Value + Self[Area.Y1 - 1, Area.X1 - 1].Value;
end;

function TSingleMatrixHelper.GetSizeMinusOne(out AWidth, AHeight: Integer): Boolean;
begin
  Result := GetSize(AWidth, AHeight);

  if Result then
  begin
    Dec(AWidth);
    Dec(AHeight);
  end;
end;

procedure TSingleMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

function TSingleMatrixHelper.Copy: TSingleMatrix;
var
  Y, RowSize: Integer;
begin
  Result.SetSize(Self.Width, Self.Height);

  RowSize := Self.Width * SizeOf(Single);
  for Y := 0 to Self.Height - 1 do
    Move(Self[Y, 0], Result[Y, 0], RowSize);
end;

function TSingleMatrixHelper.Copy(Y1, Y2: Integer): TSingleMatrix;
var
  Y, RowSize: Integer;
begin
  RowSize := Width * SizeOf(Single);

  Result.SetSize(Width, Y2-Y1);
  for Y := 0 to Result.Height - 1 do
    Move(Self[Y1+Y, 0], Result[Y, 0], RowSize);
end;

function TSingleMatrixHelper.GetValues(Indices: TPointArray): TSingleArray;
var
  Count, I: Integer;
  W, H: Integer;
begin
  Result := Default(TSingleArray);

  if GetSize(W, H) then
  begin
    Count := 0;
    SetLength(Result, Length(Indices));

    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and
         (Indices[I].X < Width) and (Indices[I].Y < Height) then
      begin
        Result[Count] := Self[Indices[I].Y, Indices[I].X];
        Inc(Count);
      end;

    SetLength(Result, Count);
  end;
end;

procedure TSingleMatrixHelper.SetValues(Indices: TPointArray; Values: TSingleArray);
var
  I, W, H: Integer;
begin
  if (Length(Values) <> Length(Indices)) then
    raise Exception.Create('SetValues: Length(Indices) <> Length(Values)');

  if Self.GetSize(W, H) then
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and (Indices[I].X < W)  and (Indices[I].Y < H) then
        Self[Indices[I].Y, Indices[I].X] := Values[I];
end;

procedure TSingleMatrixHelper.SetValue(Indices: TPointArray; Value: Single);
var
  I, W, H: Integer;
begin
  if Self.GetSize(W, H) then
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and (Indices[I].X < W) and (Indices[I].Y < H) then
        Self[Indices[I].Y, Indices[I].X] := Value;
end;

procedure TSingleMatrixHelper.Fill(Box: TBox; Value: Single);
var
  X, Y: Integer;
begin
  Box.Clip(TBox.Create(0, 0, Width - 1, Height - 1));
  for Y := Box.Y1 to Box.Y2 do
    for X := Box.X1 to Box.X2 do
      Self[Y, X] := Value;
end;

procedure TSingleMatrixHelper.Fill(Value: Single);
var
  W, H, X, Y: Integer;
begin
  if Self.GetSize(W, H) then
  begin
    for X := 0 to W - 1 do Self[0, X] := Value;
    for Y := 1 to H - 1 do Move(Self[0, 0], Self[Y, 0], W * SizeOf(Single));
  end;
end;

function TSingleMatrixHelper.Flatten: TSingleArray;
var
  Y: Integer;
begin
  SetLength(Result, Self.Area);
  for Y := 0 to Self.Height - 1 do
    Move(Self[Y, 0], Result[Y * Width], Width * SizeOf(Single));
end;

function TSingleMatrixHelper.ToIntegerMatrix: TIntegerMatrix;
var
  W, H, X, Y: Integer;
begin
  SetLength(Result, Self.Height, Self.Width);

  W := Self.Width - 1;
  H := Self.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Trunc(Self[Y, X]);
end;

function TSingleMatrixHelper.Mean: Double;
begin
  Result := Self.Sum() / Self.Area();
end;

procedure TSingleMatrixHelper.MeanStdev(out MeanValue, Stdev: Double);
var
  W, H, X, Y: Integer;
  Value: Single;
begin
  MeanValue := 0;
  Stdev := 0;

  if Self.GetSizeMinusOne(W, H) then
  begin
    MeanValue := Self.Mean();

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if IsNumber(Value) then
          Stdev += Sqr(Value - MeanValue);
      end;

    Stdev := Sqrt(Stdev / Self.Area());
  end;
end;

procedure TSingleMatrixHelper.MinMax(out MinValue, MaxValue: Single);
var
  X, Y, W, H: Integer;
  Value: Single;
  HasValue: Boolean;
begin
  MinValue := 0;
  MaxValue := 0;

  if Self.GetSizeMinusOne(W, H) then
  begin
    HasValue := False;

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if (not IsNumber(Value)) then
          Continue;

        if (not HasValue) then
        begin
          MinValue := Value;
          MaxValue := Value;

          HasValue := True;
        end;

        if (Value < MinValue) then MinValue := Value;
        if (Value > MaxValue) then MaxValue := Value;
      end;
  end;
end;

function TSingleMatrixHelper.Min: Single;
var
  MaxValue: Single;
begin
  MinMax(Result, MaxValue);
end;

function TSingleMatrixHelper.Max: Single;
var
  MinValue: Single;
begin
  MinMax(MinValue, Result);
end;

function TSingleMatrixHelper.ArgMax: TPoint;
var
  X, Y, W, H: Integer;
  Value, Best: Single;
  HasValue: Boolean;
begin
  Result := TPoint.Create(0, 0);

  if Self.GetSizeMinusOne(W, H) then
  begin
    HasValue := False;

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if (not IsNumber(Value)) or (HasValue and (Value <= {%H-}Best)) then
          Continue;

        HasValue := True;
        Best := Value;
        Result.X := X;
        Result.Y := Y;
      end;
  end;
end;

function TSingleMatrixHelper.ArgMin: TPoint;
var
  X, Y, W, H: Integer;
  Value, Best: Single;
  HasValue: Boolean;
begin
  Result := TPoint.Create(0, 0);

  if Self.GetSizeMinusOne(W, H) then
  begin
    HasValue := False;

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if (not IsNumber(Value)) or (HasValue and (Value >= {%H-}Best)) then
          Continue;

        HasValue := True;
        Best := Value;
        Result.X := X;
        Result.Y := Y;
      end;
  end;
end;

function TSingleMatrixHelper.NormMinMax(Alpha, Beta: Single): TSingleMatrix;
var
  Lo, Hi, OldRange, NewRange: Single;
  X, Y, W, H: Integer;
begin
  Result.SetSize(Self.Width, Self.Height);

  if Self.GetSizeMinusOne(W, H) then
  begin
    Self.MinMax(Lo, Hi);

    OldRange := Hi - Lo;
    NewRange := Beta - Alpha;
    if IsZero(OldRange) then
      Exit;

    for Y := 0 to H do
      for X := 0 to W do
        if IsNumber(Self[Y, X]) then
          Result[Y, X] := (Self[Y, X] - Lo) / OldRange * NewRange + Alpha;
  end;
end;

function TSingleMatrixHelper.Indices(Value: Single; Comparator: EComparator): TPointArray;
var
  W, H, X, Y: Integer;
  Buffer: TSimbaPointBuffer;
begin
  W := Self.Width - 1;
  H := Self.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      if IsNumber(Self[Y, X]) then
        case Comparator of
          __LT__: if Self[Y, X] < Value  then Buffer.Add(TPoint.Create(X, Y));
          __GT__: if Self[Y, X] > Value  then Buffer.Add(TPoint.Create(X, Y));
          __EQ__: if Self[Y, X] = Value  then Buffer.Add(TPoint.Create(X, Y));
          __LE__: if Self[Y, X] <= Value then Buffer.Add(TPoint.Create(X, Y));
          __GE__: if Self[Y, X] >= Value then Buffer.Add(TPoint.Create(X, Y));
          __NE__: if Self[Y, X] <> Value then Buffer.Add(TPoint.Create(X, Y));
        end;

  Result := Buffer.ToArray(False);
end;

function TSingleMatrixHelper.ArgMulti(Count: Integer; HiLo: Boolean): TPointArray;
var
  W, H, I, Y, X: Integer;
  HeapArray: TSimbaHeapArrayF;
begin
  Result := Default(TPointArray);

  if Self.GetSizeMinusOne(W, H) then
  begin
    case HiLo of
      True:
        for Y := 0 to H do
          for X := 0 to W do
            if IsNumber(Self[Y, X]) and ((Length(HeapArray.Data) < Count) or (Self[Y, X] > HeapArray.Peek.Value)) then
            begin
              if (Length(HeapArray.Data) = Count) then
                HeapArray.Pop(True);

              HeapArray.Push(Self[Y, X], Y*Width+X, True);
            end;

      False:
        for Y := 0 to H do
          for X := 0 to W do
            if IsNumber(Self[Y, X]) and ((Length(HeapArray.Data) < Count) or (Self[Y, X] < HeapArray.Peek.Value)) then
            begin
              if (Length(HeapArray.Data) = Count) then
                HeapArray.Pop(False);

              HeapArray.Push(Self[Y, X], Y*Width+X, False);
            end;
    end;

    SetLength(Result, Length(HeapArray.Data));
    for I := 0 to High(HeapArray.Data) do
    begin
      Result[I].Y := HeapArray.Data[I].Index div Width;
      Result[I].X := HeapArray.Data[I].Index mod Width;
    end;
  end;
end;

{
procedure TSingleMatrixHelper.Smoothen(Block: Integer);
var
  X, Y, W, H: Integer;
  Size: Integer;
  B: TBox;
  SumTable: TSingleSumTable;
begin
  Size := Sqr(Block);
  if (Size <= 1) or (Block mod 2 = 0) then
    Exit;

  Block := Block div 2;
  SumTable := TSingleSumTable.Create(Self);

  Self.GetSizeMinusOne(W,H);

  for Y := 0 to H do
    for X := 0 to W do
    begin
      B.X1 := Math.Max(X-Block, 0);
      B.Y1 := Math.Max(Y-Block, 0);
      B.X2 := Math.Min(X+Block, W);
      B.Y2 := Math.Min(Y+Block, H);

      Size := B.Area;
      with SumTable.Query(B) do
        Self[Y,X] := Value / Size;
    end;
end;
}

procedure TSingleMatrixHelper.Smoothen(Block: Integer);
var
  W, H, X, Y, Radius, fx, fy, Count: Integer;
  lx, ly, hx, hy: Integer;
  Heat: Single;
  Data: TSingleMatrix;
begin
  if (Block * Block <= 1) or (Block mod 2 = 0) then
    Exit;

  if Self.GetSizeMinusOne(W, H) then
  begin
    Data := Self.Copy();
    Radius := (Block div 2);

    for Y := 0 to H do
    begin
      ly := Math.Max(0, Y-Radius);
      hy := Math.Min(H, Y+Radius);

      for X := 0 to W do
      begin
        lx    := Math.Max(0, X-Radius);
        hx    := Math.Min(W, X+Radius);
        Count := 0;
        Heat  := 0;

        for fy := ly to hy do
          for fx := lx to hx do
          begin
            Heat += data[fy, fx];
            Count += 1;
          end;

        Self[Y, X] := Heat / Count;
      end;
    end;
  end;
end;

function TSingleMatrixHelper.Sum: Double;
var
  X, Y, W, H: Integer;
  Value: Single;
begin
  Result := 0;

  if Self.GetSizeMinusOne(W, H) then
    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if IsNumber(Value) then
          Result := Result + Value;
      end;
end;

function TSingleMatrixHelper.Equals(Other: TSingleMatrix; Epsilon: Single): Boolean;
var
  X, Y, W, H: Integer;
begin
  Result := False;
  if (Self.Width <> Other.Width) or (Self.Height <> Other.Height) then
    Exit;

  if Self.GetSizeMinusOne(W, H) then
  begin
    for Y := 0 to H do
      for X := 0 to W do
        if (not SameValue(Self[Y,X], Other[Y,X], Epsilon)) then
          Exit;

    Result := True;
  end;
end;

procedure TSingleMatrixHelper.ReplaceNaNAndInf(const ReplaceWith: Single);
var
  X, Y, W, H: Integer;
begin
  if Self.GetSizeMinusOne(W, H) then
    for Y := 0 to H do
      for X := 0 to W do
        if not IsNumber(Self[Y,X]) then
          Self[Y,X] := ReplaceWith;
end;

function TSingleMatrixHelper.Rot90: TSingleMatrix;
var
  W, H, X, Y: Integer;
begin
  SetLength(Result, Self.Width, Self.Height);

  if Self.GetSizeMinusOne(W, H) then
    for Y := 0 to H do
      for X := 0 to W do
        Result[X, Y] := Self[Y, X];
end;

function TSingleMatrixHelper.ArgExtrema(Count: Int32; HiLo: Boolean = True): TPointArray;
var
  W, H: Integer;
  Buffer: TSimbaPointBuffer;

  function pass_x(): TPointArray;
  var
    X,Y: Integer;
  begin
    Buffer.Clear();

    for Y:=0 to H-1 do
    begin
      X := 1;
      while (X < W) do
      begin
        while (X < W) and (Self[Y,X] >= Self[Y,X-1]) do Inc(X);
        Buffer.Add(X-1,Y);
        while (X < W) and (Self[Y,X] <= Self[Y,X-1]) do Inc(X);
      end;
    end;

    Result := Buffer.ToArray();
  end;

  function pass_y(): TPointArray;
  var
    X,Y: Integer;
  begin
    Buffer.Clear();

    for X:=0 to W-1 do
    begin
      Y := 1;
      while (Y < H) do
      begin
        while (Y < H) and (Self[Y,X] >= Self[Y-1,X]) do Inc(Y);
        Buffer.Add(X,Y-1);
        while (Y < H) and (Self[Y,X] <= Self[Y-1,X]) do Inc(Y);
      end;
    end;

    Result := Buffer.ToArray();
  end;

var
  I: Integer;
  Weights: TSingleArray;
begin
  W := Self.Width();
  H := Self.Height();

  Buffer.Init(Math.Max(2, Ceil(Sqrt(W * H))));

  Result := pass_x().Intersection(pass_y());

  // just use sort, since there arn't that many peaks
  SetLength(Weights, Length(Result));
  for I := 0 to High(Result) do
    Weights[I] := Self[Result[I].Y, Result[I].X];

  Result := Result.Sort(Weights, not HiLo);
  if (Length(Result) > Count) then
    SetLength(Result, Count);
end;

end.

