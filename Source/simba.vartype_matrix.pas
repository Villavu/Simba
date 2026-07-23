{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.vartype_matrix;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TIntegerMatrixHelper = type helper for TIntegerMatrix
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetArea: Integer;
  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Area: Integer read GetArea;

    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(NewWidth, NewHeight: Integer);
    function Copy: TIntegerMatrix; overload;
    function Copy(Y1, Y2: Integer): TIntegerMatrix; overload;
    function GetValues(Indices: TPointArray): TIntegerArray;
    procedure SetValues(Indices: TPointArray; Values: TIntegerArray);
    procedure SetValue(Indices: TPointArray; Value: Integer);
    procedure Fill(Box: TBox; Value: Integer); overload;
    procedure Fill(Value: Integer); overload;
    function Flatten: TIntegerArray;
    function Indices(Value: Integer; Comparator: EComparator): TPointArray;
  end;

  TByteMatrixHelper = type helper for TByteMatrix
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetArea: Integer;
  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Area: Integer read GetArea;

    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(NewWidth, NewHeight: Integer);
  end;

  TBooleanMatrixHelper = type helper for TBooleanMatrix
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetArea: Integer;
  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Area: Integer read GetArea;

    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(NewWidth, NewHeight: Integer);
  end;

  TSingleMatrixHelper = type helper for TSingleMatrix
  private
    function GetArgMax: TPoint;
    function GetArgMin: TPoint;
    function GetMax: Single;
    function GetMin: Single;
    function GetSum: Double;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetArea: Integer;
    function GetMean: Single;
  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Area: Integer read GetArea;
    property Min: Single read GetMin;
    property Max: Single read GetMax;
    property ArgMax: TPoint read GetArgMax;
    property ArgMin: TPoint read GetArgMin;
    property Sum: Double read GetSum;
    property Mean: Single read GetMean;

    procedure SetSize(NewWidth, NewHeight: Integer);
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    function GetSizeMinusOne(out AWidth, AHeight: Integer): Boolean;

    function Copy: TSingleMatrix; overload;
    function Copy(Y1, Y2: Integer): TSingleMatrix; overload;
    function GetValues(Indices: TPointArray): TSingleArray;
    procedure SetValues(Indices: TPointArray; Values: TSingleArray);
    procedure SetValue(Indices: TPointArray; Value: Single);
    procedure Fill(Box: TBox; Value: Single); overload;
    procedure Fill(Value: Single); overload;
    function Flatten: TSingleArray;
    function ToIntegerMatrix: TIntegerMatrix;
    procedure MeanStdev(out MeanValue, Stdev: Double);
    procedure MinMax(out MinValue, MaxValue: Single);

    function NormMinMax(Alpha, Beta: Single): TSingleMatrix;
    function Indices(Value: Single;  Comparator: EComparator): TPointArray;
    function ArgMulti(Count: Integer;  HiLo: Boolean): TPointArray;
    procedure Smoothen(Block: Integer);

    function Equals(Other: TSingleMatrix; Epsilon: Single = 0): Boolean;
    procedure ReplaceNaNAndInf(const ReplaceWith: Single);
    function Rot90: TSingleMatrix;
    function ArgExtrema(Count: Int32; HiLo: Boolean = True; XYIntersection: Boolean = True): TPointArray;
  end;

  TDoubleMatrixHelper = type helper for TDoubleMatrix
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetArea: Integer;
  public
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Area: Integer read GetArea;

    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(NewWidth, NewHeight: Integer);
  end;

implementation

uses
  Math,
  simba.math, simba.containers, simba.container_heaparray, simba.vartype_pointarray, simba.vartype_box;

function TIntegerMatrixHelper.GetWidth: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TIntegerMatrixHelper.GetHeight: Integer;
begin
  Result := Length(Self);
end;

function TIntegerMatrixHelper.GetArea: Integer;
begin
  Result := Width * Height;
end;

function TIntegerMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TIntegerMatrixHelper.SetSize(NewWidth, NewHeight: Integer);
begin
  SetLength(Self, NewHeight, NewWidth);
end;

function TIntegerMatrixHelper.Copy: TIntegerMatrix;
var
  Y, RowSize: Integer;
begin
  Result.SetSize(Self.Width, Self.Height);

  RowSize := Self.Width * SizeOf(Integer);
  for Y := 0 to Self.Height - 1 do
    Move(Self[Y, 0], Result[Y, 0], RowSize);
end;

function TIntegerMatrixHelper.Copy(Y1, Y2: Integer): TIntegerMatrix;
var
  RowSize, Y: Integer;
begin
  RowSize := Width * SizeOf(Integer);

  Result.SetSize(Width, Y2-Y1);
  for Y := 0 to Result.Height - 1 do
    Move(Self[Y1+Y, 0], Result[Y, 0], RowSize);
end;

function TIntegerMatrixHelper.GetValues(Indices: TPointArray): TIntegerArray;
var
  Count, I: Integer;
  W, H: Integer;
begin
  Result := Default(TIntegerArray);

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

procedure TIntegerMatrixHelper.SetValues(Indices: TPointArray; Values: TIntegerArray);
var
  I, W, H: Integer;
begin
  if (Length(Values) <> Length(Indices)) then
    raise Exception.Create('SetValues: Length(Indices) <> Length(Values)');

  if Self.GetSize(W, H) then
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and (Indices[I].X < W) and (Indices[I].Y < H) then
        Self[Indices[I].Y, Indices[I].X] := Values[I];
end;

procedure TIntegerMatrixHelper.SetValue(Indices: TPointArray; Value: Integer);
var
  I, W, H: Integer;
begin
  if Self.GetSize(W, H) then
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and (Indices[I].X < W) and (Indices[I].Y < H) then
        Self[Indices[I].Y, Indices[I].X] := Value;
end;

procedure TIntegerMatrixHelper.Fill(Box: TBox; Value: Integer);
var
  W, H, X, Y: Integer;
begin
  if not Self.GetSize(W, H) then
    Exit;

  Box.X1 := Math.Max(Box.X1, 0);
  Box.Y1 := Math.Max(Box.Y1, 0);
  Box.X2 := Math.Min(Box.X2, W - 1);
  Box.Y2 := Math.Min(Box.Y2, H - 1);

  for Y := Box.Y1 to Box.Y2 do
    for X := Box.X1 to Box.X2 do
      Self[Y, X] := Value;
end;

procedure TIntegerMatrixHelper.Fill(Value: Integer);
var
  W, H, X, Y: Integer;
begin
  if Self.GetSize(W, H) then
  begin
    for X := 0 to W - 1 do Self[0, X] := Value;
    for Y := 1 to H - 1 do Move(Self[0, 0], Self[Y, 0], W * SizeOf(Integer));
  end;
end;

function TIntegerMatrixHelper.Flatten: TIntegerArray;
var
  Y: Integer;
begin
  SetLength(Result, Self.Area);
  for Y := 0 to Self.Height - 1 do
    Move(Self[Y, 0], Result[Y * Width], Width * SizeOf(Integer));
end;

function TIntegerMatrixHelper.Indices(Value: Integer; Comparator: EComparator): TPointArray;
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

function TByteMatrixHelper.GetWidth: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TByteMatrixHelper.GetHeight: Integer;
begin
  Result := Length(Self);
end;

function TByteMatrixHelper.GetArea: Integer;
begin
  Result := Width * Height;
end;

function TByteMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TByteMatrixHelper.SetSize(NewWidth, NewHeight: Integer);
begin
  SetLength(Self, NewHeight, NewWidth);
end;

function TBooleanMatrixHelper.GetWidth: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TBooleanMatrixHelper.GetHeight: Integer;
begin
  Result := Length(Self);
end;

function TBooleanMatrixHelper.GetArea: Integer;
begin
  Result := Width * Height;
end;

function TBooleanMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TBooleanMatrixHelper.SetSize(NewWidth, NewHeight: Integer);
begin
  SetLength(Self, NewHeight, NewWidth);
end;

function TSingleMatrixHelper.GetMin: Single;
var
  MaxValue: Single;
begin
  MinMax(Result, MaxValue);
end;

function TSingleMatrixHelper.GetMax: Single;
var
  MinValue: Single;
begin
  MinMax(MinValue, Result);
end;

function TSingleMatrixHelper.GetArgMax: TPoint;
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

function TSingleMatrixHelper.GetArgMin: TPoint;
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

function TSingleMatrixHelper.GetMean: Single;
begin
  Result := Self.Sum / Self.Area;
end;

function TSingleMatrixHelper.GetSum: Double;
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

function TSingleMatrixHelper.GetWidth: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TSingleMatrixHelper.GetHeight: Integer;
begin
  Result := Length(Self);
end;

function TSingleMatrixHelper.GetArea: Integer;
begin
  Result := Width * Height;
end;

function TSingleMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TSingleMatrixHelper.SetSize(NewWidth, NewHeight: Integer);
begin
  SetLength(Self, NewHeight, NewWidth);
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
  W, H, X, Y: Integer;
begin
  if not Self.GetSize(W, H) then
    Exit;

  Box.X1 := Math.Max(Box.X1, 0);
  Box.Y1 := Math.Max(Box.Y1, 0);
  Box.X2 := Math.Min(Box.X2, W - 1);
  Box.Y2 := Math.Min(Box.Y2, H - 1);

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

procedure TSingleMatrixHelper.MeanStdev(out MeanValue, Stdev: Double);
var
  W, H, X, Y: Integer;
  Value: Single;
begin
  MeanValue := 0;
  Stdev := 0;

  if Self.GetSizeMinusOne(W, H) then
  begin
    MeanValue := Self.Mean;

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if IsNumber(Value) then
          Stdev += Sqr(Value - MeanValue);
      end;

    Stdev := Sqrt(Stdev / Self.Area);
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
  curRow: PSingle;
begin
  if Self.GetSizeMinusOne(W, H) then
    for Y := 0 to H do
    begin
      curRow := @Self[Y][0];
      for X := 0 to W do
        if not IsNumber(curRow[X]) then
          curRow[X] := ReplaceWith;
    end;
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

function TSingleMatrixHelper.ArgExtrema(Count: Int32; HiLo: Boolean = True; XYIntersection: Boolean = True): TPointArray;
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
        if (X < W) then Buffer.Add(X-1,Y);
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
        if (Y < H) then Buffer.Add(X,Y-1);
        while (Y < H) and (Self[Y,X] <= Self[Y-1,X]) do Inc(Y);
      end;
    end;

    Result := Buffer.ToArray();
  end;

var
  I: Integer;
  Weights: TSingleArray;
begin
  W := Self.Width;
  H := Self.Height;

  Buffer.Init(Math.Max(2, Ceil(Sqrt(W * H))));

  if XYIntersection then
  begin
    Result := pass_x().Intersection(pass_y());
  end else begin
    Result := pass_x();
    Result += pass_y();
    Result := Result.Unique();
  end;

  // just use sort, since there arn't that many peaks
  SetLength(Weights, Length(Result));
  for I := 0 to High(Result) do
    Weights[I] := Self[Result[I].Y, Result[I].X];

  Result := Result.Sort(Weights, not HiLo);
  if (Length(Result) > Count) then
    SetLength(Result, Count);
end;

function TDoubleMatrixHelper.GetWidth: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TDoubleMatrixHelper.GetHeight: Integer;
begin
  Result := Length(Self);
end;

function TDoubleMatrixHelper.GetArea: Integer;
begin
  Result := Width * Height;
end;

function TDoubleMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TDoubleMatrixHelper.SetSize(NewWidth, NewHeight: Integer);
begin
  SetLength(Self, NewHeight, NewWidth);
end;

end.

