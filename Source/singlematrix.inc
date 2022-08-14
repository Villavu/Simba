{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  TSingleMatrixHelper = type Helper for TSingleMatrix
  public
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
    function Copy: TSingleMatrix;
    function GetValues(Indices: TPointArray): TSingleArray;
    procedure SetValues(Indices: TPointArray;  Values: TSingleArray);
    procedure SetValue(Indices: TPointArray;  Value: Single);
    procedure Fill(Box: TBox;  Value: Single); overload;
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
  end;
{$ENDIF}

{$IFDEF BODY}
function TSingleMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TSingleMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TSingleMatrixHelper.Area: Integer;
begin
  Result := Width * Height;
end;

function TSingleMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TSingleMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

function TSingleMatrixHelper.Copy: TSingleMatrix;
var
  Y: Integer;
begin
  SetLength(Result, Self.Height);
  for Y := 0 to High(Result) do
    Result[Y] := System.Copy(Self[Y]);
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
  Count, I: Integer;
  W, H: Integer;
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
  Count, I: Integer;
  W, H: Integer;
begin
  if Self.GetSize(W, H) then
    for I := 0 to High(Indices) do
      if (Indices[I].X >= 0) and (Indices[I].Y >= 0) and (Indices[I].X < W)  and (Indices[I].Y < H) then
        Self[Indices[I].Y, Indices[I].X] := Value;
end;

procedure TSingleMatrixHelper.Fill(Box: TBox; Value: Single);
var
  X, Y: Integer;
begin
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
  Result.SetSize(Self.Width, Self.Height);

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

  if Self.GetSize(W, H) then
  begin
    MeanValue := Self.Mean();

    Dec(W);
    Dec(H);
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
begin
  MinValue := 0;
  MaxValue := 0;

  if Self.GetSize(W, H) then
  begin
    MinValue := Single.MinValue;
    MaxValue := Single.MaxValue;

    Dec(W);
    Dec(H);

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];

        if IsNumber(Value) then
        begin
          if (Value > MaxValue) then
            MaxValue := Value;
          if (Value < MinValue) then
            MinValue := Value;
        end;
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
begin
  Result := TPoint.Create(0, 0);

  if Self.GetSize(W, H) then
  begin
    Best := Self[Result.Y, Result.X];

    Dec(W);
    Dec(H);

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];

        if IsNumber(Value) and (Value > Best) then
        begin
          Result.X := X;
          Result.Y := Y;

          Best := Self[Result.Y, Result.X];
        end;
      end;
  end;
end;

function TSingleMatrixHelper.ArgMin: TPoint;
var
  X, Y, W, H: Integer;
  Value, Best: Single;
begin
  Result := TPoint.Create(0, 0);

  if Self.GetSize(W, H) then
  begin
    Best := Self[Result.Y, Result.X];

    Dec(W);
    Dec(H);

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];

        if IsNumber(Value) and (Value < Best) then
        begin
          Result.X := X;
          Result.Y := Y;

          Best := Self[Result.Y, Result.X];
        end;
      end;
  end;
end;

function TSingleMatrixHelper.NormMinMax(Alpha, Beta: Single): TSingleMatrix;
var
  Lo, Hi, OldRange, NewRange: Single;
  X, Y, W, H: Integer;
begin
  Result := Default(TSingleMatrix);

  if Self.GetSize(W, H) then
  begin
    Result.SetSize(W, H);

    Self.MinMax(Lo, Hi);

    OldRange := Hi - Lo;
    NewRange := Beta - Alpha;
    if IsZero(OldRange) then
      Exit;

    for Y := 0 to Height do
      for X := 0 to Width do
        if IsNumber(Self[Y, X]) then
          Result[Y, X] := (Self[Y, X] - Lo) / OldRange * NewRange + Alpha;
  end;
end;

function TSingleMatrixHelper.Indices(Value: Single; Comparator: EComparator): TPointArray;
var
  W, H, X, Y: Integer;
  Buffer: specialize TSimbaOverAllocateArray<TPoint>;
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

  Result := Buffer.Trim();
end;

function TSingleMatrixHelper.ArgMulti(Count: Integer; HiLo: Boolean): TPointArray;
var
  W, H, I, Y, X: Integer;
  HeapArray: TSimbaHeapArrayF;
begin
  Result := Default(TPointArray);

  if Self.GetSize(W, H) then
  begin
    W := W - 1;
    H := H - 1;

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

  Data := Self.Copy();
  W := Self.Width - 1;
  H := Self.Height - 1;
  Radius := (block div 2);

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

function TSingleMatrixHelper.Sum: Double;
var
  X, Y, W, H: Integer;
  Value: Single;
begin
  Result := 0;

  if Self.GetSize(W, H) then
  begin
    Dec(W);
    Dec(H);

    for Y := 0 to H do
      for X := 0 to W do
      begin
        Value := Self[Y, X];
        if IsNumber(Value) then
          Result := Result + Value;
      end;
  end;
end;
{$ENDIF}
