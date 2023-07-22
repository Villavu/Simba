{%MAINUNIT simba.mufasatypes}

{$IFDEF HEADER}
type
  TBoxArrayHelper = type helper for TBoxArray
  public
    class function Create(Start: TPoint; Columns, Rows, Width, Height: Int32; Spacing: TPoint): TBoxArray; static;

    function Pack: TBoxArray;

    function Sort(Weights: TIntegerArray; LowToHigh: Boolean = True): TBoxArray; overload;
    function Sort(Weights: TDoubleArray; LowToHigh: Boolean = True): TBoxArray; overload;
    function SortFrom(From: TPoint): TBoxArray;
    function SortByX(LowToHigh: Boolean): TBoxArray;
    function SortByY(LowToHigh: Boolean): TBoxArray;
    function SortByWidth(LowToHigh: Boolean): TBoxArray;
    function SortByHeight(LowToHigh: Boolean): TBoxArray;
    function SortByArea(LowToHigh: Boolean): TBoxArray;

    function ContainsPoint(P: TPoint; out Index: Integer): Boolean; overload;
    function ContainsPoint(P: TPoint): Boolean; overload;

    function Merge: TBox;
    function Centers: TPointArray;
    function Offset(P: TPoint): TBoxArray; overload;
    function Offset(X, Y: Integer): TBoxArray; overload;

    function Expand(SizeMod: Integer): TBoxArray; overload;
    function Expand(WidMod, HeiMod: Integer): TBoxArray; overload;
  end;
{$ENDIF}

{$IFDEF BODY}
class function TBoxArrayHelper.Create(Start: TPoint; Columns, Rows, Width, Height: Int32; Spacing: TPoint): TBoxArray;
var
  I: Int32;
begin
  Start.X += (Width div 2);
  Start.Y += (Height div 2);

  Spacing.X += Width;
  Spacing.Y += Height;

  SetLength(Result, Columns * Rows);

  for I := 0 to High(Result) do
  begin
    Result[I].X1 := Start.X + I mod Columns * Spacing.X - Width div 2;
    Result[I].Y1 := Start.Y + I div Columns * Spacing.Y - Height div 2;
    Result[I].X2 := Result[I].X1 + Width;
    Result[I].Y2 := Result[I].Y1 + Height;
  end;
end;

function TBoxArrayHelper.SortFrom(From: TPoint): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Distance(From, Self[I].Center);

  Result := Self.Sort(Weights);
end;

function TBoxArrayHelper.SortByX(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Center.X;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByY(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Center.Y;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByWidth(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Width;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByHeight(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Height;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.SortByArea(LowToHigh: Boolean): TBoxArray;
var
  Weights: TIntegerArray;
  I: Integer;
begin
  SetLength(Weights, Length(Self));
  for I := 0 to High(Weights) do
    Weights[I] := Self[I].Area;

  Result := Self.Sort(Weights, LowToHigh);
end;

function TBoxArrayHelper.ContainsPoint(P: TPoint; out Index: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Self) do
    if Self[I].Contains(P) then
    begin
      Index := I;

      Exit(True);
    end;

  Result := False;
end;

function TBoxArrayHelper.ContainsPoint(P: TPoint): Boolean;
var
  Index: Integer;
begin
  Result := ContainsPoint(P, Index);
end;

function TBoxArrayHelper.Merge: TBox;
var
  I: Integer;
begin
  if Length(Self) = 0 then
    Exit(TBox.Default());

  Result := Self[0];
  for I := 1 to High(Self) do
    Result := Result.Combine(Self[I]);
end;

function TBoxArrayHelper.Centers: TPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Center;
end;

function TBoxArrayHelper.Offset(P: TPoint): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Offset(P);
end;

function TBoxArrayHelper.Offset(X, Y: Integer): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Offset(X, Y);
end;

function TBoxArrayHelper.Expand(SizeMod: Integer): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Expand(SizeMod);
end;

function TBoxArrayHelper.Expand(WidMod, HeiMod: Integer): TBoxArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Self));
  for I := 0 to High(Result) do
    Result[I] := Self[I].Expand(WidMod, HeiMod);
end;

// https://github.com/mapbox/potpack
function TBoxArrayHelper.Pack: TBoxArray;
type
  TBlock = record X,Y,W,H: Integer; Index: Integer; end;
  TBlockArray = array of TBlock;

  function Block(X,Y,W,H: Integer; Index: Integer = -1): TBlock;
  begin
    Result.X := X;
    Result.Y := Y;
    Result.W := W;
    Result.H := H;
    Result.Index := Index;
  end;

var
  weights: TIntegerArray;
  I, J: Integer;
  StartWidth, Area, MaxWidth: Integer;
  Width, Height: Integer;
  Blocks, Spaces: TBlockArray;
  Len: Integer;
begin
  Len := Length(Self);
  if (Len = 0) then
    Exit(Default(TBoxArray));

  Area := 0;
  MaxWidth := 0;

  SetLength(Result, Len);
  SetLength(Blocks, Len);
  SetLength(Weights, Len);

  for I := 0 to Len - 1 do
  begin
    Blocks[I] := Block(0, 0, Self[I].Width - 1, Self[I].Height - 1, I);
    Weights[I] := Blocks[I].H;

    Area += Blocks[I].W * Blocks[I].H;
    MaxWidth := Max(MaxWidth, Blocks[I].W);
  end;

  specialize QuickSortWeighted<TBlock, Integer>(Blocks, Weights, Low(Blocks), High(Blocks), False);

  StartWidth := Max(Ceil(Sqrt(Area / 0.95)), MaxWidth);
  Spaces := [Block(0, 0, StartWidth, $FFFFFF)];

  Width := 0;
  Height := 0;

  for I := 0 to Len - 1 do
    for J := High(Spaces) downto 0 do
    begin
      if (Blocks[I].W > Spaces[J].W) or (Blocks[I].H > Spaces[J].H) then
        Continue;

      Blocks[I].X := Spaces[J].X;
      Blocks[I].Y := Spaces[J].Y;

      Width  := Max(Width, Blocks[I].X + Blocks[I].W);
      Height := Max(Height, Blocks[I].Y + Blocks[I].H);

      if (Blocks[I].W = Spaces[J].W) and (Blocks[I].H = Spaces[J].H) then
        Delete(Spaces, J, 1)
      else
      if (Blocks[I].H = Spaces[J].H) then
      begin
        Spaces[J].X += Blocks[I].W;
        Spaces[J].W -= Blocks[I].W;
      end else
      if (Blocks[I].W = Spaces[J].W) then
      begin
        Spaces[J].Y += Blocks[I].H;
        Spaces[J].H -= Blocks[I].H;
      end else
      begin
        Spaces += [Block(
          Spaces[J].X + Blocks[I].W,
          Spaces[J].Y,
          Spaces[J].W - Blocks[I].W,
          Blocks[I].H
        )];
        Spaces[J].Y += Blocks[I].H;
        Spaces[J].H -= Blocks[I].H;
      end;

      Break;
    end;

  for I := 0 to Len - 1 do
    Result[Blocks[I].Index] := TBox.Create(Blocks[I].X, Blocks[I].Y, Blocks[I].X + Blocks[I].W, Blocks[I].Y + Blocks[I].H);
end;

function TBoxArrayHelper.Sort(Weights: TIntegerArray; LowToHigh: Boolean): TBoxArray;
begin
  Result := specialize Sorted<TBox, Integer>(Self, Weights, LowToHigh);
end;

function TBoxArrayHelper.Sort(Weights: TDoubleArray; LowToHigh: Boolean): TBoxArray;
begin
  Result := specialize Sorted<TBox, Double>(Self, Weights, LowToHigh);
end;
{$ENDIF}


