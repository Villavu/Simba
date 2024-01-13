{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matrix_int;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TIntegerMatrixHelper = type helper for TIntegerMatrix
  public
    function Width: Integer;
    function Height: Integer;
    function Area: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
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
  public
    function Width: Integer;
    function Height: Integer;
    function GetSize(out AWidth, AHeight: Integer): Boolean;
    procedure SetSize(AWidth, AHeight: Integer);
  end;

implementation

uses
  simba.math, simba.arraybuffer;

function TIntegerMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TIntegerMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TIntegerMatrixHelper.Area: Integer;
begin
  Result := Width * Height;
end;

function TIntegerMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TIntegerMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
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
  X, Y: Integer;
begin
  Box.Clip(TBox.Create(0, 0, Width - 1, Height - 1));
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

function TByteMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TByteMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

function TByteMatrixHelper.GetSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := Width;
  AHeight := Height;

  Result := (AWidth > 0) and (AHeight > 0);
end;

procedure TByteMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

end.

