unit simba.matchtemplate_matrix;
{==============================================================================]
  Copyright © 2021, Jarl Krister Holta
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
[==============================================================================}
{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

type
  TComplex = record
    Re, Im: Single;
  end;
  TComplexArray  = array of TComplex;
  TComplexMatrix = array of TComplexArray;
  TComplexMatrixHelper = type helper for TComplexMatrix
  public
    procedure SetSize(AWidth, AHeight: Integer);
    function Width: Integer;
    function Height: Integer;
  end;

  TRGBComplexMatrix = record
    Width: Integer;
    Height: Integer;

    R, G, B: TComplexMatrix;
  end;

  TRGBMatrix = record
    R, G, B: TSingleMatrix;

    procedure SetSize(AWidth, AHeight: Integer);
    function Copy(Y1, Y2: Integer): TRGBMatrix;
    function Merge: TSingleMatrix;
    function Width: Integer;
    function Height: Integer;

    class function Create(const Image: TIntegerMatrix): TRGBMatrix; static;
  end;

operator *(const Left: Double; const Mat: TSingleMatrix): TSingleMatrix;
operator *(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
operator *(const Left: TRGBMatrix; const Right: Single): TRGBMatrix;
operator -(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
operator -(const Left: TRGBMatrix; const Right: TDoubleArray): TRGBMatrix;
operator +(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
operator +(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
operator +(const Left: TSingleMatrix; const Right: Single): TSingleMatrix;
operator -(const Left: TSingleMatrix; const Right: Single): TSingleMatrix;
operator -(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
operator *(const Left: TSingleMatrix; const Right: Single): TSingleMatrix;
operator *(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
operator /(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;

function SumOfSquares(const Matrix: TRGBMatrix): Double;
function Sqrt(const Matrix: TRGBMatrix): TSingleMatrix; overload;
function Sum(const Matrix: TSingleMatrix): Single; overload;
function Sum(const Matrix: TRGBMatrix): TDoubleArray; overload;
function Norm(const Matrix: TRGBMatrix): Double;
function SumsPd(const Matrix: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
function Rot90(const Matrix: TComplexMatrix): TComplexMatrix;

implementation

function SumsPd(const Matrix: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
var
  x,y,W,H: Integer;
  sum,sqsum: Double;
begin
  H := Length(Matrix);
  W := Length(Matrix[0]);
  SetLength(Result, H+1,W+1);
  SetLength(Square, H+1,W+1);

  Result[1,1] := Matrix[0,0];
  Square[1,1] := Sqr(Matrix[0,0]);
  for y:=2 to H do
  begin
    Result[y,1] := Result[y-1,1] + Matrix[y-1,0];
    Square[y,1] := Square[y-1,1] + Sqr(Matrix[y-1,0]);
  end;

  for x:=2 to W do
  begin
    Result[1,x] := Result[1,x-1] + Matrix[0,x-1];
    Square[1,x] := Square[1,x-1] + Sqr(Matrix[0,x-1]);
  end;

  for y:=2 to H do
  begin
    sum   := Matrix[y-1,0];
    sqsum := Sqr(sum);
    for x:=2 to W do
    begin
      sum += Matrix[y-1,x-1];
      Result[y,x] := Result[y-1,x] + sum;
      sqsum += Sqr(Matrix[y-1,x-1]);
      Square[y,x] := Square[y-1,x] + sqsum;
    end;
  end;
end;

function Rot90(const Matrix: TComplexMatrix): TComplexMatrix;
var
  Width, Height, X, Y: Integer;
begin
  SetLength(Result, Matrix.Width, Matrix.Height);

  Width := Matrix.Width - 1;
  Height := Matrix.Height - 1;

  for Y := 0 to Height do
    for X := 0 to Width do
      Result[X, Y] := Matrix[Y, X];
end;

function Sum(const Matrix: TRGBMatrix): TDoubleArray;
var
  X, Y, W, H: Integer;
  R, G, B: Double;
begin
  R := 0;
  G := 0;
  B := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      R += Matrix.R[Y, X];
      G += Matrix.G[Y, X];
      B += Matrix.B[Y, X];
    end;

  Result := [R, G, B];
end;

function SumOfSquares(const Matrix: TRGBMatrix): Double;
var
  X, Y, W, H: Integer;
begin
  Result := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result += Sqr(Matrix.R[Y, X]) + Sqr(Matrix.G[Y, X]) + Sqr(Matrix.B[Y, X]);
end;

function Sqrt(const Matrix: TRGBMatrix): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result, Matrix.Height, Matrix.Width);

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
     Result[Y, X] := Sqrt(Matrix.R[Y, X] + Matrix.G[Y, X] +  Matrix.B[Y, X]);
end;

operator*(const Left: Double; const Mat: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result, Mat.Height, Mat.Width);

  W := Mat.Width - 1;
  H := Mat.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Mat[Y, X] * Left;
end;

operator*(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result.R, Left.Height, Left.Width);
  SetLength(Result.G, Left.Height, Left.Width);
  SetLength(Result.B, Left.Height, Left.Width);

  W := Left.Width - 1;
  H := Left.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Left.R[Y, X] * Right.R[Y, X];
      Result.G[Y, X] := Left.G[Y, X] * Right.G[Y, X];
      Result.B[Y, X] := Left.B[Y, X] * Right.B[Y, X];
    end;
end;

operator*(const Left: TRGBMatrix; const Right: Single): TRGBMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result.R, Left.Height, Left.Width);
  SetLength(Result.G, Left.Height, Left.Width);
  SetLength(Result.B, Left.Height, Left.Width);

  W := Left.Width - 1;
  H := Left.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Left.R[Y, X] * Right;
      Result.G[Y, X] := Left.G[Y, X] * Right;
      Result.B[Y, X] := Left.B[Y, X] * Right;
    end;
end;

operator-(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result.R, Left.Height, Left.Width);
  SetLength(Result.G, Left.Height, Left.Width);
  SetLength(Result.B, Left.Height, Left.Width);

  W := Left.Width - 1;
  H := Left.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Left.R[Y, X] - Right.R[Y, X];
      Result.G[Y, X] := Left.G[Y, X] - Right.G[Y, X];
      Result.B[Y, X] := Left.B[Y, X] - Right.B[Y, X];
    end;
end;

operator+(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result.R, Left.Height, Left.Width);
  SetLength(Result.G, Left.Height, Left.Width);
  SetLength(Result.B, Left.Height, Left.Width);

  W := Left.Width - 1;
  H := Left.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Left.R[Y, X] + Right.R[Y, X];
      Result.G[Y, X] := Left.G[Y, X] + Right.G[Y, X];
      Result.B[Y, X] := Left.B[Y, X] + Right.B[Y, X];
    end;
end;

operator-(const Left: TRGBMatrix; const Right: TDoubleArray): TRGBMatrix;
var
  X, Y, W, H: Integer;
begin
  SetLength(Result.R, Left.Height, Left.Width);
  SetLength(Result.G, Left.Height, Left.Width);
  SetLength(Result.B, Left.Height, Left.Width);

  W := Left.Width - 1;
  H := Left.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Left.R[Y, X] - Right[0];
      Result.G[Y, X] := Left.G[Y, X] - Right[1];
      Result.B[Y, X] := Left.B[Y, X] - Right[2];
    end;
end;

function Sum(const Matrix: TSingleMatrix): Single;
var
  W, H, X, Y: Integer;
begin
  Result := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result += Matrix[Y, X];
end;

function Norm(const Matrix: TRGBMatrix): Double;
var
  X, Y, W, H: Integer;
begin
  Result := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result += Sqr(Matrix.R[Y, X]) + Sqr(Matrix.G[Y, X]) + Sqr(Matrix.B[Y, X]);

  Result := Sqrt(Result);
end;

operator+(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] + Right[Y, X];
end;

operator*(const Left: TSingleMatrix; const Right: Single): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] * Right;
end;

operator*(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
var
  W, H, X, Y: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] * Right[Y, X];
end;

operator-(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] - Right[Y, X];
end;

operator+(const Left: TSingleMatrix; const Right: Single): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] + Right;
end;

operator-(const Left: TSingleMatrix; const Right: Single): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] - Right;
end;

operator/(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] / Right[Y, X];
end;

procedure TComplexMatrixHelper.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self, AHeight, AWidth);
end;

function TComplexMatrixHelper.Width: Integer;
begin
  if (Length(Self) > 0) then
    Result := Length(Self[0])
  else
    Result := 0;
end;

function TComplexMatrixHelper.Height: Integer;
begin
  Result := Length(Self);
end;

procedure TRGBMatrix.SetSize(AWidth, AHeight: Integer);
begin
  SetLength(Self.R, AHeight, AWidth);
  SetLength(Self.G, AHeight, AWidth);
  SetLength(Self.B, AHeight, AWidth);
end;

function TRGBMatrix.Copy(Y1, Y2: Integer): TRGBMatrix;
var
  RowSize, Y: Integer;
begin
  RowSize := Width * SizeOf(Single);

  Result.SetSize(Width, Y2-Y1);
  for Y := 0 to Result.Height - 1 do
  begin
    Move(Self.R[Y1+Y, 0], Result.R[Y, 0], RowSize);
    Move(Self.G[Y1+Y, 0], Result.G[Y, 0], RowSize);
    Move(Self.B[Y1+Y, 0], Result.B[Y, 0], RowSize);
  end;
end;

function TRGBMatrix.Merge: TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  W := Self.Width;
  H := Self.Height;
  Result.SetSize(W, H);
  Dec(W);
  Dec(H);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Self.R[Y, X] + Self.G[Y, X] + Self.B[Y, X];
end;

function TRGBMatrix.Width: Integer;
begin
  if Length(Self.R) > 0 then
    Result := Length(Self.R[0])
  else
    Result := 0;
end;

function TRGBMatrix.Height: Integer;
begin
  Result := Length(Self.R);
end;

class function TRGBMatrix.Create(const Image: TIntegerMatrix): TRGBMatrix;
var
  W, H, X, Y: Integer;
begin
  W := Image.Width;
  H := Image.Height;

  SetLength(Result.R, H, W);
  SetLength(Result.G, H, W);
  SetLength(Result.B, H, W);

  Dec(W);
  Dec(H);
  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Image[Y, X]        and $FF;
      Result.G[Y, X] := Image[Y, X] shr 08 and $FF;
      Result.B[Y, X] := Image[Y, X] shr 16 and $FF;
    end;
end;

end.
