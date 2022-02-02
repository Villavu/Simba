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
{$DEFINE SIMBA_O4}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.matrixhelpers;

type
  TRGBMatrix = record
    R, G, B: TSingleMatrix;

    function Width: Int32;
    function Height: Int32;
  end;

operator *(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
operator *(const Left: TRGBMatrix; const Right: TDoubleArray): TRGBMatrix;
operator -(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
operator -(const Left: TRGBMatrix; const Right: TDoubleArray): TRGBMatrix;
operator +(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;

operator +(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
operator -(const Left: TSingleMatrix; const Right: Double): TSingleMatrix;
operator -(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
operator *(const Left: TSingleMatrix; const Right: Double): TSingleMatrix;
operator *(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
operator /(const Left: TSingleMatrix; const Right: Double): TSingleMatrix;
operator /(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;

function Sum(const Matrix: TRGBMatrix): TDoubleArray;
function Sqrt(const Matrix: TRGBMatrix): TSingleMatrix; overload;
function Norm(const Matrix: TRGBMatrix): Double;

function Sum(const Matrix: TSingleMatrix): Double;
function Sqrt(const Matrix: TSingleMatrix): TSingleMatrix; overload;
function Norm(const Matrix: TSingleMatrix): Double;

function SumsPd(const Matrix: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
function Rot90(const Matrix: TComplexMatrix): TComplexMatrix;

procedure SplitRGB(const Image: TIntegerMatrix; out R, G, B: TSingleMatrix);

implementation

procedure SplitRGB(const Image: TIntegerMatrix; out R, G, B: TSingleMatrix);
var
  W,H,x,y: Int32;
begin
  W := Image.Width;
  H := Image.Height;

  SetLength(R, H, W);
  SetLength(G, H, W);
  SetLength(B, H, W);

  Dec(W);
  Dec(H);

  for y:=0 to H do
    for x:=0 to W do
    begin
      R[y,x] := Image[y,x]{shr 00}and $FF;
      G[y,x] := Image[y,x] shr 08 and $FF;
      B[y,x] := Image[y,x] shr 16 and $FF;
    end;
end;

function SumsPd(const Matrix: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
var
  x,y,W,H: Int32;
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
  X, Y, W, H: Int32;
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

function Sqrt(const Matrix: TRGBMatrix): TSingleMatrix;
var
  X, Y, W, H: Int32;
begin
  SetLength(Result, Matrix.Height, Matrix.Width);

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Sqrt(Matrix.R[Y, X] + Matrix.G[Y, X] + Matrix.B[Y, X]);
end;

function Norm(const Matrix: TRGBMatrix): Double;
var
  X, Y, W, H: Int32;
begin
  Result := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result += Matrix.R[Y][X] * Matrix.R[Y][X];
      Result += Matrix.G[Y][X] * Matrix.G[Y][X];
      Result += Matrix.B[Y][X] * Matrix.B[Y][X];
    end;

  Result := Sqrt(Result);
end;

operator*(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
var
  X, Y, W, H: Int32;
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

operator-(const Left: TRGBMatrix; const Right: TRGBMatrix): TRGBMatrix;
var
  X, Y, W, H: Int32;
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
  X, Y, W, H: Int32;
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

operator*(const Left: TRGBMatrix; const Right: TDoubleArray): TRGBMatrix;
var
  X, Y, W, H: Int32;
begin
  SetLength(Result.R, Left.Height, Left.Width);
  SetLength(Result.G, Left.Height, Left.Width);
  SetLength(Result.B, Left.Height, Left.Width);

  W := Left.Width - 1;
  H := Left.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result.R[Y, X] := Left.R[Y, X] * Right[0];
      Result.G[Y, X] := Left.G[Y, X] * Right[1];
      Result.B[Y, X] := Left.B[Y, X] * Right[2];
    end;
end;

operator-(const Left: TRGBMatrix; const Right: TDoubleArray): TRGBMatrix;
var
  X, Y, W, H: Int32;
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

function Sum(const Matrix: TSingleMatrix): Double;
var
  W, H, X, Y: Int32;
begin
  Result := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result += Matrix[Y, X];
end;

function Sqrt(const Matrix: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Int32;
begin
  SetLength(Result, Matrix.Height, Matrix.Width);

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := System.Sqrt(Matrix[Y, X]);
end;

function Norm(const Matrix: TSingleMatrix): Double;
var
  W, H, X, Y: Int32;
begin
  Result := 0;

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
      Result += Matrix[Y][X] * Matrix[Y][X];

  Result := Sqrt(Result);
end;

operator+(const Left: TSingleMatrix; const Right: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Int32;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] + Right[Y, X];
end;

operator*(const Left: TSingleMatrix; const Right: Double): TSingleMatrix;
var
  X, Y, W, H: Int32;
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
  W, H, X, Y: Int32;
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
  X, Y, W, H: Int32;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] - Right[Y, X];
end;

operator-(const Left: TSingleMatrix; const Right: Double): TSingleMatrix;
var
  X, Y, W, H: Int32;
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
  X, Y, W, H: Int32;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] / Right[Y, X];
end;

operator/(const Left: TSingleMatrix; const Right: Double): TSingleMatrix;
var
  X, Y, W, H: Int32;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] / Right;
end;

function TRGBMatrix.Width: Int32;
begin
  if Length(Self.R) > 0 then
    Result := Length(Self.R[0])
  else
    Result := 0;
end;

function TRGBMatrix.Height: Int32;
begin
  Result := Length(Self.R);
end;

end.
