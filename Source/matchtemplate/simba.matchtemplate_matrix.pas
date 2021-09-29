{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
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

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.type_matrix;

function Area(a: TIntegerMatrix): Int32;
function Area(a: TSingleMatrix): Int32;
function Area(a: TDoubleMatrix): Int32;
function Area(a: TComplexMatrix): Int32;

function SumsPd(a: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;
function SumsPd(a: TDoubleMatrix; out Square: TDoubleMatrix): TDoubleMatrix;

function Rot90(a: TIntegerMatrix): TIntegerMatrix;
function Rot90(a: TSingleMatrix): TSingleMatrix;
function Rot90(a: TDoubleMatrix): TDoubleMatrix;
function Rot90(a: TComplexMatrix): TComplexMatrix;

function Mean(a: TIntegerMatrix): Double;
function Mean(a: TSingleMatrix): Double;
function Mean(a: TDoubleMatrix): Double;

procedure MeanStdev(a: TIntegerMatrix; out Mean, Stdev: Double);
procedure MeanStdev(a: TSingleMatrix; out Mean, Stdev: Double);
procedure MeanStdev(a: TDoubleMatrix; out Mean, Stdev: Double);

procedure SplitRGB(Image: TIntegerMatrix; out R,G,B: TSingleMatrix);

type
  TRGBMatrix = record
    R, G, B: TSingleMatrix;

    function Width: Int32;
    function Height: Int32;
  end;

function Sum(Matrix: TRGBMatrix): TDoubleArray;
function Sqrt(Matrix: TRGBMatrix): TSingleMatrix; overload;
function Norm(Matrix: TRGBMatrix): Double;

operator *(Left: TRGBMatrix; Right: TRGBMatrix): TRGBMatrix;
operator -(Left: TRGBMatrix; Right: TRGBMatrix): TRGBMatrix;
operator +(Left: TRGBMatrix; Right: TRGBMatrix): TRGBMatrix;
operator *(Left: TRGBMatrix; Right: TDoubleArray): TRGBMatrix;
operator -(Left: TRGBMatrix; Right: TDoubleArray): TRGBMatrix;

function Sum(Matrix: TSingleMatrix): Double;
function Sqrt(Matrix: TSingleMatrix): TSingleMatrix; overload;
function Norm(Matrix: TSingleMatrix): Double;

operator *(Left: TSingleMatrix; Right: Double): TSingleMatrix;
operator -(Left: TSingleMatrix; Right: Double): TSingleMatrix;
operator -(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
operator *(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
//operator +(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
operator /(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
operator /(Left: TSingleMatrix; Right: Double): TSingleMatrix;

implementation

procedure SplitRGB(Image: TIntegerMatrix; out R,G,B: TSingleMatrix);
var
  W,H,x,y: Int32;
begin
  W := Image.Width;
  H := Image.Height;

  SetLength(R, H,W);
  SetLength(G, H,W);
  SetLength(B, H,W);

  Dec(W);
  Dec(H);

  for y:=0 to H do
    for x:=0 to W do begin
      R[y,x] := Image[y,x]{shr 00}and $FF;
      G[y,x] := Image[y,x] shr 08 and $FF;
      B[y,x] := Image[y,x] shr 16 and $FF;
    end;
end;

{$DEFINE MACRO_AREA :=
  begin
    Result := A.Height * A.Width;
  end;
}

function Area(a: TIntegerMatrix): Int32; MACRO_AREA
function Area(a: TSingleMatrix): Int32;  MACRO_AREA
function Area(a: TDoubleMatrix): Int32;  MACRO_AREA
function Area(a: TComplexMatrix): Int32; MACRO_AREA

{$DEFINE MACRO_SUMSPD :=
var
  x,y,W,H: Int32;
  sum,sqsum: Double;
begin
  H := Length(a);
  W := Length(a[0]);
  SetLength(Result, H+1,W+1);
  SetLength(Square, H+1,W+1);

  Result[1,1] := a[0,0];
  Square[1,1] := Sqr(a[0,0]);
  for y:=2 to H do
  begin
    Result[y,1] := Result[y-1,1] + a[y-1,0];
    Square[y,1] := Square[y-1,1] + Sqr(a[y-1,0]);
  end;

  for x:=2 to W do
  begin
    Result[1,x] := Result[1,x-1] + a[0,x-1];
    Square[1,x] := Square[1,x-1] + Sqr(a[0,x-1]);
  end;

  for y:=2 to H do
  begin
    sum   := a[y-1,0];
    sqsum := Sqr(sum);
    for x:=2 to W do
    begin
      sum += a[y-1,x-1];
      Result[y,x] := Result[y-1,x] + sum;
      sqsum += Sqr(a[y-1,x-1]);
      Square[y,x] := Square[y-1,x] + sqsum;
    end;
  end;
end;
}

function SumsPd(a: TSingleMatrix; out Square: TDoubleMatrix): TDoubleMatrix; MACRO_SUMSPD
function SumsPd(a: TDoubleMatrix; out Square: TDoubleMatrix): TDoubleMatrix; MACRO_SUMSPD

{$DEFINE MACRO_ROT90 :=
  var W,H,i,j:Int32;
  begin
    W := A.Width;
    H := A.Height;
    SetLength(Result, W,H);
    for i:=0 to H-1 do
      for j:=0 to W-1 do Result[j,i] := a[i,j];
  end;
}

function Rot90(a: TIntegerMatrix): TIntegerMatrix; MACRO_ROT90
function Rot90(a: TSingleMatrix): TSingleMatrix;   MACRO_ROT90
function Rot90(a: TDoubleMatrix): TDoubleMatrix;   MACRO_ROT90
function Rot90(a: TComplexMatrix): TComplexMatrix; MACRO_ROT90

{$DEFINE MACRO_MEAN :=
  var
    W,H,i,j:Int32;
    sum: Double;
  begin
    W := A.Width;
    H := A.Height;
    sum := 0;
    for i:=0 to h-1 do
      for j:=0 to w-1 do sum += a[i,j];
    Result := sum / (W*H);
  end;
}

function Mean(a: TIntegerMatrix): Double; MACRO_MEAN
function Mean(a: TSingleMatrix): Double;  MACRO_MEAN
function Mean(a: TDoubleMatrix): Double;  MACRO_MEAN

{$DEFINE MACRO_MEANSTDEV :=
  var
    W,H,i,j:Int32;
    sum, square: Double;
  begin
    W := A.Width;
    H := A.Height;
    sum := 0;
    for i:=0 to h-1 do
      for j:=0 to w-1 do sum += a[i,j];
    Mean := sum / (W*H);

    square := 0;
    for i:=0 to h-1 do
      for j:=0 to w-1 do square += Sqr(a[i,j] - Mean);
    Stdev := Sqrt(square / (W*H));
  end;
}

procedure MeanStdev(a: TIntegerMatrix; out Mean, Stdev: Double); MACRO_MEANSTDEV
procedure MeanStdev(a: TSingleMatrix; out Mean, Stdev: Double);  MACRO_MEANSTDEV
procedure MeanStdev(a: TDoubleMatrix; out Mean, Stdev: Double);  MACRO_MEANSTDEV

function Sum(Matrix: TRGBMatrix): TDoubleArray;
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

function Sqrt(Matrix: TRGBMatrix): TSingleMatrix;
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

function Norm(Matrix: TRGBMatrix): Double;
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

operator*(Left: TRGBMatrix; Right: TRGBMatrix): TRGBMatrix;
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

operator-(Left: TRGBMatrix; Right: TRGBMatrix): TRGBMatrix;
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

operator+(Left: TRGBMatrix; Right: TRGBMatrix): TRGBMatrix;
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

operator*(Left: TRGBMatrix; Right: TDoubleArray): TRGBMatrix;
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

operator-(Left: TRGBMatrix; Right: TDoubleArray): TRGBMatrix;
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

function Sum(Matrix: TSingleMatrix): Double;
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

function Sqrt(Matrix: TSingleMatrix): TSingleMatrix;
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

function Norm(Matrix: TSingleMatrix): Double;
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

operator*(Left: TSingleMatrix; Right: Double): TSingleMatrix;
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

operator*(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
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

operator-(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
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

operator-(Left: TSingleMatrix; Right: Double): TSingleMatrix;
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

{
operator+(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
var
  X, Y, W, H: Int32;
begin
  W := Left.Width - 1;
  H := Left.Height - 1;

  SetLength(Result, H+1, W+1);

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Left[Y, X] + Right[Y, X];
end; }

operator/(Left: TSingleMatrix; Right: TSingleMatrix): TSingleMatrix;
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

operator/(Left: TSingleMatrix; Right: Double): TSingleMatrix;
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
