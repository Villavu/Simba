{==============================================================================]
  Copyright © 2018, Jarl Krister Holta
  
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
// ----------------------------------------------------------------------------
// Sum, pad zeroes first col and row

function SumsPd(a: T2DIntArray; out Square: T2DI64Array): T2DIntArray;
var
  x,y,W,H: Int32;
  sum,sqsum: Int64;
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

function SumsPd(a: T2DSingleArray; out Square: T2DDoubleArray): T2DDoubleArray;
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

function SumsPd(a: T2DDoubleArray; out Square: T2DDoubleArray): T2DDoubleArray;
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

