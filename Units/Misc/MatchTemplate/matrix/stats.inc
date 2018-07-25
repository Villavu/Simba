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
// Mean
function Mean(a: T2DIntArray): Double;
var
  W,H,i,j:Int32;
  sum: Double;
begin
  Size(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Result := sum / (W*H);
end; 

function Mean(a: T2DSingleArray): Double;
var
  W,H,i,j:Int32;
  sum: Double;
begin
  Size(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Result := sum / (W*H);
end; 

function Mean(a: T2DDoubleArray): Double;
var
  W,H,i,j:Int32;
  sum: Double;
begin
  Size(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Result := sum / (W*H);
end; 


// ----------------------------------------------------------------------------
// Mean and Stdev at once

procedure MeanStdev(a: T2DIntArray; out Mean, Stdev: Double);
var
  W,H,i,j:Int32;
  sum, square: Extended;
begin
  Size(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Mean := sum / (W*H);

  square := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do square += Sqr(a[i,j] - Mean);
  Stdev := Sqrt(square / (W*H));
end;

procedure MeanStdev(a: T2DSingleArray; out Mean, Stdev: Double);
var
  W,H,i,j:Int32;
  sum, square: Double;
begin
  Size(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Mean := sum / (W*H);

  square := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do square += Sqr(a[i,j] - Mean);
  Stdev := Sqrt(square / (W*H));
end;

procedure MeanStdev(a: T2DDoubleArray; out Mean, Stdev: Double);
var
  W,H,i,j:Int32;
  sum, square: Double;
begin
  Size(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Mean := sum / (W*H);

  square := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do square += Sqr(a[i,j] - Mean);
  Stdev := Sqrt(square / (W*H));
end;
