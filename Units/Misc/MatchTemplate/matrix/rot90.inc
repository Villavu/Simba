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
function Rot90(a: T2DIntArray): T2DIntArray;
var W,H,i,j:Int32;
begin
  Size(a, w,h);
  SetLength(Result, W,H);
  for i:=0 to H-1 do
    for j:=0 to W-1 do Result[j,i] := a[i,j];
end;

function Rot90(a: T2DSingleArray): T2DSingleArray;
var W,H,i,j:Int32;
begin
  Size(a, w,h);
  SetLength(Result, W,H);
  for i:=0 to H-1 do
    for j:=0 to W-1 do Result[j,i] := a[i,j];
end;

function Rot90(a: T2DDoubleArray): T2DDoubleArray;
var W,H,i,j:Int32;
begin
  Size(a, w,h);
  SetLength(Result, W,H);
  for i:=0 to H-1 do
    for j:=0 to W-1 do Result[j,i] := a[i,j];
end;

function Rot90(a: T2DComplexArray): T2DComplexArray;
var W,H,i,j:Int32;
begin
  Size(a, w,h);
  SetLength(Result, W,H);
  for i:=0 to H-1 do
    for j:=0 to W-1 do Result[j,i] := a[i,j];
end;
