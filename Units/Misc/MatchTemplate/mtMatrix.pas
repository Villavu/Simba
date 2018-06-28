unit mtMatrix;
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
{$I header.inc}

interface

uses
  SysUtils, mtCore;

procedure Size(a: T2DIntArray; out W,H: Int32);
procedure Size(a: T2DI64Array; out W,H: Int32);
procedure Size(a: T2DSingleArray; out W,H: Int32);
procedure Size(a: T2DDoubleArray; out W,H: Int32);
procedure Size(a: T2DComplexArray; out W,H: Int32);

function Area(a: T2DIntArray): Int32;
function Area(a: T2DI64Array): Int32;
function Area(a: T2DSingleArray): Int32;
function Area(a: T2DDoubleArray): Int32;
function Area(a: T2DComplexArray): Int32;

function SumsPd(a: T2DIntArray; out Square: T2DI64Array): T2DIntArray;
function SumsPd(a: T2DSingleArray; out Square: T2DDoubleArray): T2DDoubleArray;
function SumsPd(a: T2DDoubleArray; out Square: T2DDoubleArray): T2DDoubleArray;

function Rot90(a: T2DIntArray): T2DIntArray;
function Rot90(a: T2DSingleArray): T2DSingleArray;
function Rot90(a: T2DDoubleArray): T2DDoubleArray;
function Rot90(a: T2DComplexArray): T2DComplexArray;

function Mean(a: T2DIntArray): Double;
function Mean(a: T2DSingleArray): Double;
function Mean(a: T2DDoubleArray): Double;

procedure MeanStdev(a: T2DIntArray; out Mean, Stdev: Double);
procedure MeanStdev(a: T2DSingleArray; out Mean, Stdev: Double);
procedure MeanStdev(a: T2DDoubleArray; out Mean, Stdev: Double);

procedure SplitRGB(Image: T2DIntArray; out R,G,B: T2DSingleArray);

implementation

uses
  Math;

{$I matrix/size.inc}
{$I matrix/sums.inc}
{$I matrix/rot90.inc}
{$I matrix/stats.inc}

procedure SplitRGB(Image: T2DIntArray; out R,G,B: T2DSingleArray);
var
  W,H,x,y: Int32;
begin
  Size(Image, W,H);
  SetLength(R, H,W);
  SetLength(G, H,W);
  SetLength(B, H,W);
  for y:=0 to H-1 do
    for x:=0 to W-1 do begin
      R[y,x] := Image[y,x]{shr 00}and $FF;
      G[y,x] := Image[y,x] shr 08 and $FF;
      B[y,x] := Image[y,x] shr 16 and $FF;
    end;
end;

end.
