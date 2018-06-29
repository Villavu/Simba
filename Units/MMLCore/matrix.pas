{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

  Matrix utilities to complement methods like matchtemplate.
}
unit matrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mufasatypes;

procedure MatrixSetSize(var a: TSingleMatrix; Width, Height: Int32);
procedure MatrixSize(a: TSingleMatrix; out Width, Height: Int32);
function MatrixWidth(a: TSingleMatrix): Int32;
function MatrixHeight(a: TSingleMatrix): Int32;
function MatrixToInt(a: TSingleMatrix): T2DIntArray;
function MatrixMean(a: TSingleMatrix): Single;
procedure MatrixMeanStdev(a: TSingleMatrix; out Mean, Stdev: Single);
procedure MatrixMinMax(a: TSingleMatrix; out vMin,vMax: Single);
function MatrixArgMax(a: TSingleMatrix): TPoint;
function MatrixArgMin(a: TSingleMatrix): TPoint;
function MatrixNormMinMax(a: TSingleMatrix; Alpha, Beta: Single): TSingleMatrix;
function MatrixIndices(a: TSingleMatrix; Value: Single; Comparator: EComparator): TPointArray;
function MatrixExtract(a: TSingleMatrix; Indices: TPointArray): TSingleArray;
procedure MatrixFill(a: TSingleMatrix; Indices: TPointArray; Values: TSingleArray);

implementation

uses
  math;

procedure MatrixSetSize(var a: TSingleMatrix; Width, Height: Int32);
begin
  SetLength(a, Height, Width);
end;

procedure MatrixSize(a: TSingleMatrix; out Width, Height: Int32);
begin
  Height := Length(a);
  if Height > 0 then
    Width := Length(a[0])
  else
    Width := 0;
end;

function MatrixWidth(a: TSingleMatrix): Int32;
begin
  Result := 0;
  if Length(a) > 0 then
    Result := Length(a[0]);
end;

function MatrixHeight(a: TSingleMatrix): Int32;
begin
  Result := Length(a);
end;

function MatrixToInt(a: TSingleMatrix): T2DIntArray;
var
  Lo,Hi,oldRange,newRange: Single;
  X,Y,W,H: Int32;
begin
  MatrixSize(a, W,H);
  SetLength(Result, H,W);
  for y:=0 to H-1 do
    for x:=0 to W-1 do
      Result[y,x] := Trunc(a[y,x]);
end;

function MatrixMean(a: TSingleMatrix): Single;
var
  W,H,i,j: Int32;
  sum: Double;
begin
  MatrixSize(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Result := sum / (W*H);
end;

procedure MatrixMeanStdev(a: TSingleMatrix; out Mean, Stdev: Single);
var
  W,H,i,j:Int32;
  sum, square: Double;
begin
  MatrixSize(a, W,H);
  sum := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do sum += a[i,j];
  Mean := sum / (W*H);

  square := 0;
  for i:=0 to h-1 do
    for j:=0 to w-1 do square += Sqr(a[i,j] - Mean);
  Stdev := Sqrt(square / (W*H));
end;

procedure MatrixMinMax(a: TSingleMatrix; out vMin,vMax: Single);
var
  W,H,i,j:Int32;
begin
  MatrixSize(a, W,H);
  vMin := 0; 
  vMax := 0;
  if H = 0 then Exit;
  
  vMin := a[0,0]; vMax := a[0,0];
  for i:=0 to h-1 do
    for j:=0 to w-1 do
    begin
      if a[i,j] > vMax then vMax := a[i,j];
      if a[i,j] < vMin then vMin := a[i,j];
    end;
end; 

function MatrixArgMax(a: TSingleMatrix): TPoint;
var X,Y,W,H:Int32;
begin
  Result := Point(0,0);
  MatrixSize(a, W,H);

  for Y:=0 to H-1 do
    for X:=0 to W-1 do
      if a[Y,X] > a[Result.y,Result.x] then
      begin
        Result.x := x;
        Result.y := y;
      end;
end;

function MatrixArgMin(a: TSingleMatrix): TPoint;
var X,Y,W,H:Int32;
begin
  Result := Point(0,0);
  MatrixSize(a, W,H);

  for Y:=0 to H-1 do
    for X:=0 to W-1 do
      if a[Y,X] < a[Result.y,Result.x] then
      begin
        Result.x := x;
        Result.y := y;
      end;
end;

function MatrixNormMinMax(a: TSingleMatrix; Alpha, Beta: Single): TSingleMatrix;
var
  Lo,Hi,oldRange,newRange: Single;
  X,Y,W,H: Int32;
begin
  MatrixSize(a, W,H);
  SetLength(Result, H,W);
  MatrixMinMax(a, Lo,Hi);

  oldRange := Hi-Lo;
  newRange := Beta-Alpha;
  
  if (oldRange = 0) then 
    Exit;
  
  for Y:=0 to H-1 do
    for X:=0 to W-1 do
      Result[Y,X] := (a[Y,X] - lo) / oldRange * newRange + Alpha;
end;

function MatrixIndices(a: TSingleMatrix; Value: Single; Comparator: EComparator): TPointArray;
var 
  x,y,W,H,c: Int32;
  Match: Boolean;
begin
  MatrixSize(a, W,H);
  if (W = 0) or (H = 0) then
    Exit;

  SetLength(Result, 512);
  c := 0;
  for Y:=0 to H-1 do
    for X:=0 to W-1 do
    begin
      Match := False;
      case Comparator of
        __LT__: Match := a[y,x] < Value;
        __GT__: Match := a[y,x] > Value;
        __EQ__: Match := a[y,x] = Value;
        __LE__: Match := a[y,x] <= Value;
        __GE__: Match := a[y,x] >= Value;
        __NE__: Match := a[y,x] <> Value;
      end;

      if Match then
      begin
        if c = Length(Result) then
          SetLength(Result, 2*c);
        Result[c] := Point(x,y);
        Inc(c);
      end;
    end;
  SetLength(Result, c);
end;

function MatrixExtract(a: TSingleMatrix; Indices: TPointArray): TSingleArray;
var
  i,c,W,H: Int32;
begin
  MatrixSize(a, W,H);
  if (W = 0) or (H = 0) then
    Exit;

  SetLength(Result, Length(Indices));
  c := 0;
  for i:=0 to High(Indices) do
    if (Indices[i].x >= 0) and (Indices[i].y >= 0) and
       (Indices[i].x < W)  and (Indices[i].y < H) then
    begin
      Result[c] := a[Indices[i].y][Indices[i].x];
      Inc(c);
    end;
  SetLength(Result, c);
end;

procedure MatrixFill(a: TSingleMatrix; Indices: TPointArray; Values: TSingleArray);
var
  i,c,W,H: Int32;
begin
  MatrixSize(a, W,H);
  if (W = 0) or (H = 0) then
    Exit;

  if (Length(Values) <> 1) and (Length(Values) <> Length(Indices)) then
    raise Exception.CreateFmt('The number of values has to be 1, or equal to the number of indices - (Indices=%d, Values=%d)', [Length(Values), Length(Indices)]);

  if Length(Values) = 1 then
  begin
    for i:=0 to High(Indices) do
      if (Indices[i].x >= 0) and (Indices[i].y >= 0) and
         (Indices[i].x < W)  and (Indices[i].y < H) then
        a[Indices[i].y][Indices[i].x] := Values[0];
  end else
  begin
    for i:=0 to High(Indices) do
      if (Indices[i].x >= 0) and (Indices[i].y >= 0) and
         (Indices[i].x < W)  and (Indices[i].y < H) then
        a[Indices[i].y][Indices[i].x] := Values[i];
  end;
end;


end.

