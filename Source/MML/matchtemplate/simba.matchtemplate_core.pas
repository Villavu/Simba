unit simba.matchtemplate_core;
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
  sysutils;

type
  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

  PComplex = ^Complex;
  Complex = packed record
    Re, Im: Single;
  end;

  TComplexArray   = array of Complex;
  T2DComplexArray = array of TComplexArray;

  TSingleArray   = array of Single;
  T2DSingleArray = array of array of Single;

  TDoubleArray   = array of Double;
  T2DDoubleArray = array of array of Double;

  TIntArray   = array of Int32;
  T2DIntArray = array of array of Int32;

  TI64Array   = array of Int64;
  T2DI64Array = array of array of Int64;
  
  PBox = ^TBox;
  TBox = packed record X1,Y1,X2,Y2: Int32; end;

  PPoint = ^TPoint;
  TPoint = packed record X,Y: Int32; end;
  TPointArray = array of TPoint;

  TStrArray = array of String;

  TRGB32 = packed record B,G,R,A: Byte; end;

function NextPow2(n: Int32): Int32; inline;
function ParamArray(arr: array of Pointer): TParamArray;
function Box(X1,Y1,X2,Y2: Int32): TBox; inline;
function Point(X,Y: Int32): TPoint; inline;

//-----------------------------------------------------------------------
implementation

function NextPow2(n: Int32): Int32;
begin
  n := n - 1;
  n := n or (n shr 1);
  n := n or (n shr 2);
  n := n or (n shr 4);
  n := n or (n shr 8);
  n := n or (n shr 16);
  n := n or (n shr 32);
  Result := n + 1;
end; 

function ParamArray(arr: array of Pointer): TParamArray;
var i:Int32;
begin
  for i:=0 to High(arr) do Result[i] := arr[i];
end;

function Box(X1,Y1,X2,Y2: Int32): TBox;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function Point(X,Y: Int32): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


end.
