{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

    Math.inc for the Mufasa Macro Library
}

function psSqr( e : extended) : extended;
begin;
  result := sqr(e);
end;

function ps_RandomRange(const aFrom, aTo: Integer): Integer;
begin
  Result:=Random(Abs(aFrom-aTo))+Min(aTo,AFrom);
end;

function Factorial(number: longword): Int64;
var
  Loop : longword;
begin
  result := 1;
  for loop := number downto 2 do
    result := result * loop;
end;

function BinCoe(a, b: LongInt): Extended;
begin
  result := Factorial(a) / (factorial(b) * factorial(a-b));
end;

function FixD(Degrees : extended) : Extended;
begin;
  Result := Degrees;
  while Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;

procedure psSwap(var A,B);
var
  TempPtr : Pointer;
begin;
  TempPtr := pointer(A);
  pointer(a) := pointer(b);
  pointer(b) := TempPtr;
end;
