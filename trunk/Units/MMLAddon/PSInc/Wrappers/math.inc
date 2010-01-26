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

function ps_ceil(e : extended) : integer;ps_decl;
begin
  result := ceil(e);
end;

function ps_pow(base,exponent : extended) : extended; ps_decl;
begin
  result := power(base,exponent);
end;

function ps_Max(a,b : integer) : integer; ps_decl;
begin
  result := max(a,b);
end;

function ps_Min(a, b: Integer): Integer; ps_decl;
begin
  result := min(a,b);
end;

function ps_MinE(a, b : extended) : extended; ps_decl;
begin
  result := min(a,b);
end;

function ps_MaxE(a,b : extended) : extended; ps_decl;
begin
  result := max(a,b);
end;

function ps_Sqr(e : extended) : extended;ps_decl;
begin
  result := sqr(e);
end;

function ps_Point(x,y : integer) : TPoint;ps_decl;
begin
  result := classes.point(x,y);
end;
function ps_Distance(x1,y1,x2,y2 : integer) : integer;ps_decl;
begin
  Result := Round(Sqrt(Sqr(x2-x1) + Sqr(y2-y1)));
end;

function ps_Hypot(X, Y: Extended): Extended;ps_decl;
begin
  result := hypot(x,y);
end;

function ps_RandomRange(const aFrom, aTo: Integer): Integer; ps_decl;
begin
  Result:=Random(Abs(aFrom-aTo))+Min(aTo,AFrom);
end;

function Factorial(number: longword): Int64; ps_decl;
var
  Loop : longword;
begin
  result := 1;
  for loop := number downto 2 do
    result := result * loop;
end;

function BinCoe(a, b: LongInt): Extended; ps_decl;
begin
  result := Factorial(a) / (factorial(b) * factorial(a-b));
end;

function FixD(Degrees : extended) : Extended; ps_decl;
begin;
  Result := Degrees;
  while Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;
