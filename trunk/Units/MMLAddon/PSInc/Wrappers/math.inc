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

function ps_iAbs(a : integer) : integer;extdecl;
begin
  result := abs(a);
end;

function ps_ceil(e : extended) : integer;extdecl;
begin
  result := ceil(e);
end;

function ps_pow(base,exponent : extended) : extended; extdecl;
begin
  result := power(base,exponent);
end;

function ps_Max(a,b : integer) : integer; extdecl;
begin
  result := max(a,b);
end;

function ps_Min(a, b: Integer): Integer; extdecl;
begin
  result := min(a,b);
end;

function ps_MinE(a, b : extended) : extended; extdecl;
begin
  result := min(a,b);
end;

function ps_MaxE(a,b : extended) : extended; extdecl;
begin
  result := max(a,b);
end;

function ps_Sqr(e : extended) : extended;extdecl;
begin
  result := sqr(e);
end;

function ps_Point(x,y : integer) : TPoint;extdecl;
begin
  result := classes.point(x,y);
end;
function ps_Distance(x1,y1,x2,y2 : integer) : integer;extdecl;
begin
  Result := Round(Sqrt(Sqr(x2-x1) + Sqr(y2-y1)));
end;

function ps_Hypot(X, Y: Extended): Extended;extdecl;
begin
  result := hypot(x,y);
end;

function ps_RandomRange(const aFrom, aTo: Integer): Integer; extdecl;
begin
  Result:=Random(Abs(aFrom-aTo))+Min(aTo,AFrom);
end;

function ps_ArcTan2(x,y : extended) : extended;  extdecl;
begin
  result := ArcTan2(x,y);
end;

procedure ps_IncEx(var x : integer; increase : integer); extdecl;
begin
  x := x + increase;
end;

procedure ps_DecEx(var x : integer; Decrease : integer); extdecl;
begin
  x := x - Decrease;
end;

function Factorial(number: longword): Int64; extdecl;
var
  Loop : longword;
begin
  result := 1;
  for loop := number downto 2 do
    result := result * loop;
end;

function BinCoe(a, b: LongInt): Extended; extdecl;
begin
  result := Factorial(a) / (factorial(b) * factorial(a-b));
end;

function FixD(Degrees : extended) : Extended; extdecl;
begin;
  Result := Degrees;
  while Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;

function IntToBox(x1,y1,x2,y2 : integer) : TBox; extdecl;
begin;
  result.x1 := x1;
  result.y1 := y1;
  result.x2 := x2;
  result.y2 := y2;
end;

function IntInBox(x, y: Integer; Box: TBox): Boolean; extdecl;
begin;
  result := (((x >= Box.x1) and(x <= Box.x2)) and ((y >= box.y1) and (y <= box.y2)));
end;

function PointToBox(PT1,PT2 : TPoint) : TBox; extdecl;
begin;
  result.x1 := PT1.x;
  result.y1 := PT1.y;
  result.x2 := PT2.x;
  result.y2 := PT2.y;
end;

function PointInBox(PT : TPoint; Box: TBox): Boolean; extdecl;
begin;
  result := (((PT.x >= Box.x1) and(PT.x <= Box.x2)) and ((PT.y>= box.y1) and (PT.y <= box.y2)));
end;

function ps_floor(e : extended) : integer; extdecl;
begin;
  result := floor(e);
end;
