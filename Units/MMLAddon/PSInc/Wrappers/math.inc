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

function ps_round(e : extended) : integer; extdecl;
begin
  result := round(e);
end;
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
  if (exponent=0) then
    result := 1
  else
    result := power(base,exponent);
end;

function ps_RiemannGauss(Xstart,StepSize,Sigma : extended; AmountSteps : integer) : extended; extdecl;
begin
  result := RiemannGauss(Xstart,StepSize,Sigma,AmountSteps);
end;

function ps_DiscreteGauss(Xstart,Xend : integer; sigma : extended) : TExtendedArray; extdecl;
begin
  result := DiscreteGauss(Xstart,Xend,Sigma);
end;

function ps_GaussMatrix(N : integer; sigma : extended) : T2DExtendedArray; extdecl;
begin
  result := GaussMatrix(N,sigma);
end;

function ps_exp(exponent : extended) : extended; extdecl;
begin
  result := exp(exponent);
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

function ps_Random(Int: integer): integer; extdecl;
begin
  result := Random(int);
end;

function ps_RandomE : extended;
begin
  result := Random;
end;

function ps_ArcTan2(y,x : extended) : extended;  extdecl;
begin
  result := ArcTan2(y,x);
end;

procedure ps_IncEx(var x : integer; increase : integer); extdecl;
begin
  x := x + increase;
end;

procedure ps_DecEx(var x : integer; Decrease : integer); extdecl;
begin
  x := x - Decrease;
end;

function ps_Factorial(number: longword): Int64; extdecl;
var
  Loop : longword;
begin
  result := 1;
  for loop := number downto 2 do
    result := result * loop;
end;

function ps_BinCoe(a, b: LongInt): Extended; extdecl;
begin
  result := ps_Factorial(a) / (ps_Factorial(b) * ps_Factorial(a-b));
end;

function ps_FixD(Degrees : extended) : Extended; extdecl;
begin;
  Result := Degrees;
  while Result < 0 do
    Result := Result + 360;
  while Result > 360 do
    Result := Result - 360;
end;
function ps_InRange(const value,min,max : integer) : boolean;extdecl;
begin
  Result := InRange(value,min,max);
end;

function ps_IntToBox(x1,y1,x2,y2 : integer) : TBox; extdecl;
begin;
  result.x1 := x1;
  result.y1 := y1;
  result.x2 := x2;
  result.y2 := y2;
end;

function ps_IntInBox(x, y: Integer; Box: TBox): Boolean; extdecl;
begin;
  result := (((x >= Box.x1) and(x <= Box.x2)) and ((y >= box.y1) and (y <= box.y2)));
end;

function ps_PointToBox(PT1,PT2 : TPoint) : TBox; extdecl;
begin;
  result.x1 := PT1.x;
  result.y1 := PT1.y;
  result.x2 := PT2.x;
  result.y2 := PT2.y;
end;

function ps_PointInBox(PT : TPoint; Box: TBox): Boolean; extdecl;
begin;
  result := (((PT.x >= Box.x1) and(PT.x <= Box.x2)) and ((PT.y>= box.y1) and (PT.y <= box.y2)));
end;

function ps_floor(e : extended) : integer; extdecl;
begin;
  result := floor(e);
end;
function ps_logn(base, x : extended): extended;extdecl;
begin
  result := logn(base,x);
end;
function ps_ln(x : extended) : extended;extdecl;
begin
  result := ln(x);
end;

function ps_inttohex(value : integer) : string; extdecl;
begin
  result := IntToHex(value,1);
end;

function ps_hextoint(hex : string) : integer;extdecl;
begin
  result := StrToInt('$' + hex);
end;
function ps_sar(AValue : longint; shift : byte) : longint; extdecl;
begin;
  Shift:=Shift and 31;
  Result:=longint(dword(dword(dword(AValue) shr Shift) or (dword(longint(dword(0-dword(dword(AValue) shr 31)) and dword(longint(0-(ord(Shift<>0){ and 1}))))) shl (32-Shift))));
end;
function ps_ror(num : longword; shift : byte) : LongWord; extdecl;
begin
  result := RorDWord(num,shift);
end;

function ps_rol(num : longword; shift : byte) : LongWord; extdecl;
begin
  result := RolDWord(num,shift);
end;

function ps_tan(e: extended): extended; extdecl;
begin
  result:= tan(e);
end;

function ps_radians(e: extended): extended; extdecl;
begin
  result := e / 180.0 * pi;
end;

function ps_degrees(e: extended): extended; extdecl;
begin
  result := e * 180.0 / pi;
end;

function ps_ArcSin(e: extended): extended; extdecl;
begin
  result := ArcSin(e);
end;

function ps_ArcCos(e: extended): extended; extdecl;
begin
  result := ArcCos(e);
end;

function ps_ArcTan(e: extended): extended; extdecl;
begin
  result := ArcTan(e);
end;

function ps_Cotan(e: extended): extended; extdecl;
begin
  result := Cotan(e);
end;

function ps_Secant(e: extended): extended; extdecl;
begin
  result := Secant(e);
end;

function ps_Cosecant(e: extended): extended;extdecl;
begin
  result := Cosecant(e);
end;

function ps_Cot(e: extended): extended; extdecl;
begin
  result := Cot(e);
end;

function ps_Sec(e: extended): extended; extdecl;
begin
  result := Sec(e);
end;

function ps_Csc(e: extended): extended; extdecl;
begin
  result := Csc(e);
end;

function ps_Cosh(e: extended): extended; extdecl;
begin
  result := Cosh(e);
end;

function ps_Sinh(e: extended): extended; extdecl;
begin
  result := Sinh(e);
end;

function ps_Tanh(e: extended): extended; extdecl;
begin
  result := tanh(e);
end;

function ps_CotH(e: extended): extended;extdecl;
begin
  result := cotan(e);
end;

function ps_SecH(e: extended): extended; extdecl;
begin
  result := secant(e);
end;

function ps_CscH(e: extended): extended;extdecl;
begin
  result := Csc(e);
end;

function ps_ArcCosh(e: extended): extended;extdecl;
begin
  result := ArcCosh(e);
end;

function ps_ArcSinh(e: extended): extended; extdecl;
begin
  result := ArcSinh(e);
end;

function DecRet(e: Extended): Extended; extdecl;
begin
  result := e - Trunc(e);
end;

function ps_log10(f: Extended): Extended; extdecl;
begin
  result := log10(f);
end;
