{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.random;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray;
function RandomTPA(Amount: Integer; Box: TBox): TPointArray;

function RandomRange(Lo, Hi: Double): Double; overload;

function RandomLeft(Lo, Hi: Double): Double; overload;
function RandomLeft(Lo, Hi: Int64): Int64; overload;

function RandomRight(Lo, Hi: Double): Double; overload;
function RandomRight(Lo, Hi: Int64): Int64; overload;

function RandomMean(Lo, Hi: Double): Double; overload;
function RandomMean(Lo, Hi: Int64): Int64; overload;

function RandomMode(Mode, Lo, Hi: Double): Double; overload;
function RandomMode(Mode, Lo, Hi: Int64): Int64; overload;

function GaussRand(Mean, Dev: Double): Double;

procedure BetterRandomize;

var
  RandCutoff: Double = 5;

implementation

uses
  Math,
  simba.process;

function nzRandom: Double;
begin
  Result := Max(Double(Random()), 1.0e-320);
end;

function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray;
var
  i, xcenter, ycenter: Integer;
  x, y, xstep, ystep: Single;
begin
  SetLength(Result, Amount);

  xcenter := Box.Center.X;
  ycenter := Box.Center.Y;
  xstep := (Box.Width div 2) / Amount;
  ystep := (Box.Height div 2) /Amount;

  x:=0;
  y:=0;

  for i := 0 to Amount - 1 do
  begin
    x := x + xstep;
    y := y + ystep;
    Result[i].x := RandomRange(Round(xcenter-x), Round(xcenter+x));
    Result[i].y := RandomRange(Round(ycenter-y), Round(ycenter+y));
  end;
end;

function RandomTPA(Amount: Integer; Box: TBox): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, Amount);
  for i := 0 to Amount - 1 do
    Result[i] := Point(RandomRange(Box.X1, Box.X2), RandomRange(Box.Y1, Box.Y2));
end;

function RandomRange(Lo, Hi: Double): Double;
begin
  Result := Lo + Random() * (Hi - Lo);
end;

function RandomLeft(Lo, Hi: Double): Double;
begin
  Result := RandCutoff + 1;
  while Result >= RandCutoff do
    Result := Abs(Sqrt(-2 * Ln(nzRandom())) * Cos(2 * PI * Random()));
  Result := Result / RandCutoff * (Hi-Lo) + Lo;
end;

function RandomLeft(Lo, Hi: Int64): Int64;
begin
  Result := Round(RandomLeft(Lo * 1.00, Hi * 1.00));
end;

function RandomRight(Lo, Hi: Double): Double;
begin
  Result := RandomLeft(Hi, Lo);
end;

function RandomRight(Lo, Hi: Int64): Int64;
begin
  Result := Round(RandomRight(Lo * 1.00, Hi * 1.00));
end;

function RandomMean(Lo, Hi: Double): Double;
begin
  case Random(2) of
    0: Result := (Hi+Lo) / 2.0 + RandomLeft(0, (Hi-Lo) / 2);
    1: Result := (Hi+Lo) / 2.0 - RandomLeft(0, (Hi-Lo) / 2);
  end;
end;

function RandomMean(Lo, Hi: Int64): Int64;
begin
  Result := Round(RandomMean(Lo * 1.00, Hi * 1.00));
end;

function RandomMode(Mode, Lo, Hi: Double): Double;
var
  Top: Double;
begin
  Top := Lo;
  if Random() * (Hi-Lo) > Mode-Lo then
    Top := Hi;

  Result := RandCutoff + 1;
  while Result >= RandCutoff do
    Result := Abs(Sqrt(-2 * Ln(nzRandom())) * Cos(2 * PI * Random()));
  Result := Result / RandCutoff * (Top-Mode) + Mode;
end;

function RandomMode(Mode, Lo, Hi: Int64): Int64;
begin
  Result := Round(RandomMode(Mode * 1.00, Lo * 1.00, Hi * 1.00));
end;

function GaussRand(Mean, Dev: Double): Double;
var
  Len: Double;
begin
  Len := Dev * Sqrt(-2 * Ln(nzRandom()));
  Result := Mean + Len * Cos(2 * PI * Random());
end;

{$R-}{$Q-}

// https://github.com/dajobe/libmtwist/blob/master/seed.c
procedure BetterRandomize;

  procedure Mix(var A, B, C: UInt32);
  begin
    A -= C;  A := A xor ((C shl 4)  or (C shr (32-4)));  C += B;
    B -= A;  B := B xor ((A shl 6)  or (A shr (32-6)));  A += C;
    C -= B;  C := C xor ((B shl 8)  or (B shr (32-8)));  B += A;
    A -= C;  A := A xor ((C shl 16) or (C shr (32-16))); C += B;
    B -= A;  B := B xor ((A shl 19) or (A shr (32-19))); A += C;
    C -= B;  C := C xor ((B shl 4)  or (B shr (32-4)));  B += A;
  end;

var
  A, B, C: UInt32;
begin
  A := UInt32(GetTickCount64());
  B := UInt32(GetProcessID());
  C := UInt32(SimbaProcess.GetProcessRunnningTime(GetProcessID()));

  Mix(A, B, C);

  RandSeed := C;
end;

end.

