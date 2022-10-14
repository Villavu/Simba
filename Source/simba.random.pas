{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.random;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

function RandomCenterTPA(Amount: Integer; Box: TBox): TPointArray;
function RandomTPA(Amount: Integer; Box: TBox): TPointArray;

function RandomLeft(Lo, Hi: Double): Double; overload;
function RandomLeft(Lo, Hi: Int64): Int64; overload;

function RandomRight(Lo, Hi: Double): Double; overload;
function RandomRight(Lo, Hi: Int64): Int64; overload;

function RandomMean(Lo, Hi: Double): Double; overload;
function RandomMean(Lo, Hi: Int64): Int64; overload;

function RandomMode(Mode, Lo, Hi: Double): Double; overload;
function RandomMode(Mode, Lo, Hi: Int64): Int64; overload;

var
  RandCutoff: Double = 5;

implementation

uses
  math;

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

function RandomLeft(Lo, Hi: Double): Double;

  function nzRandom: Double;
  begin
    Result := Max(Double(Random()), 1.0e-320);
  end;

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

  function nzRandom: Double;
  begin
    Result := Max(Double(Random()), 1.0e-320);
  end;

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

end.

