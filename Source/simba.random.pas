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

end.

