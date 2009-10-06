unit mmath;
// mufasa math

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,MufasaTypes;

function RotatePoints(P: TPointArray; A, cx, cy: Extended): TPointArray;
function RotatePoint(p: TPoint; angle, mx, my: Extended): TPoint;


implementation

{/\
  Rotates the given points (P) by A (in radians) around the point defined by cx, cy.
/\}

function RotatePoints(P: TPointArray; A, cx, cy: Extended): TPointArray;

var
   I, L: Integer;

begin
  L := High(P);
  SetLength(Result, L + 1);
  for I := 0 to L do
  begin
    Result[I].X := Round(cx + cos(A) * (p[i].x - cx) - sin(A) * (p[i].y - cy));
    Result[I].Y := Round(cy + sin(A) * (p[i].x - cx) + cos(A) * (p[i].y - cy));
  end;
end;

{/\
  Rotates the given point (p) by A (in radians) around the point defined by cx, cy.
/\}

function RotatePoint(p: TPoint; angle, mx, my: Extended): TPoint;

begin
  Result.X := Round(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Round(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
end;

end.

