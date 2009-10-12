{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venentië and Merlijn Wajer

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

    Mufasa Math Unit for the Mufasa Macro Library
}

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

