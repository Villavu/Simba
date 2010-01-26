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

    Colour.inc for the Mufasa Macro Library
}

function GetColor(x,y : integer) : TColor; ps_decl;
begin;
  Result := CurrThread.Client.IOManager.GetColor(x,y);
end;

procedure GetColorsWrap(Coords : TPointArray; var Colors:  TIntegerArray); ps_decl;
begin
  Colors := CurrThread.Client.MFinder.GetColors(coords);
end;

function GetColors(Coords : TPointArray) : TIntegerArray; ps_decl;
begin
  result := CurrThread.Client.MFinder.GetColors(coords);
end;

function findcolor(out x, y: integer; color, x1, y1, x2, y2: integer): boolean; ps_decl;
begin
  Result := CurrThread.Client.MFinder.FindColor(x, y, color, x1, y1, x2, y2);
end;

function findcolortoleranceOptimised(out x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean; ps_decl;
begin
  Result := CurrThread.Client.MFinder.FindColorToleranceOptimised(x, y, color, x1, y1, x2, y2, tol);
end;
function findcolortolerance(out x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean; ps_decl;
begin
  Result := CurrThread.Client.MFinder.FindColorTolerance(x, y, color, x1, y1, x2, y2, tol);
end;

function FindColors(out TPA: TPointArray; Color, x1, y1, x2, y2: Integer): Boolean; ps_decl;
begin
  Result := CurrThread.Client.MFinder.FindColors(TPA, color, x1, y1, x2, y2);
end;

procedure SetColorToleranceSpeed(cts: Integer); ps_decl;
begin
  CurrThread.Client.MFinder.SetToleranceSpeed(cts);
end;

function GetToleranceSpeed: Integer; ps_decl;
begin
  CurrThread.Client.MFinder.GetToleranceSpeed;
end;

procedure SetToleranceSpeed2Modifiers(nHue, nSat: Extended); ps_decl;
begin
  CurrThread.Client.MFinder.SetToleranceSpeed2Modifiers(nHue, nSat);
end;

procedure GetToleranceSpeed2Modifiers(out hMod, sMod: Extended); ps_decl;
begin
  CurrThread.Client.MFinder.GetToleranceSpeed2Modifiers(hMod, sMod);
end;

function SimilarColors(Col1,Col2,Tol : integer) : boolean; ps_decl;
begin;
  Result := CurrThread.Client.MFinder.SimilarColors(Col1,Col2,Tol);
end;

function CountColor(Color, xs, ys, xe, ye: Integer): Integer; ps_decl;
begin;
  result := CurrThread.Client.MFinder.CountColor(color,xs,ys,xe,ye);
end;
function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer; ps_decl;
begin;
  result := CurrThread.Client.MFinder.CountColorTolerance(color,xs,ys,xe,ye,tolerance);
end;

function FindColorsToleranceOptimised(out Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean; ps_decl;
begin;
  result := CurrThread.Client.MFinder.FindColorsToleranceOptimised(points,color,xs,ys,xe,ye,tolerance);
end;
function FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean; ps_decl;
begin;
  result := CurrThread.Client.MFinder.FindColorsTolerance(points,color,xs,ys,xe,ye,tolerance);
end;

function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean; ps_decl;
begin;
  result := CurrThread.Client.MFinder.FindColorSpiral(x,y,color,xs,ys,xe,ye);
end;

function FindColorsSpiralTolerance(x, y: Integer; out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean; ps_decl;
begin;
  result := CurrThread.Client.MFinder.FindColorsSpiralTolerance(x,y,Points,color,xs,ys,xe,ye,tolerance);
end;

function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea: Integer): Boolean; ps_decl;
begin;
  result := CurrThread.Client.MFinder.FindColoredArea(x,y,color,xs,ys,xe,ye,minarea);
end;

function FindColoredAreaTolerance(var x, y: Integer; Color, xs, ys, xe, ye, MinArea, tol: Integer): Boolean;  ps_decl;
begin;
  result := CurrThread.Client.MFinder.FindColoredAreaTolerance(x,y,color,xs,ys,xe,ye,minarea,tol);
end;
  
