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

    colour.inc for the Mufasa Macro Library
}

procedure lpGetColor(Params: PParamArray; Result: Pointer);
begin;
  PInt32(Result)^ := CurrThread.Client.IOManager.GetColor(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure lpGetColorsWrap(Params: PParamArray);
begin
  PIntegerArray(Params^[1])^ := CurrThread.Client.MFinder.GetColors(PPointArray(Params^[0])^);
end;

procedure lpGetColors(Params: PParamArray; Result: Pointer);
begin
  PIntegerArray(Result)^ := CurrThread.Client.MFinder.GetColors(PPointArray(Params^[0])^);
end;

procedure lpFindColor(Params: PParamArray; Result: Pointer);
begin
  PBool(Result)^ := CurrThread.Client.MFinder.FindColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure lpFindColorToleranceOptimised(Params: PParamArray; Result: Pointer);
begin
  PBool(Result)^ := CurrThread.Client.MFinder.FindColorToleranceOptimised(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

procedure lpFindColorTolerance(Params: PParamArray; Result: Pointer);
begin
  PBool(Result)^ := CurrThread.Client.MFinder.FindColorTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

{function ps_FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Integer): Boolean; extdecl;
begin
  Result := CurrThread.Client.MFinder.FindColors(TPA, color, x1, y1, x2, y2);
end;}

procedure lpSetColorToleranceSpeed(Params: PParamArray);
begin
  CurrThread.Client.MFinder.SetToleranceSpeed(PInt32(Params^[0])^);
end;

procedure lpGetToleranceSpeed(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := CurrThread.Client.MFinder.GetToleranceSpeed;
end;

procedure lpSetToleranceSpeed2Modifiers(Params: PParamArray);
begin
  CurrThread.Client.MFinder.SetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure lpGetToleranceSpeed2Modifiers(Params: PParamArray);
begin
  CurrThread.Client.MFinder.GetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure lpSimilarColors(Params: PParamArray; Result: Pointer);
begin;
  PBool(Result)^ := CurrThread.Client.MFinder.SimilarColors(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

{function ps_CountColor(Color, xs, ys, xe, ye: Integer): Integer; extdecl;
begin;
  result := CurrThread.Client.MFinder.CountColor(color,xs,ys,xe,ye);
end;

function ps_CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer; extdecl;
begin;
  result := CurrThread.Client.MFinder.CountColorTolerance(color,xs,ys,xe,ye,tolerance);
end;}

{function ps_FindColorsToleranceOptimised(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindColorsToleranceOptimised(points,color,xs,ys,xe,ye,tolerance);
end;

function ps_FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindColorsTolerance(points,color,xs,ys,xe,ye,tolerance);
end;}

procedure lpFindColorSpiral(Params: PParamArray; Result: Pointer);
begin;
  PBool(Result)^ := CurrThread.Client.MFinder.FindColorSpiral(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^);
end;

procedure lpFindColorSpiralTolerance(Params: PParamArray; Result: Pointer);
begin
  PBool(Result)^ := CurrThread.Client.MFinder.FindColorSpiralTolerance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^);
end;

{function ps_FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindColorsSpiralTolerance(x,y,Points,color,xs,ys,xe,ye,tolerance);
end;

function ps_FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea: Integer): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindColoredArea(x,y,color,xs,ys,xe,ye,minarea);
end;

function ps_FindColoredAreaTolerance(var x, y: Integer; Color, xs, ys, xe, ye, MinArea, tol: Integer): Boolean;  extdecl;
begin;
  result := CurrThread.Client.MFinder.FindColoredAreaTolerance(x,y,color,xs,ys,xe,ye,minarea,tol);
end;
}