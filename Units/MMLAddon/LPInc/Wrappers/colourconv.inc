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

    colourconv.inc for the Mufasa Macro Library
}

procedure lpColorToRGB(Params: PParamArray);
begin
  colour_conv.ColorToRGB(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure lpRGBToColor(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := RGBToColor(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure lpColorToHSL(Params: PParamArray);
begin
  ColorToHSL(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure lpHSLToColor(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := HSLToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure lpColorToXYZ(Params: PParamArray);
begin
  ColorToXYZ(PInt32(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^);
end;

procedure lpXYZToColor(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := XYZToColor(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure lpRGBToHSL(Params: PParamArray);
begin
  RGBToHSL(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure lpHSLtoRGB(Params: PParamArray);
var
  R, G, B: byte;
begin
  HSLtoRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, R, G, B);
  PInt32(Params^[3])^ := R;
  PInt32(Params^[4])^ := G;
  PInt32(Params^[5])^ := B;
end;

procedure lpRGBToXYZ(Params: PParamArray);
begin
  RGBToXYZ(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure lpXYZToRGB(Params: PParamArray);
var
  R, G, B: byte;
begin;
  XYZToRGB(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, R, G, B);
  PInt32(Params^[3])^ := R;
  PInt32(Params^[4])^ := G;
  PInt32(Params^[5])^ := B;
end;
