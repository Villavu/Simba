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

    ocr.inc for the Mufasa Macro Library
}

procedure lprs_GetUpText(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;

procedure lprs_GetUpTextAtEx(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Client.MOCR.GetUpTextAtEx(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure lprs_GetUpTextAt(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Client.MOCR.GetUpTextAtEx(PLongInt(Params^[0])^, PLongInt(Params^[1])^, True);
end;

procedure lpBitmapFromText(Params: PParamArray; Result: Pointer);
var
  bmp: TMufasaBitmap;
begin
  bmp := CurrThread.Client.MOCR.TextToFontBitmap(PlpString(Params^[0])^, PlpString(Params^[1])^);
  PLongInt(Result)^ := CurrThread.Client.MBitmaps.AddBMP(bmp);
end;

procedure lpMaskFromText(Params: PParamArray; Result: Pointer);
begin
  PMask(Result)^ := CurrThread.Client.MOCR.TextToMask(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure lpTPAFromTextWrap(Params: PParamArray);
begin
  PPointArray(Params^[4])^ := CurrThread.Client.MOCR.TextToFontTPA(PlpString(Params^[0])^, PlpString(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^);
end;

procedure lpTPAFromText(Params: PParamArray; Result: Pointer);
begin
  PPointArray(Result)^ := CurrThread.Client.MOCR.TextToFontTPA(PlpString(Params^[0])^, PlpString(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^);
end;

procedure lpGetTextATPA(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Client.MOCR.GetTextATPA(P2DPointArray(Params^[0])^, PLongInt(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpGetTextAt(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Client.MOCR.GetTextAt(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^,PLongInt(Params^[6])^, PLongInt(Params^[7])^, PlpString(Params^[8])^);
end;

procedure lpGetTextAtEx(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Client.MOCR.GetTextAt(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^,PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^, PlpString(Params^[9])^);
end;

procedure lpLoadSystemFont(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MOCR.Fonts.LoadSystemFont(PFont(Params^[0])^, PlpString(Params^[1])^);
end;

procedure lpLoadFont(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MOCR.Fonts.LoadFont(PlpString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure lpFreeFont(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MOCR.Fonts.FreeFont(PlpString(Params^[0])^);
end;


