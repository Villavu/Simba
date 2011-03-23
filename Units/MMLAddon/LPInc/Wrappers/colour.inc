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
  PLongInt(Result)^ := CurrThread.Client.IOManager.GetColor(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
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
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColor(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindColorToleranceOptimised(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorToleranceOptimised(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^);
end;

procedure lpFindColorTolerance(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorTolerance(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^);
end;

procedure lpFindColors(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColors(PPointArray(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^);
end;

procedure lpSetColorToleranceSpeed(Params: PParamArray);
begin
  CurrThread.Client.MFinder.SetToleranceSpeed(PLongInt(Params^[0])^);
end;

procedure lpGetToleranceSpeed(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFinder.GetToleranceSpeed;
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
  PBool(Result)^ := CurrThread.Client.MFinder.SimilarColors(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpCountColor(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MFinder.CountColor(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^);
end;

procedure lpCountColorTolerance(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MFinder.CountColorTolerance(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^);
end;

procedure lpFindColorsToleranceOptimised(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorsToleranceOptimised(PPointArray(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindColorsTolerance(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorsTolerance(PPointArray(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindColorSpiral(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorSpiral(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindColorSpiralTolerance(Params: PParamArray; Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorSpiralTolerance(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^);
end;

procedure lpFindColorsSpiralTolerance(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColorsSpiralTolerance(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PPointArray(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^);
end;

procedure lpFindColoredArea(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColoredArea(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^);
end;

procedure lpFindColoredAreaTolerance(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindColoredAreaTolerance(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^);
end;
