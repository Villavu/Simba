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

    other.inc for the Mufasa Macro Library
}

procedure lpWriteLn_Str(Params: PParamArray);
begin
  psWriteLn(PlpString(Params^[0])^);
end;

procedure lpWriteLn_FMT(Params: PParamArray);
begin
  psWriteLn(PlpString(Params^[0])^); //Had Problems with Format....
end;

procedure lpWriteLn_Int32(Params: PParamArray);
begin
  psWriteLn(IntToStr(PInt32(Params^[0])^));
end;

procedure lpWriteLn_Int64(Params: PParamArray);
begin
  psWriteLn(IntToStr(PInt64(Params^[0])^));
end;

procedure lpWriteLn_Bool(Params: PParamArray);
begin
  psWriteLn(BoolToStr(PBoolean(Params^[0])^));
end;

procedure lpWriteLn_Extended(Params: PParamArray);
begin
  psWriteLn(FloatToStr(PExtended(Params^[0])^));
end;

procedure lpCallProc(Params: PParamArray; Result: Pointer);
begin
  PVariant(Result)^ := CallProc(PlpString(Params^[0])^, PVariantArray(Params^[1])^);
end;

procedure lpThreadSafeCall(Params: PParamArray; Result: Pointer);
begin
  PVariant(Result)^ := ThreadSafeCall(PlpString(Params^[0])^, PVariantArray(Params^[1])^);
end;

procedure lpRandom(Params: PParamArray; Result: Pointer);
begin
  PInt32(Result)^ := Random(PInt32(Params^[0])^);
end;

procedure lpWait(Params: PParamArray);
begin
  ps_Wait(PLongWord(Params^[0])^);
end;

procedure lpWait_Rand(Params: PParamArray);
begin
  ps_Wait(PLongWord(Params^[0])^ + Random(PLongWord(Params^[1])^));
end;

