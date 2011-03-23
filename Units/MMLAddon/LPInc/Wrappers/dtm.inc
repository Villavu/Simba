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

    DTM.inc for the Mufasa Macro Library
}

procedure lpFindDTM(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindDTM(MDTMs[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindDTMs(Params: PParamArray; Result: Pointer);
begin
   with CurrThread.Client do
     PBoolean(Result)^ := MFinder.FindDTMs(MDTMs[PLongInt(Params^[0])^], PPointArray(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^);
end;

procedure lpFindDTMRotatedAlternating(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, True);
end;

procedure lpFindDTMRotatedSE(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindDTMRotated(MDTMs[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, False);
end;

procedure lpFindDTMsRotatedAlternating(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PLongInt(Params^[0])^], PPointArray(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, P2DExtendedArray(Params^[9])^, True);
end;

procedure lpFindDTMsRotatedSE(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindDTMsRotated(MDTMs[PLongInt(Params^[0])^], PPointArray(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, P2DExtendedArray(Params^[9])^, False);
end;

procedure lpSetDTMName(Params: PParamArray);
begin
  CurrThread.Client.MDTMs[PLongInt(Params^[0])^].Name := PlpString(Params^[1])^;
end;

procedure lpDTMFromString(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MDTMs.StringToDTM(PlpString(Params^[0])^);
end;

procedure lpFreeDTM(Params: PParamArray);
begin
  CurrThread.Client.MDTMs.FreeDTM(PLongInt(Params^[0])^);
end;

procedure lpGetDTM(Params: PParamArray; Result: Pointer);
begin
  PMDTM(Result)^ := CurrThread.Client.MDTMs[PLongInt(Params^[0])^];
end;

procedure lpAddTSDTM(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MDTMs.AddDTM(PSDTM(Params^[0])^);
end;

procedure lpAddDTM(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MDTMs.AddDTM(PMDTM(Params^[0])^);
end;

procedure lpPrintDTM(Params: PParamArray);
begin
  PrintDTM(PMDTM(Params^[0])^);
end;

procedure lpMDTMToSDTM(Params: PParamArray; Result: Pointer);
begin
  PSDTM(Result)^ := MDTMToSDTM(PMDTM(Params^[0])^);
end;

procedure lpSDTMToMDTM(Params: PParamArray; Result: Pointer);
begin
  PMDTM(Result)^ := SDTMToMDTM(PSDTM(Params^[0])^);
  CurrThread.Client.MDTMs.AddDTM(PMDTM(Result)^);
end;

procedure lpCreateDTMPoint(Params: PParamArray; Result: Pointer);
begin
  PMDTMPoint(Result)^ := CreateDTMPoint(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PBoolean(Params^[5])^);
end;
