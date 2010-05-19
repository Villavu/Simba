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

function ps_FindDTM(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean; extdecl;
begin
  with CurrThread.Client do
    result := MFinder.FindDTM(MDTMs[DTM],x,y,xs,ys,xe,ye);
end;

function ps_FindDTMs(DTM: Integer; var p: TPointArray; xs, ys, xe, ye: Integer): Boolean; extdecl;
begin
   with CurrThread.Client do
     result := MFinder.FindDTMs(MDTMs[DTM], p, xs, ys, xe, ye);
end;

function ps_FindDTMRotatedAlternating(DTM: Integer; var x, y: Integer; xs, ys, xe, ye:
                           Integer; sAngle, eAngle, aStep: Extended;
                           var aFound: Extended): Boolean; extdecl;
begin
  with CurrThread.Client do
    result := MFinder.FindDTMRotated(MDTMs[DTM], x,y, xs, ys, xe, ye, sAngle, eAngle, aStep, aFound,true);
end;

function ps_FindDTMRotatedSE(DTM: Integer; var x, y: Integer; xs, ys, xe, ye:
                           Integer; sAngle, eAngle, aStep: Extended;
                           var aFound: Extended): Boolean; extdecl;
begin
  with CurrThread.Client do
    result := MFinder.FindDTMRotated(MDTMs[DTM], x, y, xs, ys, xe, ye, sAngle, eAngle, aStep, aFound,false);
end;

function ps_FindDTMsRotatedAlternating(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray): Boolean; extdecl;
begin
  with CurrThread.Client do
    result := MFinder.FindDTMsRotated(MDTMs[DTM], Points, xs, ys, xe, ye,
                                  sAngle, eAngle, aStep, aFound, true);
end;

function ps_FindDTMsRotatedSE(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray): Boolean; extdecl;
begin
  with CurrThread.Client do
    result := MFinder.FindDTMsRotated(MDTMs[DTM], Points, xs, ys, xe, ye,
                                  sAngle, eAngle, aStep, aFound, false);
end;

procedure ps_SetDTMName(DTM : integer;const name : string);
begin
  CurrThread.Client.MDTMs[DTM].Name := Name;
end;

function ps_DTMFromString(const DTMString: String): Integer; extdecl;
begin
  With CurrThread.Client.MDTMs do
    Result := AddDTM(StringToDTM(DTMString));
end;

procedure ps_FreeDTM(DTM: Integer); extdecl;
begin
  CurrThread.Client.MDTMs.FreeDTM(DTM);
end;

function ps_GetDTM(index: Integer) : TMDTM; extdecl;
begin
  result := CurrThread.Client.MDTMs[Index];
end;

function ps_AddTSDTM(const d: TSDTM): Integer; extdecl;
begin
  Result := CurrThread.Client.MDTMs.AddDTM(d);
end;

function ps_AddDTM(const d: TMDTM): Integer; extdecl;
begin
  Result := CurrThread.Client.MDTMs.AddDTM(d);
end;

procedure ps_PrintDTM(const aDTM : TMDTM);extdecl;
begin
  PrintDTM(aDTM);
end;

function ps_MDTMToSDTM(Const DTM: TMDTM): TSDTM;extdecl;
begin
  result := MDTMToSDTM(DTM);
end;

function ps_SDTMToMDTM(Const DTM: TSDTM): TMDTM;extdecl;
begin
  result := SDTMToMDTM(DTM);
end;
