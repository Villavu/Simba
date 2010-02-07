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

function ps_FindDTM(DTM: Integer; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean; extdecl;
var
   temp: pDTM;
begin
  if CurrThread.Client.MDTM.GetDTM(DTM, temp) then
    Result := CurrThread.Client.MFinder.FindDTM(temp, x, y, x1, y1, x2, y2)
  else
  begin
    x := 0;
    y := 0;
    Result := False;
  end;
end;

function ps_FindDTMs(DTM: Integer; out p: TPointArray; x1, y1, x2, y2: Integer): Boolean; extdecl;
var
   temp: pDTM;
begin
  if CurrThread.Client.MDTM.GetDTM(DTM, temp) then
    Result := CurrThread.Client.MFinder.FindDTMs(temp, p, x1, y1, x2, y2, 0)
  else
  begin
    setlength(p,0);
    Result := False;
  end;
end;

function ps_FindDTMRotated(DTM: Integer; out x, y: Integer; x1, y1, x2, y2:
                           Integer; sAngle, eAngle, aStep: Extended;
                           out aFound: Extended): Boolean; extdecl;
var
   temp: pDTM;
begin
  if CurrThread.Client.MDTM.GetDTM(DTM, temp) then
    Result := CurrThread.Client.MFinder.FindDTMRotated(temp, x, y, x1, y1, x2, y2, sAngle, eAngle, aStep, aFound)
  else
  begin
    x := 0;
    y := 0;
    Result := False;
  end;
end;

function ps_FindDTMsRotated(DTM: Integer; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray): Boolean; extdecl;
var
   temp: pDTM;
begin
  if CurrThread.Client.MDTM.GetDTM(DTM, temp) then
    Result := CurrThread.Client.MFinder.FindDTMsRotated(temp, Points, x1, y1, x2, y2,
                                  sAngle, eAngle, aStep, aFound, 0)
  else
    Result := False;
end;

{function ps_FindDTMsRotated(DTM: Integer; out Points: TPointArray; x1, extdecl;
                                  y1, x2, y2: Integer; sAngle, eAngle,
                                  aStep: Extended; out aFound: T2DExtendedArray)
                                  : Boolean;
begin
  Result := CurrThread.Client.MDTM.FindDTMsRotated(DTM, Points, x1, y1, x2, y2,
                                    sAngle, eAngle, aStep, aFound);
end;       }

procedure ps_SetDTMName(DTM : integer; name : string);
begin
  CurrThread.Client.MDTM.SetDTMName(DTM,name);
end;

function ps_DTMFromString(DTMString: String): Integer; extdecl;
var
   dtm: pDTM;
begin
  With CurrThread.Client.MDTM do
  begin
    dtm := StringToDTM(DTMString);
    Result := AddpDTM(dtm);
  end;
end;

procedure ps_FreeDTM(DTM: Integer); extdecl;
begin
  CurrThread.Client.MDTM.FreeDTM(DTM);
end;

function ps_GetDTM(index: Integer; out dtm: pDTM): Boolean; extdecl;
begin
 Result := CurrThread.Client.MDTM.GetDTM(index, dtm);
end;

function ps_AddDTM(d: TDTM): Integer; extdecl;
begin
  Result := CurrThread.Client.MDTM.AddDTM(d);
end;

function ps_AddpDTM(d: pDTM): Integer; extdecl;
begin
 Result := CurrThread.Client.MDTM.AddpDTM(d);
end;

Procedure ps_PrintpDTM(aDTM : pDTM);extdecl;
begin
 PrintpDTM(aDTM);
end;

