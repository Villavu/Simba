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

    DTM Utilities for the Mufasa Macro Library
}

unit dtmutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes;


Function pDTMToTDTM(Const DTM: pDTM): TDTM;
Function tDTMTopDTM(Const DTM: TDTM): pDTM;
Procedure PrintpDTM(tDTM : pDTM);

implementation

Procedure PrintpDTM(tDTM : pDTM);
var
  i : integer;
begin;
  i := 0;
  WriteLn('MainPoint ' + inttostr(tDTM.p[i].x) + ', ' + inttostr(tDTM.p[i].y) + ' col: ' + inttostr(tDTM.c[i]) + ', tol: ' + inttostr(tDTM.t[i]) + '; ashape ' + inttostr(tdtm.ash[i]) + ' asize ' + inttostr(tdtm.asz[i]));
  for I := 1 to High(tDTM.p) do
    WriteLn('SubPoint['+IntToStr(I) + '] ' + inttostr(tDTM.p[i].x) + ', ' + inttostr(tDTM.p[i].y) + ' col: ' + inttostr(tDTM.c[i]) + ', tol: ' + inttostr(tDTM.t[i]) + '; ashape ' + inttostr(tdtm.ash[i]) + ' asize ' + inttostr(tdtm.asz[i]));
end;
Function pDTMToTDTM(Const DTM: pDTM): TDTM;

Var
   Temp: TDTMPointDef;
   I: Integer;

Begin
  For I := 0 To 0 Do
  Begin
    Temp.X := DTM.p[i].x;
    Temp.Y := DTM.p[i].y;
    Temp.AreaSize := DTM.asz[i];
    Temp.AreaShape := DTM.ash[i];
    Temp.Color := DTM.c[i];
    Temp.Tolerance := DTM.t[i];
  End;
  Result.MainPoint := Temp;
  SetLength(Result.SubPoints, Length(DTM.p) - 1);

  For I := 1 To High(DTM.p) Do
  Begin
    Temp.X := 0; Temp.Y := 0; Temp.AreaSize := 0; Temp.AreaShape := 0; Temp.Color := 0; Temp.Tolerance := 0;
    Temp.X := DTM.p[i].x;
    Temp.Y := DTM.p[i].y;
    Temp.AreaSize := DTM.asz[i];
    Temp.AreaShape := DTM.ash[i];
    Temp.Color := DTM.c[i];
    Temp.Tolerance := DTM.t[i];
    Result.SubPoints[I - 1] := Temp;
  End;
End;

{/\
  Converts a TDTM to a pDTM.
/\}

Function tDTMTopDTM(Const DTM: TDTM): pDTM;

Var
   //Temp: TDTMPointDef;
   I: Integer;

Begin
  SetLength(Result.p, Length(DTM.SubPoints) + 1);
  SetLength(Result.c, Length(DTM.SubPoints) + 1);
  SetLength(Result.t, Length(DTM.SubPoints) + 1);
  SetLength(Result.asz, Length(DTM.SubPoints) + 1);
  SetLength(Result.ash, Length(DTM.SubPoints) + 1);

  Result.p[0].x := DTM.MainPoint.x;
  Result.p[0].y := DTM.MainPoint.y;
  Result.c[0]   := DTM.MainPoint.Color;
  Result.t[0]   := DTM.MainPoint.Tolerance;
  Result.asz[0] := DTM.MainPoint.AreaSize;
  Result.ash[0] := DTM.MainPoint.AreaShape;

  For I := 1 To Length(DTM.SubPoints) Do  // High + 1 = Length
  Begin
    Result.p[I].x := DTM.SubPoints[I - 1].x;
    Result.p[I].y := DTM.SubPoints[I - 1].y;
    Result.c[I]   := DTM.SubPoints[I - 1].Color;
    Result.t[I]   := DTM.SubPoints[I - 1].Tolerance;
    Result.asz[I] := DTM.SubPoints[I - 1].AreaSize;
    Result.ash[I] := DTM.SubPoints[I - 1].AreaShape;
  End;
End;

end.

