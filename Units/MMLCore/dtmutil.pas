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

procedure initdtm(out d: pdtm; len: integer);
function ValidMainPointBox(var dtm: pDTM; const x1, y1, x2, y2: Integer): TBox;
function ValidMainPointBoxRotated(var dtm: pDTM; const x1, y1, x2, y2: Integer;
                            sAngle, eAngle, aStep: Extended): TBox;
function DTMConsistent(var dtm: pdtm): boolean;
procedure NormalizeDTM(var dtm: pdtm);

const
    dtm_Rectangle = 0;
    dtm_Cross = 1;
    dtm_DiagonalCross = 2;
    dtm_Circle = 3;
    dtm_Triangle = 4;

implementation
uses math;

// macro
procedure initdtm(out d: pdtm; len: integer);
var
   i: integer;
begin
  d.l := len;
  d.n := '';
  setlength(d.p, len);
  setlength(d.c, len);
  setlength(d.t, len);
  setlength(d.ash, len);
  setlength(d.asz, len);
  setlength(d.gp, len);

  FillChar(d.p[0], SizeOf(TPoint) * len, 0);
  FillChar(d.c[0], SizeOf(Integer) * len, 0);
  FillChar(d.t[0], SizeOf(Integer) * len, 0);
  FillChar(d.ash[0], SizeOf(Integer) * len, 0);

  // Better set it to 1, than fill with 0.
  FillChar(d.asz[0], SizeOf(Integer) * len, 0);

  //FillChar(d.gp[0], SizeOf(Boolean) * len, 0);
  for i := 0 to len - 1 do
    d.gp[i] := true;
end;

Procedure PrintpDTM(tDTM : pDTM);
var
  i : integer;
begin;
  i := 0;
  if tdtm.n <> '' then
    writeln('Name: ' + tdtm.n);
  WriteLn('MainPoint ' + inttostr(tDTM.p[i].x) + ', ' + inttostr(tDTM.p[i].y) + ' col: ' + inttostr(tDTM.c[i]) + ', tol: ' + inttostr(tDTM.t[i]) + '; ashape ' + inttostr(tdtm.ash[i]) + ' asize ' + inttostr(tdtm.asz[i])+ ', Good: ' + BoolToStr(tdtm.gp[i]));
  for I := 1 to High(tDTM.p) do
    WriteLn('SubPoint['+IntToStr(I) + '] ' + inttostr(tDTM.p[i].x) + ', ' + inttostr(tDTM.p[i].y) + ' col: ' + inttostr(tDTM.c[i]) + ', tol: ' + inttostr(tDTM.t[i]) + '; ashape ' + inttostr(tdtm.ash[i]) + ' asize ' + inttostr(tdtm.asz[i]) + ', Good: ' + BoolToStr(tdtm.gp[i]));
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
    Temp.Good:= DTM.gp[i];
  End;
  Result.MainPoint := Temp;
  SetLength(Result.SubPoints, Length(DTM.p) - 1);

  For I := 1 To DTM.l-1 Do
  Begin
    Temp.X := 0; Temp.Y := 0; Temp.AreaSize := 0; Temp.AreaShape := 0; Temp.Color := 0; Temp.Tolerance := 0;
    Temp.X := DTM.p[i].x;
    Temp.Y := DTM.p[i].y;
    Temp.AreaSize := DTM.asz[i];
    Temp.AreaShape := DTM.ash[i];
    Temp.Color := DTM.c[i];
    Temp.Tolerance := DTM.t[i];
    Temp.Good:= DTM.gp[i];
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
  SetLength(Result.gp, Length(DTM.SubPoints) + 1);

  Result.p[0].x := DTM.MainPoint.x;
  Result.p[0].y := DTM.MainPoint.y;
  Result.c[0]   := DTM.MainPoint.Color;
  Result.t[0]   := DTM.MainPoint.Tolerance;
  Result.asz[0] := DTM.MainPoint.AreaSize;
  Result.ash[0] := DTM.MainPoint.AreaShape;
  Result.gp[0] := DTM.MainPoint.Good;

  For I := 1 To Length(DTM.SubPoints) Do  // High + 1 = Length
  Begin
    Result.p[I].x := DTM.SubPoints[I - 1].x;
    Result.p[I].y := DTM.SubPoints[I - 1].y;
    Result.c[I]   := DTM.SubPoints[I - 1].Color;
    Result.t[I]   := DTM.SubPoints[I - 1].Tolerance;
    Result.asz[I] := DTM.SubPoints[I - 1].AreaSize;
    Result.ash[I] := DTM.SubPoints[I - 1].AreaShape;
    Result.gp[I] := DTM.SubPoints[I - 1].Good;
  End;
  Result.l := length(Result.p);
End;

{ TODO: Check if bounds are correct? }
function DTMConsistent(var dtm: pdtm): boolean;
var
   i: integer;
begin
  if dtm.l = 0 then
    Exit(False);
  if dtm.l <> length(dtm.p) then
    Exit(False);
  if dtm.l <> length(dtm.c) then
    Exit(False);
  if dtm.l <> length(dtm.t) then
    Exit(False);
  if dtm.l <> length(dtm.asz) then
    Exit(False);
  if dtm.l <> length(dtm.ash) then
    Exit(False);
  if dtm.l <> length(dtm.gp) then
    Exit(False);
  for i := 0 to dtm.l-1 do
    if dtm.asz[i] < 0 then
      Exit(False);
  for i := 0 to dtm.l-1 do
    if dtm.c[i] < 0 then
      Exit(False);
  for i := 0 to dtm.l-1 do
    if dtm.t[i] < 0 then
      Exit(False);
  for i := 0 to dtm.l-1 do
    if dtm.ash[i] < 0 then
      Exit(False);
  Exit(True);
end;

procedure NormalizeDTM(var dtm: pdtm);
var
   i:integer;
begin
  for i := 0 to dtm.l do
    dtm.p[i] := dtm.p[i] - dtm.p[0];
end;

Function ValidMainPointBox(var dtm: pDTM; const x1, y1, x2, y2: Integer): TBox;

var
   i: Integer;
   b: TBox;

begin
 // writeln(format('%d, %d', [0,0]));
  for i := 1 to high(dtm.c) do
  begin
    dtm.p[i] := dtm.p[i] - dtm.p[0];
 //   writeln(format('%d, %d', [dtm.p[i].x, dtm.p[i].y]));
  end;
  dtm.p[0] := dtm.p[0] - dtm.p[0];


  FillChar(b, SizeOf(TBox), 0);
  for i := 0 to high(dtm.c) do
  begin
    b.x1 := min(b.x1, dtm.p[i].x - dtm.asz[i]);
    b.y1 := min(b.y1, dtm.p[i].y - dtm.asz[i]);
    b.x2 := max(b.x2, dtm.p[i].x + dtm.asz[i]);
    b.y2 := max(b.y2, dtm.p[i].y + dtm.asz[i]);
  end;

  //FillChar(Result, SizeOf(TBox), 0);
  //writeln(Format('DTM Bounding Box: %d, %d : %d, %d', [b.x1, b.y1,b.x2,b.y2]));
  Result.x1 := x1 - b.x1;
  Result.y1 := y1 - b.y1;
  Result.x2 := x2 - b.x2;
  Result.y2 := y2 - b.y2;
end;

Function ValidMainPointBoxRotated(var dtm: pDTM; const x1, y1, x2, y2: Integer;
                            sAngle, eAngle, aStep: Extended): TBox;

begin

end;


end.

