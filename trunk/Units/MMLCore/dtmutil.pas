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
  Classes, SysUtils, MufasaTypes, tpa;


Function pDTMToTDTM(Const DTM: pDTM): TDTM;
Function tDTMTopDTM(Const DTM: TDTM): pDTM;
Procedure PrintpDTM(aDTM : pDTM);

procedure initdtm(out d: pdtm; len: integer);
function ValidMainPointBox(var dtm: pDTM; const x1, y1, x2, y2: Integer): TBox;
Function ValidMainPointBoxRotated(var dtm: pDTM; const x1, y1, x2, y2: Integer;
                                  sAngle, eAngle, aStep: Extended): TBox;
function DTMConsistent(var dtm: pdtm): boolean;
procedure NormalizeDTM(var dtm: pdtm);
procedure RotateDTM(var dtm: pdtm; angle: extended);
function copydtm(const dtm: pdtm): pdtm;

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
   i: integer = 0;
begin
  d.l := len;
  d.n := '';
  setlength(d.p, len);
  setlength(d.c, len);
  setlength(d.t, len);
  setlength(d.ash, len);
  setlength(d.asz, len);
  setlength(d.bp, len);

  FillChar(d.p[0], SizeOf(TPoint) * len, 0);
  FillChar(d.c[0], SizeOf(Integer) * len, 0);
  FillChar(d.t[0], SizeOf(Integer) * len, 0);
  FillChar(d.ash[0], SizeOf(Integer) * len, 0);

  // Better set it to 1, than fill with 0.
  FillChar(d.asz[0], SizeOf(Integer) * len, 0);

  //FillChar(d.gp[0], SizeOf(Boolean) * len, 0);
  for i := 0 to len - 1 do
    d.bp[i] := False;
end;

Procedure PrintpDTM(aDTM : pDTM);
var
  i : integer;
begin;
  i := 0;
  if adtm.n <> '' then
    writeln('Name: ' + aDTM.n);
  WriteLn('MainPoint ' + inttostr(aDTM.p[i].x) + ', ' + inttostr(aDTM.p[i].y) + ' col: ' + inttostr(aDTM.c[i]) + ', tol: ' + inttostr(aDTM.t[i]) + '; ashape ' + inttostr(aDTM.ash[i]) + ' asize ' + inttostr(aDTM.asz[i])+ ', Bad Point: ' + BoolToStr(aDTM.bp[i]));
  for I := 1 to High(aDTM.p) do
    WriteLn('SubPoint['+IntToStr(I) + '] ' + inttostr(aDTM.p[i].x) + ', ' + inttostr(aDTM.p[i].y) + ' col: ' + inttostr(aDTM.c[i]) + ', tol: ' + inttostr(aDTM.t[i]) + '; ashape ' + inttostr(aDTM.ash[i]) + ' asize ' + inttostr(aDTM.asz[i]) + ', Bad Point: ' + BoolToStr(aDTM.bp[i]));
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

  For I := 1 To DTM.l-1 Do
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
  SetLength(Result.bp, Length(DTM.SubPoints) + 1);

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

  Result.l := length(Result.p);
  setlength(result.bp, result.l);
  for i := 0 to result.l -1 do
    result.bp[i] := false;
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
  if dtm.l <> length(dtm.bp) then
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

var
   i: Integer;
   d:extended;

begin
  for i := 1 to high(dtm.c) do
  begin
    dtm.p[i] := dtm.p[i] - dtm.p[0];
  end;
  dtm.p[0] := dtm.p[0] - dtm.p[0];

  for i := 0 to high(dtm.c) do
  begin
    d := max(d, sqrt(sqr(dtm.p[i].x - dtm.asz[i]) + sqr(dtm.p[i].x - dtm.asz[i])));
    d := max(d, sqrt(sqr(dtm.p[i].y - dtm.asz[i]) + sqr(dtm.p[i].y - dtm.asz[i])));
    d := max(d, sqrt(sqr(dtm.p[i].x + dtm.asz[i]) + sqr(dtm.p[i].x + dtm.asz[i])));
    d := max(d, sqrt(sqr(dtm.p[i].y + dtm.asz[i]) + sqr(dtm.p[i].y + dtm.asz[i])));
  end;

  Result.x1 := x1 + ceil(d);
  Result.y1 := y1 + ceil(d);
  Result.x2 := x2 - ceil(d);
  Result.y2 := y2 - ceil(d);
end;

procedure RotateDTM(var dtm: pdtm; angle: extended);

begin
  if length(dtm.p) = 0 then
    raise Exception.Create('RotateDTM, no points in DTM.');
  RotatePoints(dtm.p, angle, dtm.p[0].x, dtm.p[0].y);
end;

function copydtm(const dtm: pdtm): pdtm;
var
   i: integer;
begin
  initdtm(result,dtm.l);
  Move(dtm.p[0], result.p[0], length(dtm.p) * sizeof(Tpoint));
  Move(dtm.c[0], result.c[0], length(dtm.c) * sizeof(Integer));
  Move(dtm.t[0], result.t[0], length(dtm.t) * sizeof(Integer));
  Move(dtm.asz[0], result.asz[0], length(dtm.asz) * sizeof(Integer));
  Move(dtm.ash[0], result.ash[0], length(dtm.ash) * sizeof(Integer));
  Move(dtm.bp[0], result.bp[0], length(dtm.bp) * sizeof(Boolean));
  result.n := 'Copy of ' + dtm.n;
end;

end.

