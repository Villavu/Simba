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


function pDTMToTDTM(Const DTM: pDTM): TDTM;
function tDTMTopDTM(Const DTM: TDTM): pDTM;
procedure PrintpDTM(const aDTM : pDTM);

procedure initdtm(out d: pdtm; len: integer);
function ValidMainPointBox(var dtm: pDTM; const x1, y1, x2, y2: Integer): TBox;
function ValidMainPointBoxRotated(var dtm: pDTM; const x1, y1, x2, y2: Integer;const
                                  sAngle, eAngle, aStep: Extended): TBox;
function DTMConsistent(const dtm: pdtm): boolean;
procedure NormalizeDTM(var dtm: pdtm);
function RotateDTM(const dtm: pdtm; angle: extended) : pdtm;
function copydtm(const dtm: pdtm): pdtm;

const
    dtm_Rectangle = 0;
    dtm_Cross = 1;
    dtm_DiagonalCross = 2;
    dtm_Circle = 3;
    dtm_Triangle = 4;

implementation
uses math,MufasaBase;

procedure RotatePoints_(Var P: TPointArray; A, cx, cy: Extended);
Var
   I, L: Integer;
   CosA,SinA : extended;

Begin
  L := High(P);
  CosA := Cos(a);
  SinA := Sin(a);
  For I := 0 To L Do
  Begin
    P[I].X := Trunc(cx + CosA * (p[i].x - cx) - SinA * (p[i].y - cy));
    P[I].Y := Trunc(cy + SinA * (p[i].x - cx) + CosA * (p[i].y - cy));
  End;
  // I recon it's faster than Point().
End;

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

procedure PrintpDTM(const aDTM : pDTM);
var
  i : integer;
begin;
  i := 0;
  if adtm.n <> '' then
    mDebugLn('Name: ' + aDTM.n);
  mDebugLn('MainPoint ' + inttostr(aDTM.p[i].x) + ', ' + inttostr(aDTM.p[i].y) + ' col: ' + inttostr(aDTM.c[i]) + ', tol: ' + inttostr(aDTM.t[i]) + '; ashape ' + inttostr(aDTM.ash[i]) + ' asize ' + inttostr(aDTM.asz[i])+ ', Bad Point: ' + BoolToStr(aDTM.bp[i]));
  for I := 1 to High(aDTM.p) do
    mDebugLn('SubPoint['+IntToStr(I) + '] ' + inttostr(aDTM.p[i].x) + ', ' + inttostr(aDTM.p[i].y) + ' col: ' + inttostr(aDTM.c[i]) + ', tol: ' + inttostr(aDTM.t[i]) + '; ashape ' + inttostr(aDTM.ash[i]) + ' asize ' + inttostr(aDTM.asz[i]) + ', Bad Point: ' + BoolToStr(aDTM.bp[i]));
end;

function pDTMToTDTM(Const DTM: pDTM): TDTM;

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

function tDTMTopDTM(Const DTM: TDTM): pDTM;
var
   I: Integer;
begin
  Result.l := Length(DTM.SubPoints) + 1; //The mainpoint is in a different structure
  SetLength(Result.p, Result.l);
  SetLength(Result.c, Result.l);
  SetLength(Result.t, Result.l);
  SetLength(Result.asz, Result.l);
  SetLength(Result.ash, Result.l);
  SetLength(Result.bp, Result.l);

  Result.p[0].x := DTM.MainPoint.x;
  Result.p[0].y := DTM.MainPoint.y;
  Result.c[0]   := DTM.MainPoint.Color;
  Result.t[0]   := DTM.MainPoint.Tolerance;
  Result.asz[0] := DTM.MainPoint.AreaSize;
  Result.ash[0] := DTM.MainPoint.AreaShape;

  For I := 1 To Result.l - 1 Do  // High + 1 = Length
  Begin
    Result.p[I].x := DTM.SubPoints[I - 1].x;
    Result.p[I].y := DTM.SubPoints[I - 1].y;
    Result.c[I]   := DTM.SubPoints[I - 1].Color;
    Result.t[I]   := DTM.SubPoints[I - 1].Tolerance;
    Result.asz[I] := DTM.SubPoints[I - 1].AreaSize;
    Result.ash[I] := DTM.SubPoints[I - 1].AreaShape;
  End;

  setlength(result.bp, result.l);
  for i := 0 to result.l -1 do
    result.bp[i] := false;
end;

{ TODO: Check if bounds are correct? }
function DTMConsistent(const dtm: pdtm): boolean;
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
  // we don't need this check really...
  {if dtm.p[0] = Point(0,0) then  //Already normalized
    exit;}
  for i := 1 to dtm.l - 1 do
    dtm.p[i] := dtm.p[i] - dtm.p[0];
  dtm.p[0] := dtm.p[0] - dtm.p[0]; //Point(0,0);
end;

function ValidMainPointBox(var dtm: pDTM; const x1, y1, x2, y2: Integer): TBox;

var
   i: Integer;
   b: TBox;

begin
  NormalizeDTM(dtm);

  FillChar(b, SizeOf(TBox), 0); //Sets all the members to 0
  b.x1 := MaxInt;
  b.y1 := MaxInt;
  for i := 0 to dtm.l - 1 do
  begin
    b.x1 := min(b.x1, dtm.p[i].x);// - dtm.asz[i]);
    b.y1 := min(b.y1, dtm.p[i].y);// - dtm.asz[i]);
    b.x2 := max(b.x2, dtm.p[i].x);// + dtm.asz[i]);
    b.y2 := max(b.y2, dtm.p[i].y);// + dtm.asz[i]);
  end;

  //writeln(Format('DTM Bounding Box: %d, %d : %d, %d', [b.x1, b.y1,b.x2,b.y2]));
  Result.x1 := x1 - b.x1;
  Result.y1 := y1 - b.y1;
  Result.x2 := x2 - b.x2;
  Result.y2 := y2 - b.y2;
end;

function ValidMainPointBoxRotated(var dtm: pDTM; const x1, y1, x2, y2: Integer;
                                  const sAngle, eAngle, aStep: Extended): TBox;

var
   i: Integer;
   d:extended;

begin
  NormalizeDTM(dtm);

{ Delete the ASZ
  for i := 0 to high(dtm.c) do
  begin
    d := max(d, sqrt(sqr(dtm.p[i].x - dtm.asz[i]) + sqr(dtm.p[i].x - dtm.asz[i])));
    d := max(d, sqrt(sqr(dtm.p[i].y - dtm.asz[i]) + sqr(dtm.p[i].y - dtm.asz[i])));
    d := max(d, sqrt(sqr(dtm.p[i].x + dtm.asz[i]) + sqr(dtm.p[i].x + dtm.asz[i])));
    d := max(d, sqrt(sqr(dtm.p[i].y + dtm.asz[i]) + sqr(dtm.p[i].y + dtm.asz[i])));
  end;   }

  Result.x1 := x1 + ceil(d);
  Result.y1 := y1 + ceil(d);
  Result.x2 := x2 - ceil(d);
  Result.y2 := y2 - ceil(d);
end;

function RotateDTM(const dtm: pdtm; angle: extended) : pDTM;
begin
  if length(dtm.p) = 0 then
    raise Exception.Create('RotateDTM, no points in DTM.');
  result := copydtm(dtm);
  RotatePoints_(result.p, angle, result.p[0].x, result.p[0].y);
end;

function copydtm(const dtm: pdtm): pdtm;
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

