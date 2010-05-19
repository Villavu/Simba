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
  Classes, SysUtils, dtm,tpa,MufasaTypes;


function MDTMToSDTM(Const DTM: TMDTM): TSDTM;
function SDTMToMDTM(Const DTM: TSDTM): TMDTM;
procedure PrintDTM(const aDTM : TMDTM);
function CreateDTMPoint(x,y,c,t,asz : integer; bp : boolean) : TMDTMPoint;

{procedure iniTSDTM(out d: TMDTM; len: integer);}
function ValidMainPointBox(var dtm: TMDTM; const x1, y1, x2, y2: Integer): TBox;
function ValidMainPointBox(const TPA: TPointArray; const x1, y1, x2, y2: Integer): TBox;
function ValidMainPointBoxRotated(var dtm: TMDTM; const x1, y1, x2, y2: Integer;const
                                  sAngle, eAngle, aStep: Extended): TBox;
procedure NormalizeDTM(var dtm: TMDTM);
{function RotateDTM(const dtm: TMDTM; angle: extended) : TMDTM;
function copydtm(const dtm: TMDTM): TMDTM;                     }

const
    dtm_Rectangle = 0;
    dtm_Cross = 1;
    dtm_DiagonalCross = 2;
    dtm_Circle = 3;
    dtm_Triangle = 4;

implementation
uses math,MufasaBase;


function CreateDTMPoint(x,y,c,t,asz : integer; bp : boolean) : TMDTMPoint;
begin
  result.x := x;
  result.y := y;
  result.c := c;
  result.t := t;
  result.asz := asz;
  result.bp := bp;
end;

procedure PrintDTM(const aDTM : TMDTM);
var
  i : integer;
begin;
  i := 0;
  if aDTM.count = 0 then
    exit;
  if adtm.Name <> '' then
    mDebugLn('Name: ' + aDTM.name);
  mDebugLn('MainPoint ' + inttostr(aDTM.Points[i].x) + ', ' + inttostr(aDTM.Points[i].y) + ' col: ' + inttostr(aDTM.Points[i].c) + ', tol: ' + inttostr(aDTM.Points[i].t) + ', asize: ' + inttostr(aDTM.Points[i].asz)+ ', Bad Point: ' + BoolToStr(aDTM.Points[i].bp));
  for I := 1 to High(aDTM.Points) do
    mDebugLn('SubPoint['+IntToStr(I) + '] ' + inttostr(aDTM.Points[i].x) + ', ' + inttostr(aDTM.Points[i].y) + ' col: ' + inttostr(aDTM.Points[i].c) + ', tol: ' + inttostr(aDTM.Points[i].t) +', asize: ' + inttostr(aDTM.Points[i].asz) + ', Bad Point: ' + BoolToStr(aDTM.Points[i].bp));
end;

function MDTMToSDTM(Const DTM: TMDTM): TSDTM;

Var
   Temp: TSDTMPointDef;
   I: Integer;

Begin
  For I := 0 To 0 Do
  Begin
    Temp.X := DTM.Points[i].x;
    Temp.Y := DTM.Points[i].y;
    Temp.AreaSize := DTM.Points[i].asz;
    Temp.AreaShape := 0;
    Temp.Color := DTM.Points[i].c;
    Temp.Tolerance := DTM.Points[i].t;
  End;
  Result.MainPoint := Temp;
  SetLength(Result.SubPoints, DTM.Count - 1);

  For I := 1 To DTM.Count-1 Do
  Begin
    Temp.X := 0; Temp.Y := 0; Temp.AreaSize := 0; Temp.AreaShape := 0; Temp.Color := 0; Temp.Tolerance := 0;
    Temp.X := DTM.Points[i].x;
    Temp.Y := DTM.Points[i].y;
    Temp.AreaSize := DTM.Points[i].asz;
    Temp.AreaShape := 0;
    Temp.Color := DTM.Points[i].c;
    Temp.Tolerance := DTM.Points[i].t;
    Result.SubPoints[I - 1] := Temp;
  End;
End;

{/\
  Converts a TSDTM to a TMDTM.
/\}

function SDTMToMDTM(Const DTM: TSDTM): TMDTM;
var
   I: Integer;
begin
  Result := TMDTM.Create;
  Result.Count := Length(DTM.SubPoints) + 1; //The mainpoint is in a different structure

  Result.Points[0].x   := DTM.MainPoint.x;
  Result.Points[0].y   := DTM.MainPoint.y;
  Result.Points[0].c   := DTM.MainPoint.Color;
  Result.Points[0].t   := DTM.MainPoint.Tolerance;
  Result.Points[0].asz := DTM.MainPoint.AreaSize;

  For I := 1 To Result.Count - 1 Do  // High + 1 = Length
  Begin
    Result.Points[I].x   := DTM.SubPoints[I - 1].x;
    Result.Points[I].y   := DTM.SubPoints[I - 1].y;
    Result.Points[I].c   := DTM.SubPoints[I - 1].Color;
    Result.Points[I].t   := DTM.SubPoints[I - 1].Tolerance;
    Result.Points[I].asz := DTM.SubPoints[I - 1].AreaSize;
  End;

  for i := 0 to result.Count -1 do
    result.Points[i].bp := false;
end;

procedure NormalizeDTM(var dtm: TMDTM);
var
   i:integer;
begin
  if (dtm = nil) or (dtm.count < 1) or ((dtm.Points[0].x = 0) and (dtm.Points[0].y = 0)) then  //Already normalized
    exit;
  for i := 1 to dtm.Count - 1 do
  begin
    dtm.Points[i].x := dtm.Points[i].x - dtm.Points[0].x;
    dtm.Points[i].y := dtm.Points[i].y - dtm.Points[0].y;
  end;
  dtm.Points[0].x := 0;
  dtm.Points[0].y := 0;
end;

function ValidMainPointBox(var dtm: TMDTM; const x1, y1, x2, y2: Integer): TBox;

var
   i: Integer;
   b: TBox;

begin
  NormalizeDTM(dtm);

  FillChar(b, SizeOf(TBox), 0); //Sets all the members to 0
  b.x1 := MaxInt;
  b.y1 := MaxInt;
  for i := 0 to dtm.Count - 1 do
  begin
    b.x1 := min(b.x1, dtm.Points[i].x);// - dtm.asz[i]);
    b.y1 := min(b.y1, dtm.Points[i].y);// - dtm.asz[i]);
    b.x2 := max(b.x2, dtm.Points[i].x);// + dtm.asz[i]);
    b.y2 := max(b.y2, dtm.Points[i].y);// + dtm.asz[i]);
  end;

  //writeln(Format('DTM Bounding Box: %d, %d : %d, %d', [b.x1, b.y1,b.x2,b.y2]));
  Result.x1 := x1 - b.x1;
  Result.y1 := y1 - b.y1;
  Result.x2 := x2 - b.x2;
  Result.y2 := y2 - b.y2;
end;
function ValidMainPointBox(const TPA: TPointArray; const x1, y1, x2, y2: Integer): TBox;
var
  i: Integer;
  b: TBox;
begin
  b := GetTPABounds(TPA);
  Result.x1 := x1 - b.x1;
  Result.y1 := y1 - b.y1;
  Result.x2 := x2 - b.x2;
  Result.y2 := y2 - b.y2;
end;

function ValidMainPointBoxRotated(var dtm: TMDTM; const x1, y1, x2, y2: Integer;
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

{function RotateDTM(const dtm: TMDTM; angle: extended) : TMDTM;
begin
  if DTM.c then
    raise Exception.Create('RotateDTM, no points in DTM.');
  result := copydtm(dtm);
  RotatePoints_(result.p, angle, result.p[0].x, result.p[0].y);
end;    }

{function copydtm(const dtm: TMDTM): TMDTM;
begin
  iniTSDTM(result,dtm.l);
  Move(dtm.p[0], result.p[0], length(dtm.p) * sizeof(Tpoint));
  Move(dtm.c[0], result.c[0], length(dtm.c) * sizeof(Integer));
  Move(dtm.t[0], result.t[0], length(dtm.t) * sizeof(Integer));
  Move(dtm.asz[0], result.asz[0], length(dtm.asz) * sizeof(Integer));
  Move(dtm.ash[0], result.ash[0], length(dtm.ash) * sizeof(Integer));
  Move(dtm.bp[0], result.bp[0], length(dtm.bp) * sizeof(Boolean));
  result.n := 'Copy of ' + dtm.n;
end;  }

end.

