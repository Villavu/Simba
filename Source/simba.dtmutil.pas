{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dtmutil;

{$i simba.inc}

interface

uses
  Classes, SysUtils, simba.dtm, simba.tpa, simba.mufasatypes;


function MDTMToSDTM(Const DTM: TMDTM): TSDTM;
function SDTMToMDTM(Const DTM: TSDTM): TMDTM;
function CreateDTMPoint(x,y,c,t,asz : integer; bp : boolean) : TMDTMPoint;

{procedure iniTSDTM(out d: TMDTM; len: integer);}
function ValidMainPointBox(var dtm: TMDTM; const x1, y1, x2, y2: Integer): TBox;
function ValidMainPointBox(const TPA: TPointArray; const x1, y1, x2, y2: Integer): TBox;
function ValidMainPointBoxRotated(var dtm: TMDTM; const x1, y1, x2, y2: Integer;const
                                  sAngle, eAngle, aStep: Extended): TBox;
{function RotateDTM(const dtm: TMDTM; angle: extended) : TMDTM;
function copydtm(const dtm: TMDTM): TMDTM;                     }

const
    dtm_Rectangle = 0;
    dtm_Cross = 1;
    dtm_DiagonalCross = 2;
    dtm_Circle = 3;
    dtm_Triangle = 4;

implementation

uses
  math;


function CreateDTMPoint(x,y,c,t,asz : integer; bp : boolean) : TMDTMPoint;
begin
  result.x := x;
  result.y := y;
  result.c := c;
  result.t := t;
  result.asz := asz;
  result.bp := bp;
end;

function MDTMToSDTM(Const DTM: TMDTM): TSDTM;
var
   I, H: Integer;
begin
  with Result.MainPoint do
  begin
    X := DTM.Points[0].x;
    Y := DTM.Points[0].y;
    AreaSize := DTM.Points[0].asz;
    AreaShape := 0;
    Color := DTM.Points[0].c;
    Tolerance := DTM.Points[0].t;
  end;
  H := DTM.Count - 1;
  SetLength(Result.SubPoints, H);

  for I := 1 to H do
  begin
    FillChar(Result.SubPoints[I - 1], SizeOf(Result.SubPoints[I - 1]), 0);

    with Result.SubPoints[I - 1] do
    begin
      X := DTM.Points[i].x;
      Y := DTM.Points[i].y;
      AreaSize := DTM.Points[i].asz;
      AreaShape := 0;
      Color := DTM.Points[i].c;
      Tolerance := DTM.Points[i].t;
    end;
  end;
end;

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

function ValidMainPointBox(var dtm: TMDTM; const x1, y1, x2, y2: Integer): TBox;

var
   i: Integer;
   b: TBox;

begin
  dtm.Normalize;

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
   d:extended;

begin
  dtm.normalize;

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

