unit dtmutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes;


Function pDTMToTDTM(Const DTM: pDTM): TDTM;
Function tDTMTopDTM(Const DTM: TDTM): pDTM;


implementation


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

