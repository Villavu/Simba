unit simba.import_dtm;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportDTM(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.dtm;

(*
DTM
===
DTM related methods

![dtm](../../images/dtm.png)

The first point is the main one: a match is where it is, and the other points are placed from it.
*)

(*
TDTM.PointCount
---------------
```
function TDTM.PointCount: Integer;
```
*)
procedure _LapeDTM_PointCount(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointCount;
end;

(*
TDTM.DeletePoints
-----------------
```
procedure TDTM.DeletePoints;
```
*)
procedure _LapeDTM_DeletePoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.DeletePoints();
end;

(*
TDTM.DeletePoint
-----------------
```
procedure TDTM.DeletePoint(Index: Integer);
```
*)
procedure _LapeDTM_DeletePoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.DeletePoint(PInteger(Params^[1])^);
end;

(*
TDTM.AddPoint
--------------
```
procedure TDTM.AddPoint(Point: TDTMPoint);
```
*)
procedure _LapeDTM_AddPoint1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.AddPoint(PDTMPoint(Params^[1])^);
end;

(*
TDTM.AddPoint
--------------
```
procedure TDTM.AddPoint(X, Y, Color: Integer; Tolerance: Single; AreaSize: Integer);
```
*)
procedure _LapeDTM_AddPoint2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.AddPoint(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PSingle(Params^[4])^, PInteger(Params^[5])^);
end;

(*
TDTM.ToString
-------------
```
function TDTM.ToString: String;
```
*)
procedure _LapeDTM_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDTM(Params^[0])^.ToString();
end;

(*
TDTM.FromString
---------------
```
procedure TDTM.FromString(Str: String);
```
Raises for a string that is not a DTM, and the DTM is left as it was.
*)
procedure _LapeDTM_FromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.FromString(PString(Params^[1])^);
end;

(*
TDTM.MovePoint
--------------
```
procedure TDTM.MovePoint(AFrom, ATo: Integer);
```
*)
procedure _LapeDTM_MovePoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.MovePoint(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportDTM(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'DTM';

    addGlobalType([
      'record',
      '  X, Y: Integer;',
      '  Color: Integer;',
      '  Tolerance: Single;',
      '  AreaSize: Integer;',
      'end;'],
      'TDTMPoint'
    );

    addGlobalType('array of TDTMPoint', 'TDTMPointArray');
    addGlobalType('record Points: TDTMPointArray; end;', 'TDTM');

    addGlobalFunc('procedure TDTM.FromString(Str: String)', @_LapeDTM_FromString);
    addGlobalFunc('function TDTM.ToString: String', @_LapeDTM_ToString);
    addGlobalFunc('function TDTM.PointCount: Integer', @_LapeDTM_PointCount);
    addGlobalFunc('procedure TDTM.AddPoint(Point: TDTMPoint); overload', @_LapeDTM_AddPoint1);
    addGlobalFunc('procedure TDTM.AddPoint(X, Y, Color: Integer; Tolerance: Single; AreaSize: Integer); overload', @_LapeDTM_AddPoint2);
    addGlobalFunc('procedure TDTM.DeletePoints', @_LapeDTM_DeletePoints);
    addGlobalFunc('procedure TDTM.DeletePoint(Index: Integer);', @_LapeDTM_DeletePoint);
    addGlobalFunc('procedure TDTM.MovePoint(AFrom, ATo: Integer);', @_LapeDTM_MovePoint);

    DumpSection := '';
  end;
end;

end.
