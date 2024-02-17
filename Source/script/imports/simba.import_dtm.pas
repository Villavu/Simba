unit simba.import_dtm;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportDTM(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.dtm;

(*
DTM
===
DTM related methods

Image:: ../../images/dtm.png
*)

(*
TDTM.PointCount
---------------
> function TDTM.PointCount: Integer;
*)
procedure _LapeDTM_PointCount(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointCount;
end;

(*
TDTM.DeletePoints
-----------------
> procedure TDTM.DeletePoints;
*)
procedure _LapeDTM_DeletePoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.DeletePoints();
end;

(*
TDTM.DeletePoint
-----------------
> procedure TDTM.DeletePoint(Index: Integer);
*)
procedure _LapeDTM_DeletePoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.DeletePoint(PInteger(Params^[1])^);
end;

(*
TDTM.AddPoint
--------------
> procedure TDTM.AddPoint(Point: TDTMPoint);
*)
procedure _LapeDTM_AddPoint1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.AddPoint(PDTMPoint(Params^[1])^);
end;

(*
TDTM.AddPoint
--------------
> procedure TDTM.AddPoint(X, Y, Color, Tolerance, AreaSize: Integer);
*)
procedure _LapeDTM_AddPoint2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.AddPoint(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

(*
TDTM.ToString
-------------
> function TDTM.ToString: String;
*)
procedure _LapeDTM_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDTM(Params^[0])^.ToString();
end;

(*
TDTM.FromString
---------------
> procedure TDTM.FromString(Str: String);
*)
procedure _LapeDTM_FromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.FromString(PString(Params^[1])^);
end;

(*
TDTM.MovePoint
--------------
> procedure TDTM.MovePoint(AFrom, ATo: Integer);
*)
procedure _LapeDTM_MovePoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.MovePoint(PInteger(Params^[1])^, PInteger(Params^[1])^);
end;

(*
TDTM.Normalize
---------------
> procedure TDTM.Normalize;
*)
procedure _LapeDTM_Normalize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.Normalize();
end;

procedure ImportDTM(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'DTM';

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
    addGlobalFunc('procedure TDTM.AddPoint(X, Y, Color, Tolerance, AreaSize: Integer); overload', @_LapeDTM_AddPoint2);
    addGlobalFunc('procedure TDTM.DeletePoints', @_LapeDTM_DeletePoints);
    addGlobalFunc('procedure TDTM.DeletePoint(Index: Integer);', @_LapeDTM_DeletePoint);
    addGlobalFunc('procedure TDTM.MovePoint(AFrom, ATo: Integer);', @_LapeDTM_MovePoint);
    addGlobalFunc('procedure TDTM.Normalize;', @_LapeDTM_Normalize);
    addGlobalFunc('function TDTM.Valid: Boolean;', @_LapeDTM_Normalize);

    ImportingSection := '';
  end;
end;

end.
