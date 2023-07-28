unit simba.import_class_dtm;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportDTM(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.dtm;

(*
DTM
===
DTM related methods
*)

(*
TDTM.GetName
~~~~~~~~~~~~~
function TDTM.GetName: String;
*)
procedure _LapeDTM_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDTM(Params^[0])^.Name;
end;

(*
TDTM.SetName
~~~~~~~~~~~~~
procedure TDTM.SetName(Value: String);
*)
procedure _LapeDTM_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.Name := PString(Params^[1])^;
end;

(*
TDTM.FreeOnTerminate
~~~~~~~~~~~~~~~~~~~~~
procedure TDTM.FreeOnTerminate(Value: Boolean);
*)
procedure _LapeDTM_FreeOnTerminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

(*
TDTM.CreateFromString
~~~~~~~~~~~~~~~~~~~~~~
function TDTM.CreateFromString(DTMString: String): TDTM; static;
*)
procedure _LapeDTM_CreateFromString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Result)^ := TDTM.CreateFromString(PString(Params^[0])^);
end;

(*
TDTM.Create
~~~~~~~~~~~~
function TDTM.Create: TDTM; static;
*)
procedure _LapeDTM_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Result)^ := TDTM.Create();
end;

(*
TDTM.GetPoints
~~~~~~~~~~~~~~~
function TDTM.GetPoints: TDTMPointArray;
*)
procedure _LapeDTM_Points_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDTMPointArray(Result)^ := PDTM(Params^[0])^.Points;
end;

(*
TDTM.GetPointCount
~~~~~~~~~~~~~~~~~~~
function TDTM.GetPointCount: Integer;
*)
procedure _LapeDTM_PointCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointCount;
end;

(*
TDTM.GetPointColor
~~~~~~~~~~~~~~~~~~~
function TDTM.GetPointColor: Integer;
*)
procedure _LapeDTM_PointColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointColor[PInteger(Params^[1])^];
end;

(*
TDTM.SetPointColor
~~~~~~~~~~~~~~~~~~~
procedure TDTM.SetPointColor(Value: Integer);
*)
procedure _LapeDTM_PointColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.PointColor[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

(*
TDTM.GetPointTolerance
~~~~~~~~~~~~~~~~~~~~~~~
function TDTM.GetPointTolerance: Integer;
*)
procedure _LapeDTM_PointTolerance_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointTolerance[PInteger(Params^[1])^];
end;

(*
TDTM.SetPointTolerance
~~~~~~~~~~~~~~~~~~~~~~~
procedure TDTM.SetPointTolerance(Value: Integer);
*)
procedure _LapeDTM_PointTolerance_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.PointTolerance[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

(*
TDTM.GetPointX
~~~~~~~~~~~~~~~
function TDTM.GetPointX: Integer;
*)
procedure _LapeDTM_PointX_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointX[PInteger(Params^[1])^];
end;

(*
TDTM.SetPointX
~~~~~~~~~~~~~~~
procedure TDTM.SetPointX(Value: Integer);
*)
procedure _LapeDTM_PointX_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.PointX[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

(*
TDTM.GetPointY
~~~~~~~~~~~~~~~
function TDTM.GetPointY: Integer;
*)
procedure _LapeDTM_PointY_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointY[PInteger(Params^[1])^];
end;

(*
TDTM.SetPointY
~~~~~~~~~~~~~~~
procedure TDTM.SetPointY(Value: Integer);
*)
procedure _LapeDTM_PointY_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.PointY[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

(*
TDTM.GetPointAreaSize
~~~~~~~~~~~~~~~~~~~~~~
function TDTM.GetPointAreaSize: Integer;
*)
procedure _LapeDTM_PointAreaSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDTM(Params^[0])^.PointAreaSize[PInteger(Params^[1])^];
end;

(*
TDTM.SetPointAreaSize
~~~~~~~~~~~~~~~~~~~~~~
procedure TDTM.SetPointAreaSize(Value: Integer);
*)
procedure _LapeDTM_PointAreaSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.PointAreaSize[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

(*
TDTM.DeletePoints
~~~~~~~~~~~~~~~~~~
procedure TDTM.DeletePoints;
*)
procedure _LapeDTM_DeletePoints(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.DeletePoints();
end;

(*
TDTM.DeletePoint
~~~~~~~~~~~~~~~~~
procedure TDTM.DeletePoint(Index: Integer);
*)
procedure _LapeDTM_DeletePoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.DeletePoint(PInteger(Params^[1])^);
end;

(*
TDTM.AddPoint
~~~~~~~~~~~~~~
procedure TDTM.AddPoint(Point: TDTMPoint);
*)
procedure _LapeDTM_AddPoint1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.AddPoint(PDTMPoint(Params^[1])^);
end;

(*
TDTM.AddPoint
~~~~~~~~~~~~~~
procedure TDTM.AddPoint(X, Y, Color, Tolerance, AreaSize: Integer);
*)
procedure _LapeDTM_AddPoint2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.AddPoint(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

(*
TDTM.SaveToString
~~~~~~~~~~~~~~~~~~
function TDTM.SaveToString: String;
*)
procedure _LapeDTM_SaveToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDTM(Params^[0])^.SaveToString();
end;

(*
TDTM.LoadFromString
~~~~~~~~~~~~~~~~~~~~
procedure TDTM.LoadFromString(Str: String);
*)
procedure _LapeDTM_LoadFromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.LoadFromString(PString(Params^[1])^);
end;

(*
TDTM.SwapPoint
~~~~~~~~~~~~~~~
procedure TDTM.SwapPoint(Index1, Index2: Integer);
*)
procedure _LapeDTM_SwapPoint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDTM(Params^[0])^.SwapPoint(PInteger(Params^[1])^, PInteger(Params^[1])^);
end;

(*
TDTM.Normalize
~~~~~~~~~~~~~~~
procedure TDTM.Normalize;
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

    addClass('TDTM');

    addGlobalType('record X, Y, Color, Tolerance, AreaSize: Integer; end', 'TDTMPoint');
    addGlobalType('array of TDTMPoint', 'TDTMPointArray');

    addClassVar('TDTM', 'Name', 'String', @_LapeDTM_Name_Read, @_LapeDTM_Name_Write);
    addClassVar('TDTM', 'PointCount', 'Integer', @_LapeDTM_PointCount_Read);
    addClassVar('TDTM', 'PointColor', 'Integer', @_LapeDTM_PointColor_Read, @_LapeDTM_PointColor_Write, True);
    addClassVar('TDTM', 'PointTolerance', 'Integer', @_LapeDTM_PointTolerance_Read, @_LapeDTM_PointTolerance_Write, True);
    addClassVar('TDTM', 'PointX', 'Integer', @_LapeDTM_PointX_Read, @_LapeDTM_PointX_Write, True);
    addClassVar('TDTM', 'PointY', 'Integer', @_LapeDTM_PointY_Read, @_LapeDTM_PointY_Write, True);
    addClassVar('TDTM', 'PointAreaSize', 'Integer', @_LapeDTM_PointAreaSize_Read, @_LapeDTM_PointAreaSize_Write, True);
    addClassVar('TDTM', 'Points', 'TDTMPointArray', @_LapeDTM_Points_Read);

    addGlobalFunc('function TDTM.Create: TDTM; static;', @_LapeDTM_Create);
    addGlobalFunc('function TDTM.CreateFromString(DTMString: String): TDTM; static;', @_LapeDTM_CreateFromString);
    addGlobalFunc('function TDTM.SaveToString: String', @_LapeDTM_SaveToString);
    addGlobalFunc('procedure TDTM.LoadFromString(Str: String);', @_LapeDTM_LoadFromString);
    addGlobalFunc('procedure TDTM.DeletePoints', @_LapeDTM_DeletePoints);
    addGlobalFunc('procedure TDTM.DeletePoint(Index: Integer);', @_LapeDTM_DeletePoint);
    addGlobalFunc('procedure TDTM.AddPoint(Point: TDTMPoint); overload', @_LapeDTM_AddPoint1);
    addGlobalFunc('procedure TDTM.AddPoint(X, Y, Color, Tolerance, AreaSize: Integer); overload', @_LapeDTM_AddPoint2);
    addGlobalFunc('procedure TDTM.FreeOnTerminate(Value: Boolean);', @_LapeDTM_FreeOnTerminate);
    addGlobalFunc('procedure TDTM.SwapPoint(Index1, Index2: Integer);', @_LapeDTM_SwapPoint);
    addGlobalFunc('procedure TDTM.Normalize;', @_LapeDTM_Normalize);

    ImportingSection := '';
  end;
end;

end.

