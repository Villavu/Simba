unit simba.import_math;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportMath(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, math,
  simba.math, simba.geometry;

(*
Math
====
Math functions
*)

(*
Distance
--------
> function Distance(const X1, Y1, X2, Y2: Double): Double;
*)
procedure _LapeDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Distance(PDouble(Params^[0])^, PDouble(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
Distance
--------
> function Distance(const P1, P2: TPoint): Double;
*)
procedure _LapeDistanceEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Distance(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

(*
LogN
----
> function LogN(base, x: Double): Double;
*)
procedure _LapeLogn(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := logn(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

(*
Sar
---
> function Sar(x: Integer; Shift: Byte): Integer;
*)
procedure _LapeSar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Plongint(Result)^ := SarLongint(Plongint(Params^[0])^, PByte(Params^[1])^);
end;

(*
Ror
---
> function Ror(x: UInt32; Shift: Byte): UInt32;
*)
procedure _LapeRor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLongWord(Result)^ := RorDWord(Plongword(Params^[0])^, PByte(Params^[1])^);
end;

(*
Rol
---
> function Rol(x: UInt32; Shift: Byte): UInt32;
*)
procedure _LapeRol(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLongWord(Result)^ := RolDWord(Plongword(Params^[0])^, PByte(Params^[1])^);
end;

(*
DegToRad
--------
> function DegToRad(Deg: Double): Double;
*)
procedure _LapeDegToRad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := DegToRad(PDouble(Params^[0])^);
end;

(*
RadToDeg
--------
> function RadToDeg(Rad: Double): Double;
*)
procedure _LapeRadToDeg(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := RadToDeg(PDouble(Params^[0])^);
end;

(*
RadNormalize
------------
> function RadNormalize(Rad: Double): Double;
*)
procedure _LapeRadNormalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := DegToRad(DegNormalize(RadToDeg(PDouble(Params^[0])^)));
end;

(*
DegNormalize
------------
> function DegNormalize(Deg: Double): Double;
*)
procedure _LapeDegNormalize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := DegNormalize(PDouble(Params^[0])^);
end;

(*
Log2
----
> function Log2(x: Double): Double;
*)
procedure _LapeLog2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Log2(PDouble(Params^[0])^);
end;

(*
Log10
-----
> function Log10(x: Double): Double;
*)
procedure _LapeLog10(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Log10(PDouble(Params^[0])^);
end;

(*
NextPower2
----------
> function NextPower2(const n: Integer): Integer;
*)
procedure _LapeNextPower2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := NextPower2(PInteger(Params^[0])^);
end;

(*
Modulo
------
> function Modulo(const X, Y: Integer): Integer;
*)
procedure _LapeModulo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Modulo(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
Modulo
------
> function Modulo(const X, Y: Double): Double;
*)
procedure _LapeModuloF(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Modulo(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

(*
DeltaAngle
----------
> function DeltaAngle(const DegreesA, DegreesB: Double; R: Double = 360): Double;
*)
procedure _LapeDeltaAngle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.DeltaAngle(PDouble(Params^[0])^, PDouble(Params^[1])^, PDouble(Params^[2])^);
end;

(*
ExpandPolygon
-------------
> function ExpandPolygon(const Polygon: TPointArray; Amount: Integer): TPointArray;
*)
procedure _LapeExpandPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TSimbaGeometry.ExpandPolygon(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

(*
PolygonArea
-----------
> function PolygonArea(const Polygon: TPointArray): Double;
*)
procedure _LapePolygonArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.PolygonArea(PPointArray(Params^[0])^);
end;

(*
CrossProduct
------------
> function CrossProduct(const r, p, q: TPoint): Int64;
*)
procedure _LapeCrossProduct1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaGeometry.CrossProduct(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

(*
CrossProduct
------------
> function CrossProduct(const rx,ry, px,py, qx,qy: Double): Double;
*)
procedure _LapeCrossProduct2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.CrossProduct(PDouble(Params^[0])^, PDouble(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

(*
LinesIntersect
--------------
> function LinesIntersect(const P1, P2, Q1, Q2: TPoint): Boolean;
*)
procedure _LapeLinesIntersect1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.LinesIntersect(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

(*
LinesIntersect
--------------
> function LinesIntersect(const P1, P2, Q1, Q2: TPoint; out Where: TPoint): Boolean;
*)
procedure _LapeLinesIntersect2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.LinesIntersect(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^, PPoint(Params^[4])^);
end;

(*
DistToLine
----------
> function DistToLine(const P, P1, P2: TPoint; out Nearest: TPoint): Double;
*)
procedure _LapeDistToLine1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.DistToLine(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

(*
DistToLine
----------
> function DistToLine(const P, P1, P2: TPoint): Double;
*)
procedure _LapeDistToLine2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.DistToLine(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

(*
PointInTriangle
---------------
> function PointInTriangle(const P, P1, P2, P3: TPoint): Boolean;
*)
procedure _LapePointInTriangle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInTriangle(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

(*
PointInBox
----------
> function PointInBox(const P: TPoint; const Box: TBox): Boolean;
*)
procedure _LapePointInBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

(*
PointInQuad
-----------
> function PointInQuad(const P: TPoint; const A,B,C,D: TPoint): Boolean;
*)
procedure _LapePointInQuad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInQuad(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^, PPoint(Params^[4])^);
end;

(*
PointInPolygon
--------------
> function PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean;
*)
procedure _LapePointInPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInPolygon(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

(*
PointInCircle
-------------
> function PointInCircle(const P, Center: TPoint; Radius: Double): Boolean; static;
*)
procedure _LapePointInCircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInCircle(PPoint(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^);
end;

(*
PointInEllipse
--------------
> function PointInEllipse(const P, Center: TPoint; const YRadius, XRadius: Double): Boolean;
*)
procedure _LapePointInEllipse(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInEllipse(PPoint(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
end;

(*
IsNumber
--------
> function IsNumber(const Value: Single): Boolean;
*)
procedure _LapeIsNumberS(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := IsNumber(PSingle(Params^[0])^);
end;

(*
IsNumber
--------
> function IsNumber(const Value: Double): Boolean;
*)
procedure _LapeIsNumberD(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := IsNumber(PDouble(Params^[0])^);
end;

procedure ImportMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Math';

    addGlobalVar(HALF_PI, 'HALF_PI').isConstant := True;
    addGlobalVar(SQRT_2, 'SQRT_2').isConstant := True;
    addGlobalVar(SQRT_3, 'SQRT_3').isConstant := True;
    addGlobalVar(SQRT_5, 'SQRT_5').isConstant := True;

    addGlobalFunc('function IsNumber(const Value: Single): Boolean; overload', @_LapeIsNumberS);
    addGlobalFunc('function IsNumber(const Value: Double): Boolean; overload', @_LapeIsNumberD);

    addGlobalFunc('function Distance(const X1, Y1, X2, Y2: Double): Double; overload', @_LapeDistance);
    addGlobalFunc('function Distance(const P1, P2: TPoint): Double; overload', @_LapeDistanceEx);

    addGlobalFunc('function Sar(x: Integer; Shift: Byte): Integer', @_LapeSar);
    addGlobalFunc('function Ror(x: UInt32; Shift: Byte): UInt32', @_LapeRor);
    addGlobalFunc('function Rol(x: UInt32; Shift: Byte): UInt32', @_LapeRol);

    addGlobalFunc('function LogN(base, x: Double): Double', @_LapeLogN);
    addGlobalFunc('function Log2(x: Double): Double', @_LapeLog2);
    addGlobalFunc('function Log10(x: Double): Double', @_LapeLog10);

    addGlobalFunc('function DegToRad(Deg: Double): Double', @_LapeDegToRad);
    addGlobalFunc('function RadToDeg(Rad: Double): Double', @_LapeRadToDeg);
    addGlobalFunc('function RadNormalize(Rad: Double): Double', @_LapeRadNormalize);
    addGlobalFunc('function DegNormalize(Deg: Double): Double', @_LapeDegNormalize);

    addGlobalFunc('function NextPower2(const n: Integer): Integer', @_LapeNextPower2);

    addGlobalFunc('function Modulo(const X, Y: Integer): Integer; overload', @_LapeModulo);
    addGlobalFunc('function Modulo(const X, Y: Double): Double; overload', @_LapeModuloF);

    addGlobalFunc('function DeltaAngle(const DegreesA, DegreesB: Double; R: Double = 360): Double', @_LapeDeltaAngle);
    addGlobalFunc('function PolygonArea(const Polygon: TPointArray): Double', @_LapePolygonArea);
    addGlobalFunc('function ExpandPolygon(const Polygon: TPointArray; Amount: Integer): TPointArray', @_LapeExpandPolygon);
    addGlobalFunc('function CrossProduct(const r, p, q: TPoint): Int64; overload', @_LapeCrossProduct1);
    addGlobalFunc('function CrossProduct(const rx,ry, px,py, qx,qy: Double): Double; overload', @_LapeCrossProduct2);
    addGlobalFunc('function LinesIntersect(const P1, P2, Q1, Q2: TPoint): Boolean; overload', @_LapeLinesIntersect1);
    addGlobalFunc('function LinesIntersect(const P1, P2, Q1, Q2: TPoint; out Where: TPoint): Boolean; overload', @_LapeLinesIntersect2);

    addGlobalFunc('function DistToLine(const P, P1, P2: TPoint; out Nearest: TPoint): Double; overload', @_LapeDistToLine1);
    addGlobalFunc('function DistToLine(const P, P1, P2: TPoint): Double; overload', @_LapeDistToLine2);

    addGlobalFunc('function PointInTriangle(const P, P1, P2, P3: TPoint): Boolean', @_LapePointInTriangle);
    addGlobalFunc('function PointInBox(const P: TPoint; const Box: TBox): Boolean', @_LapePointInBox);
    addGlobalFunc('function PointInQuad(const P: TPoint; const A,B,C,D: TPoint): Boolean', @_LapePointInQuad);
    addGlobalFunc('function PointInPolygon(const P: TPoint; const Polygon: TPointArray): Boolean', @_LapePointInPolygon);
    addGlobalFunc('function PointInCircle(const P, Center: TPoint; Radius: Double): Boolean; static', @_LapePointInCircle);
    addGlobalFunc('function PointInEllipse(const P, Center: TPoint; const YRadius, XRadius: Double): Boolean', @_LapePointInEllipse);

    ImportingSection := '';
  end;
end;

end.

