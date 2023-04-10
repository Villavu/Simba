unit simba.import_math;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, math,
  simba.script_compiler, simba.mufasatypes, simba.math, simba.geometry;

procedure _LapeDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Distance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeDistanceEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Distance(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeFixD(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := FixD(PDouble(Params^[0])^);
end;

procedure _LapeLogn(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := logn(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

procedure _LapeSar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Plongint(Result)^ := SarLongint(Plongint(Params^[0])^, PByte(Params^[1])^);
end;

procedure _LapeRor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLongWord(Result)^ := RorDWord(Plongword(Params^[0])^, PByte(Params^[1])^);
end;

procedure _LapeRol(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLongWord(Result)^ := RolDWord(Plongword(Params^[0])^, PByte(Params^[1])^);
end;

procedure _LapeRadians(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Radians(PDouble(Params^[0])^);
end;

procedure _LapeDegrees(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Degrees(PDouble(Params^[0])^);
end;

procedure _LapeLog2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Log2(PDouble(Params^[0])^);
end;

procedure _LapeLog10(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Log10(PDouble(Params^[0])^);
end;

procedure _LapeFixRad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := FixRad(PDouble(Params^[0])^);
end;

procedure _LapeNextPower2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := NextPower2(PInteger(Params^[0])^);
end;

procedure _LapeModulo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Modulo(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeModuloF(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := Modulo(PDouble(Params^[0])^, PDouble(Params^[1])^);
end;

procedure _LapeDeltaAngle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.DeltaAngle(PDouble(Params^[0])^, PDouble(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeExpandPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := TSimbaGeometry.ExpandPolygon(PPointArray(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapePolygonArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.PolygonArea(PPointArray(Params^[0])^);
end;

procedure _LapeCrossProduct1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := TSimbaGeometry.CrossProduct(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeCrossProduct2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.CrossProduct(PDouble(Params^[0])^, PDouble(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDouble(Params^[5])^);
end;

procedure _LapeLinesIntersect1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.LinesIntersect(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

procedure _LapeLinesIntersect2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.LinesIntersect(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^, PPoint(Params^[4])^);
end;

procedure _LapeDistToLine1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.DistToLine(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

procedure _LapeDistToLine2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := TSimbaGeometry.DistToLine(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapePointInTriangle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInTriangle(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

procedure _LapePointInBox(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapePointInQuad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInQuad(PPoint(Params^[0])^, PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^, PPoint(Params^[4])^);
end;

procedure _LapePointInPolygon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInPolygon(PPoint(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure _LapePointInCircle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInCircle(PPoint(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapePointInEllipse(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaGeometry.PointInEllipse(PPoint(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^);
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

    addGlobalFunc('function Distance(const X1, Y1, X2, Y2: Integer): Integer; overload', @_LapeDistance);
    addGlobalFunc('function Distance(const P1, P2: TPoint): Integer; overload', @_LapeDistanceEx);
    addGlobalFunc('function FixD(Deg: Double): Double', @_LapeFixD);
    addGlobalFunc('function Sar(x: Integer; Shift: Byte): Integer', @_LapeSar);
    addGlobalFunc('function Ror(x: UInt32; Shift: Byte): UInt32', @_LapeRor);
    addGlobalFunc('function Rol(x: UInt32; Shift: Byte): UInt32', @_LapeRol);
    addGlobalFunc('function Radians(Deg: Double): Double', @_LapeRadians);
    addGlobalFunc('function Degrees(Rad: Double): Double', @_LapeDegrees);
    addGlobalFunc('function LogN(base, x: Double): Double', @_LapeLogn);
    addGlobalFunc('function Log2(x: Double): Double', @_LapeLog2);
    addGlobalFunc('function Log10(x: Double): Double', @_LapeLog10);
    addGlobalFunc('function FixRad(Rad: Double): Double', @_LapeFixRad);
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

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMath);

end.

