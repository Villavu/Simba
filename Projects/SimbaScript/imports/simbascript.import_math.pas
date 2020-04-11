unit simbascript.import_math;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Math(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.math, math;

procedure Lape_pow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pextended(Result)^ := pow(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_RiemannGauss(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := RiemannGauss(PExtended(Params^[1])^, PExtended(Params^[2])^, PExtended(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_DiscreteGauss(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtendedArray(Result)^ := DiscreteGauss(PInt32(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_GaussMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DExtendedArray(Result)^ := GaussMatrix(PInt32(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_Point(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Point(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_Distance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Distance(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_Hypot(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Hypot(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_RandomRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RandomRange(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_RandomE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Random();
end;

procedure Lape_ArcTan2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcTan2(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_IncEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  IncEx(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_DecEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DecEx(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_Factorial(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Factorial(Plongword(Params^[1])^);
end;

procedure Lape_BinCoe(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := BinCoe(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure Lape_FixD(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixD(PExtended(Params^[1])^);
end;

procedure Lape_InRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := InRange(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_IntToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := IntToBox(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_IntInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IntInBox(PInt32(Params^[1])^, PInt32(Params^[2])^, PBox(Params^[3])^);
end;

procedure Lape_PointToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PointToBox(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

procedure Lape_PointInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInBox(PPoint(Params^[1])^, PBox(Params^[2])^);
end;

procedure Lape_logn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := logn(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_sar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Plongint(Result)^ := sar(Plongint(Params^[1])^, Pbyte(Params^[2])^);
end;

procedure Lape_ror(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := ror(Plongword(Params^[1])^, Pbyte(Params^[2])^);
end;

procedure Lape_rol(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := rol(Plongword(Params^[1])^, Pbyte(Params^[2])^);
end;

procedure Lape_tan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := tan(PExtended(Params^[1])^);
end;

procedure Lape_radians(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := radians(PExtended(Params^[1])^);
end;

procedure Lape_degrees(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := degrees(PExtended(Params^[1])^);
end;

procedure Lape_ArcSin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcSin(PExtended(Params^[1])^);
end;

procedure Lape_ArcCos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcCos(PExtended(Params^[1])^);
end;

procedure Lape_Cotan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cotan(PExtended(Params^[1])^);
end;

procedure Lape_Secant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Secant(PExtended(Params^[1])^);
end;

procedure Lape_Cosecant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cosecant(PExtended(Params^[1])^);
end;

procedure Lape_Cot(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cot(PExtended(Params^[1])^);
end;

procedure Lape_Sec(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sec(PExtended(Params^[1])^);
end;

procedure Lape_Csc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Csc(PExtended(Params^[1])^);
end;

procedure Lape_Cosh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cosh(PExtended(Params^[1])^);
end;

procedure Lape_Sinh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sinh(PExtended(Params^[1])^);
end;

procedure Lape_Tanh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Tanh(PExtended(Params^[1])^);
end;

procedure Lape_CotH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := cotan(PExtended(Params^[1])^);
end;

procedure Lape_SecH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := secant(PExtended(Params^[1])^);
end;

procedure Lape_CscH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Csc(PExtended(Params^[1])^);
end;

procedure Lape_ArcCosh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcCosh(PExtended(Params^[1])^);
end;

procedure Lape_ArcSinh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcSinh(PExtended(Params^[1])^);
end;

procedure Lape_DecRet(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := DecRet(PExtended(Params^[1])^);
end;

procedure Lape_log10(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := log10(PExtended(Params^[1])^);
end;

procedure Lape_MaxA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := MaxA(TIntegerArray(Params^[1]^));
end;

procedure Lape_MinA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := MinA(TIntegerArray(Params^[1]^));
end;

procedure Lape_Sum64IntArr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Sum64IntArr(PIntegerArray(Params^[1])^);
end;

procedure Lape_FixRad(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixRad(PExtended(Params^[1])^);
end;

procedure Lape_InAbstractBox(const Params : PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InAbstractBox(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^, PInt32(Params^[10])^);
end;

procedure Lape_MiddleBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleBox(PBox(Params^[1])^);
end;

procedure Lape_Import_Math(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Math';

    addGlobalMethod('function Pow(base, exponent: Extended): Extended', @Lape_pow, Data);
    addGlobalMethod('function RiemannGauss(Xstart, StepSize, Sigma: Extended; AmountSteps: Int32): Extended', @Lape_RiemannGauss, Data);
    addGlobalMethod('function DiscreteGauss(Xstart, Xend: Int32; sigma: Extended): TExtendedArray', @Lape_DiscreteGauss, Data);
    addGlobalMethod('function GaussMatrix(N: Int32; sigma: Extended): T2DExtendedArray', @Lape_GaussMatrix, Data);
    addGlobalMethod('function Point(x, y: Int32): TPoint', @Lape_Point, Data);
    addGlobalMethod('function Distance(x1, y1, x2, y2: Int32): Int32', @Lape_Distance, Data);
    addGlobalMethod('function RandomRange(const aFrom, aTo: Int32): Int32', @Lape_RandomRange, Data);
    addGlobalMethod('function RandomE: Extended', @Lape_RandomE, Data);
    addGlobalMethod('procedure IncEx(var x: Int32; increase: Int32);', @Lape_IncEx, Data);
    addGlobalMethod('procedure DecEx(var x: Int32; Decrease: Int32);', @Lape_DecEx, Data);
    addGlobalMethod('function Factorial(number: longword): Int64', @Lape_Factorial, Data);
    addGlobalMethod('function BinCoe(a, b: LongInt): Extended', @Lape_BinCoe, Data);
    addGlobalMethod('function FixD(Degrees: Extended): Extended', @Lape_FixD, Data);
    addGlobalMethod('function InRange(const Value, Min, Max: Int32): boolean', @Lape_InRange, Data);
    addGlobalMethod('function IntToBox(x1, y1, x2, y2: Int32): TBox', @Lape_IntToBox, Data);
    addGlobalMethod('function IntInBox(x, y: Int32; Box: TBox): Boolean', @Lape_IntInBox, Data);
    addGlobalMethod('function PointToBox(PT1, PT2: TPoint): TBox', @Lape_PointToBox, Data);
    addGlobalMethod('function PointInBox(PT: TPoint; Box: TBox): Boolean', @Lape_PointInBox, Data);
    addGlobalMethod('function logn(base, x: Extended): Extended', @Lape_logn, Data);
    addGlobalMethod('function sar(AValue: longint; shift: byte): longint', @Lape_sar, Data);
    addGlobalMethod('function ror(num: longword; shift: byte): LongWord', @Lape_ror, Data);
    addGlobalMethod('function rol(num: longword; shift: byte): LongWord', @Lape_rol, Data);
    addGlobalMethod('function radians(e: Extended): Extended', @Lape_radians, Data);
    addGlobalMethod('function degrees(e: Extended): Extended', @Lape_degrees, Data);
    addGlobalMethod('function Cot(e: Extended): Extended', @Lape_Cot, Data);
    addGlobalMethod('function Sec(e: Extended): Extended', @Lape_Sec, Data);
    addGlobalMethod('function Csc(e: Extended): Extended', @Lape_Csc, Data);
    addGlobalMethod('function CotH(e: Extended): Extended', @Lape_CotH, Data);
    addGlobalMethod('function SecH(e: Extended): Extended', @Lape_SecH, Data);
    addGlobalMethod('function CscH(e: Extended): Extended', @Lape_CscH, Data);
    addGlobalMethod('function DecRet(e: Extended): Extended', @Lape_DecRet, Data);
    addGlobalMethod('function log10(f: Extended): Extended', @Lape_log10, Data);
    addGlobalMethod('function MinA(Arr: TIntegerArray): Int32', @Lape_MinA, Data);
    addGlobalMethod('function MaxA(Arr: TIntegerArray): Int32', @Lape_MaxA, Data);
    addGlobalMethod('function Sum64IntArr(const Arr: TIntegerArray): Int64;', @Lape_Sum64IntArr, Data);
    addGlobalMethod('function FixRad(rad: Extended): Extended;', @Lape_FixRad, Data);
    addGlobalMethod('function InAbstractBox(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Int32; X, Y: Int32): Boolean;', @Lape_InAbstractBox, Data);
    addGlobalMethod('function MiddleBox(b: TBox): TPoint', @Lape_MiddleBox, Data);
  end;
end;

end.

