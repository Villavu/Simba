unit script_import_math;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, lpcompiler, lptypes, mufasatypes, mmath,
  math;

procedure Lape_pow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pextended(Result)^ := pow(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_RiemannGauss(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := RiemannGauss(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_DiscreteGauss(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtendedArray(Result)^ := DiscreteGauss(PInt32(Params^[0])^, PInt32(Params^[1])^, PExtended(Params^[2])^);
end;

procedure Lape_GaussMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DExtendedArray(Result)^ := GaussMatrix(PInt32(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_Max(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Max(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_Min(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Min(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_MinE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Min(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_MaxE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Max(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_Point(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Point(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_Distance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Distance(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_Hypot(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Hypot(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_RandomRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := RandomRange(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_RandomE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Random();
end;

procedure Lape_ArcTan2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcTan2(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_IncEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  IncEx(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_DecEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DecEx(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_Factorial(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Factorial(Plongword(Params^[0])^);
end;

procedure Lape_BinCoe(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := BinCoe(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure Lape_FixD(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixD(PExtended(Params^[0])^);
end;

procedure Lape_InRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := InRange(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_IntToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := IntToBox(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_IntInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IntInBox(PInt32(Params^[0])^, PInt32(Params^[1])^, PBox(Params^[2])^);
end;

procedure Lape_PointToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PointToBox(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_PointInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

procedure Lape_logn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := logn(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_sar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Plongint(Result)^ := sar(Plongint(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure Lape_ror(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := ror(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure Lape_rol(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := rol(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure Lape_tan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := tan(PExtended(Params^[0])^);
end;

procedure Lape_radians(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := radians(PExtended(Params^[0])^);
end;

procedure Lape_degrees(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := degrees(PExtended(Params^[0])^);
end;

procedure Lape_ArcSin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcSin(PExtended(Params^[0])^);
end;

procedure Lape_ArcCos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcCos(PExtended(Params^[0])^);
end;

procedure Lape_Cotan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cotan(PExtended(Params^[0])^);
end;

procedure Lape_Secant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Secant(PExtended(Params^[0])^);
end;

procedure Lape_Cosecant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cosecant(PExtended(Params^[0])^);
end;

procedure Lape_Cot(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cot(PExtended(Params^[0])^);
end;

procedure Lape_Sec(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sec(PExtended(Params^[0])^);
end;

procedure Lape_Csc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Csc(PExtended(Params^[0])^);
end;

procedure Lape_Cosh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cosh(PExtended(Params^[0])^);
end;

procedure Lape_Sinh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sinh(PExtended(Params^[0])^);
end;

procedure Lape_Tanh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Tanh(PExtended(Params^[0])^);
end;

procedure Lape_CotH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := cotan(PExtended(Params^[0])^);
end;

procedure Lape_SecH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := secant(PExtended(Params^[0])^);
end;

procedure Lape_CscH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Csc(PExtended(Params^[0])^);
end;

procedure Lape_ArcCosh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcCosh(PExtended(Params^[0])^);
end;

procedure Lape_ArcSinh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcSinh(PExtended(Params^[0])^);
end;

procedure Lape_DecRet(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := DecRet(PExtended(Params^[0])^);
end;

procedure Lape_log10(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := log10(PExtended(Params^[0])^);
end;

procedure Lape_MaxA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := MaxA(TIntegerArray(Params^[0]^));
end;

procedure Lape_MinA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := MinA(TIntegerArray(Params^[0]^));
end;

procedure Lape_Sum64IntArr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Sum64IntArr(PIntegerArray(Params^[0])^);
end;

procedure Lape_FixRad(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixRad(PExtended(Params^[0])^);
end;

procedure Lape_InAbstractBox(const Params : PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := InAbstractBox(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, PInt32(Params^[6])^, PInt32(Params^[7])^, PInt32(Params^[8])^, PInt32(Params^[9])^);
end;

procedure Lape_MiddleBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleBox(PBox(Params^[0])^);
end;

procedure Lape_Import_Math(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalFunc('function Pow(base, exponent: Extended): Extended', @Lape_pow);
    addGlobalFunc('function RiemannGauss(Xstart, StepSize, Sigma: Extended; AmountSteps: Int32): Extended', @Lape_RiemannGauss);
    addGlobalFunc('function DiscreteGauss(Xstart, Xend: Int32; sigma: Extended): TExtendedArray', @Lape_DiscreteGauss);
    addGlobalFunc('function GaussMatrix(N: Int32; sigma: Extended): T2DExtendedArray', @Lape_GaussMatrix);
    addGlobalFunc('function MinE(a, b: Extended): Extended', @Lape_MinE);
    addGlobalFunc('function MaxE(a, b: Extended): Extended', @Lape_MaxE);
    addGlobalFunc('function Point(x, y: Int32): TPoint', @Lape_Point);
    addGlobalFunc('function Distance(x1, y1, x2, y2: Int32): Int32', @Lape_Distance);
    addGlobalFunc('function RandomRange(const aFrom, aTo: Int32): Int32', @Lape_RandomRange);
    addGlobalFunc('function RandomE: Extended', @Lape_RandomE);
    addGlobalFunc('procedure IncEx(var x: Int32; increase: Int32);', @Lape_IncEx);
    addGlobalFunc('procedure DecEx(var x: Int32; Decrease: Int32);', @Lape_DecEx);
    addGlobalFunc('function Factorial(number: longword): Int64', @Lape_Factorial);
    addGlobalFunc('function BinCoe(a, b: LongInt): Extended', @Lape_BinCoe);
    addGlobalFunc('function FixD(Degrees: Extended): Extended', @Lape_FixD);
    addGlobalFunc('function InRange(const Value, Min, Max: Int32): boolean', @Lape_InRange);
    addGlobalFunc('function IntToBox(x1, y1, x2, y2: Int32): TBox', @Lape_IntToBox);
    addGlobalFunc('function IntInBox(x, y: Int32; Box: TBox): Boolean', @Lape_IntInBox);
    addGlobalFunc('function PointToBox(PT1, PT2: TPoint): TBox', @Lape_PointToBox);
    addGlobalFunc('function PointInBox(PT: TPoint; Box: TBox): Boolean', @Lape_PointInBox);
    addGlobalFunc('function logn(base, x: Extended): Extended', @Lape_logn);
    addGlobalFunc('function sar(AValue: longint; shift: byte): longint', @Lape_sar);
    addGlobalFunc('function ror(num: longword; shift: byte): LongWord', @Lape_ror);
    addGlobalFunc('function rol(num: longword; shift: byte): LongWord', @Lape_rol);
    addGlobalFunc('function radians(e: Extended): Extended', @Lape_radians);
    addGlobalFunc('function degrees(e: Extended): Extended', @Lape_degrees);
    addGlobalFunc('function Cot(e: Extended): Extended', @Lape_Cot);
    addGlobalFunc('function Sec(e: Extended): Extended', @Lape_Sec);
    addGlobalFunc('function Csc(e: Extended): Extended', @Lape_Csc);
    addGlobalFunc('function CotH(e: Extended): Extended', @Lape_CotH);
    addGlobalFunc('function SecH(e: Extended): Extended', @Lape_SecH);
    addGlobalFunc('function CscH(e: Extended): Extended', @Lape_CscH);
    addGlobalFunc('function DecRet(e: Extended): Extended', @Lape_DecRet);
    addGlobalFunc('function log10(f: Extended): Extended', @Lape_log10);
    addGlobalFunc('function MinA(Arr: TIntegerArray): Int32', @Lape_MinA);
    addGlobalFunc('function MaxA(Arr: TIntegerArray): Int32', @Lape_MaxA);
    addGlobalFunc('function Sum64IntArr(const Arr: TIntegerArray): Int64;', @Lape_Sum64IntArr);
    addGlobalFunc('function FixRad(rad: Extended): Extended;', @Lape_FixRad);
    addGlobalFunc('function InAbstractBox(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Int32; X, Y: Int32): Boolean;', @Lape_InAbstractBox);
    addGlobalFunc('function MiddleBox(b: TBox): TPoint', @Lape_MiddleBox);
  end;
end;

initialization
  ScriptImports.Add('Math', @Lape_Import_Math);

end.

