unit simba.import_math;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, math,
  simba.script_compiler, simba.mufasatypes, simba.math;

procedure _LapeDistance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Distance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeDistanceEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := Distance(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeRandomRange(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := RandomRange(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFixD(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := FixD(PExtended(Params^[0])^);
end;

procedure _LapeLogn(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := logn(PExtended(Params^[0])^, PExtended(Params^[1])^);
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
  PExtended(Result)^ := Radians(PExtended(Params^[0])^);
end;

procedure _LapeDegrees(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := Degrees(PExtended(Params^[0])^);
end;

procedure _LapeLog2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := Log2(PExtended(Params^[0])^);
end;

procedure _LapeLog10(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := Log10(PExtended(Params^[0])^);
end;

procedure _LapeMaxA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize MaxA<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeMinA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := specialize MinA<Integer>(PIntegerArray(Params^[0])^);
end;

procedure _LapeFixRad(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PExtended(Result)^ := FixRad(PExtended(Params^[0])^);
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
  PExtended(Result)^ := Modulo(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure ImportMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Math';

    addGlobalVar(ltDouble, @HALF_PI, 'HALF_PI').isConstant := True;

    addGlobalFunc('function Distance(const X1, Y1, X2, Y2: Integer): Integer; overload', @_LapeDistance);
    addGlobalFunc('function Distance(const P1, P2: TPoint): Integer; overload', @_LapeDistanceEx);
    addGlobalFunc('function RandomRange(const Min, Max: Integer): Integer', @_LapeRandomRange);
    addGlobalFunc('function FixD(Degrees: Extended): Extended', @_LapeFixD);
    addGlobalFunc('function Sar(num: Integer; shift: byte): Integer', @_LapeSar);
    addGlobalFunc('function Ror(num: UInt32; shift: byte): UInt32', @_LapeRor);
    addGlobalFunc('function Rol(num: UInt32; shift: byte): UInt32', @_LapeRol);
    addGlobalFunc('function Radians(e: Extended): Extended', @_LapeRadians);
    addGlobalFunc('function Degrees(e: Extended): Extended', @_LapeDegrees);
    addGlobalFunc('function LogN(base, x: Extended): Extended', @_LapeLogn);
    addGlobalFunc('function Log2(x: Extended): Extended', @_LapeLog2);
    addGlobalFunc('function Log10(x: Extended): Extended', @_LapeLog10);
    addGlobalFunc('function FixRad(rad: Extended): Extended', @_LapeFixRad);
    addGlobalFunc('function NextPower2(const n: Integer): Integer', @_LapeNextPower2);

    addGlobalFunc('function MinA(const Arr: TIntegerArray): Integer', @_LapeMinA);
    addGlobalFunc('function MaxA(const Arr: TIntegerArray): Integer', @_LapeMaxA);

    addGlobalFunc('function Modulo(const X, Y: Integer): Integer; overload', @_LapeModulo);
    addGlobalFunc('function Modulo(const X, Y: Extended): Extended; overload', @_LapeModuloF);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMath);

end.

