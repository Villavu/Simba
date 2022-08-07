unit simba.import_math;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, math,
  simba.script_compiler, simba.mufasatypes, simba.math, simba.array_general;

procedure _LapeGaussMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtendedMatrix(Result)^ := GaussMatrix(PInteger(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeDistance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Distance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeDistanceEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Distance(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapeRandomRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := RandomRange(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFixD(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixD(PExtended(Params^[0])^);
end;

procedure _LapeLogn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := logn(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeSar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Plongint(Result)^ := SarLongint(Plongint(Params^[0])^, PByte(Params^[1])^);
end;

procedure _LapeRor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := RorDWord(Plongword(Params^[0])^, PByte(Params^[1])^);
end;

procedure _LapeRol(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := RolDWord(Plongword(Params^[0])^, PByte(Params^[1])^);
end;

procedure _LapeRadians(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Radians(PExtended(Params^[0])^);
end;

procedure _LapeDegrees(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Degrees(PExtended(Params^[0])^);
end;

procedure _LapeLog10(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Log10(PExtended(Params^[0])^);
end;

procedure _LapeMaxA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Max(PIntegerArray(Params^[0])^);
end;

procedure _LapeMinA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Min(PIntegerArray(Params^[0])^);
end;

procedure _LapeFixRad(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixRad(PExtended(Params^[0])^);
end;

procedure _LapeNextPower2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := NextPower2(PInteger(Params^[0])^);
end;

procedure _LapeModulo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Modulo(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeModuloF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Modulo(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeSum(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Sum(PIntegerArray(Params^[0])^);
end;

procedure _LapeSumF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sum(PExtendedArray(Params^[0])^);
end;

procedure _LapeMean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Mean(PIntegerArray(Params^[0])^);
end;

procedure _LapeMeanF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Mean(PExtendedArray(Params^[0])^);
end;

procedure ImportMath(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Math');

    addGlobalFunc('function GaussMatrix(N: Integer; Sigma: Extended): TExtendedMatrix', @_LapeGaussMatrix);
    addGlobalFunc('function Distance(const X1, Y1, X2, Y2: Integer): Integer; overload', @_LapeDistance);
    addGlobalFunc('function Distance(const P1, P2: TPoint): Integer; overload', @_LapeDistanceEx);
    addGlobalFunc('function RandomRange(const Min, Max: Integer): Integer', @_LapeRandomRange);
    addGlobalFunc('function FixD(Degrees: Extended): Extended', @_LapeFixD);
    addGlobalFunc('function LogN(base, x: Extended): Extended', @_LapeLogn);
    addGlobalFunc('function Sar(num: Integer; shift: byte): Integer', @_LapeSar);
    addGlobalFunc('function Ror(num: UInt32; shift: byte): UInt32', @_LapeRor);
    addGlobalFunc('function Rol(num: UInt32; shift: byte): UInt32', @_LapeRol);
    addGlobalFunc('function Radians(e: Extended): Extended', @_LapeRadians);
    addGlobalFunc('function Degrees(e: Extended): Extended', @_LapeDegrees);
    addGlobalFunc('function Log10(f: Extended): Extended', @_LapeLog10);
    addGlobalFunc('function FixRad(rad: Extended): Extended', @_LapeFixRad);
    addGlobalFunc('function NextPower2(const n: Integer): Integer', @_LapeNextPower2);

    addGlobalFunc('function MinA(const Arr: TIntegerArray): Integer', @_LapeMinA);
    addGlobalFunc('function MaxA(const Arr: TIntegerArray): Integer', @_LapeMaxA);

    addGlobalFunc('function Modulo(const X, Y: Integer): Integer; overload', @_LapeModulo);
    addGlobalFunc('function Modulo(const X, Y: Extended): Extended; overload', @_LapeModuloF);

    addGlobalFunc('function Sum(const Arr: TIntegerArray): Int64; overload', @_LapeSum);
    addGlobalFunc('function Sum(const Arr: TExtendedArray): Extended; overload', @_LapeSumF);

    addGlobalFunc('function Mean(const Arr: TIntegerArray): Int64; overload', @_LapeMean);
    addGlobalFunc('function Mean(const Arr: TExtendedArray): Extended; overload', @_LapeMeanF);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMath);

end.

