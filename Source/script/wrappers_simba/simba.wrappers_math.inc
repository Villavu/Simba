procedure _LapeRiemannGauss(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := RiemannGauss(PExtended(Params^[0])^, PExtended(Params^[1])^, PExtended(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeDiscreteGauss(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtendedArray(Result)^ := DiscreteGauss(PInteger(Params^[0])^, PInteger(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeGaussMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DExtendedArray(Result)^ := GaussMatrix(PInteger(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Point(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeDistance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Distance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeRandomRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := RandomRange(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFactorial(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Factorial(Plongword(Params^[0])^);
end;

procedure _LapeBinCoe(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := BinCoe(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure _LapeFixD(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixD(PExtended(Params^[0])^);
end;

procedure _LapeIntToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := IntToBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIntInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IntInBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapePointToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PointToBox(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapePointInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

procedure _Lapelogn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := logn(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _Lapesar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Plongint(Result)^ := SarLongint(Plongint(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure _Laperor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := RorDWord(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure _Laperol(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := RolDWord(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure _Laperadians(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := radians(PExtended(Params^[0])^);
end;

procedure _Lapedegrees(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := degrees(PExtended(Params^[0])^);
end;

procedure _LapeCsc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Csc(PExtended(Params^[0])^);
end;

procedure _Lapelog10(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := log10(PExtended(Params^[0])^);
end;

procedure _LapeMaxA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize MaxA<Integer>(TIntegerArray(Params^[0]^));
end;

procedure _LapeMinA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := specialize MinA<Integer>(TIntegerArray(Params^[0]^));
end;

procedure _LapeFixRad(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := FixRad(PExtended(Params^[0])^);
end;

procedure _LapeMiddleBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleBox(PBox(Params^[0])^);
end;

procedure _LapeNextPow2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := NextPow2(PInteger(Params^[0])^);
end;

