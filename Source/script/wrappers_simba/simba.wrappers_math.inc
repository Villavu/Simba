procedure _LapeGaussMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DExtendedArray(Result)^ := GaussMatrix(PInteger(Params^[0])^, PExtended(Params^[1])^);
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
  Plongint(Result)^ := SarLongint(Plongint(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure _LapeRor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := RorDWord(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure _LapeRol(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongWord(Result)^ := RolDWord(Plongword(Params^[0])^, Pbyte(Params^[1])^);
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
  PInteger(Result)^ := MaxA(PIntegerArray(Params^[0])^);
end;

procedure _LapeMinA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := MinA(PIntegerArray(Params^[0])^);
end;

procedure _LapeMaxAF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := MaxA(PExtendedArray(Params^[0])^);
end;

procedure _LapeMinAF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := MinA(PExtendedArray(Params^[0])^);
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

procedure _LapeMode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Mode(PIntegerArray(Params^[0])^);
end;

procedure _LapeSum(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Sum(PIntegerArray(Params^[0])^);
end;

procedure _LapeSumF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sum(PExtendedArray(Params^[0])^);
end;

procedure _LapeAverage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Average(PIntegerArray(Params^[0])^);
end;

procedure _LapeAverageF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Average(PExtendedArray(Params^[0])^);
end;

