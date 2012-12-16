procedure Lape_pow(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_pow(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_RiemannGauss(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_RiemannGauss(Pextended(Params^[0])^, Pextended(Params^[1])^, Pextended(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure Lape_DiscreteGauss(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtendedArray(Result)^ := ps_DiscreteGauss(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pextended(Params^[2])^);
end;

procedure Lape_GaussMatrix(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DExtendedArray(Result)^ := ps_GaussMatrix(Pinteger(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_Max(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_Max(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Min(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_Min(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_MinE(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_MinE(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_MaxE(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_MaxE(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_Point(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := ps_Point(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Distance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_Distance(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure Lape_Hypot(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := ps_Hypot(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_RandomRange(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_RandomRange(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_RandomE(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_RandomE();
end;

procedure Lape_ArcTan2(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_ArcTan2(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_IncEx(const Params: PParamArray); lape_extdecl
begin
  ps_IncEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_DecEx(const Params: PParamArray); lape_extdecl
begin
  ps_DecEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Factorial(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := ps_Factorial(Plongword(Params^[0])^);
end;

procedure Lape_BinCoe(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := ps_BinCoe(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure Lape_FixD(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := ps_FixD(Pextended(Params^[0])^);
end;

procedure Lape_InRange(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_InRange(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_IntToBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBox(Result)^ := ps_IntToBox(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure Lape_IntInBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_IntInBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure Lape_PointToBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBox(Result)^ := ps_PointToBox(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_PointInBox(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

procedure Lape_logn(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_logn(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_sar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Plongint(Result)^ := ps_sar(Plongint(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure Lape_ror(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongWord(Result)^ := ps_ror(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure Lape_rol(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongWord(Result)^ := ps_rol(Plongword(Params^[0])^, Pbyte(Params^[1])^);
end;

procedure Lape_tan(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_tan(Pextended(Params^[0])^);
end;

procedure Lape_radians(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_radians(Pextended(Params^[0])^);
end;

procedure Lape_degrees(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_degrees(Pextended(Params^[0])^);
end;

procedure Lape_ArcSin(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_ArcSin(Pextended(Params^[0])^);
end;

procedure Lape_ArcCos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_ArcCos(Pextended(Params^[0])^);
end;

procedure Lape_Cotan(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Cotan(Pextended(Params^[0])^);
end;

procedure Lape_Secant(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Secant(Pextended(Params^[0])^);
end;

procedure Lape_Cosecant(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Cosecant(Pextended(Params^[0])^);
end;

procedure Lape_Cot(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Cot(Pextended(Params^[0])^);
end;

procedure Lape_Sec(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Sec(Pextended(Params^[0])^);
end;

procedure Lape_Csc(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Csc(Pextended(Params^[0])^);
end;

procedure Lape_Cosh(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Cosh(Pextended(Params^[0])^);
end;

procedure Lape_Sinh(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Sinh(Pextended(Params^[0])^);
end;

procedure Lape_Tanh(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_Tanh(Pextended(Params^[0])^);
end;

procedure Lape_CotH(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_CotH(Pextended(Params^[0])^);
end;

procedure Lape_SecH(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_SecH(Pextended(Params^[0])^);
end;

procedure Lape_CscH(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_CscH(Pextended(Params^[0])^);
end;

procedure Lape_ArcCosh(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_ArcCosh(Pextended(Params^[0])^);
end;

procedure Lape_ArcSinh(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pextended(Result)^ := ps_ArcSinh(Pextended(Params^[0])^);
end;

procedure Lape_DecRet(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := DecRet(PExtended(Params^[0])^);
end;

procedure Lape_log10(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := ps_log10(PExtended(Params^[0])^);
end;

procedure Lape_MaxA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_MaxA(TIntegerArray(Params^[0]^));
end;

procedure Lape_MinA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_MinA(TIntegerArray(Params^[0]^));
end;

procedure Lape_Sum64IntArr(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := ps_Sum64IntArr(PIntegerArray(Params^[0])^);
end;

procedure Lape_IntToHex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PString(Result)^ := IntToHex(PInteger(Params^[0])^,0);
end;

procedure Lape_FixRad(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PExtended(Result)^ := FixRad(PExtended(Params^[0])^);
end;

procedure Lape_InAbstractBox(const Params : PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := InAbstractBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PInteger(Params^[9])^);
end;
