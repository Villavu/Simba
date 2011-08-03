procedure Lape_round(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_round(Pextended(Params^[0])^);
end;

procedure Lape_iAbs(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_iAbs(Pinteger(Params^[0])^);
end;

procedure Lape_ceil(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_ceil(Pextended(Params^[0])^);
end;

procedure Lape_pow(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_pow(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_RiemannGauss(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_RiemannGauss(Pextended(Params^[0])^, Pextended(Params^[1])^, Pextended(Params^[2])^, Pinteger(Params^[1])^);
end;

procedure Lape_DiscreteGauss(const Params: PParamArray; const Result: Pointer);
begin
  PExtendedArray(Result)^ := ps_DiscreteGauss(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pextended(Params^[1])^);
end;

procedure Lape_GaussMatrix(const Params: PParamArray; const Result: Pointer);
begin
  P2DExtendedArray(Result)^ := ps_GaussMatrix(Pinteger(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_exp(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_exp(Pextended(Params^[0])^);
end;

procedure Lape_Max(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_Max(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Min(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_Min(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_MinE(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_MinE(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_MaxE(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_MaxE(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_Sqr(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_Sqr(Pextended(Params^[0])^);
end;

procedure Lape_Point(const Params: PParamArray; const Result: Pointer);
begin
  PPoint(Result)^ := ps_Point(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Distance(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_Distance(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure Lape_Hypot(const Params: PParamArray; const Result: Pointer);
begin
  PExtended(Result)^ := ps_Hypot(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_RandomRange(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_RandomRange(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_Random(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_Random(Pinteger(Params^[0])^);
end;

procedure Lape_RandomE(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_RandomE();
end;

procedure Lape_ArcTan2(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_ArcTan2(Pextended(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_IncEx(const Params: PParamArray);
begin
  ps_IncEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_DecEx(const Params: PParamArray);
begin
  ps_DecEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_Factorial(const Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := ps_Factorial(Plongword(Params^[0])^);
end;
