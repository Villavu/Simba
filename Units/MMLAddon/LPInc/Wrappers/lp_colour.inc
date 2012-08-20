procedure Lape_GetColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := ps_GetColor(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_GetColorsWrap(const Params: PParamArray); lape_extdecl
begin
  ps_GetColorsWrap(PPointArray(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure Lape_GetColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PIntegerArray(Result)^ := ps_GetColors(PPointArray(Params^[0])^);
end;

procedure Lape_findcolor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_findcolor(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^);
end;

procedure Lape_findcolortolerance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_findcolortolerance(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^);
end;

procedure Lape_FindColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindColors(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_SetColorToleranceSpeed(const Params: PParamArray); lape_extdecl
begin
  ps_SetColorToleranceSpeed(PInteger(Params^[0])^);
end;

procedure Lape_GetToleranceSpeed(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_GetToleranceSpeed();
end;

procedure Lape_SetToleranceSpeed2Modifiers(const Params: PParamArray); lape_extdecl
begin
  ps_SetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_GetToleranceSpeed2Modifiers(const Params: PParamArray); lape_extdecl
begin
  ps_GetToleranceSpeed2Modifiers(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_SetToleranceSpeed3Modifier(const Params: PParamArray); lape_extdecl
begin
  ps_SetToleranceSpeed3Modifier(PExtended(Params^[0])^);
end;

function Lape_GetToleranceSpeed3Modifier(const Params: PParamArray): Extended;
begin
  Result := ps_GetToleranceSpeed3Modifier();
end;

procedure Lape_SimilarColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_SimilarColors(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_CountColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_CountColor(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure Lape_CountColorTolerance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_CountColorTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_FindColorsTolerance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindColorsTolerance(PPointArray(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure Lape_FindColorSpiral(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindColorSpiral(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure Lape_FindColorSpiralTolerance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindColorSpiralTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure Lape_FindColorsSpiralTolerance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_FindColorsSpiralTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PPointArray(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure Lape_FindColoredArea(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindColoredArea(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure Lape_FindColoredAreaTolerance(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindColoredAreaTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;
