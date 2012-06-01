procedure Lape_FindDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindDTM(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure Lape_FindDTMs(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindDTMs(PInteger(Params^[0])^, PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure Lape_FindDTMRotatedAlternating(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindDTMRotatedAlternating(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^);
end;

procedure Lape_FindDTMRotatedSE(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindDTMRotatedSE(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^);
end;

procedure Lape_FindDTMsRotatedAlternating(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindDTMsRotatedAlternating(PInteger(Params^[0])^, PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, P2DExtendedArray(Params^[9])^);
end;

procedure Lape_FindDTMsRotatedSE(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FindDTMsRotatedSE(PInteger(Params^[0])^, PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PExtended(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, P2DExtendedArray(Params^[9])^);
end;

procedure Lape_SetDTMName(const Params: PParamArray); lape_extdecl
begin
  ps_SetDTMName(Pinteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_DTMFromString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_DTMFromString(PString(Params^[0])^);
end;

procedure Lape_FreeDTM(const Params: PParamArray); lape_extdecl
begin
  ps_FreeDTM(PInteger(Params^[0])^);
end;

procedure Lape_GetDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMDTM(Result)^ := ps_GetDTM(PInteger(Params^[0])^);
end;

procedure Lape_AddTSDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_AddTSDTM(PSDTM(Params^[0])^);
end;

procedure Lape_AddDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_AddDTM(PMDTM(Params^[0])^);
end;

procedure Lape_PrintDTM(const Params: PParamArray); lape_extdecl
begin
  ps_PrintDTM(PMDTM(Params^[0])^);
end;

procedure Lape_MDTMToSDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PSDTM(Result)^ := ps_MDTMToSDTM(PMDTM(Params^[0])^);
end;

procedure Lape_SDTMToMDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMDTM(Result)^ := ps_SDTMToMDTM(PSDTM(Params^[0])^);
end;

procedure Lape_CreateDTMPoint(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMDTMPoint(Result)^ := ps_CreateDTMPoint(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pboolean(Params^[5])^);
end;

procedure Lape_DTMExists(const Params : PParamArray; const Result : Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_DTMExists(PLongInt(Params^[0])^);
end;
