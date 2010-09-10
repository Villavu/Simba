procedure RUTIS_ps_FindDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDTM(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FindDTMs(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDTMs(PInteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^);
end;

procedure RUTIS_ps_FindDTMRotatedAlternating(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDTMRotatedAlternating(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PExtended(Params^[7].Data)^, PExtended(Params^[8].Data)^, PExtended(Params^[9].Data)^, PExtended(Params^[10].Data)^);
end;

procedure RUTIS_ps_FindDTMRotatedSE(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDTMRotatedSE(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PExtended(Params^[7].Data)^, PExtended(Params^[8].Data)^, PExtended(Params^[9].Data)^, PExtended(Params^[10].Data)^);
end;

procedure RUTIS_ps_FindDTMsRotatedAlternating(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDTMsRotatedAlternating(PInteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PExtended(Params^[6].Data)^, PExtended(Params^[7].Data)^, PExtended(Params^[8].Data)^, P2DExtendedArray(Params^[9].Data)^);
end;

procedure RUTIS_ps_FindDTMsRotatedSE(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDTMsRotatedSE(PInteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PExtended(Params^[6].Data)^, PExtended(Params^[7].Data)^, PExtended(Params^[8].Data)^, P2DExtendedArray(Params^[9].Data)^);
end;

procedure RUTIS_ps_SetDTMName(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetDTMName(Pinteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_DTMFromString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_DTMFromString(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_FreeDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FreeDTM(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PMDTM(Result^.Data)^ := ps_GetDTM(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_AddTSDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_AddTSDTM(PSDTM(Params^[0].Data)^);
end;

procedure RUTIS_ps_AddDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_AddDTM(PMDTM(Params^[0].Data)^);
end;

procedure RUTIS_ps_PrintDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_PrintDTM(PMDTM(Params^[0].Data)^);
end;

procedure RUTIS_ps_MDTMToSDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PSDTM(Result^.Data)^ := ps_MDTMToSDTM(PMDTM(Params^[0].Data)^);
end;

procedure RUTIS_ps_SDTMToMDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PMDTM(Result^.Data)^ := ps_SDTMToMDTM(PSDTM(Params^[0].Data)^);
end;

procedure RUTIS_ps_CreateDTMPoint(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PMDTMPoint(Result^.Data)^ := ps_CreateDTMPoint(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pinteger(Params^[3].Data)^, Pinteger(Params^[4].Data)^, Pboolean(Params^[5].Data)^);
end;
