procedure RUTIS_ps_Capitalize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_Capitalize(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_CompressString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_CompressString(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_DecompressString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_DecompressString(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_Base64Encode(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_Base64Encode(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_Base64Decode(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_Base64Decode(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_ExtractFromStr(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_ExtractFromStr(Pstring(Params^[0].Data)^, PStrExtr(Params^[1].Data)^);
end;

procedure RUTIS_ps_BoolToStr(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_BoolToStr(Pboolean(Params^[0].Data)^);
end;

procedure RUTIS_ps_Replace(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_Replace(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^, Pstring(Params^[2].Data)^, PReplaceFlags(Params^[3].Data)^);
end;

procedure RUTIS_ps_IntToStr(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_IntToStr(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_FloatToStr(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_FloatToStr(Pextended(Params^[0].Data)^);
end;

procedure RUTIS_ps_StrToInt(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_StrToInt(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_StrToIntDef(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_StrToIntDef(PString(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_StrToFloat(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PExtended(Result^.Data)^ := ps_StrToFloat(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_StrToFloatDef(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PExtended(Result^.Data)^ := ps_StrToFloatDef(PString(Params^[0].Data)^, PExtended(Params^[1].Data)^);
end;

procedure RUTIS_ps_StrToBool(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_StrToBool(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_StrToBoolDef(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_StrToBoolDef(PString(Params^[0].Data)^, PBoolean(Params^[1].Data)^);
end;

procedure RUTIS_ps_Between(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_Between(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^, Pstring(Params^[2].Data)^);
end;
