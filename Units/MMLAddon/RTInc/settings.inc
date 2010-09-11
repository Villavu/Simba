procedure RUTIS_ps_SetSettingValue(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_SetSettingValue(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_KeyIsSetting(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_KeyIsSetting(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_KeyIsDirectory(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_KeyIsDirectory(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetSettingValue(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PString(Result^.Data)^ := ps_GetSettingValue(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetSettingValueDef(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PString(Result^.Data)^ := ps_GetSettingValueDef(PString(Params^[0].Data)^, PString(Params^[1].Data)^);
end;

procedure RUTIS_ps_ListSettings(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_ListSettings(PString(Params^[0].Data)^, PStringArray(PPointer(Params^[1].Data)^)^);
end;

procedure RUTIS_ps_DeleteSetting(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_DeleteSetting(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_DeleteSubSettings(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_DeleteSubSettings(PString(Params^[0].Data)^);
end;
