procedure Lape_SetSettingValue(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_SetSettingValue(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_KeyIsSetting(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_KeyIsSetting(PString(Params^[0])^);
end;

procedure Lape_KeyIsDirectory(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_KeyIsDirectory(PString(Params^[0])^);
end;

procedure Lape_GetSettingValue(const Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := ps_GetSettingValue(PString(Params^[0])^);
end;

procedure Lape_GetSettingValueDef(const Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := ps_GetSettingValueDef(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_ListSettings(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_ListSettings(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure Lape_DeleteSetting(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_DeleteSetting(PString(Params^[0])^);
end;

procedure Lape_DeleteSubSettings(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_DeleteSubSettings(PString(Params^[0])^);
end;
