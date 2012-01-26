procedure Lape_ext_UnTar(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ext_UnTar(Pstring(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure Lape_ext_UnTarEx(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ext_UnTarEx(Pstring(Params^[0])^, Pstring(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure Lape_ext_DecompressBZip2(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ext_DecompressBZip2(Pstring(Params^[0])^, Pstring(Params^[1])^, PCardinal(Params^[2])^);
end;

procedure Lape_ext_GetPage(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ext_GetPage(Pstring(Params^[0])^);
end;

procedure Lape_ext_MessageDlg(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ext_MessageDlg(Pstring(Params^[0])^, Pstring(Params^[1])^, PMsgDlgType(Params^[2])^, PMsgDlgButtons(Params^[3])^, PLongint(Params^[4])^);
end;

procedure Lape_ext_SDTMToMDTM(const Params: PParamArray; const Result: Pointer);
begin
  PMDTM(Result)^ := ext_SDTMToMDTM(PSDTM(Params^[0])^);
end;

procedure Lape_ext_InputQuery(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ext_InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_ext_ScriptText(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ext_ScriptText();
end;

procedure Lape_ext_GetSelectedText(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ext_GetSelectedText();
end;

procedure Lape_ext_OpenScript(const Params: PParamArray);
begin
  ext_OpenScript(Pstring(Params^[0])^, Pstring(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure Lape_ext_OpenScriptEx(const Params: PParamArray);
begin
  ext_OpenScriptEx(Pstring(Params^[0])^, Pboolean(Params^[1])^);
end;

procedure Lape_ext_GetPageEx(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ext_GetPageEx(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_ext_GetJSONValue(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ext_GetJSONValue(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;
