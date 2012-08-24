procedure Lape_rs_GetUpText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PString(Result)^ := ps_rs_GetUpText();
end;

procedure Lape_rs_GetUpTextAtEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_rs_GetUpTextAtEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure Lape_rs_GetUpTextAt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_rs_GetUpTextAt(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_BitmapFromText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_BitmapFromText(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_MaskFromText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMask(Result)^ := ps_MaskFromText(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_TPAFromTextWrap(const Params: PParamArray); lape_extdecl
begin
  ps_TPAFromTextWrap(PString(Params^[0])^, PString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, PPointArray(Params^[4])^);
end;

procedure Lape_TPAFromText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := ps_TPAFromText(PString(Params^[0])^, PString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure Lape_GetTextATPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_GetTextATPA(P2DPointArray(Params^[0])^, Pinteger(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_GetTextAt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_GetTextAt(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pstring(Params^[8])^);
end;

procedure Lape_GetTextAtEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_GetTextAtEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pinteger(Params^[8])^, Pstring(Params^[9])^);
end;

procedure Lape_LoadSystemFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_LoadSystemFont(PFont(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_LoadFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_LoadFont(Pstring(Params^[0])^, Pboolean(Params^[1])^);
end;

procedure Lape_FreeFont(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_FreeFont(Pstring(Params^[0])^);
end;

procedure Lape_FilterUpTextByCharacteristics(const Params: PParamArray); lape_extdecl
begin
  ps_FilterUpTextByCharacteristics(TMufasaBitmap(Params^[0]^));
end;

procedure Lape_SetUpTextFilter(const Params: PParamArray); lape_extdecl
begin
  ps_SetUpTextFilter(POcrFilterDataArray(Params^[0])^);
end;

procedure Lape_ResetUpTextFilter(const Params: PParamArray); lape_extdecl
begin
  ps_ResetUpTextFilter;
end;
