procedure Lape_Capitalize(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Capitalize(Pstring(Params^[0])^);
end;

procedure Lape_CompressString(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_CompressString(Pstring(Params^[0])^);
end;

procedure Lape_DecompressString(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_DecompressString(Pstring(Params^[0])^);
end;

procedure Lape_Base64Encode(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Base64Encode(Pstring(Params^[0])^);
end;

procedure Lape_Base64Decode(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Base64Decode(Pstring(Params^[0])^);
end;

procedure Lape_ExtractFromStr(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_ExtractFromStr(Pstring(Params^[0])^, PStrExtr(Params^[1])^);
end;

procedure Lape_BoolToStr(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_BoolToStr(Pboolean(Params^[0])^);
end;

procedure Lape_Replace(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Replace(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^, PReplaceFlags(Params^[1])^);
end;

procedure Lape_IntToStr(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_IntToStr(Pinteger(Params^[0])^);
end;

procedure Lape_FloatToStr(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_FloatToStr(Pextended(Params^[0])^);
end;

procedure Lape_StrToInt(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_StrToInt(PString(Params^[0])^);
end;

procedure Lape_StrToIntDef(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_StrToIntDef(PString(Params^[0])^, PInteger(Params^[1])^);
end;

procedure Lape_StrToFloat(const Params: PParamArray; const Result: Pointer);
begin
  PExtended(Result)^ := ps_StrToFloat(PString(Params^[0])^);
end;

procedure Lape_StrToFloatDef(const Params: PParamArray; const Result: Pointer);
begin
  PExtended(Result)^ := ps_StrToFloatDef(PString(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_StrToBool(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_StrToBool(PString(Params^[0])^);
end;

procedure Lape_StrToBoolDef(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_StrToBoolDef(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_Between(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Between(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;
