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

procedure Lape_Replace(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Replace(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^, PReplaceFlags(Params^[3])^);
end;

procedure Lape_Between(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Between(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_Implode(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_Implode(Pstring(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure Lape_Explode(const Params: PParamArray; const Result: Pointer);
begin
  PStringArray(Result)^ := ps_Explode(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_ExplodeWrap(const Params: PParamArray);
begin
  ps_ExplodeWrap(Pstring(Params^[0])^, Pstring(Params^[1])^, PStringArray(Params^[2])^);
end;

procedure Lape_ExecRegExpr(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_ExecRegExpr(PString(Params^[0])^, PString(Params^[1])^);
end;

//procedure Lape_SplitRegExpr(const Params: PParamArray);
//begin
//  ps_SplitRegExpr(PString(Params^[0])^, PString(Params^[1])^, PStrings(Params^[2])^);
//end;

procedure Lape_ReplaceRegExpr(const Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := ps_ReplaceRegExpr(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, Pboolean(Params^[3])^);
end;

procedure Lape_posex(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_posex(PString(Params^[0])^, PString(Params^[1])^, Pinteger(Params^[2])^);
end;
