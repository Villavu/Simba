procedure Lape_rs_GetUpText(const Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := ps_rs_GetUpText();
end;

procedure Lape_rs_GetUpTextAtEx(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_rs_GetUpTextAtEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pboolean(Params^[1])^);
end;

procedure Lape_rs_GetUpTextAt(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_rs_GetUpTextAt(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_BitmapFromText(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_BitmapFromText(PString(Params^[0])^, PString(Params^[1])^);
end;
