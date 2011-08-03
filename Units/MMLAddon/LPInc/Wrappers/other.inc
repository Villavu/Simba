procedure Lape_Writeln(const Params: PParamArray);
begin
  ps_Writeln(Pstring(Params^[0])^);
end;

procedure Lape_SetScriptProp(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_SetScriptProp(PSP_Property(Params^[0])^, PVariantArray(Params^[1])^);
end;

procedure Lape_GetScriptProp(const Params: PParamArray; const Result: Pointer);
begin
  Pboolean(Result)^ := ps_GetScriptProp(PSP_Property(Params^[0])^, PVariantArray(Params^[1])^);
end;

procedure Lape_Wait(const Params: PParamArray);
begin
  ps_Wait(PDWord(Params^[0])^);
end;
