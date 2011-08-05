procedure Lape_haval(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_haval(Pstring(Params^[0])^);
end;
