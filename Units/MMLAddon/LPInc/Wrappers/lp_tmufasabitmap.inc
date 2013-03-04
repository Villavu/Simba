procedure Lape_TMufasaBitmap_LoadFromFile(const Params: PParamArray); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  Self.LoadFromFile(PString(Params^[1])^);
end;

procedure Lape_TMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PBoolean(Result)^ := Self.SaveToFile(PString(Params^[1])^);
end;
