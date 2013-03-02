procedure Lape_TMufasaBitmap_Foo(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Self: TMufasaBitmap;
begin
  Self := PMufasaBitmap(Params^[0])^;
  PPtrUInt(Result)^ := PtrUInt(Self);
end;
