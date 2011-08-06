procedure Lape_haval(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_haval(Pstring(Params^[0])^);
end;

procedure Lape_md4(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_md4(Pstring(Params^[0])^);
end;

procedure Lape_md5(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_md5(Pstring(Params^[0])^);
end;

procedure Lape_ripemd128(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_ripemd128(Pstring(Params^[0])^);
end;

procedure Lape_ripemd160(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_ripemd160(Pstring(Params^[0])^);
end;

procedure Lape_sha1(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_sha1(Pstring(Params^[0])^);
end;

procedure Lape_sha256(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_sha256(Pstring(Params^[0])^);
end;

procedure Lape_sha384(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_sha384(Pstring(Params^[0])^);
end;

procedure Lape_sha512(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_sha512(Pstring(Params^[0])^);
end;

procedure Lape_tiger(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_tiger(Pstring(Params^[0])^);
end;
