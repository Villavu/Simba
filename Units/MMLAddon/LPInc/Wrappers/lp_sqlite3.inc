procedure lp_sqlite_version(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PString(Result)^ := ps_sqlite_version();
end;

procedure lp_sqlite_version_num(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PLongInt(Result)^ := ps_sqlite_version_num();
end;

procedure lp_sqlite_escape(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PString(Result)^ := ps_sqlite_escape(PString(Params^[0])^);
end;

procedure lp_sqlite_close(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  ps_sqlite_close(PLongInt(Params^[0])^);
end;

procedure lp_sqlite_open(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PLongInt(Result)^ := ps_sqlite_open(PString(Params^[0])^);
end;

procedure lp_sqlite_open_v2(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PLongInt(Result)^ := ps_sqlite_open_v2(PString(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure lp_sqlite_errmsg(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PString(Result)^ := ps_sqlite_errmsg(PLongInt(Params^[0])^);
end;

procedure lp_sqlite_errcode(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PLongInt(Result)^ := ps_sqlite_errcode(PLongInt(Params^[0])^);
end;

procedure lp_sqlite_query(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_sqlite_query(PLongInt(Params^[0])^, PString(Params^[1])^);
end;

procedure lp_sqlite_queryvalue(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_sqlite_queryvalue(PLongInt(Params^[0])^, PString(Params^[1])^, P2DStringArray(Params^[2])^);
end;

procedure lp_sqlite_queryresult(Params : PParamArray; Result : Pointer); lape_extdecl
begin
  P2DStringArray(Result)^ := ps_sqlite_queryresult(PLongInt(Params^[0])^, PString(Params^[1])^, PBoolean(Params^[2])^);
end;

