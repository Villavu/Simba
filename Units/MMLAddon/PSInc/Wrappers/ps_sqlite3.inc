function ps_sqlite_version() : string; extdecl;
begin;
  result := CurrThread.SQLite3.version();
end;

function ps_sqlite_version_num() : integer; extdecl;
begin;
  result := CurrThread.SQLite3.version_num();
end;

function ps_sqlite_escape(s : string) : string; extdecl;
begin;
  result := CurrThread.SQLite3.escape(s);
end;

function ps_sqlite_open(filename : string) : integer; extdecl;
begin;
  result := CurrThread.SQLite3.open_db(filename, 6);
end;

function ps_sqlite_open_v2(filename : string; flags : integer) : integer; extdecl;
begin;
  result := CurrThread.SQLite3.open_db(filename, flags);
end;

procedure ps_sqlite_close(index : integer); extdecl;
begin;
  CurrThread.SQLite3.closeHandle(index);
end;

function ps_sqlite_errmsg(index : integer) : string; extdecl;
begin;
  result := CurrThread.SQLite3.errmsg(index);
end;

function ps_sqlite_errcode(index : integer) : integer; extdecl;
begin;
  result := CurrThread.SQLite3.errcode(index);
end;

function ps_sqlite_query(index : integer; sql : string) : boolean; extdecl;
begin;
  result := CurrThread.SQLite3.query(index, sql);
end;

function ps_sqlite_queryvalue(index : integer; sql : string; out data : T2DStringArray) : boolean; extdecl;
begin;
  result := CurrThread.SQLite3.queryvalue(index, sql, data);
end;

function ps_sqlite_queryresult(index : integer; sql : string; var error : boolean) : T2DStringArray; extdecl;
begin;
  result := CurrThread.SQLite3.queryresult(index, sql, error);
end;