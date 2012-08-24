procedure Lape_OpenWebPage(const Params: PParamArray); lape_extdecl
begin
  ps_OpenWebPage(Pstring(Params^[0])^);
end;

procedure Lape_GetPage(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PString(Result)^ := ps_GetPage(PString(Params^[0])^);
end;

procedure Lape_InitializeHTTPClient(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_InitializeHTTPClient(PBoolean(Params^[0])^);
end;

procedure Lape_FreeHTTPClient(const Params: PParamArray); lape_extdecl
begin
  ps_FreeHTTPClient(PInteger(Params^[0])^);
end;

procedure Lape_GetHTTPPage(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_GetHTTPPage(PInteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_SetHTTPUserAgent(const Params: PParamArray); lape_extdecl
begin
  ps_SetHTTPUserAgent(PInteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_PostHTTPPage(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_PostHTTPPage(PInteger(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_PostHTTPPageEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_PostHTTPPageEx(PInteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_ClearPostData(const Params: PParamArray); lape_extdecl
begin
  ps_ClearPostData(PInteger(Params^[0])^);
end;

procedure Lape_AddPostVariable(const Params: PParamArray); lape_extdecl
begin
  ps_AddPostVariable(PInteger(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_GetRawHeaders(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_GetRawHeaders(PInteger(Params^[0])^);
end;

procedure Lape_SetProxy(const Params: PParamArray); lape_extdecl
begin
  ps_SetProxy(PInteger(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_RecvSocketStr(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_RecvSocketStr(Pinteger(Params^[0])^);
end;

procedure Lape_RecvSocket(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_RecvSocket(Pinteger(Params^[0])^);
end;

procedure Lape_RecvSocketEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_RecvSocketEx(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_SendSocket(const Params: PParamArray); lape_extdecl
begin
  ps_SendSocket(Pinteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_ConnectSocket(const Params: PParamArray); lape_extdecl
begin
  ps_ConnectSocket(Pinteger(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_CloseSocket(const Params: PParamArray); lape_extdecl
begin
  ps_CloseSocket(Pinteger(Params^[0])^);
end;

procedure Lape_SetSocketTimeout(const Params: PParamArray); lape_extdecl
begin
  ps_SetSocketTimeout(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_BindSocket(const Params: PParamArray); lape_extdecl
begin
  ps_BindSocket(Pinteger(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_ListenSocket(const Params: PParamArray); lape_extdecl
begin
  ps_ListenSocket(Pinteger(Params^[0])^);
end;

procedure Lape_AcceptSocket(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_AcceptSocket(Pinteger(Params^[0])^);
end;

procedure Lape_SocketInfo(const Params: PParamArray); lape_extdecl
begin
  ps_SocketInfo(Pinteger(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_CreateSocket(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := ps_CreateSocket();
end;

procedure Lape_FreeSocket(const Params: PParamArray); lape_extdecl
begin
  ps_FreeSocket(Pinteger(Params^[0])^);
end;
