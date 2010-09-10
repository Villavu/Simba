procedure RUTIS_ps_OpenWebPage(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_OpenWebPage(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetPage(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PString(Result^.Data)^ := ps_GetPage(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_InitializeHTTPClient(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_InitializeHTTPClient(PBoolean(Params^[0].Data)^);
end;

procedure RUTIS_ps_FreeHTTPClient(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FreeHTTPClient(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetHTTPPage(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_GetHTTPPage(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_SetHTTPUserAgent(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetHTTPUserAgent(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_PostHTTPPage(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_PostHTTPPage(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^, Pstring(Params^[2].Data)^);
end;

procedure RUTIS_ps_PostHTTPPageEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_PostHTTPPageEx(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_ClearPostData(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ClearPostData(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_AddPostVariable(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_AddPostVariable(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^, Pstring(Params^[2].Data)^);
end;

procedure RUTIS_ps_GetRawHeaders(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_GetRawHeaders(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_SetProxy(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetProxy(PInteger(Params^[0].Data)^, PString(Params^[1].Data)^, PString(Params^[2].Data)^);
end;
