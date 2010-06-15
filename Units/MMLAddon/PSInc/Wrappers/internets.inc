procedure ps_OpenWebPage( const url : string); extdecl;
begin
  OpenWebPage(url)
end;

function ps_GetPage(const S: String): String; extdecl;
begin
  result := CurrThread.MInternet.GetPage(S);
end;

function ps_InitializeHTTPClient(HandleCookies: Boolean): Integer; extdecl;
begin
  result := CurrThread.MInternet.CreateHTTPClient(HandleCookies);
end;

procedure ps_FreeHTTPClient(Client: Integer); extdecl;
begin;
  CurrThread.MInternet.FreeHTTPClient(client);
end;

function ps_GetHTTPPage(Client: Integer;const URL: string): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(Client).GetHTTPPage(url);
end;
procedure ps_SetHTTPUserAgent(Client: Integer;const Agent: string); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(Client).SetHTTPUserAgent(agent);
end;

function ps_PostHTTPPage(Client: Integer;const Url,PostData: string): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(Client).PostHTTPPage(url,postdata);
end;

function ps_PostHTTPPageEx(Client: Integer;const Url: string): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(Client).PostHTTPPage(url);
end;

procedure ps_ClearPostData(Client: Integer); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(Client).ClearPostData;
end;

procedure ps_AddPostVariable(Client: Integer;const VarName, VarValue: string); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(Client).AddPostVariable(varname,varvalue);
end;

function ps_GetRawHeaders(Client: Integer): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(client).GetRawHeaders;
end;

procedure ps_SetProxy(Client : Integer; pHost, pPort : String); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(client).SetProxy(pHost, pPort);
end;
