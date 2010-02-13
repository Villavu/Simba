function ps_GetPage(S: String): String; extdecl;
begin
  result := GetPage(S);
end;

function ps_InitializeHTTPClient(HandleCookies, HandleRedirects: Boolean): Integer; extdecl;
begin
  result := CurrThread.MInternet.CreateHTTPClient(HandleCookies); extdecl;
end;

procedure ps_FreeHTTPClient(Client: Integer); extdecl;
begin;
  CurrThread.MInternet.GetHTTPClient(Client).Free;
end;

function ps_GetHTTPPage(Client: Integer; URL: string): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(Client).GetHTTPPage(url);
end;
procedure ps_SetHTTPUserAgent(Client: Integer; Agent: string); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(Client).SetHTTPUserAgent(agent);
end;

function ps_PostHTTPPage(Client: Integer; Url: string; PostData: string): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(Client).PostHTTPPage(url,postdata);
end;

function ps_PostHTTPPageEx(Client: Integer; Url: string): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(Client).PostHTTPPage(url);
end;

procedure ps_ClearPostData(Client: Integer); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(Client).ClearPostData;
end;

procedure ps_AddPostVariable(Client: Integer; VarName, VarValue: string); extdecl;
begin
  CurrThread.MInternet.GetHTTPClient(Client).AddPostVariable(varname,varvalue);
end;

function ps_GetRawHeaders(Client: Integer): string; extdecl;
begin
  result := CurrThread.MInternet.GetHTTPClient(client).GetRawHeaders;
end;
