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

function ps_RecvSocketStr(Client: integer): string;
begin
  Result := CurrThread.Socks.GetSocket(Client).RecvString;
end;

function ps_RecvSocket(Client: integer): string;
begin
  Result := CurrThread.Socks.GetSocket(Client).Recv;
end;

function ps_RecvSocketEx(Client, Length: integer): string;
begin
  Result := CurrThread.Socks.GetSocket(Client).RecvBufferStr(Length);
end;

procedure ps_SendSocket(Client: integer; Data: string);
begin
  CurrThread.Socks.GetSocket(Client).Send(Data);
end;

procedure ps_ConnectSocket(Client: integer; IP, Port: string);
begin
  CurrThread.Socks.GetSocket(Client).Connect(IP, Port);
end;

procedure ps_CloseSocket(Client: integer);
begin
  CurrThread.Socks.GetSocket(Client).Close;
end;

procedure ps_SetSocketTimeout(Client, Time: integer);
begin
  CurrThread.Socks.GetSocket(Client).SetTimeout(Time);
end;

procedure ps_BindSocket(Client: integer; IP, Port: string);
begin
  CurrThread.Socks.GetSocket(Client).Bind(IP, Port);
end;

procedure ps_ListenSocket(Client: integer);
begin
  CurrThread.Socks.GetSocket(Client).Listen;
end;

function ps_AcceptSocket(Client: integer): integer;
begin
  Result := CurrThread.Socks.CreateSocketEx(CurrThread.Socks.GetSocket(Client).Accept);
end;

function ps_CreateSocket: integer; extdecl;
begin
  Result := CurrThread.Socks.CreateSocket();
end;

procedure ps_FreeSocket(Client: integer); extdecl;
begin;
  CurrThread.Socks.FreeSocket(Client);
end;
