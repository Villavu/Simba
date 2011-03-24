{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    internets.inc for the Mufasa Macro Library
}

procedure lpOpenWebPage(Params: PParamArray);
begin
  OpenWebPage(PlpString(Params^[0])^)
end;

procedure lpGetPage(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.MInternet.GetPage(PlpString(Params^[0])^);
end;

procedure lpInitializeHTTPClient(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.MInternet.CreateHTTPClient(PBoolean(Params^[0])^);
end;

procedure lpFreeHTTPClient(Params: PParamArray);
begin;
  CurrThread.MInternet.FreeHTTPClient(PLongInt(Params^[0])^);
end;

procedure lpGetHTTPPage(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).GetHTTPPage(PlpString(Params^[1])^);
end;
procedure lpSetHTTPUserAgent(Params: PParamArray);
begin
  CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).SetHTTPUserAgent(PlpString(Params^[1])^);
end;

procedure lpPostHTTPPage(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).PostHTTPPage(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpPostHTTPPageEx(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).PostHTTPPage(PlpString(Params^[1])^);
end;

procedure lpClearPostData(Params: PParamArray);
begin
  CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).ClearPostData;
end;

procedure lpAddPostVariable(Params: PParamArray);
begin
  CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).AddPostVariable(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpGetRawHeaders(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).GetRawHeaders;
end;

procedure lpSetProxy(Params: PParamArray);
begin
  CurrThread.MInternet.GetHTTPClient(PLongInt(Params^[0])^).SetProxy(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpRecvSocketStr(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).RecvString;
end;

procedure lpRecvSocket(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Recv;
end;

procedure lpRecvSocketEx(Params: PParamArray; Result: Pointer);
begin
  PlpString(Result)^ := CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).RecvBufferStr(PLongInt(Params^[1])^);
end;

procedure lpSendSocket(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Send(PlpString(Params^[1])^);
end;

procedure lpConnectSocket(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Connect(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpCloseSocket(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Close;
end;

procedure lpSetSocketTimeout(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).SetTimeout(PLongInt(Params^[1])^);
end;

procedure lpBindSocket(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Bind(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpListenSocket(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Listen;
end;

procedure lpAcceptSocket(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Socks.CreateSocketEx(CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Accept);
end;

procedure lpSocketInfo(Params: PParamArray);
begin
  CurrThread.Socks.GetSocket(PLongInt(Params^[0])^).Info(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpCreateSocket(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Socks.CreateSocket();
end;

procedure lpFreeSocket(Params: PParamArray);
begin;
  CurrThread.Socks.FreeSocket(PLongInt(Params^[0])^);
end;
