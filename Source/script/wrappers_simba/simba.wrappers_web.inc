procedure _LapeOpenWebPage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OpenURL(PString(Params^[0])^);
end;

procedure _LapeGetPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetPage(PString(Params^[0])^);
end;

procedure _LapeInitializeHTTPClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MInternets.CreateHTTPClient(PBoolean(Params^[0])^);
end;

procedure _LapeFreeHTTPClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.FreeHTTPClient(PInt32(Params^[0])^);
end;

procedure _LapeGetHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).GetHTTPPage(PString(Params^[1])^);
end;

procedure _LapeSetHTTPUserAgent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).UserAgent := PString(Params^[1])^;
end;

procedure _LapePostHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).PostHTTPPage(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePostHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).PostHTTPPage(PString(Params^[1])^);
end;

procedure _LapeClearPostData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).ClearPostData();
end;

procedure _LapeAddPostVariable(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).AddPostVariable(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeGetRawHeaders(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).Headers;
end;

procedure _LapeSetProxy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).SetProxy(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeRecvSocketStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).RecvString();
end;

procedure _LapeRecvSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Recv();
end;

procedure _LapeRecvSocketEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).RecvBufferStr(PInt32(Params^[1])^);
end;

procedure _LapeSendSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Send(PString(Params^[1])^);
end;

procedure _LapeConnectSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Connect(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeCloseSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Close();
end;

procedure _LapeSetSocketTimeout(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).SetTimeout(PInt32(Params^[1])^);
end;

procedure _LapeBindSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Bind(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeListenSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Listen();
end;

procedure _LapeAcceptSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MSockets.CreateSocketEx(MSockets.GetSocket(PInt32(Params^[0])^).Accept);
end;

procedure _LapeSocketInfo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   SimbaScript.Client.MSockets.GetSocket(PInt32(Params^[0])^).Info(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeCreateSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MSockets.CreateSocket();
end;

procedure _LapeFreeSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MSockets.FreeSocket(PInt32(Params^[0])^);
end;

procedure _LapeGetHTTPResponseCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).ResponseCode;
end;

procedure _LapeGetHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).GetHTTPPage(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeGetHTTPUserAgent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).UserAgent;
end;

procedure _LapeSetHTTPContentType(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).RequestContentType := PString(Params^[1])^;
end;

procedure _LapeSetHTTPHeader(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).RequestHeader[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeFormPost(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).FormPost(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeFormPostEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PStream = ^TStream;
begin
  PString(Result)^ := SimbaScript.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).FormPost(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^, PStream(Params^[4])^);
end;

