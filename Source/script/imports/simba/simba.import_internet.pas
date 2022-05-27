unit simba.import_internet;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lclintf, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.internet;

type
  PStream = ^TStream;

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
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MInternets.CreateHTTPClient(PBoolean(Params^[0])^);
end;

procedure _LapeFreeHTTPClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.FreeHTTPClient(PInt32(Params^[0])^);
end;

procedure _LapeGetHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).GetHTTPPage(PString(Params^[1])^);
end;

procedure _LapeSetHTTPUserAgent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).UserAgent := PString(Params^[1])^;
end;

procedure _LapePostHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).PostHTTPPage(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapePostHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).PostHTTPPage(PString(Params^[1])^);
end;

procedure _LapeClearPostData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).ClearPostData();
end;

procedure _LapeAddPostVariable(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).AddPostVariable(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeGetRawHeaders(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).Headers;
end;

procedure _LapeSetProxy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).SetProxy(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeRecvSocketStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).RecvString();
end;

procedure _LapeRecvSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Recv();
end;

procedure _LapeRecvSocketEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).RecvBufferStr(PInt32(Params^[1])^);
end;

procedure _LapeSendSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Send(PString(Params^[1])^);
end;

procedure _LapeConnectSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Connect(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeCloseSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Close();
end;

procedure _LapeSetSocketTimeout(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).SetTimeout(PInt32(Params^[1])^);
end;

procedure _LapeBindSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Bind(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeListenSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Listen();
end;

procedure _LapeAcceptSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInt32(Result)^ := MSockets.CreateSocketEx(MSockets.GetSocket(PInt32(Params^[0])^).Accept);
end;

procedure _LapeSocketInfo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   SimbaScriptThread.Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Info(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeCreateSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MSockets.CreateSocket();
end;

procedure _LapeFreeSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MSockets.FreeSocket(PInt32(Params^[0])^);
end;

procedure _LapeGetHTTPResponseCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).ResponseCode;
end;

procedure _LapeGetHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).GetHTTPPage(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeGetHTTPUserAgent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).UserAgent;
end;

procedure _LapeSetHTTPContentType(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).RequestContentType := PString(Params^[1])^;
end;

procedure _LapeSetHTTPHeader(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).RequestHeader[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeFormPost(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).FormPost(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeFormPostEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScriptThread.Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).FormPost(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^, PStream(Params^[4])^);
end;

procedure ImportInternet(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Internet');

    addGlobalFunc('procedure OpenWebPage(URL: String)', @_LapeOpenWebPage);
    addGlobalFunc('function GetPage(URL: String): String', @_LapeGetPage);
    addGlobalFunc('function InitializeHTTPClient(HandleCookies: Boolean = True): Int32', @_LapeInitializeHTTPClient);
    addGlobalFunc('procedure FreeHTTPClient(Client: Int32)', @_LapeFreeHTTPClient);
    addGlobalFunc('function GetHTTPResponseCode(Client: Int32): Int32', @_LapeGetHTTPResponseCode);
    addGlobalFunc('function GetHTTPPage(Client: Int32; URL: String): String', @_LapeGetHTTPPage);
    addGlobalFunc('procedure SetHTTPUserAgent(Client: Int32; Agent: String)', @_LapeSetHTTPUserAgent);
    addGlobalFunc('function GetHTTPUserAgent(Client: Int32): String', @_LapeGetHTTPUserAgent);
    addGlobalFunc('function GetHTTPPageEx(Client: Int32; URL: String; FilePath: String): Int32', @_LapeGetHTTPPageEx);
    addGlobalFunc('function PostHTTPPage(Client: Int32; URL, PostData: String): String', @_LapePostHTTPPage);
    addGlobalFunc('function PostHTTPPageEx(Client: Int32; URL: String): String', @_LapePostHTTPPageEx);
    addGlobalFunc('procedure SetHTTPContentType(Client: Int32; Value: String)', @_LapeSetHTTPContentType);
    addGlobalFunc('procedure SetHTTPHeader(Client: Int32; Name: String; Value: String)', @_LapeSetHTTPHeader);
    addGlobalFunc('function PostHTTPForm(Client: Int32; const URL, FieldName, FileName: string): String; overload', @_LapeFormPost);
    addGlobalFunc('function PostHTTPForm(Client: Int32; const URL, FieldName, FileName: string; Stream: TStream): String; overload', @_LapeFormPostEx);
    addGlobalFunc('procedure ClearPostData(Client: Int32)', @_LapeClearPostData);
    addGlobalFunc('procedure AddPostVariable(Client: Int32; VariableName, VariableValue: String)', @_LapeAddPostVariable);
    addGlobalFunc('function GetRawHeaders(Client: Int32): String', @_LapeGetRawHeaders);
    addGlobalFunc('procedure SetProxy(Client: Int32; Host, Port: String)', @_LapeSetProxy);
    addGlobalFunc('function RecvSocketStr(Client: Int32): string', @_LapeRecvSocketStr);
    addGlobalFunc('function RecvSocket(Client: Int32): string', @_LapeRecvSocket);
    addGlobalFunc('function RecvSocketEx(Client, Length: Int32): string', @_LapeRecvSocketEx);
    addGlobalFunc('procedure SendSocket(Client: Int32; Data: string)', @_LapeSendSocket);
    addGlobalFunc('procedure ConnectSocket(Client: Int32; IP, Port: string)', @_LapeConnectSocket);
    addGlobalFunc('procedure CloseSocket(Client: Int32)', @_LapeCloseSocket);
    addGlobalFunc('procedure SetSocketTimeout(Client, Time: Int32)', @_LapeSetSocketTimeout);
    addGlobalFunc('procedure BindSocket(Client: Int32; IP, Port: string)', @_LapeBindSocket);
    addGlobalFunc('procedure ListenSocket(Client: Int32)', @_LapeListenSocket);
    addGlobalFunc('function AcceptSocket(Client: Int32): Int32', @_LapeAcceptSocket);
    addGlobalFunc('procedure SocketInfo(Client: Int32; out IP, Port: string)', @_LapeSocketInfo);
    addGlobalFunc('function CreateSocket: Int32', @_LapeCreateSocket);
    addGlobalFunc('procedure FreeSocket(Client: Int32)', @_LapeFreeSocket);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportInternet);

end.

