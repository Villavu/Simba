unit simbascript.import_web;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

implementation

uses
  lclintf, simba.internet;

procedure Lape_OpenWebPage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OpenURL(PString(Params^[0])^);
end;

procedure Lape_GetPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetPage(PString(Params^[0])^);
end;

procedure Lape_InitializeHTTPClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.MInternets.CreateHTTPClient(PBoolean(Params^[0])^);
end;

procedure Lape_FreeHTTPClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MInternets.FreeHTTPClient(PInt32(Params^[0])^);
end;

procedure Lape_GetHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).GetHTTPPage(PString(Params^[1])^);
end;

procedure Lape_SetHTTPUserAgent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).UserAgent := PString(Params^[1])^;
end;

procedure Lape_PostHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).PostHTTPPage(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_PostHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).PostHTTPPage(PString(Params^[1])^);
end;

procedure Lape_ClearPostData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).ClearPostData();
end;

procedure Lape_AddPostVariable(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).AddPostVariable(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_GetRawHeaders(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).Headers;
end;

procedure Lape_SetProxy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).SetProxy(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_RecvSocketStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).RecvString();
end;

procedure Lape_RecvSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Recv();
end;

procedure Lape_RecvSocketEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).RecvBufferStr(PInt32(Params^[1])^);
end;

procedure Lape_SendSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Send(PString(Params^[1])^);
end;

procedure Lape_ConnectSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Connect(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_CloseSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Close();
end;

procedure Lape_SetSocketTimeout(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).SetTimeout(PInt32(Params^[1])^);
end;

procedure Lape_BindSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Bind(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_ListenSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Listen();
end;

procedure Lape_AcceptSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with Script.Client do
    PInt32(Result)^ := MSockets.CreateSocketEx(MSockets.GetSocket(PInt32(Params^[0])^).Accept);
end;

procedure Lape_SocketInfo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   Script.Client.MSockets.GetSocket(PInt32(Params^[0])^).Info(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_CreateSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.MSockets.CreateSocket();
end;

procedure Lape_FreeSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.MSockets.FreeSocket(PInt32(Params^[0])^);
end;

procedure Lape_GetHTTPResponseCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).ResponseCode;
end;

procedure Lape_GetHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).GetHTTPPage(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_GetHTTPUserAgent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := Script.Client.MInternets.GetHTTPClient(PInt32(Params^[0])^).UserAgent;
end;

procedure Lape_Import_Web(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    Section := 'Web';

    addGlobalFunc('procedure OpenWebPage(URL: String);', @Lape_OpenWebPage);
    addGlobalFunc('function GetPage(URL: String): String', @Lape_GetPage);
    addGlobalFunc('function InitializeHTTPClient(HandleCookies: Boolean = True): Int32', @Lape_InitializeHTTPClient);
    addGlobalFunc('procedure FreeHTTPClient(Client: Int32);', @Lape_FreeHTTPClient);
    addGlobalFunc('function GetHTTPResponseCode(Client: Int32): Int32;', @Lape_GetHTTPResponseCode);
    addGlobalFunc('function GetHTTPPage(Client: Int32; URL: String): String', @Lape_GetHTTPPage);
    addGlobalFunc('procedure SetHTTPUserAgent(Client: Int32; Agent: String);', @Lape_SetHTTPUserAgent);
    addGlobalFunc('function GetHTTPUserAgent(Client: Int32): String;', @Lape_GetHTTPUserAgent);
    addGlobalFunc('function GetHTTPPageEx(Client: Int32; URL: String; FilePath: String): Int32;', @Lape_GetHTTPPageEx);
    addGlobalFunc('function PostHTTPPage(Client: Int32; URL, PostData: String): String', @Lape_PostHTTPPage);
    addGlobalFunc('function PostHTTPPageEx(Client: Int32; URL: String): String', @Lape_PostHTTPPageEx);
    addGlobalFunc('procedure ClearPostData(Client: Int32);', @Lape_ClearPostData);
    addGlobalFunc('procedure AddPostVariable(Client: Int32; VariableName, VariableValue: String);', @Lape_AddPostVariable);
    addGlobalFunc('function GetRawHeaders(Client: Int32): String', @Lape_GetRawHeaders);
    addGlobalFunc('procedure SetProxy(Client: Int32; Host, Port: String);', @Lape_SetProxy);
    addGlobalFunc('function RecvSocketStr(Client: Int32): string', @Lape_RecvSocketStr);
    addGlobalFunc('function RecvSocket(Client: Int32): string', @Lape_RecvSocket);
    addGlobalFunc('function RecvSocketEx(Client, Length: Int32): string', @Lape_RecvSocketEx);
    addGlobalFunc('procedure SendSocket(Client: Int32; Data: string);', @Lape_SendSocket);
    addGlobalFunc('procedure ConnectSocket(Client: Int32; IP, Port: string);', @Lape_ConnectSocket);
    addGlobalFunc('procedure CloseSocket(Client: Int32);', @Lape_CloseSocket);
    addGlobalFunc('procedure SetSocketTimeout(Client, Time: Int32);', @Lape_SetSocketTimeout);
    addGlobalFunc('procedure BindSocket(Client: Int32; IP, Port: string);', @Lape_BindSocket);
    addGlobalFunc('procedure ListenSocket(Client: Int32);', @Lape_ListenSocket);
    addGlobalFunc('function AcceptSocket(Client: Int32): Int32', @Lape_AcceptSocket);
    addGlobalFunc('procedure SocketInfo(Client: Int32; out IP, Port: string);', @Lape_SocketInfo);
    addGlobalFunc('function CreateSocket: Int32', @Lape_CreateSocket);
    addGlobalFunc('procedure FreeSocket(Client: Int32);', @Lape_FreeSocket);
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_Web);

end.
