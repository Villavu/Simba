unit simbascript.import_web;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Web(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  lclintf, simba.internet;

procedure Lape_OpenWebPage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OpenURL(PString(Params^[1])^);
end;

procedure Lape_GetPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetPage(PString(Params^[1])^);
end;

procedure Lape_InitializeHTTPClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.CreateHTTPClient(PBoolean(Params^[1])^);
end;

procedure Lape_FreeHTTPClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.FreeHTTPClient(PInt32(Params^[1])^);
end;

procedure Lape_GetHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).GetHTTPPage(PString(Params^[2])^);
end;

procedure Lape_SetHTTPUserAgent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).UserAgent := PString(Params^[2])^;
end;

procedure Lape_PostHTTPPage(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).PostHTTPPage(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_PostHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).PostHTTPPage(PString(Params^[2])^);
end;

procedure Lape_ClearPostData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).ClearPostData();
end;

procedure Lape_AddPostVariable(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).AddPostVariable(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_GetRawHeaders(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).Headers;
end;

procedure Lape_SetProxy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).SetProxy(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_RecvSocketStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).RecvString();
end;

procedure Lape_RecvSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Recv();
end;

procedure Lape_RecvSocketEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).RecvBufferStr(PInt32(Params^[2])^);
end;

procedure Lape_SendSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Send(PString(Params^[2])^);
end;

procedure Lape_ConnectSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Connect(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_CloseSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Close();
end;

procedure Lape_SetSocketTimeout(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).SetTimeout(PInt32(Params^[2])^);
end;

procedure Lape_BindSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Bind(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_ListenSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Listen();
end;

procedure Lape_AcceptSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TSimbaScript(Params^[0]).Client do
    PInt32(Result)^ := MSockets.CreateSocketEx(MSockets.GetSocket(PInt32(Params^[1])^).Accept);
end;

procedure Lape_SocketInfo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   TSimbaScript(Params^[0]).Client.MSockets.GetSocket(PInt32(Params^[1])^).Info(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_CreateSocket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TSimbaScript(Params^[0]).Client.MSockets.CreateSocket();
end;

procedure Lape_FreeSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MSockets.FreeSocket(PInt32(Params^[1])^);
end;

procedure Lape_GetHTTPResponseCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).ResponseCode;
end;

procedure Lape_GetHTTPPageEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).GetHTTPPage(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_GetHTTPUserAgent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).UserAgent;
end;

procedure Lape_SetHTTPContentType(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).RequestContentType := PString(Params^[2])^;
end;

procedure Lape_SetHTTPHeader(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).RequestHeader[PString(Params^[2])^] := PString(Params^[3])^;
end;

procedure Lape_FormPost(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).FormPost(PString(Params^[2])^, PString(Params^[3])^, PString(Params^[4])^);
end;

procedure Lape_FormPostEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PStream = ^TStream;
begin
  PString(Result)^ := TSimbaScript(Params^[0]).Client.MInternets.GetHTTPClient(PInt32(Params^[1])^).FormPost(PString(Params^[2])^, PString(Params^[3])^, PString(Params^[4])^, PStream(Params^[5])^);
end;

procedure Lape_Import_Web(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Web';

    addGlobalMethod('procedure OpenWebPage(URL: String);', @Lape_OpenWebPage, Data);
    addGlobalMethod('function GetPage(URL: String): String', @Lape_GetPage, Data);
    addGlobalMethod('function InitializeHTTPClient(HandleCookies: Boolean = True): Int32', @Lape_InitializeHTTPClient, Data);
    addGlobalMethod('procedure FreeHTTPClient(Client: Int32);', @Lape_FreeHTTPClient, Data);
    addGlobalMethod('function GetHTTPResponseCode(Client: Int32): Int32;', @Lape_GetHTTPResponseCode, Data);
    addGlobalMethod('function GetHTTPPage(Client: Int32; URL: String): String', @Lape_GetHTTPPage, Data);
    addGlobalMethod('procedure SetHTTPUserAgent(Client: Int32; Agent: String);', @Lape_SetHTTPUserAgent, Data);
    addGlobalMethod('function GetHTTPUserAgent(Client: Int32): String;', @Lape_GetHTTPUserAgent, Data);
    addGlobalMethod('function GetHTTPPageEx(Client: Int32; URL: String; FilePath: String): Int32;', @Lape_GetHTTPPageEx, Data);
    addGlobalMethod('function PostHTTPPage(Client: Int32; URL, PostData: String): String', @Lape_PostHTTPPage, Data);
    addGlobalMethod('function PostHTTPPageEx(Client: Int32; URL: String): String', @Lape_PostHTTPPageEx, Data);
    addGlobalMethod('procedure SetHTTPContentType(Client: Int32; Value: String);', @Lape_SetHTTPContentType, Data);
    addGlobalMethod('procedure SetHTTPHeader(Client: Int32; Name: String; Value: String);', @Lape_SetHTTPHeader, Data);
    addGlobalMethod('function PostHTTPForm(Client: Int32; const URL, FieldName, FileName: string): String; overload;', @Lape_FormPost, Data);
    addGlobalMethod('function PostHTTPForm(Client: Int32; const URL, FieldName, FileName: string; Stream: TStream): String; overload;', @Lape_FormPostEx, Data);

    addGlobalMethod('procedure ClearPostData(Client: Int32);', @Lape_ClearPostData, Data);
    addGlobalMethod('procedure AddPostVariable(Client: Int32; VariableName, VariableValue: String);', @Lape_AddPostVariable, Data);
    addGlobalMethod('function GetRawHeaders(Client: Int32): String', @Lape_GetRawHeaders, Data);
    addGlobalMethod('procedure SetProxy(Client: Int32; Host, Port: String);', @Lape_SetProxy, Data);
    addGlobalMethod('function RecvSocketStr(Client: Int32): string', @Lape_RecvSocketStr, Data);
    addGlobalMethod('function RecvSocket(Client: Int32): string', @Lape_RecvSocket, Data);
    addGlobalMethod('function RecvSocketEx(Client, Length: Int32): string', @Lape_RecvSocketEx, Data);
    addGlobalMethod('procedure SendSocket(Client: Int32; Data: string);', @Lape_SendSocket, Data);
    addGlobalMethod('procedure ConnectSocket(Client: Int32; IP, Port: string);', @Lape_ConnectSocket, Data);
    addGlobalMethod('procedure CloseSocket(Client: Int32);', @Lape_CloseSocket, Data);
    addGlobalMethod('procedure SetSocketTimeout(Client, Time: Int32);', @Lape_SetSocketTimeout, Data);
    addGlobalMethod('procedure BindSocket(Client: Int32; IP, Port: string);', @Lape_BindSocket, Data);
    addGlobalMethod('procedure ListenSocket(Client: Int32);', @Lape_ListenSocket, Data);
    addGlobalMethod('function AcceptSocket(Client: Int32): Int32', @Lape_AcceptSocket, Data);
    addGlobalMethod('procedure SocketInfo(Client: Int32; out IP, Port: string);', @Lape_SocketInfo, Data);
    addGlobalMethod('function CreateSocket: Int32', @Lape_CreateSocket, Data);
    addGlobalMethod('procedure FreeSocket(Client: Int32);', @Lape_FreeSocket, Data);
  end;
end;

end.
