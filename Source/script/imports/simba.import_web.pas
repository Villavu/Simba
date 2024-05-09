unit simba.import_web;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportWeb(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi,
  simba.nativeinterface, simba.httpclient, simba.internetsocket, simba.openssl, simba.misc;

type
  PSimbaHTTPClient = ^TSimbaHTTPClient;
  PHTTPStatus = ^EHTTPStatus;

(*
Web
===
Internet HTTP request/post methods.

- There is a pre-defined variable `HTTPClient` to use.
*)

(*
TSimbaHTTPClient.Create
-----------------------
> function TSimbaHTTPClient.Create: TSimbaHTTPClient; static;

Create a SimbaHTTPClient. This must be freed.

Example:

```
var
  AnotherHTTPClient: TSimbaHTTPClient;
begin
  AnotherHTTPClient := TSimbaHTTPClient.Create();
  WriteLn AnotherHTTPClient.Get('www.google.com');
  AnotherHTTPClient.Free();
end;
```
*)
procedure _LapeSimbaHTTPClient_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Result)^ := TSimbaHTTPClient.Create();
end;

(*
TSimbaHTTPClient.CreateWithProxy
--------------------------------
> function TSimbaHTTPClient.CreateWithProxy(Host: String; User: String = ''; Pass: String = ''): THTTPClient; static;

Variant which uses a proxy for all connections.
*)
procedure _LapeSimbaHTTPClient_CreateWithProxy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Result)^ := TSimbaHTTPClient.CreateWithProxy(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.OnDownloadProgress
-----------------------------------
> property TSimbaHTTPClient.OnDownloadProgress: TSimbaHTTPDownloadingEvent;
*)
procedure _LapeSimbaHTTPClient_OnDownloadProgress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaHTTPDownloadingEvent(Result^) := PSimbaHTTPClient(Params^[0])^.OnDownloadProgress;
end;

procedure _LapeSimbaHTTPClient_OnDownloadProgress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.OnDownloadProgress := TSimbaHTTPDownloadingEvent(Params^[1]^);
end;

(*
TSimbaHTTPClient.OnExtractProgress
----------------------------------
> property TSimbaHTTPClient.OnExtractProgress: TSimbaHTTPExtractingEvent;
*)
procedure _LapeSimbaHTTPClient_OnExtractProgress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaHTTPExtractingEvent(Result^) := PSimbaHTTPClient(Params^[0])^.OnExtractProgress;
end;

procedure _LapeSimbaHTTPClient_OnExtractProgress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.OnExtractProgress := TSimbaHTTPExtractingEvent(Params^[1]^);
end;

(*
TSimbaHTTPClient.ConnectTimeout
-------------------------------
> property TSimbaHTTPClient.ConnectTimeout: Integer;
*)
procedure _LapeSimbaHTTPClient_ConnectTimeout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaHTTPClient(Params^[0])^.ConnectTimeout;
end;

procedure _LapeSimbaHTTPClient_ConnectTimeout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.ConnectTimeout := PInteger(Params^[1])^;
end;

(*
TSimbaHTTPClient.ReadWriteTimeout
------------------------
> property TSimbaHTTPClient.ReadWriteTimeout: Integer;
*)
procedure _LapeSimbaHTTPClient_ReadWriteTimeout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaHTTPClient(Params^[0])^.ReadWriteTimeout;
end;

procedure _LapeSimbaHTTPClient_ReadWriteTimeout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.ReadWriteTimeout := PInteger(Params^[1])^;
end;

(*
TSimbaHTTPClient.Cookies
------------------------
> property TSimbaHTTPClient.Cookies: TStringArray;
*)
procedure _LapeSimbaHTTPClient_Cookies_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaHTTPClient(Params^[0])^.Cookies.ToStringArray();
end;

procedure _LapeSimbaHTTPClient_Cookies_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.Cookies.AddStrings(PStringArray(Params^[1])^, True);
end;

(*
TSimbaHTTPClient.ResponseStatus
-------------------------------
> property TSimbaHTTPClient.ResponseStatus: EHTTPStatus;

Returns the response status of the last response.

```
  if (HTTPClient.ResponseStatus = EHTTPStaus.OK) then
    WriteLn('Response status was OK!')
```
*)
procedure _LapeSimbaHTTPClient_ResponseStatus_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseStatus;
end;

(*
TSimbaHTTPClient.ResponseHeaders
--------------------------------
> function TSimbaHTTPClient.ResponseHeaders: TStringArray;
*)
procedure _LapeSimbaHTTPClient_ResponseHeaders_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseHeaders.ToStringArray();
end;

(*
TSimbaHTTPClient.ResponseHeader
-------------------------------
> property TSimbaHTTPClient.ResponseHeader[Name: String]: String;
*)
procedure _LapeSimbaHTTPClient_ResponseHeader_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseHeader[PString(Params^[1])^];
end;

(*
TSimbaHTTPClient.RequestHeader
------------------------------
> property TSimbaHTTPClient.RequestHeader[Name: String]: String;
*)
procedure _LapeSimbaHTTPClient_RequestHeader_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.RequestHeader[PString(Params^[1])^];
end;

procedure _LapeSimbaHTTPClient_RequestHeader_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.RequestHeader[PString(Params^[1])^] := PString(Params^[2])^;
end;

(*
TSimbaHTTPClient.Reset
----------------------
> procedure TSimbaHTTPClient.Reset;
*)
procedure _LapeSimbaHTTPClient_Reset(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.Reset();
end;

(*
TSimbaHTTPClient.Get
--------------------
> function TSimbaHTTPClient.Get(URL: String): String;

Return a webpages content as a string.
*)
procedure _LapeSimbaHTTPClient_Get(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Get(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.GetJson
------------------------
> function TSimbaHTTPClient.GetJson(URL: String): TSimbaJSONParser;
*)
procedure _LapeSimbaHTTPClient_GetJson(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaHTTPClient(Params^[0])^.GetJson(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.GetFile
------------------------
> procedure TSimbaHTTPClient.GetFile(URL, LocalFileName: String);

Save a webpages content to a local file.
*)
procedure _LapeSimbaHTTPClient_GetFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.GetFile(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.GetZip
-----------------------
> procedure TSimbaHTTPClient.GetZip(URL: String; OutputPath: String);
*)
procedure _LapeSimbaHTTPClient_GetZIP(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.GetZip(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Head
---------------------
> function TSimbaHTTPClient.Head(URL: String): EHTTPStatus;

Header request. Headers will be written to `HTTPClient.GetResponseHeaders()`
*)
procedure _LapeSimbaHTTPClient_Head(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PSimbaHTTPClient(Params^[0])^.Head(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.Post
---------------------
> function TSimbaHTTPClient.Post(URL: String; Data: String): String;

HTTP post request.

- `Data` is sent in request body.
*)
procedure _LapeSimbaHTTPClient_Post(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Post(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Patch
----------------------
> function TSimbaHTTPClient.Patch(URL, Data: String): String;
*)
procedure _LapeSimbaHTTPClient_Patch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Patch(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Put
--------------------
> function TSimbaHTTPClient.Put(URL, Data: String): String;
*)
procedure _LapeSimbaHTTPClient_Put(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Put(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Delete
-----------------------
> function TSimbaHTTPClient.Delete(URL, Data: String): String;
*)
procedure _LapeSimbaHTTPClient_Delete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Delete(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Options
-----------------------
> function TSimbaHTTPClient.Options(URL, Data: String): String;
*)
procedure _LapeSimbaHTTPClient_Options(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Options(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.PostForm
-------------------------
> function TSimbaHTTPClient.PostForm(URL: String; Data: String): String;

Post form data (www-urlencoded)
*)
procedure _LapeSimbaHTTPClient_PostForm(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.PostForm(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.PostFormFile
-----------------------------
> function TSimbaHTTPClient.PostFormFile(const URL, FieldName, FileName: string): String;

Post form with a local file file
*)
procedure _LapeSimbaHTTPClient_PostFormFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.PostFormFile(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
URLOpenInBrowser
----------------
> procedure URLOpenInBrowser(URL: String);

Opens a URL in the systems default internet browser.
*)
procedure _LapeURLOpenInBrowser(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.OpenURL(PString(Params^[0])^);
end;

(*
URLFetch
--------
> function URLFetch(URL: String): String;

Simple method to return the contents of a webpage.
*)
procedure _LapeURLFetch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := URLFetch(PString(Params^[0])^);
end;

(*
URLFetchToFile
--------------
> function URLFetchToFile(URL, FileName: String): Boolean;

Simple method to download the contents of a webpage to a file.
*)
procedure _LapeURLFetchToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := URLFetchToFile(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
URLEncode
---------
> function URLEncode(S: String): String;

URL encode a string. For example a space character is changed to `%20`.
*)
procedure _LapeURLEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := URLEncode(PString(Params^[0])^);
end;

(*
URLDecode
---------
> function URLDecode(S: String): String;

Inverse of EncodeURLElement.
*)
procedure _LapeURLDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := URLDecode(PString(Params^[0])^);
end;

(*
TSimbaInternetSocket.Create
---------------------------
> function TSimbaInternetSocket.Create(AHost: String; APort: UInt16; UseSSL: Boolean): TSimbaInternetSocket; static;

Basic internet socket functionality.
The socket is blocking which means Read calls will wait for data to arrive.

Use either SetReadWriteTimeout() / HasData() / ReadStringUntil() to avoid hanging.
*)
procedure _LapeSimbaInternetSocket_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInternetSocket(Result)^ := TSimbaInternetSocket.Create(PString(Params^[0])^, PUInt16(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaInternetSocket.Connect
----------------------------
> procedure TSimbaInternetSocket.Connect;

Connects to the host and port.
*)
procedure _LapeSimbaInternetSocket_Connect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInternetSocket(Params^[0])^.Connect();
end;

(*
TSimbaInternetSocket.HasData
----------------------------
> function TSimbaInternetSocket.HasData: Boolean;

Returns true if there is data waiting to be read.
*)
procedure _LapeSimbaInternetSocket_HasData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaInternetSocket(Params^[0])^.HasData();
end;

(*
TSimbaInternetSocket.ReadString
-------------------------------
> function TSimbaInternetSocket.ReadString(MaxLen: Integer = 8192): String;

Read a string from the socket up to `MaxLen` bytes.
*)
procedure _LapeSimbaInternetSocket_ReadString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaInternetSocket(Params^[0])^.ReadString(PInteger(Params^[1])^);
end;

(*
TSimbaInternetSocket.ReadStringUntil
------------------------------------
> function TSimbaInternetSocket.ReadStringUntil(Seq: String; Timeout: Integer): String;

Reads a string until the data ends with `Seq` or `Timeout` (in milliseconds) is reached.
This is useful if you are reading data which is terminated with consistent endings.
*)
procedure _LapeSimbaInternetSocket_ReadStringUntil(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaInternetSocket(Params^[0])^.ReadStringUntil(PString(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaInternetSocket.WriteString
--------------------------------
> function TSimbaInternetSocket.WriteString(Str: String): Integer;

Write a string to the socket.
*)
procedure _LapeSimbaInternetSocket_WriteString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaInternetSocket(Params^[0])^.WriteString(PString(Params^[1])^);
end;

(*
TSimbaInternetSocket.GetReadWriteTimeout
----------------------------------------
> function TSimbaInternetSocket.GetReadWriteTimeout: Integer

Returns the timeout (in milliseconds) on Read/Write operations.
*)
procedure _LapeSimbaInternetSocket_GetReadWriteTimeout(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaInternetSocket(Params^[0])^.ReadWriteTimeout;
end;

(*
TSimbaInternetSocket.SetReadWriteTimeout
----------------------------------------
> function TSimbaInternetSocket.SetReadWriteTimeout(Value: Integer)

Sets the timeout (in milliseconds) on Read/Write operations.
*)
procedure _LapeSimbaInternetSocket_SetReadWriteTimeout(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInternetSocket(Params^[0])^.ReadWriteTimeout := PInteger(Params^[1])^;
end;

(*
TSimbaInternetSocket.GetConnectTimeout
--------------------------------------
> function TSimbaInternetSocket.GetConnectTimeout: Integer;

Returns the connect timeout (in milliseconds).
*)
procedure _LapeSimbaInternetSocket_GetConnectTimeout(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaInternetSocket(Params^[0])^.ConnectTimeout;
end;

(*
TSimbaInternetSocket.SetConnectTimeout
--------------------------------------
> function TSimbaInternetSocket.SetConnectTimeout(Value: Integer);

Sets the connect timeout (in milliseconds).
*)
procedure _LapeSimbaInternetSocket_SetConnectTimeout(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInternetSocket(Params^[0])^.ConnectTimeout := PInteger(Params^[1])^;
end;

(*
TSimbaInternetSocket.LastError
------------------------------
> function TSimbaInternetSocket.LastError: Integer;

Returns the sockets last error code.
*)
procedure _LapeSimbaInternetSocket_LastError(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaInternetSocket(Params^[0])^.LastError;
end;

(*
LoadSSL
-------
> function LoadSSL(Debug: Boolean = False): Boolean;

Loads SSL. This is automatically done on demand but is useful for debugging errors relating to loading openssl.
*)
procedure _LapeLoadSSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := LoadSSL(PBoolean(Params^[0])^);
end;

procedure ImportWeb(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Web';

    addGlobalType(specialize GetEnumDecl<EHTTPStatus>(True, True), 'EHTTPStatus');
    addGlobalFunc(
      'function EHTTPStatus.AsInteger: Integer;', [
      'begin',
      '  Result := Integer(Self);',
      'end;'
    ]);
    addGlobalFunc(
      'function EHTTPStatus.AsString: String;', [
      'begin',
      '  Result := Copy(ToString(Self), 13);',
      'end;'
    ]);

    addClass('THTTPClient');

    addGlobalType('procedure(Sender: THTTPClient; URL, ContentType: String; Position, Size: Int64) of object', 'THTTPDownloadingEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: THTTPClient; URL: String; Percent: Double) of object', 'THTTPExtractingEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('function THTTPClient.Create: THTTPClient; static;', @_LapeSimbaHTTPClient_Create);
    addGlobalFunc('function THTTPClient.CreateWithProxy(Proxy: String; User: String = ''; Pass: String = ''): THTTPClient; static;', @_LapeSimbaHTTPClient_CreateWithProxy);

    addProperty('THTTPClient', 'OnDownloadProgress', 'THTTPDownloadingEvent', @_LapeSimbaHTTPClient_OnDownloadProgress_Read, @_LapeSimbaHTTPClient_OnDownloadProgress_Write);
    addProperty('THTTPClient', 'OnExtractProgress', 'THTTPExtractingEvent', @_LapeSimbaHTTPClient_OnExtractProgress_Read, @_LapeSimbaHTTPClient_OnExtractProgress_Write);

    addProperty('THTTPClient', 'ReadWriteTimeout', 'Integer', @_LapeSimbaHTTPClient_ReadWriteTimeout_Read, @_LapeSimbaHTTPClient_ReadWriteTimeout_Write);
    addProperty('THTTPClient', 'ConnectTimeout', 'Integer', @_LapeSimbaHTTPClient_ConnectTimeout_Read, @_LapeSimbaHTTPClient_ConnectTimeout_Write);
    addProperty('THTTPClient', 'Cookies', 'TStringArray', @_LapeSimbaHTTPClient_Cookies_Read, @_LapeSimbaHTTPClient_Cookies_Write);
    addProperty('THTTPClient', 'ResponseStatus', 'EHTTPStatus', @_LapeSimbaHTTPClient_ResponseStatus_Read);
    addProperty('THTTPClient', 'ResponseHeaders', 'TStringArray', @_LapeSimbaHTTPClient_ResponseHeaders_Read);
    addPropertyIndexed('THTTPClient', 'RequestHeader', 'Name: String', 'String', @_LapeSimbaHTTPClient_RequestHeader_Read, @_LapeSimbaHTTPClient_RequestHeader_Write);
    addPropertyIndexed('THTTPClient', 'ResponseHeader', 'Name: String', 'String', @_LapeSimbaHTTPClient_ResponseHeader_Read);

    addGlobalFunc('procedure THTTPClient.Reset', @_LapeSimbaHTTPClient_Reset);

    addGlobalFunc('function THTTPClient.Get(URL: String): String', @_LapeSimbaHTTPClient_Get);
    addGlobalFunc('function THTTPClient.GetJson(URL: String): TJsonParser', @_LapeSimbaHTTPClient_GetJson);
    addGlobalFunc('procedure THTTPClient.GetFile(URL, LocalFileName: String)', @_LapeSimbaHTTPClient_GetFile);
    addGlobalFunc('procedure THTTPClient.GetZip(URL: String; OutputPath: String)', @_LapeSimbaHTTPClient_GetZIP);
    addGlobalFunc('function THTTPClient.Head(URL: String): EHTTPStatus', @_LapeSimbaHTTPClient_Head);

    addGlobalFunc('function THTTPClient.Post(URL: String; Data: String): String', @_LapeSimbaHTTPClient_Post);
    addGlobalFunc('function THTTPClient.Patch(URL, PostData: String): String', @_LapeSimbaHTTPClient_Patch);
    addGlobalFunc('function THTTPClient.Put(URL, PostData: String): String', @_LapeSimbaHTTPClient_Put);
    addGlobalFunc('function THTTPClient.Delete(URL, PostData: String): String', @_LapeSimbaHTTPClient_Delete);
    addGlobalFunc('function THTTPClient.Options(URL, PostData: String): String', @_LapeSimbaHTTPClient_Options);

    addGlobalFunc('function THTTPClient.PostForm(URL: String; Data: String): String', @_LapeSimbaHTTPClient_PostForm);
    addGlobalFunc('function THTTPClient.PostFormFile(const URL, FieldName, FileName: string): String', @_LapeSimbaHTTPClient_PostFormFile);

    addGlobalFunc('procedure URLOpenInBrowser(URL: String)', @_LapeURLOpenInBrowser);
    addGlobalFunc('function URLFetch(URL: String): String', @_LapeURLFetch);
    addGlobalFunc('procedure URLFetchToFile(URL, FileName: String)', @_LapeURLFetchToFile);

    addGlobalFunc('function URLEncode(S: String): String', @_LapeURLEncode);
    addGlobalFunc('function URLDecode(S: String): String', @_LapeURLDecode);

    with addGlobalVar('THTTPClient', nil, 'HTTPClient') do
    begin
      TSimbaHTTPClient(Ptr^) := TSimbaHTTPClient.Create();
      TSimbaHTTPClient(Ptr^).FreeOnTerminate := True
    end;

    ImportingSection := '';

    addClass('TInternetSocket');
    addGlobalFunc('function TInternetSocket.Create(AHost: String; APort: UInt16; UseSSL: Boolean = False): TInternetSocket; static;', @_LapeSimbaInternetSocket_Create);
    addGlobalFunc('procedure TInternetSocket.Connect;', @_LapeSimbaInternetSocket_Connect);
    addGlobalFunc('function TInternetSocket.HasData: Boolean', @_LapeSimbaInternetSocket_HasData);
    addGlobalFunc('function TInternetSocket.ReadString(MaxLen: Integer = 8192): String;', @_LapeSimbaInternetSocket_ReadString);
    addGlobalFunc('function TInternetSocket.ReadStringUntil(Seq: String; Timeout: Integer): String;', @_LapeSimbaInternetSocket_ReadStringUntil);
    addGlobalFunc('function TInternetSocket.WriteString(Str: String): Integer;', @_LapeSimbaInternetSocket_WriteString);
    addGlobalFunc('function TInternetSocket.LastError: Integer;', @_LapeSimbaInternetSocket_LastError);
    addProperty('TInternetSocket', 'ConnectTimeout', 'Integer', @_LapeSimbaInternetSocket_GetConnectTimeout, @_LapeSimbaInternetSocket_SetConnectTimeout);
    addProperty('TInternetSocket', 'ReadWriteTimeout', 'Integer', @_LapeSimbaInternetSocket_GetReadWriteTimeout, @_LapeSimbaInternetSocket_SetReadWriteTimeout);

    addGlobalFunc('function LoadSSL(Debug: Boolean = False): Boolean', @_LapeLoadSSL);
  end;
end;

end.
