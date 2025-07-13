unit simba.import_web;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportWeb(Script: TSimbaScript);

implementation

uses
  lptypes, ffi,
  simba.script_objectutil,
  simba.nativeinterface, simba.httpclient, simba.internetsocket, simba.openssl, simba.misc;

type
  PHTTPStatus = ^EHTTPStatus;
  PInternetSocket = ^TInternetSocket;
  PInternetSocketASync = ^TInternetSocketASync;
  PInternetSocketServer = ^TInternetSocketServer;

(*
Web
===
HTTP request/post methods.

- There is a pre-defined variable `HTTPClient` to use.
*)

(*
EHTTPStatus.AsInteger
---------------------
```
property EHTTPStatus.AsInteger: Integer;
```
*)
procedure _LapeHTTPStatus_AsInteger_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PHTTPStatus(Params^[0])^.AsInteger;
end;

(*
EHTTPStatus.AsString
--------------------
```
property EHTTPStatus.AsString: String;
```
*)
procedure _LapeHTTPStatus_AsString_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PHTTPStatus(Params^[0])^.AsString;
end;

(*
THTTPClient.Create
------------------
```
function THTTPClient.Construct: THTTPClient; static;
function THTTPClient.Construct(Proxy: String; Auth: String = ''): THTTPClient; static;
```

THTTPClient constructor. Use the `new` keyword to call this method like so:
```
myClient := new THTTPClient();
```
*)
procedure _LapeHTTPClient_Construct1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Result)^^ := TSimbaHTTPClient.Create();
end;

procedure _LapeHTTPClient_Construct2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Result)^^ := TSimbaHTTPClient.CreateWithProxy(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeHTTPClient_Destroy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  LapeObjectDestroy(PLapeObject(Params^[0]));
end;

(*
THTTPClient.OnDownloadProgress
------------------------------
```
property THTTPClient.OnDownloadProgress: TSimbaHTTPDownloadingEvent;
```
*)
procedure _LapeHTTPClient_OnDownloadProgress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaHTTPDownloadingEvent(Result^) := PLapeObjectHTTPClient(Params^[0])^^.OnDownloadProgress;
end;

procedure _LapeHTTPClient_OnDownloadProgress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.OnDownloadProgress := TSimbaHTTPDownloadingEvent(Params^[1]^);
end;

(*
THTTPClient.OnExtractProgress
-----------------------------
```
property THTTPClient.OnExtractProgress: TSimbaHTTPExtractingEvent;
```
*)
procedure _LapeHTTPClient_OnExtractProgress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaHTTPExtractingEvent(Result^) := PLapeObjectHTTPClient(Params^[0])^^.OnExtractProgress;
end;

procedure _LapeHTTPClient_OnExtractProgress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.OnExtractProgress := TSimbaHTTPExtractingEvent(Params^[1]^);
end;

(*
THTTPClient.ConnectTimeout
--------------------------
```
property THTTPClient.ConnectTimeout: Integer;
```
*)
procedure _LapeHTTPClient_ConnectTimeout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.ConnectTimeout;
end;

procedure _LapeHTTPClient_ConnectTimeout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.ConnectTimeout := PInteger(Params^[1])^;
end;

(*
THTTPClient.ReadWriteTimeout
----------------------------
```
property THTTPClient.ReadWriteTimeout: Integer;
```
*)
procedure _LapeHTTPClient_ReadWriteTimeout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.ReadWriteTimeout;
end;

procedure _LapeHTTPClient_ReadWriteTimeout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.ReadWriteTimeout := PInteger(Params^[1])^;
end;

(*
THTTPClient.Cookies
-------------------
```
property THTTPClient.Cookies: TStringArray;
```
*)
procedure _LapeHTTPClient_Cookies_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Cookies.ToStringArray();
end;

procedure _LapeHTTPClient_Cookies_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.Cookies.AddStrings(PStringArray(Params^[1])^, True);
end;

(*
THTTPClient.ResponseStatus
--------------------------
```
property THTTPClient.ResponseStatus: EHTTPStatus;
```

Returns the response status of the last response.

```
  if (HTTPClient.ResponseStatus = EHTTPStaus.OK) then
    WriteLn('Response status was OK!')
```
*)
procedure _LapeHTTPClient_ResponseStatus_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.ResponseStatus;
end;

(*
THTTPClient.ResponseHeaders
---------------------------
```
function THTTPClient.ResponseHeaders: TStringArray;
```
*)
procedure _LapeHTTPClient_ResponseHeaders_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.ResponseHeaders.ToStringArray();
end;

(*
THTTPClient.ResponseHeader
--------------------------
```
property THTTPClient.ResponseHeader[Name: String]: String;
```
*)
procedure _LapeHTTPClient_ResponseHeader_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.ResponseHeader[PString(Params^[1])^];
end;

(*
THTTPClient.RequestHeader
-------------------------
```
property THTTPClient.RequestHeader[Name: String]: String;
```
*)
procedure _LapeHTTPClient_RequestHeader_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.RequestHeader[PString(Params^[1])^];
end;

procedure _LapeHTTPClient_RequestHeader_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.RequestHeader[PString(Params^[1])^] := PString(Params^[2])^;
end;

(*
THTTPClient.Reset
-----------------
```
procedure THTTPClient.Reset;
```
*)
procedure _LapeHTTPClient_Reset(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.Reset();
end;

(*
THTTPClient.Get
---------------
```
function THTTPClient.Get(URL: String): String;
```

Return a webpages content as a string.
*)


procedure _LapeHTTPClient_Get(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Get(PString(Params^[1])^);
end;

(*
THTTPClient.GetJson
-------------------
```
function THTTPClient.GetJson(URL: String): TJsonItem;
```
*)
procedure _LapeHTTPClient_GetJson(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.GetJson(PString(Params^[1])^);
end;

(*
THTTPClient.GetFile
-------------------
```
procedure THTTPClient.GetFile(URL, LocalFileName: String);
```

Save a webpages content to a local file.
*)
procedure _LapeHTTPClient_GetFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.GetFile(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.GetZip
------------------
```
procedure THTTPClient.GetZip(URL: String; OutputPath: String);
```
*)
procedure _LapeHTTPClient_GetZIP(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectHTTPClient(Params^[0])^^.GetZip(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.Head
----------------
```
function THTTPClient.Head(URL: String): EHTTPStatus;
```

Header request. Headers will be written to `HTTPClient.GetResponseHeaders()`
*)
procedure _LapeHTTPClient_Head(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Head(PString(Params^[1])^);
end;

(*
THTTPClient.Post
----------------
```
function THTTPClient.Post(URL: String; Data: String): String;
```

HTTP post request.

- `Data` is sent in request body.
*)
procedure _LapeHTTPClient_Post(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Post(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.Patch
-----------------
```
function THTTPClient.Patch(URL, Data: String): String;
```
*)
procedure _LapeHTTPClient_Patch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Patch(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.Put
---------------
```
function THTTPClient.Put(URL, Data: String): String;
```
*)
procedure _LapeHTTPClient_Put(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Put(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.Delete
------------------
```
function THTTPClient.Delete(URL, Data: String): String;
```
*)
procedure _LapeHTTPClient_Delete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Delete(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.Options
-------------------
```
function THTTPClient.Options(URL, Data: String): String;
```
*)
procedure _LapeHTTPClient_Options(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.Options(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.PostForm
--------------------
```
function THTTPClient.PostForm(URL: String; Data: String): String;
```

Post form data (www-urlencoded)
*)
procedure _LapeHTTPClient_PostForm(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.PostForm(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
THTTPClient.PostFormFile
------------------------
```
function THTTPClient.PostFormFile(const URL, FieldName, FileName: string): String;
```

Post form with a local file file
*)
procedure _LapeHTTPClient_PostFormFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectHTTPClient(Params^[0])^^.PostFormFile(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
URLOpenInBrowser
----------------
```
procedure URLOpenInBrowser(URL: String);
```

Opens a URL in the systems default internet browser.
*)
procedure _LapeURLOpenInBrowser(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.OpenURL(PString(Params^[0])^);
end;

(*
URLFetch
--------
```
function URLFetch(URL: String): String;
```

Simple method to return the contents of a webpage.
*)
procedure _LapeURLFetch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := URLFetch(PString(Params^[0])^);
end;

(*
URLFetchToFile
--------------
```
function URLFetchToFile(URL, FileName: String): Boolean;
```

Simple method to download the contents of a webpage to a file.
*)
procedure _LapeURLFetchToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := URLFetchToFile(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
URLEncode
---------
```
function URLEncode(S: String): String;
```

URL encode a string. For example a space character is changed to `%20`.
*)
procedure _LapeURLEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := URLEncode(PString(Params^[0])^);
end;

(*
URLDecode
---------
```
function URLDecode(S: String): String;
```

Inverse of EncodeURLElement.
*)
procedure _LapeURLDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := URLDecode(PString(Params^[0])^);
end;

(*
TInternetSocket.Create
----------------------
```
function TInternetSocket.Create(AHost: String; APort: UInt16; UseSSL: Boolean): TInternetSocket; static;
```

Basic internet socket functionality.
The socket is blocking which means `Read` calls will wait for data to arrive.

Use either ReadWriteTimeout() / HasData() / ReadStringUntil() to avoid hanging.
*)
procedure _LapeSimbaInternetSocket_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocket(Result)^ := TInternetSocket.Create(PString(Params^[0])^, PUInt16(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TInternetSocket.Connect
-----------------------
```
procedure TInternetSocket.Connect;
```

Connects to the host and port.
*)
procedure _LapeSimbaInternetSocket_Connect(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocket(Params^[0])^.Connect();
end;

(*
TInternetSocket.Close
---------------------
```
procedure TInternetSocket.Close;
```

Closes the socket
*)
procedure _LapeSimbaInternetSocket_Close(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocket(Params^[0])^.Close();
end;

(*
TInternetSocket.HasData
-----------------------
```
function TInternetSocket.HasData: Boolean;
```

Returns true if there is data waiting to be read.
*)
procedure _LapeSimbaInternetSocket_HasData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PInternetSocket(Params^[0])^.HasData();
end;

(*
TInternetSocket.Read
--------------------
```
function TInternetSocket.Read(MaxLen: Integer = 8192): TByteArray;
```

Read bytes from the socket up to `MaxLen` bytes.

```{note}
By default this call is blocking - it will wait until there is data to be read.
To change this behavior use `ReadWriteTimeout` or use `HasData`.
```
*)
procedure _LapeSimbaInternetSocket_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := PInternetSocket(Params^[0])^.Read(PInteger(Params^[1])^);
end;

(*
TInternetSocket.ReadUntil
-------------------------
```
function TInternetSocket.ReadUntil(Seq: TByteArray; Timeout: Integer): TByteArray;
```

Reads until the data ends with `Seq` or `Timeout` (in milliseconds) is reached.
This is useful if you are reading data which is terminated with consistent endings.
*)
procedure _LapeSimbaInternetSocket_ReadUntil(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := PInternetSocket(Params^[0])^.ReadUntil(PByteArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TInternetSocket.ReadString
--------------------------
```
function TInternetSocket.ReadString(MaxLen: Integer = 8192): String;
```

ReadString a string from the socket up to `MaxLen` bytes.
*)
procedure _LapeSimbaInternetSocket_ReadString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PInternetSocket(Params^[0])^.ReadString(PInteger(Params^[1])^);
end;

(*
TInternetSocket.ReadStringUntil
-------------------------------
```
function TInternetSocket.ReadStringUntil(Seq: String; Timeout: Integer): String;
```

Reads a string until the data ends with `Seq` or `Timeout` (in milliseconds) is reached.
This is useful if you are ReadStringing data which is terminated with consistent endings.
*)
procedure _LapeSimbaInternetSocket_ReadStringUntil(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PInternetSocket(Params^[0])^.ReadStringUntil(PString(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TInternetSocket.Write
---------------------
```
function TInternetSocket.Write(Data: TByteArray): Integer;
```

Write bytes to the socket.
*)
procedure _LapeSimbaInternetSocket_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInternetSocket(Params^[0])^.Write(PByteArray(Params^[1])^);
end;

(*
TInternetSocket.WriteString
---------------------------
```
function TInternetSocket.WriteString(Str: String): Integer;
```

Write a string to the socket.
*)
procedure _LapeSimbaInternetSocket_WriteString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInternetSocket(Params^[0])^.WriteString(PString(Params^[1])^);
end;

(*
TInternetSocket.LocalAddress
----------------------------
```
property TInternetSocket.LocalAddress: String;
```
*)
procedure _LapeSimbaInternetSocket_LocalAddress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PInternetSocket(Params^[0])^.LocalAddress;
end;

(*
TInternetSocket.RemoteAddress
-----------------------------
```
property TInternetSocket.RemoteAddress: String;
```
*)
procedure _LapeSimbaInternetSocket_RemoteAddress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PInternetSocket(Params^[0])^.RemoteAddress;
end;

(*
TInternetSocket.ReadWriteTimeout
--------------------------------
```
property TInternetSocket.ReadWriteTimeout: Integer
property TInternetSocket.ReadWriteTimeout(Value: Integer)
```

Timeout (in milliseconds) on Read/Write operations.
*)
procedure _LapeSimbaInternetSocket_ReadWriteTimeout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInternetSocket(Params^[0])^.ReadWriteTimeout;
end;

procedure _LapeSimbaInternetSocket_ReadWriteTimeout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocket(Params^[0])^.ReadWriteTimeout := PInteger(Params^[1])^;
end;

(*
TInternetSocket.ConnectTimeout
------------------------------
```
property TInternetSocket.ConnectTimeout: Integer;
```
```
property TInternetSocket.ConnectTimeout(Value: Integer);
```

Connect timeout (in milliseconds).
*)
procedure _LapeSimbaInternetSocket_ConnectTimeout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInternetSocket(Params^[0])^.ConnectTimeout;
end;

procedure _LapeSimbaInternetSocket_ConnectTimeout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocket(Params^[0])^.ConnectTimeout := PInteger(Params^[1])^;
end;

(*
TInternetSocket.LastError
-------------------------
```
property TInternetSocket.LastError: Integer;
```

Returns the sockets last error code.
*)
procedure _LapeSimbaInternetSocket_LastError_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInternetSocket(Params^[0])^.LastError;
end;

(*
TInternetSocketASync.Create
---------------------------
```
function TInternetSocketASync.Create(AHost: String; APort: UInt16; UseSSL: Boolean): TInternetSocketASync; static;
```

Internet socket but runs in the background and calls the OnData callback when data arrives.
*)
procedure _LapeInternetSocketASync_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketASync(Result)^ := TInternetSocketASync.Create(PString(Params^[0])^, PUInt16(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TInternetSocketASync.OnData
---------------------------
```
property TInternetSocketASync.OnData: TSocketDataEvent;
```
```
property TInternetSocketASync.OnData(Value: TSocketDataEvent);
```
*)
procedure _LapeInternetSocketASync_DataEvent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TInternetSocketASync.TDataEvent(Result^) := PInternetSocketASync(Params^[0])^.OnData;
end;

procedure _LapeInternetSocketASync_DataEvent_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketASync(Params^[0])^.OnData := TInternetSocketASync.TDataEvent(Params^[1]^);
end;

(*
TInternetSocketASync.OnDisconnect
---------------------------------
```
property TInternetSocketASync.OnDisconnect: TSocketDisconnectEvent;
```
```
property TInternetSocketASync.OnDisconnect(Value: TSocketDisconnectEvent);
```
*)
procedure _LapeInternetSocketASync_DisconnectEvent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TInternetSocketASync.TDisconnectEvent(Result^) := PInternetSocketASync(Params^[0])^.OnDisconnect;
end;

procedure _LapeInternetSocketASync_DisconnectEvent_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketASync(Params^[0])^.OnDisconnect := TInternetSocketASync.TDisconnectEvent(Params^[1]^);
end;

(*
TInternetSocketASync.Running
----------------------------
```
property TInternetSocketASync.Running: Boolean;
```
*)
procedure _LapeInternetSocketASync_Running_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PInternetSocketASync(Params^[0])^.Running;
end;

(*
TInternetSocketServer.Create
----------------------------
```
function TInternetSocketServer.Create(APort: Integer): TInternetSocketServer;
```
```
function TInternetSocketServer.Create(AHost: String; APort: Integer): TInternetSocketServer;
```
*)
procedure _LapeInternetSocketServer_Create1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketServer(Result)^ := TInternetSocketServer.Create(PInteger(Params^[0])^);
end;

procedure _LapeInternetSocketServer_Create2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketServer(Result)^ := TInternetSocketServer.Create(PString(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TInternetSocketServer.Start
---------------------------
```
procedure TInternetSocketServer.Start;
```
*)
procedure _LapeInternetSocketServer_Start(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketServer(Params^[0])^.Start();
end;

(*
TInternetSocketServer.Stop
--------------------------
```
procedure TInternetSocketServer.Stop;
```
*)
procedure _LapeInternetSocketServer_Stop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketServer(Params^[0])^.Stop();
end;

(*
TInternetSocketServer.Running
-----------------------------
```
function TInternetSocketServer.Running: Boolean;
```
*)
procedure _LapeInternetSocketServer_Running_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PInternetSocketServer(Params^[0])^.Running;
end;

(*
TInternetSocketServer.ConnectionCount
-------------------------------------
```
property TInternetSocketServer.ConnectionCount: Integer;
```
*)
procedure _LapeInternetSocketServer_ConnectionCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInternetSocketServer(Params^[0])^.ConnectionCount;
end;

(*
TInternetSocketServer.OnHandleClient
------------------------------------
```
property TInternetSocketServer.OnHandleClient: THandleClientEvent;
```
```
property TInternetSocketServer.OnHandleClient(Value: THandleClientEvent);
```
*)
procedure _LapeInternetSocketServer_OnHandleClient_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TInternetSocketServer.THandleClientEvent(Result^) := PInternetSocketServer(Params^[0])^.OnHandleClient;
end;

procedure _LapeInternetSocketServer_OnHandleClient_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketServer(Params^[0])^.OnHandleClient := TInternetSocketServer.THandleClientEvent(Params^[1]^);
end;

(*
TInternetSocketServer.OnAllowClient
-----------------------------------
```
property TInternetSocketServer.OnAllowClient: TAllowClientEvent;
```
```
property TInternetSocketServer.OnAllowClient(Value: TAllowClientEvent);
```
*)
procedure _LapeInternetSocketServer_OnAllowClient_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TInternetSocketServer.TAllowClientEvent(Result^) := PInternetSocketServer(Params^[0])^.OnAllowClient;
end;

procedure _LapeInternetSocketServer_OnAllowClient_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PInternetSocketServer(Params^[0])^.OnAllowClient := TInternetSocketServer.TAllowClientEvent(Params^[1]^);
end;

(*
LoadSSL
-------
```
function LoadSSL(Debug: Boolean = False): Boolean;
```

Loads SSL. This is automatically done on demand but is useful for debugging errors relating to loading OpenSSL.
*)
procedure _LapeLoadSSL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := LoadSSL(PBoolean(Params^[0])^);
end;

procedure ImportWeb(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Web';

    LapeObjectImport(Script.Compiler, 'THTTPClient');

    addGlobalType(specialize GetEnumDecl<EHTTPStatus>(True, True), 'EHTTPStatus');
    addGlobalFunc('property EHTTPStatus.AsInteger: Integer', @_LapeHTTPStatus_AsInteger_Read);
    addGlobalFunc('property EHTTPStatus.AsString: String', @_LapeHTTPStatus_AsString_Read);

    addGlobalType('procedure(Sender: THTTPClient; URL, ContentType: String; Position, Size: Int64) of object', 'THTTPDownloadingEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: THTTPClient; URL: String; Percent: Double) of object', 'THTTPExtractingEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('function THTTPClient.Construct: THTTPClient; static; overload;', @_LapeHTTPClient_Construct1);
    addGlobalFunc('function THTTPClient.Construct(Proxy: String; Auth: String = ""): THTTPClient; static; overload', @_LapeHTTPClient_Construct2);
    addGlobalFunc('procedure THTTPClient.Destroy;', @_LapeHTTPClient_Destroy);

    addProperty('THTTPClient', 'OnDownloadProgress', 'THTTPDownloadingEvent', @_LapeHTTPClient_OnDownloadProgress_Read, @_LapeHTTPClient_OnDownloadProgress_Write);
    addProperty('THTTPClient', 'OnExtractProgress', 'THTTPExtractingEvent', @_LapeHTTPClient_OnExtractProgress_Read, @_LapeHTTPClient_OnExtractProgress_Write);

    addProperty('THTTPClient', 'ReadWriteTimeout', 'Integer', @_LapeHTTPClient_ReadWriteTimeout_Read, @_LapeHTTPClient_ReadWriteTimeout_Write);
    addProperty('THTTPClient', 'ConnectTimeout', 'Integer', @_LapeHTTPClient_ConnectTimeout_Read, @_LapeHTTPClient_ConnectTimeout_Write);
    addProperty('THTTPClient', 'Cookies', 'TStringArray', @_LapeHTTPClient_Cookies_Read, @_LapeHTTPClient_Cookies_Write);
    addProperty('THTTPClient', 'ResponseStatus', 'EHTTPStatus', @_LapeHTTPClient_ResponseStatus_Read);
    addProperty('THTTPClient', 'ResponseHeaders', 'TStringArray', @_LapeHTTPClient_ResponseHeaders_Read);
    addPropertyIndexed('THTTPClient', 'RequestHeader', 'Name: String', 'String', @_LapeHTTPClient_RequestHeader_Read, @_LapeHTTPClient_RequestHeader_Write);
    addPropertyIndexed('THTTPClient', 'ResponseHeader', 'Name: String', 'String', @_LapeHTTPClient_ResponseHeader_Read);

    addGlobalFunc('procedure THTTPClient.Reset', @_LapeHTTPClient_Reset);

    addGlobalFunc('function THTTPClient.Get(URL: String): String', @_LapeHTTPClient_Get);
    addGlobalFunc('function THTTPClient.GetJson(URL: String): TJsonItem', @_LapeHTTPClient_GetJson);
    addGlobalFunc('procedure THTTPClient.GetFile(URL, LocalFileName: String)', @_LapeHTTPClient_GetFile);
    addGlobalFunc('procedure THTTPClient.GetZip(URL: String; OutputPath: String)', @_LapeHTTPClient_GetZIP);
    addGlobalFunc('function THTTPClient.Head(URL: String): EHTTPStatus', @_LapeHTTPClient_Head);

    addGlobalFunc('function THTTPClient.Post(URL: String; Data: String): String', @_LapeHTTPClient_Post);
    addGlobalFunc('function THTTPClient.Patch(URL, PostData: String): String', @_LapeHTTPClient_Patch);
    addGlobalFunc('function THTTPClient.Put(URL, PostData: String): String', @_LapeHTTPClient_Put);
    addGlobalFunc('function THTTPClient.Delete(URL, PostData: String): String', @_LapeHTTPClient_Delete);
    addGlobalFunc('function THTTPClient.Options(URL, PostData: String): String', @_LapeHTTPClient_Options);

    addGlobalFunc('function THTTPClient.PostForm(URL: String; Data: String): String', @_LapeHTTPClient_PostForm);
    addGlobalFunc('function THTTPClient.PostFormFile(const URL, FieldName, FileName: string): String', @_LapeHTTPClient_PostFormFile);

    addGlobalFunc('procedure URLOpenInBrowser(URL: String)', @_LapeURLOpenInBrowser);
    addGlobalFunc('function URLFetch(URL: String): String', @_LapeURLFetch);
    addGlobalFunc('procedure URLFetchToFile(URL, FileName: String)', @_LapeURLFetchToFile);

    addGlobalFunc('function URLEncode(S: String): String', @_LapeURLEncode);
    addGlobalFunc('function URLDecode(S: String): String', @_LapeURLDecode);

    with addGlobalVar('THTTPClient', '[]', 'HTTPClient') do
      Used := duTrue;

    addDelayedCode(
      'begin HTTPClient := new THTTPClient(); end;',
      '!WebSetup',
      False
    );

    DumpSection := '';

    addGlobalType('type TBaseClass', 'TInternetSocket');
    addGlobalFunc('function TInternetSocket.Create(AHost: String; APort: UInt16; UseSSL: Boolean = False): TInternetSocket; static;', @_LapeSimbaInternetSocket_Create);
    addGlobalFunc('procedure TInternetSocket.Connect;', @_LapeSimbaInternetSocket_Connect);
    addGlobalFunc('procedure TInternetSocket.Close;', @_LapeSimbaInternetSocket_Close);
    addGlobalFunc('function TInternetSocket.HasData: Boolean', @_LapeSimbaInternetSocket_HasData);

    addGlobalFunc('function TInternetSocket.Read(MaxLen: Integer = 8192): TByteArray;', @_LapeSimbaInternetSocket_Read);
    addGlobalFunc('function TInternetSocket.ReadUntil(Seq: TByteArray; Timeout: Integer): TByteArray;', @_LapeSimbaInternetSocket_ReadUntil);
    addGlobalFunc('function TInternetSocket.ReadString(MaxLen: Integer = 8192): String;', @_LapeSimbaInternetSocket_ReadString);
    addGlobalFunc('function TInternetSocket.ReadStringUntil(Seq: String; Timeout: Integer): String;', @_LapeSimbaInternetSocket_ReadStringUntil);

    addGlobalFunc('function TInternetSocket.Write(Data: TByteArray): Integer;', @_LapeSimbaInternetSocket_Write);
    addGlobalFunc('function TInternetSocket.WriteString(Str: String): Integer;', @_LapeSimbaInternetSocket_WriteString);

    addProperty('TInternetSocket', 'LocalAddress', 'String', @_LapeSimbaInternetSocket_LocalAddress_Read);
    addProperty('TInternetSocket', 'RemoteAddress', 'String', @_LapeSimbaInternetSocket_RemoteAddress_Read);
    addProperty('TInternetSocket', 'LastError', 'Integer', @_LapeSimbaInternetSocket_LastError_Read);
    addProperty('TInternetSocket', 'ConnectTimeout', 'Integer', @_LapeSimbaInternetSocket_ConnectTimeout_Read, @_LapeSimbaInternetSocket_ConnectTimeout_Write);
    addProperty('TInternetSocket', 'ReadWriteTimeout', 'Integer', @_LapeSimbaInternetSocket_ReadWriteTimeout_Read, @_LapeSimbaInternetSocket_ReadWriteTimeout_Write);

    addGlobalType('type TBaseClass', 'TInternetSocketASync');
    addGlobalType('procedure(Socket: TInternetSocketASync) of object', 'TSocketDataEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Socket: TInternetSocketASync) of object', 'TSocketDisconnectEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('function TInternetSocketASync.Create(AHost: String; APort: UInt16; UseSSL: Boolean = False): TInternetSocketASync; static;', @_LapeInternetSocketASync_Create);
    addProperty('TInternetSocketASync', 'OnData', 'TSocketDataEvent', @_LapeInternetSocketASync_DataEvent_Read, @_LapeInternetSocketASync_DataEvent_Write);
    addProperty('TInternetSocketASync', 'OnDisconnect', 'TSocketDisconnectEvent', @_LapeInternetSocketASync_DisconnectEvent_Read, @_LapeInternetSocketASync_DisconnectEvent_Write);
    addProperty('TInternetSocketASync', 'Running', 'Boolean', @_LapeInternetSocketASync_Running_Read);

    addGlobalType('type TBaseClass', 'TInternetSocketServer');
    addClassConstructor('TInternetSocketServer', '(Port: Integer)', @_LapeInternetSocketServer_Create1);
    addClassConstructor('TInternetSocketServer', '(AHost: String; APort: Integer)', @_LapeInternetSocketServer_Create2, True);
    addGlobalType('procedure(Sender: TInternetSocketServer; Sock: TInternetSocket) of object', 'THandleClientEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TInternetSocketServer; Address: String; ConnectionCount: Integer; var Allow: Boolean) of object', 'TAllowClientEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('procedure TInternetSocketServer.Start', @_LapeInternetSocketServer_Start);
    addGlobalFunc('procedure TInternetSocketServer.Stop', @_LapeInternetSocketServer_Stop);
    addProperty('TInternetSocketServer', 'Running', 'Boolean', @_LapeInternetSocketServer_Running_Read);
    addProperty('TInternetSocketServer', 'ConnectionCount', 'Integer', @_LapeInternetSocketServer_ConnectionCount_Read);
    addProperty('TInternetSocketServer', 'OnHandleClient', 'THandleClientEvent', @_LapeInternetSocketServer_OnHandleClient_Read, @_LapeInternetSocketServer_OnHandleClient_Write);
    addProperty('TInternetSocketServer', 'OnAllowClient', 'TAllowClientEvent', @_LapeInternetSocketServer_OnAllowClient_Read, @_LapeInternetSocketServer_OnAllowClient_Write);

    addGlobalFunc('function LoadSSL(Debug: Boolean = False): Boolean', @_LapeLoadSSL);
  end;
end;

end.
