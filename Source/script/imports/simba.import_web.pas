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
  PInternetSocket = ^TInternetSocket;
  PInternetSocketASync = ^TInternetSocketASync;
  PInternetSocketServer = ^TInternetSocketServer;

(*
Web
===
Internet HTTP request/post methods.

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
TSimbaHTTPClient.Create
-----------------------
```
function TSimbaHTTPClient.Create: TSimbaHTTPClient; static;
```

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
```
function TSimbaHTTPClient.CreateWithProxy(Host: String; User: String = ''; Pass: String = ''): THTTPClient; static;
```

Variant which uses a proxy for all connections.
*)
procedure _LapeSimbaHTTPClient_CreateWithProxy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Result)^ := TSimbaHTTPClient.CreateWithProxy(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.OnDownloadProgress
-----------------------------------
```
property TSimbaHTTPClient.OnDownloadProgress: TSimbaHTTPDownloadingEvent;
```
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
```
property TSimbaHTTPClient.OnExtractProgress: TSimbaHTTPExtractingEvent;
```
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
```
property TSimbaHTTPClient.ConnectTimeout: Integer;
```
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
```
property TSimbaHTTPClient.ReadWriteTimeout: Integer;
```
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
```
property TSimbaHTTPClient.Cookies: TStringArray;
```
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
```
property TSimbaHTTPClient.ResponseStatus: EHTTPStatus;
```

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
```
function TSimbaHTTPClient.ResponseHeaders: TStringArray;
```
*)
procedure _LapeSimbaHTTPClient_ResponseHeaders_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseHeaders.ToStringArray();
end;

(*
TSimbaHTTPClient.ResponseHeader
-------------------------------
```
property TSimbaHTTPClient.ResponseHeader[Name: String]: String;
```
*)
procedure _LapeSimbaHTTPClient_ResponseHeader_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseHeader[PString(Params^[1])^];
end;

(*
TSimbaHTTPClient.RequestHeader
------------------------------
```
property TSimbaHTTPClient.RequestHeader[Name: String]: String;
```
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
```
procedure TSimbaHTTPClient.Reset;
```
*)
procedure _LapeSimbaHTTPClient_Reset(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.Reset();
end;

(*
TSimbaHTTPClient.Get
--------------------
```
function TSimbaHTTPClient.Get(URL: String): String;
```

Return a webpages content as a string.
*)
procedure _LapeSimbaHTTPClient_Get(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Get(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.GetJson
------------------------
```
function TSimbaHTTPClient.GetJson(URL: String): TSimbaJSONParser;
```
*)
procedure _LapeSimbaHTTPClient_GetJson(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaHTTPClient(Params^[0])^.GetJson(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.GetFile
------------------------
```
procedure TSimbaHTTPClient.GetFile(URL, LocalFileName: String);
```

Save a webpages content to a local file.
*)
procedure _LapeSimbaHTTPClient_GetFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.GetFile(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.GetZip
-----------------------
```
procedure TSimbaHTTPClient.GetZip(URL: String; OutputPath: String);
```
*)
procedure _LapeSimbaHTTPClient_GetZIP(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.GetZip(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Head
---------------------
```
function TSimbaHTTPClient.Head(URL: String): EHTTPStatus;
```

Header request. Headers will be written to `HTTPClient.GetResponseHeaders()`
*)
procedure _LapeSimbaHTTPClient_Head(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PSimbaHTTPClient(Params^[0])^.Head(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.Post
---------------------
```
function TSimbaHTTPClient.Post(URL: String; Data: String): String;
```

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
```
function TSimbaHTTPClient.Patch(URL, Data: String): String;
```
*)
procedure _LapeSimbaHTTPClient_Patch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Patch(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Put
--------------------
```
function TSimbaHTTPClient.Put(URL, Data: String): String;
```
*)
procedure _LapeSimbaHTTPClient_Put(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Put(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Delete
-----------------------
```
function TSimbaHTTPClient.Delete(URL, Data: String): String;
```
*)
procedure _LapeSimbaHTTPClient_Delete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Delete(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Options
-----------------------
```
function TSimbaHTTPClient.Options(URL, Data: String): String;
```
*)
procedure _LapeSimbaHTTPClient_Options(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Options(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.PostForm
-------------------------
```
function TSimbaHTTPClient.PostForm(URL: String; Data: String): String;
```

Post form data (www-urlencoded)
*)
procedure _LapeSimbaHTTPClient_PostForm(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.PostForm(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.PostFormFile
-----------------------------
```
function TSimbaHTTPClient.PostFormFile(const URL, FieldName, FileName: string): String;
```

Post form with a local file file
*)
procedure _LapeSimbaHTTPClient_PostFormFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.PostFormFile(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
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
```
```
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

procedure ImportWeb(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Web';

    addGlobalType(specialize GetEnumDecl<EHTTPStatus>(True, True), 'EHTTPStatus');
    addGlobalFunc('property EHTTPStatus.AsInteger: Integer', @_LapeHTTPStatus_AsInteger_Read);
    addGlobalFunc('property EHTTPStatus.AsString: String', @_LapeHTTPStatus_AsString_Read);

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

    addClass('TInternetSocketASync', 'TInternetSocket');
    addGlobalType('procedure(Socket: TInternetSocketASync) of object', 'TSocketDataEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Socket: TInternetSocketASync) of object', 'TSocketDisconnectEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('function TInternetSocketASync.Create(AHost: String; APort: UInt16; UseSSL: Boolean = False): TInternetSocketASync; static;', @_LapeInternetSocketASync_Create);
    addProperty('TInternetSocketASync', 'OnData', 'TSocketDataEvent', @_LapeInternetSocketASync_DataEvent_Read, @_LapeInternetSocketASync_DataEvent_Write);
    addProperty('TInternetSocketASync', 'OnDisconnect', 'TSocketDisconnectEvent', @_LapeInternetSocketASync_DisconnectEvent_Read, @_LapeInternetSocketASync_DisconnectEvent_Write);
    addProperty('TInternetSocketASync', 'Running', 'Boolean', @_LapeInternetSocketASync_Running_Read);

    addClass('TInternetSocketServer');
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