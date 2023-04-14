unit simba.import_internet;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

implementation

uses
  fphttpclient,
  lptypes, ffi,
  simba.script_compiler, simba.nativeinterface, simba.httpclient;

(*
Internet
========
Internet HTTP request/post methods.

- There is a pre-defined variable `HTTPClient` to use.
*)

(*
OpenURL
~~~~~~~
procedure OpenURL(URL: String);

Open a URL in the systems default internet browser.
*)
procedure _LapeOpenURL(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.OpenURL(PString(Params^[0])^);
end;

(*
GetPage
~~~~~~~
function GetPage(URL: String): String;

Simple method to return the contents of a webpage.
*)
procedure _LapeGetPage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaHTTPClient.SimpleGet(PString(Params^[0])^, []);
end;

(*
EncodeURLElement
~~~~~~~~~~~~~~~~
function EncodeURLElement(S: String): String;

URL encode a string. For example a space character is changed to `%20`.
*)
procedure _LapeEncodeURLElement(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := EncodeURLElement(PString(Params^[0])^);
end;

(*
DecodeURLElement
~~~~~~~~~~~~~~~~
function DecodeURLElement(S: String): String;

Inverse of EncodeURLElement.
*)
procedure _LapeDecodeURLElement(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := DecodeURLElement(PString(Params^[0])^);
end;

(*
TSimbaHTTPClient.Create
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Create: TSimbaHTTPClient; static;

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
TSimbaHTTPClient.Free
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.Free
*)
procedure _LapeSimbaHTTPClient_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.Free();
end;

(*
TSimbaHTTPClient.SetProxy
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetProxy(Host: String; Port: Integer; UserName, Password: String);
*)
procedure _LapeSimbaHTTPClient_SetProxy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.SetProxy(PString(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^, PString(Params^[4])^);
end;

(*
TSimbaHTTPClient.ClearProxy
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.ClearProxy;
*)
procedure _LapeSimbaHTTPClient_ClearProxy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.ClearProxy();
end;

(*
TSimbaHTTPClient.Reset
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.Reset;
*)
procedure _LapeSimbaHTTPClient_Reset(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.Reset();
end;

(*
TSimbaHTTPClient.Get
~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Get(URL: String; AllowedStatusCodes: THTTPStatusArray = []): String;

Return a webpages content as a string.

Note: | If `AllowedStatusCodes` is empty **any** response code is accepted.
      | If the response code is **not** in `AllowedStatusCodes` an exception is raised.
*)
procedure _LapeSimbaHTTPClient_Get(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Get(PString(Params^[1])^, PHTTPStatusArray(Params^[2])^);
end;

(*
TSimbaHTTPClient.GetFile
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.GetFile(URL, LocalFileName: String; AllowedStatusCodes: THTTPStatusArray = []);

Save a webpages content to a local file.

Note: | If `AllowedStatusCodes` is empty **any** response code is accepted.
      | If the response code is **not** in `AllowedStatusCodes` an exception is raised.
*)
procedure _LapeSimbaHTTPClient_GetFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.GetFile(PString(Params^[1])^, PString(Params^[2])^, PHTTPStatusArray(Params^[3])^);
end;

(*
TSimbaHTTPClient.GetZip
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.GetZip(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray);
*)
procedure _LapeSimbaHTTPClient_GetZIP(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.GetZip(PString(Params^[1])^, PString(Params^[2])^, PBoolean(Params^[3])^, PStringArray(Params^[4])^);
end;

(*
TSimbaHTTPClient.Head
~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Head(URL: String): EHTTPStatus;

Header request. Headers will be written to `HTTPClient.GetResponseHeaders()`
*)
procedure _LapeSimbaHTTPClient_Head(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PSimbaHTTPClient(Params^[0])^.Head(PString(Params^[1])^);
end;

(*
TSimbaHTTPClient.GetCookies
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetCookies: TStringArray;
*)
procedure _LapeSimbaHTTPClient_GetCookies(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaHTTPClient(Params^[0])^.Cookies.ToStringArray();
end;

(*
TSimbaHTTPClient.SetCookies
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetCookies(Cookies: TStringArray);
*)
procedure _LapeSimbaHTTPClient_SetCookies(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.Cookies.AddStrings(PStringArray(Params^[1])^, True);
end;

(*
TSimbaHTTPClient.ResponseStatus
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.ResponseStatus: EHTTPStatus;

Returns the response status.

```
  if (HTTPClient.ResponseStatus = EHTTPStaus.OK) then
    WriteLn('Response status was OK!')
```
*)
procedure _LapeSimbaHTTPClient_ResponseStatus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHTTPStatus(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseStatus;
end;

(*
TSimbaHTTPClient.GetResponseHeader
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetResponseHeader(Name: String): String;
*)
procedure _LapeSimbaHTTPClient_GetResponseHeader(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseHeader[PString(Params^[1])^];
end;

(*
TSimbaHTTPClient.GetResponseHeaders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetResponseHeaders: TStringArray;
*)
procedure _LapeSimbaHTTPClient_GetResponseHeaders(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaHTTPClient(Params^[0])^.ResponseHeaders.ToStringArray();
end;

(*
TSimbaHTTPClient.GetRequestHeader
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetRequestHeader(Name: String): String;
*)
procedure _LapeSimbaHTTPClient_GetRequestHeader(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.RequestHeader[PString(Params^[1])^];
end;

(*
TSimbaHTTPClient.SetRequestContentType
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetRequestContentType(Value: String);
*)
procedure _LapeSimbaHTTPClient_SetRequestContentType(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.RequestContentType := PString(Params^[1])^;
end;

(*
TSimbaHTTPClient.SetRequestHeader
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetRequestHeader(Name, Value: String);
*)
procedure _LapeSimbaHTTPClient_SetRequestHeader(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.RequestHeader[PString(Params^[1])^] := PString(Params^[2])^;
end;

(*
TSimbaHTTPClient.Post
~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Post(URL: String; Data: String): String;

HTTP post request.

- `Data` is sent in request body.
*)
procedure _LapeSimbaHTTPClient_Post(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Post(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.PostForm
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.PostForm(URL: String; Data: String): String;

Post form data (www-urlencoded)
*)
procedure _LapeSimbaHTTPClient_PostForm(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.PostForm(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.PostFormFile
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.PostFormFile(const URL, FieldName, FileName: string): String;

Post form with a local file file
*)
procedure _LapeSimbaHTTPClient_PostFormFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.PostFormFile(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

(*
TSimbaHTTPClient.Patch
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Patch(URL, PostData: String): String;
*)
procedure _LapeSimbaHTTPClient_Patch(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Patch(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Put
~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Put(URL, PostData: String): String;
*)
procedure _LapeSimbaHTTPClient_Put(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Put(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.Delete
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.Delete(URL, PostData: String): String;
*)
procedure _LapeSimbaHTTPClient_Delete(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.Delete(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaHTTPClient.GetUserAgent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetUserAgent: String;
*)
procedure _LapeSimbaHTTPClient_GetUserAgent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaHTTPClient(Params^[0])^.UserAgent;
end;

(*
TSimbaHTTPClient.SetUserAgent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetUserAgent(Value: String);
*)
procedure _LapeSimbaHTTPClient_SetUserAgent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.UserAgent := PString(Params^[1])^;
end;

(*
TSimbaHTTPClient.GetOnDownloadProgress
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetOnDownloadProgress: TSimbaHTTPDownloadingEvent;
*)
procedure _LapeSimbaHTTPClient_OnDownloadProgress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaHTTPDownloadingEvent(Result^) := PSimbaHTTPClient(Params^[0])^.OnDownloadProgress;
end;

(*
TSimbaHTTPClient.SetOnDownloadProgress
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetOnDownloadProgress(Value: TSimbaHTTPDownloadingEvent);
*)
procedure _LapeSimbaHTTPClient_OnDownloadProgress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.OnDownloadProgress := TSimbaHTTPDownloadingEvent(Params^[1]^);
end;

(*
TSimbaHTTPClient.GetOnExtractProgress
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaHTTPClient.GetOnExtractProgress: TSimbaHTTPExtractingEvent;
*)
procedure _LapeSimbaHTTPClient_OnExtractProgress_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaHTTPExtractingEvent(Result^) := PSimbaHTTPClient(Params^[0])^.OnExtractProgress;
end;

(*
TSimbaHTTPClient.SetOnExtractProgress
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaHTTPClient.SetOnExtractProgress(Value: TSimbaHTTPExtractingEvent);
*)
procedure _LapeSimbaHTTPClient_OnExtractProgress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaHTTPClient(Params^[0])^.OnExtractProgress := TSimbaHTTPExtractingEvent(Params^[1]^);
end;

procedure ImportInternet(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Internet';

    addGlobalType([
      'enum(',
      '  CONTINUE = 100,',
      '  SWITCHING_PROTOCOLS = 101,',
      '  PROCESSING = 102,',
      '  EARLY_HINTS = 103,',
      '',
      '  OK = 200,',
      '  CREATED = 201,',
      '  ACCEPTED = 202,',
      '  NONAUTHORITATIVE_INFORMATION = 203,',
      '  NO_CONTENT = 204,',
      '  RESET_CONTENT = 205,',
      '  PARTIAL_CONTENT = 206,',
      '',
      '  MULTIPLE_CHOICES = 300,',
      '  MOVED_PERMANENTLY = 301,',
      '  FOUND = 302,',
      '  SEE_OTHER = 303,',
      '  NOT_MODIFIED = 304,',
      '  USE_PROXY = 305,',
      '  TEMPORARY_REDIRECT = 307,',
      '  PERMANENT_REDIRECT = 308,',
      '',
      '  BAD_REQUEST = 400,',
      '  UNAUTHORIZED  = 401,',
      '  PAYMENT_REQUIRED = 402,',
      '  FORBIDDEN = 403,',
      '  NOT_FOUND = 404,',
      '  METHOD_NOT_ALLOWED = 405,',
      '  NOT_ACCEPTABLE = 406,',
      '  PROXY_AUTHENTICATION_REQUIRED = 407,',
      '  REQUEST_TIMEOUT = 408,',
      '  CONFLICT = 409,',
      '  GONE = 410,',
      '  LENGTH_REQUIRED = 411,',
      '  PRECONDITION_FAILED = 412,',
      '  REQUEST_ENTITY_TOO_LARGE = 413,',
      '  REQUEST_URI_TOO_LONG = 414,',
      '  UNSUPPORTED_MEDIA_TYPE = 415,',
      '  REQUESTED_RANGE_NOT_SATISFIABLE = 416,',
      '  EXPECTATION_FAILED = 417,',
      '  IM_A_TEAPOT = 418,',
      '  MISDIRECTED_REQUEST = 421,',
      '  UNPROCESSABLE_ENTITY = 422,',
      '  LOCKED = 423,',
      '  FAILED_DEPENDENCY = 424,',
      '  TOO_EARLY = 425,',
      '  UPGRADE_REQUIRED = 426,',
      '  PRECONDITION_REQUIRED = 428,',
      '  TOO_MANY_REQUESTS = 429,',
      '  REQUEST_HEADER_FIELDS_TOO_LARGE = 431,',
      '  UNAVAILABLE_FOR_LEGAL_REASONS = 451,',
      '',
      '  INTERNAL_SERVER_ERROR = 500,',
      '  NOT_IMPLEMENTED = 501,',
      '  BAD_GATEWAY = 502,',
      '  SERVICE_UNAVAILABLE = 503,',
      '  GATEWAY_TIMEOUT = 504,',
      '  VERSION_NOT_SUPPORTED = 505',
      ' );'],
      'EHTTPStatus');

    addGlobalType('array of EHTTPStatus', 'THTTPStatusArray');

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

    addClass('TSimbaHTTPClient');

    addGlobalType('procedure(Sender: TObject; URL, ContentType: String; Position, Size: Int64) of object', 'TSimbaHTTPDownloadingEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; URL: String; Percent: Double) of object', 'TSimbaHTTPExtractingEvent', FFI_DEFAULT_ABI);

    addClassVar('TSimbaHTTPClient', 'OnDownloadProgress', 'TSimbaHTTPDownloadingEvent', @_LapeSimbaHTTPClient_OnDownloadProgress_Read, @_LapeSimbaHTTPClient_OnDownloadProgress_Write);
    addClassVar('TSimbaHTTPClient', 'OnExtractProgress', 'TSimbaHTTPExtractingEvent', @_LapeSimbaHTTPClient_OnExtractProgress_Read, @_LapeSimbaHTTPClient_OnExtractProgress_Write);

    addGlobalFunc('function TSimbaHTTPClient.Create: TSimbaHTTPClient; static;', @_LapeSimbaHTTPClient_Create);
    addGlobalFunc('procedure TSimbaHTTPClient.Free', @_LapeSimbaHTTPClient_Free);

    addGlobalFunc('function TSimbaHTTPClient.Get(URL: String; AllowedStatusCodes: THTTPStatusArray = []): String', @_LapeSimbaHTTPClient_Get);
    addGlobalFunc('procedure TSimbaHTTPClient.GetFile(URL, LocalFileName: String; AllowedStatusCodes: THTTPStatusArray = [])', @_LapeSimbaHTTPClient_GetFile);
    addGlobalFunc('procedure TSimbaHTTPClient.GetZip(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray)', @_LapeSimbaHTTPClient_GetZIP);
    addGlobalFunc('function TSimbaHTTPClient.Head(URL: String): EHTTPStatus', @_LapeSimbaHTTPClient_Head);
    addGlobalFunc('function TSimbaHTTPClient.GetCookies: TStringArray', @_LapeSimbaHTTPClient_GetCookies);
    addGlobalFunc('procedure TSimbaHTTPClient.SetCookies(Cookies: TStringArray)', @_LapeSimbaHTTPClient_SetCookies);

    addGlobalFunc('function TSimbaHTTPClient.GetRequestHeader(Name: String): String', @_LapeSimbaHTTPClient_GetRequestHeader);
    addGlobalFunc('procedure TSimbaHTTPClient.SetRequestHeader(Name, Value: String)', @_LapeSimbaHTTPClient_SetRequestHeader);
    addGlobalFunc('procedure TSimbaHTTPClient.SetRequestContentType(Value: String)', @_LapeSimbaHTTPClient_SetRequestContentType);
    addGlobalFunc('function TSimbaHTTPClient.GetResponseHeader(Name: String): String', @_LapeSimbaHTTPClient_GetResponseHeader);
    addGlobalFunc('function TSimbaHTTPClient.GetResponseHeaders: TStringArray', @_LapeSimbaHTTPClient_GetResponseHeaders);
    addGlobalFunc('function TSimbaHTTPClient.ResponseStatus: EHTTPStatus', @_LapeSimbaHTTPClient_ResponseStatus);

    addGlobalFunc('procedure TSimbaHTTPClient.SetProxy(Host: String; Port: Integer; UserName, Password: String)', @_LapeSimbaHTTPClient_SetProxy);
    addGlobalFunc('procedure TSimbaHTTPClient.ClearProxy', @_LapeSimbaHTTPClient_ClearProxy);
    addGlobalFunc('procedure TSimbaHTTPClient.Reset', @_LapeSimbaHTTPClient_Reset);

    addGlobalFunc('function TSimbaHTTPClient.Post(URL: String; Data: String): String', @_LapeSimbaHTTPClient_Post);
    addGlobalFunc('function TSimbaHTTPClient.PostForm(URL: String; Data: String): String', @_LapeSimbaHTTPClient_PostForm);
    addGlobalFunc('function TSimbaHTTPClient.PostFormFile(const URL, FieldName, FileName: string): String', @_LapeSimbaHTTPClient_PostFormFile);

    addGlobalFunc('function TSimbaHTTPClient.Patch(URL, PostData: String): String', @_LapeSimbaHTTPClient_Patch);
    addGlobalFunc('function TSimbaHTTPClient.Put(URL, PostData: String): String', @_LapeSimbaHTTPClient_Put);
    addGlobalFunc('function TSimbaHTTPClient.Delete(URL, PostData: String): String', @_LapeSimbaHTTPClient_Delete);

    addGlobalFunc('function TSimbaHTTPClient.GetUserAgent: String', @_LapeSimbaHTTPClient_GetUserAgent);
    addGlobalFunc('procedure TSimbaHTTPClient.SetUserAgent(Value: String)', @_LapeSimbaHTTPClient_SetUserAgent);

    addGlobalFunc('procedure OpenURL(URL: String)', @_LapeOpenURL);
    addGlobalFunc('function GetPage(URL: String): String', @_LapeGetPage);

    addGlobalFunc('function EncodeURLElement(S: String): String', @_LapeEncodeURLElement);
    addGlobalFunc('function DecodeURLElement(S: String): String', @_LapeDecodeURLElement);

    with addGlobalVar('TSimbaHTTPClient', nil, 'HTTPClient') do
    begin
      TSimbaHTTPClient(Ptr^) := TSimbaHTTPClient.Create();
      TSimbaHTTPClient(Ptr^).FreeOnTerminate := True
    end;

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportInternet);

end.

