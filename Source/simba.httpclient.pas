{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.httpclient;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.baseclass, simba.fphttpclient;

{$PUSH}
{$SCOPEDENUMS ON}
type
  EHTTPStatus = (
    // Information
    CONTINUE = 100,
    SWITCHING_PROTOCOLS = 101,
    PROCESSING = 102,
    EARLY_HINTS = 103,

    // Success
    OK = 200,
    CREATED = 201,
    ACCEPTED = 202,
    NONAUTHORITATIVE_INFORMATION = 203,
    NO_CONTENT = 204,
    RESET_CONTENT = 205,
    PARTIAL_CONTENT = 206,

    // Redirection
    MULTIPLE_CHOICES = 300,
    MOVED_PERMANENTLY = 301,
    FOUND = 302,
    SEE_OTHER = 303,
    NOT_MODIFIED = 304,
    USE_PROXY = 305,
    TEMPORARY_REDIRECT = 307,
    PERMANENT_REDIRECT = 308,

    // Client Error
    BAD_REQUEST = 400,
    UNAUTHORIZED  = 401,
    PAYMENT_REQUIRED = 402,
    FORBIDDEN = 403,
    NOT_FOUND = 404,
    METHOD_NOT_ALLOWED = 405,
    NOT_ACCEPTABLE = 406,
    PROXY_AUTHENTICATION_REQUIRED = 407,
    REQUEST_TIMEOUT = 408,
    CONFLICT = 409,
    GONE = 410,
    LENGTH_REQUIRED = 411,
    PRECONDITION_FAILED = 412,
    REQUEST_ENTITY_TOO_LARGE = 413,
    REQUEST_URI_TOO_LONG = 414,
    UNSUPPORTED_MEDIA_TYPE = 415,
    REQUESTED_RANGE_NOT_SATISFIABLE = 416,
    EXPECTATION_FAILED = 417,
    IM_A_TEAPOT = 418,
    MISDIRECTED_REQUEST = 421,
    UNPROCESSABLE_ENTITY = 422,
    LOCKED = 423,
    FAILED_DEPENDENCY = 424,
    TOO_EARLY = 425,
    UPGRADE_REQUIRED = 426,
    PRECONDITION_REQUIRED = 428,
    TOO_MANY_REQUESTS = 429,
    REQUEST_HEADER_FIELDS_TOO_LARGE = 431,
    UNAVAILABLE_FOR_LEGAL_REASONS = 451,

    // Server Error
    INTERNAL_SERVER_ERROR = 500,
    NOT_IMPLEMENTED = 501,
    BAD_GATEWAY = 502,
    SERVICE_UNAVAILABLE = 503,
    GATEWAY_TIMEOUT = 504,
    VERSION_NOT_SUPPORTED = 505
   );
   THTTPStatusArray = array of EHTTPStatus;

   PHTTPStatus = ^EHTTPStatus;
   PHTTPStatusArray = ^THTTPStatusArray;
{$POP}

type
  TSimbaHTTPConnectingEvent  = procedure(Sender: TObject; URL: String) of object;
  TSimbaHTTPExtractingEvent  = procedure(Sender: TObject; URL: String; Percent: Double) of object;
  TSimbaHTTPStatusEvent      = procedure(Sender: TObject; ResponseStatus: EHTTPStatus) of object;
  TSimbaHTTPDownloadingEvent = procedure(Sender: TObject; URL, ContentType: String; Position, Size: Int64) of object;

  TSimbaHTTPClient = class(TSimbaBaseClass)
  protected
    FHTTPClient: TSimbaFPHTTPClient;
    FURL: String;
    FContentType: String;
    FOnDownloadProgress: TSimbaHTTPDownloadingEvent;
    FOnExtractProgress: TSimbaHTTPExtractingEvent;
    FOnConnecting: TSimbaHTTPConnectingEvent;
    FOnHeaders: TNotifyEvent;
    FOnResponseStatus: TSimbaHTTPStatusEvent;
    FDownloadingFinished: TNotifyEvent;
    FExtractingFinished: TNotifyEvent;

    function StatusCodesToIntegerArray(StatusCodes: THTTPStatusArray): TIntegerArray;
    function SetURL(URL: String): String;

    procedure DoExtractProgress(Sender: TObject; FileName: String; Percent: Double);
    procedure DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
    procedure DoRedirect(Sender: TObject; const Source: String; var Dest: String);
    procedure DoConnecting(Sender: TObject; URL: String);
    procedure DoHeaders(Sender: TObject);
    procedure DoResponseStatus(Sender: TObject; ResponseStatus: Integer);

    function GetResponseHeader(AName: String): String;
    function GetResponseStatus: EHTTPStatus;
    function GetResponseHeaders: TStringList;
    function GetRequestHeader(AName: String): String;
    function GetRequestHeaders: TStringList;
    function GetCookies: TStringList;
    function GetRequestContentType: String;
    function GetUserAgent: String;

    procedure SetUserAgent(Value: String);
    procedure SetRequestContentType(Value: String);
    procedure SetRequestHeader(AName: String; Value: String);
    procedure SetCookies(Value: TStringList);
  public
    property OnDownloadProgress: TSimbaHTTPDownloadingEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnExtractProgress: TSimbaHTTPExtractingEvent read FOnExtractProgress write FOnExtractProgress;
    property OnConnecting: TSimbaHTTPConnectingEvent read FOnConnecting write FOnConnecting;
    property OnHeadersReceived: TNotifyEvent read FOnHeaders write FOnHeaders;
    property OnResponseStatus: TSimbaHTTPStatusEvent read FOnResponseStatus write FOnResponseStatus;
    property OnDownloadingFinished: TNotifyEvent read FDownloadingFinished write FDownloadingFinished;
    property OnExtractingFinished: TNotifyEvent read FExtractingFinished write FExtractingFinished;

    property ResponseStatus: EHTTPStatus read GetResponseStatus;
    property ResponseHeader[AName: String]: String read GetResponseHeader;
    property ResponseHeaders: TStringList read GetResponseHeaders;

    property RequestHeader[AName: String]: String read GetRequestHeader write SetRequestHeader;
    property RequestHeaders: TStringList read GetRequestHeaders;
    property RequestContentType: String read GetRequestContentType write SetRequestContentType;

    property Cookies: TStringList read GetCookies write SetCookies;
    property UserAgent: String read GetUserAgent write SetUserAgent;

    procedure SetProxy(Host: String; Port: Integer; UserName, Password: String);
    procedure ClearProxy;

    // Clear cookies and request data
    procedure Reset;

    // Header request, returns response code
    function Head(URL: String): EHTTPStatus;

    // Writes page contents to result
    function Get(URL: String; AllowedStatusCodes: THTTPStatusArray): String;

    // Writes page contents to a file.
    procedure GetFile(URL, LocalFileName: String; AllowedStatusCodes: THTTPStatusArray);

    // Extracts page contents to file treating contents as .zip
    procedure GetZip(URL, OutputPath: String; Flat: Boolean; IgnoreList: TStringArray);

    // Post data in request body returns response
    function Post(URL, PostData: String): String;

    // Post Form data (www-urlencoded) in request body.
    // Return response
    function PostForm(URL, Data: String): String;
    // Post a file
    // Return respose
    function PostFormFile(URL, FieldName, FileName: string): String;

    function Patch(URL, Data: String): String;
    function Put(URL, Data: String): String;
    function Delete(URL, Data: String): String;

    class function SimpleGet(URL: String; AllowedStatusCodes: THTTPStatusArray): String; static;
    class procedure SimpleGetFile(URL, LocalFileName: String; AllowedStatusCodes: THTTPStatusArray); static;

    constructor Create;
    destructor Destroy; override;
  end;

  PSimbaHTTPClient = ^TSimbaHTTPClient;

  function ToStr(Status: EHTTPStatus): String;

implementation

uses
  TypInfo,
  simba.zip;

function TSimbaHTTPClient.GetResponseHeaders: TStringList;
begin
  Result := FHTTPClient.ResponseHeaders as TStringList;
end;

procedure TSimbaHTTPClient.DoRedirect(Sender: TObject; const Source: String; var Dest: String);
begin
  FURL := Dest;
end;

procedure TSimbaHTTPClient.DoConnecting(Sender: TObject; URL: String);
begin
  if Assigned(OnConnecting) then
    OnConnecting(Self, URL);
end;

procedure TSimbaHTTPClient.DoHeaders(Sender: TObject);
begin
  FContentType := ResponseHeader['Content-Type'];
  if Assigned(OnHeadersReceived) then
    OnHeadersReceived(Self);
end;

procedure TSimbaHTTPClient.DoResponseStatus(Sender: TObject; ResponseStatus: Integer);
begin
  if Assigned(FOnResponseStatus) then
    FOnResponseStatus(Self, EHTTPStatus(ResponseStatus));
end;

function TSimbaHTTPClient.GetRequestHeader(AName: String): String;
begin
  Result := FHTTPClient.GetHeader(AName);
end;

function TSimbaHTTPClient.GetRequestHeaders: TStringList;
begin
  Result := FHTTPClient.RequestHeaders as TStringList;
end;

function TSimbaHTTPClient.GetCookies: TStringList;
begin
  Result := FHTTPClient.Cookies as TStringList;
end;

procedure TSimbaHTTPClient.SetRequestHeader(AName: String; Value: String);
begin
  FHTTPClient.AddHeader(AName, Value);
end;

procedure TSimbaHTTPClient.SetProxy(Host: String; Port: Integer; UserName, Password: String);
begin
  FHTTPClient.Proxy.Host := Host;
  FHTTPClient.Proxy.Port := Port;
  FHTTPClient.Proxy.UserName := UserName;
  FHTTPClient.Proxy.Password := Password;
end;

procedure TSimbaHTTPClient.ClearProxy;
begin
  FHTTPClient.Proxy.Host := '';
  FHTTPClient.Proxy.Port := 0;
  FHTTPClient.Proxy.UserName := '';
  FHTTPClient.Proxy.Password := '';
end;

procedure TSimbaHTTPClient.Reset;
begin
  FHTTPClient.Cookies.Clear();
  FHTTPClient.RequestHeaders.Clear();
end;

function TSimbaHTTPClient.Head(URL: String): EHTTPStatus;
begin
  FHTTPClient.HTTPMethod('HEAD', SetURL(URL), nil, []);

  Result := GetResponseStatus();
end;

function TSimbaHTTPClient.GetRequestContentType: String;
begin
  Result := RequestHeader['Content-Type'];
end;

procedure TSimbaHTTPClient.SetRequestContentType(Value: String);
begin
  RequestHeader['Content-Type'] := Value;
end;

procedure TSimbaHTTPClient.SetCookies(Value: TStringList);
begin
  FHTTPClient.Cookies := Value;
end;

function TSimbaHTTPClient.GetUserAgent: String;
begin
  Result := RequestHeader['User-Agent'];
end;

procedure TSimbaHTTPClient.SetUserAgent(Value: String);
begin
  RequestHeader['User-Agent'] := Value;
end;

function TSimbaHTTPClient.StatusCodesToIntegerArray(StatusCodes: THTTPStatusArray): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, Length(StatusCodes));
  for I := 0 to High(StatusCodes) do
    Result[I] := Ord(StatusCodes[I]);
end;

function TSimbaHTTPClient.SetURL(URL: String): String;
begin
  FURL := URL;
  if (not FURL.StartsWith('http://', False)) and (not FURL.StartsWith('https://', False)) then
    FURL := 'http://' + FURL;

  Result := FURL;
end;

procedure TSimbaHTTPClient.DoExtractProgress(Sender: TObject; FileName: String; Percent: Double);
begin
  if (FOnExtractProgress <> nil) then
    FOnExtractProgress(Self, FileName, Percent);
end;

procedure TSimbaHTTPClient.DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
begin
  if (FOnDownloadProgress <> nil) then
    FOnDownloadProgress(Self, FURL, FContentType, Position, Size);
end;

function TSimbaHTTPClient.GetResponseHeader(AName: String): String;
begin
  Result := FHTTPClient.GetHeader(FHTTPClient.ResponseHeaders, AName);
end;

function TSimbaHTTPClient.GetResponseStatus: EHTTPStatus;
begin
  Result := EHTTPStatus(FHTTPClient.ResponseStatusCode);
end;

function TSimbaHTTPClient.Get(URL: String; AllowedStatusCodes: THTTPStatusArray): String;
var
  Stream: TRawByteStringStream;
begin
  Stream := TRawByteStringStream.Create();
  try
    FHTTPClient.HTTPMethod('GET', SetURL(URL), Stream, StatusCodesToIntegerArray(AllowedStatusCodes));

    Result := Stream.DataString;
  finally
    Stream.Free();
  end;
end;

procedure TSimbaHTTPClient.GetFile(URL, LocalFileName: String; AllowedStatusCodes: THTTPStatusArray);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(LocalFileName, fmCreate);
  try
    FHTTPClient.HTTPMethod('GET', SetURL(URL), Stream, StatusCodesToIntegerArray(AllowedStatusCodes));
  finally
    Stream.Free();
  end;
end;

procedure TSimbaHTTPClient.GetZip(URL, OutputPath: String; Flat: Boolean; IgnoreList: TStringArray);
var
  Stream: TMemoryStream;
  Extractor: TSimbaZipExtractor;
begin
  Stream := TMemoryStream.Create();

  try
    FHTTPClient.HTTPMethod('GET', SetURL(URL), Stream, [200]);

    Extractor := TSimbaZipExtractor.Create();
    Extractor.OnProgress := @DoExtractProgress;
    Extractor.OnExtractingFinished := FExtractingFinished;
    Extractor.InputStream := Stream;
    Extractor.OutputPath := OutputPath;
    Extractor.Flat := Flat;
    Extractor.IgnoreList.AddStrings(IgnoreList);

    try
      Extractor.Extract();
    finally
      Extractor.Free();
    end;
  finally
    if (Stream <> nil) then
      Stream.Free();
  end;
end;

function TSimbaHTTPClient.Post(URL, PostData: String): String;
begin
  FHTTPClient.RequestBody := TStringStream.Create(PostData);
  try
    Result := FHTTPClient.Post(SetURL(URL));
  finally
    FHTTPClient.RequestBody.Free();
    FHTTPClient.RequestBody := nil;
  end;
end;

function TSimbaHTTPClient.PostForm(URL, Data: String): String;
begin
  Result := FHTTPClient.FormPost(SetURL(URL), Data);
end;

function TSimbaHTTPClient.PostFormFile(URL, FieldName, FileName: string): String;
var
  Response: TStringStream;
begin
  Result := '';

  Response := TStringStream.Create();
  try
    FHTTPClient.FileFormPost(SetURL(URL), FieldName, FileName, Response);
  finally
    Result := Response.DataString;

    Response.Free();
  end;
end;

function TSimbaHTTPClient.Patch(URL, Data: String): String;
var
  Response: TStringStream;
begin
  Response := TStringStream.Create();

  FHTTPClient.RequestBody := TStringStream.Create(Data);
  try
    FHTTPClient.HTTPMethod('PATCH', SetURL(URL), Response, []);

    Result := Response.DataString;
  finally
    Response.Free();

    FHTTPClient.RequestBody.Free();
    FHTTPClient.RequestBody := nil;
  end;
end;

function TSimbaHTTPClient.Put(URL, Data: String): String;
begin
  FHTTPClient.RequestBody := TStringStream.Create(Data);
  try
    Result := FHTTPClient.Put(SetURL(URL));
  finally
    FHTTPClient.RequestBody.Free();
    FHTTPClient.RequestBody := nil;
  end;
end;

function TSimbaHTTPClient.Delete(URL, Data: String): String;
begin
  FHTTPClient.RequestBody := TStringStream.Create(Data);
  try
    Result := FHTTPClient.Delete(SetURL(URL));
  finally
    FHTTPClient.RequestBody.Free();
    FHTTPClient.RequestBody := nil;
  end;
end;

class function TSimbaHTTPClient.SimpleGet(URL: String; AllowedStatusCodes: THTTPStatusArray): String;
begin
  Result := '';

  with TSimbaHTTPClient.Create() do
  try
    Result := Get(URL, AllowedStatusCodes);
  finally
    Free();
  end;
end;

class procedure TSimbaHTTPClient.SimpleGetFile(URL, LocalFileName: String; AllowedStatusCodes: THTTPStatusArray);
begin
  with TSimbaHTTPClient.Create() do
  try
    GetFile(URL, LocalFileName, AllowedStatusCodes);
  finally
    Free();
  end;
end;

constructor TSimbaHTTPClient.Create;
begin
  inherited Create();

  FHTTPClient := TSimbaFPHTTPClient.Create(nil);
  FHTTPClient.AllowRedirect := True;
  FHTTPClient.OnDataReceived := @DoDownloadProgress;
  FHTTPClient.OnRedirect := @DoRedirect;
  FHTTPClient.OnConnecting := @DoConnecting;
  FHTTPClient.OnHeaders := @DoHeaders;
  FHTTPClient.OnResponseCode := @DoResponseStatus;

  UserAgent := Format('Mozilla/5.0 (compatible; Simba/%d; Target/%s)', [SIMBA_VERSION, {$I %FPCTARGETOS%} + '-' + {$I %FPCTARGETCPU%}]);
end;

destructor TSimbaHTTPClient.Destroy;
begin
  if (FHTTPClient <> nil) then
    FreeAndNil(FHTTPClient);

  inherited Destroy();
end;

function ToStr(Status: EHTTPStatus): String;
begin
  Result := '';
  WriteStr(Result, Status);
end;

end.

