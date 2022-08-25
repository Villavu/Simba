{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.httpclient;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.fphttpclient, simba.mufasatypes;

const
  // Information
  HTTP_CONTINUE = 100;
  HTTP_SWITCHING_PROTOCOLS = 101;

  // Success
  HTTP_OK = 200;
  HTTP_CREATED = 201;
  HTTP_ACCEPTED = 202;
  HTTP_NONAUTHORITATIVE_INFORMATION = 203;
  HTTP_NO_CONTENT = 204;
  HTTP_RESET_CONTENT = 205;
  HTTP_PARTIAL_CONTENT = 206;

  // Redirection
  HTTP_MULTIPLE_CHOICES = 300;
  HTTP_MOVED_PERMANENTLY = 301;
  HTTP_FOUND = 302;
  HTTP_SEE_OTHER = 303;
  HTTP_NOT_MODIFIED = 304;
  HTTP_USE_PROXY = 305;
  HTTP_UNUSED = 306;
  HTTP_TEMPORARY_REDIRECT = 307;

  // Client Error
  HTTP_BAD_REQUEST = 400;
  HTTP_UNAUTHORIZED  = 401;
  HTTP_PAYMENT_REQUIRED = 402;
  HTTP_FORBIDDEN = 403;
  HTTP_NOT_FOUND = 404;
  HTTP_METHOD_NOT_ALLOWED = 405;
  HTTP_NOT_ACCEPTABLE = 406;
  HTTP_PROXY_AUTHENTICATION_REQUIRED = 407;
  HTTP_REQUEST_TIMEOUT = 408;
  HTTP_CONFLICT = 409;
  HTTP_GONE = 410;
  HTTP_LENGTH_REQUIRED = 411;
  HTTP_PRECONDITION_FAILED = 412;
  HTTP_REQUEST_ENTITY_TOO_LARGE = 413;
  HTTP_REQUEST_URI_TOO_LONG = 414;
  HTTP_UNSUPPORTED_MEDIA_TYPE = 415;
  HTTP_REQUESTED_RANGE_NOT_SATISFIABLE = 416;
  HTTP_EXPECTATION_FAILED = 417;

  // Server Error
  HTTP_INTERNAL_SERVER_ERROR = 500;
  HTTP_NOT_IMPLEMENTED = 501;
  HTTP_BAD_GATEWAY = 502;
  HTTP_SERVICE_UNAVAILABLE = 503;
  HTTP_GATEWAY_TIMEOUT = 504;
  HTTP_VERSION_NOT_SUPPORTED = 505;

type
  THTTPClientHeadersEvent = procedure(Sender: TObject) of object;
  THTTPClientConnectingEvent = procedure(Sender: TObject; URL: String) of object;
  THTTPClientExtractEvent = procedure(Sender: TObject; URL: String; Percent: Double) of object;
  THTTPClientResponseCodeEvent = procedure(Sender: TObject; ResponseCode: Integer) of object;

  TSimbaHTTPProgressEvent = procedure(Sender: TObject; URL, ContentType: String; Position, Size: Int64) of object;
  TSimbaHTTPClientProxy = record
    Host: String;
    Port: Int32;
    UserName: String;
    Password: String;
  end;

type
  TSimbaHTTPClient = class
  protected
    FHTTPClient: TSimbaFPHTTPClient;
    FURL: String;
    FContentType: String;
    FOnDownloadProgress: TSimbaHTTPProgressEvent;
    FOnExtractProgress: THTTPClientExtractEvent;
    FOnConnecting: THTTPClientConnectingEvent;
    FOnHeaders: THTTPClientHeadersEvent;
    FOnCopyingProgress: THTTPClientExtractEvent;
    FOnResponseCode: THTTPClientResponseCodeEvent;
    FDownloadingFinished: TNotifyEvent;
    FExtractingFinished: TNotifyEvent;
    FCopyingFinished: TNotifyEvent;

    procedure DoExtractProgress(Sender: TObject; FileName: String; Percent: Double);
    procedure DoCopyingProgress(Sender: TObject; FileName: String; Percent: Double);
    procedure DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
    procedure DoRedirect(Sender: TObject; const Source: String; var Dest: String);
    procedure DoConnecting(Sender: TObject; URL: String);
    procedure DoHeaders(Sender: TObject);
    procedure DoResponseCode(Sender: TObject; ResponseCode: Integer);

    function GetResponseHeader(Name: String): String;
    function GetResponseCode: Int32;
    function GetResponseHeaders: TStringList;
    function GetRequestHeader(Name: String): String;
    function GetRequestHeaders: TStringList;
    function GetCookies: TStringList;
    function GetProxy: TSimbaHTTPClientProxy;
    function GetRequestContentType: String;

    procedure SetRequestContentType(Value: String);
    procedure SetRequestHeader(Name: String; Value: String);
    procedure SetProxy(Value: TSimbaHTTPClientProxy);
  public
    property OnDownloadProgress: TSimbaHTTPProgressEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnExtractProgress: THTTPClientExtractEvent read FOnExtractProgress write FOnExtractProgress;
    property OnConnecting: THTTPClientConnectingEvent read FOnConnecting write FOnConnecting;
    property OnHeadersReceived: THTTPClientHeadersEvent read FOnHeaders write FOnHeaders;
    property OnCopyingProgress: THTTPClientExtractEvent read FOnCopyingProgress write FOnCopyingProgress;
    property OnResponseCode: THTTPClientResponseCodeEvent read FOnResponseCode write FOnResponseCode;

    property OnDownloadingFinished: TNotifyEvent read FDownloadingFinished write FDownloadingFinished;
    property OnExtractingFinished: TNotifyEvent read FExtractingFinished write FExtractingFinished;
    property OnCopyingFinished: TNotifyEvent read FCopyingFinished write FCopyingFinished;

    property ResponseCode: Int32 read GetResponseCode;
    property ResponseHeader[Name: String]: String read GetResponseHeader;
    property ResponseHeaders: TStringList read GetResponseHeaders;

    property RequestHeader[Name: String]: String read GetRequestHeader write SetRequestHeader;
    property RequestHeaders: TStringList read GetRequestHeaders;

    property RequestContentType: String read GetRequestContentType write SetRequestContentType;

    property Cookies: TStringList read GetCookies;
    property Proxy: TSimbaHTTPClientProxy read GetProxy write SetProxy;

    // Header request, returns response code
    function Head(URL: String): Integer;

    // Writes page contents to result
    function Get(URL: String; AllowedResponseCodes: array of Integer): String; overload;

    // Writes page contents to stream.
    procedure Get(URL: String; Stream: TStream; AllowedResponseCodes: array of Integer); overload;

    // Writes page contents to a file.
    procedure Get(URL: String; FileName: String; AllowedResponseCodes: array of Integer); overload;

    // Extracts page contents to file treating contents as .zip
    procedure GetZip(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray);

    // Post string (URL parameters) returns response
    function Post(URL: String; Parameters: TStringArray): String; overload;

    // Posts string (URL parameters) and writes response to stream
    procedure Post(URL: String; Parameters: TStringArray; Response: TMemoryStream); overload;

    // Post string (form data) returns response
    function Post(URL: String; Data: String): String; overload;

    // Post string (form data) and writes response to stream
    procedure Post(URL: String; Data: String; Response: TMemoryStream); overload;

    function FormPost(const URL, FieldName, FileName: string): String; overload;
    function FormPost(const URL, FieldName, FileName: string; Stream: TStream): String; overload;

    class function SimpleGet(URL: String; AllowedResponseCodes: array of Integer): String; static;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
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

procedure TSimbaHTTPClient.DoResponseCode(Sender: TObject; ResponseCode: Integer);
begin
  if Assigned(FOnResponseCode) then
    FOnResponseCode(Self, ResponseCode);
end;

function TSimbaHTTPClient.GetRequestHeader(Name: String): String;
begin
  Result := FHTTPClient.GetHeader(Name);
end;

function TSimbaHTTPClient.GetRequestHeaders: TStringList;
begin
  Result := FHTTPClient.RequestHeaders as TStringList;
end;

function TSimbaHTTPClient.GetCookies: TStringList;
begin
  Result := FHTTPClient.Cookies as TStringList;
end;

function TSimbaHTTPClient.GetProxy: TSimbaHTTPClientProxy;
begin
  Result.Host := FHTTPClient.Proxy.Host;
  Result.Port := FHTTPClient.Proxy.Port;
  Result.UserName := FHTTPClient.Proxy.UserName;
  Result.Password := FHTTPClient.Proxy.Password;
end;

procedure TSimbaHTTPClient.SetRequestHeader(Name: String; Value: String);
begin
  FHTTPClient.AddHeader(Name, Value);
end;

procedure TSimbaHTTPClient.SetProxy(Value: TSimbaHTTPClientProxy);
begin
  FHTTPClient.Proxy.Host := Value.Host;
  FHTTPClient.Proxy.Port := Value.Port;
  FHTTPClient.Proxy.UserName := Value.UserName;
  FHTTPClient.Proxy.Password := Value.Password;
end;

function TSimbaHTTPClient.Head(URL: String): Integer;
begin
  FHTTPClient.HTTPMethod('HEAD', URL, nil, []);

  Result := FHTTPClient.ResponseStatusCode;
end;

function TSimbaHTTPClient.GetRequestContentType: String;
begin
  Result := RequestHeader['Content-Type'];
end;

procedure TSimbaHTTPClient.SetRequestContentType(Value: String);
begin
  RequestHeader['Content-Type'] := Value;
end;

procedure TSimbaHTTPClient.DoExtractProgress(Sender: TObject; FileName: String; Percent: Double);
begin
  if (FOnExtractProgress <> nil) then
    FOnExtractProgress(Self, FileName, Percent);
end;

procedure TSimbaHTTPClient.DoCopyingProgress(Sender: TObject; FileName: String; Percent: Double);
begin
  if (FOnCopyingProgress <> nil) then
    FOnCopyingProgress(Self, FileName, Percent);
end;

procedure TSimbaHTTPClient.DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
begin
  if (FOnDownloadProgress <> nil) then
    FOnDownloadProgress(Self, FURL, FContentType, Position, Size);
end;

function TSimbaHTTPClient.GetResponseHeader(Name: String): String;
begin
  Result := FHTTPClient.GetHeader(FHTTPClient.ResponseHeaders, Name);
end;

function TSimbaHTTPClient.GetResponseCode: Int32;
begin
  Result := FHTTPClient.ResponseStatusCode;
end;

procedure TSimbaHTTPClient.Get(URL: String; Stream: TStream; AllowedResponseCodes: array of Integer);
begin
  FURL := URL;
  if (not FURL.StartsWith('http://', False)) and (not FURL.StartsWith('https://', False)) then
    FURL := 'http://' + FURL;

  FHTTPClient.HTTPMethod('GET', FURL, Stream, AllowedResponseCodes);
end;

function TSimbaHTTPClient.Get(URL: String; AllowedResponseCodes: array of Integer): String;
var
  Stream: TRawByteStringStream;
begin
  Result := '';

  Stream := TRawByteStringStream.Create();
  try
    Get(URL, Stream, AllowedResponseCodes);

    Result := Stream.DataString;
  finally
    Stream.Free();
  end;
end;

procedure TSimbaHTTPClient.Get(URL: String; FileName: String; AllowedResponseCodes: array of Integer);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Get(URL, Stream, AllowedResponseCodes);
  finally
    Stream.Free();
  end;
end;

procedure TSimbaHTTPClient.GetZip(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray);
var
  Stream: TMemoryStream;
  Extractor: TSimbaZipExtractor;
begin
  Stream := TMemoryStream.Create();

  try
    Get(URL, Stream, [HTTP_OK]);

    if (ResponseCode = HTTP_OK) then
    begin
      Extractor := TSimbaZipExtractor.Create();
      Extractor.OnProgress := @DoExtractProgress;
      Extractor.OnCopying := @DoCopyingProgress;
      Extractor.OnCopyingFinished := FCopyingFinished;
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
    end;
  finally
    Stream.Free();
  end;
end;

function TSimbaHTTPClient.Post(URL: String; Parameters: TStringArray): String;
var
  i: Int32;
begin
  FURL := URL;

  if (FURL <> '') then
  begin
    if FURL[Length(FURL)] <> '?' then
      FURL := FURL + '?';
    for i := 0 to High(Parameters) do
      FURL := FURL + Parameters[i] + '&';

    Result := Get(FURL, [HTTP_OK]);
  end;
end;

procedure TSimbaHTTPClient.Post(URL: String; Parameters: TStringArray; Response: TMemoryStream);
var
  i: Int32;
begin
  FURL := URL;

  if (FURL <> '') then
  begin
    if FURL[Length(FURL)] <> '?' then
      FURL := FURL + '?';
    for i := 0 to High(Parameters) do
      FURL := FURL + '&' + Parameters[i];

    Get(FURL, Response, [HTTP_OK]);
  end;
end;

function TSimbaHTTPClient.Post(URL: String; Data: String): String;
begin
  FURL := URL;

  Result := FHTTPClient.FormPost(FURL, Data);
end;

procedure TSimbaHTTPClient.Post(URL: String; Data: String; Response: TMemoryStream);
begin
  FURL := URL;

  FHTTPClient.FormPost(FURL, Data, Response);
end;

function TSimbaHTTPClient.FormPost(const URL, FieldName, FileName: string): String;
var
  Response: TStringStream;
begin
  Result := '';

  Response := TStringStream.Create();

  try
    FURL := URL;
    FHTTPClient.FileFormPost(URL, FieldName, FileName, Response);
  finally
    Result := Response.DataString;

    Response.Free();
  end;
end;

function TSimbaHTTPClient.FormPost(const URL, FieldName, FileName: string; Stream: TStream): String;
var
  Response: TStringStream;
begin
  Result := '';

  Response := TStringStream.Create();
  try
    FURL := URL;
    FHTTPClient.StreamFormPost(URL, FieldName, FileName, Stream, Response);
  finally
    Result := Response.DataString;

    Response.Free();
  end;
end;

class function TSimbaHTTPClient.SimpleGet(URL: String; AllowedResponseCodes: array of Integer): String;
begin
  Result := '';

  with TSimbaHTTPClient.Create() do
  try
    Result := Get(URL, AllowedResponseCodes);
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
  FHTTPClient.OnResponseCode := @DoResponseCode;
  FHTTPClient.OnComplete := FCopyingFinished;
  FHTTPClient.AddHeader('User-Agent', Format('Mozilla/5.0 (compatible; Simba/%d; Target/%s)', [SIMBA_VERSION, {$I %FPCTARGETOS%} + '-' + {$I %FPCTARGETCPU%}]));
end;

destructor TSimbaHTTPClient.Destroy;
begin
  if (FHTTPClient <> nil) then
    FreeAndNil(FHTTPClient);

  inherited Destroy();
end;

end.

