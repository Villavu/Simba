{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.httpclient;

{$i simba.inc}

{$IFDEF DARWIN}
  {$MODESWITCH objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, fphttpclient, ssockets,
  simba.base, simba.baseclass, simba.json;

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
{$POP}

type
  EHTTPStatusHelper = type helper for EHTTPStatus
  public
    function AsInteger: Integer;
    function AsString: String;
  end;

  TSimbaHTTPConnectingEvent  = procedure(Sender: TObject; URL: String) of object;
  TSimbaHTTPExtractingEvent  = procedure(Sender: TObject; URL: String; Pos, Size: Int64) of object;
  TSimbaHTTPStatusEvent      = procedure(Sender: TObject; ResponseStatus: EHTTPStatus) of object;
  TSimbaHTTPDownloadingEvent = procedure(Sender: TObject; URL, ContentType: String; Position, Size: Int64) of object;

  TSimbaHTTPClient = class(TSimbaBaseClass)
  protected
  type
    TInternalClient = class(TFPHTTPClient)
    protected
      {$IFDEF DARWIN}
      FResponseCode: Integer;
      {$ELSE}
      function ReadResponseHeaders: Integer; override;
      function GetSocketHandler(const UseSSL: Boolean): TSocketHandler; override;
      {$ENDIF}
      procedure DoMethod(const AMethod, AURL: String; Stream: TStream; const AllowedResponseCodes: array of Integer); override;
    public
      OnConnecting: TSimbaHTTPConnectingEvent;
      OnResponseCode: TSimbaHTTPStatusEvent;

      {$IFDEF DARWIN}
      property ResponseStatusCode read FResponseCode;
      {$ENDIF}
    end;
  protected
    FHTTPClient: TInternalClient;
    FURL: String;
    FContentType: String;
    FOnDownloadProgress: TSimbaHTTPDownloadingEvent;
    FOnExtractProgress: TSimbaHTTPExtractingEvent;
    FOnConnecting: TSimbaHTTPConnectingEvent;
    FOnHeaders: TNotifyEvent;
    FOnResponseStatus: TSimbaHTTPStatusEvent;
    FDownloadingFinished: TNotifyEvent;
    FExtractingFinished: TNotifyEvent;

    function SetURL(URL: String): String;

    procedure DoExtractProgress(Sender: TObject; FileName: String; Pos, Size: Int64);
    procedure DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
    procedure DoRedirect(Sender: TObject; const Source: String; var Dest: String);
    procedure DoHeaders(Sender: TObject);

    function GetConnectTimeout: Integer;
    function GetReadWriteTimeout: Integer;
    function GetResponseHeader(AName: String): String;
    function GetResponseStatus: EHTTPStatus;
    function GetResponseHeaders: TStringList;
    function GetRequestHeader(AName: String): String;
    function GetRequestHeaders: TStringList;
    function GetCookies: TStringList;

    procedure SetConnectTimeout(AValue: Integer);
    procedure SetReadWriteTimeout(AValue: Integer);
    procedure SetRequestHeader(AName: String; Value: String);
    procedure SetCookies(Value: TStringList);
  public
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property ReadWriteTimeout: Integer read GetReadWriteTimeout write SetReadWriteTimeout;

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

    property Cookies: TStringList read GetCookies write SetCookies;

    // Clear cookies and request data
    procedure Reset;

    // Header request, returns response code
    // Use ResponseHeader after
    function Head(URL: String): EHTTPStatus;

    // Writes page contents to result
    function Get(URL: String): String;

    // Parses page contents as json and returns
    function GetJson(URL: String): TSimbaJSONParser;

    // Writes page contents to a file.
    function GetFile(URL, LocalFileName: String): Boolean;

    // Extracts page contents to file treating contents as .zip
    procedure GetZip(URL, OutputPath: String; Flat: Boolean = False; IgnoreList: TStringArray = nil);

    // Post Form data (www-urlencoded) in request body.
    // Return response
    function PostForm(URL, Data: String): String;
    function PostFormFile(URL, FieldName, FileName: string): String;

    // Other http requests with `data` being sent in request body.
    function Post(URL, Data: String): String;
    function Patch(URL, Data: String): String;
    function Put(URL, Data: String): String;
    function Delete(URL, Data: String): String;
    function Options(URL, Data: String): String;

    constructor Create; reintroduce;
    constructor CreateWithProxy(Host: String; User: String = ''; Pass: String = ''); reintroduce;
    destructor Destroy; override;
  end;

  function URLFetch(URL: String): String;
  function URLFetchToFile(URL, DestFile: String): Boolean;

  function URLEncode(URL: String): String;
  function URLDecode(URL: String): String;

implementation

uses
  {$IFDEF DARWIN}
  CocoaAll, CocoaUtils,
  {$ENDIF}
  simba.zip, simba.openssl, simba.vartype_string;

function EHTTPStatusHelper.AsInteger: Integer;
begin
  Result := Integer(Self);
end;

function EHTTPStatusHelper.AsString: String;
begin
  Result := '';
  WriteStr(Result, Self);
end;

function TSimbaHTTPClient.GetResponseHeaders: TStringList;
begin
  Result := FHTTPClient.ResponseHeaders as TStringList;
end;

procedure TSimbaHTTPClient.DoRedirect(Sender: TObject; const Source: String; var Dest: String);
begin
  FURL := Dest;
end;

procedure TSimbaHTTPClient.DoHeaders(Sender: TObject);
begin
  FContentType := ResponseHeader['Content-Type'];
  if Assigned(OnHeadersReceived) then
    OnHeadersReceived(Self);
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

procedure TSimbaHTTPClient.Reset;
begin
  FHTTPClient.Cookies.Clear();
  FHTTPClient.RequestHeaders.Clear();
  FHTTPClient.AddHeader('User-Agent', Format('Mozilla/5.0 (compatible; Simba/%d; Target/%s)', [SIMBA_VERSION, {$I %FPCTARGETOS%} + '-' + {$I %FPCTARGETCPU%}]));
end;

function TSimbaHTTPClient.Head(URL: String): EHTTPStatus;
begin
  FHTTPClient.HTTPMethod('HEAD', SetURL(URL), nil, []);

  Result := GetResponseStatus();
end;

procedure TSimbaHTTPClient.SetCookies(Value: TStringList);
begin
  FHTTPClient.Cookies := Value;
end;

function TSimbaHTTPClient.GetConnectTimeout: Integer;
begin
  Result := FHTTPClient.ConnectTimeout;
end;

function TSimbaHTTPClient.GetReadWriteTimeout: Integer;
begin
  Result := FHTTPClient.IOTimeout;
end;

procedure TSimbaHTTPClient.SetConnectTimeout(AValue: Integer);
begin
  FHTTPClient.ConnectTimeout := AValue;
end;

procedure TSimbaHTTPClient.SetReadWriteTimeout(AValue: Integer);
begin
  FHTTPClient.IOTimeout := AValue;
end;

function TSimbaHTTPClient.SetURL(URL: String): String;
begin
  FURL := URL;
  if (not FURL.StartsWith('http://', False)) and (not FURL.StartsWith('https://', False)) then
    FURL := 'http://' + FURL;

  Result := FURL;
end;

procedure TSimbaHTTPClient.DoExtractProgress(Sender: TObject; FileName: String; Pos, Size: Int64);
begin
  if (FOnExtractProgress <> nil) then
    FOnExtractProgress(Self, FileName, Pos, Size);
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

function TSimbaHTTPClient.Get(URL: String): String;
var
  Stream: TRawByteStringStream;
begin
  Result := '';

  Stream := TRawByteStringStream.Create();
  try
    FHTTPClient.HTTPMethod('GET', SetURL(URL), Stream, []);

    Result := Stream.DataString;
  finally
    Stream.Free();
  end;
end;

function TSimbaHTTPClient.GetJson(URL: String): TSimbaJSONParser;
begin
  Result := TSimbaJSONParser.Create(Get(URL));
end;

function TSimbaHTTPClient.GetFile(URL, LocalFileName: String): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;

  Stream := TFileStream.Create(LocalFileName, fmCreate);
  try
    FHTTPClient.HTTPMethod('GET', SetURL(URL), Stream, []);

    Result := Stream.Size > 0;
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

    if (FHTTPClient.ResponseStatusCode = 200) then
    begin
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
    end;
  finally
    if (Stream <> nil) then
      Stream.Free();
  end;
end;

function TSimbaHTTPClient.Post(URL, Data: String): String;
begin
  FHTTPClient.RequestBody := TStringStream.Create(Data);
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

function TSimbaHTTPClient.Options(URL, Data: String): String;
begin
  FHTTPClient.RequestBody := TStringStream.Create(Data);
  try
    Result := FHTTPClient.Options(SetURL(URL));
  finally
    FHTTPClient.RequestBody.Free();
    FHTTPClient.RequestBody := nil;
  end;
end;

constructor TSimbaHTTPClient.Create;
begin
  inherited Create();

  FHTTPClient := TInternalClient.Create(nil);
  FHTTPClient.AllowRedirect := True;
  FHTTPClient.OnDataReceived := @DoDownloadProgress;
  FHTTPClient.OnRedirect := @DoRedirect;
  FHTTPClient.OnHeaders := @DoHeaders;
  FHTTPClient.OnConnecting := FOnConnecting;
  FHTTPClient.OnResponseCode := FOnResponseStatus;

  Reset();
end;

constructor TSimbaHTTPClient.CreateWithProxy(Host: String; User: String; Pass: String);
begin
  Create();

  with FHTTPClient.Proxy do
  begin
    Host := Host.Before(':');
    Port := Host.After(':').ToInteger();
    UserName := User;
    Password := Pass;
  end;
end;

destructor TSimbaHTTPClient.Destroy;
begin
  if (FHTTPClient <> nil) then
    FreeAndNil(FHTTPClient);

  inherited Destroy();
end;

procedure TSimbaHTTPClient.TInternalClient.DoMethod(const AMethod, AURL: String; Stream: TStream; const AllowedResponseCodes: array of Integer);
{$IFDEF DARWIN}
var
  urlRequest: NSMutableURLRequest;
  requestData: NSMutableData;
  urlResponse: NSHTTPURLResponse;
  error: NSError;
  urlData: NSData;
  ResponseKeys, ResponseValues: NSArray;
  LocalPool: NSAutoReleasePool;
  I: Integer;
begin
  FResponseCode := 0;
  if Assigned(OnConnecting) then
    OnConnecting(Self, AURL);

  LocalPool := NSAutoReleasePool.alloc.init;
  try
    urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(NSURL.URLWithString(NSSTR(AURL)), NSURLRequestUseProtocolCachePolicy, ConnectTimeout div 1000);
    urlRequest.setHTTPMethod(NSSTR(AMethod));

    if Assigned(RequestBody) and (RequestBody.Size > 0) then
    begin
      RequestData := NSMutableData.alloc.initWithLength(RequestBody.Size);

      RequestBody.Position := 0;
      RequestBody.Write(RequestData.mutableBytes^, RequestBody.Size);

      urlRequest.setHTTPBody(RequestData);
    end;

    for I := 0 to RequestHeaders.Count - 1 do
      urlRequest.addValue_forHTTPHeaderField(NSSTR(RequestHeaders.ValueFromIndex[I]), NSSTR(RequestHeaders.Names[I]));

    urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(urlRequest, @urlResponse, @error);
    if not Assigned(urlData) then
      Exit;

    FResponseCode := urlResponse.statusCode;
    if Assigned(OnResponseCode) then
      OnResponseCode(Self, EHTTPStatus(FResponseCode));

    ResponseKeys   := urlResponse.allHeaderFields.allKeys;
    ResponseValues := urlResponse.allHeaderFields.allValues;

    for I := 0 to ResponseKeys.count - 1 do
    begin
      ResponseHeaders.AddPair(
        NSStringToString(NSString(ResponseKeys.objectAtIndex(I))),
        NSStringToString(NSString(ResponseValues.objectAtIndex(I)))
      );
    end;

    if (Stream <> nil) then // Can be nil for HEAD request etc.
    begin
      Stream.Position := 0;
      Stream.Write(urlData.bytes^, urlData.length);
      Stream.Position := 0;
    end;
  finally
    LocalPool.release;
  end;
end;
{$ELSE}
begin
  if Assigned(OnConnecting) then
    OnConnecting(Self, AURL);

  inherited;
end;

function TSimbaHTTPClient.TInternalClient.ReadResponseHeaders: Integer;
begin
  Result := inherited ReadResponseHeaders();

  if Assigned(OnResponseCode) then
    OnResponseCode(Self, EHTTPStatus(Result));
end;

function TSimbaHTTPClient.TInternalClient.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;
begin
  if UseSSL then
    LoadSSL();

  Result := inherited GetSocketHandler(UseSSL);
end;
{$ENDIF}

function URLFetch(URL: String): String;
begin
  with TSimbaHTTPClient.Create() do
  try
    Result := Get(URL);
  finally
    Free();
  end;
end;

function URLFetchToFile(URL, DestFile: String): Boolean;
begin
  with TSimbaHTTPClient.Create() do
  try
    Result := GetFile(URL, DestFile);
  finally
    Free();
  end;
end;

function URLEncode(URL: String): String;
begin
  Result := EncodeURLElement(URL);
end;

function URLDecode(URL: String): String;
begin
  Result := DecodeURLElement(URL);
end;

end.

