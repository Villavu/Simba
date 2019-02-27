unit simba.httpclient;

{$mode objfpc}{$H+}
{$i Simba.inc}

interface

uses
  classes, sysutils,
  simba.fphttpclient, simba.archive;

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
  TSimbaHTTPProgressEvent = procedure(Sender: TObject; URL: String; Position, Size: Int64) of object;
  TSimbaHTTPClientProxy = record
    Host: String;
    Port: Int32;
    UserName: String;
    Password: String;
  end;

  TSimbaHTTPClient = class
  protected
    FHTTPClient: TFPHTTPClient;
    FURL: String;
    FOnDownloadProgress: TSimbaHTTPProgressEvent;
    FOnExtractProgress: TSimbaHTTPProgressEvent;

    procedure DoExtractProgress(Sender: TObject; FileName: String; Position, Size: Int64);
    procedure DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
    procedure DoRedirect(Sender: TObject; const Source: String; var Dest: String);

    function GetResponseHeader(Name: String): String;
    function GetResponseCode: Int32;
    function GetResponseHeaders: TStrings;
    function GetRequestHeader(Name: String): String;
    function GetRequestHeaders: TStrings;
    function GetCookies: TStrings;
    function GetProxy: TSimbaHTTPClientProxy;

    procedure SetRequestHeader(Name: String; Value: String);
    procedure SetProxy(Value: TSimbaHTTPClientProxy);
  public
    property OnDownloadProgress: TSimbaHTTPProgressEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnExtractProgress: TSimbaHTTPProgressEvent read FOnExtractProgress write FOnExtractProgress;

    property ResponseCode: Int32 read GetResponseCode;
    property ResponseHeader[Name: String]: String read GetResponseHeader;
    property ResponseHeaders: TStrings read GetResponseHeaders;

    property RequestHeader[Name: String]: String read GetRequestHeader write SetRequestHeader;
    property RequestHeaders: TStrings read GetRequestHeaders;

    property Cookies: TStrings read GetCookies;
    property Proxy: TSimbaHTTPClientProxy read GetProxy write SetProxy;

    // Writes page contents to result
    function Get(URL: String): String; overload;

    // Writes page contents to stream.
    procedure Get(URL: String; Stream: TMemoryStream); overload;

    // Writes page contents to a file.
    procedure Get(URL: String; FileName: String); overload;

    // Extracts page contents to file using a TSimbaArchiveExtract class
    procedure GetArchive(URL: String; OutputPath: String; Flat: Boolean; ExtractorClass: TSimbaArchiveExtractorClass);

    // Extracts page contents to file treating contents as .zip
    procedure GetZip(URL: String; OutputPath: String; Flat: Boolean);

    // Extracts page contents to file treating contents as .tar
    procedure GetTar(URL: String; OutputPath: String; Flat: Boolean);

    // Extracts page contents to file treating contents as .tar.gz
    procedure GetTarGZ(URL: String; OutputPath: String; Flat: Boolean);

    // Extracts page contents to file treating contents as .tar.bz2
    procedure GetTarBZ2(URL: String; OutputPath: String; Flat: Boolean);

    // Post string (URL parameters) returns response
    function Post(URL: String; Parameters: TStringArray): String; overload;

    // Postsstring (URL parameters) and writes response to stream
    procedure Post(URL: String; Parameters: TStringArray; Response: TMemoryStream); overload;

    // Post string (form data) returns response
    function Post(URL: String; Data: String): String; overload;

    // Post string (form data) and writes response to stream
    procedure Post(URL: String; Data: String; Response: TMemoryStream); overload;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  mufasabase, simbaunit,
  simba.zip, simba.tar, simba.tar_gz, simba.tar_bz2,
  simba.environment, simba.openssloader;

type
  TSimbaHTTPClientBase = class(TFPHTTPClient) // default will raise a exception on a response code that isn't HTTP_OK or HTTP_FOUND.
  protected
    function CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean; override;
  end;

function TSimbaHTTPClientBase.CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean;
begin
  Result := True;
end;

function TSimbaHTTPClient.GetResponseHeaders: TStrings;
begin
  Result := FHTTPClient.ResponseHeaders;
end;

procedure TSimbaHTTPClient.DoRedirect(Sender: TObject; const Source: String; var Dest: String);
begin
  FURL := Dest;
end;

function TSimbaHTTPClient.GetRequestHeader(Name: String): String;
begin
  Result := FHTTPClient.GetHeader(Name);
end;

function TSimbaHTTPClient.GetRequestHeaders: TStrings;
begin
  Result := FHTTPClient.RequestHeaders;
end;

function TSimbaHTTPClient.GetCookies: TStrings;
begin
  Result := FHTTPClient.Cookies;
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

procedure TSimbaHTTPClient.DoExtractProgress(Sender: TObject; FileName: String; Position, Size: Int64);
begin
  if (FOnExtractProgress <> nil) then
    FOnExtractProgress(Self, FileName, Position, Size);
end;

procedure TSimbaHTTPClient.DoDownloadProgress(Sender: TObject; const Size, Position: Int64);
begin
  if (FOnDownloadProgress <> nil) then
    FOnDownloadProgress(Self, FURL, Position, Size);
end;

function TSimbaHTTPClient.GetResponseHeader(Name: String): String;
var
  Header: String;
begin
  Result := '';

  if Name <> '' then
  begin
    if Name[Length(Name)] <> ':' then
      Name := Name + ':';

    for Header in ResponseHeaders do
      if Header.StartsWith(Name) then
        Exit(Name.SubString(Length(Name) + 1).Trim());
  end;
end;

function TSimbaHTTPClient.GetResponseCode: Int32;
begin
  Result := FHTTPClient.ResponseStatusCode;
end;

procedure TSimbaHTTPClient.Get(URL: String; Stream: TMemoryStream);
begin
  FURL := URL;

  FHTTPClient.Get(FURL, Stream);
end;

function TSimbaHTTPClient.Get(URL: String): String;
begin
  FURL := URL;

  Result := FHTTPClient.Get(FURL);
end;

procedure TSimbaHTTPClient.Get(URL: String; FileName: String);
begin
  FURL := URL;

  FHTTPClient.Get(FURL, FileName);
end;

procedure TSimbaHTTPClient.GetArchive(URL: String; OutputPath: String; Flat: Boolean; ExtractorClass: TSimbaArchiveExtractorClass);
var
  Stream: TMemoryStream;
  Extractor: TSimbaArchiveExtractor;
begin
  Stream := TMemoryStream.Create();

  try
    Get(URL, Stream);

    if (ResponseCode = HTTP_OK) then
    begin
      Extractor := ExtractorClass.Create();
      Extractor.OnProgress := @DoExtractProgress;
      Extractor.InputStream := Stream;
      Extractor.OutputPath := OutputPath;
      Extractor.Flat := Flat;

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

procedure TSimbaHTTPClient.GetZip(URL: String; OutputPath: String; Flat: Boolean);
begin
  GetArchive(URL, OutputPath, Flat, TSimbaZipExtractor);
end;

procedure TSimbaHTTPClient.GetTar(URL: String; OutputPath: String; Flat: Boolean);
begin
  GetArchive(URL, OutputPath, Flat, TSimbaTarExtractor);
end;

procedure TSimbaHTTPClient.GetTarGZ(URL: String; OutputPath: String; Flat: Boolean);
begin
  GetArchive(URL, OutputPath, Flat, TSimbaTarGZExtractor);
end;

procedure TSimbaHTTPClient.GetTarBZ2(URL: String; OutputPath: String; Flat: Boolean);
begin
  GetArchive(URL, OutputPath, Flat, TSimbaTarBZ2Extractor);
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

    Result := Get(FURL);
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

    Get(FURL, Response);
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

constructor TSimbaHTTPClient.Create;
begin
  FHTTPClient := TSimbaHTTPClientBase.Create(nil);
  FHTTPClient.AllowRedirect := True;
  FHTTPClient.OnDataReceived := @DoDownloadProgress;
  FHTTPClient.OnRedirect := @DoRedirect;
  FHTTPClient.AddHeader('User-Agent', Format('Mozilla/5.0 (compatible; Simba/%d; Target/%s)', [SimbaVersion, {$I %FPCTARGETOS%} + '-' + {$I %FPCTARGETCPU%}]));
end;

destructor TSimbaHTTPClient.Destroy;
begin
  FHTTPClient.Free();

  inherited Destroy();
end;

initialization
  {$IFDEF SIMBA_OPENSSL}
  with TSimbaOpenSSLLoader.Create() do
  try
    if not Load() then
      WriteLn('[WEB]: OpenSSL initialization failed. Falling back to system libaries');
  finally
    Free();
  end;
  {$ENDIF}

end.

