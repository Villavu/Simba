{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Some minor changes to FPHTTPClient
    - Load OpenSSL if needed
    - Use NSURLRequest on macOS. Experimental but Apple are trying to move away from OpenSSL it seems
}
unit simba.fphttpclient;

{$i simba.inc}

{$IFDEF DARWIN}
  {$DEFINE USE_NSURLCONNECTION}
{$ENDIF}

{$IFDEF USE_NSURLCONNECTION}
  {$MODESWITCH objectivec1}
{$ENDIF}

interface

uses
  classes, sysutils,
  fphttpclient, ssockets, opensslsockets
  {$IFDEF USE_NSURLCONNECTION},
  cocoaall, cocoautils
  {$ENDIF};

type
  TConnectingEvent = procedure(Sender: TObject; URL: String) of object;
  TResponseCodeEvent = procedure(Sender: TObject; ResponseCode: Integer) of object;

  {$IFDEF USE_NSURLCONNECTION}
  TSimbaFPHTTPClient = class(TFPHTTPClient)
  protected
    FResponseCode: Integer;

    procedure DoMethod(const AMethod, AURL: String; Stream : TStream; const AllowedResponseCodes: array of Integer); override;
  public
    OnConnecting: TConnectingEvent;
    OnResponseCode: TResponseCodeEvent;
    OnComplete: TNotifyEvent;

    property ResponseStatusCode read FResponseCode;
  end;
  {$ELSE}
  TSimbaFPHTTPClient = class(TFPHTTPClient)
  protected
    procedure DoMethod(const AMethod, AURL: String; Stream: TStream; const AllowedResponseCodes: array of Integer); override;
    function ReadResponseHeaders: integer; override;
    function GetSocketHandler(const UseSSL: Boolean): TSocketHandler; override;
  public
    OnConnecting: TConnectingEvent;
    OnResponseCode: TResponseCodeEvent;
    OnComplete: TNotifyEvent;
  end;
  {$ENDIF}

implementation

uses
  simba.openssl;

{$IFDEF USE_NSURLCONNECTION}
procedure TSimbaFPHTTPClient.DoMethod(const AMethod, AURL: String; Stream: TStream; const AllowedResponseCodes: array of Integer);
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
      OnResponseCode(Self, FResponseCode);

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

  if Assigned(OnComplete) then
    OnComplete(Self);
end;
{$ELSE}
function TSimbaFPHTTPClient.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;
begin
  if UseSSL then
    LoadSSL();

  Result := inherited GetSocketHandler(UseSSL);
end;

procedure TSimbaFPHTTPClient.DoMethod(const AMethod, AURL: String; Stream: TStream; const AllowedResponseCodes: array of Integer);
begin
  if Assigned(OnConnecting) then
    OnConnecting(Self, AURL);

  inherited;

  if Assigned(OnComplete) then
    OnComplete(Self);
end;

function TSimbaFPHTTPClient.ReadResponseHeaders: integer;
begin
  Result := inherited ReadResponseHeaders();

  if Assigned(OnResponseCode) then
    OnResponseCode(Self, Result);
end;
{$ENDIF}

end.

