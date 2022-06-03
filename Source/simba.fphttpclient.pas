{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Some minor changes to FPHTTPClient
    - Load OpenSSL if needed
    - Never raise exception on CheckResponseCode
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
  fphttpclient, openssl, ssockets, opensslsockets
  {$IFDEF USE_NSURLCONNECTION},
  cocoaall, cocoautils
  {$ENDIF};

type
  {$IFDEF USE_NSURLCONNECTION}
  TSimbaFPHTTPClient = class(TFPHTTPClient)
  protected
    FResponseCode: Integer;

    procedure DoMethod(const AMethod, AURL: String; Stream : TStream; const AllowedResponseCodes: array of Integer); override;
  public
    property ResponseStatusCode read FResponseCode;
  end;
  {$ELSE}
  TSimbaFPHTTPClient = class(TFPHTTPClient)
  protected
    function CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean; override;
    function GetSocketHandler(const UseSSL: Boolean): TSocketHandler; override;
  end;
  {$ENDIF}

implementation

{$IFDEF USE_NSURLCONNECTION}
procedure TSimbaFPHTTPClient.DoMethod(const AMethod, AURL: String; Stream: TStream; const AllowedResponseCodes: array of Integer);
var
  urlRequest  : NSMutableURLRequest;
  requestData : NSMutableData;
  urlResponse : NSHTTPURLResponse;
  error       : NSError;
  urlData     : NSData;
  ResponseKeys, ResponseValues: NSArray;
  I: Integer;
begin
  FResponseCode := 0;

  urlRequest := NSMutableURLRequest.requestWithURL_cachePolicy_timeoutInterval(NSURL.URLWithString(NSSTR(AURL)), NSURLRequestUseProtocolCachePolicy, ConnectTimeout div 1000);
  urlRequest.setHTTPMethod(NSSTR(AMethod));

  if (RequestBody <> nil) and (RequestBody.Size > 0) then
  try
    RequestData := NSMutableData.alloc.initWithLength(RequestBody.Size);

    RequestBody.Position := 0;
    RequestBody.Write(RequestData.mutableBytes^, RequestBody.Size);

    urlRequest.setHTTPBody(RequestData);
  finally
    RequestData.release;
  end;

  for I := 0 to RequestHeaders.Count - 1 do
    urlRequest.addValue_forHTTPHeaderField(NSSTR(RequestHeaders.ValueFromIndex[I]), NSSTR(RequestHeaders.Names[I]));

  urlData := NSURLConnection.sendSynchronousRequest_returningResponse_error(urlRequest, @urlResponse, @error);
  if not Assigned(urlData) then
    Exit;

  FResponseCode := urlResponse.statusCode;

  ResponseKeys   := urlResponse.allHeaderFields.allKeys;
  ResponseValues := urlResponse.allHeaderFields.allValues;

  for I := 0 to ResponseKeys.count - 1 do
  begin
    ResponseHeaders.AddPair(
      NSStringToString(NSString(ResponseKeys.objectAtIndex(I))),
      NSStringToString(NSString(ResponseValues.objectAtIndex(I)))
    );
  end;

  Stream.Position := 0;
  Stream.Write(urlData.bytes^, urlData.length);
  Stream.Position := 0;
end;
{$ELSE}
function TSimbaFPHTTPClient.GetSocketHandler(const UseSSL: Boolean): TSocketHandler;
begin
  if UseSSL and (not IsSSLLoaded) then
    InitSSLInterface();

  Result := inherited GetSocketHandler(UseSSL);
end;

function TSimbaFPHTTPClient.CheckResponseCode(ACode: Integer; const AllowedResponseCodes: array of Integer): Boolean;
begin
  Result := True;
end;
{$ENDIF}

end.
