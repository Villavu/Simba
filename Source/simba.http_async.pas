{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.http_async;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.httpclient;

type
  TASyncHTTPResult = record
    URL: String;
    Response: EHTTPStatus;
    Data: String;
    Headers: TStringArray;
    Exception: String;
    TimeUsed: Double;
  end;
  TASyncHTTPFinishEvent = procedure(constref Result: TASyncHTTPResult) of object;
  TASyncHTTPProgressEvent = procedure(URL: String; Position, Size: Int64) of object;

  ASyncHTTP = class
    class procedure Get(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent); static;
    class procedure Post(URL: String; RequestHeaders: TStringArray; Data: String; OnFinish: TASyncHTTPFinishEvent); static;
    class procedure GetFile(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent); static;
  end;

implementation

uses
  simba.datetime;

type
  TURLFetchInBackground = class(TThread)
  protected
    FURL: String;
    FDestFile: String;
    FRequestHeaders: TStringArray;
    FOnFinished: TASyncHTTPFinishEvent;
    FOnProgress: TASyncHTTPProgressEvent;

    procedure DoProgress(Sender: TObject; URL, ContentType: String; Position, Size: Int64);

    procedure Execute; override;
  public
    constructor Create(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent); reintroduce;
    constructor Create(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent); reintroduce;
  end;

  TURLPostInBackground = class(TThread)
  protected
    FURL: String;
    FPostData: String;
    FRequestHeaders: TStringArray;
    FOnFinished: TASyncHTTPFinishEvent;

    procedure Execute; override;
  public
    constructor Create(URL, PostData: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent); reintroduce;
  end;

constructor TURLPostInBackground.Create(URL, PostData: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FURL := URL;
  FPostData := PostData;
  FRequestHeaders := RequestHeaders;
  FOnFinished := OnFinish;
end;

procedure TURLPostInBackground.Execute;
var
  Result: TASyncHTTPResult;
  I: Integer;
begin
  Result := Default(TASyncHTTPResult);
  Result.URL := FURL;
  Result.TimeUsed := HighResolutionTime();

  try
    with TSimbaHTTPClient.Create() do
    try
      I := 0;
      while (I < High(FRequestHeaders)) do
      begin
        RequestHeader[FRequestHeaders[i]] := FRequestHeaders[i+1];
        I += 2;
      end;

      Result.Data := Post(FURL, FPostData);
      Result.Response := ResponseStatus;
      Result.Headers := ResponseHeaders.ToStringArray;
    finally
      Free();
    end;
  except
    on E: Exception do
      Result.Exception := E.Message;
  end;

  Result.TimeUsed := HighResolutionTime() - Result.TimeUsed;

  if Assigned(FOnFinished) then
    FOnFinished(Result);
end;

class procedure ASyncHTTP.Get(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent);
begin
  TURLFetchInBackground.Create(URL, RequestHeaders, OnFinish, OnProgress);
end;

class procedure ASyncHTTP.GetFile(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent);
begin
  TURLFetchInBackground.Create(URL, RequestHeaders, DestFile, OnFinish, OnProgress);
end;

class procedure ASyncHTTP.Post(URL: String; RequestHeaders: TStringArray; Data: String; OnFinish: TASyncHTTPFinishEvent);
begin
  TURLPostInBackground.Create(URL, Data, RequestHeaders, OnFinish);
end;

procedure TURLFetchInBackground.DoProgress(Sender: TObject; URL, ContentType: String; Position, Size: Int64);
begin
  if Assigned(FOnProgress) then
    FOnProgress(URL, Position, Size);
end;

procedure TURLFetchInBackground.Execute;
var
  Result: TASyncHTTPResult;
  I: Integer;
begin
  Result := Default(TASyncHTTPResult);
  Result.URL := FURL;
  Result.TimeUsed := HighResolutionTime();

  try
    with TSimbaHTTPClient.Create() do
    try
      OnDownloadProgress := @DoProgress;

      I := 0;
      while (I < High(FRequestHeaders)) do
      begin
        RequestHeader[FRequestHeaders[i]] := FRequestHeaders[i+1];
        I += 2;
      end;

      if (FDestFile <> '') then
      begin
        GetFile(FURL, FDestFile);

        Result.Data := FDestFile;
      end else
        Result.Data := Get(FURL);

      Result.Response := ResponseStatus;
      Result.Headers := ResponseHeaders.ToStringArray;
    finally
      Free();
    end;
  except
    on E: Exception do
      Result.Exception := E.Message;
  end;

  Result.TimeUsed := HighResolutionTime() - Result.TimeUsed;

  if Assigned(FOnFinished) then
    FOnFinished(Result);
end;

constructor TURLFetchInBackground.Create(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FURL := URL;
  FOnFinished := OnFinish;
  FOnProgress := OnProgress;
  FRequestHeaders := RequestHeaders;
end;

constructor TURLFetchInBackground.Create(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent);
begin
  Create(URL, RequestHeaders, OnFinish, OnProgress);

  FDestFile := DestFile;
end;

end.

