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
  TASyncHTTPFinishedEvent = procedure(constref Result: TASyncHTTPResult) of object;

  ASyncHTTP = class
    class procedure Get(URL: String; OnFinished: TASyncHTTPFinishedEvent); static;
    class procedure Get(URL: String; DestFile: String; OnFinished: TASyncHTTPFinishedEvent); static;
    class procedure Post(URL, PostData: String; OnFinished: TASyncHTTPFinishedEvent); static;
    class procedure Post(URL, PostData: String; Headers: TStringArray; OnFinished: TASyncHTTPFinishedEvent); static;
  end;

implementation

uses
  simba.datetime;

type
  TURLFetchInBackground = class(TThread)
  protected
    FURL: String;
    FDestFile: String;
    FOnFinished: TASyncHTTPFinishedEvent;

    procedure Execute; override;
  public
    constructor Create(URL: String; OnFinished: TASyncHTTPFinishedEvent); reintroduce;
    constructor Create(URL: String; DestFile: String; OnFinished: TASyncHTTPFinishedEvent); reintroduce;
  end;

  TURLPostInBackground = class(TThread)
  protected
    FURL: String;
    FPostData: String;
    FHeaders: TStringArray;
    FOnFinished: TASyncHTTPFinishedEvent;

    procedure Execute; override;
  public
    constructor Create(URL, PostData: String; Headers: TStringArray; OnFinished: TASyncHTTPFinishedEvent); reintroduce;
  end;

constructor TURLPostInBackground.Create(URL, PostData: String; Headers: TStringArray; OnFinished: TASyncHTTPFinishedEvent);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FURL := URL;
  FPostData := PostData;
  FHeaders := Headers;
  FOnFinished := OnFinished;
end;

procedure TURLPostInBackground.Execute;
var
  Result: TASyncHTTPResult;
begin
  Result := Default(TASyncHTTPResult);
  Result.URL := FURL;
  Result.TimeUsed := HighResolutionTime();

  try
    with TSimbaHTTPClient.Create() do
    try
      RequestHeaders.AddStrings(FHeaders);

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

class procedure ASyncHTTP.Get(URL: String; OnFinished: TASyncHTTPFinishedEvent);
begin
  TURLFetchInBackground.Create(URL, OnFinished);
end;

class procedure ASyncHTTP.Get(URL: String; DestFile: String; OnFinished: TASyncHTTPFinishedEvent);
begin
  TURLFetchInBackground.Create(URL, DestFile, OnFinished);
end;

class procedure ASyncHTTP.Post(URL, PostData: String; OnFinished: TASyncHTTPFinishedEvent);
begin
  TURLPostInBackground.Create(URL, PostData, [], OnFinished);
end;

class procedure ASyncHTTP.Post(URL, PostData: String; Headers: TStringArray; OnFinished: TASyncHTTPFinishedEvent);
begin
  TURLPostInBackground.Create(URL, PostData, Headers, OnFinished);
end;

procedure TURLFetchInBackground.Execute;
var
  Result: TASyncHTTPResult;
begin
  Result := Default(TASyncHTTPResult);
  Result.URL := FURL;
  Result.TimeUsed := HighResolutionTime();

  try
    with TSimbaHTTPClient.Create() do
    try
      if (FDestFile <> '') then
      begin
        GetFile(FURL, FDestFile, []);

        Result.Data := FDestFile;
      end else
        Result.Data := Get(FURL, []);

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

constructor TURLFetchInBackground.Create(URL: String; OnFinished: TASyncHTTPFinishedEvent);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FURL := URL;
  FOnFinished := OnFinished;
end;

constructor TURLFetchInBackground.Create(URL: String; DestFile: String; OnFinished: TASyncHTTPFinishedEvent);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FDestFile := DestFile;
  FURL := URL;
  FOnFinished := OnFinished;
end;

end.

