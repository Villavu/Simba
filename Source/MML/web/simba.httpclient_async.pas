unit simba.httpclient_async;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.httpclient;

type
  TSimbaHTTPRequest = class
  protected
    FURL: String;
    FContents: String;
    FRunning: Boolean;
    FHTTPClient: TSimbaHTTPClient;
    FResponseCode: Int32;
    FException: Exception;

    procedure Execute;
  public
    property Contents: String read FContents;
    property Exception: Exception read FException;
    property ResponseCode: Int32 read FResponseCode;
    property Running: Boolean read FRunning;

    constructor Create(URL: String; OnProgress: TSimbaHTTPProgressEvent = nil);
    destructor Destroy; override;
  end;

  TSimbaHTTPRequestZIP = class
  protected
    FURL: String;
    FOutputPath: String;
    FFlat: Boolean;
    FRunning: Boolean;
    FHTTPClient: TSimbaHTTPClient;
    FResponseCode: Int32;
    FException: Exception;

    procedure Execute;
  public
    property Running: Boolean read FRunning;
    property ResponseCode: Int32 read FResponseCode;
    property Exception: Exception read FException;

    constructor Create(URL: String; OutputPath: String; Flat: Boolean; OnDownloadProgress: TSimbaHTTPProgressEvent = nil; OnExtractProgress: TSimbaHTTPProgressEvent = nil);
    destructor Destroy; override;
  end;

implementation

procedure TSimbaHTTPRequest.Execute;
begin
  try
    FContents := FHTTPClient.Get(FURL);
  except
    FException := SysUtils.Exception(AcquireExceptionObject());
  end;

  FResponseCode := FHTTPClient.ResponseCode;
  FRunning := False;
end;

constructor TSimbaHTTPRequest.Create(URL: String; OnProgress: TSimbaHTTPProgressEvent);
begin
  FHTTPClient := TSimbaHTTPClient.Create();
  FHTTPClient.OnDownloadProgress := OnProgress;
  FURL := URL;
  FRunning := True;

  TThread.ExecuteInThread(@Self.Execute);
end;

destructor TSimbaHTTPRequest.Destroy;
begin
  FHTTPClient.Free();
  if (FException <> nil) then
    FException.Free();

  inherited Destroy();
end;

procedure TSimbaHTTPRequestZIP.Execute;
begin
  try
    FHTTPClient.GetZip(FURL, FOutputPath, FFlat);
  except
    FException := SysUtils.Exception(AcquireExceptionObject());
  end;

  FResponseCode := FHTTPClient.ResponseCode;
  FRunning := False;
end;

constructor TSimbaHTTPRequestZIP.Create(URL: String; OutputPath: String; Flat: Boolean; OnDownloadProgress: TSimbaHTTPProgressEvent; OnExtractProgress: TSimbaHTTPProgressEvent);
begin
  FHTTPClient := TSimbaHTTPClient.Create();
  FHTTPClient.OnDownloadProgress := OnDownloadProgress;
  FHTTPClient.OnExtractProgress := OnExtractProgress;
  FURL := URL;
  FOutputPath := OutputPath;
  FFlat := Flat;
  FRunning := True;

  TThread.ExecuteInThread(@Self.Execute);
end;

destructor TSimbaHTTPRequestZIP.Destroy;
begin
  FHTTPClient.Free();
  if (FException <> nil) then
    FException.Free();

  inherited Destroy();
end;

end.

