{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_endpoint;

(*

[
  {
    "zip" : "https://api.github.com/repos/ollydev/SRL-Development/zipball/8",
    "notes" : "Fixes Options.GetZoom after RS update",
    "time" : "2021-12-09T16:06:41Z",
    "version" : "8"
  },
  {
    "zip" : "https://api.github.com/repos/ollydev/SRL-Development/zipball/7",
    "notes" : "Lots of documentation!",
    "time" : "2021-09-12T21:55:26Z",
    "version" : "7"
  },
  {
    "zip" : "https://github.com/ollydev/srl-development/archive/interface-elements.zip",
    "branch" : "interface-elements"
  },
  {
    "zip" : "https://github.com/ollydev/srl-development/archive/master.zip",
    "branch" : "master"
  }
]

*)

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.httpclient, simba.httpclient_async;

type
  TSimbaPackageEndpoint = class
  public
  type
    TMessageEvent = procedure(const S: String) of object;
  protected
    FURL: String;
    FProgress: String;

    FOnStatus: TMessageEvent;
    FOnError: TMessageEvent;

    procedure UpdateDownloadProgress(Sender: TObject; URL: String; APosition, ASize: Int64);
    procedure UpdateExtractProgress(Sender: TObject; FileName: String; APosition, ASize: Int64);
  public
    constructor Create(URL: String); virtual;

    function GetReleasesJSON: String; virtual;
    function GetPage(URL: String): String; virtual;
    function GetZip(URL, Destination: String; Flat: Boolean; IgnoreList: TStringArray): Boolean; virtual;

    property OnStatus: TMessageEvent read FOnStatus write FOnStatus;
    property OnError: TMessageEvent read FOnError write FOnError;
  end;

  TSimbaPackageEndpoint_Github = class(TSimbaPackageEndpoint)
  protected
  const
    URL_BRANCHES        = 'https://api.github.com/repos/%s/%s/branches';              // {Owner} {Name}
    URL_RELEASES        = 'https://api.github.com/repos/%s/%s/releases';              // {Owner} {Name}
    URL_OPTIONS         = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

    URL_DOWNLOAD_BRANCH  = 'https://github.com/%s/%s/zipball/refs/heads/%s';          // {Owner} {Name} {Branch}
    URL_DOWNLOAD_RELEASE = 'https://github.com/%s/%s/zipball/refs/tags/%s';           // {Owner} {Name} {Branch}
  protected
    FOwner: String;
    FName: String;
  public
    constructor Create(URL: String); override;

    function GetReleasesJSON: String; override;

    property Owner: String read FOwner;
    property Name: String read FName;
  end;

implementation

uses
  forms, lazloggerbase, fpjson;

procedure TSimbaPackageEndpoint.UpdateDownloadProgress(Sender: TObject; URL: String; APosition, ASize: Int64);
begin
  if (APosition = -1) and (ASize = -1) then
    FProgress := 'Connecting...'
  else
  if (ASize > 0) then
    FProgress := Format('Downloading... (%f / %f MB)', [APosition / (1024 * 1024), ASize / (1024 * 1024)])
  else
    FProgress := Format('Downloading... (%f MB)', [APosition / (1024 * 1024)]);
end;

procedure TSimbaPackageEndpoint.UpdateExtractProgress(Sender: TObject; FileName: String; APosition, ASize: Int64);
begin
  if (ASize > 0) then
    FProgress := Format('Extracting... (%f / %f MB)', [APosition / (1024 * 1024), ASize / (1024 * 1024)])
  else
    FProgress := Format('Extracting... (%f MB)', [APosition / (1024 * 1024)]);
end;

function TSimbaPackageEndpoint.GetPage(URL: String): String;
var
  HTTPRequest: TSimbaHTTPRequest;
begin
  DebugLn('[TSimbaPackageEndpoint.GetPage]: ', URL);

  Result := '';

  HTTPRequest := TSimbaHTTPRequest.Create(URL, @UpdateDownloadProgress);
  try
    while HTTPRequest.Running do
    begin
      if (FOnStatus <> nil) then
        FOnStatus(FProgress);

      if (ThreadID = MainThreadID) then
        Application.ProcessMessages();

      Sleep(50);
    end;

    if (FOnStatus <> nil) then
      FOnStatus('');

    if (HTTPRequest.Exception <> nil) then
      raise Exception.CreateFmt('Request failed: Exception %s', [HTTPRequest.Exception.Message]);
    if (not HTTPRequest.IsResponseCode([HTTP_OK, HTTP_FORBIDDEN, HTTP_NOT_FOUND, HTTP_BAD_REQUEST])) then
      raise Exception.CreateFmt('Request failed: HTTP error %d', [HTTPRequest.ResponseCode]);

    if (HTTPRequest.ResponseCode = HTTP_OK) then
      Result := HTTPRequest.Contents;
  except
    on E: Exception do
    begin
      DebugLn('[TSimbaPackageEndpoint.GetPage]: ', E.Message);

      if (FOnError <> nil) then
        FOnError(E.Message);
    end;
  end;

  HTTPRequest.Free();
end;

constructor TSimbaPackageEndpoint.Create(URL: String);
begin
  inherited Create();

  FURL := URL;
end;

function TSimbaPackageEndpoint.GetReleasesJSON: String;
begin
  Result := GetPage(FURL);
end;

function TSimbaPackageEndpoint.GetZip(URL, Destination: String; Flat: Boolean; IgnoreList: TStringArray): Boolean;
var
  HTTPRequest: TSimbaHTTPRequestZIP;
begin
  DebugLn('[TSimbaPackageEndpoint.GetZip]: ', URL, ' to ', Destination);

  Result := False;

  HTTPRequest := TSimbaHTTPRequestZIP.Create(URL, Destination, Flat, IgnoreList, @UpdateDownloadProgress, @UpdateExtractProgress);
  try
    while HTTPRequest.Running do
    begin
      if (FOnStatus <> nil) then
        FOnStatus(FProgress);

      if (ThreadID = MainThreadID) then
        Application.ProcessMessages();

      Sleep(50);
    end;

    if (OnStatus <> nil) then
      FOnStatus('');

    if (HTTPRequest.Exception <> nil) then
    begin
      if HTTPRequest.Exception is EFCreateError then
        raise Exception.CreateFmt('Install failed: %s. A file could be in use. Restart Simba(s) and try again.', [HTTPRequest.Exception.Message])
      else
        raise Exception.CreateFmt('Install failed: %s', [HTTPRequest.Exception.Message]);
    end;

    Result := HTTPRequest.ResponseCode = HTTP_OK;
    if (not Result) then
      raise Exception.CreateFmt('Install failed: HTTP error %d', [HTTPRequest.ResponseCode]);
  except
    on E: Exception do
    begin
      DebugLn('[TSimbaPackageEndpoint.GetPage]: ', E.Message);

      if (FOnError <> nil) then
        FOnError(E.Message);
    end;
  end;

  HTTPRequest.Free();
end;

constructor TSimbaPackageEndpoint_Github.Create(URL: String);
var
  URLPath: TStringArray;
begin
  inherited Create(URL);

  URLPath := URL.Split('/');
  while (Length(URLPath) > 2) do
    URLPath := Copy(URLPath, 1);

  if (Length(URLPath) <> 2) then
  begin
    DebugLn('TSimbaPackageEndpoint_Github.Create: URL is invalid');
    Exit;
  end;

  FOwner := URLPath[0];
  FName  := URLPath[1];
end;

function TSimbaPackageEndpoint_Github.GetReleasesJSON: String;
var
  Releases: TJSONArray;

  procedure ParseReleases(const Data: String);
  var
    JSON: TJSONData;
    I: Integer;
    Notes, Time, Version: TJSONData;
  begin
    JSON := nil;

    try
      JSON := GetJSON(Data);

      if (JSON is TJSONArray) then
      begin
        for I := 0 to JSON.Count - 1 do
          if (JSON.Items[I] is TJSONObject) then
          begin
            with TJSONObject(JSON.Items[I]) do
              if Find('body', Notes) and Find('published_at', Time) and Find('tag_name', Version) then
              begin
                Releases.Add(TJSONObject.Create(
                  ['zip',     Format(URL_DOWNLOAD_RELEASE, [FOwner, FName, Version.AsString]),
                   'notes',   Notes.AsString,
                   'time',    Time.AsString,
                   'version', Version.AsString,
                   'options', Format(URL_OPTIONS, [FOwner, FName, Version.AsString])
                  ])
                );
             end;
          end;
      end;
    except
      on E: Exception do
        DebugLn('[TSimbaPackageEndpoint_Github.GetReleasesJSON]: ParseReleases Exception "%s"', [E.Message]);
    end;

    if (JSON <> nil) then
      JSON.Free();
  end;

  procedure ParseBranches(const Data: String);
  var
    JSON: TJSONData;
    I: Integer;
    Branch: TJSONData;
  begin
    JSON := nil;

    try
      JSON := GetJSON(Data);

      if (JSON is TJSONArray) then
      begin
        for I := 0 to JSON.Count - 1 do
          if (JSON.Items[I] is TJSONObject) then
          begin
           with TJSONObject(JSON.Items[I]) do
             if Find('name', Branch) then
             begin
               Releases.Add(TJSONObject.Create(
                 ['zip',     Format(URL_DOWNLOAD_BRANCH, [FOwner, FName, Branch.AsString]),
                  'branch',  Branch.AsString,
                  'options', Format(URL_OPTIONS, [FOwner, FName, Branch.AsString])
                 ])
               );
             end;
          end;
      end;
    except
      on E: Exception do
        DebugLn('[TSimbaPackageEndpoint_Github.GetReleasesJSON]: ParseBranches Exception "%s"', [E.Message]);
    end;

    if (JSON <> nil) then
      JSON.Free();
  end;

begin
  Releases := TJSONArray.Create();

  ParseReleases(GetPage(Format(URL_RELEASES, [FOwner, FName])));
  ParseBranches(GetPage(Format(URL_BRANCHES, [FOwner, FName])));

  Result := Releases.FormatJSON();

  Releases.Free();
end;

end.

