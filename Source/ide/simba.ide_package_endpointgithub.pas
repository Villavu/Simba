{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  List releases from Github using RSS feed to detect changes.

  Unauthed Github conditional requests were counting towards the rate limit
  so RSS updated timestamp is used.
}
unit simba.ide_package_endpointgithub;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.ide_package;

type
  TSimbaPackageEndpoint_Github = class(TSimbaPackageEndpoint)
  protected
    FOwner: String;
    FName: String;
    FBranchesAPI: String;
    FReleasesAPI: String;
    FReleasesRSS: String;

    FReleasesCache: String;
    FBranchesCache: String;

    procedure DebugRateLimit;

    function ReadRSSUpdated(URL: String): String;

    function ParseBranchesAPI(Data: String): TSimbaPackageVersions;
    function ParseReleasesAPI(Data: String): TSimbaPackageVersions;

    function GetReleases: TSimbaPackageVersions;
    function GetBranches: TSimbaPackageVersions; // only loads cache, requires DownloadBraches to be used

    function GetVersions: TSimbaPackageVersions; override;
  public
    constructor Create(URL: String); override;

    procedure DeleteCache; override;
    procedure DownloadBranches;
  end;

implementation

uses
  XMLRead, DOM, fpjson,
  simba.env, simba.vartype_string, simba.httpclient;

const
  URL_RELEASES_RSS = 'https://www.github.com/%s/%s/releases.atom'; // {Owner} {Name}

  URL_BRANCHES_API = 'https://api.github.com/repos/%s/%s/branches?per_page=100'; // {Owner} {Name}
  URL_RELEASES_API = 'https://api.github.com/repos/%s/%s/releases?per_page=100'; // {Owner} {Name}
  URL_OPTIONS      = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

  URL_DOWNLOAD_RELEASE = 'https://github.com/%s/%s/archive/refs/tags/%s.zip';  // {Owner} {Name} {Version}
  URL_DOWNLOAD_BRANCH  = 'https://github.com/%s/%s/archive/refs/heads/%s.zip'; // {Owner} {Name} {Branch}

function TSimbaPackageEndpoint_Github.ReadRSSUpdated(URL: String): String;
var
  Data: String;
  Doc: TXMLDocument;
  Node: TDOMNode;
  Stream: TStringStream;
begin
  Result := '';

  Stream := nil;
  Doc := nil;

  Data := GetPage(URL);
  if (Data <> '') then
  try
    Stream := TStringStream.Create(Data);

    ReadXMLFile(Doc, Stream);
    if Assigned(Doc.DocumentElement.FindNode('entry')) then
    begin
      Node := Doc.DocumentElement.FindNode('updated');
      if Assigned(Node) then
        Result := Node.TextContent;
    end;
  finally
    Stream.Free();
    Doc.Free();
  end;
end;

constructor TSimbaPackageEndpoint_Github.Create(URL: String);
var
  Path: TStringArray;
begin
  inherited Create(URL);

  Path := URL.Split('/');
  if (Length(Path) < 2) then // Should never happen
    Exit;

  FOwner := Path[High(Path) - 1];
  FName  := Path[High(Path)];

  FBranchesAPI := Format(URL_BRANCHES_API, [FOwner, FName]);
  FReleasesAPI := Format(URL_RELEASES_API, [FOwner, FName]);
  FReleasesRSS := Format(URL_RELEASES_RSS, [FOwner, FName]);

  FReleasesCache := SimbaEnv.PackagesPath + FOwner + '-' + FName + '.releases';
  FBranchesCache := SimbaEnv.PackagesPath + FOwner + '-' + FName + '.branches';
end;

procedure TSimbaPackageEndpoint_Github.DeleteCache;
begin
  if FileExists(FReleasesCache) then DeleteFile(FReleasesCache);
  if FileExists(FBranchesCache) then DeleteFile(FBranchesCache);
end;

function TSimbaPackageEndpoint_Github.ParseBranchesAPI(Data: String): TSimbaPackageVersions;
var
  Json: TJSONData;
  Ver: TSimbaPackageVersion;
  I: Integer;
begin
  Result := [];

  Json := Data.ParseJSON();
  if (Json is TJSONArray) then
    for I := 0 to Json.Count - 1 do
      if (Json.Items[I] is TJSONObject) then
      try
        with TJSONObject(Json.Items[I]) do
        begin
          Ver := Default(TSimbaPackageVersion);
          Ver.IsBranch := True;
          Ver.Name := Strings['name'];
          Ver.DownloadURL := Format(URL_DOWNLOAD_BRANCH, [FOwner, FName, Strings['name']]);
          Ver.Age := '(branch)';

          Result += [Ver];
        end;
      except
      end;
  Json.Free();
end;

function TSimbaPackageEndpoint_Github.ParseReleasesAPI(Data: String): TSimbaPackageVersions;
var
  Json: TJSONData;
  Ver: TSimbaPackageVersion;
  I: Integer;
begin
  Result := [];

  Json := Data.ParseJSON();
  if (Json is TJSONArray) then
    for I := 0 to JSON.Count - 1 do
      if (JSON.Items[I] is TJSONObject) then
      try
        with TJSONObject(JSON.Items[I]) do
        begin
          Ver := Default(TSimbaPackageVersion);
          Ver.Name := Strings['tag_name'];
          Ver.Notes := Strings['body'];
          Ver.DownloadURL := Format(URL_DOWNLOAD_RELEASE, [FOwner, FName, Strings['tag_name']]);
          Ver.OptionsURL := Format(URL_OPTIONS, [FOwner, FName, Strings['tag_name']]);
          if (Ver.Notes = '') then
            Ver.Notes := '(no version notes)';

          ParseTime(Strings['published_at'], Ver.Time, Ver.Age);

          Result := Result + [Ver];
        end;
      except
      end;
  JSON.Free();
end;

function TSimbaPackageEndpoint_Github.GetReleases: TSimbaPackageVersions;
var
  Cache: TStringList;
  RemoteVersion: String;

  function IsCacheUpdated: Boolean;
  begin
    Result := (Cache.Count > 0) and (Cache[0].After('//') = RemoteVersion);
  end;

  function Download: String;
  var
    Data: String;
    JsonData: TJSONData;
  begin
    Result := '';

    Data := GetPage(FReleasesAPI);
    if (Data <> '') then
    begin
      JsonData := Data.ParseJSON();
      if (JsonData <> nil) then
        Result := JsonData.FormatJSON();
      JsonData.Free();
    end;
  end;

begin
  Cache := TStringList.Create();
  if FileExists(FReleasesCache) then
    Cache.LoadFromFile(FReleasesCache);

  RemoteVersion := ReadRSSUpdated(FReleasesRSS);

  if IsCacheUpdated() then
    Result := ParseReleasesAPI(Cache.Text)
  else
  begin
    Cache.Clear();
    Cache.Add('//' + RemoteVersion);
    Cache.Add(Download());
    Cache.SaveToFile(FReleasesCache);

    Result := ParseReleasesAPI(Cache.Text);

    {$IFDEF DebugRateLimit}
    DebugRateLimit();
    {$ENDIF}
  end;

  Cache.Free();
end;

function TSimbaPackageEndpoint_Github.GetBranches: TSimbaPackageVersions;
var
  Cache: TStringList;
begin
  Result := [];

  if FileExists(FBranchesCache) then
  begin
    Cache := TStringList.Create();
    Cache.LoadFromFile(FBranchesCache);

    Result := ParseBranchesAPI(Cache.Text);

    Cache.Free();
  end;
end;

procedure TSimbaPackageEndpoint_Github.DebugRateLimit;
begin
  with TSimbaHTTPClient.Create() do
  try
    Get('https://api.github.com/rate_limit');

    DebugLn('Github rate limit remaining: %s', [ResponseHeader['X-RateLimit-Remaining']]);
  finally
    Free();
  end;
end;

function TSimbaPackageEndpoint_Github.GetVersions: TSimbaPackageVersions;
begin
  Result := GetReleases() + GetBranches();
end;

procedure TSimbaPackageEndpoint_Github.DownloadBranches;

  function Download: String;
  var
    Data: String;
    JsonData: TJSONData;
  begin
    Result := '';

    try
      Data := URLFetch(FBranchesAPI);

      JsonData := Data.ParseJSON();
      if (JsonData <> nil) then
        Result := JsonData.FormatJSON();
      JsonData.Free();
    except
    end;
  end;

var
  Cache: TStringList;
begin
  Cache := TStringList.Create();
  Cache.Add(Download());
  Cache.SaveToFile(FBranchesCache);
  Cache.Free();

  {$IFDEF DebugRateLimit}
  DebugRateLimit();
  {$ENDIF}
end;

end.

