unit simba.package_endpoint_github;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.package;

type
  TSimbaPackageEndpoint_Github = class(TSimbaPackageEndpoint)
  protected
    FOwner: String;
    FName: String;

    FReleasesAPI: String;
    FBranchesAPI: String;
    FInfoAPI: String;

    function GetPageCache(Key: String; URL: String): String;
  public
    constructor Create(URL: String); override;

    function GetInfo: TSimbaPackageInfo; override;
    function GetVersions: TSimbaPackageVersionArray; override;
  end;

implementation

uses
  forms, fpjson, jsonparser, jsonscanner, lazloggerbase, dateutils,
  simba.httpclient, simba.files, simba.mufasatypes;

const
  URL_REPOS            = 'https://api.github.com/repos/%s/%s';                       // {Owner} {Name}
  URL_RELEASES         = 'https://api.github.com/repos/%s/%s/releases?per_page=100'; // {Owner} {Name}
  URL_BRANCHES         = 'https://api.github.com/repos/%s/%s/branches?per_page=100'; // {Owner} {Name}
  URL_DOWNLOAD_BRANCH  = 'https://github.com/%s/%s/archive/refs/heads/%s.zip';       // {Owner} {Name} {Version}
  URL_DOWNLOAD_RELEASE = 'https://github.com/%s/%s/archive/refs/tags/%s.zip';        // {Owner} {Name} {Version}
  URL_OPTIONS          = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

function ParseJSON(const S: String): TJSONData;
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(S, [joComments]);
  try
    Result := Parser.Parse();
  except
    Result := nil;
  end;

  Parser.Free();
end;

constructor TSimbaPackageEndpoint_Github.Create(URL: String);
var
  Path: TStringArray;
begin
  Path := URL.Split('/');
  if (Length(Path) < 2) then // Should never happen
    Exit;

  FOwner := Path[High(Path) - 1];
  FName  := Path[High(Path)];

  FReleasesAPI := Format(URL_RELEASES, [FOwner, FName]);
  FBranchesAPI := Format(URL_BRANCHES, [FOwner, FName]);
  FInfoAPI     := Format(URL_REPOS,    [FOwner, FName]);
end;

function TSimbaPackageEndpoint_Github.GetInfo: TSimbaPackageInfo;
var
  Json: TJSONData;
begin
  Result := Default(TSimbaPackageInfo);

  Json := ParseJSON(GetPageCache('info', FInfoAPI));
  if (Json = nil) then
    Exit;

  if (Json is TJSONObject) then
  begin
    Result.Name := TJSONObject(Json).Get('name', '');
    Result.FullName := TJSONObject(Json).Get('full_name', '');
    Result.Description := TJSONObject(Json).Get('description', '');
    Result.HomepageURL := TJSONObject(Json).Get('html_url', '');
  end;

  JSON.Free();
end;

function TSimbaPackageEndpoint_Github.GetVersions: TSimbaPackageVersionArray;
var
  Json: TJsonData;
  I: Integer;
  Version: TSimbaPackageVersion;
  DownloadURL, OptionsURL: String;
begin
  Result := [];

  Json := ParseJSON(GetPageCache('releases', FReleasesAPI));
  if (Json <> nil) then
  begin
    for I := 0 to JSON.Count - 1 do
      if (JSON.Items[I] is TJSONObject) then
      begin
        with TJSONObject(JSON.Items[I]) do
        try
          DownloadURL := URL_DOWNLOAD_RELEASE.Format([FOwner, FName, Strings['tag_name']]);
          OptionsURL  := URL_OPTIONS.Format([FOwner, FName, Strings['tag_name']]);

          Version := TSimbaPackageVersion.Create(
            Strings['tag_name'] ,
            Strings['body'],
            DownloadURL,
            OptionsURL,
            Strings['published_at']
          );

          Result := Result + [Version];
        except
          FreeAndNil(Version);
        end;
      end;

    JSON.Free();
  end;

  Json := ParseJSON(GetPageCache('branches', FBranchesAPI));
  if (Json <> nil) then
  begin
    for I := 0 to JSON.Count - 1 do
      if (JSON.Items[I] is TJSONObject) then
      begin
        with TJSONObject(JSON.Items[I]) do
        try
          DownloadURL := URL_DOWNLOAD_BRANCH.Format([FOwner, FName, Strings['name']]);
          OptionsURL  := URL_OPTIONS.Format([FOwner, FName, Strings['name']]);

          Version := TSimbaPackageBranch.Create(
            Strings['name'],
            '',
            DownloadURL,
            OptionsURL,
            ''
          );

          Result := Result + [Version];
        except
          FreeAndNil(Version);
        end;
      end;

    JSON.Free();
  end;
end;

function TSimbaPackageEndpoint_Github.GetPageCache(Key: String; URL: String): String;
var
  ETag: String;
  CacheFileName: String;
  Cache: TStringList;

  function CacheUpToDate: Boolean;
  begin
    Result := False;

    if FileExists(CacheFileName) then
      Cache.LoadFromFile(CacheFileName);

    if (Cache.Count > 0) then
    begin
      ETag := Cache[0].After('//');

      if (ETag <> '') then
      begin
        with TSimbaHTTPClient.Create() do
        try
          RequestHeader['If-None-Match'] := ETag; // ETag is added as a comment on the first line

          Result := Head(URL) = HTTP_NOT_MODIFIED;
        finally
          Free();
        end;
      end;
    end;
  end;

  procedure Download;
  var
    JsonParser: TJSONParser;
    JsonData: TJSONData;
  begin
    with TSimbaHTTPClient.Create() do
    try
      Cache.Text := Get(URL, [HTTP_OK]);

      ETag := ResponseHeader['ETag'];
    finally
      Free();
    end;

    JsonParser := TJSONParser.Create(Cache.Text, [joComments]);
    try
      JsonData := JsonParser.Parse();

      if (JsonData <> nil) then
      begin
        Cache.Text := JsonData.FormatJSON();
        Cache.Insert(0, '//' + ETag);
        Cache.SaveToFile(CacheFileName);
      end;
    except
    end;

    if (JsonData <> nil) then
      JsonData.Free();
    JsonParser.Free();
  end;

begin
  Result := '';

  CacheFileName := GetPackagePath() + FOwner + '-' + FName + '.' + Key;
  Cache := TStringList.Create();

  try
    if (not CacheUpToDate()) then
      Download();

    Result := Cache.Text;
  except
    on E: Exception do
      DebugLn(URL, ' :: ', E.ToString());
  end;

  Cache.Free();
end;

end.

