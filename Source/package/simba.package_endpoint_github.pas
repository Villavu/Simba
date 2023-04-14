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
    function GetReleases: TSimbaPackageReleaseArray; override;
    function GetBranches: TSimbaPackageBranchArray; override;
  end;

implementation

uses
  forms, fpjson, dateutils,
  simba.httpclient, simba.files, simba.mufasatypes;

const
  URL_REPOS            = 'https://api.github.com/repos/%s/%s';                       // {Owner} {Name}
  URL_RELEASES         = 'https://api.github.com/repos/%s/%s/releases?per_page=100'; // {Owner} {Name}
  URL_BRANCHES         = 'https://api.github.com/repos/%s/%s/branches?per_page=100'; // {Owner} {Name}
  URL_DOWNLOAD_BRANCH  = 'https://github.com/%s/%s/archive/refs/heads/%s.zip';       // {Owner} {Name} {Version}
  URL_DOWNLOAD_RELEASE = 'https://github.com/%s/%s/archive/refs/tags/%s.zip';        // {Owner} {Name} {Version}
  URL_OPTIONS          = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

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

  Json := GetPageCache('info', FInfoAPI).ParseJSON();
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

function TSimbaPackageEndpoint_Github.GetReleases: TSimbaPackageReleaseArray;
var
  JSON: TJSONData;
  I: Integer;
  Release: TSimbaPackageRelease;
begin
  Result := [];

  JSON := GetPageCache('releases', FReleasesAPI).ParseJSON();
  if (JSON = nil) then
    Exit;

  for I := 0 to JSON.Count - 1 do
    if (JSON.Items[I] is TJSONObject) then
      with TJSONObject(JSON.Items[I]) do
      begin
        Release := Default(TSimbaPackageRelease);
        Release.Age := Strings['published_at'];
        Release.Name := Strings['tag_name'];
        Release.Notes := Strings['body'];
        Release.DownloadURL := URL_DOWNLOAD_RELEASE.Format([FOwner, FName, Strings['tag_name']]);
        Release.OptionsURL := URL_OPTIONS.Format([FOwner, FName, Strings['tag_name']]);

        if (Release.Notes = '') then
          Release.Notes := '(no release notes)';

        if (Release.Age <> '') then
        begin
          if Release.Age.IsInteger() then
            Release.Time := UnixToDateTime(Release.Age.ToInt64())
          else
            Release.Time := ISO8601ToDate(Release.Age);

          case DaysBetween(Now(), Time) of
            0: Release.Age := '(today)';
            1: Release.Age := '(yesterday)';
            else
              Release.Age := '(' + IntToStr(DaysBetween(Now(), Release.Time)) + ' days ago)';
          end;
        end;

        Result := Result + [Release];
      end;

  JSON.Free();
end;

function TSimbaPackageEndpoint_Github.GetBranches: TSimbaPackageBranchArray;
var
  JSON: TJSONData;
  I: Integer;
  Branch: TSimbaPackageBranch;
begin
  Result := [];

  JSON := GetPageCache('branches', FBranchesAPI).ParseJSON();
  if (JSON = nil) then
    Exit;

  for I := 0 to JSON.Count - 1 do
    if (JSON.Items[I] is TJSONObject) then
      with TJSONObject(JSON.Items[I]) do
      begin
        Branch := Default(TSimbaPackageBranch);
        Branch.Name := Strings['name'];
        Branch.DownloadURL := URL_DOWNLOAD_BRANCH.Format([FOwner, FName, Strings['name']]);

        Result := Result + [Branch];
      end;

  JSON.Free();
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

          Result := Head(URL) = EHTTPStatus.NOT_MODIFIED;
        finally
          Free();
        end;
      end;
    end;
  end;

  procedure Download;
  var
    JsonData: TJSONData;
  begin
    with TSimbaHTTPClient.Create() do
    try
      Cache.Text := Get(URL, [EHTTPStatus.OK]);

      ETag := ResponseHeader['ETag'];
    finally
      Free();
    end;

    JsonData := Cache.Text.ParseJSON();
    if (JsonData <> nil) then
    try
      Cache.Text := JsonData.FormatJSON();
      Cache.Insert(0, '//' + ETag);
      Cache.SaveToFile(CacheFileName);
    finally
      JsonData.Free();
    end;
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
      DebugLn(URL + ' :: ' + E.ToString());
  end;

  Cache.Free();
end;

end.

