unit simba.package_github_releases;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, fpjson;

type
  TSimbaPackage_GithubRelease = record
    Version: String;
    Notes: String;
    Time: TDateTime;
    DownloadURL: String;
  end;

  TSimbaPackage_GithubReleases = class
  protected
    FPackage: TObject;
    FURL: String;
    FUseCache: Boolean;
    FCacheFile: String;

    function GetReleasesJSON: TJSONArray;
    function GetOptions(Version: String): String;
    function GetRelease(Version: String): TSimbaPackage_GithubRelease;
    function GetLatestRelease: TSimbaPackage_GithubRelease;
    function GetVersions: TStringArray;
    function GetCacheAge: TDateTime;
  public
    property Release[Version: String]: TSimbaPackage_GithubRelease read GetRelease; default;
    property Options[Version: String]: String read GetOptions;
    property Versions: TStringArray read GetVersions;
    property CacheAge: TDateTime read GetCacheAge;
    property LatestRelease: TSimbaPackage_GithubRelease read GetLatestRelease;
    property UseCache: Boolean read FUseCache write FUseCache;

    constructor Create(Package: TObject);
  end;

implementation

uses
  dateutils,
  simba.httpclient, simba.settings, simba.package, simba.package_github_json, simba.package_github_url;

function TSimbaPackage_GithubReleases.GetRelease(Version: String): TSimbaPackage_GithubRelease;
var
  i: Int32;
  JSON: TJSONArray;
begin
  Result := Default(TSimbaPackage_GithubRelease);

  JSON := GetReleasesJSON();

  if (JSON <> nil) then
  try
    for i := 0 to JSON.Count - 1 do
      if (JSON[i] is TJSONObject) and (TJSONObject(JSON[i]).Version = Version) then
      begin
        Result.Version := TJSONObject(JSON[i]).Version;
        Result.Time := TJSONObject(JSON[i]).Time;
        Result.Notes := TJSONObject(JSON[i]).Notes;
        Result.DownloadURL := TJSONObject(JSON[i]).DownloadURL;
      end;
  finally
    JSON.Free();
  end;
end;

function TSimbaPackage_GithubReleases.GetVersions: TStringArray;
var
  i: Int32;
  JSON: TJSONArray;
begin
  Result := Default(TStringArray);

  JSON := GetReleasesJSON();

  if (JSON <> nil) then
  try
    for i := 0 to JSON.Count - 1 do
      if (JSON[i] is TJSONObject) and (TJSONObject(JSON[i]).Version <> '') then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := TJSONObject(JSON[i]).Version;
      end;
  finally
    JSON.Free();
  end;
end;

function TSimbaPackage_GithubReleases.GetCacheAge: TDateTime;
begin
  if not FileAge(FCacheFile, Result) then
    Result := -1;
end;

function TSimbaPackage_GithubReleases.GetLatestRelease: TSimbaPackage_GithubRelease;
var
  JSON: TJSONArray;
begin
  Result := Default(TSimbaPackage_GithubRelease);

  JSON := GetReleasesJSON();

  if (JSON <> nil) then
  try
    if (JSON.Count > 0) and (JSON[0] is TJSONObject) then
    begin
      Result.Version := TJSONObject(JSON[0]).Version;
      Result.Time := TJSONObject(JSON[0]).Time;
      Result.Notes := TJSONObject(JSON[0]).Notes;
      Result.DownloadURL := TJSONObject(JSON[0]).DownloadURL;
    end;
  finally
    JSON.Free();
  end;
end;

function TSimbaPackage_GithubReleases.GetOptions(Version: String): String;
begin
  with FPackage as TSimbaPackage do
    Result := GetPage(Format(GITHUB_URL_PACKAGE_OPTIONS, [Owner, Name, Version]), [HTTP_OK, HTTP_NOT_FOUND]);
end;

function TSimbaPackage_GithubReleases.GetReleasesJSON: TJSONArray;
var
  Master: TJSONObject;
  Stream: TFileStream;
  Contents, JSON: String;
begin
  Result := nil;

  // If using cache and is less than 60 minutes old use it
  if FUseCache then
  begin
    if MinutesBetween(Now(), CacheAge) < 60 then
    begin
      Result := LoadJSON(FCacheFile);
      if (Result <> nil) then
        Exit;
    end;
  end;

  // Download new releases
  with FPackage as TSimbaPackage do
  begin
    Contents := GetPage(ReleasesURL, [HTTP_OK]);

    if (Contents <> '') then
    begin
      Result := ParseJSON(Contents);
      if (Result = nil) then
        Result := TJSONArray.Create();

      // Append master branch
      Master := TJSONObject.Create();
      with Master do
      begin
        DownloadURL := Format(GITHUB_URL_DOWNLOAD_MASTER, [Owner, Name]);
        Version := 'master';
        Time := 0;
        Notes := Format(GITHUB_URL_REPOSITORY, [Owner, Name, 'master']);
      end;

      Result.Add(Master);

      // Update cache
      try
        JSON := Result.FormatJSON();

        Stream := TFileStream.Create(FCacheFile, fmCreate or fmShareDenyWrite);
        Stream.Write(JSON[1], Length(JSON));
        Stream.Size := Length(JSON);
        Stream.Free();
      except
        // File being written
      end;
    end;
  end;
end;

constructor TSimbaPackage_GithubReleases.Create(Package: TObject);
begin
  FPackage := Package;
  FUseCache := True;
  with FPackage as TSimbaPackage do
    FCacheFile := SimbaSettings.Environment.PackagePath.Value + Owner + '-' + Name;
end;

end.

