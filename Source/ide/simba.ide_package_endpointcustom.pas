unit simba.ide_package_endpointcustom;

(*
  ollydev.mooo.com/mypackage

  JSON Object:

  {
    "name" : "",
    "full_name" : "",
    "description" : "",
    "homepage_url" : "",
  }

*)

(*
  ollydev.mooo.com/mypackageversions

  JSON Array:

  [
    {
      "download_url" : "",
      "options_url" : "",
      "notes" : "",
      "time" : "",
      "name" : ""
    },
    {
      "download_url" : "",
      "options_url" : "",
      "notes" : "",
      "time" : "",
      "name" : ""
    }
  ]

*)

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.ide_package;

type
  TSimbaPackageEndpoint_Custom = class(TSimbaPackageEndpoint)
  protected
    FURL: String;

    function GetPage(URL: String): String;
  public
    constructor Create(URL: String); override;

    function GetInfo: TSimbaPackageInfo; override;
    function GetReleases: TSimbaPackageReleaseArray; override;
    function GetBranches: TSimbaPackageBranchArray; override;
  end;

implementation

uses
  dateutils, fpjson,
  simba.httpclient, simba.base, simba.vartype_string;

function TSimbaPackageEndpoint_Custom.GetPage(URL: String): String;
begin
  Result := '';

  try
    with TSimbaHTTPClient.Create() do
    try
      Result := Get(URL, [EHTTPStatus.OK]);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn(URL + ' :: ' + E.ToString());
  end;
end;

constructor TSimbaPackageEndpoint_Custom.Create(URL: String);
begin
  FURL := URL;
  if FURL.EndsWith('/') then
    SetLength(FURL, Length(FURL) - 1);
end;

function TSimbaPackageEndpoint_Custom.GetInfo: TSimbaPackageInfo;
var
  Json: TJSONData;
begin
  Result := Default(TSimbaPackageInfo);

  Json := GetPage(FURL).ParseJSON();
  if (Json = nil) then
    Exit;

  if (Json is TJSONObject) then
  begin
    Result.Name := TJSONObject(Json).Get('name', '');
    Result.FullName := TJSONObject(Json).Get('full_name', '');
    Result.Description := TJSONObject(Json).Get('description', '');
    Result.HomepageURL := TJSONObject(Json).Get('homepage_url', '');
  end;

  JSON.Free();
end;

function TSimbaPackageEndpoint_Custom.GetReleases: TSimbaPackageReleaseArray;
var
  JSON: TJSONData;
  I: Integer;
  Release: TSimbaPackageRelease;
begin
  Result := [];

  JSON := GetPage(FURL + 'versions').ParseJSON();
  if (JSON = nil) then
    Exit;

  for I := 0 to JSON.Count - 1 do
    if (JSON.Items[I] is TJSONObject) then
      with TJSONObject(JSON.Items[I]) do
      begin
        Release := Default(TSimbaPackageRelease);
        Release.Name := Strings['name'];
        Release.Notes := Strings['notes'];
        Release.DownloadURL := Strings['download_url'];
        Release.OptionsURL := Strings['options_url'];
        Release.Age := Strings['time'];

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

function TSimbaPackageEndpoint_Custom.GetBranches: TSimbaPackageBranchArray;
var
  JSON: TJSONData;
  I: Integer;
  Branch: TSimbaPackageBranch;
begin
  Result := [];

  JSON := GetPage(FURL + 'branches').ParseJSON();
  if (JSON = nil) then
    Exit;

  for I := 0 to JSON.Count - 1 do
    if (JSON.Items[I] is TJSONObject) then
      with TJSONObject(JSON.Items[I]) do
      begin
        Branch := Default(TSimbaPackageBranch);
        Branch.Name := Strings['name'];
        Branch.DownloadURL := Strings['download_url'];

        Result := Result + [Branch];
      end;

  JSON.Free();
end;

end.

