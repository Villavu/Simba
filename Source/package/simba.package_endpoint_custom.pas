unit simba.package_endpoint_custom;

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
  simba.package;

type
  TSimbaPackageEndpoint_Custom = class(TSimbaPackageEndpoint)
  protected
    FURL: String;

    function GetPage(URL: String): String;
  public
    constructor Create(URL: String); override;

    function GetInfo: TSimbaPackageInfo; override;
    function GetVersions: TSimbaPackageVersionArray; override;
  end;

implementation

uses
  jsonparser, jsonscanner, fpjson, dateutils, lazloggerbase,
  simba.httpclient, simba.mufasatypes;

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

function TSimbaPackageEndpoint_Custom.GetPage(URL: String): String;
begin
  Result := '';

  try
    with TSimbaHTTPClient.Create() do
    try
      Result := Get(URL, [HTTP_OK]);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn(URL, ' :: ', E.ToString());
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

  Json := ParseJSON(GetPage(FURL));
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

function TSimbaPackageEndpoint_Custom.GetVersions: TSimbaPackageVersionArray;
var
  Json: TJsonData;
  I: Integer;
  Version: TSimbaPackageVersion;
begin
  Result := [];

  Json := ParseJSON(GetPage(FURL + 'versions'));
  if (Json = nil) then
    Exit;

  for I := 0 to JSON.Count - 1 do
    if (JSON.Items[I] is TJSONObject) then
    begin
      with TJSONObject(JSON.Items[I]) do
      try
        Version := TSimbaPackageVersion.Create(
          Strings['name'],
          Strings['notes'],
          Strings['download_url'],
          Strings['options_url'],
          Strings['time']
        );

        Result := Result + [Version];
      except
        FreeAndNil(Version);
      end;
    end;

  JSON.Free();
end;

end.

