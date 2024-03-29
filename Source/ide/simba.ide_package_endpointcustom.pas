{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_package_endpointcustom;

(*
  1) Base URL, this will be the link inserted by the user:
    www.something.com/mypackage

  2) Versions are queried by appending "_versions" to the base URL:
    www.something.com/mypackage_versions

    Which should contain a json array of versions:

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
  Classes, SysUtils,
  simba.base, simba.ide_package;

type
  TSimbaPackageEndpoint_Custom = class(TSimbaPackageEndpoint)
  protected
    FVersionsURL: String;

    function GetVersions: TSimbaPackageVersions; override;
  public
    constructor Create(URL: String); override;
  end;

implementation

uses
  fpjson,
  simba.vartype_string;

constructor TSimbaPackageEndpoint_Custom.Create(URL: String);
begin
  inherited Create(URL);

  FVersionsURL := URL + '_versions';
end;

function TSimbaPackageEndpoint_Custom.GetVersions: TSimbaPackageVersions;
var
  JSON: TJSONData;
  I: Integer;
  Ver: TSimbaPackageVersion;
begin
  Result := [];

  JSON := GetPage(FVersionsURL).ParseJSON();
  if (JSON = nil) then
    Exit;

  for I := 0 to JSON.Count - 1 do
    if (JSON.Items[I] is TJSONObject) then
      with TJSONObject(JSON.Items[I]) do
      try
        Ver := Default(TSimbaPackageVersion);
        Ver.Name := Strings['name'];
        Ver.Notes := Strings['notes'];
        Ver.DownloadURL := Strings['download_url'];
        Ver.OptionsURL := Strings['options_url'];
        Ver.Age := Strings['time'];

        if (Ver.Notes = '') then
          Ver.Notes := '(no version notes)';

        ParseTime(Strings['time'], Ver.Time, Ver.Age);

        Result := Result + [Ver];
      except
      end;

  JSON.Free();
end;

end.

