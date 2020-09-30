unit simba.package_github_json;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, fpjson;

type
  TJSONObject_Helper = class helper for TJSONObject
  protected
    function GetDownloadURL: String;
    function GetNotes: String;
    function GetTime: TDateTime;
    function GetVersion: String;

    procedure SetDownloadURL(Value: String);
    procedure SetNotes(Value: String);
    procedure SetTime(Value: TDateTime);
    procedure SetVersion(Value: String);
  public
    property Version: String read GetVersion write SetVersion;
    property Notes: String read GetNotes write SetNotes;
    property DownloadURL: String read GetDownloadURL write SetDownloadURL;
    property Time: TDateTime read GetTime write SetTime;
  end;

  function LoadJSON(Path: String): TJSONArray;
  function ParseJSON(Contents: String): TJSONArray;

implementation

uses
  dateutils, jsonparser,
  simba.package_form;

function LoadJSON(Path: String): TJSONArray;
var
  JSON: TJSONData;
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);

    try
      JSON := GetJSON(Stream);

      if (JSON <> nil) then
      begin
        if (JSON is TJSONArray) and (JSON.Count > 0) then
          Exit(JSON as TJSONArray);

        JSON.Free();
      end;
    finally
      Stream.Free();
    end;
  except
    // File in use or bad json.
  end;

  Exit(nil);
end;

function ParseJSON(Contents: String): TJSONArray;
var
  JSON: TJSONData;
begin
  Result := nil;

  try
    JSON := GetJSON(Contents);
  except
    JSON := nil;
  end;

  if (JSON is TJSONArray) then
    Result := TJSONArray(JSON)
  else
    JSON.Free();
end;

function TJSONObject_Helper.GetDownloadURL: String;
begin
  Result := '';

  try
    Result := Strings['zipball_url'];
  except
  end;
end;

function TJSONObject_Helper.GetNotes: String;
begin
  Result := '';

  try
    Result := Strings['body'];
  except
  end;
end;

function TJSONObject_Helper.GetTime: TDateTime;
var
  Str: String;
begin
  Result := 0;

  Str := Strings['published_at'];
  Str := Str.Replace('T', '');
  Str := Str.Replace('Z', '');

  if (Str <> '') then
  try
    Result := ScanDateTime('yyyy-mm-ddhh:nn:ss', Str);
  except
  end;
end;

function TJSONObject_Helper.GetVersion: String;
begin
  Result := '';

  try
    Result := Strings['tag_name'];
  except
  end
end;

procedure TJSONObject_Helper.SetDownloadURL(Value: String);
begin
  Strings['zipball_url'] := Value;
end;

procedure TJSONObject_Helper.SetNotes(Value: String);
begin
  Strings['body'] := Value;
end;

procedure TJSONObject_Helper.SetTime(Value: TDateTime);
begin
  Strings['published_at'] := ''; // Not implemented.
end;

procedure TJSONObject_Helper.SetVersion(Value: String);
begin
  Strings['tag_name'] := Value;
end;

end.
