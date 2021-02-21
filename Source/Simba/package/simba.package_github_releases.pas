{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Parses Githubs releases JSON into a simple array.
}
unit simba.package_github_releases;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  TGithubRelease = record
    Version: String;
    Notes: String;
    Time: TDateTime;
    TimeStamp: String;
    DownloadURL: String;
  end;

  TGithubReleases = array of TGithubRelease;

  TGithubReleasesHelper = type helper for TGithubReleases
  public
    procedure Fill(Data: String); // Parse JSON array
    procedure Add(Version, Download, Notes, TimeStamp: String);
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
  end;

const
  NullRelease: TGithubRelease = (Version: ''; Notes: ''; Time: 0; TimeStamp: ''; DownloadURL: '');
  NullReleases: TGithubReleases = ();

implementation

uses
  fpjson, dateutils,
  simba.files;

procedure TGithubReleasesHelper.Add(Version, Download, Notes, TimeStamp: String);
var
  Release: TGithubRelease;
begin
  Release.TimeStamp := TimeStamp;
  Release.Time := ISO8601ToDateDef(TimeStamp, 0, False);
  Release.Notes := Notes;
  Release.Version := Version;
  Release.DownloadURL := Download;

  Self := Self + [Release];
end;

procedure TGithubReleasesHelper.SaveToFile(FileName: String);
var
  Release: TGithubRelease;
  JSON: TJSONArray;
begin
  JSON := TJSONArray.Create();

  try
    for Release in Self do
      JSON.Add(TJSONObject.Create(
        ['zipball_url',  Release.DownloadURL,
         'body',         Release.Notes,
         'published_at', Release.TimeStamp,
         'tag_name',     Release.Version])
      );

    WriteFile(FileName, JSON.FormatJSON());
  finally
    JSON.Free();
  end;
end;

procedure TGithubReleasesHelper.LoadFromFile(FileName: String);
begin
  Fill(ReadFile(FileName));
end;

procedure TGithubReleasesHelper.Fill(Data: String);
var
  JSON: TJSONData;
  Download, Notes, Time, Version: TJSONData;
  I: Int32;
begin
  Self := NullReleases;

  try
    JSON := GetJSON(Data);
  except
    JSON := nil;
  end;

  if (JSON is TJSONArray) then
  begin
    for I := 0 to JSON.Count - 1 do
      if (JSON.Items[I] is TJSONObject) then
      begin
        with TJSONObject(JSON.Items[I]) do
        begin
          Download := Find('zipball_url');
          Notes := Find('body');
          Time := Find('published_at');
          Version := Find('tag_name');
        end;

        if (Download = nil) or (Notes = nil) or (Time = nil) or (Version = nil) then
          Continue;

        Self.Add(Version.AsString, Download.AsString, Notes.AsString, Time.AsString);
      end;
  end;

  if (JSON <> nil) then
    JSON.Free();
end;

end.

