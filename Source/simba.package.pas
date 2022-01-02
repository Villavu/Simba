{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package;

{$i simba.inc}

interface

uses
  classes, sysutils, fgl;

const
  GITHUB_API_BRANCHES           = 'https://api.github.com/repos/%s/%s/branches';              // {Owner} {Name}
  GITHUB_API_RELEASES           = 'https://api.github.com/repos/%s/%s/releases';              // {Owner} {Name}
  GITHUB_URL_REPOSITORY         = 'https://github.com/%s/%s';                                 // {Owner} {Name}
  GITHUB_URL_REPOSITORY_ISSUES  = 'https://github.com/%s/%s/issues';                          // {Owner} {Name}
  GITHUB_URL_DOWNLOAD_BRANCH    = 'https://github.com/%s/%s/archive/%s.zip';                  // {Owner} {Name} {Branch}
  GITHUB_URL_PACKAGE_OPTIONS    = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

type
  TSimbaPackage_Options = record
    Loaded: Boolean;
    Name: String;
    Directory: String;
    Flat: Boolean;
    IgnoreList: String;
  end;

  TSimbaPackage_InstalledVersion = record
    Name: String;
    Path: String;
    Version: String;
    VersionTime: TDateTime;
    Scripts: String;
    Examples: String;
  end;

  TSimbaPackageRelease = record
    Version: String;
    Notes: String;
    Time: TDateTime;
    TimeStamp: String;
    DownloadURL: String;

    function IsEmpty: Boolean;
    class function Empty: TSimbaPackageRelease; static;

    class operator = (const Left, Right: TSimbaPackageRelease): Boolean;
    class operator <> (const Left, Right: TSimbaPackageRelease): Boolean;
  end;

  TSimbaPackageReleaseList = specialize TFPGList<TSimbaPackageRelease>;

  TSimbaPackageGetPage = function(URL: String; AllowedResponseCodes: array of Integer; out ResponseCode: Integer): String of object;
  TSimbaPackageGetZIP  = function(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray): Boolean of object;

  TSimbaPackage = class
  public
    FOwner: String;
    FName: String;
    FURL: String;
    FIssuesURL: String;
    FReleasesURL: String;
    FBranchesURL: String;
    FReleases: TSimbaPackageReleaseList;
    FCacheFile: String;
    FInstalledVersion: TSimbaPackage_InstalledVersion;

    function GetOptions(Version: String): TSimbaPackage_Options;
    function GetRelease(Version: String): TSimbaPackageRelease;
    function GetCacheAge: Integer;
  public
    GetPageFunction: TSimbaPackageGetPage;
    GetZIPFunction: TSimbaPackageGetZIP;

    property InstalledVersion: TSimbaPackage_InstalledVersion read FInstalledVersion;
    property Owner: String read FOwner;
    property Name: String read FName;
    property URL: String read FURL;
    property IssuesURL: String read FIssuesURL;
    property Releases: TSimbaPackageReleaseList read FReleases;
    property Release[Version: String]: TSimbaPackageRelease read GetRelease;
    property Options[Version: String]: TSimbaPackage_Options read GetOptions;

    property CacheAge: Integer read GetCacheAge;

    procedure AddRelease(Version, TimeStamp, Notes, DownloadURL: String);
    function LoadReleases(UseCache: Boolean = True): Boolean;

    function Install(Version: String; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
    procedure Remove;

    constructor Create(Repository: String);
    destructor Destroy; override;
  end;

  TSimbaPackageList = specialize TFPGObjectList<TSimbaPackage>;

  function LoadPackages: TSimbaPackageList;

implementation

uses
  fileutil, dateutils, lazloggerbase, inifiles, jsonparser, fpjson,
  simba.files, simba.httpclient;

type
  TSimbaPackageConfig = class
    class function LoadPackages: TSimbaPackageList; static;

    class procedure LoadPackage(Package: TSimbaPackage); static;
    class procedure SavePackage(Package: TSimbaPackage); static;
    class procedure RemovePackage(Package: TSimbaPackage); static;
  end;

class function TSimbaPackageConfig.LoadPackages: TSimbaPackageList;
var
  Sections: TStringList;
  Repository: String;
begin
  Result := TSimbaPackageList.Create();

  Sections := TStringList.Create();

  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      ReadSections(Sections);
      for Repository in Sections do
        Result.Add(TSimbaPackage.Create(Repository));
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('TSimbaPackageConfig.LoadPackages: ' + E.Message);
  end;

  if (Sections <> nil) then
    Sections.Free();
end;

class procedure TSimbaPackageConfig.SavePackage(Package: TSimbaPackage);
begin
  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      CacheUpdates := True;

      WriteString(Package.Owner + '/' + Package.Name, 'Name', Package.FInstalledVersion.Name);
      WriteString(Package.Owner + '/' + Package.Name, 'Path', Package.FInstalledVersion.Path);
      WriteString(Package.Owner + '/' + Package.Name, 'InstalledVersion', Package.FInstalledVersion.Version);

      WriteDateTime(Package.Owner + '/' + Package.Name, 'InstalledVersionTime', Package.FInstalledVersion.VersionTime);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('TSimbaPackageConfig.SavePackage: ' + E.Message);
  end;
end;

class procedure TSimbaPackageConfig.LoadPackage(Package: TSimbaPackage);
begin
  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      Package.FInstalledVersion.Name     := ReadString(Package.Owner + '/' + Package.Name, 'Name', '');
      Package.FInstalledVersion.Path     := ReadString(Package.Owner + '/' + Package.Name, 'Path', '');
      Package.FInstalledVersion.Version  := ReadString(Package.Owner + '/' + Package.Name, 'InstalledVersion', '');
      Package.FInstalledVersion.Scripts  := ReadString(Package.Owner + '/' + Package.Name, 'Scripts', '');
      Package.FInstalledVersion.Examples := ReadString(Package.Owner + '/' + Package.Name, 'Examples', '');

      Package.FInstalledVersion.VersionTime := ReadDateTime(Package.Owner + '/' + Package.Name, 'InstalledVersionTime', 0);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('TSimbaPackageConfig.LoadPackage: ' + E.Message);
  end;
end;

class procedure TSimbaPackageConfig.RemovePackage(Package: TSimbaPackage);
begin
  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      EraseSection(Package.Owner + '/' + Package.Name);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('TSimbaPackageConfig.RemovePackage: ' + E.Message);
  end;
end;

function LoadPackages: TSimbaPackageList;
begin
  Result := TSimbaPackageConfig.LoadPackages();
end;

function TSimbaPackageRelease.IsEmpty: Boolean;
begin
  Result := Self = Default(TSimbaPackageRelease);
end;

class function TSimbaPackageRelease.Empty: TSimbaPackageRelease;
begin
  Result := Default(TSimbaPackageRelease);
end;

class operator TSimbaPackageRelease.=(const Left, Right: TSimbaPackageRelease): Boolean;
begin
  Result := (Left.Version = Right.Version) and
            (Left.Notes = Right.Notes) and
            (Left.Time = Right.Time) and
            (Left.TimeStamp = Right.TimeStamp) and
            (Left.DownloadURL = Right.DownloadURL);
end;

class operator TSimbaPackageRelease.<>(const Left, Right: TSimbaPackageRelease): Boolean;
begin
   Result := (Left.Version <> Right.Version) or
             (Left.Notes <> Right.Notes) or
             (Left.Time <> Right.Time) or
             (Left.TimeStamp <> Right.TimeStamp) or
             (Left.DownloadURL <> Right.DownloadURL);
end;

function TSimbaPackage.Install(Version: String; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
begin
  Result := False;

  if Release[Version].IsEmpty() then
  begin
    DebugLn('TSimbaPackage.Install: Empty release');
    Exit;
  end;

  if (GetZipFunction <> nil) then
  begin
    with Release[Version] do
    begin
      Result := GetZipFunction(DownloadURL, Path, Flat, IgnoreList.Split([',']));

      if Result then
      begin
        FInstalledVersion.Name := FName;
        if (Options[Version].Name <> '') then
          FInstalledVersion.Name := Options[Version].Name;

        FInstalledVersion.Path        := Path;
        FInstalledVersion.Version     := Version;
        FInstalledVersion.VersionTime := Time;

        TSimbaPackageConfig.SavePackage(Self);
      end;
    end;
  end else
    DebugLn('TSimbaPackage.Install: GetZipFunction = nil');
end;

procedure TSimbaPackage.Remove;
begin
  TSimbaPackageConfig.RemovePackage(Self);
end;

function TSimbaPackage.GetCacheAge: Integer;
var
  DateTime: TDateTime;
begin
  Result := 0;
  if FileAge(FCacheFile, DateTime) then
    Result := MinutesBetween(Now(), DateTime);
end;

procedure TSimbaPackage.AddRelease(Version, TimeStamp, Notes, DownloadURL: String);
var
  Item: TSimbaPackageRelease;
begin
  Item.Version := Version;
  Item.TimeStamp := TimeStamp;
  Item.Time := ISO8601ToDateDef(TimeStamp, 0, False);
  Item.Notes := Notes;
  Item.DownloadURL := DownloadURL;

  FReleases.Add(Item);
end;

function TSimbaPackage.GetOptions(Version: String): TSimbaPackage_Options;
var
  ResponseCode: Integer;
  Contents: String;
begin
  Result := Default(TSimbaPackage_Options);

  if (GetPageFunction <> nil) then
  begin
    Contents := GetPageFunction(Format(GITHUB_URL_PACKAGE_OPTIONS, [FOwner, FName, Version]), [HTTP_OK, HTTP_FORBIDDEN, HTTP_NOT_FOUND, HTTP_BAD_REQUEST], ResponseCode);

    if (ResponseCode = HTTP_OK) then
      with TStringList.Create() do
      try
        Text := Contents;

        Result.Loaded := True;
        Result.Name := Values['name'];
        Result.Directory := Values['directory'];
        Result.IgnoreList := Values['ignore'];
        Result.Flat := StrToBoolDef(Values['flat'], False);
      finally
        Free();
      end;
  end else
    DebugLn('TSimbaPackage.GetOptions: GetPageFunction = nil');
end;

function TSimbaPackage.LoadReleases(UseCache: Boolean): Boolean;

  function ParseJSON(Data: String): Boolean;
  var
    JSON: TJSONData;
    I: Integer;
    Download, Notes, Time, Version: TJSONData;
  begin
    try
      JSON := GetJSON(Data);
    except
    end;

    if (JSON is TJSONArray) then
    begin
      for I := 0 to JSON.Count - 1 do
       if (JSON.Items[I] is TJSONObject) then
       begin
         with TJSONObject(JSON.Items[I]) do
           if Find('zipball_url', Download) and Find('body', Notes) and Find('published_at', Time) and Find('tag_name', Version) then
             AddRelease(Version.AsString, Time.AsString, Notes.AsString, Download.AsString);
        end;
    end;

    Result := FReleases.Count > 0;

    if (JSON <> nil) then
      JSON.Free();
  end;

  procedure ParseBranchesJSON(Data: String);
  var
    JSON, Name: TJSONData;
    I: Integer;
  begin
    try
      JSON := GetJSON(Data);
    except
    end;

    if (JSON is TJSONArray) then
      for I := 0 to JSON.Count - 1 do
        if (JSON.Items[I] is TJSONObject) then
         begin
           with TJSONObject(JSON.Items[I]) do
             if Find('name', Name) then
               AddRelease(Name.AsString, '', 'Branch', Format(GITHUB_URL_DOWNLOAD_BRANCH, [FOwner, FName, Name.AsString]));
          end;

    if (JSON <> nil) then
      JSON.Free();
  end;

  function LoadCache: Boolean;
  begin
    Result := (CacheAge < 60) and ParseJSON(ReadFile(FCacheFile));
  end;

  procedure UpdateCache;
  var
    JSON: TJSONArray;
    I: Integer;
  begin
    JSON := TJSONArray.Create();

    try
      for I := 0 to FReleases.Count - 1 do
        JSON.Add(TJSONObject.Create(
          ['zipball_url',  FReleases[I].DownloadURL,
           'body',         FReleases[I].Notes,
           'published_at', FReleases[I].TimeStamp,
           'tag_name',     FReleases[I].Version])
        );

      WriteFile(FCacheFile, JSON.FormatJSON());
    finally
      JSON.Free();
    end;
  end;

var
  ResponseCode: Integer;
  Contents: String;
begin
  FReleases.Clear();

  Result := UseCache and LoadCache();
  if Result then
    Exit;

  if (GetPageFunction <> nil) then
  begin
    Contents := GetPageFunction(FReleasesURL, [HTTP_OK, HTTP_FORBIDDEN, HTTP_NOT_FOUND, HTTP_BAD_REQUEST], ResponseCode);

    Result := (ResponseCode = HTTP_OK);
    if Result then
    begin
      if (Contents <> '') then
        ParseJSON(Contents);
      // Add branches
      ParseBranchesJSON(GetPageFunction(FBranchesURL, [HTTP_OK, HTTP_FORBIDDEN, HTTP_NOT_FOUND, HTTP_BAD_REQUEST], ResponseCode));

      UpdateCache();
    end;
  end else
    DebugLn('TSimbaPackage.LoadReleases: GetPageFunction = nil');
end;

function TSimbaPackage.GetRelease(Version: String): TSimbaPackageRelease;
var
  I: Integer;
begin
  for I := 0 to FReleases.Count - 1 do
    if (Version = FReleases[I].Version) then
    begin
      Result := FReleases[I];
      Exit;
    end;

  Result := TSimbaPackageRelease.Empty();
end;

constructor TSimbaPackage.Create(Repository: String);
var
  Args: TStringArray;
begin
  FReleases := TSimbaPackageReleaseList.Create();

  Args := Repository.Split('/');
  if (Length(Args) = 2) then
  begin
    FOwner := Args[0];
    FName := Args[1];
    FIssuesURL := Format(GITHUB_URL_REPOSITORY_ISSUES, [FOwner, FName]);
    FReleasesURL := Format(GITHUB_API_RELEASES, [FOwner, FName]);
    FBranchesURL := Format(GITHUB_API_BRANCHES, [FOwner, FName]);
    FURL := Format(GITHUB_URL_REPOSITORY, [FOwner, FName]);
    FCacheFile := GetPackagePath() + FOwner + '-' + FName;

    TSimbaPackageConfig.LoadPackage(Self);
  end else
    DebugLn('TSimbaPackage.Create: Invalid Repository');
end;

destructor TSimbaPackage.Destroy;
begin
  if (FReleases <> nil) then
    FreeAndNil(FReleases);

  inherited Destroy();
end;

end.

