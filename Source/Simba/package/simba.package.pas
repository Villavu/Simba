unit simba.package;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, generics.collections, inifiles,
  simba.package_github_releases;

const
  GITHUB_URL_RELEASES          = 'https://api.github.com/repos/%s/%s/releases';              // {Owner} {Name}
  GITHUB_URL_REPOSITORY        = 'https://github.com/%s/%s/tree/%s';                         // {Owner} {Name}
  GITHUB_URL_REPOSITORY_ISSUES = 'https://github.com/%s/%s/issues';                          // {Owner} {Name}
  GITHUB_URL_DOWNLOAD_MASTER   = 'https://github.com/%s/%s/archive/master.zip';              // {Owner} {Name}
  GITHUB_URL_PACKAGE_OPTIONS   = 'https://raw.githubusercontent.com/%s/%s/%s/.simbapackage'; // {Owner} {Name} {Version}

type
  TSimbaPackage_Options = record
    Loaded: Boolean;
    Name: String;
    Directory: String;
    Flat: Boolean;
    IgnoreList: String;
  end;

  TSimbaPackageGetPage = function(URL: String; AllowedResponseCodes: array of Int32; out ResponseCode: Int32): String of object;
  TSimbaPackageGetZip  = function(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray): Boolean of object;

  TSimbaPackage = class
  public
    FOwner: String;
    FName: String;
    FURL: String;
    FIssuesURL: String;
    FReleasesURL: String;
    FReleases: TGithubReleases;
    FCacheFile: String;

    FGetPage: TSimbaPackageGetPage;
    FGetZIP: TSimbaPackageGetZip;

    function GetInstalledVersion: String;
    function GetInstalledVersionTime: TDateTime;
    function GetOptions(Version: String): TSimbaPackage_Options;
    function GetReleases: TGithubReleases;
    function GetRelease(Version: String): TGithubRelease;
    function GetCacheAge: Int32;

    procedure SetInstalledVersion(Value: String);
    procedure SetInstallVersionTime(Value: TDateTime);
  public
    property Owner: String read FOwner;
    property Name: String read FName;
    property URL: String read FURL;
    property IssuesURL: String read FIssuesURL;
    property Releases: TGithubReleases read GetReleases;
    property Release[Version: String]: TGithubRelease read GetRelease;
    property Options[Version: String]: TSimbaPackage_Options read GetOptions;

    property CacheAge: Int32 read GetCacheAge;

    property InstalledVersion: String read GetInstalledVersion write SetInstalledVersion;
    property InstalledVersionTime: TDateTime read GetInstalledVersionTime write SetInstallVersionTime;

    procedure UpdateReleases(UseCache: Boolean);

    function Install(Version: String; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
    procedure Remove;

    constructor Create(Repository: String; GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZip);
  end;

  TSimbaPackageList = class(specialize TObjectList<TSimbaPackage>)
  public
    // Fills from packages.ini
    procedure Load(GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZip);
  end;

  TSimbaPackage_Config = class(TINIFile)
  public
    // Creates using packages.ini
    constructor Create; reintroduce;
  end;

const
  NullOptions: TSimbaPackage_Options = (Loaded: False; Name: ''; Directory: ''; Flat: False; IgnoreList: '');

implementation

uses
  fileutil, dateutils,
  simba.package_form, simba.files, simba.httpclient;

function TSimbaPackage.Install(Version: String; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
begin
  Result := False;

  with Release[Version] do
    if FGetZIP(DownloadURL, Path, Flat, IgnoreList.Split([','])) then
    begin
      InstalledVersion := Version;
      InstalledVersionTime := Time;

      Result := True;
    end;
end;

function TSimbaPackage.GetCacheAge: Int32;
var
  DateTime: TDateTime;
begin
  Result := 0;
  if FileAge(FCacheFile, DateTime) then
    Result := MinutesBetween(Now(), DateTime);
end;

function TSimbaPackage.GetOptions(Version: String): TSimbaPackage_Options;
var
  ResponseCode: Int32;
  Contents: String;
begin
  Result := NullOptions;

  Contents := FGetPage(Format(GITHUB_URL_PACKAGE_OPTIONS, [FOwner, FName, Version]), [HTTP_OK, HTTP_NOT_FOUND], ResponseCode);
  if (ResponseCode = HTTP_NOT_FOUND) then
    Exit;

  with TStringList.Create() do
  try
    Text := Contents;

    Result.Loaded := True;
    Result.Name := Values['name'];
    Result.Directory := Values['directory'];
    Result.IgnoreList := Values['blacklist'] + Values['ignore']; // blacklist & collapse deprecated
    Result.Flat := StrToBoolDef(Values['collapse'] + Values['flat'], False);
  finally
    Free();
  end;
end;

procedure TSimbaPackage.SetInstalledVersion(Value: String);
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      WriteString(FOwner + '/' + FName, 'InstalledVersion', Value);
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.SetInstalledVersion: %s', [E.Message]);
  end;
end;

procedure TSimbaPackage.SetInstallVersionTime(Value: TDateTime);
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      WriteDateTime(FOwner + '/' + FName, 'InstalledVersionTime', Value);
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.SetInstallVersionTime: %s', [E.Message]);
  end;
end;

function TSimbaPackage.GetInstalledVersion: String;
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      Result := ReadString(FOwner + '/' + FName, 'InstalledVersion', '');
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.SetInstalledVersion: %s', [E.Message]);
  end
end;

function TSimbaPackage.GetInstalledVersionTime: TDateTime;
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      Result := ReadDateTime(FOwner + '/' + FName, 'InstalledVersionTime', 0);
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.SetInstallVersionTime: %s', [E.Message]);
  end;
end;

procedure TSimbaPackage.UpdateReleases(UseCache: Boolean);
var
  ResponseCode: Int32;
  Contents: String;
begin
  FReleases := NullReleases;

  if UseCache and (CacheAge < 60) then
  begin
    FReleases.LoadFromFile(FCacheFile);
    if Length(FReleases) > 0 then
      Exit;
  end;

  Contents := FGetPage(FReleasesURL, [HTTP_OK, HTTP_FORBIDDEN, HTTP_NOT_FOUND], ResponseCode);
  if (ResponseCode = HTTP_OK) then
  begin
    FReleases.Fill(Contents);
    FReleases.Add('master',
                  Format(GITHUB_URL_DOWNLOAD_MASTER, [Owner, Name]),
                  Format(GITHUB_URL_REPOSITORY, [Owner, Name, 'master']),
                  '');

    // Update Cache
    FReleases.SaveToFile(FCacheFile);
  end;
end;

function TSimbaPackage.GetReleases: TGithubReleases;
begin
  UpdateReleases(True);

  Result := FReleases;
end;

function TSimbaPackage.GetRelease(Version: String): TGithubRelease;
var
  Rel: TGithubRelease;
begin
  Result := NullRelease;

  for Rel in Releases do
    if Rel.Version = Version then
    begin
      Result := Rel;
      Exit;
    end;
end;

procedure TSimbaPackage.Remove;
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      EraseSection(FOwner + '/' + FName);
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.Remove: %s', [E.Message]);
  end;
end;

constructor TSimbaPackage.Create(Repository: String; GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZip);
var
  Args: TStringArray;
begin
  FGetPage := GetPage;
  FGetZIP := GetZip;

  Args := Copy(Repository.Split('/'), Length(Repository.Split('/')) - 2);

  if (Length(Args) = 2) then
  begin
    FOwner := Args[0];
    FName := Args[1];
    FIssuesURL := Format(GITHUB_URL_REPOSITORY_ISSUES, [FOwner, FName]);
    FReleasesURL := Format(GITHUB_URL_RELEASES, [FOwner, FName]);
    FURL := Format(GITHUB_URL_REPOSITORY, [FOwner, FName, 'master']);
    FCacheFile := GetPackagePath() + FOwner + '-' + FName;
  end;
end;

constructor TSimbaPackage_Config.Create;
begin
  inherited Create(GetPackagePath() + 'packages.ini');

  CacheUpdates := True;
end;

procedure TSimbaPackageList.Load(GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZip);
var
  Sections: TStringList;
  Repository: String;
begin
  Clear();

  Sections := TStringList.Create();

  try
    with TSimbaPackage_Config.Create() do
    try
      ReadSections(Sections);
      for Repository in Sections do
        Add(TSimbaPackage.Create(Repository, GetPage, GetZip));
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackageList.Load: %s', [E.Message]);
  end;

  Sections.Free();
end;

end.

