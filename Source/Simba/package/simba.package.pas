unit simba.package;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, IniFiles, Generics.Collections,
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
    Templates: String;
  end;

  TSimbaPackage_InstalledData = record
    Version: String;
    VersionTime: TDateTime;
    Templates: String;
    Name: String;
  end;

  TSimbaPackageGetPage = function(URL: String; AllowedResponseCodes: array of Int32; out ResponseCode: Int32): String of object;
  TSimbaPackageGetZIP  = function(URL: String; OutputPath: String; Flat: Boolean; IgnoreList: TStringArray): Boolean of object;

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
    FGetZIP: TSimbaPackageGetZIP;

    function GetOptions(Version: String): TSimbaPackage_Options;
    function GetReleases: TGithubReleases;
    function GetRelease(Version: String): TGithubRelease;
    function GetCacheAge: Int32;
    function GetInstalledData: TSimbaPackage_InstalledData;

    procedure SetInstalledData(Data: TSimbaPackage_InstalledData);
  public
    property Owner: String read FOwner;
    property Name: String read FName;
    property URL: String read FURL;
    property IssuesURL: String read FIssuesURL;
    property Releases: TGithubReleases read GetReleases;
    property Release[Version: String]: TGithubRelease read GetRelease;
    property Options[Version: String]: TSimbaPackage_Options read GetOptions;

    property CacheAge: Int32 read GetCacheAge;

    property InstalledData: TSimbaPackage_InstalledData read GetInstalledData write SetInstalledData;

    procedure UpdateReleases(UseCache: Boolean);

    function Install(Version: String; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
    procedure Remove;

    constructor Create(Repository: String; GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZIP);
  end;

  TSimbaPackageList = class(specialize TObjectList<TSimbaPackage>)
  public
    // Fills from packages.ini
    procedure Load(GetPage: TSimbaPackageGetPage = nil; GetZip: TSimbaPackageGetZIP = nil);
  end;

  TSimbaPackage_Config = class(TINIFile)
  public
    // Creates using packages.ini
    constructor Create; reintroduce;
  end;

implementation

uses
  FileUtil, DateUtils, LazLoggerBase,
  simba.package_form, simba.files, simba.httpclient;

function TSimbaPackage.Install(Version: String; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
var
  Data: TSimbaPackage_InstalledData;
begin
  Result := False;

  if (FGetZip <> nil) then
  begin
    with Release[Version] do
    begin
      Result := FGetZIP(DownloadURL, Path, Flat, IgnoreList.Split([',']));

      if Result then
      begin
        Data.Name := Options[Version].Name;
        if (Data.Name = '') then
          Data.Name := FName;

        Data.Templates := Options[Version].Templates;
        if (Data.Templates <> '') then
          Data.Templates := ConcatPaths([Path, Data.Templates]);

        Data.Version     := Version;
        Data.VersionTime := Time;

        InstalledData := Data;
      end;
    end;
  end else
    DebugLn('TSimbaPackage.Install: FGetZip = nil');
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
  Result := Default(TSimbaPackage_Options);

  if (FGetPage <> nil) then
  begin
    Contents := FGetPage(Format(GITHUB_URL_PACKAGE_OPTIONS, [FOwner, FName, Version]), [HTTP_OK, HTTP_NOT_FOUND], ResponseCode);
    if (ResponseCode = HTTP_NOT_FOUND) then
      Exit;

    with TStringList.Create() do
    try
      Text := Contents;

      Result.Loaded := True;
      Result.Name := Values['name'];
      Result.Directory := Values['directory'];
      Result.IgnoreList := Values['ignore'];
      Result.Flat := StrToBoolDef(Values['flat'], False);
      Result.Templates := Values['templates'];
    finally
      Free();
    end;
  end else
    DebugLn('TSimbaPackage.GetOptions: FGetPage = nil');
end;

function TSimbaPackage.GetInstalledData: TSimbaPackage_InstalledData;
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      Result.Name      := ReadString(FOwner + '/' + FName, 'Name', '');
      Result.Templates := ReadString(FOwner + '/' + FName, 'Templates', '');
      Result.Version   := ReadString(FOwner + '/' + FName, 'InstalledVersion', '');

      Result.VersionTime := ReadDateTime(FOwner + '/' + FName, 'InstalledVersionTime', 0);
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.GetInstalledData: %s', [E.Message]);
  end;
end;

procedure TSimbaPackage.SetInstalledData(Data: TSimbaPackage_InstalledData);
begin
  try
    with TSimbaPackage_Config.Create() do
    try
      WriteString(FOwner + '/' + FName, 'Name', Data.Name);
      WriteString(FOwner + '/' + FName, 'Templates', Data.Templates);
      WriteString(FOwner + '/' + FName, 'InstalledVersion', Data.Version);

      WriteDateTime(FOwner + '/' + FName, 'InstalledVersionTime', Data.VersionTime);
    finally
      Free();
    end;
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception in TSimbaPackage.SetInstalledData: %s', [E.Message]);
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

  if (FGetPage <> nil) then
  begin
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
  end else
    DebugLn('TSimbaPackage.UpdateReleases: FGetPage = nil');
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

constructor TSimbaPackage.Create(Repository: String; GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZIP);
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

procedure TSimbaPackageList.Load(GetPage: TSimbaPackageGetPage; GetZip: TSimbaPackageGetZIP);
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

