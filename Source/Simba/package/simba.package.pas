unit simba.package;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, generics.collections, inifiles,
  simba.package_github_releases;

type
  TSimbaPackage_InstallOptions = record
    Version: String;
    Path: String;
    Flat: Boolean;
    IgnoreList: String;
  end;

  TSimbaPackage_GetPageFunction = function(URL: String; AllowedResponseCodes: array of Int32): String of object;
  TSimbaPackage_GetZIPFunction = function(URL: String; OutputPath: String; Flat: Boolean): Boolean of object;

  TSimbaPackage = class
  public
    FOwner: String;
    FName: String;
    FURL: String;
    FIssuesURL: String;
    FReleasesURL: String;
    FReleases: TSimbaPackage_GithubReleases;

    FInstalledVersion: String;
    FInstalledVersionTime: TDateTime;

    FGetPage: TSimbaPackage_GetPageFunction;
    FGetZIP: TSimbaPackage_GetZIPFunction;

    function GetPageFunction: TSimbaPackage_GetPageFunction;
    function GetZIPFunction: TSimbaPackage_GetZIPFunction;
  public
    property Owner: String read FOwner;
    property Name: String read FName;
    property URL: String read FURL;
    property IssuesURL: String read FIssuesURL;
    property ReleasesURL: String read FReleasesURL;
    property Releases: TSimbaPackage_GithubReleases read FReleases;

    property InstalledVersion: String read FInstalledVersion;
    property InstalledVersionTime: TDateTime read FInstalledVersionTime;

    property GetPage: TSimbaPackage_GetPageFunction read GetPageFunction write FGetPage;
    property GetZIP: TSimbaPackage_GetZIPFunction read GetZIPFunction write FGetZIP;

    procedure Remove;
    procedure Save;

    function Install(Options: TSimbaPackage_InstallOptions): Boolean;

    constructor Create(AOwner, AName: String); overload;
    constructor Create(INI: TINIFile; Section: String); overload;
  end;

  TSimbaPackageList = class(specialize TObjectList<TSimbaPackage>)
  public
    // Fills from packages.ini
    procedure Load;
  end;

  TSimbaPackage_Config = class(TINIFile)
  public
    // Creates using packages.ini
    constructor Create; reintroduce;
  end;

implementation

uses
  fileutil,
  simba.package_form, simba.package_github_url, simba.files;

function TSimbaPackage.Install(Options: TSimbaPackage_InstallOptions): Boolean;
var
  FileName: String;
begin
  with FReleases[Options.Version] do
  begin
    Result := GetZIP(DownloadURL, Options.Path, Options.Flat);

    if Result then
    begin
      for FileName in Options.IgnoreList.Split([','], TStringSplitOptions.ExcludeEmpty) do
      begin
        if FileName = '' then
          Continue; // FPC bug...

        if FileExists(Options.Path + FileName) then
          DeleteFile(Options.Path + FileName)
        else
        if DirectoryExists(Options.Path + FileName) then
          DeleteDirectory(Options.Path + FileName, False);
      end;

      FInstalledVersion := Options.Version;
      FInstalledVersionTime := Time;

      Save();
    end;
  end;
end;

function TSimbaPackage.GetPageFunction: TSimbaPackage_GetPageFunction;
begin
  if (FGetPage = nil) then
    raise Exception.Create('TSimbaPackage.GetPage is nil');

  Result := FGetPage;
end;

function TSimbaPackage.GetZIPFunction: TSimbaPackage_GetZIPFunction;
begin
  if (FGetZIP = nil) then
    raise Exception.Create('TSimbaPackage.GetZIP is nil');

  Result := FGetZIP;
end;

procedure TSimbaPackage.Remove;
var
  Config: TSimbaPackage_Config;
begin
  try
    Config := TSimbaPackage_Config.Create();
    Config.EraseSection(FOwner + '/' + FName);
    Config.Free();
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception while removing package: ' + E.Message);
  end;
end;

procedure TSimbaPackage.Save;
var
  Config: TSimbaPackage_Config;
begin
  try
    Config := TSimbaPackage_Config.Create();
    Config.WriteString(FOwner + '/' + FName, 'InstalledVersion', FInstalledVersion);
    Config.WriteDateTime(FOwner + '/' + FName, 'InstalledVersionTime', FInstalledVersionTime);
    Config.Free();
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception while saving package: ' + E.Message);
  end;
end;

constructor TSimbaPackage.Create(AOwner, AName: String);
begin
  FOwner := AOwner;
  FName := AName;
  FReleases := TSimbaPackage_GithubReleases.Create(Self);
  FIssuesURL := Format(GITHUB_URL_REPOSITORY_ISSUES, [FOwner, FName]);
  FReleasesURL := Format(GITHUB_URL_RELEASES, [FOwner, FName]);
  FURL := Format(GITHUB_URL_REPOSITORY, [FOwner, FName, 'master']);
end;

constructor TSimbaPackage.Create(INI: TINIFile; Section: String);
begin
  if Length(Section.Split('/')) = 2 then
    Create(Section.Split('/')[0], Section.Split('/')[1]);

  FInstalledVersion := INI.ReadString(Section, 'InstalledVersion', '');
  FInstalledVersionTime := INI.ReadDateTime(Section, 'InstalledVersionTime', 0);
end;

constructor TSimbaPackage_Config.Create;
begin
  inherited Create(GetPackagePath() + 'packages.ini');

  CacheUpdates := True;
end;

procedure TSimbaPackageList.Load;
var
  Config: TSimbaPackage_Config;
  Sections: TStringList;
  Section: String;
begin
  Config := nil;
  Sections := nil;

  Self.Clear();

  try
    Sections := TStringList.Create();

    Config := TSimbaPackage_Config.Create();
    Config.ReadSections(Sections);

    for Section in Sections do
      Add(TSimbaPackage.Create(Config, Section));
  except
    on E: Exception do
      SimbaPackageForm.Debug('Exception while loading packages: ' + E.Message);
  end;

  if (Config <> nil) then
    Config.Free();
  if (Sections <> nil) then
    Sections.Free();
end;

end.

