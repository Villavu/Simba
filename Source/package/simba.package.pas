{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

const
  PACKAGE_SETTINGS_VERSION = 1;

type
  TSimbaPackageBranch = record
    Name: String;
    DownloadURL: String;
  end;
  TSimbaPackageBranchArray = array of TSimbaPackageBranch;

  TSimbaPackageRelease = record
    Name: String;
    Notes: String;
    Age: String;
    Time: TDateTime;
    DownloadURL: String;
    OptionsURL: String;
  end;
  TSimbaPackageReleaseArray = array of TSimbaPackageRelease;

  TSimbaPackageInfo = record
    Description: String;
    FullName: String;
    Name: String;
    HomepageURL: String;
  end;

  TSimbaPackageEndpoint = class
  public
    constructor Create(URL: String); virtual; abstract;

    function GetInfo: TSimbaPackageInfo; virtual; abstract;
    function GetReleases: TSimbaPackageReleaseArray; virtual; abstract;
    function GetBranches: TSimbaPackageBranchArray; virtual; abstract;
  end;

  TSimbaPackage = class
  protected
    FEndpoint: TSimbaPackageEndpoint;
    FURL: String;
    FLoaded: Boolean;
    FInfo: TSimbaPackageInfo;
    FReleases: TSimbaPackageReleaseArray;
    FBranches: TSimbaPackageBranchArray;

    procedure ClearConfig;
    procedure WriteConfig(Key: String; Value: String);
    function ReadConfig(Key: String): String;

    procedure SetAutoUpdateEnabled(AValue: Boolean);
    procedure SetInstalledVersion(Value: String);
    procedure SetInstalledVersionTime(Value: TDateTime);
    procedure SetInstalledPath(Value: String);

    function GetAutoUpdateEnabled: Boolean;
    function GetInstalledPath: String;
    function GetInstalledVersion: String;
    function GetInstalledVersionTime: TDateTime;
    function GetLatestVersionTime: TDateTime;
    function GetLatestVersion: String;
  public
    constructor Create(AURL: String);
    destructor Destroy; override;

    procedure Load;
    function Exists: Boolean;

    property URL: String read FURL;
    property EndPoint: TSimbaPackageEndpoint read FEndpoint;

    property Branches: TSimbaPackageBranchArray read FBranches;
    property Releases: TSimbaPackageReleaseArray read FReleases;
    property Info: TSimbaPackageInfo read FInfo;

    property InstalledPath: String read GetInstalledPath write SetInstalledPath;
    property InstalledVersion: String read GetInstalledVersion write SetInstalledVersion;
    property InstalledVersionTime: TDateTime read GetInstalledVersionTime write SetInstalledVersionTime;
    property LatestVersion: String read GetLatestVersion;
    property LatestVersionTime: TDateTime read GetLatestVersionTime;
    property AutoUpdateEnabled: Boolean read GetAutoUpdateEnabled write SetAutoUpdateEnabled;

    function IsBranchInstalled: Boolean;
    function IsInstalled: Boolean;
    function UnInstall(RemoveFiles: Boolean): Boolean;

    function HasUpdate: Boolean;
    function HasReleases: Boolean;

    // if file extension is ".packageexample.simba" the file is added to the "file > open example" form.
    function GetExamples: TStringArray;
    // if file extension is ".packagescript.simba" the file is to simba's main menu bar.
    function GetScripts: TStringArray;
  end;
  TSimbaPackageArray = array of TSimbaPackage;

  function LoadPackages: TSimbaPackageArray;
  function GetInstalledPackages: TSimbaPackageArray;
  procedure FreePackages(Packages: TSimbaPackageArray);

implementation

uses
  inifiles, dateutils, fileutil, lazfileutils,
  simba.env, simba.httpclient,
  simba.package_endpoint_github, simba.package_endpoint_custom, simba.threading,
  simba.files;

function LoadPackageURLs: TStringArray;
var
  List: TStringList;

  procedure Load;
  begin
    List.Text := TSimbaHTTPClient.SimpleGet(SIMBA_PACKAGES_URL, []);
  end;

var
  Thread: TThread;
  I: Integer;
  Sections: TStringList;
begin
  Sections := TStringList.Create();

  List := TStringList.Create();
  List.CaseSensitive := False;

  try
    Thread := RunInThread(@Load);
    Thread.WaitFor();
    Thread.Free();

    with TIniFile.Create(SimbaEnv.PackagesPath + 'packages.ini') do
    try
      ReadSections(Sections);

      if (Sections.Count > 0) then
      begin
        if (ReadInteger('Settings', 'Version', -1) = PACKAGE_SETTINGS_VERSION) then
        begin
          for I := 0 to Sections.Count - 1 do
            if (Sections[I] <> 'Settings') and (List.IndexOf(Sections[I]) = -1) then
              List.Add(Sections[I]);
        end else
        begin
          DebugLn('Package setting versions changed (%d). Erasing!', [PACKAGE_SETTINGS_VERSION]);
          DeleteFile(FileName);
        end;
      end;
    finally
      Free();
    end;
  except
  end;

  Result := List.ToStringArray();

  List.Free();
  Sections.Free();
end;

function LoadPackages: TSimbaPackageArray;
var
  URLs: TStringArray;
  Threads: specialize TArray<TThread>;
  I: Integer;
begin
  URLs := LoadPackageURLs();

  SetLength(Result, Length(URLs));
  SetLength(Threads, Length(URLs));
  for I := 0 to High(URLs) do
  begin
    Result[I] := TSimbaPackage.Create(URLs[I]);
    Threads[I] := RunInThread(@Result[I].Load);

    Sleep(100);
  end;

  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor();
    Threads[I].Free();
  end;

  // reorder so updates are first, then installed, then uninstalled
  for I := 0 to High(Result) do
    if Result[I].IsInstalled() then
      specialize MoveElement<TSimbaPackage>(Result, I, 0);
  for I := 0 to High(Result) do
    if Result[I].HasUpdate() then
      specialize MoveElement<TSimbaPackage>(Result, I, 0);
end;

function GetInstalledPackages: TSimbaPackageArray;
var
  Sections: TStringList;
  I: Integer;
begin
  Result := [];

  Sections := TStringList.Create();
  try
    with TIniFile.Create(SimbaEnv.PackagesPath + 'packages.ini') do
    try
      ReadSections(Sections);

      if (Sections.Count > 0) and (ReadInteger('Settings', 'Version', -1) = PACKAGE_SETTINGS_VERSION) then
      begin
        for I := 0 to Sections.Count - 1 do
        begin
          if (Sections[I] = 'Settings') or (ReadString(Sections[I], 'InstalledVersion', '') = '') then
            Continue;
          if DirectoryExists(ReadString(Sections[I], 'InstalledPath', '')) then
            Result := Result + [TSimbaPackage.Create(Sections[I])];
        end;
      end;
    finally
      Free();
    end;
  except
  end;
  Sections.Free();
end;

procedure FreePackages(Packages: TSimbaPackageArray);
var
  I: Integer;
begin
  for I := 0 to High(Packages) do
    Packages[I].Free();
end;

function TSimbaPackage.GetLatestVersion: String;
begin
  Result := '';
  if HasReleases() then
    Result := FReleases[0].Name;
end;

function TSimbaPackage.GetLatestVersionTime: TDateTime;
begin
  Result := 0;
  if HasReleases() then
    Result := FReleases[0].Time;
end;

function TSimbaPackage.GetInstalledVersionTime: TDateTime;
begin
  Result := StrToDateTimeDef(ReadConfig('InstalledVersionTime'), 0);
end;

procedure TSimbaPackage.SetInstalledVersionTime(Value: TDateTime);
begin
  WriteConfig('InstalledVersionTime', DateTimeToStr(Value));
end;

function TSimbaPackage.GetInstalledVersion: String;
begin
  Result := ReadConfig('InstalledVersion');
  if (Result <> '') and (not DirectoryExists(InstalledPath)) then
    Result := '';
end;

function TSimbaPackage.GetAutoUpdateEnabled: Boolean;
begin
  Result := (Self.InstalledVersion <> '') and (Self.ReadConfig('AutoUpdate') = 'True');
end;

procedure TSimbaPackage.SetAutoUpdateEnabled(AValue: Boolean);
begin
  case AValue of
    True:  Self.WriteConfig('AutoUpdate', 'True');
    False: Self.WriteConfig('AutoUpdate', 'False');
  end;
end;

procedure TSimbaPackage.ClearConfig;
var
  Keys: TStringList;
  Key: String;
begin
  Keys := TStringList.Create();

  try
    with TIniFile.Create(SimbaEnv.PackagesPath + 'packages.ini') do
    try
      ReadSection(FURL, Keys);
      for Key in Keys do
        DeleteKey(FURL, Key);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaPackage.ClearConfig]: %s', [E.ToString()]);
  end;

  Keys.Free();
end;

procedure TSimbaPackage.WriteConfig(Key: String; Value: String);
begin
  try
    with TIniFile.Create(SimbaEnv.PackagesPath + 'packages.ini') do
    try
      WriteInteger('Settings', 'Version', PACKAGE_SETTINGS_VERSION);
      WriteString(FURL, Key, Value);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaPackage.WriteConfig]: %s', [E.ToString()]);
  end;
end;

function TSimbaPackage.ReadConfig(Key: String): String;
begin
  try
    with TIniFile.Create(SimbaEnv.PackagesPath + 'packages.ini') do
    try
      Result := ReadString(FURL, Key, '');
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaPackage.ReadConfig]: %s', [E.ToString()]);
  end;
end;

function TSimbaPackage.GetInstalledPath: String;
begin
  Result := ReadConfig('InstalledPath');
  if (Result <> '') then
    Result := CleanAndExpandDirectory(Result);
end;

procedure TSimbaPackage.SetInstalledPath(Value: String);
begin
  WriteConfig('InstalledPath', Value);
end;

procedure TSimbaPackage.SetInstalledVersion(Value: String);
begin
  WriteConfig('InstalledVersion', Value);
end;

constructor TSimbaPackage.Create(AURL: String);
begin
  inherited Create();

  FURL := AURL;
  if ('github.com' in FURL) then
    FEndpoint := TSimbaPackageEndpoint_Github.Create(FURL)
  else
    FEndpoint := TSimbaPackageEndpoint_Custom.Create(FURL);
end;

destructor TSimbaPackage.Destroy;
begin
  if (FEndpoint <> nil) then
    FreeAndNil(FEndpoint);

  inherited Destroy();
end;

procedure TSimbaPackage.Load;
begin
  FLoaded := True;

  FInfo := FEndpoint.GetInfo();
  FBranches := FEndpoint.GetBranches();
  FReleases := FEndpoint.GetReleases();
end;

function TSimbaPackage.Exists: Boolean;
begin
  Result := Self.Info.Name <> '';
end;

function TSimbaPackage.IsBranchInstalled: Boolean;
begin
  Result := (InstalledVersion <> '') and (InstalledVersionTime = 0);
end;

function TSimbaPackage.IsInstalled: Boolean;
begin
  Result := (InstalledVersion <> '') and DirectoryExists(InstalledPath);
end;

function TSimbaPackage.UnInstall(RemoveFiles: Boolean): Boolean;
begin
  Result := IsInstalled();

  if Result then
  begin
    if RemoveFiles and PathIsInPath(InstalledPath, SimbaEnv.SimbaPath) then
      DeleteDirectory(InstalledPath, False);

    ClearConfig();
  end;
end;

function TSimbaPackage.HasUpdate: Boolean;
begin
  Result := IsInstalled() and (not IsBranchInstalled()) and (LatestVersionTime > InstalledVersionTime);
end;

function TSimbaPackage.HasReleases: Boolean;
begin
  Result := Length(FReleases) > 0;
end;

function TSimbaPackage.GetExamples: TStringArray;
begin
  if IsInstalled() then
    Result := TSimbaDir.DirSearch(InstalledPath, '*.packageexample.simba', True)
  else
    Result := [];
end;

function TSimbaPackage.GetScripts: TStringArray;
begin
  if IsInstalled() then
    Result := TSimbaDir.DirSearch(InstalledPath, '*.packagescript.simba', True)
  else
    Result := [];
end;

end.

