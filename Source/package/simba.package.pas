unit simba.package;

{$i simba.inc}

interface

uses
  classes, sysutils;

const
  PACKAGE_SETTINGS_VERSION = 1;

type
  TSimbaPackageVersion = class(TInterfacedObject)
    Name: String;
    Notes: String;
    Age: String;
    Time: TDateTime;
    DownloadURL: String;
    OptionsURL: String;

    constructor Create(AName, ANotes, ADownloadURL, AOptionsURL, ATime: String);
  end;
  TSimbaPackageBranch = class(TSimbaPackageVersion);
  TSimbaPackageVersionArray = array of TSimbaPackageVersion;

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
    function GetVersions: TSimbaPackageVersionArray; virtual; abstract;
  end;

  TSimbaPackage = class
  private
    function GetAutoUpdateEnabled: Boolean;
    procedure SetAutoUpdateEnabled(AValue: Boolean);
  protected
    FEndpoint: TSimbaPackageEndpoint;
    FURL: String;
    FLoaded: Boolean;
    FInfo: TSimbaPackageInfo;
    FVersions: TSimbaPackageVersionArray;
    FVersionsNoBranch: TSimbaPackageVersionArray;

    procedure ClearConfig;
    procedure WriteConfig(Key: String; Value: String);
    function ReadConfig(Key: String): String;

    procedure SetInstalledVersion(Value: String);
    procedure SetInstalledVersionTime(Value: TDateTime);
    procedure SetInstalledPath(Value: String);

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

    property VersionsNoBranch: TSimbaPackageVersionArray read FVersionsNoBranch;
    property Versions: TSimbaPackageVersionArray read FVersions;
    property Info: TSimbaPackageInfo read FInfo;

    property InstalledPath: String read GetInstalledPath write SetInstalledPath;
    property InstalledVersion: String read GetInstalledVersion write SetInstalledVersion;
    property InstalledVersionTime: TDateTime read GetInstalledVersionTime write SetInstalledVersionTime;
    property LatestVersion: String read GetLatestVersion;
    property LatestVersionTime: TDateTime read GetLatestVersionTime;
    property AutoUpdateEnabled: Boolean read GetAutoUpdateEnabled write SetAutoUpdateEnabled;

    function IsInstalled: Boolean;
    function UnInstall(RemoveFiles: Boolean): Boolean;

    function HasUpdate: Boolean;
    function HasVersions: Boolean;
  end;
  TSimbaPackageArray = array of TSimbaPackage;

  function LoadPackages: TSimbaPackageArray;
  function GetPackageFiles(Name: String): TStringList;

implementation

uses
  inifiles, lazloggerbase, forms, dateutils, fileutil, lazfileutils,
  simba.files, simba.mufasatypes, simba.httpclient,
  simba.package_endpoint_github, simba.package_endpoint_custom;

function GetPackageFiles(Name: String): TStringList;
var
  Files: TStringList;
  I, J: Integer;
  FileName: String;
begin
  Result := TStringList.Create();

  Files := FindAllFiles(GetIncludePath(), '.simbapackage');
  for I := 0 to Files.Count - 1 do
    with TStringList.Create() do
    try
      LoadFromFile(Files[I]);

      for J := 0 to Count - 1 do
        if (Names[J] = Name) then
        begin
          FileName := ConcatPaths([ExtractFileDir(Files[I]), SetDirSeparators(ValueFromIndex[J])]);
          if FileExists(FileName) then
            Result.AddPair(ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFileDir(Files[I]))), FileName);
        end;
    finally
      Free();
    end;
end;

constructor TSimbaPackageVersion.Create(AName, ANotes, ADownloadURL, AOptionsURL, ATime: String);
begin
  inherited Create();

  Name := AName;
  DownloadURL := ADownloadURL;
  OptionsURL := AOptionsURL;
  Notes := ANotes.Trim();
  if (Notes = '') then
    Notes := '(no release notes)';

  if (ATime <> '') then
  begin
    if ATime.IsInteger() then
      Time := UnixToDateTime(ATime.ToInt64())
    else
      Time := ISO8601ToDate(ATime);

    case DaysBetween(Now(), Time) of
      0: Age := '(today)';
      1: Age := '(yesterday)';
      else
        Age := '(' + IntToStr(DaysBetween(Now(), Time)) + ' days ago)';
    end;
  end;
end;

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
    Thread := Threaded(@Load);
    Thread.WaitFor();
    Thread.Free();

    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
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
  Procs: TProcArray;
  Weights: TIntegerArray;
  I, J: Integer;
begin
  URLs := LoadPackageURLs();

  SetLength(Result, Length(URLs));
  SetLength(Procs, Length(URLs));
  for I := 0 to High(URLs) do
  begin
    Result[I] := TSimbaPackage.Create(URLs[I]);
    Procs[I] := @Result[I].Load;
  end;

  Threaded(Procs, 100);

  // sort so updates first, installed second
  SetLength(Weights, Length(Result));
  for I := 0 to High(Result) do
  begin
    if Result[I].IsInstalled() then
      Inc(Weights[I]);
    if Result[I].HasUpdate() then
      Inc(Weights[I]);
  end;

  for I := 0 to High(Result) do
    for J := 0 to High(Result) do
    begin
      if (Weights[I] > Weights[J]) then
      begin
        Swap(Pointer(Result[I]), Pointer(Result[J]));
        Swap(Weights[I], Weights[J]);
      end;
    end;
end;

function TSimbaPackage.GetLatestVersion: String;
begin
  Result := '';
  if (Length(Versions) > 0) then
    Result := Versions[0].Name;
end;

function TSimbaPackage.GetLatestVersionTime: TDateTime;
begin
  Result := 0;
  if (Length(VersionsNoBranch) > 0) then
    Result := VersionsNoBranch[0].Time;
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
  Result := (Self.InstalledVersion <> '') and (Self.ReadConfig('AutoUpdate') <> '');
end;

procedure TSimbaPackage.SetAutoUpdateEnabled(AValue: Boolean);
begin
  Self.WriteConfig('AutoUpdate', 'True')
end;

procedure TSimbaPackage.ClearConfig;
var
  Keys: TStringList;
  Key: String;
begin
  Keys := TStringList.Create();

  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
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
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
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
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
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
var
  I: Integer;
begin
  FLoaded := True;

  FInfo := FEndpoint.GetInfo();
  FVersions := FEndpoint.GetVersions();
  FVersionsNoBranch := [];

  for I := 0 to High(FVersions) do
  begin
    if (FVersions[I] is TSimbaPackageBranch) then
      Continue;

    FVersionsNoBranch += [FVersions[I]];
  end;
end;

function TSimbaPackage.Exists: Boolean;
begin
  Result := Self.Info.Name <> '';
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
    if RemoveFiles and PathIsInPath(InstalledPath, GetSimbaPath()) then
      DeleteDirectory(InstalledPath, False);

    ClearConfig();
  end;
end;

function TSimbaPackage.HasUpdate: Boolean;
begin
  Result := IsInstalled() and (LatestVersionTime > InstalledVersionTime);
end;

function TSimbaPackage.HasVersions: Boolean;
begin
  Result := Length(Self.Versions) > 0;
end;

end.

