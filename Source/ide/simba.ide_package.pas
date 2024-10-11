{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_package;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.httpclient;

type
  TSimbaPackageVersion = record
    Name: String;
    Notes: String;
    Age: String;
    Time: TDateTime;
    DownloadURL: String;
    OptionsURL: String;
    IsBranch: Boolean;
  end;
  TSimbaPackageVersions = array of TSimbaPackageVersion;

  TSimbaPackageEndpoint = class
  protected
    FURL: String;
    FLastHTTPStatus: EHTTPStatus; // last http status of GetPage, useful for detecting when a request failed

    procedure ParseTime(Str: String; out Time: TDateTime; out PrettyDays: String);

    function GetPage(URL: String): String;
    function GetVersions: TSimbaPackageVersions; virtual; abstract;
  public
    constructor Create(URL: String); virtual;

    procedure DeleteCache; virtual;

    property LastHTTPStatus: EHTTPStatus read FLastHTTPStatus;
    property Versions: TSimbaPackageVersions read GetVersions;
  end;

  TSimbaPackage = class
  protected
    FEndpoint: TSimbaPackageEndpoint;
    FURL: String;
    FDisplayName: String;
    FName: String;
    FLoaded: Boolean;
    FVersions: TSimbaPackageVersions;
    FExampleFiles: TStringArray;
    FScriptFiles: TSTringArray;

    procedure ClearConfig;
    procedure WriteConfig(Key: String; Value: String);
    function ReadConfig(Key: String): String;

    procedure SetAutoUpdateEnabled(AValue: Boolean);
    procedure SetInstalledVersion(Value: String);
    procedure SetInstalledVersionTime(Value: TDateTime);
    procedure SetInstalledPath(Value: String);

    function GetExampleFiles: TStringArray;
    function GetScriptFiles: TStringArray;
    function GetAutoUpdateEnabled: Boolean;
    function GetInstalledPath: String;
    function GetInstalledVersion: String;
    function GetInstalledVersionTime: TDateTime;
    function GetLatestVersionTime: TDateTime;
    function GetLatestVersion: String;
  public
    constructor Create(AURL: String);
    destructor Destroy; override;

    function FindFilesWithStartingComment(Comment: String): TStringArray;

    procedure Load;

    function IsInstalled: Boolean;
    function UnInstall(RemoveFiles: Boolean): Boolean;

    function HasUpdate: Boolean;
    function HasVersions: Boolean;

    property DisplayName: String read FDisplayName;
    property Name: String read FName;
    property URL: String read FURL;
    property EndPoint: TSimbaPackageEndpoint read FEndpoint;
    property Versions: TSimbaPackageVersions read FVersions;

    property InstalledPath: String read GetInstalledPath write SetInstalledPath;
    property InstalledVersion: String read GetInstalledVersion write SetInstalledVersion;
    property InstalledVersionTime: TDateTime read GetInstalledVersionTime write SetInstalledVersionTime;
    property LatestVersion: String read GetLatestVersion;
    property LatestVersionTime: TDateTime read GetLatestVersionTime;
    property AutoUpdateEnabled: Boolean read GetAutoUpdateEnabled write SetAutoUpdateEnabled;

    property ExampleFiles: TStringArray read GetExampleFiles;
    property ScriptFiles: TStringArray read GetScriptFiles;
  end;
  TSimbaPackageArray = array of TSimbaPackage;

  function GetRemotePackageURLs: TStringArray;
  function GetLocalPackageURLs(OnlyInstalled: Boolean): TStringArray;

implementation

uses
  IniFiles, DateUtils,
  simba.env, simba.fs, simba.vartype_string,
  simba.ide_package_endpointgithub, simba.ide_package_endpointcustom;

function GetRemotePackageURLs: TStringArray;
begin
  try
    Result := URLFetch(SIMBA_PACKAGES_URL).Split(#10);
  except
    Result := [];
  end;
end;

function GetLocalPackageURLs(OnlyInstalled: Boolean): TStringArray;
var
  Sections: TStringList;
  I: Integer;
begin
  Sections := TStringList.Create();
  Sections.Duplicates := dupIgnore;

  with TIniFile.Create(SimbaEnv.PackagesPath + 'packages.ini') do
  try
    ReadSections(Sections);

    // Remove non installed entries
    for I := Sections.Count - 1 downto 0 do
      if OnlyInstalled and (not DirectoryExists(ReadString(Sections[I], 'InstalledPath', ''))) then
        Sections.Delete(I);
  finally
    Free();
  end;

  Result := Sections.ToStringArray();

  Sections.Free();
end;

procedure TSimbaPackageEndpoint.ParseTime(Str: String; out Time: TDateTime; out PrettyDays: String);
var
  Days: Integer;
begin
  Time := 0;
  PrettyDays := '';

  if (Str <> '') then
  begin
    if Str.IsInteger() then
      Time := Str.ToDateTime('unix', 0)
    else
      Time := Str.ToDateTime('iso8601', 0);

    if (Time = 0) then
      PrettyDays := '(unknown)'
    else
    begin
      Days := DaysBetween(Now(), Time);

      if (Days = 0) then
        PrettyDays := '(today)'
      else if (Days = 1) then
        PrettyDays := '(yesterday)'
      else
        PrettyDays := '(' + IntToStr(Days) + ' days ago)';
    end;
  end;
end;

function TSimbaPackageEndpoint.GetPage(URL: String): String;
begin
  Result := '';

  with TSimbaHTTPClient.Create() do
  try
    Result := Get(URL);
    if (ResponseStatus <> EHTTPStatus.OK) then
      Result := '';

    FLastHTTPStatus := ResponseStatus;
  finally
    Free();
  end;
end;

constructor TSimbaPackageEndpoint.Create(URL: String);
begin
  inherited Create;

  FURL := URL;
end;

procedure TSimbaPackageEndpoint.DeleteCache;
begin
  { nothing }
end;

function TSimbaPackage.GetLatestVersion: String;
begin
  Result := '';
  if HasVersions() then
    Result := FVersions[0].Name;
end;

function TSimbaPackage.GetLatestVersionTime: TDateTime;
begin
  Result := 0;
  if HasVersions() then
    Result := FVersions[0].Time;
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
    Result := TSimbaPath.PathIncludeTrailingSep(TSimbaPath.PathNormalize(Result));
end;

procedure TSimbaPackage.SetInstalledPath(Value: String);
begin
  WriteConfig('InstalledPath', Value);
end;

function TSimbaPackage.GetExampleFiles: TStringArray;
begin
  if (FExampleFiles = nil) then
    FExampleFiles := FindFilesWithStartingComment('!PACKAGE EXAMPLE');

  Result := FExampleFiles;
end;

function TSimbaPackage.GetScriptFiles: TStringArray;
begin
  if (FScriptFiles = nil) then
    FScriptFiles := FindFilesWithStartingComment('!PACKAGE SCRIPT');

  Result := FScriptFiles;
end;

procedure TSimbaPackage.SetInstalledVersion(Value: String);
begin
  WriteConfig('InstalledVersion', Value);
end;

constructor TSimbaPackage.Create(AURL: String);
begin
  inherited Create();

  FURL := AURL;
  FDisplayName := FURL.TrimRight(['/']);
  while (FDisplayName.Count('/') > 1) do
    FDisplayName := FDisplayName.After('/');
  FName := FDisplayName.After('/');

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

function TSimbaPackage.FindFilesWithStartingComment(Comment: String): TStringArray;
const
  COMMENT_PREFIX = UInt16(12079); // UInt16 representation of //
var
  FileName, Buffer: String;
begin
  Result := [];

  if IsInstalled() then
  begin
    SetLength(Buffer, 128);
    for FileName in TSimbaDir.DirSearch(InstalledPath, '*.simba', True) do
      with TFileStream.Create(FileName, fmOpenRead) do
      try
        if (Size > 2) and (ReadWord() = COMMENT_PREFIX) then
        begin
          FillChar(Buffer[1], Length(Buffer), #32);
          Read(Buffer[1], Min(Length(Buffer), Size));
          if (Comment in Buffer) then
            Result := Result + [FileName];
        end;
      finally
        Free();
      end;
  end;
end;

procedure TSimbaPackage.Load;
begin
  FLoaded := True;

  FVersions := FEndpoint.GetVersions();
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
    if RemoveFiles and TSimbaPath.PathIsInDir(InstalledPath, SimbaEnv.SimbaPath) then
      TSimbaDir.DirDelete(InstalledPath, False);

    ClearConfig();
  end;
end;

function TSimbaPackage.HasUpdate: Boolean;
begin
  Result := IsInstalled() and (InstalledVersionTime > 0) and (LatestVersionTime > InstalledVersionTime);
end;

function TSimbaPackage.HasVersions: Boolean;
begin
  Result := Length(FVersions) > 0;
end;

end.

