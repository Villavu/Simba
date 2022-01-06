{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package;

{$i simba.inc}

interface

uses
  classes, sysutils, fgl,
  simba.mufasatypes, simba.package_endpoint;

type
  TSimbaPackage = class;
  TSimbaPackageList = specialize TFPGObjectList<TSimbaPackage>;

  TSimbaPackageOptions = record
    Name: String;
    Directory: String;
    Flat: Boolean;
    IgnoreList: String;
    Scripts: String;
    Examples: String;

    function IsEmpty: Boolean;
  end;

  TSimbaPackageRelease = class
  protected
    FPackage: TSimbaPackage;

    FBranch: Boolean;
    FVersion: String;
    FNotes: String;
    FTime: TDateTime;
    FTimeStamp: String;
    FDownloadURL: String;
    FOptionsURL: String;

    FOptions: TSimbaPackageOptions;
    FOptionsCached: Boolean;

    function GetOptions: TSimbaPackageOptions;
  public
    // Release
    constructor Create(Package: TSimbaPackage; const Version, TimeStamp, Notes, DownloadURL, OptionsURL: String); overload;
    // Branch
    constructor Create(Package: TSimbaPackage; const Version, DownloadURL, OptionsURL: String); overload;

    property Branch: Boolean read FBranch;
    property Version: String read FVersion;
    property Notes: String read FNotes;
    property Time: TDateTime read FTime;
    property TimeStamp: String read FTimeStamp;
    property DownloadURL: String read FDownloadURL;
    property OptionsURL: String read FOptionsURL;

    property Options: TSimbaPackageOptions read GetOptions;
  end;

  TSimbaPackageReleaseList = specialize TFPGObjectList<TSimbaPackageRelease>;

  TSimbaPackage = class
  public
    FName: String;
    FURL: String;
    FReleases: TSimbaPackageReleaseList;
    FCacheFileName: String;
    FCacheHash: String;

    FInstalledName: String;
    FInstalledPath: String;
    FInstalledScripts: String;
    FInstalledExamples: String;
    FInstalledVersion: String;
    FInstalledVersionTime: TDateTime;

    FEndpoint: TSimbaPackageEndpoint;

    function GetCacheAge: Integer;
    function GetInstalledScripts: TStringArray;
    function GetInstalledExamples: TStringArray;
  public
    property Endpoint: TSimbaPackageEndpoint read FEndpoint;
    property Name: String read FName;
    property Releases: TSimbaPackageReleaseList read FReleases;
    property CacheAge: Integer read GetCacheAge;

    function LoadReleases(UseCache: Boolean): Boolean;

    function Install(Release: TSimbaPackageRelease; Path: String; Flat: Boolean; IgnoreList: String): Boolean;

    procedure Load;
    procedure Save;
    procedure Remove;

    property InstalledExamples: TStringArray read GetInstalledExamples;
    property InstalledScripts: TStringArray read GetInstalledScripts;
    property InstalledVersion: String read FInstalledVersion;
    property InstalledVersionTime: TDateTime read FInstalledVersionTime;
    property InstalledPath: String read FInstalledPath;
    property InstalledName: String read FInstalledName;

    constructor Create(URL: String);
    destructor Destroy; override;
  end;

  function LoadPackages(FreeObjects: Boolean = True): TSimbaPackageList;

implementation

uses
  fileutil, dateutils, lazloggerbase, inifiles, jsonparser, fpjson, sha1,
  simba.files, simba.helpers_string;

function LoadPackages(FreeObjects: Boolean = True): TSimbaPackageList;
var
  Sections: TStringList;
  Section: String;
  URL: String;
begin
  Result := TSimbaPackageList.Create(FreeObjects);

  Sections := TStringList.Create();
  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      ReadSections(Sections);

      for Section in Sections do
      begin
        URL := ReadString(Section, 'URL', '');
        if (URL = '') then
          Continue;

        Result.Add(TSimbaPackage.Create(URL));
      end;
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[LoadPackages]: Exception "%s"', [E.Message]);
  end;

  if (Sections <> nil) then
    Sections.Free();
end;

function TSimbaPackageOptions.IsEmpty: Boolean;
begin
  Result := CompareMem(@Self, @Default(TSimbaPackageOptions), SizeOf(TSimbaPackageOptions));
end;

function TSimbaPackageRelease.GetOptions: TSimbaPackageOptions;
begin
  if (not FOptionsCached) then
  begin
    FOptionsCached := True;

    with TStringList.Create() do
    try
      Text := FPackage.Endpoint.GetPage(FOptionsURL);

      if (Text <> '') then
      begin
        FOptions.Name       := Values['name'];
        FOptions.Directory  := Values['directory'];
        FOptions.IgnoreList := Values['ignore'];
        FOptions.Examples   := Values['examples'];
        FOptions.Scripts    := Values['scripts'];
        FOptions.Flat       := Values['flat'].ToLower() = 'true';
      end;
    finally
      Free();
    end;
  end;

  Result := FOptions;
end;

constructor TSimbaPackageRelease.Create(Package: TSimbaPackage; const Version, TimeStamp, Notes, DownloadURL, OptionsURL: String);
begin
  FPackage := Package;

  FVersion := Version;
  FTimeStamp := TimeStamp;
  FTime := ISO8601ToDateDef(FTimeStamp, 0, False);
  FNotes := Notes;
  FDownloadURL := DownloadURL;
  FOptionsURL := OptionsURL;
end;

constructor TSimbaPackageRelease.Create(Package: TSimbaPackage; const Version, DownloadURL, OptionsURL: String);
begin
  FPackage := Package;

  FBranch := True;
  FVersion := Version;
  FDownloadURL := DownloadURL;
  FOptionsURL := OptionsURL;
end;

function TSimbaPackage.Install(Release: TSimbaPackageRelease; Path: String; Flat: Boolean; IgnoreList: String): Boolean;
begin
  Result := False;

  with Release do
  begin
    Result := FEndpoint.GetZip(DownloadURL, Path, Flat, IgnoreList.Split(','));

    if Result then
    begin
      FInstalledName        := FName;
      FInstalledPath        := Path;
      FInstalledVersion     := Version;
      FInstalledVersionTime := Time;
      FInstalledScripts     := Options.Scripts;
      FInstalledExamples    := Options.Examples;

       if (Options.Name <> '') then
        FInstalledName := Options.Name;

      Save();
    end;
  end;
end;

procedure TSimbaPackage.Load;
var
  Section: String;
begin
  try
    Section := SHA1Print(SHA1String(FURL));

    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      if not SectionExists(Section) then
        Exit;

      FInstalledName        := ReadString(Section, 'Name', '');
      FInstalledPath        := ReadString(Section, 'Path', '');
      FInstalledScripts     := ReadString(Section, 'Scripts', '');
      FInstalledExamples    := ReadString(Section, 'Examples', '');
      FInstalledVersion     := ReadString(Section, 'InstalledVersion', '');
      FInstalledVersionTime := ReadDateTime(Section, 'InstalledVersionTime', 0);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaPackage.Load]: Exception "%s"', [E.Message]);
  end;
end;

procedure TSimbaPackage.Save;
var
  Section: String;
begin
  try
    Section := SHA1Print(SHA1String(FURL));

    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      CacheUpdates := True;

      WriteString(Section, 'URL', FURL);
      WriteString(Section, 'Name', FInstalledName);
      WriteString(Section, 'Path', FInstalledPath);
      WriteString(Section, 'Scripts', FInstalledScripts);
      WriteString(Section, 'Examples', FInstalledExamples);
      WriteString(Section, 'InstalledVersion', FInstalledVersion);
      WriteDateTime(Section, 'InstalledVersionTime', FInstalledVersionTime);
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaPackage.Save]: Exception "%s"', [E.Message]);
  end;
end;

procedure TSimbaPackage.Remove;
begin
  try
    with TIniFile.Create(GetPackagePath() + 'packages.ini') do
    try
      EraseSection(SHA1Print(SHA1String(FURL)));
    finally
      Free();
    end;
  except
    on E: Exception do
      DebugLn('[TSimbaPackage.Remove]: Exception "%s"', [E.Message]);
  end;
end;

function TSimbaPackage.GetInstalledScripts: TStringArray;

  function GetFiles(FileName: String): TStringArray;
  begin
    if DirectoryExists(FileName) then
      Result := FindFiles([FileName], '*.simba')
    else
    if FileExists(FileName) then
      Result := [FileName];
  end;

var
  I, J: Integer;
  FileName: String;
  Scripts, FileNames: TStringArray;
begin
  Result := [];

  Scripts := FInstalledScripts.Split(',');
  for I := 0 to High(Scripts) do
  begin
    FileName := Scripts[I].After('=');
    if (FileName = '') then
      FileName := Scripts[I];
    FileName := ConcatPaths([FInstalledPath, FileName]);

    FileNames := GetFiles(FileName);
    for J := 0 to High(FileNames) do
      Result := Result + [Scripts[I].Before('='), FileNames[J]];
  end;
end;

function TSimbaPackage.GetInstalledExamples: TStringArray;
var
  I: Integer;
  FileName: String;
  Scripts: TStringArray;
begin
  Result := [];

  Scripts := FInstalledExamples.Split(',');
  for I := 0 to High(Scripts) do
  begin
    FileName := ConcatPaths([FInstalledPath, Scripts[I]]);

    if DirectoryExists(FileName) then
      Result := Result + FindFiles([FileName], '*.simba')
    else
    if FileExists(FileName) then
      Result := Result + [FileName];
  end;
end;

function TSimbaPackage.GetCacheAge: Integer;
var
  DateTime: TDateTime;
begin
  Result := 0;
  if FileAge(FCacheFileName, DateTime) then
    Result := MinutesBetween(Now(), DateTime);
end;

function TSimbaPackage.LoadReleases(UseCache: Boolean): Boolean;

  function AddReleases(const Data: String): Boolean;
  var
    JSON: TJSONData;
    I: Integer;
    Zip, Notes, Time, Version, Branch, OptionsURL: TJSONData;
    Options: String;
  begin
    JSON := nil;

    try
      JSON := GetJSON(Data);

      if (JSON is TJSONArray) then
      begin
        for I := 0 to JSON.Count - 1 do
          if (JSON.Items[I] is TJSONObject) then
          begin
            with TJSONObject(JSON.Items[I]) do
            begin
              Options := '';
              if Find('options', OptionsURL) then
                Options := OptionsURL.AsString;

              if Find('branch', Branch) and Find('zip', Zip) then
                FReleases.Add(TSimbaPackageRelease.Create(Self, Branch.AsString, Zip.AsString, Options))
              else
              if Find('zip', Zip) and Find('notes', Notes) and Find('time', Time) and Find('version', Version) then
                FReleases.Add(TSimbaPackageRelease.Create(Self, Version.AsString, Time.AsString, Notes.AsString, Zip.AsString, Options));
            end;
         end;
      end;
    except
      on E: Exception do
        DebugLn('[TSimbaPackage.LoadReleases]: Exception "%s"', [E.Message]);
    end;

    if (JSON <> nil) then
      JSON.Free();

    Result := FReleases.Count > 0;
  end;

  function LoadCache: Boolean;
  begin
    Result := (GetCacheAge() < 60) and AddReleases(ReadFile(FCacheFileName));
    if Result then
      FCacheHash := HashFile(FCacheFileName);
  end;

  procedure UpdateCache(const Data: String);
  begin
    WriteFile(FCacheFileName, Data);

    FCacheHash := HashFile(FCacheFileName);
  end;

  function CacheUnchanged: Boolean;
  begin
    Result := (FCacheHash = HashFile(FCacheFileName));
  end;

var
  Data: String;
begin
  if UseCache and CacheUnchanged then
    Exit(True);

  FReleases.Clear();

  if UseCache and LoadCache() then
    Exit(True);

  Data := FEndpoint.GetReleasesJSON();

  Result := AddReleases(Data);
  if Result then
    UpdateCache(Data);
end;

constructor TSimbaPackage.Create(URL: String);

  function IsGithubShorthand: Boolean;
  begin
    Result := (URL.Count('/') = 1) and (URL.Count('.') = 0); // "ollydev/simba"
  end;

  function IsGithub: Boolean;
  begin
    Result := URL.Contains('github.com', False) or IsGithubShorthand();
  end;

begin
  FURL := URL;

  if IsGithub() then
  begin
    if IsGithubShorthand() then
      FURL := 'https://github.com/' + FURL;

    FEndpoint := TSimbaPackageEndpoint_Github.Create(FURL);
    FName := TSimbaPackageEndpoint_Github(FEndpoint).Owner + '/' + TSimbaPackageEndpoint_Github(FEndpoint).Name;
  end else
  begin
    FEndpoint := TSimbaPackageEndpoint.Create(FURL);

    FName := FURL;
    FName := FName.Replace('http://', '');
    FName := FName.Replace('https://', '');
    FName := FName.After('/');
    if (FName = '') then
      FName := FURL;
  end;

  FReleases := TSimbaPackageReleaseList.Create();
  FCacheFileName := GetPackagePath() + SHA1Print(SHA1String(FURL));

  Load();
end;

destructor TSimbaPackage.Destroy;
begin
  if (FEndpoint <> nil) then
    FreeAndNil(FEndpoint);
  if (FReleases <> nil) then
    FreeAndNil(FReleases);

  inherited Destroy();
end;

end.

