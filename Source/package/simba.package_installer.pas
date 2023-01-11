unit simba.package_installer;

{$i simba.inc}

interface

uses
  Classes, SysUtils, synedit, syncobjs,
  simba.package;

type
  TSimbaPackageInstallOptions = record
    Path: String;
    IgnoreList: TStringArray;
    Flat: Boolean;
    AutoUpdate: Boolean;
  end;

  TSimbaPackageInstaller = class
  protected
    FPackage: TSimbaPackage;
    FOutput: TSynEdit;

    FProgressBufferLock: TCriticalSection;
    FProgressBuffer: TStringList;
    FProgressLast: String;

    FDownloadProgress, FExtractProgress: record
      Time: UInt64;
      Progress: String;
    end;

    procedure FlushLog;
    procedure Log(S: String; Flush: Boolean = False);

    procedure DoConnectingProgress(Sender: TObject; URL: String);
    procedure DoDownloadingProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
    procedure DoExtractingProgress(Sender: TObject; FileName: String; Percent: Double);
    procedure DoResponseCode(Sender: TObject; Code: Integer);

    procedure DoDownloadingFinished(Sender: TObject);
    procedure DoExtractingFinished(Sender: TObject);
  public
    constructor Create(Package: TSimbaPackage; Output: TSynEdit);
    destructor Destroy; override;

    function GetOptions(Version: TSimbaPackageVersion; out Options: TSimbaPackageInstallOptions): Boolean;
    function Install(Version: TSimbaPackageVersion; Options: TSimbaPackageInstallOptions): Boolean;
    function InstallLatestVersion: Boolean;
  end;

implementation

uses
  Forms, FileUtil,
  simba.mufasatypes, simba.httpclient, simba.files;

procedure TSimbaPackageInstaller.FlushLog;

  procedure Execute;
  var
    I: Integer;
  begin
    FProgressBufferLock.Enter();

    try
      if (FProgressBuffer.Count > 0) then
      begin
        FOutput.BeginUpdate(False);
        for I := 0 to FProgressBuffer.Count - 1 do
          FOutput.Append(FProgressBuffer[I]);
        FOutput.CaretXY := TPoint.Create(0, FOutput.Lines.Count);
        FOutput.EndUpdate();

        FProgressBuffer.Clear();
      end;
    finally
      FProgressBufferLock.Leave();
    end;

    Application.ProcessMessages();
  end;

begin
  Sync(@Execute);
end;

procedure TSimbaPackageInstaller.Log(S: String; Flush: Boolean);
begin
  FProgressBufferLock.Enter();
  try
    if (FProgressLast <> S) then
      FProgressBuffer.Add(S);

    FProgressLast := S;
  finally
    FProgressBufferLock.Leave();
  end;

  if Flush then
    FlushLog();
end;

procedure TSimbaPackageInstaller.DoConnectingProgress(Sender: TObject; URL: String);
begin
  Log('Connecting to %s'.Format([URL]));
end;

procedure TSimbaPackageInstaller.DoDownloadingProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
begin
  if (ContentType <> 'application/zip') then
    Exit;

  if (Size > 0) then
    FDownloadProgress.Progress := 'Downloading: %f / %f MB'.Format([Pos / (1024 * 1024), Size / (1024 * 1024)])
  else
    FDownloadProgress.Progress := 'Downloading: %f MB'.Format([Pos / (1024 * 1024)]);

  if (GetTickCount64() - FDownloadProgress.Time) < 500 then
    Exit;
  FDownloadProgress.Time := GetTickCount64();

  Log(FDownloadProgress.Progress);
end;

procedure TSimbaPackageInstaller.DoExtractingProgress(Sender: TObject; FileName: String; Percent: Double);
begin
  FExtractProgress.Progress := 'Extracting: %d%%'.Format([Round(Percent)]);
  if (GetTickCount64() - FExtractProgress.Time) < 500 then
    Exit;
  FExtractProgress.Time := GetTickCount64();

  Log(FExtractProgress.Progress);
end;

procedure TSimbaPackageInstaller.DoResponseCode(Sender: TObject; Code: Integer);
begin
  Log('HTTP response: %d'.Format([Code]));
end;

procedure TSimbaPackageInstaller.DoDownloadingFinished(Sender: TObject);
begin
  Log(FDownloadProgress.Progress);
end;

procedure TSimbaPackageInstaller.DoExtractingFinished(Sender: TObject);
begin
  Log(FExtractProgress.Progress);
end;

constructor TSimbaPackageInstaller.Create(Package: TSimbaPackage; Output: TSynEdit);
begin
  inherited Create();

  FProgressBufferLock := TCriticalSection.Create();
  FProgressBuffer := TStringList.Create();

  FPackage := Package;
  FOutput := Output;
end;

destructor TSimbaPackageInstaller.Destroy;
begin
  if (FProgressBufferLock <> nil) then
    FreeAndNil(FProgressBufferLock);
  if (FProgressBuffer <> nil) then
    FreeAndNil(FProgressBuffer);

  inherited Destroy();
end;

function TSimbaPackageInstaller.GetOptions(Version: TSimbaPackageVersion; out Options: TSimbaPackageInstallOptions): Boolean;
var
  Strings: TStringList;

  procedure Run;
  begin
    with TSimbaHTTPClient.Create() do
    try
      Strings.Text := Get(Version.OptionsURL, []);
      if (ResponseCode <> HTTP_OK) then
        Strings.Text := '';
    finally
      Free();
    end;
  end;

var
  Thread: TThread;
  I: Integer;
begin
  Options := Default(TSimbaPackageInstallOptions);

  Strings := TStringList.Create();
  Thread := Threaded(@Run);
  while (not Thread.Finished) do
  begin
    if (GetCurrentThreadID() = MainThreadID) then
      Application.ProcessMessages();

    Sleep(500);
  end;

  Result := Strings.Count > 0;
  if Result then
  begin
    Options.Path := Strings.Values['path'];
    Options.Flat := Strings.Values['flat'] = 'true';
    Options.AutoUpdate := Strings.Values['autoupdate'] = 'true';
    for I := 0 to Strings.Count - 1 do
      if (Strings.Names[I] = 'ignore') then
        Options.IgnoreList += [Strings.ValueFromIndex[I]];
  end;

  if (Options.Path = '') then
    Options.Path := ConcatPaths([GetIncludePath(), FPackage.Info.Name])
  else
    Options.Path := ConcatPaths([GetSimbaPath(), SetDirSeparators(Options.Path)]);

  Thread.Free();
  Strings.Free();
end;

function TSimbaPackageInstaller.Install(Version: TSimbaPackageVersion; Options: TSimbaPackageInstallOptions): Boolean;

  procedure Run;
  begin
    with TSimbaHTTPClient.Create() do
    try
      OnConnecting       := @DoConnectingProgress;
      OnDownloadProgress := @DoDownloadingProgress;
      OnExtractProgress  := @DoExtractingProgress;
      OnResponseCode     := @DoResponseCode;

      OnDownloadingFinished := @DoDownloadingFinished;
      OnExtractingFinished  := @DoExtractingFinished;

      GetZIP(Version.DownloadURL, Options.Path, Options.Flat, Options.IgnoreList);
    finally
      Free();
    end;
  end;

var
  Thread: TThread;
  FileName: String;
begin
  Result := False;

  Log('Installing: %s'.Format([FPackage.Info.FullName]));
  Log('Version: %s'.Format([Version.Name]));
  Log('Path: %s'.Format([Options.Path]));
  Log('', True);

  if DirectoryExists(Options.Path) then
  begin
    FileName := GetOldPackagePath() + ExtractFileName(Options.Path);
    if DirectoryExists(FileName) then
      DeleteDirectory(FileName, False);

    Log('Moving old files to %s'.Format([FileName]), True);
    if (not RenameFile(Options.Path, FileName)) then
    begin
      Log('Unable to move old files %s'.Format([Options.Path]), True);
      Log('Please delete the files manually', True);

      Exit;
    end;
    Log('', True);
  end;

  Thread := Threaded(@Run);
  while (not Thread.Finished) do
  begin
    FlushLog();

    Sleep(100);
  end;

  Log('', True);

  Result := Thread.FatalException = nil;
  if Result then
  begin
    Log('Succesfully installed!', True);

    FPackage.InstalledVersion     := Version.Name;
    FPackage.InstalledVersionTime := Version.Time;
    FPackage.InstalledPath        := Options.Path;
    FPackage.AutoUpdateEnabled    := Options.AutoUpdate;
  end else
    Log('Installing failed: %s'.Format([Thread.FatalException.ToString()]), True);

  Thread.Free();
end;

function TSimbaPackageInstaller.InstallLatestVersion: Boolean;
var
  Version: TSimbaPackageVersion;
  Options: TSimbaPackageInstallOptions;
begin
  if FPackage.HasVersions() then
  begin
    Version := FPackage.Versions[0];

    Result := GetOptions(Version, Options) and Install(Version, Options);
  end;
end;

end.

