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
  end;

  TSimbaPackageInstaller = class
  protected
    FPackage: TSimbaPackage;
    FOutput: TSynEdit;

    FProgressBufferLock: TCriticalSection;
    FProgressBuffer: TStringList;
    FProgressLast: String;

    procedure FlushProgress;
    procedure Progress(S: String);
    procedure Log(S: String);

    procedure DoConnectingProgress(Sender: TObject; URL: String);
    procedure DoDownloadingProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
    procedure DoExtractingProgress(Sender: TObject; FileName: String; Percent: Double);
    procedure DoCopyingProgress(Sender: TObject; FileName: String; Percent: Double);
    procedure DoResponseCode(Sender: TObject; Code: Integer);
  public
    constructor Create(Package: TSimbaPackage; Output: TSynEdit);
    destructor Destroy; override;

    function GetOptions(Version: TSimbaPackageVersion; out Options: TSimbaPackageInstallOptions): Boolean;
    function Install(Version: TSimbaPackageVersion; Options: TSimbaPackageInstallOptions): Boolean;
  end;

implementation

uses
  forms,
  simba.mufasatypes, simba.httpclient, simba.helpers_string, simba.files;

procedure TSimbaPackageInstaller.FlushProgress;
var
  I: Integer;
begin
  FProgressBufferLock.Enter();

  try
    FOutput.BeginUpdate(False);
    for I := 0 to FProgressBuffer.Count - 1 do
      FOutput.Append(FProgressBuffer[I]);
    FOutput.CaretXY := TPoint.Create(0, FOutput.Lines.Count);
    FOutput.EndUpdate();

    FProgressBuffer.Clear();
  finally
    FProgressBufferLock.Leave();
  end;

  Application.ProcessMessages();
end;

procedure TSimbaPackageInstaller.Progress(S: String);
begin
  FProgressBufferLock.Enter();
  try
    if (FProgressLast <> S) then
      FProgressBuffer.Add(S);

    FProgressLast := S;
  finally
    FProgressBufferLock.Leave();
  end;
end;

procedure TSimbaPackageInstaller.Log(S: String);
begin
  FOutput.Append(S);
  FOutput.CaretXY := TPoint.Create(0, FOutput.Lines.Count);
end;

procedure TSimbaPackageInstaller.DoConnectingProgress(Sender: TObject; URL: String);
begin
  Progress('Connecting to %s'.Format([URL]));
end;

procedure TSimbaPackageInstaller.DoDownloadingProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
begin
  if (ContentType <> 'application/zip') then
    Exit;

  if (Size > 0) then
    Progress('Downloading: %f / %f MB'.Format([Pos / (1024 * 1024), Size / (1024 * 1024)]))
  else
    Progress('Downloading: %f MB'.Format([Pos / (1024 * 1024)]));
end;

procedure TSimbaPackageInstaller.DoExtractingProgress(Sender: TObject; FileName: String; Percent: Double);
begin
  Progress('Extracting: %d%%'.Format([Round(Percent)]));
end;

procedure TSimbaPackageInstaller.DoCopyingProgress(Sender: TObject; FileName: String; Percent: Double);
begin
  Progress('Copying: %d%%'.Format([Round(Percent)]));
end;

procedure TSimbaPackageInstaller.DoResponseCode(Sender: TObject; Code: Integer);
begin
  Progress('HTTP response: %d'.Format([Code]));
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
    Application.ProcessMessages();

    Sleep(50);
  end;

  Result := Strings.Count > 0;
  if Result then
  begin
    Options.Path       := Strings.Values['path'];
    Options.Flat       := Strings.Values['flat'] = 'true';
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
      OnCopyingProgress  := @DoCopyingProgress;
      OnResponseCode     := @DoResponseCode;

      GetZIP(Version.DownloadURL, Options.Path, Options.Flat, Options.IgnoreList);
    finally
      Free();
    end;
  end;

var
  Thread: TThread;
begin
  FOutput.Clear();

  Log('Installing: %s'.Format([FPackage.Info.FullName]));
  Log('Version: %s'.Format([Version.Name]));
  Log('Path: %s'.Format([Options.Path]));
  Log('');

  Thread := Threaded(@Run);
  while (not Thread.Finished) do
  begin
    FlushProgress();

    Sleep(50);
  end;

  FlushProgress();
  Log('');

  Result := Thread.FatalException = nil;

  if Result then
  begin
    Log('Succesfully installed!');

    FPackage.InstalledVersion     := Version.Name;
    FPackage.InstalledVersionTime := Version.Time;
    FPackage.InstalledPath        := Options.Path;
  end else
    Log('Installing failed: %s'.Format([Thread.FatalException.ToString()]));

  Thread.Free();
end;

end.

