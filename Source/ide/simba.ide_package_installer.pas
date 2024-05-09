{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_package_installer;

{$i simba.inc}

interface

uses
  Classes, SysUtils, synedit, syncobjs,
  simba.base, simba.ide_package, simba.httpclient;

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
    FVersion: TSimbaPackageVersion;
    FHasRemoteInstallOpts: Boolean;
    FRemoteInstallOpts: TSimbaPackageInstallOptions;
    FOutput: TSynEdit;

    FProgressBufferLock: TCriticalSection;
    FProgressBuffer: TStringList;
    FProgressLast: String;

    FDownloadProgress, FExtractProgress: record
      Time: UInt64;
      Progress: String;
    end;

    function InternalInstall(URL: String; Path: String; IgnoreList: TStringArray; Flat: Boolean): Boolean;
    procedure FlushLog;
    procedure Log(S: String; Flush: Boolean = False);

    procedure DoConnectingProgress(Sender: TObject; URL: String);
    procedure DoDownloadingProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
    procedure DoExtractingProgress(Sender: TObject; FileName: String; Pos, Size: Int64);
    procedure DoResponseStatus(Sender: TObject; Code: EHTTPStatus);

    procedure DoDownloadingFinished(Sender: TObject);
    procedure DoExtractingFinished(Sender: TObject);

    procedure SetVersion(Value: TSimbaPackageVersion);
  public
    OnStartInstall: TNotifyEvent;
    OnEndInstall: TNotifyEvent;

    constructor Create(Package: TSimbaPackage; Output: TSynEdit);
    destructor Destroy; override;

    property Version: TSimbaPackageVersion read FVersion write SetVersion;

    property HasRemoteInstallOpts: Boolean read FHasRemoteInstallOpts;
    property RemoteInstallOpts: TSimbaPackageInstallOptions read FRemoteInstallOpts;

    function Install(Opts: TSimbaPackageInstallOptions): Boolean;
  end;

implementation

uses
  Forms, FileUtil,
  simba.ide_utils, simba.env, simba.threading, simba.fs;

function TSimbaPackageInstaller.InternalInstall(URL: String; Path: String; IgnoreList: TStringArray; Flat: Boolean): Boolean;

  procedure DoInstall;
  begin
    with TSimbaHTTPClient.Create() do
    try
      OnConnecting       := @DoConnectingProgress;
      OnDownloadProgress := @DoDownloadingProgress;
      OnExtractProgress  := @DoExtractingProgress;
      OnResponseStatus   := @DoResponseStatus;

      OnDownloadingFinished := @DoDownloadingFinished;
      OnExtractingFinished  := @DoExtractingFinished;

      GetZIP(URL, Path, Flat, IgnoreList);
    finally
      Free();
    end;
  end;

  procedure DoIdle;
  begin
    FlushLog();

    Sleep(100);
  end;

var
  FileName, ExceptionMsg: String;
begin
  Result := False;
  if Assigned(OnStartInstall) then
    OnStartInstall(Self);

  // Move old files to Data/Temp/
  if DirectoryExists(Path) then
  begin
    FileName := SimbaEnv.TempPath + ExtractFileName(Path);
    if DirectoryExists(FileName) then
      DeleteDirectory(FileName, False);

    Log('Moving old files to %s'.Format([FileName]), True);
    if (not RenameFile(Path, FileName)) then
    begin
      Log('Unable to move old files %s'.Format([Path]), True);
      Log('Please delete the files manually', True);

      Exit;
    end;
    Log('', True);
  end;

  ExceptionMsg := Application.RunInThreadAndWait(@DoInstall, @DoIdle);
  if (ExceptionMsg = '') then
    Log('Succesfully installed!', True)
  else
    Log('Installing failed: ' + ExceptionMsg, True);

  Result := ExceptionMsg = '';

  if Assigned(OnEndInstall) then
    OnEndInstall(Self);
end;

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
  RunInMainThread(@Execute);
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

procedure TSimbaPackageInstaller.DoExtractingProgress(Sender: TObject; FileName: String; Pos, Size: Int64);
begin
  FExtractProgress.Progress := 'Extracting: %f%%'.Format([Pos / Size * 100.0]);
  if (GetTickCount64() - FExtractProgress.Time) < 500 then
    Exit;
  FExtractProgress.Time := GetTickCount64();

  Log(FExtractProgress.Progress);
end;

procedure TSimbaPackageInstaller.DoResponseStatus(Sender: TObject; Code: EHTTPStatus);
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

procedure TSimbaPackageInstaller.SetVersion(Value: TSimbaPackageVersion);

  procedure GetRemoteInstallOpts;
  var
    Strings: TStringList;

    procedure Run;
    begin
      with TSimbaHTTPClient.Create() do
      try
        Strings.Text := Get(FVersion.OptionsURL);
        if (ResponseStatus <> EHTTPStatus.OK) then
          Strings.Text := '';
      finally
        Free();
      end;
    end;

  var
    I: Integer;
  begin
    Strings := TStringList.Create();

    Application.RunInThreadAndWait(@Run);

    FHasRemoteInstallOpts := Strings.Count > 0;
    if FHasRemoteInstallOpts then
    begin
      FRemoteInstallOpts := Default(TSimbaPackageInstallOptions);
      FRemoteInstallOpts.Path := Strings.Values['path'];
      FRemoteInstallOpts.Flat := Strings.Values['flat'] = 'true';
      FRemoteInstallOpts.AutoUpdate := Strings.Values['autoupdate'] = 'true';
      for I := 0 to Strings.Count - 1 do
        if (Strings.Names[I] = 'ignore') then
          FRemoteInstallOpts.IgnoreList += [Strings.ValueFromIndex[I]];

      if (FRemoteInstallOpts.Path <> '') then // make relative
        FRemoteInstallOpts.Path := TSimbaPath.PathJoin([SimbaEnv.SimbaPath, TSimbaPath.PathSetSeperators(FRemoteInstallOpts.Path)])
      else // no path: default to includes under package name
        FRemoteInstallOpts.Path := TSimbaPath.PathJoin([SimbaEnv.IncludesPath, FPackage.Name]);
    end;

    Strings.Free();
  end;

begin
  FVersion := Value;

  FHasRemoteInstallOpts := False;
  if (FVersion.OptionsURL <> '') then
    GetRemoteInstallOpts();
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

function TSimbaPackageInstaller.Install(Opts: TSimbaPackageInstallOptions): Boolean;
begin
  Log('Installing: %s'.Format([FPackage.DisplayName]));
  Log('Version: %s'.Format([Version.Name]));
  Log('Path: %s'.Format([Opts.Path]));
  Log('', True);

  Result := InternalInstall(Version.DownloadURL, Opts.Path, Opts.IgnoreList, Opts.Flat);
  if Result then
  begin
    FPackage.InstalledVersion     := Version.Name;
    FPackage.InstalledPath        := Opts.Path;

    if Version.IsBranch then
      FPackage.InstalledVersionTime := 0
    else
      FPackage.InstalledVersionTime := Version.Time;

    FPackage.AutoUpdateEnabled    := Opts.AutoUpdate;
  end;

  Log('', True);
end;

end.

