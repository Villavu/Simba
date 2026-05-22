{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Checks for new package versions and updates if auto-update is enabled.
  Once finished posts ESimbaEvent.PACKAGE_INSTALLS_CHANGED
  Is also ran on package form close to post above event.
}
unit simba.ide_package_autoupdater;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ExtCtrls,
  simba.base,
  simba.ide_events;

type
  TPackageAutoUpdater = class(TComponent)
  protected
    FTimer: TTimer;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoTimer(Sender: TObject);
  public
    constructor Create; reintroduce;

    procedure Run;
  end;

var
  PackageAutoUpdater: TPackageAutoUpdater;

implementation

uses
  simba.initializations,
  simba.ide_package,
  simba.ide_package_installer;

type
  TPackageUpdater = class(TThread)
  protected
    FPackages: TSimbaPackageArray;
    FDelay: Integer;

    procedure DoTerminated(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create(Delay: Integer = 0); reintroduce;
    destructor Destroy; override;
  end;

procedure TPackageAutoUpdater.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.PACKAGE_FORM_CLOSED:
      Run();
  end;
end;

procedure TPackageAutoUpdater.DoTimer(Sender: TObject);
begin
  Run();
end;

constructor TPackageAutoUpdater.Create;
begin
  inherited Create(nil);

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @DoTimer;
  FTimer.Interval := 60000 * 10;
  FTimer.Enabled := True;

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.PACKAGE_FORM_CLOSED]);

  // Run on create, in a few seconds
  TPackageUpdater.Create(2500);
end;

procedure TPackageAutoUpdater.Run;
begin
  TPackageUpdater.Create();
end;

procedure TPackageUpdater.DoTerminated(Sender: TObject);
begin
  if (FatalException <> nil) then
    DebugLn('Package updating exception: ' + Exception(FatalException).Message);

  SimbaEvents.Post(ESimbaEvent.PACKAGE_INSTALLS_CHANGED, Pointer(FPackages));
end;

procedure TPackageUpdater.Execute;
var
  URLs: TStringArray;
  I: Integer;
  Package: TSimbaPackage;
  InstallOpts: TSimbaPackageInstallOptions;
begin
  if (FDelay > 0) then
    Sleep(FDelay);

  URLs := GetLocalPackageURLs(True);

  SetLength(FPackages, Length(URLs));
  for I := 0 to High(FPackages) do
  begin
    FPackages[I] := TSimbaPackage.Create(URLs[I]);
    FPackages[I].Load();
  end;

  for I := 0 to High(FPackages) do
  begin
    Package := FPackages[I];

    if Package.HasUpdate() and Package.AutoUpdateEnabled then
    begin
      DebugLn(DEBUG_YELLOW + 'Automatically updating ' + Package.Name + DEBUG_RESET);
      DebugLn(DEBUG_FOCUS);

      try
        with TSimbaPackageInstaller.Create(Package) do
        try
          Version := Package.Versions[0];

          if HasRemoteInstallOpts then
            InstallOpts := RemoteInstallOpts
          else
          begin
            // I guess we can auto update this way too...
            InstallOpts := Default(TSimbaPackageInstallOptions);
            InstallOpts.Path := Package.InstalledPath;
          end;

          if Install(InstallOpts) then
          begin
            DebugLn(DEBUG_GREEN + 'Succesfully updated "%s"' + DEBUG_RESET, [Package.Name]);
            DebugLn(DEBUG_GREEN + 'Now at version: %s' + DEBUG_RESET, [Package.InstalledVersion]);
          end else
            DebugLn(DEBUG_RED + 'Failed to update: %s' + DEBUG_RESET, [Package.Name]);
        finally
          Free();
        end;
      except
        on E: Exception do
          DebugLn(DEBUG_RED + 'Failed to update: %s (%s)' + DEBUG_RESET, [Package.Name, E.Message]);
      end;
    end;
  end;

  // find in this thread - these are cached.
  for I := 0 to High(FPackages) do
  begin
    FPackages[I].ScriptFiles;
    FPackages[I].ExampleFiles;
  end;
end;

constructor TPackageUpdater.Create(Delay: Integer);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;
  OnTerminate := @DoTerminated;

  FDelay := Delay;
end;

destructor TPackageUpdater.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FPackages) do
    FPackages[I].Free();

  inherited Destroy();
end;

procedure DoCreate;
begin
  PackageAutoUpdater := TPackageAutoUpdater.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(PackageAutoUpdater);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'PackageAutoUpdater');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'PackageAutoUpdater');

end.

