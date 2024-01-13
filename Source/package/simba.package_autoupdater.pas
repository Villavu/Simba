{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_autoupdater;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls;

type
  TPackageAutoUpdater = class(TComponent)
  protected
    FTimer: TTimer;

    procedure DoPackageFormClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoTimer(Sender: TObject);
  public
    constructor Create; reintroduce;

    procedure Run;
  end;

var
  PackageAutoUpdater: TPackageAutoUpdater;

implementation

uses
  simba.base, simba.package, simba.package_installer,
  simba.main, simba.outputform, simba.package_menubuilder, simba.threading,
  simba.ide_maintoolbar, simba.ide_initialization, simba.package_form;

type
  TPackageUpdater = class(TThread)
  protected
    FPackages: TSimbaPackageArray;
    FUpdates: TStringList;

    procedure DoTerminateOnMainThread(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

procedure TPackageAutoUpdater.DoPackageFormClosed(Sender: TObject; var CloseAction: TCloseAction);
begin
  Run();
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
  FTimer.Interval := 60000 * 5;
  FTimer.Enabled := True;

  SimbaPackageForm.AddHandlerClose(@DoPackageFormClosed);
end;

procedure TPackageAutoUpdater.Run;
begin
  TPackageUpdater.Create();
end;

procedure TPackageUpdater.DoTerminateOnMainThread(Sender: TObject);
begin
  CheckMainThread('TPackageUpdater');

  // Update main menu
  BuildPackageMenus(FPackages);

  // Update icon
  if (FUpdates.Count > 0) then
  begin
    SimbaMainToolBar.ButtonPackage.Hint       := 'Open packages' + LineEnding + FUpdates.Text;
    SimbaMainToolBar.ButtonPackage.ImageIndex := IMG_PACKAGE + Min(1 + FUpdates.Count, 9);
  end else
  begin
    SimbaMainToolBar.ButtonPackage.Hint       := 'Open packages';
    SimbaMainToolBar.ButtonPackage.ImageIndex := IMG_PACKAGE;
  end;
end;

procedure TPackageUpdater.Execute;
var
  I: Integer;
  Package: TSimbaPackage;
begin
  FPackages := LoadPackages();

  for I := 0 to High(FPackages) do
  begin
    Package := FPackages[I];

    if Package.HasUpdate() and Package.AutoUpdateEnabled then
    begin
      SimbaDebugLn([EDebugLn.FOCUS], 'Automatically updating ' + Package.Info.FullName);

      with TSimbaPackageInstaller.Create(Package, SimbaOutputForm.SimbaOutputBox) do
      try
        if InstallLatestVersion() then
        begin
          SimbaDebugLn([EDebugLn.FOCUS, EDebugLn.GREEN], [
            'Succesfully updated "' + Package.Info.FullName + '"',
            'Now at version: ' + Package.InstalledVersion,
            'Scripts will need to be restarted for changes to take effect.'
          ]);
        end else
          SimbaDebugLn([EDebugLn.FOCUS, EDebugLn.RED], [
            'Failed to update "' + Package.Info.FullName + '"'
          ]);
      finally
        Free();
      end;
    end;
  end;

  for I := 0 to High(FPackages) do
    if FPackages[I].HasUpdate() then
      FUpdates.Add('%s can be updated to version %s', [FPackages[I].Info.FullName, FPackages[I].LatestVersion]);
end;

constructor TPackageUpdater.Create;
begin
  inherited Create(False, 512 * 512);

  FreeOnTerminate := True;
  OnTerminate := @DoTerminateOnMainThread;

  FUpdates := TStringList.Create();
  FUpdates.SkipLastLineBreak := True;
end;

destructor TPackageUpdater.Destroy;
var
  I: Integer;
begin
  FUpdates.Free();
  for I := 0 to High(FPackages) do
    FPackages[I].Free();

  inherited Destroy();
end;

procedure SetupPackageAutoUpdater;
begin
  PackageAutoUpdater := TPackageAutoUpdater.Create();
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@SetupPackageAutoUpdater, 'Setup Package AutoUpdater');

finalization
  if Assigned(PackageAutoUpdater) then
    FreeAndNil(PackageAutoUpdater);

end.

