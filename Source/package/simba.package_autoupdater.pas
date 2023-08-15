{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_autoupdater;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

procedure UpdatePackages;

implementation

uses
  simba.mufasatypes, simba.package, simba.package_installer,
  simba.main, simba.outputform, simba.package_menubuilder;

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

procedure TPackageUpdater.DoTerminateOnMainThread(Sender: TObject);
begin
  AssertMainThread('TPackageUpdater');

  // Update main menu
  BuildPackageMenus(FPackages, SimbaForm.MenuBar);

  // Update icon
  if (FUpdates.Count > 0) then
  begin
    SimbaForm.ToolbarButtonPackages.Hint       := 'Open packages' + LineEnding + FUpdates.Text;
    SimbaForm.ToolbarButtonPackages.ImageIndex := IMG_PACKAGE + Min(FUpdates.Count, 9);
  end else
  begin
    SimbaForm.ToolbarButtonPackages.Hint       := 'Open packages';
    SimbaForm.ToolbarButtonPackages.ImageIndex := IMG_PACKAGE;
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
    if Package.HasUpdate() then
      FUpdates.Add('%s can be updated to version %s', [Package.Info.FullName, Package.LatestVersion]);
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

procedure UpdatePackages;
begin
  TPackageUpdater.Create();
end;

end.

