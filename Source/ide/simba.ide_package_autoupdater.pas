{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Checks for new package Versions.
    - Also handles auto updating
    - Menu bar building
    - Example form
}
unit simba.ide_package_autoupdater;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Menus,
  simba.base;

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
  simba.ide_package, simba.ide_package_installer, simba.ide_maintoolbar, simba.ide_initialization, simba.ide_mainmenubar,
  simba.form_main, simba.form_package, simba.form_output, simba.form_tabs, simba.form_openexample,
  simba.vartype_string, simba.fs, simba.vartype_stringarray;

type
  TPackagePopupMenu = class(TPopupMenu)
  public
    PackageFullName: String;
    Hash: UInt32;
  end;

  TPackageMenuItem = class(TMenuItem)
  public
    FileName: String;

    procedure Click; override;
  end;

procedure TPackageMenuItem.Click;
begin
  if SimbaTabsForm.Open(FileName, True) and (Caption = 'Run') then
    SimbaMainForm.MenuItemRun.Click();
end;

type
  TPackageUpdater = class(TThread)
  protected
    FPackages: TSimbaPackageArray;
    FUpdates: TStringList;
    FDelay: Integer;

    procedure BuildMenu;
    procedure BuildExamples;

    procedure DoTerminateOnMainThread(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create(Delay: Integer = 0); reintroduce;
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
  FTimer.Interval := 60000 * 10;
  FTimer.Enabled := True;

  SimbaPackageForm.AddHandlerClose(@DoPackageFormClosed);

  // Run on create, in a few seconds
  TPackageUpdater.Create(2500);
end;

procedure TPackageAutoUpdater.Run;
begin
  TPackageUpdater.Create();
end;

procedure TPackageUpdater.BuildMenu;

  function GetMenu(PackageFullName: String): TPackagePopupMenu;
  var
    Menu: TPopupMenu;
  begin
    for Menu in SimbaMainMenuBar.MenuBar.Menus do
      if (Menu is TPackagePopupMenu) and (TPackagePopupMenu(Menu).PackageFullName = PackageFullName) then
        Exit(TPackagePopupMenu(Menu));

    Result := TPackagePopupMenu.Create(SimbaMainMenuBar.MenuBar);
  end;

var
  I: Integer;
  Menu: TPackagePopupMenu;
  MenuItemOpen, MenuItemRun: TPackageMenuItem;
  SubMenu: TMenuItem;
  Package: TSimbaPackage;
  Hash: UInt32;
  Files: TStringArray;
begin
  for Package in FPackages do
  begin
    Files := Package.ScriptFiles;
    if (Length(Files) = 0) then
      Continue;

    Hash := ''.Join(Files).Hash();
    Menu := GetMenu(Package.Name);
    if (Menu.Hash = Hash) then // Already built and no changes
      Continue;

    Menu.PackageFullName := Package.Name;
    Menu.Hash := Hash;
    Menu.Items.Clear();

    for I := 0 to High(Files) do
    begin
      SubMenu := TMenuItem.Create(Menu);
      SubMenu.Caption := TSimbaPath.PathExtractNameWithoutExt(Files[I]);

      MenuItemOpen := TPackageMenuItem.Create(SubMenu);
      MenuItemOpen.Caption := 'Open';
      MenuItemOpen.FileName := Files[I];

      MenuItemRun := TPackageMenuItem.Create(SubMenu);
      MenuItemRun.Caption := 'Run';
      MenuItemRun.FileName := Files[I];

      SubMenu.Add(MenuItemOpen);
      SubMenu.Add(MenuItemRun);

      Menu.Items.Add(SubMenu);
    end;

    SimbaMainMenuBar.MenuBar.AddMenu(Package.Name, Menu);
  end;
end;

procedure TPackageUpdater.BuildExamples;
var
  Package: TSimbaPackage;
  Files: TStringArray;
begin
  for Package in FPackages do
  begin
    Files := Package.ExampleFiles;
    if (Length(Files) = 0) then
      Continue;

    SimbaOpenExampleForm.AddPackageExamples(Package.Name, Files);
  end;
end;

procedure TPackageUpdater.DoTerminateOnMainThread(Sender: TObject);
begin
  BuildExamples(); // Open example form
  BuildMenu(); // Main menu bar

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

  SimbaMainToolBar.ButtonPackage.Invalidate();
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
      DebugLn([EDebugLn.FOCUS, EDebugLn.YELLOW], 'Automatically updating %s', [Package.Name]);
      Sleep(750); // whatever, let above flush... TSimbaPackageInstaller directly writes to the synedit.

      try
        with TSimbaPackageInstaller.Create(Package, SimbaOutputForm.SimbaOutputBox) do
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
            DebugLn([EDebugLn.FOCUS, EDebugLn.GREEN], 'Succesfully updated "%s"', [Package.Name]);
            DebugLn([EDebugLn.FOCUS, EDebugLn.GREEN], 'Now at version: %s', [Package.InstalledVersion]);
          end else
            DebugLn([EDebugLn.FOCUS, EDebugLn.RED], 'Failed to update: %s', [Package.Name]);
        finally
          Free();
        end;
      except
        on E: Exception do
          DebugLn([EDebugLn.FOCUS, EDebugLn.RED], 'Failed to update: %s (%s)', [Package.Name, E.Message]);
      end;
    end;
  end;

  for I := 0 to High(FPackages) do
    if FPackages[I].HasUpdate() then
      FUpdates.Add('%s can be updated to version %s', [FPackages[I].Name, FPackages[I].LatestVersion]);

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
  OnTerminate := @DoTerminateOnMainThread;

  FDelay := Delay;

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

