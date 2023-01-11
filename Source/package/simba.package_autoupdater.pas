unit simba.package_autoupdater;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

procedure UpdatePackages;

implementation

uses
  Menus, ComCtrls, LazFileUtils,
  simba.mufasatypes, simba.package, simba.package_configfile, simba.scripttabsform,
  simba.main, simba.outputform, simba.package_installer;

type
  TPackageMenuItem = class(TMenuItem)
  public
    Hash: UInt32;
  end;

  TPackageMenuItem_File = class(TMenuItem)
  public
    FileName: String;

    procedure Click; override;
  end;

procedure TPackageMenuItem_File.Click;
begin
  if (FileName = '') then
    Exit;

  if SimbaScriptTabsForm.Open(FileName, True) and (Caption = 'Run') then
    SimbaForm.MenuItemRun.Click();
end;

type
  TPackageUpdater = class(TThread)
  protected
    FPackages: TSimbaPackageArray;
    FConfigFiles: TSimbaPackageConfigFileArray;
    FUpdates: TStringList;

    procedure DoTerminateOnMainThread(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

procedure TPackageUpdater.DoTerminateOnMainThread(Sender: TObject);

  procedure BuildMenus(MainMenu: TMainMenu);

    function AddMenu(Name: String): TPackageMenuItem;
    var
      I: Integer;
    begin
      for I := 0 to MainMenu.Items.Count - 1 do
        if (MainMenu.Items[I] is TPackageMenuItem) and (MainMenu.Items[I].Caption = Name) then
          Exit(MainMenu.Items[I] as TPackageMenuItem);

      Result := TPackageMenuItem.Create(MainMenu);
      Result.Caption := Name;

      MainMenu.Items.Add(Result);
    end;

    function AddFileMenu(Parent: TMenuItem; Caption, FileName: String): TPackageMenuItem_File;
    begin
      Result := TPackageMenuItem_File.Create(Parent);
      Result.Caption := Caption;
      Result.FileName := FileName;

      Parent.Add(Result);
    end;

  var
    I: Integer;
    Menu: TPackageMenuItem;
    MenuItem: TPackageMenuItem_File;
    Script: String;
  begin
    for I := 0 to High(FPackages) do
    begin
      if (Length(FConfigFiles[I].Scripts) = 0) then
        Continue;

      Menu := AddMenu(FPackages[I].Info.Name);
      if (Menu.Hash = FConfigFiles[I].Hash) then // No changes
        Continue;

      Menu.Hash := FConfigFiles[I].Hash;
      Menu.Clear();

      for Script in FConfigFiles[I].Scripts do
      begin
        MenuItem := AddFileMenu(Menu, ExtractFileNameOnly(Script), '');

        AddFileMenu(MenuItem, 'Run', Script);
        AddFileMenu(MenuItem, 'Open', Script);
      end;
    end;
  end;

  procedure UpdateIcon(Button: TToolButton);
  begin
    if (FUpdates.Count > 0) then
    begin
      Button.Hint       := 'Open packages' + LineEnding + FUpdates.Text;
      Button.ImageIndex := IMAGE_PACKAGE_UPDATE + Min(FUpdates.Count, 9);
    end else
    begin
      Button.Hint       := 'Open packages';
      Button.ImageIndex := IMAGE_PACKAGE;
    end;
  end;

begin
  AssertMainThread('TPackageUpdater.DoTerminateOnMainThread');

  BuildMenus(SimbaForm.MainMenu);
  UpdateIcon(SimbaForm.ToolbarButtonPackages);
end;

procedure TPackageUpdater.Execute;
var
  I: Integer;
begin
  FPackages := LoadPackages();

  for I := 0 to High(FPackages) do
    if FPackages[I].HasUpdate and FPackages[I].AutoUpdateEnabled then
    begin
      SimbaDebugLn('Auto updating ' + FPackages[I].Info.FullName);

      with TSimbaPackageInstaller.Create(FPackages[I], SimbaOutputForm.SimbaOutputBox) do
      try
        if InstallLatestVersion() then
        begin
          SimbaDebugLn('');
          SimbaDebugLn(ESimbaDebugLn.GREEN, 'Succesfully auto updated "%s"', [FPackages[I].Info.FullName]);
          SimbaDebugLn(ESimbaDebugLn.GREEN, '"%s" is now at version "%s"', [FPackages[I].Info.FullName, FPackages[I].InstalledVersion]);
          SimbaDebugLn(ESimbaDebugLn.GREEN, 'Scripts will need to be restarted for changes to take effect.');
        end else
          SimbaDebugLn(ESimbaDebugLn.RED, 'Auto updating %s failed.', [FPackages[I].Info.FullName]);
      finally
        Free();
      end;
    end;

  SetLength(FConfigFiles, Length(FPackages));
  for I := 0 to High(FPackages) do
  begin
    FConfigFiles[I] := ParsePackageConfigFile(FPackages[I].ConfigPath);
    if FPackages[I].HasUpdate() then
      FUpdates.Add('%s can be updated to version %s', [FPackages[I].Info.FullName, FPackages[I].LatestVersion]);
  end;
end;

constructor TPackageUpdater.Create;
begin
  inherited Create(False, 1024*1024);

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

