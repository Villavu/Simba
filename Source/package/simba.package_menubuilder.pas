{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_menubuilder;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.package, simba.component_menubar;

procedure BuildPackageMenus(Packages: TSimbaPackageArray; MenuBar: TSimbaMainMenuBar);

implementation

uses
  Menus,
  simba.main, simba.scripttabsform, simba.files;

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
  if SimbaScriptTabsForm.Open(FileName, True) and (Caption = 'Run') then
    SimbaForm.MenuItemRun.Click();
end;

procedure BuildPackageMenus(Packages: TSimbaPackageArray; MenuBar: TSimbaMainMenuBar);

  function GetMenu(PackageFullName: String): TPackagePopupMenu;
  var
    Menu: TPopupMenu;
  begin
    for Menu in MenuBar.Menus do
      if (Menu is TPackagePopupMenu) and (TPackagePopupMenu(Menu).PackageFullName = PackageFullName) then
      begin
        Result := TPackagePopupMenu(Menu);
        Exit;
      end;

    Result := TPackagePopupMenu.Create(MenuBar);
  end;

var
  I: Integer;
  Files: TStringArray;
  FilesHash: UInt32;
  FileName: String;
  Menu: TPackagePopupMenu;
  MenuItemOpen, MenuItemRun: TPackageMenuItem;
  SubMenu: TMenuItem;
begin
  for I := 0 to High(Packages) do
    if Packages[I].IsInstalled() then
    begin
      Files := Packages[I].GetScripts();

      if (Length(Files) > 0) then
      begin
        FilesHash := ''.Join(Files).Hash();

        Menu := GetMenu(Packages[I].Info.FullName);
        if (Menu.Hash = FilesHash) then // Already built and no changes
          Continue;

        Menu.PackageFullName := Packages[I].Info.FullName;
        Menu.Hash := FilesHash;
        Menu.Items.Clear();

        for FileName in Files do
        begin
          SubMenu := TMenuItem.Create(Menu);
          SubMenu.Caption := TSimbaPath.PathExtractNameWithoutExt(FileName);

          MenuItemOpen := TPackageMenuItem.Create(SubMenu);
          MenuItemOpen.Caption := 'Open';
          MenuItemOpen.FileName := FileName;

          MenuItemRun := TPackageMenuItem.Create(SubMenu);
          MenuItemRun.Caption := 'Run';
          MenuItemRun.FileName := FileName;

          SubMenu.Add(MenuItemOpen);
          SubMenu.Add(MenuItemRun);

          Menu.Items.Add(SubMenu);
        end;

        MenuBar.AddMenu(Packages[I].Info.Name, Menu);
      end;
    end;
end;

end.

