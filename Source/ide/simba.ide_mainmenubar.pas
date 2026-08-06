{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_mainmenubar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Menus,
  simba.base,
  simba.component_menubar,
  simba.ide_events;

type
  TSimbaMainMenuBar = class(TComponent)
  protected
    FMenuBar: TSimbaMenuBar;

    FFileMenu: TPopupMenu;
    FEditMenu: TPopupMenu;
    FSearchMenu: TPopupMenu;
    FScriptMenu: TPopupMenu;
    FToolsMenu: TPopupMenu;
    FViewMenu: TPopupMenu;
    FHelpMenu: TPopupMenu;

    FRecentFilesMenu: TMenuItem;

    FSave: TMenuItem;
    FSaveAll: TMenuItem;

    FUndo: TMenuItem;
    FRedo: TMenuItem;
    FCut: TMenuItem;
    FCopy: TMenuItem;
    FPaste: TMenuItem;

    FRun: TMenuItem;
    FCompile: TMenuItem;
    FStop: TMenuItem;
    FPause: TMenuItem;

    function addMenu(Text: String): TPopupMenu;

    // Add item, storing the event in the items .Tag
    function addItem(
      Menu: TMenu;
      Image: Integer; Text: String; Shortcut: TShortCut;
      Event: ESimbaEvent
    ): TMenuItem;

    function addCheckItem(
      Menu: TMenu;
      Checked: Boolean;
      Text: String;
      Event: ESimbaEvent
    ): TMenuItem;

    function addLine(Menu: TMenu): TMenuItem;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoMenuOpen(Sender: TObject);
    procedure DoMenuItemClick(Sender: TObject);
    procedure DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoActiveControlChange(Sender: TObject; LastControl: TControl);
    procedure DoFillRecentFilesPopup(Sender: TObject);
    procedure DoOpenRecentFileClick(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property MenuBar: TSimbaMenuBar read FMenuBar;
  end;

var
  SimbaMainMenuBar: TSimbaMainMenuBar;

implementation

uses
  Forms, LCLType, LMessages, LazFileUtils,
  simba.initializations,
  simba.ide_tab,
  simba.ide_editor,
  simba.ide_editor_completionbox,
  simba.ide_controller,
  simba.ide_package,
  simba.settings,
  simba.component_images,
  simba.vartype_string, simba.hash,
  simba.fs;

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
  if (Caption = 'Run') then
    SimbaController.OpenInTabAndRun(FileName)
  else
    SimbaController.OpenInTab(FileName);
end;

function TSimbaMainMenuBar.addMenu(Text: String): TPopupMenu;
begin
  Result := TPopupMenu.Create(Self);
  Result.Images := SimbaImages;
  Result.OnPopup := @DoMenuOpen;

  FMenuBar.addMenu(Text, Result);
end;

function TSimbaMainMenuBar.addItem(Menu: TMenu; Image: Integer; Text: String; Shortcut: TShortCut; Event: ESimbaEvent): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Tag := Ord(Event);
  Result.Caption := Text;
  Result.OnClick := @DoMenuItemClick;
  Result.ImageIndex := Image;
  Result.ShortCut := Shortcut;

  Menu.Items.Add(Result);
end;

function TSimbaMainMenuBar.addCheckItem(Menu: TMenu; Checked: Boolean; Text: String; Event: ESimbaEvent): TMenuItem;
begin
  Result := addItem(Menu, -1, Text, scNone, Event);
  Result.ShowAlwaysCheckable := True;
  Result.AutoCheck := True;
  Result.Checked := Checked;
end;

function TSimbaMainMenuBar.addLine(Menu: TMenu): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := '-';

  Menu.Items.Add(Result);
end;

procedure TSimbaMainMenuBar.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  // update recent files submenu
  procedure DoTabLoaded(Tab: TSimbaScriptTab);
  var
    Files: TStringList;
  begin
    Files := TStringList.Create();
    Files.Text := SimbaSettings.General.RecentFiles.Value;
    if (Files.IndexOf(Tab.ScriptFileName) > -1) then
      Files.Delete(Files.IndexOf(Tab.ScriptFileName));
    Files.Insert(0, Tab.ScriptFileName);
    while (Files.Count > 8) do
      Files.Delete(Files.Count - 1);
    Files.Free();
  end;

  procedure DoPackageInstallsChanged(Packages: TSimbaPackageArray);

    function GetMenu(PackageFullName: String): TPackagePopupMenu;
    var
      Menu: TPopupMenu;
    begin
      for Menu in FMenuBar.Menus do
        if (Menu is TPackagePopupMenu) and (TPackagePopupMenu(Menu).PackageFullName = PackageFullName) then
          Exit(TPackagePopupMenu(Menu));

      Result := TPackagePopupMenu.Create(FMenuBar);
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
    for Package in Packages do
    begin
      Files := Package.ScriptFiles;
      if (Length(Files) = 0) then
        Continue;

      Hash := HashString(''.Join(Files));
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

      MenuBar.AddMenu(Package.Name, Menu);
    end;
  end;

begin
  case Event of
    ESimbaEvent.TAB_LOADED: DoTabLoaded(TSimbaScriptTab(Data));
    ESimbaEvent.PACKAGE_INSTALLS_CHANGED: DoPackageInstallsChanged(TSimbaPackageArray(Data));
  end;
end;

procedure TSimbaMainMenuBar.DoMenuOpen(Sender: TObject);
var
  CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean;
  CanSave, CanCut, CanCopy, CanPaste: Boolean;
begin
  if SimbaController.GetScriptButtonStates(CanRun, CanPause, CanCompile, CanStop, CanForceStop) and
     SimbaController.GetEditorButtonStates(CanSave, CanCut, CanCopy, CanPaste) then
  begin
    FRun.Enabled      := CanRun;
    FPause.Enabled    := CanPause;
    FCompile.Enabled  := CanCompile;
    FStop.Enabled     := CanStop;
    FStop.ImageIndex  := IfThen(CanForceStop, SimbaImages.POWER, SimbaImages.STOP);

    FSave.Enabled  := CanSave;
    FCut.Enabled   := CanCut;
    FCopy.Enabled  := CanCopy;
    FPaste.Enabled := CanPaste;
  end;
end;

procedure TSimbaMainMenuBar.DoMenuItemClick(Sender: TObject);
begin
  SimbaEvents.Post(ESimbaEvent(TMenuItem(Sender).Tag), Sender);
end;

procedure TSimbaMainMenuBar.DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure CheckShortcuts;
  var
    Msg: TLMKey;
  begin
    FMenuBar.HotIndex := -1;

    // quick exit: cant be anything we want.
    if (Key <> VK_F3) and (Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssAltGr] = []) then
      Exit;

    // Only check these if completion form or editor is focused and doesnt have such a keystroke
    if (Screen.ActiveControl is TSimbaCompletionBox_Form) or
       ((Screen.ActiveControl is TSimbaEditor) and (TSimbaEditor(Screen.ActiveControl).Keystrokes.FindKeycode(Key, Shift) = -1)) then
    begin
      Msg := Default(TLMKey);
      Msg.CharCode := Key;
      if (ssAlt in Shift) then
        Msg.KeyData := MK_ALT;

      if FFileMenu.IsShortcut(Msg) or
         FEditMenu.IsShortcut(Msg) or
         FScriptMenu.IsShortcut(Msg) or
         FSearchMenu.IsShortcut(Msg) then
        Key := 0;
    end;
  end;

begin
  case Key of
    VK_ESCAPE:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;
        FMenuBar.HotIndex := -1;

        Key := 0;
      end;

    VK_MENU:
      begin
        if (FMenuBar.HotIndex > -1) then // move to next
        begin
          if (FMenuBar.HotIndex = FMenuBar.MenuCount - 1) then
            FMenuBar.HotIndex := 0
          else
            FMenuBar.HotIndex := FMenuBar.HotIndex + 1;
        end else
          FMenuBar.HotIndex := 0;

        Key := 0;
      end;

    VK_LEFT:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;

        if (FMenuBar.HotIndex = 0) then
          FMenuBar.HotIndex := FMenuBar.MenuCount - 1
        else
          FMenuBar.HotIndex := FMenuBar.HotIndex - 1;

        Key := 0;
      end;

    VK_RIGHT:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;

        if (FMenuBar.HotIndex = FMenuBar.MenuCount - 1) then
          FMenuBar.HotIndex := 0
        else
          FMenuBar.HotIndex := FMenuBar.HotIndex + 1;

        Key := 0;
      end;

    VK_RETURN:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;
        Application.QueueAsyncCall(@FMenuBar.PopupDelayed, FMenuBar.HotIndex);

        Key := 0;
      end;
    else
      CheckShortcuts();
  end;
end;

procedure TSimbaMainMenuBar.DoActiveControlChange(Sender: TObject; LastControl: TControl);
begin
  FMenuBar.HotIndex := -1;
end;

procedure TSimbaMainMenuBar.DoFillRecentFilesPopup(Sender: TObject);
var
  RecentFiles: TStringList;
  Item: TMenuItem;
  I: Integer;
begin
  RecentFiles := TStringList.Create();
  RecentFiles.Text := SimbaSettings.General.RecentFiles.Value;

  FRecentFilesMenu.Clear();
  for I := 0 to RecentFiles.Count - 1 do
  begin
    Item := TMenuItem.Create(FRecentFilesMenu);
    Item.Caption := ShortDisplayFilename(RecentFiles[I]);
    Item.Hint := RecentFiles[I];
    Item.OnClick := @DoOpenRecentFileClick;

    FRecentFilesMenu.Add(Item);
  end;

  RecentFiles.Free();
end;

procedure TSimbaMainMenuBar.DoOpenRecentFileClick(Sender: TObject);
begin
  SimbaController.OpenInTab(TMenuItem(Sender).Hint);
end;

constructor TSimbaMainMenuBar.Create;

  procedure addFileMenu();
  begin
    FFileMenu := addMenu('File');
    FFileMenu.OnPopup := @DoFillRecentFilesPopup;
    addItem(FFileMenu, SimbaImages.NEW, 'New', ShortCut(VK_N, [ssCtrl]), ESimbaEvent.ACTION_NEW);
    addLine(FFileMenu);
    addItem(FFileMenu, SimbaImages.FOLDER, 'Open', ShortCut(VK_O, [ssCtrl]), ESimbaEvent.ACTION_OPEN);
    FRecentFilesMenu := addItem(FFileMenu, SimbaImages.FOLDER_RECENT, 'Open Recent', scNone, ESimbaEvent.NONE);
    addLine(FFileMenu);
    addItem(FFileMenu, SimbaImages.EYE, 'Open Example ...', scNone, ESimbaEvent.ACTION_OPEN_EXAMPLE);
    addItem(FFileMenu, -1, 'Open Backup ...', scNone, ESimbaEvent.ACTION_OPEN_BACKUP);
    addLine(FFileMenu);
    FSave := addItem(FFileMenu, SimbaImages.SAVE, 'Save', ShortCut(VK_S, [ssCtrl]), ESimbaEvent.ACTION_SAVE);
    addItem(FFileMenu, SimbaImages.SAVE_AS, 'Save As', scNone, ESimbaEvent.ACTION_SAVE_AS);
    addItem(FFileMenu, -1, 'Save As Default', scNone, ESimbaEvent.ACTION_SAVE_AS_DEFAULT);
    FSaveAll := addItem(FFileMenu, SimbaImages.SAVE_ALL, 'Save All', scNone, ESimbaEvent.ACTION_SAVE_ALL);
    addLine(FFileMenu);
    addItem(FFileMenu, SimbaImages.CLOSE, 'Close Tab', ShortCut(VK_W, [ssCtrl]), ESimbaEvent.ACTION_CLOSE_TAB);
    addItem(FFileMenu, SimbaImages.CLOSE_ALL, 'Close All Tabs', scNone, ESimbaEvent.ACTION_CLOSE_ALL_TABS);
    addLine(FFileMenu);
    addItem(FFileMenu, SimbaImages.POWER, 'Quit', ShortCut(VK_Q, [ssCtrl]), ESimbaEvent.ACTION_QUIT);
  end;

  procedure addEditMenu();
  begin
    FEditMenu := addMenu('Edit');

    FUndo := addItem(FEditMenu, SimbaImages.UNDO, 'Undo', ShortCut(VK_Z, [ssCtrl]), ESimbaEvent.ACTION_UNDO);
    FRedo := addItem(FEditMenu, SimbaImages.REDO, 'Redo', ShortCut(VK_Z, [ssShift, ssCtrl]), ESimbaEvent.ACTION_REDO);
    addLine(FEditMenu);
    FCut   := addItem(FEditMenu, SimbaImages.CUT, 'Cut', ShortCut(VK_X, [ssCtrl]), ESimbaEvent.ACTION_CUT);
    FCopy  := addItem(FEditMenu, SimbaImages.COPY, 'Copy', ShortCut(VK_C, [ssCtrl]), ESimbaEvent.ACTION_COPY);
    FPaste := addItem(FEditMenu, SimbaImages.PASTE, 'Paste', ShortCut(VK_V, [ssCtrl]), ESimbaEvent.ACTION_PASTE);
    addLine(FEditMenu);
    addItem(FEditMenu, SimbaImages.SELECT_ALL, 'Select All', ShortCut(VK_A, [ssCtrl]), ESimbaEvent.ACTION_SELECT_ALL);
    addItem(FEditMenu, SimbaImages.SELECT_LINE, 'Select Line', ShortCut(VK_L, [ssCtrl]), ESimbaEvent.ACTION_SELECT_LINE);
    addItem(FEditMenu, SimbaImages.SELECT_WORD, 'Select Word', ShortCut(VK_D, [ssCtrl]), ESimbaEvent.ACTION_SELECT_WORD);
    addLine(FEditMenu);
    addItem(FEditMenu, SimbaImages.LOWERCASE, 'Lowercase Selection', scNone, ESimbaEvent.ACTION_LOWER_SELECTION);
    addItem(FEditMenu, SimbaImages.UPPERCASE, 'Uppercase Selection', scNone, ESimbaEvent.ACTION_UPPER_SELECTION);
  end;

  procedure addSearchMenu();
  begin
    FSearchMenu := addMenu('Search');

    addItem(FSearchMenu, SimbaImages.FIND, 'Find', ShortCut(VK_F, [ssCtrl]), ESimbaEvent.ACTION_FIND);
    addItem(FSearchMenu, SimbaImages.FIND_NEXT, 'Find Next', ShortCut(VK_F3, []), ESimbaEvent.ACTION_FIND_NEXT);
    addItem(FSearchMenu, SimbaImages.FIND_PREV, 'Find Previous', ShortCut(VK_F3, [ssShift]), ESimbaEvent.ACTION_FIND_PREV);
    addLine(FSearchMenu);
    addItem(FSearchMenu, SimbaImages.FIND_FILES, 'Find in Files ...', ShortCut(VK_F, [ssShift, ssCtrl]), ESimbaEvent.ACTION_FIND_IN_FILES);
    addLine(FSearchMenu);
    addItem(FSearchMenu, SimbaImages.FIND_REPLACE, 'Replace ...', ShortCut(VK_R, [ssCtrl]), ESimbaEvent.ACTION_REPLACE);
    addLine(FSearchMenu);
    addItem(FSearchMenu, -1, 'Go-to Line ...', ShortCut(VK_G, [ssCtrl]), ESimbaEvent.ACTION_GOTO_LINE);
    addLine(FSearchMenu);
  end;

  procedure addScriptMenu();
  begin
    FScriptMenu := addMenu('Script');

    FCompile := addItem(FScriptMenu, SimbaImages.COMPILE, 'Compile', ShortCut(VK_C, [ssAlt]), ESimbaEvent.ACTION_COMPILE);
    FRun     := addItem(FScriptMenu, SimbaImages.PLAY, 'Run', ShortCut(VK_R, [ssAlt]), ESimbaEvent.ACTION_RUN);
    FPause   := addItem(FScriptMenu, SimbaImages.PAUSE, 'Pause', scNone, ESimbaEvent.ACTION_PAUSE);
    FStop    := addItem(FScriptMenu, SimbaImages.STOP, 'Stop', ShortCut(VK_S, [ssAlt]), ESimbaEvent.ACTION_STOP);
    addLine(FScriptMenu);
    addCheckItem(FScriptMenu, SimbaSettings.Compiler.ShowHints.Value, 'Compiler Hints', ESimbaEvent.ACTION_COMPILER_HINTS);
  end;

  procedure addToolsMenu();
  begin
    FToolsMenu := addMenu('Tools');

    addItem(FToolsMenu, SimbaImages.SETTINGS, 'Settings', scNone, ESimbaEvent.ACTION_SETTINGS);
    addItem(FToolsMenu, SimbaImages.PACKAGE, 'Packages', scNone, ESimbaEvent.ACTION_PACKAGES);
    addLine(FToolsMenu);
    addItem(FToolsMenu, SimbaImages.SIMBA, 'Associate Scripts', scNone, ESimbaEvent.ACTION_ASSOCIATE);
    addLine(FToolsMenu);
    addItem(FToolsMenu, -1, 'Image To String', scNone, ESimbaEvent.ACTION_IMG_TO_STRING); // todo: remove usage of form designer
    addItem(FToolsMenu, SimbaImages.COLORS, 'ACA', scNone, ESimbaEvent.ACTION_ACA);
    addItem(FToolsMenu, -1, 'DTM Editor', scNone, ESimbaEvent.ACTION_DTM_EDITOR);
    addItem(FToolsMenu, SimbaImages.SHAPES, 'Shape Box', scNone, ESimbaEvent.ACTION_SHAPE_BOX); // todo: remove usage of form designer
    addLine(FToolsMenu);
    addItem(FToolsMenu, -1, 'Format Script', scNone, ESimbaEvent.ACTION_FORMAT_SCRIPT);
    addLine(FToolsMenu);
    addItem(FToolsMenu, -1, 'Download Simba ...', scNone, ESimbaEvent.ACTION_DOWNLOAD_SIMBA); // todo: remove usage of form designer
  end;

  procedure addViewMenu();
  begin
    FViewMenu := addMenu('View');

    addCheckItem(FViewMenu, SimbaSettings.General.TrayIconVisible.Value, 'Tray Icon', ESimbaEvent.ACTION_VIEW_TRAYICON);
    addLine(FViewMenu);
    addItem(FViewMenu, -1, 'Colour Picker History', scNone, ESimbaEvent.ACTION_VIEW_COLORHISTORY);
    addItem(FViewMenu, -1, 'Debug Image', scNone, ESimbaEvent.ACTION_VIEW_DEBUGIMAGE);   // todo debugimage should be a control, not a form
    addItem(FViewMenu, -1, 'Debug Matrix', scNone, ESimbaEvent.ACTION_VIEW_DEBUGMATRIX); // ..
    addItem(FViewMenu, -1, 'Editor', scNone, ESimbaEvent.ACTION_VIEW_EDITOR);
    addItem(FViewMenu, -1, 'Function List', scNone, ESimbaEvent.ACTION_VIEW_FUNCTIONLIST);
    addItem(FViewMenu, -1, 'Notes', scNone, ESimbaEvent.ACTION_VIEW_NOTES); // todo ditch form designer
    addItem(FViewMenu, -1, 'File Browser', scNone, ESimbaEvent.ACTION_VIEW_FILEBROWSER);
    addItem(FViewMenu, -1, 'Output', scNone, ESimbaEvent.ACTION_VIEW_OUTPUT);
    addItem(FViewMenu, -1, 'Backup', scNone, ESimbaEvent.ACTION_VIEW_BACKUP);
    addItem(FViewMenu, -1, 'Find In Files', scNone, ESimbaEvent.ACTION_VIEW_FINDINFILES);
    addLine(FViewMenu);
    addItem(FViewMenu, -1, 'Reset Layout', scNone, ESimbaEvent.ACTION_RESET_LAYOUT);
    addCheckItem(FViewMenu, SimbaSettings.General.LockLayout.Value, 'Lock Layout', ESimbaEvent.ACTION_LOCK_LAYOUT);
  end;

  procedure addHelpMenu();
  begin
    FHelpMenu := addMenu('Help');

    addItem(FHelpMenu, SimbaImages.SIMBA, 'About', scNone, ESimbaEvent.ACTION_ABOUT); // todo ditch form designer
    addItem(FHelpMenu, SimbaImages.WRITE_BUG, 'Report a Bug', scNone, ESimbaEvent.ACTION_REPORTBUG);
    addItem(FHelpMenu, SimbaImages.GITHUB, 'Simba Github', scNone, ESimbaEvent.ACTION_SIMBAGITHUB);
    addItem(FHelpMenu, -1, 'Online Documentation', scNone, ESimbaEvent.ACTION_ONLINEDOCS);
  end;

begin
  inherited Create(nil);

  FMenuBar := TSimbaMenuBar.Create(Self);
  FMenuBar.Parent := Application.MainForm;
  FMenuBar.Align := alTop;
  FMenuBar.Top := -1; // important to ensure is always at the very top

  addFileMenu();
  addEditMenu();
  addSearchMenu();
  addScriptMenu();
  addToolsMenu();
  addViewMenu();
  addHelpMenu();

  Application.AddOnKeyDownBeforeHandler(@DoApplicationKeyDown);
  Screen.AddHandlerActiveControlChanged(@DoActiveControlChange);

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.TAB_LOADED, ESimbaEvent.PACKAGE_INSTALLS_CHANGED]);
end;

destructor TSimbaMainMenuBar.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  Application.RemoveAllHandlersOfObject(Self);

  Screen.RemoveAllHandlersOfObject(Self);

  inherited Destroy();
end;

procedure DoCreate;
begin
  SimbaMainMenuBar := TSimbaMainMenuBar.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaMainMenuBar);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaMainMenuBar', 5); // seems some priority (before script tabs)
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaMainMenuBar', 5);

end.

