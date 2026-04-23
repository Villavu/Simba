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

    // Add item, storing the event in the items .tag
    function addItem(
      Menu: TMenu;
      Image: Integer; Text: String; Shortcut: TShortCut;
      Event: ESimbaEvent
    ): TMenuItem;

    function addCheckItem(
      Menu: TMenu;
      Text: String; Shortcut: TShortCut;
      Event: ESimbaEvent
    ): TMenuItem;

    function addLine(Menu: TMenu): TMenuItem;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoMenuItemClick(Sender: TObject);
    procedure DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoActiveControlChange(Sender: TObject; LastControl: TControl);
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
  simba.initializations, simba.ide_maintoolbar, simba.form_main,
  simba.ide_tab,
  simba.ide_editor,
  simba.ide_controller,
  simba.settings;

function TSimbaMainMenuBar.addMenu(Text: String): TPopupMenu;
begin
  Result := TPopupMenu.Create(Self);
  Result.Images := SimbaMainForm.Images;

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

function TSimbaMainMenuBar.addCheckItem(Menu: TMenu; Text: String; Shortcut: TShortCut; Event: ESimbaEvent): TMenuItem;
begin
  Result := addItem(Menu, IMG_NONE, Text, Shortcut, Event);
  Result.ShowAlwaysCheckable := True;
  Result.AutoCheck := True;
end;

function TSimbaMainMenuBar.addLine(Menu: TMenu): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := '-';

  Menu.Items.Add(Result);
end;

// TODO: SaveAll handling
procedure TSimbaMainMenuBar.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  // update run/pause/compile/stop enable
  procedure DoTabScriptStateChanged(Tab: TSimbaScriptTab);
  var
    State: ESimbaScriptState;
  begin
    if (not Tab.IsActiveTab) then
      Exit;

    State := Tab.ScriptState;

    FRun.Enabled      := (State = ESimbaScriptState.PAUSED) or (State = ESimbaScriptState.NONE);
    FPause.Enabled    := (State = ESimbaScriptState.RUNNING);
    FCompile.Enabled  := (State = ESimbaScriptState.PAUSED) or (State = ESimbaScriptState.NONE);
    FStop.Enabled     := (State <> ESimbaScriptState.NONE);

    if (State = ESimbaScriptState.STOP) then
      FStop.ImageIndex := IMG_POWER
    else
      FStop.ImageIndex := IMG_STOP;
  end;

  // update save/cut/copy/paste enable
  procedure DoTabModified(Tab: TSimbaScriptTab);
  begin
    FSave.Enabled  := Tab.ScriptChanged;
    FCut.Enabled   := Tab.Editor.SelAvail;
    FCopy.Enabled  := Tab.Editor.SelAvail;
    FPaste.Enabled := Tab.Editor.CanPaste;
  end;

  // enable various menu items
  procedure DoTabChange(Tab: TSimbaScriptTab);
  begin
    DoTabModified(Tab);
    DoTabScriptStateChanged(Tab);
  end;

  // update recent files submenu
  procedure DoTabLoaded(Tab: TSimbaScriptTab);
  var
    I: Integer;
    Item: TMenuItem;
    Files: TStringList;
  begin
    Files := TStringList.Create();
    Files.Text := SimbaSettings.General.RecentFiles.Value;
    if (Files.IndexOf(Tab.ScriptFileName) > -1) then
      Files.Delete(Files.IndexOf(Tab.ScriptFileName));
    Files.Insert(0, Tab.ScriptFileName);
    while (Files.Count > 8) do
      Files.Delete(Files.Count - 1);

    FRecentFilesMenu.Clear();
    for I := 0 to Files.Count - 1 do
    begin
      Item := TMenuItem.Create(FRecentFilesMenu);
      Item.Caption := ShortDisplayFilename(Files[I]);
      Item.Hint := Files[I];
      Item.OnClick := @DoOpenRecentFileClick;

      FRecentFilesMenu.Add(Item);
    end;
    SimbaSettings.General.RecentFiles.Value := Files.Text;

    Files.Free();
  end;

begin
  case Event of
    ESimbaEvent.TAB_MODIFIED:           DoTabModified(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_CHANGE:             DoTabChange(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_SCRIPTSTATE_CHANGE: DoTabScriptStateChanged(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_LOADED:             DoTabLoaded(TSimbaScriptTab(Data));
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

    // Only check these if editor is focused and doesnt have such a keystroke
    if (Screen.ActiveControl is TSimbaEditor) and
       (TSimbaEditor(Screen.ActiveControl).Keystrokes.FindKeycode(Key, Shift) = -1) then
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

procedure TSimbaMainMenuBar.DoOpenRecentFileClick(Sender: TObject);
begin
  SimbaController.OpenInTab(TMenuItem(Sender).Hint);
end;

constructor TSimbaMainMenuBar.Create;

  procedure addFileMenu();
  begin
    FFileMenu := addMenu('File');

    addItem(FFileMenu, IMG_NEW, 'New', ShortCut(VK_N, [ssCtrl]), ESimbaEvent.ACTION_NEW);
    addLine(FFileMenu);
    addItem(FFileMenu, IMG_OPEN, 'Open', ShortCut(VK_O, [ssCtrl]), ESimbaEvent.ACTION_OPEN);
    FRecentFilesMenu := addItem(FFileMenu, IMG_OPEN_RECENT, 'Open Recent', scNone, ESimbaEvent.NONE);
    addLine(FFileMenu);
    addItem(FFileMenu, IMG_EYE, 'Open Example ...', scNone, ESimbaEvent.ACTION_OPEN_EXAMPLE);
    addItem(FFileMenu, IMG_NONE, 'Open Backup ...', scNone, ESimbaEvent.ACTION_OPEN_BACKUP);
    addLine(FFileMenu);
    FSave := addItem(FFileMenu, IMG_SAVE, 'Save', ShortCut(VK_S, [ssCtrl]), ESimbaEvent.ACTION_SAVE);
    addItem(FFileMenu, IMG_SAVE_AS, 'Save As', scNone, ESimbaEvent.ACTION_SAVE_AS);
    addItem(FFileMenu, IMG_NONE, 'Save As Default', scNone, ESimbaEvent.ACTION_SAVE_AS_DEFAULT);
    FSaveAll := addItem(FFileMenu, IMG_SAVE_ALL, 'Save All', scNone, ESimbaEvent.ACTION_SAVE_ALL);
    addLine(FFileMenu);
    addItem(FFileMenu, IMG_CLOSE, 'Close Tab', ShortCut(VK_W, [ssCtrl]), ESimbaEvent.ACTION_CLOSE_TAB);
    addItem(FFileMenu, IMG_CLOSE_ALL, 'Close All Tabs', scNone, ESimbaEvent.ACTION_CLOSE_ALL_TABS);
    addLine(FFileMenu);
    addItem(FFileMenu, IMG_POWER, 'Quit', ShortCut(VK_Q, [ssCtrl]), ESimbaEvent.ACTION_QUIT);
  end;

  procedure addEditMenu();
  begin
    FEditMenu := addMenu('Edit');

    FUndo := addItem(FEditMenu, IMG_UNDO, 'Undo', ShortCut(VK_Z, [ssCtrl]), ESimbaEvent.ACTION_UNDO);
    FRedo := addItem(FEditMenu, IMG_REDO, 'Redo', ShortCut(VK_Z, [ssShift, ssCtrl]), ESimbaEvent.ACTION_REDO);
    addLine(FEditMenu);
    FCut   := addItem(FEditMenu, IMG_CUT, 'Cut', ShortCut(VK_X, [ssCtrl]), ESimbaEvent.ACTION_CUT);
    FCopy  := addItem(FEditMenu, IMG_COPY, 'Copy', ShortCut(VK_C, [ssCtrl]), ESimbaEvent.ACTION_COPY);
    FPaste := addItem(FEditMenu, IMG_PASTE, 'Paste', ShortCut(VK_V, [ssCtrl]), ESimbaEvent.ACTION_PASTE);
    addLine(FEditMenu);
    addItem(FEditMenu, IMG_SELECT_ALL, 'Select All', ShortCut(VK_A, [ssCtrl]), ESimbaEvent.ACTION_SELECT_ALL);
    addItem(FEditMenu, IMG_SELECT_LINE, 'Select Line', ShortCut(VK_L, [ssCtrl]), ESimbaEvent.ACTION_SELECT_LINE);
    addItem(FEditMenu, IMG_SELECT_WORD, 'Select Word', ShortCut(VK_D, [ssCtrl]), ESimbaEvent.ACTION_SELECT_WORD);
    addLine(FEditMenu);
    addItem(FEditMenu, IMG_LOWERCASE, 'Lowercase Selection', scNone, ESimbaEvent.ACTION_LOWER_SELECTION);
    addItem(FEditMenu, IMG_UPPERCASE, 'Uppercase Selection', scNone, ESimbaEvent.ACTION_UPPER_SELECTION);
  end;

  procedure addSearchMenu();
  begin
    FSearchMenu := addMenu('Search');

    addItem(FSearchMenu, IMG_FIND, 'Find', ShortCut(VK_F, [ssCtrl]), ESimbaEvent.ACTION_FIND);
    addItem(FSearchMenu, IMG_FIND_NEXT, 'Find Next', ShortCut(VK_F3, []), ESimbaEvent.ACTION_FIND_NEXT);
    addItem(FSearchMenu, IMG_FIND_PREV, 'Find Previous', ShortCut(VK_F3, [ssShift]), ESimbaEvent.ACTION_FIND_PREV);
    addLine(FSearchMenu);
    addItem(FSearchMenu, IMG_FIND_FILES, 'Find in Files ...', ShortCut(VK_F, [ssShift, ssCtrl]), ESimbaEvent.ACTION_FIND_IN_FILES);
    addLine(FSearchMenu);
    addItem(FSearchMenu, IMG_FIND, 'Replace ...', ShortCut(VK_R, [ssCtrl]), ESimbaEvent.ACTION_REPLACE);
    addLine(FSearchMenu);
    addItem(FSearchMenu, IMG_FIND, 'Go-to Line ...', ShortCut(VK_G, [ssCtrl]), ESimbaEvent.ACTION_GOTO_LINE);
    addLine(FSearchMenu);
  end;

  procedure addScriptMenu();
  begin
    FScriptMenu := addMenu('Script');

    FCompile := addItem(FScriptMenu, IMG_COMPILE, 'Compile', ShortCut(VK_C, [ssAlt]), ESimbaEvent.ACTION_COMPILE);
    FRun     := addItem(FScriptMenu, IMG_PLAY, 'Run', ShortCut(VK_R, [ssAlt]), ESimbaEvent.ACTION_RUN);
    FPause   := addItem(FScriptMenu, IMG_PAUSE, 'Pause', scNone, ESimbaEvent.ACTION_PAUSE);
    FStop    := addItem(FScriptMenu, IMG_STOP, 'Stop', ShortCut(VK_S, [ssAlt]), ESimbaEvent.ACTION_STOP);
    addLine(FScriptMenu);
    addCheckItem(FScriptMenu, 'Compiler Hints', scNone, ESimbaEvent.ACTION_COMPILER_HINTS);
  end;

  procedure addToolsMenu();
  begin
    FToolsMenu := addMenu('Tools');

    addItem(FToolsMenu, IMG_OPTIONS, 'Settings', scNone, ESimbaEvent.ACTION_SETTINGS);
    addItem(FToolsMenu, IMG_PACKAGE, 'Packages', scNone, ESimbaEvent.ACTION_PACKAGES);
    addLine(FToolsMenu);
    addItem(FToolsMenu, IMG_SIMBA, 'Associate Scripts', scNone, ESimbaEvent.ACTION_ASSOCIATE);
    addLine(FToolsMenu);
    addItem(FToolsMenu, IMG_NONE, 'Image To String', scNone, ESimbaEvent.ACTION_IMG_TO_STRING); // todo: remove usage of form designer
    addItem(FToolsMenu, IMG_COLORS, 'ACA', scNone, ESimbaEvent.ACTION_ACA);
    addItem(FToolsMenu, IMG_NONE, 'DTM Editor', scNone, ESimbaEvent.ACTION_DTM_EDITOR);
    addItem(FToolsMenu, IMG_SHAPE, 'Shape Box', scNone, ESimbaEvent.ACTION_SHAPE_BOX); // todo: remove usage of form designer
    addLine(FToolsMenu);
    addItem(FToolsMenu, IMG_NONE, 'Format Script', scNone, ESimbaEvent.ACTION_FORMAT_SCRIPT);
    addLine(FToolsMenu);
    addItem(FToolsMenu, IMG_NONE, 'Download Simba ...', scNone, ESimbaEvent.ACTION_DOWNLOAD_SIMBA); // todo: remove usage of form designer
  end;

  procedure addViewMenu();
  begin
    FViewMenu := addMenu('View');

    addCheckItem(FViewMenu, 'Tray Icon', scNone, ESimbaEvent.ACTION_VIEW_TRAYICON);
    addLine(FViewMenu);
    addItem(FViewMenu, IMG_NONE, 'Colour Picker History', scNone, ESimbaEvent.ACTION_VIEW_COLORHISTORY);
    addItem(FViewMenu, IMG_NONE, 'Debug Image', scNone, ESimbaEvent.ACTION_VIEW_DEBUGIMAGE);   // todo debugimage should be a control, not a form
    addItem(FViewMenu, IMG_NONE, 'Debug Matrix', scNone, ESimbaEvent.ACTION_VIEW_DEBUGMATRIX); // ..
    addItem(FViewMenu, IMG_NONE, 'Editor', scNone, ESimbaEvent.ACTION_VIEW_EDITOR);
    addItem(FViewMenu, IMG_NONE, 'Function List', scNone, ESimbaEvent.ACTION_VIEW_FUNCTIONLIST);
    addItem(FViewMenu, IMG_NONE, 'Notes', scNone, ESimbaEvent.ACTION_VIEW_NOTES); // todo ditch form designer
    addItem(FViewMenu, IMG_NONE, 'File Browser', scNone, ESimbaEvent.ACTION_VIEW_FILEBROWSER);
    addItem(FViewMenu, IMG_NONE, 'Output', scNone, ESimbaEvent.ACTION_VIEW_OUTPUT);
    addItem(FViewMenu, IMG_NONE, 'Backup', scNone, ESimbaEvent.ACTION_VIEW_BACKUP);
    addItem(FViewMenu, IMG_NONE, 'Find In Files', scNone, ESimbaEvent.ACTION_VIEW_FINDINFILES);
    addLine(FViewMenu);
    addItem(FViewMenu, IMG_NONE, 'Reset Layout', scNone, ESimbaEvent.ACTION_RESET_LAYOUT);
    addCheckItem(FViewMenu, 'Lock Layout', scNone, ESimbaEvent.ACTION_LOCK_LAYOUT);
  end;

  procedure addHelpMenu();
  begin
    FHelpMenu := addMenu('Help');

    addItem(FHelpMenu, IMG_SIMBA, 'About', scNone, ESimbaEvent.ACTION_ABOUT); // todo ditch form designer
    addItem(FHelpMenu, IMG_WRITE_BUG, 'Report a Bug', scNone, ESimbaEvent.ACTION_REPORTBUG);
    addItem(FHelpMenu, IMG_GITHUB, 'Simba Github', scNone, ESimbaEvent.ACTION_SIMBAGITHUB);
    addItem(FHelpMenu, IMG_NONE, 'Online Documentation', scNone, ESimbaEvent.ACTION_ONLINEDOCS);
  end;

begin
  inherited Create(nil);

  FMenuBar := TSimbaMenuBar.Create(Self);
  FMenuBar.Parent := SimbaMainForm.MainMenuPanel;
  FMenuBar.Align := alTop;

  addFileMenu();
  addEditMenu();
  addSearchMenu();
  addScriptMenu();
  addToolsMenu();
  addViewMenu();
  addHelpMenu();

  Application.AddOnKeyDownBeforeHandler(@DoApplicationKeyDown);
  Screen.AddHandlerActiveControlChanged(@DoActiveControlChange);

  SimbaEvents.Register(Self, @DoSimbaEvent);
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

