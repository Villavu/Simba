{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_main;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Menus, ImgList, AnchorDockPanel, LMessages,
  simba.base, simba.settings, simba.ide_mouselogger;

const
  IMG_NONE = -1;
  IMG_COMPILE = 0;
  IMG_PLAY = 1;
  IMG_PAUSE = 2;
  IMG_STOP = 3;
  IMG_POWER = 4;
  IMG_NEW = 5;
  IMG_OPEN = 6;
  IMG_OPEN_RECENT = 7;
  IMG_SAVE = 8;
  IMG_SAVE_ALL = 9;
  IMG_TARGET = 10;
  IMG_PICK = 11;
  IMG_AREA = 12;
  IMG_ERASER = 13;
  IMG_PACKAGE = 14;
  IMG_COPY = 25;
  IMG_CUT = 26;
  IMG_PASTE = 27;
  IMG_FIND = 28;
  IMG_FIND_NEXT = 29;
  IMG_FIND_PREV = 30;
  IMG_REPLACE = 31;
  IMG_UNDO = 32;
  IMG_REDO = 33;
  IMG_OPTIONS = 34;
  IMG_COLORS = 35;
  IMG_SHAPE = 36;
  IMG_GITHUB = 37;
  IMG_BUG = 38;
  IMG_WRITE_BUG = 39;
  IMG_SIMBA = 40;
  IMG_FILE = 41;
  IMG_FOLDER = 42;
  IMG_FUNC = 43;
  IMG_PROC = 44;
  IMG_TYPE = 45;
  IMG_VAR = {55}46;
  IMG_CONST = 47;
  IMG_ENUM = 48;
  IMG_ANCHOR = 49;
  IMG_SELECT_ALL = 50;
  IMG_CLOSE = 51;
  IMG_CLOSE_ALL = 52;
  IMG_ARROW_UP = 53;
  IMG_ARROW_DOWN = 54;
  IMG_TICK = 62;
  IMG_PROPERTY = 63;

type
  TSimbaMainForm = class(TForm)
    DockPanel: TAnchorDockPanel;
    Images: TImageList;
    MenuItemFindInFiles: TMenuItem;
    MenuItemBackup: TMenuItem;
    MenuItemRunLast: TMenuItem;
    MenuItemShowCompilerHints: TMenuItem;
    MenuItemDownloadSimba: TMenuItem;
    MenuItemSelectLine: TMenuItem;
    MenuItemSelectWord: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemFindNext: TMenuItem;
    MenuItemFindPrev: TMenuItem;
    MenuItemSearchFindInFiles: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemGoto: TMenuItem;
    MenuItemLowercase: TMenuItem;
    MenuItemUppercase: TMenuItem;
    MainMenuSearch: TPopupMenu;
    RecentFilesPopup: TPopupMenu;
    MainMenuTools: TPopupMenu;
    MainMenuView: TPopupMenu;
    MainMenuHelp: TPopupMenu;
    MenuItemBackups: TMenuItem;
    MenuItemShapeBox: TMenuItem;
    MenuItemDocumentation: TMenuItem;
    MenuItemGithub: TMenuItem;
    MenuEdit: TMenuItem;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemACA: TMenuItem;
    MenuItemAssociateScripts: TMenuItem;
    MenuItemImageToString: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseTabs: TMenuItem;
    MenuItemColourHistory: TMenuItem;
    MenuItemCompile: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemDebugImage: TMenuItem;
    MenuItemDivider10: TMenuItem;
    MenuItemDivider11: TMenuItem;
    MenuItemDivider2: TMenuItem;
    MenuItemDivider3: TMenuItem;
    MenuItemDivider4: TMenuItem;
    MenuItemDivider5: TMenuItem;
    MenuItemDivider6: TMenuItem;
    MenuItemDTMEditor: TMenuItem;
    MenuItemEditor: TMenuItem;
    MenuItemExample: TMenuItem;
    MenuItemFileBrowser: TMenuItem;
    MenuItemFormatScript: TMenuItem;
    MenuItemFunctionList: TMenuItem;
    MenuItemLockLayout: TMenuItem;
    MenuItemMainExit: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemNotes: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemOpenRecent: TMenuItem;
    MenuItemOutput: TMenuItem;
    MenuItemPackages: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemPause: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemReportBug: TMenuItem;
    MenuItemResetLayout: TMenuItem;
    MenuItemRun: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAll: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSaveDefault: TMenuItem;
    MenuItemScript: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemStop: TMenuItem;
    MenuItemTrayIcon: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuTools: TMenuItem;
    MenuView: TMenuItem;
    MainMenuFile: TPopupMenu;
    MainMenuEdit: TPopupMenu;
    MainMenuScript: TPopupMenu;
    MainMenuPanel: TPanel;
    MainMenuSearchSep1: TMenuItem;
    MainMenuSearchSep2: TMenuItem;
    MainMenuSearchSep3: TMenuItem;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    TrayPopupExit: TMenuItem;

    procedure DoMenuItemPackagesClick(Sender: TObject);
    procedure DoMenuItemRunClick(Sender: TObject);
    procedure DoMenuItemCompileClick(Sender: TObject);
    procedure DoMenuItemStopClick(Sender: TObject);
    procedure DoMenuItemNewClick(Sender: TObject);
    procedure DoMenuItemOpenClick(Sender: TObject);
    procedure DoMenuItemSaveClick(Sender: TObject);
    procedure DoMenuItemSaveAllClick(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ImagesGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
    procedure MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
    procedure MenuClearOutputClick(Sender: TObject);
    procedure MenuCloseAllTabsClick(Sender: TObject);
    procedure MenuCloseTabClick(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuCutClick(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure MenuGotoClick(Sender: TObject);
    procedure MenuItemDownloadSimbaClick(Sender: TObject);
    procedure MenuItemSearchFindInFilesClick(Sender: TObject);
    procedure MenuItemRunLastClick(Sender: TObject);
    procedure MenuItemSelectLineClick(Sender: TObject);
    procedure MenuItemSelectWordClick(Sender: TObject);
    procedure MenuItemBackupsClick(Sender: TObject);
    procedure MenuItemLowercaseClick(Sender: TObject);
    procedure MenuItemShapeBoxClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemACAClick(Sender: TObject);
    procedure MenuItemAssociateScriptsClick(Sender: TObject);
    procedure MenuItemImageToStringClick(Sender: TObject);
    procedure MenuItemDocumentationClick(Sender: TObject);
    procedure MenuItemDTMEditorClick(Sender: TObject);
    procedure MenuItemFindNextClick(Sender: TObject);
    procedure MenuItemFindPrevClick(Sender: TObject);
    procedure MenuItemFormatScriptClick(Sender: TObject);
    procedure MenuItemGithubClick(Sender: TObject);
    procedure MenuItemLockLayoutClick(Sender: TObject);
    procedure MenuItemReportBugClick(Sender: TObject);
    procedure MenuItemResetLayoutClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure DoMenuItemShowCompilerHintsClick(Sender: TObject);
    procedure MenuItemTrayIconClick(Sender: TObject);
    procedure MenuItemUppercaseClick(Sender: TObject);
    procedure MenuNewTemplateClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuReplaceClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuSaveAsDefaultClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure RecentFilesPopupPopup(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayPopupExitClick(Sender: TObject);
  protected
    FRecentFiles: TStringList;
    FMouseLogger: TSimbaMouseLogger;

    procedure AddRecentFile(FileName: String);

    procedure DoResetDocking;
    procedure DoDefaultDocking;

    procedure SetupCompleted;

    procedure DoSettingChanged_CustomFontSize(Setting: TSimbaSetting);
    procedure DoSettingChanged_LockLayout(Setting: TSimbaSetting);
    procedure DoSettingChanged_TrayIconVisible(Setting: TSimbaSetting);
    procedure DoSettingChanged_ShowCompilerHints(Setting: TSimbaSetting);

    procedure HandleRecentFileClick(Sender: TObject);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure HandleFormCreated(Sender: TObject; Form: TCustomForm);

    // Handle main menu shortcuts if editor is focused
    procedure DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoTabLoaded(Sender: TObject);

    procedure SetCustomFontSize(Value: Integer);
    procedure SetLayoutLocked(Value: Boolean);
    procedure SetTrayIconVisible(Value: Boolean);

    procedure DoApplicationParameters;
    procedure DoFocusEditor;
    procedure UpdateTitle;
  public
    procedure Setup;
  end;

var
  SimbaMainForm: TSimbaMainForm;

implementation

{$R *.lfm}

uses
  LazFileUtils, AnchorDocking, LCLType,

  simba.ide_initialization, simba.ide_events, simba.ide_utils,
  simba.ide_mainstatusbar, simba.ide_mainmenubar, simba.ide_maintoolbar,
  simba.ide_scriptbackup, simba.ide_associate,

  simba.form_shapebox, simba.form_openexample, simba.form_colorpickhistory,
  simba.form_debugimage, simba.form_imagestring, simba.form_about,
  simba.form_findinfiles, simba.form_output, simba.form_filebrowser,
  simba.form_notes, simba.form_settings, simba.form_tabs,
  simba.form_functionlist, simba.form_downloadsimba, simba.form_backups,

  simba.ide_tab,
  simba.aca, simba.dtmeditor, simba.env, simba.ide_dockinghelpers, simba.nativeinterface,
  simba.ide_simpleformatter, simba.ide_theme,
  simba.threading, simba.ide_editor, simba.vartype_string, simba.misc;

procedure TSimbaMainForm.HandleException(Sender: TObject; E: Exception);

  procedure Dump(Addr: Pointer; List: TStringList);
  begin
    // preventing another exception, while dumping stack trace
    try
      List.Add(BackTraceStrFunc(Addr));
    except
      List.Add(SysBackTraceStr(Addr));
    end;
  end;

  procedure DumpStack(List: TStringList);
  var
    FrameCount, FrameIndex: Integer;
    Frames: PPointer;
  begin
    Dump(ExceptAddr, List);

    FrameCount := ExceptFrameCount;
    Frames := ExceptFrames;
    for FrameIndex := 0 to FrameCount - 1 do
      Dump(Frames[FrameIndex], List);
  end;

var
  Log: TStringList;
  FileName, Message: String;
begin
  try
    Log := TStringList.Create();
    Log.Add('Simba %d encountered an unhandled exception.', [SIMBA_VERSION]);
    Log.Add('Simba commit: %s', [SIMBA_COMMIT]);
    Log.Add('');
    Log.Add('Exception: %s', [E.Message]);
    Log.Add('Exception Class: %s', [E.ClassName]);
    Log.Add('');

    DumpStack(Log);

    FileName := SimbaEnv.DataPath + FormatDateTime('dd-mm_hh-mm-ss', Now()) + '.crash';

    Log.SaveToFile(FileName);
    Log.Free();

    Message := '%s'                                                     + LineEnding +
               ''                                                       + LineEnding +
               'Press OK to save your scripts and close. (Recommended)' + LineEnding +
               'Press Cancel to ignore and risk data corruption.'       + LineEnding +
               ''                                                       + LineEnding +
               'A crash log has been saved in the data directory.';

    if MessageDlg(Format(Message, [E.Message, ExtractRelativePath(SimbaEnv.DataPath, FileName)]), mtError, mbOKCancel, 0) = mrOk then
    begin
      SimbaTabsForm.CloseAllTabs();

      Halt(1);
    end;
  except
    // circular exception ...
  end;
end;

procedure TSimbaMainForm.SetCustomFontSize(Value: Integer);
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    Screen.CustomForms[I].HandleNeeded();
    Screen.CustomForms[I].Font.Size := Value;
  end;
end;

procedure TSimbaMainForm.SetLayoutLocked(Value: Boolean);
begin
  MenuItemLockLayout.Checked := Value;

  DockMaster.ShowHeader := not Value;
  DockMaster.AllowDragging := not Value;
end;

procedure TSimbaMainForm.SetTrayIconVisible(Value: Boolean);
begin
  MenuItemTrayIcon.Checked := Value;

  TrayIcon.Visible := Value;
end;

procedure TSimbaMainForm.DoApplicationParameters;
begin
  if (Application.ParamCount > 0) then
  begin
    if (Application.ParamCount = 1) and FileExists(Application.Params[1]) then
      SimbaTabsForm.Open(Application.Params[1])
    else
    if Application.HasOption('open') and FileExists(Application.Params[Application.ParamCount]) then
    begin
      SimbaTabsForm.Open(Application.Params[Application.ParamCount]);

      if Application.HasOption('compile') then
        SimbaMainToolBar.ButtonCompile.Click();
      if Application.HasOption('run') then
        SimbaMainToolBar.ButtonRun.Click();
    end;
  end;
end;

procedure TSimbaMainForm.DoFocusEditor;
begin
  if Assigned(SimbaTabsForm.CurrentEditor) then
    if SimbaTabsForm.CurrentEditor.CanSetFocus() then
      SimbaTabsForm.CurrentEditor.SetFocus();
end;

procedure TSimbaMainForm.UpdateTitle;
begin
  Caption := Format('Simba %.1f', [SIMBA_VERSION / 1000]);
end;

procedure TSimbaMainForm.HandleFormCreated(Sender: TObject; Form: TCustomForm);
begin
  if (SimbaSettings.General.CustomFontSize.Value > 0) then
    Form.Font.Size := SimbaSettings.General.CustomFontSize.Value;
end;

procedure TSimbaMainForm.HandleRecentFileClick(Sender: TObject);
begin
  SimbaTabsForm.Open(TMenuItem(Sender).Hint, True);
end;

procedure TSimbaMainForm.MenuItemAssociateScriptsClick(Sender: TObject);
const
  Message = 'Would you like to associate Simba files with this Simba?'                                   + LineEnding +
            'This means when opening a .simba file the file will be opened using this Simba executable.' + LineEnding +
            'It also adds right click actions to run the script.';
begin
  {$IFDEF WINDOWS}
  if (MessageDlg(Message, mtConfirmation, mbYesNo, 0) = mrYes) then
    Associate();
  {$ENDIF}
end;

procedure TSimbaMainForm.MenuNewTemplateClick(Sender: TObject);
begin
  SimbaOpenExampleForm.ShowModal();
end;

procedure TSimbaMainForm.RecentFilesPopupPopup(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  RecentFilesPopup.Items.Clear();

  I := 0;
  while (I < FRecentFiles.Count) do
  begin
    if (not FileExists(FRecentFiles[I])) then
    begin
      FRecentFiles.Delete(I);

      Continue;
    end;

    Item := TMenuItem.Create(RecentFilesPopup);
    Item.Caption := ShortDisplayFilename(FRecentFiles[I], 80);
    Item.OnClick := @HandleRecentFileClick;
    Item.Hint := FRecentFiles[I];

    RecentFilesPopup.Items.Add(Item);

    Inc(I);
  end;
end;

procedure TSimbaMainForm.MenuItemFormatScriptClick(Sender: TObject);
var
  Script: String;
begin
  try
    SimbaTabsForm.CurrentEditor.BeginUndoBlock();

    try
      if SimbaTabsForm.CurrentEditor.SelAvail then
        SimbaTabsForm.CurrentEditor.SelText := FormatScript(SimbaTabsForm.CurrentEditor.SelText)
      else
      begin
        Script := SimbaTabsForm.CurrentEditor.Text;

        SimbaTabsForm.CurrentEditor.ClearAll();
        SimbaTabsForm.CurrentEditor.InsertTextAtCaret(FormatScript(Script));
      end;
    finally
      SimbaTabsForm.CurrentEditor.EndUndoBlock();
    end;
  except
    on E: Exception do
      ShowMessage('Exception while formatting script: ' + E.Message);
  end;
end;

procedure TSimbaMainForm.MenuItemGithubClick(Sender: TObject);
begin
  SimbaNativeInterface.OpenURL(SIMBA_GITHUB_URL);
end;

procedure TSimbaMainForm.MenuItemAboutClick(Sender: TObject);
begin
  SimbaAboutForm.ShowModal();
end;

procedure TSimbaMainForm.TrayPopupExitClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaMainForm.AddRecentFile(FileName: String);
begin
  if FRecentFiles.IndexOf(FileName) >= 0 then
    FRecentFiles.Delete(FRecentFiles.IndexOf(FileName));
  FRecentFiles.Insert(0, FileName);
  while (FRecentFiles.Count > 10) do
    FRecentFiles.Pop();
end;

procedure TSimbaMainForm.DoResetDocking;
var
  I: Integer;
begin
  if (MessageDlg('Reset to default layout?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    Exit;

  SimbaSettings.General.Layout.Value := '';
  SimbaSettings.General.LockLayout.Value := False;

  Hide();
  WindowState := wsNormal;
  for I := 0 to Screen.CustomFormCount - 1 do
    if (Screen.CustomForms[I].HostDockSite is TCustomForm) then
    begin
      if (DockMaster.GetAnchorSite(Screen.CustomForms[I]) <> nil) then
        DockMaster.GetAnchorSite(Screen.CustomForms[I]).Visible := False;
      DockMaster.ManualFloat(Screen.CustomForms[I]);
      if (DockMaster.GetAnchorSite(Screen.CustomForms[I]) <> nil) then
        DockMaster.GetAnchorSite(Screen.CustomForms[I]).Header.Visible := True;
    end;

  DoDefaultDocking();
end;

procedure TSimbaMainForm.DoDefaultDocking;
var
  Splitter: TAnchorDockSplitter;
begin
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaTabsForm), DockPanel, alClient);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaOutputForm), DockPanel, alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFunctionListForm), DockPanel, alLeft);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFileBrowserForm), DockPanel, alRight);

  DockMaster.MakeVisible(SimbaTabsForm, False);
  DockMaster.MakeVisible(SimbaOutputForm, False);
  DockMaster.MakeVisible(SimbaFunctionListForm, False);
  DockMaster.MakeVisible(SimbaFileBrowserForm, False);
  DockMaster.ScaleOnResize := False;

  Width := 1200;
  Height := 850;

  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaTabsForm), akLeft, Splitter) then
    Splitter.SetSplitterPosition(250);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaTabsForm), akRight, Splitter) then
    Splitter.SetSplitterPosition(1200 - 250);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaTabsForm), akBottom, Splitter) then
    Splitter.SetSplitterPosition(350);

  Dockmaster.ScaleOnResize := True;

  DockMaster.GetAnchorSite(SimbaTabsForm).Header.Visible := False;
  DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

  MoveToDefaultPosition();
  EnsureVisible();
end;

procedure TSimbaMainForm.Setup;
begin
  // Register events etc
  Application.CaptureExceptions := True;
  Application.OnException := @Self.HandleException;
  Application.AddOnKeyDownBeforeHandler(@DoApplicationKeyDown);

  Screen.AddHandlerFormAdded(@Self.HandleFormCreated, True);

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_LOADED, @DoTabLoaded);

  with SimbaSettings do
  begin
    RegisterChangeHandler(Self, General.CustomFontSize, @DoSettingChanged_CustomFontSize, True);
    RegisterChangeHandler(Self, General.LockLayout, @DoSettingChanged_LockLayout, True);
    RegisterChangeHandler(Self, General.TrayIconVisible, @DoSettingChanged_TrayIconVisible, True);
    RegisterChangeHandler(Self, Compiler.ShowHints, @DoSettingChanged_ShowCompilerHints, True);
  end;

  // Create things
  FRecentFiles := TStringList.Create();
  FRecentFiles.Text := SimbaSettings.General.RecentFiles.Value;

  FMouseLogger := TSimbaMouseLogger.Create();

  // Docking
  BeginFormUpdate();
  try
    DockMaster.BeginUpdate();
    DockMaster.SplitterWidth := Scale96ToScreen(8);
    DockMaster.HeaderClass := TSimbaAnchorDockHeader;
    DockMaster.SplitterClass := TSimbaAnchorDockSplitter;
    DockMaster.SiteClass := TSimbaAnchorDockHostSite;
    DockMaster.HideHeaderCaptionFloatingControl := False;
    DockMaster.HeaderAlignTop := $FFFFFF;
    DockMaster.PageAreaInPercent := 0;
    DockMaster.HeaderHint := 'Use the mouse to drag and dock this window';
    DockMaster.MakeDockPanel(DockPanel, admrpChild);
    DockMaster.DragTreshold := 40;

    DockMaster.MakeDockable(SimbaTabsForm, MenuItemEditor);
    DockMaster.MakeDockable(SimbaOutputForm, MenuItemOutput);
    DockMaster.MakeDockable(SimbaFileBrowserForm, MenuItemFileBrowser);
    DockMaster.MakeDockable(SimbaFunctionListForm, MenuItemFunctionList);
    DockMaster.MakeDockable(SimbaNotesForm, MenuItemNotes);
    DockMaster.MakeDockable(SimbaDebugImageForm, MenuItemDebugImage);
    DockMaster.MakeDockable(SimbaColorPickHistoryForm, MenuItemColourHistory);
    DockMaster.MakeDockable(SimbaBackupsForm, MenuItemBackup);
    DockMaster.MakeDockable(SimbaFindInFilesForm, MenuItemFindInFiles);

    if (SimbaSettings.General.Layout.Value <> '') then
    begin
      DockMaster.LoadLayout(SimbaSettings.General.Layout.Value);

      if (DockMaster.GetAnchorSite(SimbaTabsForm) <> nil) then
        DockMaster.GetAnchorSite(SimbaTabsForm).Header.Visible := False;
      if (DockMaster.GetAnchorSite(SimbaOutputForm) <> nil) then
        DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

      EnsureVisible();
    end else
      QueueOnMainThread(@DoDefaultDocking);
  finally
    DockMaster.EndUpdate();

    EndFormUpdate();
  end;

  // Main title
  UpdateTitle();

  // Add a tab
  SimbaTabsForm.AddTab();

  // If first simba launch, associate
  if SimbaSettings.FirstLaunch then
    QueueOnMainThread(@MenuItemAssociateScripts.Click);

  QueueOnMainThread(@DoApplicationParameters); // open/compile/run parameters
  QueueOnMainThread(@DoFocusEditor); // finally focus the tab
end;

procedure TSimbaMainForm.FormDestroy(Sender: TObject);
begin
  if (FMouseLogger <> nil) then
  begin
    FMouseLogger.Terminate();
    FMouseLogger.WaitFor();

    FreeAndNil(FMouseLogger);
  end;

  if (FRecentFiles <> nil) then
  begin
    SimbaSettings.General.RecentFiles.Value := FRecentFiles.Text;

    FreeAndNil(FRecentFiles);
  end;

  SimbaSettings.Save();
end;

procedure TSimbaMainForm.DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function isEditor: Boolean;
  begin
    Result := SimbaTabsForm.IsParentOf(Screen.ActiveControl) and (Screen.ActiveControl is TSimbaEditor) and (TSimbaEditor(Screen.ActiveControl).Keystrokes.FindKeycode(Key, Shift) = -1);
  end;

var
  Msg: TLMKey;
begin
  // quick exit: cant be anything we want.
  if (Shift * [ssShift, ssAlt, ssCtrl, ssMeta, ssAltGr] = []) then
    Exit;

  // Only check these if editor is focused
  if isEditor() then
  begin
    Msg := Default(TLMKey);
    Msg.CharCode := Key;
    if (ssAlt in Shift) then
      Msg.KeyData := MK_ALT;

    if MainMenuFile.IsShortcut(Msg)   or MainMenuEdit.IsShortcut(Msg) or
       MainMenuScript.IsShortcut(Msg) or MainMenuSearch.IsShortcut(Msg) then
    begin
      SimbaMainMenuBar.MenuBar.HotIndex := -1;

      Key := 0;
    end;
  end;
end;

procedure TSimbaMainForm.FormWindowStateChange(Sender: TObject);
begin
  case WindowState of
    wsMinimized: DockMaster.Minimized();
    wsNormal:    DockMaster.Restored();
  end;
end;

procedure TSimbaMainForm.ImagesGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := ImageWidthForDPI(APPI);
end;

procedure TSimbaMainForm.MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

procedure TSimbaMainForm.MenuItemDocumentationClick(Sender: TObject);
begin
  SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL);
end;

procedure TSimbaMainForm.MenuItemACAClick(Sender: TObject);
begin
  TSimbaACAForm.Create(SimbaMainToolBar.WindowSelection).ShowOnTop();
end;

procedure TSimbaMainForm.DoMenuItemRunClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonRun.Click();
end;

procedure TSimbaMainForm.DoMenuItemPackagesClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonPackage.Click();
end;

procedure TSimbaMainForm.DoMenuItemStopClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonStop.Click();
end;

procedure TSimbaMainForm.DoMenuItemCompileClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonCompile.Click();
end;

procedure TSimbaMainForm.DoSettingChanged_CustomFontSize(Setting: TSimbaSetting);
begin
  SetCustomFontSize(Setting.Value);
end;

procedure TSimbaMainForm.DoSettingChanged_LockLayout(Setting: TSimbaSetting);
begin
  SetLayoutLocked(Setting.Value);
end;

procedure TSimbaMainForm.DoSettingChanged_TrayIconVisible(Setting: TSimbaSetting);
begin
  SetTrayIconVisible(Setting.Value);
end;

procedure TSimbaMainForm.DoSettingChanged_ShowCompilerHints(Setting: TSimbaSetting);
begin
  MenuItemShowCompilerHints.Checked := Setting.Value;
end;

procedure TSimbaMainForm.MenuCloseTabClick(Sender: TObject);
begin
  SimbaTabsForm.CloseTab(SimbaTabsForm.CurrentTab, True);
end;

procedure TSimbaMainForm.MenuCopyClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentEditor <> nil) then
    SimbaTabsForm.CurrentEditor.CopyToClipboard();
end;

procedure TSimbaMainForm.MenuCutClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentEditor <> nil) then
    SimbaTabsForm.CurrentEditor.CutToClipboard();
end;

procedure TSimbaMainForm.MenuExitClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TSimbaMainForm.MenuGotoClick(Sender: TObject);
var
  Value: String;
begin
  if SimbaTabsForm.CurrentEditor <> nil then
  begin
    Value := '';
    if InputQuery('Goto line', 'Goto line:', Value) and Value.IsInteger() then
      SimbaTabsForm.CurrentEditor.TopLine := StrToInt(Value) - (SimbaTabsForm.CurrentEditor.LinesInWindow div 2);
  end;
end;

procedure TSimbaMainForm.MenuItemDownloadSimbaClick(Sender: TObject);
begin
  SimbaDownloadSimbaForm.ShowModal();
end;

procedure TSimbaMainForm.MenuItemSearchFindInFilesClick(Sender: TObject);
begin
  MenuItemFindInFiles.Checked := True;
  if Assigned(MenuItemFindInFiles.OnClick) then
    MenuItemFindInFiles.OnClick(MenuItemFindInFiles);
end;

procedure TSimbaMainForm.MenuItemRunLastClick(Sender: TObject);
begin
  if Assigned(SimbaTabsForm.PreviousTab) then
    SimbaTabsForm.PreviousTab.Run(SimbaMainToolBar.WindowSelection);
end;

procedure TSimbaMainForm.MenuItemSelectLineClick(Sender: TObject);
begin
  if Assigned(SimbaTabsForm.CurrentEditor) then
    SimbaTabsForm.CurrentEditor.SelectLine();
end;

procedure TSimbaMainForm.MenuItemSelectWordClick(Sender: TObject);
begin
  if Assigned(SimbaTabsForm.CurrentEditor) then
    SimbaTabsForm.CurrentEditor.SelectWord();
end;

procedure TSimbaMainForm.MenuItemLowercaseClick(Sender: TObject);
begin
  if Assigned(SimbaTabsForm.CurrentEditor) then
    if SimbaTabsForm.CurrentEditor.SelAvail then
      SimbaTabsForm.CurrentEditor.SelText := LowerCase(SimbaTabsForm.CurrentEditor.SelText);
end;

procedure TSimbaMainForm.MenuItemBackupsClick(Sender: TObject);
begin
  MenuItemBackup.Checked := True;
  if Assigned(MenuItemBackup.OnClick) then
    MenuItemBackup.OnClick(MenuItemBackup);
end;

procedure TSimbaMainForm.MenuItemShapeBoxClick(Sender: TObject);
begin
  SimbaShapeBoxForm.Show();
end;

procedure TSimbaMainForm.MenuClearOutputClick(Sender: TObject);
begin
  SimbaOutputForm.ActiveOutputBox.Empty();
end;

procedure TSimbaMainForm.MenuFileClick(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  MenuItemOpenRecent.Clear();

  I := 0;
  while (I < FRecentFiles.Count) do
  begin
    if (not FileExists(FRecentFiles[I])) then
    begin
      FRecentFiles.Delete(I);

      Continue;
    end;

    Item := TMenuItem.Create(MenuItemOpenRecent);
    Item.Caption := ShortDisplayFilename(FRecentFiles[I], 80);
    Item.OnClick := @HandleRecentFileClick;
    Item.Hint := FRecentFiles[I];

    MenuItemOpenRecent.Add(Item);

    Inc(I);
  end;
end;

procedure TSimbaMainForm.MenuSaveAsDefaultClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to overwrite the default script?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    SimbaSettings.Editor.DefaultScript.Value := SimbaTabsForm.CurrentEditor.Text;
end;

procedure TSimbaMainForm.MenuCloseAllTabsClick(Sender: TObject);
begin
  if SimbaTabsForm.CloseAllTabs() then
    SimbaTabsForm.AddTab();
end;

procedure TSimbaMainForm.DoMenuItemNewClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonNew.Click();
end;

procedure TSimbaMainForm.DoMenuItemOpenClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonOpen.Click();
end;

procedure TSimbaMainForm.DoMenuItemSaveClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonSave.Click();
end;

procedure TSimbaMainForm.DoMenuItemSaveAllClick(Sender: TObject);
begin
  SimbaMainToolBar.ButtonSaveAll.Click();
end;

procedure TSimbaMainForm.MenuItemDTMEditorClick(Sender: TObject);
begin
  TSimbaDTMEditorForm.Create(SimbaMainToolBar.WindowSelection).ShowOnTop();
end;

procedure TSimbaMainForm.MenuFindClick(Sender: TObject);
begin
  SimbaTabsForm.Find();
end;

procedure TSimbaMainForm.MenuItemFindNextClick(Sender: TObject);
begin
  SimbaTabsForm.FindNext();
end;

procedure TSimbaMainForm.MenuItemFindPrevClick(Sender: TObject);
begin
  SimbaTabsForm.FindPrevious();
end;

procedure TSimbaMainForm.MenuReplaceClick(Sender: TObject);
begin
  SimbaTabsForm.Replace();
end;

procedure TSimbaMainForm.MenuPasteClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentEditor <> nil) then
    SimbaTabsForm.CurrentEditor.PasteFromClipboard();
end;

procedure TSimbaMainForm.MenuRedoClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentEditor <> nil) then
    SimbaTabsForm.CurrentEditor.Redo();
end;

procedure TSimbaMainForm.MenuSelectAllClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentEditor <> nil) then
    SimbaTabsForm.CurrentEditor.SelectAll();
end;

procedure TSimbaMainForm.MenuUndoClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentEditor <> nil) then
    SimbaTabsForm.CurrentEditor.Undo();
end;

procedure TSimbaMainForm.MenuItemResetLayoutClick(Sender: TObject);
begin
  QueueOnMainThread(@DoResetDocking);
end;

procedure TSimbaMainForm.MenuItemLockLayoutClick(Sender: TObject);
begin
  SimbaSettings.General.LockLayout.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaMainForm.MenuItemTrayIconClick(Sender: TObject);
begin
  SimbaSettings.General.TrayIconVisible.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaMainForm.MenuItemUppercaseClick(Sender: TObject);
begin
  if Assigned(SimbaTabsForm.CurrentEditor) then
    if SimbaTabsForm.CurrentEditor.SelAvail then
      SimbaTabsForm.CurrentEditor.SelText := UpperCase(SimbaTabsForm.CurrentEditor.SelText);
end;

procedure TSimbaMainForm.MenuSaveAsClick(Sender: TObject);
begin
  if (SimbaTabsForm.CurrentTab <> nil) then
    SimbaTabsForm.CurrentTab.Save('');
end;

procedure TSimbaMainForm.MenuItemImageToStringClick(Sender: TObject);
begin
  SimbaImageStringForm.Show();
end;

procedure TSimbaMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not SimbaTabsForm.CloseAllTabs()) then
    CloseAction := caNone
  else
  begin
    CloseAction := caFree;
    if (WindowState <> wsMinimized) then
      SimbaSettings.General.Layout.Value := DockMaster.SaveLayout();

    Visible := False;
  end;
end;

procedure TSimbaMainForm.DoTabLoaded(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    AddRecentFile(TSimbaScriptTab(Sender).ScriptFileName);
end;

procedure TSimbaMainForm.MenuEditClick(Sender: TObject);
begin
  if SimbaTabsForm.CurrentEditor <> nil then
    with SimbaTabsForm.CurrentEditor do
    begin
      MenuItemCut.Enabled := SelText <> '';
      MenuItemCopy.Enabled := SelText <> '';
      MenuItemPaste.Enabled := SelText <> '';
    end;
end;

procedure TSimbaMainForm.MenuItemReportBugClick(Sender: TObject);
begin
  SimbaNativeInterface.OpenURL(SIMBA_BUGS_URL);
end;

procedure TSimbaMainForm.MenuItemSettingsClick(Sender: TObject);
begin
  SimbaSettingsForm.ShowModal();
end;

procedure TSimbaMainForm.DoMenuItemShowCompilerHintsClick(Sender: TObject);
begin
  SimbaSettings.Compiler.ShowHints.Value := not SimbaSettings.Compiler.ShowHints.Value;
end;

procedure TSimbaMainForm.TrayIconClick(Sender: TObject);
begin
  ShowOnTop();
  if CanSetFocus() then
    SetFocus();
end;

procedure TSimbaMainForm.SetupCompleted;
begin
  if SimbaSettings.FirstLaunch then
    MenuItemAssociateScripts.Click();

  if (Application.ParamCount > 0) then
  begin
    if (Application.ParamCount = 1) and FileExists(Application.Params[1]) then
      SimbaTabsForm.Open(Application.Params[1])
    else
    if Application.HasOption('open') and FileExists(Application.Params[Application.ParamCount]) then
    begin
      SimbaTabsForm.Open(Application.Params[Application.ParamCount]);

      if Application.HasOption('compile') then
        SimbaMainToolBar.ButtonCompile.Click();
      if Application.HasOption('run') then
        SimbaMainToolBar.ButtonRun.Click();
    end;
  end;
end;

procedure SetupSimbaForm;
begin
  if (SimbaMainForm = nil) then
    SimbaException('SimbaForm is nil');

  SimbaMainForm.Setup();
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@SetupSimbaForm, 'Setup SimbaForm');

end.
