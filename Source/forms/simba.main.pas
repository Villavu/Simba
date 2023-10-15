{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.main;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  Menus, ImgList, LMessages, AnchorDockPanel,
  simba.settings, simba.mufasatypes, simba.mouselogger, simba.areaselector,
  simba.scriptinstance, simba.component_menubar, simba.process;

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
  IMG_PICK = 10;
  IMG_TARGET = 11;
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

type
  TSimbaForm = class(TForm)
    DockPanel: TAnchorDockPanel;
    Images: TImageList;
    MenuItemSelectLine: TMenuItem;
    MenuItemSelectWord: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemFindNext: TMenuItem;
    MenuItemFindPrev: TMenuItem;
    MenuItemFindInFiles: TMenuItem;
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
    MenuItemConsole: TMenuItem;
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
    ToolbarButtonStop: TToolButton;
    PackageUpdateTimer: TTimer;
    ToolBar: TToolBar;
    ToolbarButtonClearOutput: TToolButton;
    ToolbarButtonColorPicker: TToolButton;
    ToolbarButtonCompile: TToolButton;
    ToolbarButtonOpen: TToolButton;
    ToolbarButtonPackages: TToolButton;
    ToolbarButtonPause: TToolButton;
    ToolbarButtonRun: TToolButton;
    ToolbarButtonSave: TToolButton;
    ToolbarButtonSaveAll: TToolButton;
    ToolbarButtonTargetSelector: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonAreaSelector: TToolButton;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    TrayPopupExit: TMenuItem;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
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
    procedure MenuItemFindInFilesClick(Sender: TObject);
    procedure MenuItemSelectLineClick(Sender: TObject);
    procedure MenuItemSelectWordClick(Sender: TObject);
    procedure MenuItemBackupsClick(Sender: TObject);
    procedure MenuItemLowercaseClick(Sender: TObject);
    procedure MenuItemShapeBoxClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemACAClick(Sender: TObject);
    procedure MenuItemAssociateScriptsClick(Sender: TObject);
    procedure MenuItemImageToStringClick(Sender: TObject);
    procedure MenuItemConsoleClick(Sender: TObject);
    procedure MenuItemDocumentationClick(Sender: TObject);
    procedure MenuItemDTMEditorClick(Sender: TObject);
    procedure MenuItemFindNextClick(Sender: TObject);
    procedure MenuItemFindPrevClick(Sender: TObject);
    procedure MenuItemFormatScriptClick(Sender: TObject);
    procedure MenuItemGithubClick(Sender: TObject);
    procedure MenuItemLockLayoutClick(Sender: TObject);
    procedure MenuItemReportBugClick(Sender: TObject);
    procedure MenuItemResetLayoutClick(Sender: TObject);
    procedure MenuItemScriptStateClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure MenuItemTrayIconClick(Sender: TObject);
    procedure MenuItemUppercaseClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuNewTemplateClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuReplaceClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuSaveAsDefaultClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure DoPackageMenuTimer(Sender: TObject);
    procedure RecentFilesPopupPopup(Sender: TObject);
    procedure ToolbarButtonColorPickerClick(Sender: TObject);
    procedure ToolbarButtonPackagesClick(Sender: TObject);
    procedure ToolbarButtonSaveAllClick(Sender: TObject);
    procedure ToolbarButtonSelectTargetClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ToolBarPaintButton(Sender: TToolButton; State: integer);
    procedure ToolButtonAreaSelectorClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayPopupExitClick(Sender: TObject);
  protected
    FToolbarImages: TImageList;

    FWindowSelection: TWindowHandle;
    FProcessSelection: TProcessID;
    FRecentFiles: TStringList;

    FMenuBar: TSimbaMainMenuBar;
    FMouseLogger: TSimbaMouseLogger;

    FAreaSelector: TSimbaAreaSelector;
    FAreaSelection: TBox;

    procedure AddRecentFile(FileName: String);
    procedure SetButtonStates(Instance: TSimbaScriptInstance);

    procedure DoResetDocking;
    procedure DoDefaultDocking;

    procedure SetupDocking;
    procedure SetupCompleted;

    procedure DoColorPicked(Data: PtrInt);

    procedure DoSettingChanged_Toolbar(Setting: TSimbaSetting);
    procedure DoSettingChanged_CustomFontSize(Setting: TSimbaSetting);
    procedure DoSettingChanged_LockLayout(Setting: TSimbaSetting);
    procedure DoSettingChanged_TrayIconVisible(Setting: TSimbaSetting);
    procedure DoSettingChanged_ConsoleVisible(Setting: TSimbaSetting);

    procedure HandleRecentFileClick(Sender: TObject);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure HandleFormCreated(Sender: TObject; Form: TCustomForm);

    procedure DoTabLoaded(Sender: TObject);
    procedure DoTabModified(Sender: TObject);
    procedure DoScriptTabChange(Sender: TObject);
    procedure DoScriptStateChange(Sender: TObject);

    procedure SetCustomFontSize(Value: Integer);
    procedure SetConsoleVisible(Value: Boolean);
    procedure SetLayoutLocked(Value: Boolean);
    procedure SetTrayIconVisible(Value: Boolean);
  public
    property WindowSelection: TWindowHandle read FWindowSelection;
    property ProcessSelection: TProcessID read FProcessSelection;
    property MenuBar: TSimbaMainMenuBar read FMenuBar;

    procedure Setup;
  end;

var
  SimbaForm: TSimbaForm;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, LazFileUtils, AnchorDocking, ATCanvasPrimitives, Types, GraphType,

  simba.shapeboxform, simba.openexampleform, simba.colorpickerhistoryform,
  simba.debugimageform, simba.imagetostringform, simba.aboutform, simba.findinfilesform,
  simba.outputform, simba.filebrowserform, simba.notesform, simba.settingsform,
  simba.functionlistform, simba.scripttabsform, simba.ide_mainstatusbar,
  simba.package_form, simba.package_autoupdater,
  simba.associate, simba.ide_initialization, simba.ide_events,
  simba.aca, simba.dtmeditor,
  simba.windowselector, simba.colorpicker,
  simba.env,
  simba.dockinghelpers, simba.nativeinterface,
  simba.scriptformatter, simba.windowhandle, simba.scripttab, simba.theme,
  simba.scriptbackup, simba.backupsform, simba.ide_utils, simba.threading;

procedure TSimbaForm.HandleException(Sender: TObject; E: Exception);

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
      SimbaScriptTabsForm.CloseAllTabs();

      Halt(1);
    end;
  except
    // circular exception ...
  end;
end;

procedure TSimbaForm.SetCustomFontSize(Value: Integer);
var
  I: Integer;
begin
  HandleNeeded();
  for I := 0 to Screen.CustomFormCount - 1 do
    Screen.CustomForms[I].Font.Size := Value;
end;

procedure TSimbaForm.SetConsoleVisible(Value: Boolean);
begin
  MenuItemConsole.Checked := Value;

  if Value then
    SimbaNativeInterface.ShowTerminal()
  else
    SimbaNativeInterface.HideTerminal();
end;

procedure TSimbaForm.SetLayoutLocked(Value: Boolean);
begin
  MenuItemLockLayout.Checked := Value;

  DockMaster.ShowHeader := not Value;
  DockMaster.AllowDragging := not Value;
end;

procedure TSimbaForm.SetTrayIconVisible(Value: Boolean);
begin
  MenuItemTrayIcon.Checked := Value;

  TrayIcon.Visible := Value;
end;

procedure TSimbaForm.HandleFormCreated(Sender: TObject; Form: TCustomForm);
begin
  if (SimbaSettings.General.CustomFontSize.Value > 0) then
    Form.Font.Size := SimbaSettings.General.CustomFontSize.Value;
end;

procedure TSimbaForm.HandleRecentFileClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Open(TMenuItem(Sender).Hint, True);
end;

procedure TSimbaForm.MenuItemAssociateScriptsClick(Sender: TObject);
const
  Message = 'Would you like to associate scripts with this Simba?' + LineEnding +
            'This means when opening a script, the script will be opened using this Simba executable.';
begin
  {$IFDEF WINDOWS}
  if MessageDlg(Message, mtConfirmation, mbYesNo, 0) = mrYes then
    Associate();
  {$ENDIF}
end;

procedure TSimbaForm.MenuNewTemplateClick(Sender: TObject);
begin
  SimbaOpenExampleForm.ShowModal();
end;

procedure TSimbaForm.DoPackageMenuTimer(Sender: TObject);
begin
  UpdatePackages();
end;

procedure TSimbaForm.RecentFilesPopupPopup(Sender: TObject);
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

procedure TSimbaForm.MenuItemFormatScriptClick(Sender: TObject);
var
  Script: String;
begin
  try
    SimbaScriptTabsForm.CurrentEditor.BeginUndoBlock();

    try
      if SimbaScriptTabsForm.CurrentEditor.SelAvail then
        SimbaScriptTabsForm.CurrentEditor.SelText := FormatScript(SimbaScriptTabsForm.CurrentEditor.SelText)
      else
      begin
        Script := SimbaScriptTabsForm.CurrentEditor.Text;

        SimbaScriptTabsForm.CurrentEditor.ClearAll();
        SimbaScriptTabsForm.CurrentEditor.InsertTextAtCaret(FormatScript(Script));
      end;
    finally
      SimbaScriptTabsForm.CurrentEditor.EndUndoBlock();
    end;
  except
    on E: Exception do
      ShowMessage('Exception while formatting script: ' + E.Message);
  end;
end;

procedure TSimbaForm.MenuItemGithubClick(Sender: TObject);
begin
  OpenURL(SIMBA_GITHUB_URL);
end;

procedure TSimbaForm.MenuItemAboutClick(Sender: TObject);
begin
  SimbaAboutForm.ShowModal();
end;

procedure TSimbaForm.TrayPopupExitClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaForm.AddRecentFile(FileName: String);
begin
  if FRecentFiles.IndexOf(FileName) >= 0 then
    FRecentFiles.Delete(FRecentFiles.IndexOf(FileName));
  FRecentFiles.Insert(0, FileName);
  while (FRecentFiles.Count > 10) do
    FRecentFiles.Pop();
end;

procedure TSimbaForm.SetButtonStates(Instance: TSimbaScriptInstance);
begin
  if Assigned(Instance) then
    case Instance.State of
      ESimbaScriptState.STATE_PAUSED:
        begin
          ToolbarButtonRun.Enabled := True;
          ToolbarButtonPause.Enabled := False;
          ToolbarButtonCompile.Enabled := True;

          ToolbarButtonStop.Enabled := True;
          ToolbarButtonStop.ImageIndex := IMG_STOP;
        end;

      ESimbaScriptState.STATE_STOP:
        begin
          ToolbarButtonRun.Enabled := False;
          ToolbarButtonPause.Enabled := False;
          ToolbarButtonCompile.Enabled := False;

          ToolbarButtonStop.Enabled := True;
          ToolbarButtonStop.ImageIndex := IMG_POWER;
        end;

      ESimbaScriptState.STATE_RUNNING:
        begin
          ToolbarButtonRun.Enabled := False;
          ToolbarButtonPause.Enabled := True;
          ToolbarButtonCompile.Enabled := False;

          ToolbarButtonStop.Enabled := True;
          ToolbarButtonStop.ImageIndex := IMG_STOP;
        end;

      ESimbaScriptState.STATE_NONE:
        begin
          ToolbarButtonRun.Enabled := True;
          ToolbarButtonPause.Enabled := False;
          ToolbarButtonCompile.Enabled := True;

          ToolbarButtonStop.Enabled := False;
          ToolbarButtonStop.ImageIndex := IMG_STOP;
        end;
     end
  else
  begin
    ToolbarButtonRun.Enabled := True;
    ToolbarButtonPause.Enabled := False;
    ToolbarButtonCompile.Enabled := True;

    ToolbarButtonStop.Enabled := False;
    ToolbarButtonStop.ImageIndex := IMG_STOP;
  end;
end;

procedure TSimbaForm.DoResetDocking;
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
      Screen.CustomForms[I].Hide();

      DockMaster.ManualFloat(Screen.CustomForms[I]);
      if (DockMaster.GetAnchorSite(Screen.CustomForms[I]) <> nil) then
        DockMaster.GetAnchorSite(Screen.CustomForms[I]).Header.Visible := True;
    end;

  DoDefaultDocking();
end;

procedure TSimbaForm.DoDefaultDocking;
var
  Splitter: TAnchorDockSplitter;
begin
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaScriptTabsForm), DockPanel, alClient);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaOutputForm), DockPanel, alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFunctionListForm), DockPanel, alLeft);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFileBrowserForm), DockPanel, alRight);

  DockMaster.MakeVisible(SimbaScriptTabsForm, False);
  DockMaster.MakeVisible(SimbaOutputForm, False);
  DockMaster.MakeVisible(SimbaFunctionListForm, False);
  DockMaster.MakeVisible(SimbaFileBrowserForm, False);
  DockMaster.ScaleOnResize:=False;

  Width := 1200;
  Height := 850;

  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akLeft, Splitter) then
    Splitter.SetSplitterPosition(250);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akRight, Splitter) then
    Splitter.SetSplitterPosition(1200 - 250);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akBottom, Splitter) then
    Splitter.SetSplitterPosition(350);

  Dockmaster.ScaleOnResize := True;

  DockMaster.GetAnchorSite(SimbaScriptTabsForm).Header.Visible := False;
  DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

  MoveToDefaultPosition();
  EnsureVisible();
end;

procedure TSimbaForm.Setup;
begin
  SimbaIDEEvents.RegisterMethodOnEditorLoaded(@DoTabLoaded);
  SimbaIDEEvents.RegisterMethodOnEditorModified(@DoTabModified);
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoTabModified); // Also do this
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoScriptTabChange);
  SimbaIDEEvents.RegisterMethodOnScriptStateChange(@DoScriptStateChange);

  with SimbaSettings do
  begin
    RegisterChangeHandler(Self, General.ToolbarSize, @DoSettingChanged_Toolbar, True);
    RegisterChangeHandler(Self, General.ToolbarPosition, @DoSettingChanged_Toolbar, True);
    RegisterChangeHandler(Self, General.ToolBarSpacing, @DoSettingChanged_Toolbar, True);

    RegisterChangeHandler(Self, General.CustomFontSize, @DoSettingChanged_CustomFontSize, True);
    RegisterChangeHandler(Self, General.LockLayout, @DoSettingChanged_LockLayout, True);
    RegisterChangeHandler(Self, General.TrayIconVisible, @DoSettingChanged_TrayIconVisible, True);
    RegisterChangeHandler(Self, General.ConsoleVisible, @DoSettingChanged_ConsoleVisible, True);
  end;

  Application.CaptureExceptions := True;
  Application.OnException := @Self.HandleException;
  Application.OnShortcut := @Self.FormShortCut;

  Screen.AddHandlerFormAdded(@Self.HandleFormCreated, True);

  FRecentFiles := TStringList.Create();
  FRecentFiles.Text := SimbaSettings.General.RecentFiles.Value;

  FToolbarImages := TImageList.Create(Self); // Create a copy so ImagesGetWidthForPPI is not used for toolbar
  FToolbarImages.Assign(Images);

  ToolBar.Color := SimbaTheme.ColorFrame;
  ToolBar.Images := FToolbarImages;

  FMouseLogger := TSimbaMouseLogger.Create();
  FMouseLogger.Hotkey := VK_F1;

  FMenuBar := TSimbaMainMenuBar.Create(Self);
  FMenuBar.Parent := MainMenuPanel;
  FMenuBar.Align := alTop;
  FMenuBar.AddMenu('File', MainMenuFile);
  FMenuBar.AddMenu('Edit', MainMenuEdit);
  FMenuBar.AddMenu('Search', MainMenuSearch);
  FMenuBar.AddMenu('Script', MainMenuScript);
  FMenuBar.AddMenu('Tools', MainMenuTools);
  FMenuBar.AddMenu('View', MainMenuView);
  FMenuBar.AddMenu('Help', MainMenuHelp);

  Color := SimbaTheme.ColorFrame;

  SimbaScriptTabsForm.AddTab();

  SetupDocking();

  // Finally, give the editor focus as default.
  if Assigned(SimbaScriptTabsForm.CurrentEditor) then
    if SimbaScriptTabsForm.CurrentEditor.CanSetFocus() then
      SimbaScriptTabsForm.CurrentEditor.SetFocus();

  SetupCompleted();
end;

procedure TSimbaForm.FormDestroy(Sender: TObject);
begin
  if (FAreaSelector <> nil) then
    FreeAndNil(FAreaSelector);

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

procedure TSimbaForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_MENU) and (not (ssCtrl in Shift)) and MenuBar.CanSetFocus() then
    MenuBar.SetFocus();
end;

procedure TSimbaForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  Handled := MainMenuFile.IsShortcut(Msg)   or MainMenuView.IsShortcut(Msg)   or
             MainMenuEdit.IsShortcut(Msg)   or MainMenuScript.IsShortcut(Msg) or
             MainMenuTools.IsShortcut(Msg)  or MainMenuHelp.IsShortcut(Msg)   or
             MainMenuSearch.IsShortcut(Msg) or (KeyDataToShiftState(Msg.KeyData) = [ssAlt]); // Suppress windows freaking out
end;

procedure TSimbaForm.FormWindowStateChange(Sender: TObject);
begin
  case WindowState of
    wsMinimized: DockMaster.Minimized();
    wsNormal:    DockMaster.Restored();
  end;
end;

procedure TSimbaForm.ImagesGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  AResultWidth := ImageWidthForDPI(APPI);
end;

procedure TSimbaForm.MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

procedure TSimbaForm.MenuItemDocumentationClick(Sender: TObject);
begin
  OpenURL(SIMBA_DOCS_URL);
end;

procedure TSimbaForm.MenuItemACAClick(Sender: TObject);
begin
  TSimbaACAForm.Create(WindowSelection).ShowOnTop();
end;

procedure TSimbaForm.MenuItemScriptStateClick(Sender: TObject);
type
  EAction = (Unknown, Compile, Run, Pause, Stop);

  function GetAction: EAction;
  begin
    if (Sender = MenuItemCompile) or (Sender = ToolbarButtonCompile) then Exit(Compile);
    if (Sender = MenuItemRun)     or (Sender = ToolbarButtonRun)     then Exit(Run);
    if (Sender = MenuItemPause)   or (Sender = ToolbarButtonPause)   then Exit(Pause);
    if (Sender = MenuItemStop)    or (Sender = ToolbarButtonStop)    then Exit(Stop);

    Result := Unknown;

    DebugLn('[TSimbaForm.MenuItemScriptStateClick]: Unknown component "' + Sender.ClassName + '"');
  end;

var
  CurrentTab: TSimbaScriptTab;
begin
  CurrentTab := SimbaScriptTabsForm.CurrentTab;

  if (CurrentTab <> nil) then
  try
    if (GetAction() in [Compile, Run]) then
    begin
      CurrentTab.OutputBox.Tab.Show();
      if SimbaSettings.OutputBox.ClearOnCompile.Value then
        CurrentTab.OutputBox.Empty();
    end;

    case GetAction() of
      Compile: CurrentTab.Compile();
      Run:     CurrentTab.Run(FWindowSelection);
      Pause:   CurrentTab.Pause();
      Stop:    CurrentTab.Stop();
    end;

    if CurrentTab.Editor.CanSetFocus() then
      CurrentTab.Editor.SetFocus();
  except
    on E: Exception do
      MessageDlg('Exception while changing script state: ' + E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TSimbaForm.DoColorPicked(Data: PtrInt);
begin
  MenuItemColourHistory.Checked := True;
  MenuItemColourHistory.OnClick(MenuItemColourHistory);

  if (SimbaColorPickerHistoryForm.ColorListBox.Items.Count > 0) then
    SimbaColorPickerHistoryForm.ColorListBox.ItemIndex := SimbaColorPickerHistoryForm.ColorListBox.Count - 1;
end;

procedure TSimbaForm.DoSettingChanged_Toolbar(Setting: TSimbaSetting);
var
  I: Integer;
begin
  if Setting.Equals(SimbaSettings.General.ToolbarPosition) then
  begin
    MainMenuPanel.Align := alNone;

    case String(Setting.Value) of
      'Top':   ToolBar.Align := alTop;
      'Left':  ToolBar.Align := alLeft;
      'Right': ToolBar.Align := alRight;
    end;

    MainMenuPanel.Align := alTop;
  end;

  ToolBar.ImagesWidth := SimbaSettings.General.ToolbarSize.Value;

  if (ToolBar.Align = alTop) then
  begin
    ToolBar.ButtonWidth  := Round(ToolBar.ImagesWidth * 1.50);
    ToolBar.ButtonHeight := Round(ToolBar.ImagesWidth * 1.75);
    for I := 0 to Toolbar.ButtonCount - 1 do
      if (ToolBar.Buttons[I].Style = tbsSeparator) then
        ToolBar.Buttons[I].Visible := True;
  end else
  begin
    ToolBar.ButtonWidth  := Round(ToolBar.ImagesWidth * 2.50);
    ToolBar.ButtonHeight := Round(ToolBar.ImagesWidth * 1.75);
    for I := 0 to Toolbar.ButtonCount - 1 do
      if (ToolBar.Buttons[I].Style = tbsSeparator) then
        ToolBar.Buttons[I].Visible := False;
  end;

  if Setting.Equals(SimbaSettings.General.ToolBarSpacing) then
    ToolBar.BorderSpacing.Around := Setting.Value;
end;

procedure TSimbaForm.DoSettingChanged_CustomFontSize(Setting: TSimbaSetting);
begin
  SetCustomFontSize(Setting.Value);
end;

procedure TSimbaForm.DoSettingChanged_LockLayout(Setting: TSimbaSetting);
begin
  SetLayoutLocked(Setting.Value);
end;

procedure TSimbaForm.DoSettingChanged_TrayIconVisible(Setting: TSimbaSetting);
begin
  SetTrayIconVisible(Setting.Value);
end;

procedure TSimbaForm.DoSettingChanged_ConsoleVisible(Setting: TSimbaSetting);
begin
  SetConsoleVisible(Setting.Value);
end;

procedure TSimbaForm.MenuCloseTabClick(Sender: TObject);
begin
  SimbaScriptTabsForm.CloseTab(SimbaScriptTabsForm.CurrentTab, True);
end;

procedure TSimbaForm.MenuCopyClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentEditor <> nil) then
    SimbaScriptTabsForm.CurrentEditor.CopyToClipboard();
end;

procedure TSimbaForm.MenuCutClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentEditor <> nil) then
    SimbaScriptTabsForm.CurrentEditor.CutToClipboard();
end;

procedure TSimbaForm.MenuExitClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TSimbaForm.MenuGotoClick(Sender: TObject);
var
  Value: String;
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
  begin
    Value := '';
    if InputQuery('Goto line', 'Goto line:', Value) and (StrToIntDef(Value, -1) > -1) then
      SimbaScriptTabsForm.CurrentEditor.TopLine := StrToInt(Value) - (SimbaScriptTabsForm.CurrentEditor.LinesInWindow div 2);
  end;
end;

procedure TSimbaForm.MenuItemFindInFilesClick(Sender: TObject);
begin
  SimbaFindInFilesForm.ShowModal();
end;

procedure TSimbaForm.MenuItemSelectLineClick(Sender: TObject);
begin
  if Assigned(SimbaScriptTabsForm.CurrentEditor) then
    SimbaScriptTabsForm.CurrentEditor.SelectLine();
end;

procedure TSimbaForm.MenuItemSelectWordClick(Sender: TObject);
begin
  if Assigned(SimbaScriptTabsForm.CurrentEditor) then
    SimbaScriptTabsForm.CurrentEditor.SelectWord();
end;

procedure TSimbaForm.MenuItemLowercaseClick(Sender: TObject);
begin
  if Assigned(SimbaScriptTabsForm.CurrentEditor) then
    if SimbaScriptTabsForm.CurrentEditor.SelAvail then
      SimbaScriptTabsForm.CurrentEditor.SelText := LowerCase(SimbaScriptTabsForm.CurrentEditor.SelText);
end;

procedure TSimbaForm.MenuItemBackupsClick(Sender: TObject);
begin
  SimbaBackupsForm.ShowModal();
end;

procedure TSimbaForm.MenuItemShapeBoxClick(Sender: TObject);
begin
  SimbaShapeBoxForm.Show();
end;

procedure TSimbaForm.MenuClearOutputClick(Sender: TObject);
begin
  SimbaOutputForm.CurrentTab.Empty();
end;

procedure TSimbaForm.MenuFileClick(Sender: TObject);
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

procedure TSimbaForm.MenuSaveAsDefaultClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to overwrite the default script?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    SimbaSettings.Editor.DefaultScript.Value := SimbaScriptTabsForm.CurrentEditor.Text;
end;

procedure TSimbaForm.MenuCloseAllTabsClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CloseAllTabs() then
    SimbaScriptTabsForm.AddTab();
end;

procedure TSimbaForm.MenuNewClick(Sender: TObject);
begin
  SimbaScriptTabsForm.AddTab();
end;

procedure TSimbaForm.MenuItemDTMEditorClick(Sender: TObject);
begin
 TSimbaDTMEditorForm.Create(WindowSelection).ShowOnTop();
end;

procedure TSimbaForm.MenuOpenClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Open();
end;

procedure TSimbaForm.MenuFindClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Find();
end;

procedure TSimbaForm.MenuItemFindNextClick(Sender: TObject);
begin
  SimbaScriptTabsForm.FindNext();
end;

procedure TSimbaForm.MenuItemFindPrevClick(Sender: TObject);
begin
  SimbaScriptTabsForm.FindPrevious();
end;

procedure TSimbaForm.MenuReplaceClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Replace();
end;

procedure TSimbaForm.MenuPasteClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentEditor <> nil) then
    SimbaScriptTabsForm.CurrentEditor.PasteFromClipboard();
end;

procedure TSimbaForm.MenuRedoClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentEditor <> nil) then
    SimbaScriptTabsForm.CurrentEditor.Redo();
end;

procedure TSimbaForm.MenuSaveClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentTab <> nil) then
    SimbaScriptTabsForm.CurrentTab.Save(SimbaScriptTabsForm.CurrentTab.ScriptFileName);
end;

procedure TSimbaForm.MenuSelectAllClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentEditor <> nil) then
    SimbaScriptTabsForm.CurrentEditor.SelectAll();
end;

procedure TSimbaForm.MenuUndoClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentEditor <> nil) then
    SimbaScriptTabsForm.CurrentEditor.Undo();
end;

procedure TSimbaForm.MenuItemResetLayoutClick(Sender: TObject);
begin
  QueueOnMainThread(@DoResetDocking);
end;

procedure TSimbaForm.MenuItemConsoleClick(Sender: TObject);
begin
  SimbaSettings.General.ConsoleVisible.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaForm.MenuItemLockLayoutClick(Sender: TObject);
begin
  SimbaSettings.General.LockLayout.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaForm.MenuItemTrayIconClick(Sender: TObject);
begin
  SimbaSettings.General.TrayIconVisible.Value := TMenuItem(Sender).Checked;
end;

procedure TSimbaForm.MenuItemUppercaseClick(Sender: TObject);
begin
  if Assigned(SimbaScriptTabsForm.CurrentEditor) then
    if SimbaScriptTabsForm.CurrentEditor.SelAvail then
      SimbaScriptTabsForm.CurrentEditor.SelText := UpperCase(SimbaScriptTabsForm.CurrentEditor.SelText);
end;

procedure TSimbaForm.ToolbarButtonPackagesClick(Sender: TObject);
begin
  SimbaPackageForm.OnHide := @DoPackageMenuTimer;
  SimbaPackageForm.Show();
end;

procedure TSimbaForm.ToolbarButtonSaveAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := SimbaScriptTabsForm.TabCount - 1 downto 0 do
    if SimbaScriptTabsForm.Tabs[I].ScriptChanged then
    begin
      if (SimbaScriptTabsForm.Tabs[I].ScriptFileName = '') then
        SimbaScriptTabsForm.Tabs[I].Show();

      SimbaScriptTabsForm.Tabs[I].Save(SimbaScriptTabsForm.Tabs[I].ScriptFileName);
    end;
end;

procedure TSimbaForm.MenuSaveAsClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentTab <> nil) then
    SimbaScriptTabsForm.CurrentTab.Save('');
end;

procedure TSimbaForm.MenuItemImageToStringClick(Sender: TObject);
begin
  SimbaImageToStringForm.Show();
end;

procedure TSimbaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not SimbaScriptTabsForm.CloseAllTabs()) then
    CloseAction := caNone
  else
  begin
    CloseAction := caFree;
    if (WindowState <> wsMinimized) then
      SimbaSettings.General.Layout.Value := DockMaster.SaveLayout();

    Visible := False;
  end;
end;

procedure TSimbaForm.DoTabLoaded(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    AddRecentFile(TSimbaScriptTab(Sender).ScriptFileName);
end;

procedure TSimbaForm.DoTabModified(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    with TSimbaScriptTab(Sender) do
    begin
      if ScriptChanged then
      begin
        SimbaForm.ToolbarButtonSave.Enabled := True;
        SimbaForm.MenuItemSave.Enabled := True;

        Caption := '*' + ScriptTitle;
      end else
      begin
        SimbaForm.ToolbarButtonSave.Enabled := False;
        SimbaForm.MenuItemSave.Enabled := False;

        Caption := ScriptTitle;
      end;
    end;
end;

procedure TSimbaForm.DoScriptTabChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
    with TSimbaScriptTab(Sender) do
    begin
      SetButtonStates(ScriptInstance);

      MenuItemSaveAll.Enabled      := TabControl.TabCount > 1;
      ToolbarButtonSaveAll.Enabled := TabControl.TabCount > 1;
    end;
end;

procedure TSimbaForm.DoScriptStateChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptInstance) and TSimbaScriptInstance(Sender).IsActiveTab() then
    SetButtonStates(TSimbaScriptInstance(Sender));
end;

procedure TSimbaForm.MenuEditClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    with SimbaScriptTabsForm.CurrentEditor do
    begin
      MenuItemCut.Enabled := SelText <> '';
      MenuItemCopy.Enabled := SelText <> '';
      MenuItemPaste.Enabled := SelText <> '';
    end;
end;

procedure TSimbaForm.MenuItemReportBugClick(Sender: TObject);
begin
  OpenURL(SIMBA_BUGS_URL);
end;

procedure TSimbaForm.MenuItemSettingsClick(Sender: TObject);
begin
  SimbaSettingsForm.ShowModal();
end;

procedure TSimbaForm.TrayIconClick(Sender: TObject);
begin
  Self.ShowOnTop();
  if Self.CanSetFocus() then
    Self.SetFocus();
end;

procedure TSimbaForm.ToolbarButtonColorPickerClick(Sender: TObject);
begin
  try
    with TSimbaColorPicker.Create(FWindowSelection) do
    try
      if not Picked then
        Exit;

      SimbaColorPickerHistoryForm.Add(Point, Color);
      SimbaDebugLn([EDebugLn.FOCUS], 'Color picked: %d at (%d, %d)'.Format([Color, Point.X, Point.Y]));

      Application.QueueAsyncCall(@DoColorPicked, 0);
    finally
      Free();
    end;
  except
    on E: Exception do
      ShowMessage('Exception while picking color: ' + E.ToString());
  end;
end;

procedure TSimbaForm.ToolbarButtonSelectTargetClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    try
      FWindowSelection := ShowWindowSelector();
      FProcessSelection := FWindowSelection.GetPID();
      FMouseLogger.WindowHandle := FWindowSelection;

      SimbaDebugLn([EDebugLn.FOCUS], [
         'Window Selected: %d'.Format([FWindowSelection]),
         ' - Dimensions: %dx%d'.Format([FWindowSelection.GetBounds().Width - 1, FWindowSelection.GetBounds().Height - 1]),
         ' - Title: "%s"'.Format([FWindowSelection.GetTitle()]),
         ' - Class: "%s"'.Format([FWindowSelection.GetClassName()]),
         ' - PID: %d (%s bit)'.Format([FWindowSelection.GetPID(), BoolToStr(SimbaProcess.IsProcess64Bit(FWindowSelection.GetPID()), '64', '32')]),
         ' - Executable: "%s"'.Format([SimbaProcess.GetProcessPath(FWindowSelection.GetPID())])
      ]);
    except
      on E: Exception do
        ShowMessage('Exception while selecting window: ' + E.ToString());
    end;
  end;
end;

procedure TSimbaForm.ToolBarPaintButton(Sender: TToolButton; State: integer);
var
  IconSize: TSize;
  IconPos: TPoint;
  MainBtnRect: TRect;
  DropDownRect: TRect;
  R: TRect;
begin
  with Sender do
  begin
    if (Style = tbsDivider) then
    begin
      Canvas.Pen.Color := ColorBlendHalf(SimbaTheme.ColorFrame, SimbaTheme.ColorLine);
      with ClientRect.CenterPoint do
        Canvas.Line(X,3,X,Height-3);

      Exit;
    end;

    IconSize := ToolBar.Images.SizeForPPI[ToolBar.ImagesWidth, ToolBar.Font.PixelsPerInch];
    if IconSize.cy <= 0 then
      IconSize.cx := 0;

    MainBtnRect := ClientRect;
    if (Style = tbsButtonDrop) then
    begin
      DropDownRect := MainBtnRect;
      DropDownRect.Left := MainBtnRect.CenterPoint.X + 8;
      DropDownRect.Right := MainBtnRect.Right - 3;

      MainBtnRect.Right := MainBtnRect.Right - 10;
    end;

    IconPos.X := (MainBtnRect.Left + MainBtnRect.Right - IconSize.cx) div 2;
    IconPos.Y := (MainBtnRect.Top + MainBtnRect.Bottom - IconSize.cy) div 2;

    Canvas.Brush.Color := SimbaTheme.ColorFrame;
    Canvas.FillRect(Rect(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom - 1));

    if State in [2, 3] then // down/hover
    begin
      R := ClientRect;
      R.Inflate(-1,-2);

      Canvas.Brush.Color := SimbaTheme.ColorActive;
      Canvas.FillRect(R);
      CanvasPaintRoundedCorners(Canvas, R, [acckLeftTop, acckRightTop, acckLeftBottom, acckRightBottom], Color, Canvas.Brush.Color, Canvas.Brush.Color);
    end;

    if Enabled then
      ToolBar.Images.ResolutionForPPI[ToolBar.ImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(Canvas, IconPos.X, IconPos.Y, ImageIndex)
    else
      ToolBar.Images.ResolutionForPPI[ToolBar.ImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor].Draw(Canvas, IconPos.X, IconPos.Y, ImageIndex, gdeDisabled);

    if (Style = tbsButtonDrop) then
      if (IconSize.cx <= 20) then
        CanvasPaintTriangleDown(Canvas, SimbaTheme.ColorFont, DropDownRect.CenterPoint, 1)
      else
        CanvasPaintTriangleDown(Canvas, SimbaTheme.ColorFont, DropDownRect.CenterPoint, 2);
  end;
end;

procedure TSimbaForm.ToolButtonAreaSelectorClick(Sender: TObject);
begin
  try
    if (FAreaSelector = nil) then
      FAreaSelector := TSimbaAreaSelector.Create();

    FAreaSelection := FAreaSelector.Pick(FWindowSelection);
    with FAreaSelection do
      SimbaDebugLn([EDebugLn.FOCUS], 'Area picked: [%d, %d, %d, %d]'.Format([X1, Y1, X2, Y2]));
  except
    on E: Exception do
      ShowMessage('Exception while selecting area: ' + E.ToString());
  end;
end;

procedure TSimbaForm.SetupDocking;
begin
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

    DockMaster.MakeDockable(SimbaScriptTabsForm, MenuItemEditor);
    DockMaster.MakeDockable(SimbaOutputForm, MenuItemOutput);
    DockMaster.MakeDockable(SimbaFileBrowserForm, MenuItemFileBrowser);
    DockMaster.MakeDockable(SimbaFunctionListForm, MenuItemFunctionList);
    DockMaster.MakeDockable(SimbaNotesForm, MenuItemNotes);
    DockMaster.MakeDockable(SimbaDebugImageForm, MenuItemDebugImage);
    DockMaster.MakeDockable(SimbaColorPickerHistoryForm, MenuItemColourHistory);

    if (SimbaSettings.General.Layout.Value <> '') then
    begin
      DockMaster.LoadLayout(SimbaSettings.General.Layout.Value);

      if (DockMaster.GetAnchorSite(SimbaScriptTabsForm) <> nil) then
        DockMaster.GetAnchorSite(SimbaScriptTabsForm).Header.Visible := False;
      if (DockMaster.GetAnchorSite(SimbaOutputForm) <> nil) then
        DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

      EnsureVisible();
    end else
      QueueOnMainThread(@DoDefaultDocking);
  finally
    DockMaster.EndUpdate();

    EndFormUpdate();
  end;
end;

procedure TSimbaForm.SetupCompleted;
begin
  PackageUpdateTimer.Enabled := True;

  if SimbaSettings.FirstLaunch then
    MenuItemAssociateScripts.Click();

  if (Application.ParamCount > 0) then
  begin
    if (Application.ParamCount = 1) and FileExists(Application.Params[1]) then
      SimbaScriptTabsForm.Open(Application.Params[1])
    else
    if Application.HasOption('open') and FileExists(Application.Params[Application.ParamCount]) then
    begin
      SimbaScriptTabsForm.Open(Application.Params[Application.ParamCount]);

      if Application.HasOption('compile') then
        ToolbarButtonCompile.Click();
      if Application.HasOption('run') then
        ToolbarButtonRun.Click();
    end;
  end;
end;

procedure SetupSimbaForm;
begin
  if (SimbaForm = nil) then
    SimbaException('SimbaForm is nil');

  SimbaForm.Setup();
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@SetupSimbaForm, 'Setup SimbaForm');

end.
