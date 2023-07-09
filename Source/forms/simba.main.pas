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
  simba.scriptbackup, simba.scriptinstance, simba.component_menubar;

const
  IMAGE_NONE                = -1;
  IMAGE_COMPILE             = 0;
  IMAGE_PACKAGE             = 1;
  IMAGE_COPY                = 2;
  IMAGE_CUT                 = 3;
  IMAGE_OPEN                = 4;
  IMAGE_PASTE               = 5;
  IMAGE_SAVE                = 6;
  IMAGE_TRASH               = 7;
  IMAGE_CLOSE               = 8;
  IMAGE_CLOSE_ALL           = 9;
  IMAGE_POWER               = 10;
  IMAGE_NEW_FILE            = 11;
  IMAGE_PAUSE               = 12;
  IMAGE_REDO                = 13;
  IMAGE_PLAY                = 14;
  IMAGE_SAVE_ALL            = 15;
  IMAGE_STOP                = 16;
  IMAGE_UNDO                = 17;
  IMAGE_SEARCH              = 18;
  IMAGE_COLOR_PICKER        = 19;
  IMAGE_SELECT              = 20;
  IMAGE_GEAR                = 21;
  IMAGE_OPEN_RECENT         = 22;
  IMAGE_PACKAGE_NOTIFCATION = 23;
  IMAGE_WRITE_BUG           = 24;
  IMAGE_MINUS               = 25;
  IMAGE_PLUS                = 26;
  IMAGE_DIRECTORY           = 27;
  IMAGE_SIMBA               = 28;
  IMAGE_BOOK                = 29;
  IMAGE_FILE                = 30;
  IMAGE_TYPE                = 31;
  IMAGE_FUNCTION            = 32;
  IMAGE_PROCEDURE           = 33;
  IMAGE_GITHUB              = 34;
  IMAGE_CONSTANT            = 35;
  IMAGE_VARIABLE            = 36;
  IMAGE_OPERATOR            = 39;
  IMAGE_FONT                = 42;
  IMAGE_PACKAGE_UPDATE      = 45;

type
  TSimbaForm = class(TForm)
    DockPanel: TAnchorDockPanel;
    MainMenuTools: TPopupMenu;
    MainMenuView: TPopupMenu;
    MainMenuHelp: TPopupMenu;
    MenuItemShapeBox: TMenuItem;
    MenuItemDocumentation: TMenuItem;
    MenuItemGithub: TMenuItem;
    Images: TImageList;
    MenuEdit: TMenuItem;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemACA: TMenuItem;
    MenuItemAssociateScripts: TMenuItem;
    MenuItemBitmapConv: TMenuItem;
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
    MenuItemDivider14: TMenuItem;
    MenuItemDivider2: TMenuItem;
    MenuItemDivider3: TMenuItem;
    MenuItemDivider4: TMenuItem;
    MenuItemDivider5: TMenuItem;
    MenuItemDivider6: TMenuItem;
    MenuItemDTMEditor: TMenuItem;
    MenuItemEditor: TMenuItem;
    MenuItemExample: TMenuItem;
    MenuItemFileBrowser: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemFindNext: TMenuItem;
    MenuItemFindPrev: TMenuItem;
    MenuItemFormatScript: TMenuItem;
    MenuItemFunctionList: TMenuItem;
    MenuItemGoto: TMenuItem;
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
    MenuItemReplace: TMenuItem;
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
    StopButtonStop: TToolButton;
    PackageMenuTimer: TTimer;
    ToolBar: TToolBar;
    ToolbarButtonClearOutput: TToolButton;
    ToolbarButtonColorPicker: TToolButton;
    ToolbarButtonCompile: TToolButton;
    ToolbarButtonNew: TToolButton;
    ToolbarButtonOpen: TToolButton;
    ToolbarButtonPackages: TToolButton;
    ToolbarButtonPause: TToolButton;
    ToolbarButtonRun: TToolButton;
    ToolbarButtonSave: TToolButton;
    ToolbarButtonSaveAll: TToolButton;
    ToolbarButtonTargetSelector: TToolButton;
    ToolbarDivider1: TToolButton;
    ToolbarDivider2: TToolButton;
    ToolbarDivider3: TToolButton;
    ToolbarDivider4: TToolButton;
    ToolbarDivider5: TToolButton;
    ToolButtonAreaSelector: TToolButton;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    TrayPopupExit: TMenuItem;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
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
    procedure MenuItemShapeBoxClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemACAClick(Sender: TObject);
    procedure MenuItemAssociateScriptsClick(Sender: TObject);
    procedure MenuItemBitmapConvClick(Sender: TObject);
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
    procedure ToolbarButtonColorPickerClick(Sender: TObject);
    procedure ToolbarButtonPackagesClick(Sender: TObject);
    procedure ToolbarButtonSaveAllClick(Sender: TObject);
    procedure ToolbarButtonSelectTargetClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ToolButtonAreaSelectorClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayPopupExitClick(Sender: TObject);
  protected
    FWindowSelection: TWindowHandle;
    FProcessSelection: Integer;
    FRecentFiles: TStringList;

    FMenuBar: TSimbaMainMenuBar;
    FMouseLogger: TSimbaMouseLogger;

    FAreaSelector: TSimbaAreaSelector;
    FAreaSelection: TBox;

    procedure AddRecentFile(FileName: String);
    procedure SetButtonStates(Instance: TSimbaScriptInstance);

    procedure SetDefaultDocking(IsResetting: Boolean = False);
    procedure SetupDocking;
    procedure SetupCompleted;

    procedure DoColorPicked(Data: PtrInt);
    procedure DoSettingChanged_Toolbar(Setting: TSimbaSetting);

    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    procedure HandleRecentFileClick(Sender: TObject);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure HandleFormCreated(Sender: TObject; Form: TCustomForm);

    procedure DoTabLoaded(Sender: TObject);
    procedure DoTabModified(Sender: TObject);
    procedure DoScriptTabChange(Sender: TObject);
    procedure DoScriptStateChange(Sender: TObject);

    procedure SetToolbarSize(Value: Integer);
    procedure SetToolbarPosition(Value: String);
    procedure SetCustomFontSize(Value: Integer);
    procedure SetConsoleVisible(Value: Boolean);
    procedure SetLayoutLocked(Value: Boolean);
    procedure SetTrayIconVisible(Value: Boolean);
  public
    property WindowSelection: TWindowHandle read FWindowSelection;
    property ProcessSelection: Integer read FProcessSelection;

    procedure Setup(Data: PtrInt);
  end;

var
  SimbaForm: TSimbaForm;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, LazFileUtils, AnchorDocking, ToolWin,

  simba.shapeboxform, simba.openexampleform, simba.colorpickerhistoryform,
  simba.debugimageform, simba.bitmaptostringform, simba.aboutform,
  simba.outputform, simba.filebrowserform, simba.notesform, simba.settingsform,
  simba.functionlistform, simba.scripttabsform, simba.ide_mainstatusbar,

  simba.package_form, simba.package_autoupdater,

  simba.associate, simba.ide_initialization, simba.ide_events,
  simba.aca, simba.dtmeditor,

  simba.windowselector, simba.colorpicker,

  simba.openssl, simba.env, simba.process,
  simba.dockinghelpers, simba.nativeinterface,
  simba.scriptformatter, simba.windowhandle, simba.scripttab, simba.theme;

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

    FileName := GetDataPath() + FormatDateTime('dd-mm_hh-mm-ss', Now()) + '.crash';

    Log.SaveToFile(FileName);
    Log.Free();

    Message := '%s'                                                     + LineEnding +
               ''                                                       + LineEnding +
               'Press OK to save your scripts and close. (Recommended)' + LineEnding +
               'Press Cancel to ignore and risk data corruption.'       + LineEnding +
               ''                                                       + LineEnding +
               'A crash log has been saved in the data directory.';

    if MessageDlg(Format(Message, [E.Message, ExtractRelativePath(GetSimbaPath(), FileName)]), mtError, mbOKCancel, 0) = mrOk then
    begin
      SimbaScriptTabsForm.CloseAllTabs();

      Halt(1);
    end;
  except
    // circular exception ...
  end;
end;

procedure TSimbaForm.SetToolbarSize(Value: Integer);
begin
  ToolBar.ImagesWidth := Value;

  ToolBar.ButtonWidth  := Value + Scale96ToScreen(8);
  ToolBar.ButtonHeight := Value + Scale96ToScreen(16);
end;

procedure TSimbaForm.SetToolbarPosition(Value: String);
begin
  MainMenuPanel.Align := alNone;

  case Value of
    'Top':   ToolBar.Align := alTop;
    'Left':  ToolBar.Align := alLeft;
    'Right': ToolBar.Align := alRight;
  end;

  if (Value = 'Top') then
    ToolBar.EdgeBorders := [ebTop, ebBottom]
  else
    ToolBar.EdgeBorders := [];

  MainMenuPanel.Align := alTop;
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
  if (Instance = nil) or (Instance.State in [ESimbaScriptState.STATE_NONE, ESimbaScriptState.STATE_PAUSED]) then
  begin
    ToolbarButtonRun.Enabled := True;
    ToolbarButtonPause.Enabled := False;
    ToolbarButtonCompile.Enabled := True;

    StopButtonStop.Enabled := False;
    StopButtonStop.ImageIndex := IMAGE_STOP;

    Exit;
  end;

  case Instance.State of
    ESimbaScriptState.STATE_PAUSED:
      begin
        ToolbarButtonRun.Enabled := True;
        ToolbarButtonPause.Enabled := False;
        ToolbarButtonCompile.Enabled := True;

        StopButtonStop.Enabled := True;
        StopButtonStop.ImageIndex := IMAGE_STOP;
      end;

    ESimbaScriptState.STATE_STOP:
      begin
        ToolbarButtonRun.Enabled := False;
        ToolbarButtonPause.Enabled := False;
        ToolbarButtonCompile.Enabled := False;

        StopButtonStop.Enabled := True;
        StopButtonStop.ImageIndex := IMAGE_POWER;
      end;

    ESimbaScriptState.STATE_RUNNING:
      begin
        ToolbarButtonRun.Enabled := False;
        ToolbarButtonPause.Enabled := True;
        ToolbarButtonCompile.Enabled := False;

        StopButtonStop.Enabled := True;
        StopButtonStop.ImageIndex := IMAGE_STOP;
      end;
   end;
end;

procedure TSimbaForm.SetDefaultDocking(IsResetting: Boolean);
var
  I: Integer;
  Splitter: TAnchorDockSplitter;
begin
  if IsResetting then
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
  end;

  DockMaster.ManualDockPanel(DockMaster.GetAnchorSite(SimbaScriptTabsForm), DockPanel, alClient);
  DockMaster.ManualDockPanel(DockMaster.GetAnchorSite(SimbaOutputForm), DockPanel, alBottom);
  DockMaster.ManualDockPanel(DockMaster.GetAnchorSite(SimbaFunctionListForm), DockPanel, alLeft);
  DockMaster.ManualDockPanel(DockMaster.GetAnchorSite(SimbaFileBrowserForm), DockPanel, alRight);

  DockMaster.MakeVisible(SimbaScriptTabsForm, False);
  DockMaster.MakeVisible(SimbaOutputForm, False);
  DockMaster.MakeVisible(SimbaFunctionListForm, False);
  DockMaster.MakeVisible(SimbaFileBrowserForm, False);

  // Default size
  Width := 1250;
  Height := 850;

  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akLeft, Splitter) then
    Splitter.SetSplitterPosition(180);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akRight, Splitter) then
    Splitter.SetSplitterPosition(720);
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akBottom, Splitter) then
    Splitter.SetSplitterPosition(350);

  DockMaster.GetAnchorSite(SimbaScriptTabsForm).Header.Visible := False;
  DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;

  MoveToDefaultPosition();
  EnsureVisible();
end;

procedure TSimbaForm.Setup(Data: PtrInt);
begin
  SimbaIDEInitialization.CallOnCreatedMethods();

  SimbaIDEEvents.RegisterMethodOnEditorLoaded(@DoTabLoaded);
  SimbaIDEEvents.RegisterMethodOnEditorModified(@DoTabModified);
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoTabModified); // Also do this
  SimbaIDEEvents.RegisterMethodOnScriptTabChange(@DoScriptTabChange);
  SimbaIDEEvents.RegisterMethodOnScriptStateChange(@DoScriptStateChange);

  SimbaScriptTabsForm.AddTab();

  SetupDocking();
  SetupCompleted();

  FMouseLogger := TSimbaMouseLogger.Create();
  FMouseLogger.Hotkey := VK_F1;

  SimbaIDEInitialization.CallOnAfterCreateMethods();

  FMenuBar := TSimbaMainMenuBar.Create(Self);
  FMenuBar.Parent := MainMenuPanel;
  FMenuBar.Align := alTop;
  FMenuBar.AddMenu('File', MainMenuFile);
  FMenuBar.AddMenu('Edit', MainMenuEdit);
  FMenuBar.AddMenu('Script', MainMenuScript);
  FMenuBar.AddMenu('Tools', MainMenuTools);
  FMenuBar.AddMenu('View', MainMenuView);
  FMenuBar.AddMenu('Help', MainMenuHelp);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);

  with SimbaSettings do
  begin
    RegisterChangeHandler(Self, General.ToolbarSize, @DoSettingChanged_Toolbar, True);
    RegisterChangeHandler(Self, General.ToolbarPosition, @DoSettingChanged_Toolbar, True);
    RegisterChangeHandler(Self, General.ToolBarSpacing, @DoSettingChanged_Toolbar, True);
  end;

  SimbaSettingChanged(SimbaSettings.General.CustomFontSize);
  SimbaSettingChanged(SimbaSettings.General.LockLayout);
  SimbaSettingChanged(SimbaSettings.General.TrayIconVisible);
  SimbaSettingChanged(SimbaSettings.General.ConsoleVisible);

  // Finally, give the editor focus as default.
  if Assigned(SimbaScriptTabsForm.CurrentEditor) then
    if SimbaScriptTabsForm.CurrentEditor.CanSetFocus() then
      SimbaScriptTabsForm.CurrentEditor.SetFocus();
end;

procedure TSimbaForm.FormCreate(Sender: TObject);
begin
  SimbaIDEInitialization.CallOnBeforeCreateMethods();

  Application.CaptureExceptions := True;
  Application.OnException := @Self.HandleException;
  Application.OnShortcut := @Self.FormShortCut;

  Screen.AddHandlerFormAdded(@Self.HandleFormCreated, True);

  FRecentFiles := TStringList.Create();
  FRecentFiles.Text := SimbaSettings.General.RecentFiles.Value;

  Self.Color := SimbaTheme.ColorFrame;
  ToolBar.Color := SimbaTheme.ColorFrame;
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

  if (SimbaSettings <> nil) then
    SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  if (FRecentFiles <> nil) then
  begin
    SimbaSettings.General.RecentFiles.Value := FRecentFiles.Text;

    FreeAndNil(FRecentFiles);
  end;

  SimbaSettings.Save();
end;

procedure TSimbaForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  Handled := MainMenuFile.IsShortcut(Msg)  or MainMenuView.IsShortcut(Msg)   or
             MainMenuEdit.IsShortcut(Msg)  or MainMenuScript.IsShortcut(Msg) or
             MainMenuTools.IsShortcut(Msg) or MainMenuHelp.IsShortcut(Msg)   or
             (KeyDataToShiftState(Msg.KeyData) = [ssAlt]); // Suppress windows freaking out
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
    if (Sender = MenuItemStop)    or (Sender = StopButtonStop)       then Exit(Stop);

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
begin
  if Setting.Equals(SimbaSettings.General.ToolbarSize) then
  begin
    ToolBar.ImagesWidth := Setting.Value;

    ToolBar.ButtonWidth  := Setting.Value + Scale96ToScreen(8);
    ToolBar.ButtonHeight := Setting.Value + Scale96ToScreen(16);
  end;

  if Setting.Equals(SimbaSettings.General.ToolbarPosition) then
  begin
    MainMenuPanel.Align := alNone;

    case String(Setting.Value) of
      'Top':
        begin
          ToolBar.Align := alTop;
          ToolBar.EdgeBorders := [ebTop, ebBottom];
        end;
      'Left':
        begin
          ToolBar.Align := alLeft;
          ToolBar.EdgeBorders := [];
        end;
      'Right':
        begin
          ToolBar.Align := alRight;
          ToolBar.EdgeBorders := [];
        end;
    end;

    MainMenuPanel.Align := alTop;
  end;

  if Setting.Equals(SimbaSettings.General.ToolBarSpacing) then
  begin
    ToolBar.BorderSpacing.Around := Setting.Value;
  end;
end;

procedure TSimbaForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.General.CustomFontSize) then
    SetCustomFontSize(Setting.Value);
  if (Setting = SimbaSettings.General.ConsoleVisible) then
    SetConsoleVisible(Setting.Value);
  if (Setting = SimbaSettings.General.LockLayout) then
    SetLayoutLocked(Setting.Value);
  if (Setting = SimbaSettings.General.TrayIconVisible) then
    SetTrayIconVisible(Setting.Value);
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
  SetDefaultDocking(True);
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

procedure TSimbaForm.MenuItemBitmapConvClick(Sender: TObject);
begin
  SimbaBitmapConversionForm.Show();
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
      SetDefaultDocking();
  finally
    DockMaster.EndUpdate();

    EndFormUpdate();
  end;
end;

procedure TSimbaForm.SetupCompleted;
begin
  PackageMenuTimer.Enabled := True;

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

end.
