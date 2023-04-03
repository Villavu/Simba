{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.main;

{$i simba.inc}

interface

uses
  classes, sysutils, fileutil, anchordockpanel, forms, controls, graphics, dialogs,
  stdctrls, menus, comctrls, extctrls, buttons, imglist,
  simba.settings, simba.mufasatypes, simba.mouselogger, simba.areaselector, simba.scriptbackup;

const
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
  IMAGE_PACKAGE_UPDATE      = 45;

type
  TSimbaForm = class(TForm)
    DockPanel: TAnchorDockPanel;
    MenuItemShapeBox: TMenuItem;
    MenuItemDocumentation: TMenuItem;
    MenuItemGithub: TMenuItem;
    Images: TImageList;
    MainMenu: TMainMenu;
    MenuEdit: TMenuItem;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
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
    MenuItemDebugger: TMenuItem;
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
    MenuItemRunWithDebugging: TMenuItem;
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
    StatusBar: TPanel;
    StatusPanelCaret: TPanel;
    StatusPanelCursor: TPanel;
    StatusPanelFileName: TPanel;
    StatusPanelState: TPanel;
    StopButtonStop: TToolButton;
    ScriptStateTimer: TTimer;
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
    procedure MenuItemCloseTabsClick(Sender: TObject);
    procedure MenuItemConsoleClick(Sender: TObject);
    procedure MenuItemDebuggerClick(Sender: TObject);
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
    procedure MenuViewClick(Sender: TObject);
    procedure DoPackageMenuTimer(Sender: TObject);
    procedure DoScriptStateTimer(Sender: TObject);
    procedure StatusPanelPaint(Sender: TObject);
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

    FMouseLogger: TSimbaMouseLogger;

    FAreaSelector: TSimbaAreaSelector;
    FAreaSelection: TBox;

    procedure SetDefaultDocking(IsResetting: Boolean = False);
    procedure SetupDocking;
    procedure SetupCompleted;

    procedure DoColorPicked(Data: PtrInt);
    procedure DoMouseLoggerChange(Sender: TObject; X, Y: Integer; HotkeyPressed: Boolean);

    procedure FontChanged(Sender: TObject); override;

    procedure SizeComponents;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    procedure HandleRecentFileClick(Sender: TObject);
    procedure HandlePrintDTM(DTM: String);
    procedure HandleException(Sender: TObject; E: Exception);
    procedure HandleFormCreated(Sender: TObject; Form: TCustomForm);
    procedure HandleEditorChanged(Sender: TObject);
    procedure HandleEditorLoaded(Sender: TObject);
    procedure HandleEditorCaretChange(Sender: TObject);
    procedure HandleEditorSearch(Count: Integer);

    procedure SetToolbarSize(Value: Integer);
    procedure SetToolbarPosition(Value: String);
    procedure SetCustomFontSize(Value: Integer);
    procedure SetConsoleVisible(Value: Boolean);
    procedure SetLayoutLocked(Value: Boolean);
    procedure SetTrayIconVisible(Value: Boolean);
    procedure SetMacOSKeystokes(Value: Boolean);
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
  LCLType, LCLIntf, AnchorDocking, Toolwin, LazFileUtils,

  simba.shapeboxform, simba.openexampleform, simba.colorpickerhistoryform,
  simba.debugimageform, simba.bitmaptostringform, simba.aboutform,
  simba.outputform, simba.filebrowserform, simba.notesform, simba.settingsform,
  simba.functionlistform, simba.scripttabsform,

  simba.package_form, simba.package_autoupdater,

  simba.associate, simba.ide_initialization,
  simba.functionlist_simbasection, simba.functionlist_updater,
  simba.scripttab, simba.editor,
  simba.aca, simba.dtmeditor,

  simba.windowselector, simba.colorpicker,

  simba.openssl, simba.files, simba.process,
  simba.dockinghelpers, simba.datetime, simba.nativeinterface,
  simba.scriptformatter, simba.windowhandle;

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
  ToolBar.ButtonHeight := Value + Scale96ToScreen(8);
end;

procedure TSimbaForm.SetToolbarPosition(Value: String);
var
  I: Integer;
begin
  case Value of
    'Top':   ToolBar.Align := alTop;
    'Left':  ToolBar.Align := alLeft;
    'Right': ToolBar.Align := alRight;
  end;

  if (Value = 'Top') then
    ToolBar.EdgeBorders := [ebTop, ebBottom]
  else
    ToolBar.EdgeBorders := [];

  for I := 0 to Toolbar.ButtonCount - 1 do
    if (ToolBar.Buttons[I].Style = tbsDivider) then
      ToolBar.Buttons[I].Visible := Value = 'Top';
end;

procedure TSimbaForm.SetCustomFontSize(Value: Integer);
var
  I: Integer;
begin
  CreateHandle;
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

procedure TSimbaForm.SetMacOSKeystokes(Value: Boolean);
var
  Find: TShiftStateEnum;
  Replace: TShiftStateEnum;

  procedure SetMacOSKeystroke(const MenuItem: TMenuItem);
  var
    I: Integer;
    Key: Word;
    Shift: TShiftState;
  begin
    if (MenuItem.ShortCut > 0) then
    begin
      ShortCutToKey(MenuItem.ShortCut, Key, Shift);
      if (Find in Shift) then
        MenuItem.ShortCut := ShortCut(Key, Shift - [Find] + [Replace]);
    end;

    for I := 0 to MenuItem.Count - 1 do
      SetMacOSKeystroke(MenuItem.Items[I]);
  end;

begin
  if Value then Find := ssCtrl else Find := ssMeta;
  if Value then Replace := ssMeta else Replace := ssCtrl;

  SetMacOSKeystroke(MainMenu.Items);
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

procedure TSimbaForm.MenuViewClick(Sender: TObject);
begin
  MenuItemDebugger.Enabled := SimbaScriptTabsForm.CurrentTab.DebuggingForm <> nil;
end;

procedure TSimbaForm.DoPackageMenuTimer(Sender: TObject);
begin
  UpdatePackages();
end;

procedure TSimbaForm.StatusPanelPaint(Sender: TObject);
var
  Style: TTextStyle;
begin
  Style := Default(TTextStyle);
  Style.Layout := tlCenter;

  with Sender as TPanel do
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);

    Canvas.Pen.Color := clActiveBorder;
    Canvas.Line(0, 0, ClientWidth, 0);
    Canvas.Line(0, 0, 0, ClientHeight);

    Canvas.TextRect(ClientRect, 5, 0, Caption, Style);
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
  SimbaScriptTabsForm.OnEditorLoaded := @HandleEditorLoaded;
  SimbaScriptTabsForm.OnEditorChanged := @HandleEditorChanged;
  SimbaScriptTabsForm.OnEditorCaretChanged := @HandleEditorCaretChange;
  SimbaScriptTabsForm.OnSearch := @HandleEditorSearch;
  SimbaScriptTabsForm.AddTab();

  SimbaFunctionListUpdater := TSimbaFunctionListUpdater.Create();

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);

  SimbaSettingChanged(SimbaSettings.General.ToolbarSize);
  SimbaSettingChanged(SimbaSettings.General.ToolbarPosition);
  SimbaSettingChanged(SimbaSettings.General.CustomFontSize);
  SimbaSettingChanged(SimbaSettings.General.LockLayout);
  SimbaSettingChanged(SimbaSettings.General.TrayIconVisible);
  SimbaSettingChanged(SimbaSettings.General.ConsoleVisible);
  SimbaSettingChanged(SimbaSettings.General.MacOSKeystrokes);

  SizeComponents();
  SetupDocking();
  SetupCompleted();

  FMouseLogger := TSimbaMouseLogger.Create();
  FMouseLogger.OnChange := @DoMouseLoggerChange;
  FMouseLogger.Hotkey := VK_F1;

  SimbaIDEInitialization.CallOnAfterCreateMethods();
end;

procedure TSimbaForm.FormCreate(Sender: TObject);
begin
  SimbaIDEInitialization.CallOnCreateMethods();

  Application.CaptureExceptions := True;
  Application.OnException := @SimbaForm.HandleException;

  Screen.AddHandlerFormAdded(@SimbaForm.HandleFormCreated, True);

  FRecentFiles := TStringList.Create();
  FRecentFiles.Text := SimbaSettings.General.RecentFiles.Value;
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

procedure TSimbaForm.MenuItemDebuggerClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentTab.DebuggingForm <> nil) then
    SimbaScriptTabsForm.CurrentTab.DebuggingForm.ShowOnTop();
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
  EAction = (Unknown, Compile, Run, Debug, Pause, Stop);

  function GetAction: EAction;
  begin
    if (Sender = MenuItemCompile) or (Sender = ToolbarButtonCompile) then Exit(Compile);
    if (Sender = MenuItemRun)     or (Sender = ToolbarButtonRun)     then Exit(Run);
    if (Sender = MenuItemPause)   or (Sender = ToolbarButtonPause)   then Exit(Pause);
    if (Sender = MenuItemStop)    or (Sender = StopButtonStop)       then Exit(Stop);
    if (Sender = MenuItemRunWithDebugging)                           then Exit(Debug);

    DebugLn('[TSimbaForm.MenuItemScriptStateClick]: Unknown component "' + Sender.ClassName + '"');
  end;

var
  CurrentTab: TSimbaScriptTab;
begin
  CurrentTab := SimbaScriptTabsForm.CurrentTab;

  if (CurrentTab <> nil) then
  try
    if (GetAction() in [Compile, Run, Debug]) then
    begin
      CurrentTab.OutputBox.Tab.Show();
      if SimbaSettings.General.OutputClearOnCompile.Value then
        CurrentTab.OutputBox.Empty();
    end;

    case GetAction() of
      Compile: CurrentTab.Compile();
      Run:     CurrentTab.Run(FWindowSelection);
      Debug:   CurrentTab.RunWithDebugging(FWindowSelection);
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

procedure TSimbaForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  SizeComponents();
end;

procedure TSimbaForm.SizeComponents;
begin
  if (ControlCount = 0) then
    Exit;

  with TBitmap.Create() do
  try
    // Measure on larger font size
    // Font size can be 0 so use GetFontData
    Canvas.Font := Self.Font;
    Canvas.Font.Size := Round(-GetFontData(Canvas.Font.Reference.Handle).Height * 72 / Canvas.Font.PixelsPerInch) + 6;

    StatusBar.Height := Round(Canvas.TextHeight('Taylor Swift') * 0.8);

    StatusPanelCursor.Width := Canvas.TextWidth('(1000,1000)');
    StatusPanelState.Width  := Canvas.TextWidth('[00:00:00]');
    StatusPanelCaret.Width  := Canvas.TextWidth('Line 100, Col 100');
  finally
    Free();
  end;
end;

procedure TSimbaForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.General.ToolbarPosition) then
    SetToolbarPosition(Setting.Value);
  if (Setting = SimbaSettings.General.ToolbarSize) then
    SetToolbarSize(Setting.Value);
  if (Setting = SimbaSettings.General.CustomFontSize) then
    SetCustomFontSize(Setting.Value);
  if (Setting = SimbaSettings.General.ConsoleVisible) then
    SetConsoleVisible(Setting.Value);
  if (Setting = SimbaSettings.General.LockLayout) then
    SetLayoutLocked(Setting.Value);
  if (Setting = SimbaSettings.General.TrayIconVisible) then
    SetTrayIconVisible(Setting.Value);
  if (Setting = SimbaSettings.General.MacOSKeystrokes) then
    SetMacOSKeystokes(Setting.Value);
end;

procedure TSimbaForm.MenuCloseTabClick(Sender: TObject);
begin
  SimbaScriptTabsForm.CloseTab(SimbaScriptTabsForm.CurrentTab);
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
  SimbaScriptTabsForm.CurrentTab.OutputBox.Empty();
end;

procedure TSimbaForm.MenuFileClick(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  MenuItemSaveAll.Enabled := SimbaScriptTabsForm.TabCount > 1;
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
    Item.Caption := ShortDisplayFilename(FRecentFiles[I], 100);
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
  SimbaScriptTabsForm.CloseAllTabs();
end;

procedure TSimbaForm.MenuNewClick(Sender: TObject);
begin
  SimbaScriptTabsForm.AddTab();
end;

procedure TSimbaForm.HandlePrintDTM(DTM: String);
begin
  SimbaDebugLn([EDebugLn.FOCUS], 'DTM := TDTM.CreateFromString(' + #39 + DTM + #39 + ');');
end;

procedure TSimbaForm.MenuItemDTMEditorClick(Sender: TObject);
begin
  with TSimbaDTMEditorForm.Create(WindowSelection) do
  begin
    OnPrintDTM := @HandlePrintDTM;
    ShowOnTop();
  end;
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

procedure TSimbaForm.DoScriptStateTimer(Sender: TObject);
var
  Tab: TSimbaScriptTab;
begin
  Tab := SimbaScriptTabsForm.CurrentTab;
  if (Tab = nil) then
    Exit;

  case Tab.ScriptState of
    ESimbaScriptState.STATE_PAUSED:
      begin
        ToolbarButtonRun.Enabled := True;
        ToolbarButtonPause.Enabled := False;
        ToolbarButtonCompile.Enabled := True;

        StopButtonStop.Enabled := True;
        StopButtonStop.ImageIndex := IMAGE_STOP;

        StatusPanelState.Caption := 'Paused';
      end;

    ESimbaScriptState.STATE_STOP:
      begin
        ToolbarButtonRun.Enabled := False;
        ToolbarButtonPause.Enabled := False;
        ToolbarButtonCompile.Enabled := False;

        StopButtonStop.Enabled := True;
        StopButtonStop.ImageIndex := IMAGE_POWER;

        StatusPanelState.Caption := 'Stopping';
      end;

    ESimbaScriptState.STATE_RUNNING:
      begin
        ToolbarButtonRun.Enabled := False;
        ToolbarButtonPause.Enabled := True;
        ToolbarButtonCompile.Enabled := False;

        StopButtonStop.Enabled := True;
        StopButtonStop.ImageIndex := IMAGE_STOP;

        StatusPanelState.Caption := FormatMilliseconds(Tab.ScriptTimeRunning(), 'hh:mm:ss');
      end;

    ESimbaScriptState.STATE_NONE:
      begin
        ToolbarButtonRun.Enabled := True;
        ToolbarButtonPause.Enabled := False;
        ToolbarButtonCompile.Enabled := True;

        StopButtonStop.Enabled := False;
        StopButtonStop.ImageIndex := IMAGE_STOP;

        StatusPanelState.Caption := 'Stopped';
      end;
  end;
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

procedure TSimbaForm.HandleEditorChanged(Sender: TObject);
begin
  with Sender as TSimbaScriptTab do
  begin
    if (ScriptFileName <> '') then
      StatusPanelFileName.Caption := ScriptFileName
    else
      StatusPanelFileName.Caption := ScriptTitle;

    StatusPanelCaret.Caption := 'Line ' + IntToStr(Editor.CaretY) + ', Col ' + IntToStr(Editor.CaretX);

    MenuItemSaveAll.Enabled := PageControl.PageCount > 1;
    ToolbarButtonSaveAll.Enabled := PageControl.PageCount > 1;
  end;
end;

procedure TSimbaForm.HandleEditorLoaded(Sender: TObject);
begin
  with Sender as TSimbaScriptTab do
  begin
    StatusPanelFileName.Caption := ScriptFileName;

    if FRecentFiles.IndexOf(ScriptFileName) >= 0 then
      FRecentFiles.Delete(FRecentFiles.IndexOf(ScriptFileName));
    FRecentFiles.Insert(0, ScriptFileName);
    while (FRecentFiles.Count > 10) do
      FRecentFiles.Pop();
  end;
end;

procedure TSimbaForm.HandleEditorCaretChange(Sender: TObject);
begin
  with Sender as TSimbaEditor do
    StatusPanelCaret.Caption := 'Line ' + IntToStr(CaretY) + ', Col ' + IntToStr(CaretX);
end;

procedure TSimbaForm.HandleEditorSearch(Count: Integer);
begin
  StatusPanelFileName.Caption := 'Find matches: ' + IntToStr(Count);
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

procedure TSimbaForm.MenuItemCloseTabsClick(Sender: TObject);
begin
  SimbaScriptTabsForm.CloseAllTabs();
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
    DockMaster.SplitterWidth := 5;
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
  ScriptStateTimer.Enabled := True;
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

procedure TSimbaForm.DoMouseLoggerChange(Sender: TObject; X, Y: Integer; HotkeyPressed: Boolean);
begin
  StatusPanelCursor.Caption := '(' + IntToStr(X) + ', ' + IntToStr(Y) + ')';
  if HotkeyPressed then
    SimbaDebugLn([EDebugLn.FOCUS], StatusPanelCursor.Caption);
end;

end.
