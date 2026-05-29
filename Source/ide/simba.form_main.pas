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
  Menus, ImgList, AnchorDockPanel,
  simba.base,
  simba.settings,
  simba.ide_events,
  simba.image;

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
  IMG_VAR = 46;
  IMG_CONST = 47;
  IMG_ENUM = 48;
  IMG_ANCHOR = 49;
  IMG_SELECT_ALL = 50;
  IMG_CLOSE = 51;
  IMG_CLOSE_ALL = 52;
  IMG_ARROW_UP = 53;
  IMG_ARROW_DOWN = 54;
  IMG_SELECT_WORD = 55;
  IMG_SELECT_LINE = 56;
  IMG_UPPERCASE = 57;
  IMG_LOWERCASE = 58;
  IMG_FIND_FILES = 59;
  IMG_SAVE_AS = 61;
  IMG_TICK = 62;
  IMG_PROPERTY = 63;
  IMG_INFO = 64;
  IMG_EYE = 65;
  IMG_FILE_CATEGORY = 66;

type
  TSimbaMainForm = class(TForm)
    DockPanel: TAnchorDockPanel;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    TrayPopupExit: TMenuItem;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ImagesGetWidthForPPI(Sender: TCustomImageList; AImageWidth, APPI: Integer; var AResultWidth: Integer);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayPopupExitClick(Sender: TObject);
  protected
    procedure DoResetDocking;
    procedure DoDefaultDocking;
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoAssociateIfFirstLaunch;
    procedure DoApplicationParameters;
    function DoGetTargetImage: TSimbaImage;
  public
    procedure Setup;

    destructor Destroy; override;
  end;

var
  SimbaMainForm: TSimbaMainForm;

implementation

{$R *.lfm}

uses
  AnchorDocking,

  simba.initializations,

  simba.vartype_windowhandle,

  simba.ide_utils,
  simba.ide_vars,
  simba.ide_scriptbackup,
  simba.ide_associate,
  simba.ide_debugimage,
  simba.ide_docking,
  simba.ide_dtmeditor,
  simba.ide_controller,

  simba.form_colorpickhistory,
  simba.form_findinfiles,
  simba.form_filebrowser,
  simba.form_notes,
  simba.form_functionlist,
  simba.form_backups,
  simba.form_scripttabs,
  simba.form_output,

  simba.aca,
  simba.env,
  simba.nativeinterface,
  simba.threading;

procedure TSimbaMainForm.DoException(Sender: TObject; E: Exception);

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
    Log.Add('Exception class: %s', [E.ClassName]);
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
      SimbaController.CloseAllTabs();

      Halt(1);
    end;
  except
    // circular exception ...
  end;
end;

procedure TSimbaMainForm.DoAssociateIfFirstLaunch;
begin
  if SimbaSettings.FirstLaunch then
    SimbaEvents.Post(ESimbaEvent.ACTION_ASSOCIATE, nil);
end;

procedure TSimbaMainForm.DoApplicationParameters;
var
  FileName: String;
begin
  if Application.HasOption('open') then
  begin
    FileName := Application.Params[Application.ParamCount];
    if FileExists(FileName) then
    begin
      SimbaController.OpenInTab(FileName);

      if Application.HasOption('compile') then
        SimbaEvents.Post(ESimbaEvent.ACTION_COMPILE, nil)
      else if Application.HasOption('run') then
        SimbaEvents.Post(ESimbaEvent.ACTION_RUN, nil);
    end;
  end;
end;

procedure TSimbaMainForm.TrayPopupExitClick(Sender: TObject);
begin
  Close();
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
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaScriptTabsForm), DockPanel, alClient);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaOutputForm), DockPanel, alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFunctionListForm), DockPanel, alLeft);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFileBrowserForm), DockPanel, alRight);

  DockMaster.MakeVisible(SimbaScriptTabsForm, False);
  DockMaster.MakeVisible(SimbaOutputForm, False);
  DockMaster.MakeVisible(SimbaFunctionListForm, False);
  DockMaster.MakeVisible(SimbaFileBrowserForm, False);
  DockMaster.ScaleOnResize := False;

  Width := Scale96ToScreen(1200);
  Height := Scale96ToScreen(850);

  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akLeft, Splitter) then
    Splitter.SetSplitterPosition(Scale96ToScreen(250));
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akRight, Splitter) then
    Splitter.SetSplitterPosition(Scale96ToScreen(1200 - 250));
  if GetDockSplitter(DockMaster.GetAnchorSite(SimbaScriptTabsForm), akBottom, Splitter) then
    Splitter.SetSplitterPosition(Scale96ToScreen(500));

  Dockmaster.ScaleOnResize := True;

  DockMaster.GetAnchorSite(SimbaScriptTabsForm).Header.Visible := False;
  DockMaster.GetAnchorSite(SimbaOutputForm).Header.Visible := False;


  MoveToDefaultPosition();
  EnsureVisible();
end;

procedure TSimbaMainForm.Setup;
begin
  // Register events etc
  Application.CaptureExceptions := True;
  Application.OnException := @Self.DoException;

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.ACTION_RESET_LAYOUT,
    ESimbaEvent.ACTION_COMPILER_HINTS,
    ESimbaEvent.ACTION_LOCK_LAYOUT,
    ESimbaEvent.ACTION_VIEW_TRAYICON,
    ESimbaEvent.ACTION_REPORTBUG,
    ESimbaEvent.ACTION_SIMBAGITHUB,
    ESimbaEvent.ACTION_ONLINEDOCS,
    ESimbaEvent.ACTION_ASSOCIATE,
    ESimbaEvent.ACTION_ACA,
    ESimbaEvent.ACTION_DTM_EDITOR,
    ESimbaEvent.ACTION_QUIT
  ]);

  // Docking
  BeginFormUpdate();
  try
    DockMaster.BeginUpdate();
    DockMaster.SplitterWidth := Scale96ToScreen(6);
    DockMaster.HeaderClass := TSimbaAnchorDockHeader;
    DockMaster.SplitterClass := TSimbaAnchorDockSplitter;
    DockMaster.SiteClass := TSimbaAnchorDockHostSite;
    DockMaster.HideHeaderCaptionFloatingControl := False;
    DockMaster.HeaderAlignTop := $FFFFFF;
    DockMaster.PageAreaInPercent := 0;
    DockMaster.HeaderHint := 'Use the mouse to drag and dock this window';
    DockMaster.MakeDockPanel(DockPanel, admrpChild);
    DockMaster.DragTreshold := 40;

    DockMaster.MakeDockable(SimbaScriptTabsForm);
    DockMaster.MakeDockable(SimbaOutputForm);
    DockMaster.MakeDockable(SimbaFileBrowserForm);
    DockMaster.MakeDockable(SimbaFunctionListForm);
    DockMaster.MakeDockable(SimbaNotesForm);
    DockMaster.MakeDockable(SimbaDebugImageForm);
    DockMaster.MakeDockable(SimbaDebugMatrixForm);
    DockMaster.MakeDockable(SimbaColorPickHistoryForm);
    DockMaster.MakeDockable(SimbaBackupsForm);
    DockMaster.MakeDockable(SimbaFindInFilesForm);

    if (SimbaSettings.General.Layout.Value <> '') and
       (SimbaSettings.General.LayoutVersion.Value = SIMBA_DOCKING_VERSION) then
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

  Caption := Format('Simba %.1f', [SIMBA_VERSION / 1000]);

  DockMaster.ShowHeader := not SimbaSettings.General.LockLayout.Value;
  DockMaster.AllowDragging := not SimbaSettings.General.LockLayout.Value;
  TrayIcon.Visible := SimbaSettings.General.TrayIconVisible.Value;

  QueueOnMainThread(@DoAssociateIfFirstLaunch);
  QueueOnMainThread(@DoApplicationParameters); // open/compile/run parameters
end;

destructor TSimbaMainForm.Destroy;
begin
  inherited Destroy();

  SimbaInitialization_Call(ESimbaInit.IDE_DESTROY);
end;

procedure TSimbaMainForm.FormDestroy(Sender: TObject);
begin
  SimbaSettings.Save();
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

procedure TSimbaMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not SimbaScriptTabsForm.CloseAllTabs()) then
    CloseAction := caNone
  else
  begin
    CloseAction := caFree;
    if (WindowState <> wsMinimized) then
    begin
      SimbaSettings.General.Layout.Value := DockMaster.SaveLayout();
      SimbaSettings.General.LayoutVersion.Value := SIMBA_DOCKING_VERSION;
    end;

    Visible := False;
  end;
end;

procedure TSimbaMainForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoResetLayout;
  begin
    QueueOnMainThread(@DoResetDocking);
  end;

  procedure DoLockLayout(MenuItem: TMenuItem);
  begin
    SimbaSettings.General.LockLayout.Value := MenuItem.Checked;

    DockMaster.ShowHeader := not MenuItem.Checked;
    DockMaster.AllowDragging := not MenuItem.Checked;
  end;

  procedure DoViewTrayIcon(MenuItem: TMenuItem);
  begin
    SimbaSettings.General.TrayIconVisible.Value := MenuItem.Checked;

    TrayIcon.Visible := MenuItem.Checked;
  end;

  procedure DoReportBug;
  begin
    SimbaNativeInterface.OpenURL(SIMBA_BUGS_URL);
  end;

  procedure DoSimbaGithub;
  begin
    SimbaNativeInterface.OpenURL(SIMBA_GITHUB_URL);
  end;

  procedure DoOnlineDocs;
  begin
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL);
  end;

  procedure DoDTMEditor;
  begin
    with TSimbaDTMEditor.Create(@DoGetTargetImage) do
    begin
      FreeOnClose := True;
      ShowOnTop();
    end;
  end;

  procedure DoACA;
  begin
    with TSimbaACA.Create(@DoGetTargetImage) do
    begin
      FreeOnClose := True;
      ShowOnTop();
    end;
  end;

  procedure DoAssociate;
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

  procedure DoQuit;
  begin
    Close();
  end;

  procedure DoCompilerHints(MenuItem: TMenuItem);
  begin
    SimbaSettings.Compiler.ShowHints.Value := MenuItem.Checked;
  end;

begin
  case Event of
    ESimbaEvent.ACTION_RESET_LAYOUT:   DoResetLayout();
    ESimbaEvent.ACTION_COMPILER_HINTS: DoCompilerHints(TMenuItem(Data));
    ESimbaEvent.ACTION_LOCK_LAYOUT:    DoLockLayout(TMenuItem(Data));
    ESimbaEvent.ACTION_VIEW_TRAYICON:  DoViewTrayIcon(TMenuItem(Data));
    ESimbaEvent.ACTION_REPORTBUG:      DoReportBug();
    ESimbaEvent.ACTION_SIMBAGITHUB:    DoSimbaGithub();
    ESimbaEvent.ACTION_ONLINEDOCS:     DoOnlineDocs();
    ESimbaEvent.ACTION_ASSOCIATE:      DoAssociate();
    ESimbaEvent.ACTION_ACA:            DoACA();
    ESimbaEvent.ACTION_DTM_EDITOR:     DoDTMEditor();
    ESimbaEvent.ACTION_QUIT:           DoQuit();
  end;
end;

function TSimbaMainForm.DoGetTargetImage: TSimbaImage;
begin
  if SimbaIDEVars.WindowSelection.IsValid() then
    Result := TSimbaImage.CreateFromWindow(SimbaIDEVars.WindowSelection)
  else
    Result := TSimbaImage.CreateFromWindow(GetDesktopWindow());
end;

procedure TSimbaMainForm.TrayIconClick(Sender: TObject);
begin
  ShowOnTop();
  if CanSetFocus() then
    SetFocus();
end;

procedure SetupSimbaForm;
begin
  if (SimbaMainForm = nil) then
    SimbaException('SimbaForm is nil');

  SimbaMainForm.Setup();
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @SetupSimbaForm, 'SimbaForm');

end.
