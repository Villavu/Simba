{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_scripttabs;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Menus,
  simba.base,
  simba.ide_events,
  simba.ide_tab,
  simba.ide_findpanel,
  simba.ide_editor_findreplace,
  simba.component_tabcontrol;

type
  TSimbaScriptTabsForm = class(TForm)
  private
    FTabControl: TSimbaTabControl;
    FTabPopup: TPopupMenu;

    FMenuItemNewTab: TMenuItem;
    FMenuItemClose: TMenuItem;
    FMenuItemCloseOther: TMenuItem;
    FMenuItemCloseOnRight: TMenuItem;
    FMenuItemCloseAll: TMenuItem;

    FMouseDown: Boolean;
    FMouseDownX: Integer;
    FMouseDownY: Integer;

    FFindPanel: TSimbaFindPanel;
    FEditorReplace: TSimbaEditorReplace;
    FEditorFind: TSimbaEditorFind;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

    procedure DoDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DoMenuItemClick(Sender: TObject);

    procedure DoTabCanChange(Sender: TSimbaTabControl; OldTab, NewTab: TSimbaTab; var AllowChange: Boolean);
    procedure DoTabChange(Sender: TSimbaTabControl; NewTab: TSimbaTab);
    procedure DoTabMoved(Sender: TSimbaTabControl; AFrom, ATo: Integer);
    procedure DoTabClosed(Sender: TSimbaTabControl; Tab: TSimbaTab; var CanClose: Boolean);

    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure DoFindEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoFindEditChange(Sender: TObject);
    procedure DoFindButtonDownClick(Sender: TObject);
    procedure DoFindButtonUpClick(Sender: TObject);

    function GetActiveTab: TSimbaScriptTab;
    function GetTabByIndex(Index: Integer): TSimbaScriptTab;
    function GetTabCount: Integer;
    procedure SetActiveTab(Value: TSimbaScriptTab);
  public
    constructor Create; reintroduce;

    function Open(FileName: String; CheckOtherTabs: Boolean = True): Boolean;
    procedure OpenFromDialog;

    function HasTab(Tab: TSimbaScriptTab): Boolean;
    function FindTab(ID: Int64): TSimbaScriptTab;
    function AddTab: TSimbaScriptTab;
    function CloseTab(Tab: TSimbaScriptTab; KeepOne: Boolean = True): Boolean;
    function CloseAllTabs(KeepOne: Boolean = True): Boolean;

    procedure Find;
    procedure FindNext;
    procedure FindPrev;
    procedure Replace;

    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TSimbaScriptTab read GetTabByIndex;
    property ActiveTab: TSimbaScriptTab read GetActiveTab write SetActiveTab;
  end;

var
  SimbaScriptTabsForm: TSimbaScriptTabsForm;

implementation

uses
  LCLType, AnchorDocking, Clipbrd,
  simba.env,
  simba.vartype_string,
  simba.component_images,
  simba.ide_docking,
  simba.ide_simpleformatter,
  simba.ide_controller,
  simba.ide_editor_commands,
  simba.settings,
  simba.initializations;

procedure TSimbaScriptTabsForm.Find;
begin
  FFindPanel.Show();
  FFindPanel.Edit.SelectAll();
  if FFindPanel.Edit.CanSetFocus() then
    FFindPanel.Edit.SetFocus();
end;

procedure TSimbaScriptTabsForm.FindNext;
begin
  FEditorFind.FindNext(ActiveTab.Editor);
end;

procedure TSimbaScriptTabsForm.FindPrev;
begin
  FEditorFind.FindPrev(ActiveTab.Editor);
end;

procedure TSimbaScriptTabsForm.Replace;
begin
  FEditorReplace.Execute(ActiveTab.Editor);
end;

procedure TSimbaScriptTabsForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoViewEditor(Item: TMenuItem);
  begin
    SimbaDocking.Show(Self);
  end;

  procedure DoRunCompileStopPause(Tab: TSimbaScriptTab);
  begin
         if (Event = ESimbaEvent.ACTION_RUN)     then Tab.Run()
    else if (Event = ESimbaEvent.ACTION_COMPILE) then Tab.Compile()
    else if (Event = ESimbaEvent.ACTION_PAUSE)   then Tab.Pause()
    else if (Event = ESimbaEvent.ACTION_STOP)    then Tab.Stop();

    if Tab.Editor.CanSetFocus() then
      Tab.Editor.SetFocus();
  end;

  procedure DoCloseTab(Tab: TSimbaScriptTab);
  begin
    CloseTab(Tab);
  end;

  procedure DoCloseAllTabs();
  begin
    CloseAllTabs(True);
  end;

  procedure DoSave(Tab: TSimbaScriptTab);
  begin
    Tab.Save(Tab.ScriptFileName);
  end;

  procedure DoSaveAs(Tab: TSimbaScriptTab);
  begin
    Tab.Save('');
  end;

  procedure DoSaveAll;
  var
    I: Integer;
  begin
    for I := TabCount - 1 downto 0 do
      if Tabs[I].CanSave() then
      begin
        if (Tabs[I].ScriptFileName = '') then
          Tabs[I].Show();
        Tabs[I].Save(Tabs[I].ScriptFileName);
      end;
  end;

  procedure DoSaveAsDefault(Tab: TSimbaScriptTab);
  begin
    if MessageDlg('Are you sure you want to overwrite the default script?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    begin
      SimbaSettings.Editor.DefaultScript.Value := Tab.Script;
      SimbaSettings.Editor.DefaultScriptType.Value := 1;
    end;
  end;

  procedure DoNew;
  begin
    AddTab();
  end;

  procedure DoOpen;
  begin
    OpenFromDialog();
  end;

  procedure DoOpenFile(FileName: String);
  begin
    Open(FileName);
  end;

  procedure DoUndo(Tab: TSimbaScriptTab);
  begin
    if not Tab.Editor.ReadOnly then
      Tab.Editor.Undo();
  end;

  procedure DoRedo(Tab: TSimbaScriptTab);
  begin
    if not Tab.Editor.ReadOnly then
      Tab.Editor.Redo();
  end;

  procedure DoCut(Tab: TSimbaScriptTab);
  begin
    if not Tab.Editor.ReadOnly then
      Tab.Editor.CutToClipboard();
  end;

  procedure DoCopy(Tab: TSimbaScriptTab);
  begin
    Tab.Editor.CopyToClipboard();
  end;

  procedure DoPaste(Tab: TSimbaScriptTab);
  begin
    if not Tab.Editor.ReadOnly then
      Tab.Editor.PasteFromClipboard();
  end;

  procedure DoSelectAll(Tab: TSimbaScriptTab);
  begin
    Tab.Editor.SelectAll();
  end;

  procedure DoSelectLine(Tab: TSimbaScriptTab);
  begin
    Tab.Editor.SelectLine();
  end;

  procedure DoSelectWord(Tab: TSimbaScriptTab);
  begin
    Tab.Editor.SelectWord();
  end;

  procedure DoLowerSelection(Tab: TSimbaScriptTab);
  begin
    if not Tab.Editor.ReadOnly and Tab.Editor.SelAvail then
      Tab.Editor.SelText := LowerCase(Tab.Editor.SelText);
  end;

  procedure DoUpperSelection(Tab: TSimbaScriptTab);
  begin
    if not Tab.Editor.ReadOnly and Tab.Editor.SelAvail then
      Tab.Editor.SelText := UpperCase(Tab.Editor.SelText);
  end;

  procedure DoFind();
  begin
    Find();
  end;

  procedure DoFindNext();
  begin
    FindNext();
  end;

  procedure DoFindPrev();
  begin
    FindPrev();
  end;

  procedure DoReplace;
  begin
    Replace();
  end;

  procedure DoGotoLine;
  var
    Value: String;
  begin
    Value := '';
    if InputQuery('Goto line', 'Goto line:', Value) and Value.IsNumeric then
      ActiveTab.Editor.TopLine := Value.ToInt - (ActiveTab.Editor.LinesInWindow div 2);
  end;

  procedure DoFormatScript(Tab: TSimbaScriptTab);
  var
    Script: String;
  begin
    Tab.Editor.BeginUndoBlock();
    try
      if Tab.Editor.SelAvail then
        Tab.Editor.SelText := FormatScript(Tab.Editor.SelText)
      else
      begin
        Script := FormatScript(Tab.Editor.Text);

        Tab.Editor.ClearAll();
        Tab.Editor.InsertTextAtCaret(Script);
      end;
    finally
      Tab.Editor.EndUndoBlock();
    end;
  end;

  procedure DoFindDeclAtCaret(Tab: TSimbaScriptTab);
  begin
    Tab.FindDeclarationAtCaret();
  end;

  procedure DoCopyFileName(Tab: TSimbaScriptTab);
  begin
    Clipboard.AsText := Tab.ScriptFileName;
  end;

  procedure DoOpenDirectory(Tab: TSimbaScriptTab);
  begin
    SimbaController.OpenInExplorer(ExtractFileDir(Tab.ScriptFileName));
  end;

  procedure DoDocComment(Tab: TSimbaScriptTab);
  begin
    Tab.Editor.ExecuteSimpleCommand(ecDocumentation);
  end;

  procedure DoTabActiveTimer(Tab: TSimbaScriptTab);
  begin
    SimbaEvents.Post(ESimbaEvent.TAB_ACTIVE_750, Tab);
  end;

begin
  if (FTabControl.TabCount = 0) then // for safety
    Exit;

  case Event of
    ESimbaEvent.ACTION_VIEW_EDITOR: DoViewEditor(TMenuItem(Data));

    ESimbaEvent.ACTION_RUN,
    ESimbaEvent.ACTION_COMPILE,
    ESimbaEvent.ACTION_PAUSE,
    ESimbaEvent.ACTION_STOP: DoRunCompileStopPause(ActiveTab);

    ESimbaEvent.ACTION_CLOSE_TAB:       DoCloseTab(ActiveTab);
    ESimbaEvent.ACTION_CLOSE_ALL_TABS:  DoCloseAllTabs();
    ESimbaEvent.ACTION_SAVE:            DoSave(ActiveTab);
    ESimbaEvent.ACTION_SAVE_AS:         DoSaveAs(ActiveTab);
    ESimbaEvent.ACTION_SAVE_ALL:        DoSaveAll();
    ESimbaEvent.ACTION_SAVE_AS_DEFAULT: DoSaveAsDefault(ActiveTab);
    ESimbaEvent.ACTION_NEW:             DoNew();
    ESimbaEvent.ACTION_OPEN:            DoOpen();
    ESimbaEvent.ACTION_UNDO:            DoUndo(ActiveTab);
    ESimbaEvent.ACTION_REDO:            DoRedo(ActiveTab);
    ESimbaEvent.ACTION_CUT:             DoCut(ActiveTab);
    ESimbaEvent.ACTION_COPY:            DoCopy(ActiveTab);
    ESimbaEvent.ACTION_PASTE:           DoPaste(ActiveTab);
    ESimbaEvent.ACTION_SELECT_ALL:      DoSelectAll(ActiveTab);
    ESimbaEvent.ACTION_SELECT_LINE:     DoSelectLine(ActiveTab);
    ESimbaEvent.ACTION_SELECT_WORD:     DoSelectWord(ActiveTab);
    ESimbaEvent.ACTION_LOWER_SELECTION: DoLowerSelection(ActiveTab);
    ESimbaEvent.ACTION_UPPER_SELECTION: DoUpperSelection(ActiveTab);

    ESimbaEvent.ACTION_FIND:      DoFind();
    ESimbaEvent.ACTION_FIND_NEXT: DoFindNext();
    ESimbaEvent.ACTION_FIND_PREV: DoFindPrev();
    ESimbaEvent.ACTION_REPLACE:   DoReplace();
    ESimbaEvent.ACTION_GOTO_LINE: DoGotoLine();

    ESimbaEvent.ACTION_FORMAT_SCRIPT: DoFormatScript(ActiveTab);

    ESimbaEvent.ACTION_FIND_DECL_AT_CARET: DoFindDeclAtCaret(ActiveTab);
    ESimbaEvent.ACTION_COPY_FILENAME: DoCopyFileName(ActiveTab);
    ESimbaEvent.ACTION_OPEN_DIRECTORY: DoOpenDirectory(ActiveTab);
    ESimbaEvent.ACTION_DOC_COMMENT: DoDocComment(ActiveTab);

    ESimbaEvent.TIMER_750: DoTabActiveTimer(ActiveTab);
  end;
end;

procedure TSimbaScriptTabsForm.DoDropFiles(Sender: TObject; const FileNames: array of String);
var
  FileName: String;
begin
  for FileName in FileNames do
    Open(FileName, True);
end;

procedure TSimbaScriptTabsForm.DoMenuItemClick(Sender: TObject);

  procedure CloseOtherTabs(Tab: TSimbaScriptTab);
  var
    I: Integer;
  begin
    for I := TabCount - 1 downto 0 do
      if (Tabs[I] <> Tab) and (not CloseTab(Tabs[I])) then
        Exit;
  end;

  procedure CloseTabsOnRight(Tab: TSimbaScriptTab);
  var
    I: Integer;
  begin
    for I := TabCount - 1 downto 0 do
      if (Tabs[I] = Tab) or (not CloseTab(Tabs[I])) then
        Exit;
  end;

var
  Tab: TSimbaScriptTab;
begin
  if (Sender = FMenuItemNewTab) then
    AddTab()
  else
  begin
    with FTabControl.ScreenToClient(FTabPopup.PopupPoint) do
      Tab := TSimbaScriptTab(FTabControl.GetTabAt(X, Y));

    if Assigned(Tab) then
    begin
      if (Sender = FMenuItemClose)        then CloseTab(Tab);
      if (Sender = FMenuItemCloseOther)   then CloseOtherTabs(Tab);
      if (Sender = FMenuItemCloseOnRight) then CloseTabsOnRight(Tab);
      if (Sender = FMenuItemCloseAll)     then CloseAllTabs();
    end;
  end;
end;

procedure TSimbaScriptTabsForm.DoTabCanChange(Sender: TSimbaTabControl; OldTab, NewTab: TSimbaTab; var AllowChange: Boolean);
begin
  if (OldTab is TSimbaScriptTab) then
    SimbaEvents.Post(ESimbaEvent.TAB_BEFORECHANGE, OldTab);
end;

procedure TSimbaScriptTabsForm.DoTabChange(Sender: TSimbaTabControl; NewTab: TSimbaTab);
begin
  SimbaEvents.Post(ESimbaEvent.TAB_CHANGE, NewTab);
  if (NewTab is TSimbaScriptTab) and TSimbaScriptTab(NewTab).Editor.CanSetFocus() then
    TSimbaScriptTab(NewTab).Editor.SetFocus();
end;

procedure TSimbaScriptTabsForm.DoTabMoved(Sender: TSimbaTabControl; AFrom, ATo: Integer);
var
  Data: TSimbaEvents.TTabMoved;
begin
  Data.Tab := Self;
  Data.FromIndex := AFrom;
  Data.ToIndex := ATo;

  SimbaEvents.Post(ESimbaEvent.TAB_MOVED, @Data);
end;

procedure TSimbaScriptTabsForm.DoTabClosed(Sender: TSimbaTabControl; Tab: TSimbaTab; var CanClose: Boolean);
begin
  CanClose := TSimbaScriptTab(Tab).CanClose();
  if CanClose and (FTabControl.TabCount <= 1) and FTabControl.IsClickingCloseButton then
    AddTab();
end;

procedure TSimbaScriptTabsForm.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := True;
  FMouseDownX := X;
  FMouseDownY := Y;
end;

procedure TSimbaScriptTabsForm.DoMouseLeave(Sender: TObject);
begin
  FMouseDown := False;
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaScriptTabsForm.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  // Needs special handling so double click to add tabs works
  // Forwarding all mouse events to header wont work.
  // Only forward events if dragged > 10 distance.
  function CanAnchorDocking(X, Y: Integer): Boolean;
  begin
    Result := FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging) and (Abs(X - FMouseDownX) > 10) and (Abs(Y - FMouseDownY) > 10);
  end;

begin
  if FMouseDown and CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    if TSimbaAnchorDockHostSite(HostDockSite).Header.Dragging then
      TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y)
    else
      TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(mbLeft, Shift, X, Y);
  end;
end;

procedure TSimbaScriptTabsForm.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tab: TSimbaScriptTab;
begin
  if (Button = mbMiddle) then
  begin
    Tab := TSimbaScriptTab(FTabControl.GetTabAt(X, Y));
    if (Tab <> nil) then
      CloseTab(Tab);
  end;

  FMouseDown := False;

  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseUp(Button, Shift, X, Y);
end;

procedure TSimbaScriptTabsForm.DoFindEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F3:
      begin
        if (ssShift in Shift) then
          FFindPanel.ButtonUp.Click()
        else
          FFindPanel.ButtonDown.Click();

        Key := 0;
      end;

    VK_F:
      if (ssCtrl in Shift) then
      begin
        FFindPanel.Hide();
        Key := 0;
      end;

    VK_ESCAPE:
      begin
        FFindPanel.Hide();
        Key := 0;
      end;

    VK_UP:
      begin
        FFindPanel.ButtonUp.Click();
        Key := 0;
      end;

    VK_RETURN, VK_DOWN:
      begin
        FFindPanel.ButtonDown.Click();
        Key := 0;
      end;
  end;
end;

procedure TSimbaScriptTabsForm.DoFindEditChange(Sender: TObject);
begin
  FEditorFind.ExecuteNoDialog(
    ActiveTab.Editor,
    FFindPanel.Edit.Text,
    FFindPanel.ButtonCaseSens.Down,
    FFindPanel.ButtonWholeWord.Down
  );
end;

procedure TSimbaScriptTabsForm.DoFindButtonDownClick(Sender: TObject);
begin
  FEditorFind.FindNext(ActiveTab.Editor);
end;

procedure TSimbaScriptTabsForm.DoFindButtonUpClick(Sender: TObject);
begin
  FEditorFind.FindPrev(ActiveTab.Editor);
end;

function TSimbaScriptTabsForm.GetActiveTab: TSimbaScriptTab;
begin
  Result := TSimbaScriptTab(FTabControl.ActiveTab);
end;

function TSimbaScriptTabsForm.GetTabByIndex(Index: Integer): TSimbaScriptTab;
begin
  Result := TSimbaScriptTab(FTabControl.Tabs[Index]);
end;

function TSimbaScriptTabsForm.GetTabCount: Integer;
begin
  Result := FTabControl.TabCount;
end;

procedure TSimbaScriptTabsForm.SetActiveTab(Value: TSimbaScriptTab);
begin
  FTabControl.ActiveTab := Value;
end;

constructor TSimbaScriptTabsForm.Create;

  function addItem(Menu: TMenu; Image: Integer; Text: String; Shortcut: TShortCut): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := Text;
    Result.OnClick := @DoMenuItemClick;
    Result.ImageIndex := Image;
    Result.ShortCut := Shortcut;

    Menu.Items.Add(Result);
  end;

begin
  inherited Create(nil);

  Name := 'SimbaScriptTabsForm'; // important - docking requires control names
  Caption := 'Script Tabs';
  OnDropFiles := @DoDropFiles;

  FFindPanel := TSimbaFindPanel.Create(Self);
  FFindPanel.Parent := Self;
  FFindPanel.Align := alBottom;
  FFindPanel.Visible := False;
  FFindPanel.Edit.OnKeyDown := @DoFindEditKeyDown;
  FFindPanel.Edit.OnChange := @DoFindEditChange;
  FFindPanel.ButtonDown.OnClick := @DoFindButtonDownClick;
  FFindPanel.ButtonUp.OnClick := @DoFindButtonUpClick;

  FEditorReplace := TSimbaEditorReplace.Create(Self);
  FEditorFind := TSimbaEditorFind.Create(Self);

  FTabPopup := TPopupMenu.Create(Self);
  FTabPopup.Images := SimbaImages;
  FMenuItemNewTab := addItem(FTabPopup, SimbaImages.NEW, 'New Tab', ShortCut(VK_N, [ssCtrl]));
  FMenuItemClose := addItem(FTabPopup, SimbaImages.CLOSE, 'Close Tab', ShortCut(VK_W, [ssCtrl]));
  FMenuItemCloseOther := addItem(FTabPopup, -1, 'Close Other Tabs', scNone);
  FMenuItemCloseOnRight := addItem(FTabPopup, -1, 'Close Tabs on Right', scNone);
  FMenuItemCloseAll := addItem(FTabPopup, SimbaImages.CLOSE_ALL, 'Close All Tabs', scNone);

  FTabControl := TSimbaTabControl.Create(Self, TSimbaScriptTab);
  FTabControl.Parent := Self;
  FTabControl.Align := alClient;
  FTabControl.PopupMenu := FTabPopup;
  FTabControl.OnTabMoved := @DoTabMoved;
  FTabControl.OnTabClose := @DoTabClosed;
  FTabControl.OnTabCanChange := @DoTabCanChange;
  FTabControl.OnTabChange := @DoTabChange;
  FTabControl.OnMouseMove := @DoMouseMove;
  FTabControl.OnMouseDown := @DoMouseDown;
  FTabControl.OnMouseUp := @DoMouseUp;
  FTabControl.OnMouseLeave := @DoMouseLeave;
  FTabControl.DefaultTitle := 'Untitled';
  FTabControl.CanAddTabOnDoubleClick := True;

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.ACTION_VIEW_EDITOR,
    ESimbaEvent.ACTION_RUN,
    ESimbaEvent.ACTION_COMPILE,
    ESimbaEvent.ACTION_PAUSE,
    ESimbaEvent.ACTION_STOP,
    ESimbaEvent.ACTION_CLOSE_TAB,
    ESimbaEvent.ACTION_CLOSE_ALL_TABS,
    ESimbaEvent.ACTION_SAVE,
    ESimbaEvent.ACTION_SAVE_AS,
    ESimbaEvent.ACTION_SAVE_ALL,
    ESimbaEvent.ACTION_SAVE_AS_DEFAULT,
    ESimbaEvent.ACTION_NEW,
    ESimbaEvent.ACTION_OPEN,
    ESimbaEvent.ACTION_UNDO,
    ESimbaEvent.ACTION_REDO,
    ESimbaEvent.ACTION_CUT,
    ESimbaEvent.ACTION_COPY,
    ESimbaEvent.ACTION_PASTE,
    ESimbaEvent.ACTION_SELECT_ALL,
    ESimbaEvent.ACTION_SELECT_LINE,
    ESimbaEvent.ACTION_SELECT_WORD,
    ESimbaEvent.ACTION_LOWER_SELECTION,
    ESimbaEvent.ACTION_UPPER_SELECTION,
    ESimbaEvent.ACTION_FIND,
    ESimbaEvent.ACTION_FIND_NEXT,
    ESimbaEvent.ACTION_FIND_PREV,
    ESimbaEvent.ACTION_REPLACE,
    ESimbaEvent.ACTION_GOTO_LINE,
    ESimbaEvent.ACTION_FORMAT_SCRIPT,
    ESimbaEvent.ACTION_FIND_DECL_AT_CARET,
    ESimbaEvent.ACTION_COPY_FILENAME,
    ESimbaEvent.ACTION_OPEN_DIRECTORY,
    ESimbaEvent.TIMER_750
  ]);

  AddTab();
end;

function TSimbaScriptTabsForm.Open(FileName: String; CheckOtherTabs: Boolean): Boolean;
var
  I: Integer;
begin
  FileName := ExpandFileName(FileName);

  if CheckOtherTabs then
    for I := 0 to TabCount - 1 do
      if SameFileName(Tabs[I].ScriptFileName, FileName) then
      begin
        ActiveTab := Tabs[I];
        SimbaEvents.Post(ESimbaEvent.TAB_LOADED, ActiveTab);
        Exit(True);
      end;

  if FileExists(FileName) then
  begin
    if (ActiveTab.ScriptFileName <> '') or ActiveTab.CanSave() then // Use current tab if default
      ActiveTab := AddTab();

    Result := ActiveTab.Load(FileName);
  end;
end;

procedure TSimbaScriptTabsForm.OpenFromDialog;
var
  I: Integer;
begin
  with TOpenDialog.Create(Self) do
  try
    Options := [ofEnableSizing, ofAllowMultiSelect, ofFileMustExist];
    InitialDir := ExtractFileDir(ActiveTab.ScriptFileName);
    if (InitialDir = '') then
      InitialDir := SimbaEnv.ScriptsPath;

    if Execute() then
      for I := 0 to Files.Count - 1 do
        Open(Files[I], True);
  except
    on E: Exception do
      ShowMessage('Opening file exception: ' + E.Message);
  end;
end;

function TSimbaScriptTabsForm.HasTab(Tab: TSimbaScriptTab): Boolean;
var
  I: Integer;
begin
  for I := 0 to FTabControl.TabCount - 1 do
    if (FTabControl.Tabs[I] = Tab) then
      Exit(True);
  Result := False;
end;

function TSimbaScriptTabsForm.FindTab(ID: Int64): TSimbaScriptTab;
begin
  Result := TSimbaScriptTab(FTabControl.FindTab(ID));
end;

function TSimbaScriptTabsForm.AddTab: TSimbaScriptTab;
begin
  Result := FTabControl.AddTab() as TSimbaScriptTab;
end;

function TSimbaScriptTabsForm.CloseTab(Tab: TSimbaScriptTab; KeepOne: Boolean): Boolean;
begin
  Result := FTabControl.DeleteTab(Tab);
  if (FTabControl.TabCount = 0) then
    AddTab();
end;

function TSimbaScriptTabsForm.CloseAllTabs(KeepOne: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := TabCount - 1 downto 0 do
    if not CloseTab(Tabs[I], KeepOne) then
    begin
      Result := False;
      Exit;
    end;
end;

procedure DoCreate;
begin
  SimbaScriptTabsForm := TSimbaScriptTabsForm.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaScriptTabsForm);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaScriptTabsForm');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaScriptTabsForm');

end.
