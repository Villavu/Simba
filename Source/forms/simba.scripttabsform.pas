{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scripttabsform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs,
  extctrls, comctrls, extendednotebook, menus, StdCtrls, Buttons, synedit, synedittypes,
  simba.scripttab, simba.editor, simba.editor_findreplace;

type
  TEditorSearchEvent = procedure(MatchCount: Integer) of object;

  TSimbaScriptTabsForm = class(TForm)
    FindCheckBoxCaseSens: TCheckBox;
    FindCheckboxWholeWord: TCheckBox;
    FindButtonDown: TBitBtn;
    FindButtonUp: TBitBtn;
    FindEdit: TEdit;
    MenuItem1: TMenuItem;
    MenuItemOpenFileDir: TMenuItem;
    MenuItemCopyFileName: TMenuItem;
    MenuItemDocumentation: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemFindDeclaration: TMenuItem;
    MenuItemSeperator4: TMenuItem;
    MenuItemSeperator1: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemSeperator3: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemSeperator2: TMenuItem;
    OpenDialog: TOpenDialog;
    MenuItemNewTab: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseOtherTabs: TMenuItem;
    FindPanel: TPanel;
    FindButtonClose: TSpeedButton;
    TabPopupMenu: TPopupMenu;
    Notebook: TExtendedNotebook;
    EditorPopupMenu: TPopupMenu;

    procedure DoEditorPopupClick(Sender: TObject);
    procedure DoEditorPopupShow(Sender: TObject);
    procedure DoOnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DoOnTabChange(Sender: TObject);
    procedure DoTabPopupClick(Sender: TObject);
    procedure DoTabPopupOpen(Sender: TObject);
    // Open new tab if empty tab area on the right is clicked
    procedure DoDoubleClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FindButtonResize(Sender: TObject);
    procedure FindButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FindEditChange(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    // Keep output tabs in same order
    procedure NotebookTabDragOverEx(Sender, Source: TObject; OldIndex, NewIndex: Integer; CopyDrag: Boolean; var Accept: Boolean);
    procedure NotebookTabEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure NotebookTabStartDrag(Sender: TObject; var DragObject: TDragObject);
  protected
    FDraggingTab: record
      StartIndex: Integer;
      EndIndex: Integer;
    end;

    FEditorReplace: TSimbaEditorReplace;
    FEditorFind: TSimbaEditorFind;

    FOnEditorAdded: TNotifyEvent;
    FOnEditorChanged: TNotifyEvent;
    FOnEditorLoaded: TNotifyEvent;
    FOnEditorCaretChanged: TNotifyEvent;
    FOnSearch: TEditorSearchEvent;

    procedure CaretMoved(Sender: TObject; Changes: TSynStatusChanges);

    function GetTabCount: Integer;
    function GetTab(Index: Integer): TSimbaScriptTab;
    function GetCurrentTab: TSimbaScriptTab;
    function GetCurrentEditor: TSimbaEditor;

    procedure SetCurrentTab(Value: TSimbaScriptTab);
  public
    constructor Create(AOwner: TComponent); override;

    property OnEditorAdded: TNotifyEvent read FOnEditorAdded write FOnEditorAdded;
    property OnEditorLoaded: TNotifyEvent read FOnEditorLoaded write FOnEditorLoaded;
    property OnEditorChanged: TNotifyEvent read FOnEditorChanged write FOnEditorChanged;
    property OnEditorCaretChanged: TNotifyEvent read FOnEditorCaretChanged write FOnEditorCaretChanged;
    property OnSearch: TEditorSearchEvent read FOnSearch write FOnSearch;

    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TSimbaScriptTab read GetTab;

    property CurrentTab: TSimbaScriptTab read GetCurrentTab write SetCurrentTab;
    property CurrentEditor: TSimbaEditor read GetCurrentEditor;

    procedure Replace;
    procedure Find;
    procedure FindNext;
    procedure FindPrevious;

    function FindTab(ScriptInstance: TObject): TSimbaScriptTab;

    function AddTab: TSimbaScriptTab;

    function CloseTab(Tab: TSimbaScriptTab): Boolean;
    function CloseOtherTabs(Tab: TSimbaScriptTab): Boolean;
    function CloseAllTabs: Boolean;

    function Open(FileName: String; CheckOtherTabs: Boolean = True): Boolean; overload;
    procedure Open; overload;
  end;

var
  SimbaScriptTabsForm: TSimbaScriptTabsForm;

implementation

{$R *.lfm}

uses
  LCLType,
  simba.mufasatypes, simba.files, simba.editor_docgenerator,
  simba.dockinghelpers, simba.nativeinterface, simba.outputform;

procedure TSimbaScriptTabsForm.DoEditorPopupShow(Sender: TObject);
var
  CurrentWord: String;
begin
  if (CurrentEditor = nil) then
    EditorPopupMenu.Close()
  else
  begin
    CurrentWord := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);

    if (CurrentWord <> '') then
    begin
      MenuItemFindDeclaration.Caption := 'Find Declaration of ' + CurrentWord;
      MenuItemFindDeclaration.Enabled := True;
    end else
    begin
      MenuItemFindDeclaration.Caption := 'Find Declaration';
      MenuItemFindDeclaration.Enabled := False;
    end;

    MenuItemUndo.Enabled   := CurrentEditor.CanUndo;
    MenuItemRedo.Enabled   := CurrentEditor.CanRedo;
    MenuItemPaste.Enabled  := CurrentEditor.CanPaste;
    MenuItemCut.Enabled    := CurrentEditor.SelAvail;
    MenuItemCopy.Enabled   := CurrentEditor.SelAvail;
    MenuItemDelete.Enabled := CurrentEditor.SelAvail;
  end;
end;

procedure TSimbaScriptTabsForm.DoEditorPopupClick(Sender: TObject);
begin
  if (CurrentTab = nil) or (CurrentEditor = nil) then
    Exit;

  if (Sender = MenuItemFindDeclaration) then CurrentEditor.OnClickLink(Self, mbLeft, [], CurrentEditor.CaretX, CurrentEditor.CaretY);
  if (Sender = MenuItemUndo)            then CurrentEditor.Undo();
  if (Sender = MenuItemRedo)            then CurrentEditor.Redo();
  if (Sender = MenuItemCut)             then CurrentEditor.CutToClipboard();
  if (Sender = MenuItemCopy)            then CurrentEditor.CopyToClipboard();
  if (Sender = MenuItemPaste)           then CurrentEditor.PasteFromClipboard();
  if (Sender = MenuItemDelete)          then CurrentEditor.ClearSelection();
  if (Sender = MenuItemSelectAll)       then CurrentEditor.SelectAll();
  if (Sender = MenuItemDocumentation)   then CurrentEditor.ExecuteSimpleCommand(TSimbaEditorPlugin_DocGenerator.EditorCommand);
  if (Sender = MenuItemCopyFileName)    then CurrentEditor.DoCopyToClipboard(CurrentTab.ScriptFileName);

  if (Sender = MenuItemFind)            then Self.Find();
  if (Sender = MenuItemReplace)         then Self.Replace();

  if (Sender = MenuItemOpenFileDir)     then SimbaNativeInterface.OpenDirectory(ExtractFileDir(CurrentTab.ScriptFileName));
end;

procedure TSimbaScriptTabsForm.DoOnDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  for I := 0 to High(FileNames) do
    Self.Open(FileNames[I], True);
end;

procedure TSimbaScriptTabsForm.DoOnTabChange(Sender: TObject);
begin
  if (FOnEditorChanged <> nil) then
    FOnEditorChanged(CurrentTab);
end;

procedure TSimbaScriptTabsForm.DoTabPopupClick(Sender: TObject);
var
  Tab: Integer;
begin
  if (Sender = MenuItemNewTab) then
    AddTab()
  else
  begin
    Tab := Notebook.IndexOfPageAt(NoteBook.ScreenToClient(TabPopupMenu.PopupPoint));

    if (Tab >= 0) and (Tab < TabCount) then
    begin
      if (Sender = MenuItemCloseTab)       then CloseTab(Tabs[Tab]);
      if (Sender = MenuItemCloseOtherTabs) then CloseOtherTabs(Tabs[Tab]);
    end;
  end;
end;

procedure TSimbaScriptTabsForm.DoTabPopupOpen(Sender: TObject);
begin
  MenuItemCloseTab.Enabled := Notebook.IndexOfPageAt(NoteBook.ScreenToClient(TabPopupMenu.PopupPoint)) >= 0;
  MenuItemCloseOtherTabs.Enabled := Notebook.IndexOfPageAt(NoteBook.ScreenToClient(TabPopupMenu.PopupPoint)) >= 0;
end;

procedure TSimbaScriptTabsForm.DoDoubleClick(Sender: TObject);
begin
  if (ScreenToClient(Mouse.CursorPos).Y < NoteBook.TabRect(0).Height) then
    AddTab();
end;

procedure TSimbaScriptTabsForm.FindButtonClick(Sender: TObject);
begin
  if Sender.Equals(FindButtonUp) then
    FindPrevious()
  else
  if Sender.Equals(FindButtonDown) then
    FindNext()
  else
  if Sender.Equals(FindButtonClose) then
    FindPanel.Hide();
end;

procedure TSimbaScriptTabsForm.FindButtonResize(Sender: TObject);
var
  Button: TBitBtn absolute Sender;
begin
  Button.Width := Button.Height;

  if (Button.Width >= 45) then
    TBitBtn(Sender).ImageWidth := 32
  else
  if (Button.Width >= 35) then
    TBitBtn(Sender).ImageWidth := 24
  else
    TBitBtn(Sender).ImageWidth := 16;
end;

procedure TSimbaScriptTabsForm.FindButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FindPanel.Hide();
        if (CurrentEditor <> nil) and CurrentEditor.CanSetFocus() then
          CurrentEditor.SetFocus();
        Key := 0;
      end;

    VK_UP:
      begin
        FindButtonUp.Click();
        Key := 0;
      end;

    VK_RETURN, VK_DOWN:
      begin
        FindButtonDown.Click();
        Key := 0;
      end;
  end;
end;

procedure TSimbaScriptTabsForm.FindEditChange(Sender: TObject);
begin
  if (CurrentEditor <> nil) then
  begin
    FEditorFind.ExecuteNoDialog(CurrentEditor, FindEdit.Text, FindCheckBoxCaseSens.Checked, FindCheckboxWholeWord.Checked);
    if Assigned(FOnSearch) then
      FOnSearch(FEditorFind.Matches);
  end;
end;

procedure TSimbaScriptTabsForm.FindPanelResize(Sender: TObject);
begin
  FindEdit.Width := FindPanel.Width div 3;
end;

procedure TSimbaScriptTabsForm.FormDestroy(Sender: TObject);
begin
  SimbaScriptTabsForm := nil;
end;

procedure TSimbaScriptTabsForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaScriptTabsForm.FormMouseLeave(Sender: TObject);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaScriptTabsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y);
end;

procedure TSimbaScriptTabsForm.NotebookTabDragOverEx(Sender, Source: TObject; OldIndex, NewIndex: Integer; CopyDrag: Boolean; var Accept: Boolean);
begin
  FDraggingTab.EndIndex := NewIndex;
end;

procedure TSimbaScriptTabsForm.NotebookTabEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
 if (FDraggingTab.StartIndex <> FDraggingTab.EndIndex) then
   TCustomTabControl(SimbaOutputForm.PageControl).Pages.Move(FDraggingTab.StartIndex + 1, FDraggingTab.EndIndex + 1);
end;

procedure TSimbaScriptTabsForm.NotebookTabStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  FDraggingTab.StartIndex := Notebook.DraggingTabIndex;
end;

procedure TSimbaScriptTabsForm.CaretMoved(Sender: TObject; Changes: TSynStatusChanges);
begin
  if (FOnEditorCaretChanged <> nil) then
    FOnEditorCaretChanged(Sender);
end;

procedure TSimbaScriptTabsForm.SetCurrentTab(Value: TSimbaScriptTab);
begin
  Notebook.ActivePage := Value;
end;

constructor TSimbaScriptTabsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEditorReplace := TSimbaEditorReplace.Create(Self);
  FEditorFind := TSimbaEditorFind.Create(Self);

  OnDblClick := @DoDoubleClick;
end;

function TSimbaScriptTabsForm.GetTabCount: Integer;
begin
  Result := Notebook.PageCount;
end;

function TSimbaScriptTabsForm.GetTab(Index: Integer): TSimbaScriptTab;
begin
  Result := Notebook.Pages[Index] as TSimbaScriptTab;
end;

function TSimbaScriptTabsForm.GetCurrentTab: TSimbaScriptTab;
begin
  if (Notebook.TabIndex = -1) then
  begin
    DebugLn('[TSimbaScriptTabsForm.GetCurrentTab]: TabIndex = -1');

    Result := nil;
    Exit;
  end;

  Result := Notebook.Pages[Notebook.TabIndex] as TSimbaScriptTab;
end;

function TSimbaScriptTabsForm.GetCurrentEditor: TSimbaEditor;
begin
  if (CurrentTab <> nil) then
    Result := CurrentTab.Editor
  else
    Result := nil;
end;

procedure TSimbaScriptTabsForm.Replace;
begin
  if (CurrentEditor <> nil) then
    FEditorReplace.Execute(CurrentEditor);
end;

procedure TSimbaScriptTabsForm.Find;
begin
  FindPanel.Show();
  if FindEdit.CanSetFocus() then
    FindEdit.SetFocus();
end;

procedure TSimbaScriptTabsForm.FindNext;
begin
  if (CurrentEditor <> nil) then
    FEditorFind.FindNext(CurrentEditor);
end;

procedure TSimbaScriptTabsForm.FindPrevious;
begin
  if (CurrentEditor <> nil) then
    FEditorFind.FindPrev(CurrentEditor);
end;

function TSimbaScriptTabsForm.FindTab(ScriptInstance: TObject): TSimbaScriptTab;
var
  I: Integer;
begin
  for I := 0 to TabCount - 1 do
    if (Tabs[I].ScriptInstance = ScriptInstance) then
    begin
      Result := Tabs[I];
      Exit;
    end;

  Result := nil;
end;

function TSimbaScriptTabsForm.AddTab: TSimbaScriptTab;
begin
  Result := TSimbaScriptTab.Create(Notebook);
  Result.Parent := Notebook;
  Result.Editor.PopupMenu := EditorPopupMenu;
  Result.Editor.RegisterStatusChangedHandler(@CaretMoved, [scCaretX, scCaretY]);

  if (Result.TabIndex = 0) then
  begin
    NoteBook.TabIndex := Result.TabIndex;
    if (Notebook.OnChange <> nil) then
      NoteBook.OnChange(Self);
  end else
    NoteBook.TabIndex := Result.TabIndex;
end;

function TSimbaScriptTabsForm.CloseTab(Tab: TSimbaScriptTab): Boolean;
begin
  Result := Tab.CanClose();

  if Result then
  begin
    if (TabCount = 1) then
    begin
      Tab.Reset();
      if (Notebook.OnChange <> nil) then
        Notebook.OnChange(Self);
    end else
      Tab.Free();
  end;
end;

function TSimbaScriptTabsForm.CloseOtherTabs(Tab: TSimbaScriptTab): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := TabCount - 1 downto 0 do
    if (Tabs[I] <> Tab) and (not CloseTab(Tabs[I])) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSimbaScriptTabsForm.CloseAllTabs: Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := TabCount - 1 downto 0 do
    if not CloseTab(Tabs[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSimbaScriptTabsForm.Open(FileName: String; CheckOtherTabs: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;

  FileName := ExpandFileName(FileName);

  if CheckOtherTabs then
    for I := 0 to TabCount - 1 do
      if SameFileName(Tabs[I].ScriptFileName, FileName) then
      begin
        CurrentTab := Tabs[I];
        Result := True;
        Exit;
      end;

  if FileExists(FileName) then
  begin
    if (CurrentTab.ScriptFileName <> '') or CurrentTab.ScriptChanged then // Use current tab if default
      CurrentTab := AddTab();

    Result := CurrentTab.Load(FileName);
    if Result and (FOnEditorLoaded <> nil) then
      FOnEditorLoaded(CurrentTab);
  end;
end;

procedure TSimbaScriptTabsForm.Open;
var
  I: Integer;
begin
  try
    OpenDialog.InitialDir := ExtractFileDir(CurrentTab.ScriptFileName);
    if OpenDialog.InitialDir = '' then
      OpenDialog.InitialDir := GetScriptPath();

    if OpenDialog.Execute() then
      for I := 0 to OpenDialog.Files.Count - 1 do
        Open(OpenDialog.Files[I], True);
  except
    on E: Exception do
      ShowMessage('Exception while opening file: ' + E.Message);
  end;
end;

end.

