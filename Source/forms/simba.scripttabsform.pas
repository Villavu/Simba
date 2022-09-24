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
  stdctrls, extctrls, comctrls, extendednotebook, menus, synedit, synedittypes,
  simba.scripttab, simba.editor, simba.codeparser, simba.editor_findreplace;

type
  TSimbaScriptTabsForm = class(TForm)
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
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  protected
    FEditorReplace: TSimbaEditorReplace;
    FEditorFind: TSimbaEditorFind;

    FOnEditorAdded: TNotifyEvent;
    FOnEditorChanged: TNotifyEvent;
    FOnEditorLoaded: TNotifyEvent;
    FOnEditorCaretChanged: TNotifyEvent;

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

    procedure OpenDeclaration(StartPos, EndPos, Line: Integer; FileName: String); overload;
    procedure OpenInternalDeclaration(Header: String; FileName: String);
    procedure OpenDeclaration(Declaration: TDeclaration); overload;
  end;

var
  SimbaScriptTabsForm: TSimbaScriptTabsForm;

implementation

{$R *.lfm}

uses
  simba.mufasatypes, simba.scripttabhistory, simba.files, simba.editor_docgenerator,
  simba.dockinghelpers, simba.nativeinterface;

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
  if (Notebook.TabIndex > -1) then
    Result := Notebook.Pages[Notebook.TabIndex] as TSimbaScriptTab
  else
    Result := nil;
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
  if (CurrentEditor <> nil) then
    FEditorFind.Execute(CurrentEditor);
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

procedure TSimbaScriptTabsForm.OpenDeclaration(StartPos, EndPos, Line: Integer; FileName: String);
begin
  if FileExists(FileName) then
    Open(FileName);

  with CurrentEditor do
  begin
    SelStart := StartPos + 1;
    SelEnd := EndPos + 1;
    TopLine := (Line + 1) - (LinesInWindow div 2);
    if CanSetFocus() then
      SetFocus();

    SimbaScriptTabHistory.Add(CurrentTab);
  end;
end;

procedure TSimbaScriptTabsForm.OpenInternalDeclaration(Header: String; FileName: String);
begin
  if (Header = '') then
    Exit;

  SimbaDebugLn('Declared internally in Simba: ' + FileName);
  SimbaDebugLn('Declaration:');
  SimbaDebugLn(Header);
end;

procedure TSimbaScriptTabsForm.OpenDeclaration(Declaration: TDeclaration);
var
  FileName: String;
  IsLibrary: Boolean;
begin
  FileName := Declaration.Lexer.FileName;
  IsLibrary := Declaration.Lexer.IsLibrary;

  if (FileName = '') or FileExists(FileName) and (not IsLibrary) then
  begin
    if FileExists(FileName) then
      Open(FileName);

    CurrentEditor.SelStart := Declaration.StartPos + 1;
    CurrentEditor.SelEnd := Declaration.EndPos + 1;
    CurrentEditor.TopLine := (Declaration.Line + 1) - (CurrentEditor.LinesInWindow div 2);
    if CurrentEditor.CanSetFocus() then
      CurrentEditor.SetFocus();

    SimbaScriptTabHistory.Add(CurrentTab);
  end else
  begin
    if IsLibrary then
      SimbaDebugLn('Declared internally in plugin: ' + FileName)
    else
      SimbaDebugLn('Declared internally in Simba: ' + FileName);

    SimbaDebugLn('Declaration:');

    if Declaration is TciProcedureDeclaration then
      SimbaDebugLn(TciProcedureDeclaration(Declaration).Header)
    else
      SimbaDebugLn(Declaration.RawText);
  end;
end;

end.

