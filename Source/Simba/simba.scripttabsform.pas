unit simba.scripttabsform;

{$mode objfpc}{$H+}
{$I simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs,
  stdctrls, extctrls, comctrls, extendednotebook, menus, synedit, synedittypes,
  simba.scripttab, simba.editor, simba.codeparser;

type
  TSimbaScriptTabsForm = class(TForm)
    FindDialog: TFindDialog;
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
    ReplaceDialog: TReplaceDialog;
    Notebook: TExtendedNotebook;
    EditorPopupMenu: TPopupMenu;
    ClearLineMarksTimer: TTimer;

    procedure DoOnFindDialog(Sender: TObject);
    procedure DoOnReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure DoOnReplaceDialog(Sender: TObject);
    procedure DoEditorPopupClick(Sender: TObject);
    procedure DoEditorPopupShow(Sender: TObject);
    procedure DoOnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DoOnTabChange(Sender: TObject);
    procedure DoTabPopupClick(Sender: TObject);
    procedure DoTabPopupOpen(Sender: TObject);
    procedure HandleClearMarksTimer(Sender: TObject);
    procedure ReplaceDialogShow(Sender: TObject);
  protected
    FOnEditorAdded: TNotifyEvent;
    FOnEditorChanged: TNotifyEvent;
    FOnEditorLoaded: TNotifyEvent;
    FOnEditorCaretChanged: TNotifyEvent;
    FOnEditorSearch: TNotifyEvent;

    procedure CaretMoved(Sender: TObject; Changes: TSynStatusChanges);

    function GetTabCount: Integer;
    function GetTab(Index: Integer): TSimbaScriptTab;
    function GetCurrentTab: TSimbaScriptTab;
    function GetCurrentEditor: TSimbaEditor;

    procedure SetCurrentTab(Value: TSimbaScriptTab);
  public
    property OnEditorAdded: TNotifyEvent read FOnEditorAdded write FOnEditorAdded;
    property OnEditorLoaded: TNotifyEvent read FOnEditorLoaded write FOnEditorLoaded;
    property OnEditorChanged: TNotifyEvent read FOnEditorChanged write FOnEditorChanged;
    property OnEditorCaretChanged: TNotifyEvent read FOnEditorCaretChanged write FOnEditorCaretChanged;
    property OnEditorSearch: TNotifyEvent read FOnEditorSearch write FOnEditorSearch;

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

    procedure OpenDeclaration(Header: String; StartPos, EndPos, Line: Integer; FileName: String); overload;
    procedure OpenInternalDeclaration(Header: String; FileName: String);
    procedure OpenDeclaration(Declaration: TDeclaration); overload;
  end;

var
  SimbaScriptTabsForm: TSimbaScriptTabsForm;

implementation

{$R *.lfm}

uses
  syneditsearch, LCLStrConsts,
  simba.debugform, simba.scripttabhistory, simba.files;

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

procedure TSimbaScriptTabsForm.DoOnReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  case MessageDlg('Replace', Format('Replace "%s" with "%s"?', [ASearch, AReplace]), mtConfirmation, [mbYes, mbYesToAll, mbNo, mbCancel], 0) of
    mrYes:      ReplaceAction := raReplace;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrNo:       ReplaceAction := raSkip;
    mrCancel:   ReplaceAction := raCancel;
  end;
end;

procedure TSimbaScriptTabsForm.DoOnFindDialog(Sender: TObject);

  procedure AddLineMarks(CaseSensitive, WholeWorld: Boolean; Pattern: String);
  var
    Search: TSynEditSearch;
    SearchStart, SearchEnd, FoundStart, FoundEnd: TPoint;
  begin
    Search := TSynEditSearch.Create();

    try
      Search.Sensitive := CaseSensitive;
      Search.Whole := WholeWorld;
      Search.Pattern := Pattern;
      Search.IdentChars := CurrentEditor.IdentChars;

      with CurrentEditor, TextView do
      begin
        if SelAvail then
        begin
          SearchStart := BlockBegin;
          SearchEnd := BlockEnd;
        end else
        begin
          SearchStart := Point(1, 1);
          SearchEnd := CaretMax;
        end;

        while Search.FindNextOne(TextView, SearchStart, SearchEnd, FoundStart, FoundEnd, True) do
        begin
          ModifiedLinesGutter.AddLineMark(FoundStart.Y + (FoundEnd.Y - FoundStart.Y), RGBToColor(206,0,206));

          SearchStart := FoundEnd;
        end;
      end;
    finally
      Search.Free();
    end;

    CurrentEditor.ModifiedLinesGutter.DrawLineMarks := True;

    ClearLineMarksTimer.Enabled := False;
    ClearLineMarksTimer.Enabled := True;
  end;

var
  Options: TSynSearchOptions;
  Editor: TSimbaEditor;
begin
  Editor := CurrentEditor;

  if (Editor <> nil) then
  begin
    Options := [];
    if (frMatchCase in FindDialog.Options) then
      Options := Options + [ssoMatchCase];
    if (frWholeWord in FindDialog.Options) then
      Options := Options + [ssoWholeWord];

    AddLineMarks(frMatchCase in FindDialog.Options, frWholeWord in FindDialog.Options, FindDialog.FindText);

    if (FOnEditorSearch <> nil) then
      FOnEditorSearch(Editor);

    if Editor.SelAvail then
      Editor.SearchReplace(FindDialog.FindText, '', Options + [ssoSelectedOnly])
    else
    if Editor.SearchReplace(FindDialog.FindText, '', Options + [ssoEntireScope]) = 0 then
      Editor.SearchReplaceEx(FindDialog.FindText, '', Options + [ssoEntireScope], Point(1, 1));
  end;

  FindDialog.CloseDialog();
  if Editor.CanSetFocus() then
    Editor.SetFocus();
end;

procedure TSimbaScriptTabsForm.DoEditorPopupClick(Sender: TObject);
begin
  if (CurrentEditor = nil) then
    Exit;

  if (Sender = MenuItemFindDeclaration) then CurrentEditor.OnClickLink(Self, mbLeft, [], CurrentEditor.CaretX, CurrentEditor.CaretY);
  if (Sender = MenuItemUndo)            then CurrentEditor.Undo();
  if (Sender = MenuItemRedo)            then CurrentEditor.Redo();
  if (Sender = MenuItemCut)             then CurrentEditor.CutToClipboard();
  if (Sender = MenuItemCopy)            then CurrentEditor.CopyToClipboard();
  if (Sender = MenuItemPaste)           then CurrentEditor.PasteFromClipboard();
  if (Sender = MenuItemDelete)          then CurrentEditor.ClearSelection();
  if (Sender = MenuItemSelectAll)       then CurrentEditor.SelectAll();
  if (Sender = MenuItemFind)            then Self.Find();
  if (Sender = MenuItemReplace)         then Self.Replace();
end;

procedure TSimbaScriptTabsForm.DoOnDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  for I := 0 to High(FileNames) do
    Self.Open(FileNames[I], True);
end;

procedure TSimbaScriptTabsForm.DoOnReplaceDialog(Sender: TObject);
var
  Options: TSynSearchOptions;
  Editor: TSimbaEditor;
begin
  Editor := CurrentEditor;

  if (Editor <> nil) then
  begin
    Options := [ssoReplaceAll];

    if (frMatchCase in ReplaceDialog.Options) then
      Options := Options + [ssoMatchCase];
    if (frWholeWord in ReplaceDialog.Options) then
      Options := Options + [ssoWholeWord];
    if (frPromptOnReplace in ReplaceDialog.Options) then
      Options := Options + [ssoPrompt];

    Editor.BeginUndoBlock();
    Editor.OnReplaceText := @DoOnReplaceText;

    try
      if Editor.SearchReplace(ReplaceDialog.FindText, ReplaceDialog.ReplaceText, Options) = 0 then
        Editor.SearchReplaceEx(ReplaceDialog.FindText, ReplaceDialog.ReplaceText, Options, Point(1, 1));
    finally
      Editor.OnReplaceText := nil;
    end;

    Editor.EndUndoBlock();
  end;

  ReplaceDialog.CloseDialog();
  if Editor.CanSetFocus() then
    Editor.SetFocus();
end;

procedure TSimbaScriptTabsForm.DoOnTabChange(Sender: TObject);
begin
  if (FOnEditorChanged <> nil) then
    FOnEditorChanged(CurrentTab);
end;

procedure TSimbaScriptTabsForm.DoTabPopupClick(Sender: TObject);
var
  Tab: Integer;
  Abort: Boolean;
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

procedure TSimbaScriptTabsForm.HandleClearMarksTimer(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TabCount - 1 do
    Tabs[I].Editor.ModifiedLinesGutter.DrawLineMarks := False;

  ClearLineMarksTimer.Enabled := False;
end;

type
  TReplaceDialogHack = class(TReplaceDialog);

procedure TSimbaScriptTabsForm.ReplaceDialogShow(Sender: TObject);
var
  I: Integer;
  Button: TButton;
  ReplaceButton, ReplaceAllButton, FindMoreButton: TButton;
begin
  ReplaceButton    := nil;
  ReplaceAllButton := nil;
  FindMoreButton   := nil;

  with TReplaceDialogHack(Sender).FFindForm do
    for I := 0 to ComponentCount - 1 do
    begin
      if (Components[I] is TButton) then
      begin
        Button := TButton(Components[I]);

        if (Button.Caption = rsReplace)    then ReplaceButton := Button;
        if (Button.Caption = rsReplaceAll) then ReplaceAllButton := Button;
        if (Button.Caption = rsFindMore)   then FindMoreButton := Button;
      end;
    end;

  if (FindMoreButton <> nil) then
    FindMoreButton.Hide();

  if (ReplaceButton <> nil) and (ReplaceAllButton <> nil) then
  begin
    ReplaceButton.Anchors := ReplaceButton.Anchors + [akLeft];
    ReplaceButton.AnchorSide[akLeft].Control := ReplaceAllButton;
    ReplaceButton.AnchorSide[akLeft].Side := asrLeft;

    ReplaceButton.BorderSpacing.Top := 0;
    ReplaceButton.BorderSpacing.Bottom := 0;
    ReplaceAllButton.BorderSpacing.Top := 0;
    ReplaceAllButton.BorderSpacing.Bottom := 0;
  end;
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
  Result := nil;
  if (Notebook.TabIndex > -1) then
    Result := Notebook.Pages[Notebook.TabIndex] as TSimbaScriptTab;
end;

function TSimbaScriptTabsForm.GetCurrentEditor: TSimbaEditor;
begin
  Result := nil;
  if (CurrentTab <> nil) then
    Result := CurrentTab.Editor;
end;

procedure TSimbaScriptTabsForm.Replace;
begin
  if (CurrentEditor <> nil) then
  begin
    if CurrentEditor.SelAvail then
      FindDialog.Options := FindDialog.Options - [frEntireScope]
    else
      FindDialog.Options := FindDialog.Options + [frEntireScope];

    ReplaceDialog.FindText := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);
    ReplaceDialog.Execute();
  end;
end;

procedure TSimbaScriptTabsForm.Find;
begin
  if (CurrentEditor <> nil) then
  begin
    FindDialog.FindText := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);
    FindDialog.Execute();
  end;
end;

procedure TSimbaScriptTabsForm.FindNext;
var
  Options: TSynSearchOptions;
  Editor: TSimbaEditor;
begin
  Editor := CurrentEditor;

  if (Editor <> nil) then
  begin
    Options := [ssoFindContinue];
    if (frMatchCase in FindDialog.Options) then
      Options := Options + [ssoMatchCase];
    if (frWholeWord in FindDialog.Options) then
      Options := Options + [ssoWholeWord];

    if Editor.SearchReplace(FindDialog.FindText, '', Options) = 0 then
      Editor.SearchReplaceEx(FindDialog.FindText, '', Options, Point(1, 1));

    Editor.ModifiedLinesGutter.DrawLineMarks := True;

    ClearLineMarksTimer.Enabled := False;
    ClearLineMarksTimer.Enabled := True;
  end;
end;

procedure TSimbaScriptTabsForm.FindPrevious;
var
  Options: TSynSearchOptions;
  Editor: TSimbaEditor;
begin
  Editor := CurrentEditor;

  if (Editor <> nil) then
  begin
    Options := [ssoFindContinue, ssoBackwards];

    if (frMatchCase in FindDialog.Options) then
      Options := Options + [ssoMatchCase];
    if (frWholeWord in FindDialog.Options) then
      Options := Options + [ssoWholeWord];

    if Editor.SearchReplace(FindDialog.FindText, '', Options) = 0 then
      Editor.SearchReplaceEx(FindDialog.FindText, '', Options, Editor.CaretMax);

    Editor.ModifiedLinesGutter.DrawLineMarks := True;

    ClearLineMarksTimer.Enabled := False;
    ClearLineMarksTimer.Enabled := True;
  end;
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

procedure TSimbaScriptTabsForm.OpenDeclaration(Header: String; StartPos, EndPos, Line: Integer; FileName: String);
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

  SimbaDebugForm.Add('Declared internally in Simba: ' + FileName);
  SimbaDebugForm.Add('Declaration:');
  SimbaDebugForm.Add(Header);
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
      SimbaDebugForm.Add('Declared internally in plugin: ' + FileName)
    else
      SimbaDebugForm.Add('Declared internally in Simba: ' + FileName);

    SimbaDebugForm.Add('Declaration:');

    if Declaration is TciProcedureDeclaration then
      SimbaDebugForm.Add(TciProcedureDeclaration(Declaration).Header)
    else
      SimbaDebugForm.Add(Declaration.RawText);
  end;
end;

end.

