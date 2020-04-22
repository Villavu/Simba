unit simba.scripttabsform;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, lresources, forms, controls, graphics, dialogs,
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
    StatusPanelState: TPanel;
    StatusPanelFileName: TPanel;
    StatusPanelCaret: TPanel;
    StatusBar: TPanel;
    MenuItemNewTab: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseOtherTabs: TMenuItem;
    TabPopupMenu: TPopupMenu;
    ReplaceDialog: TReplaceDialog;
    Notebook: TExtendedNotebook;
    EditorPopupMenu: TPopupMenu;

    procedure DoOnFindDialog(Sender: TObject);
    procedure DoOnReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure DoOnReplaceDialog(Sender: TObject);
    procedure DoEditorPopupClick(Sender: TObject);
    procedure DoEditorPopupShow(Sender: TObject);
    procedure DoOnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DoOnTabChange(Sender: TObject);
    procedure DoOnTabChanging(Sender: TObject; var AllowChange: Boolean);
    procedure DoTabPopupClick(Sender: TObject);
    procedure DoStatusBarPaint(Sender: TObject);
    procedure DoStatusPanelPaint(Sender: TObject);
    procedure DoTabPopupOpen(Sender: TObject);
  protected
    procedure CaretChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;

    function GetTabCount: Int32;
    function GetTab(Index: Int32): TSimbaScriptTab;
    function GetCurrentTab: TSimbaScriptTab;
    function GetCurrentEditor: TSimbaEditor;

    procedure SetCurrentTab(Value: TSimbaScriptTab);
  public
    property TabCount: Int32 read GetTabCount;
    property Tabs[Index: Int32]: TSimbaScriptTab read GetTab;

    property CurrentTab: TSimbaScriptTab read GetCurrentTab write SetCurrentTab;
    property CurrentEditor: TSimbaEditor read GetCurrentEditor;

    procedure Replace;
    procedure Find;
    procedure FindNext;
    procedure FindPrevious;

    function AddTab: TSimbaScriptTab;

    procedure RemoveTab(ScriptTab: TSimbaScriptTab; out Abort: Boolean); overload;
    procedure RemoveAllTabs(out Aborted: Boolean);
    procedure RemoveOtherTabs(Exclude: TSimbaScriptTab);

    procedure Open(FileName: String; CheckOtherTabs: Boolean = True); overload;
    procedure Open; overload;

    procedure OpenDeclaration(Declaration: TDeclaration);

    constructor Create(TheOwner: TComponent); override;
  end;

var
  SimbaScriptTabsForm: TSimbaScriptTabsForm;

implementation

uses
  syneditpointclasses,
  simba.settings, simba.main, simba.debugform, simba.functionlistform, simba.scripttabhistory;

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
  case MessageDlg('Replace', Format('Replace "%s" with "%s"?', [ReplaceDialog.FindText, ReplaceDialog.ReplaceText]), mtConfirmation, [mbYes, mbYesToAll, mbNo, mbCancel], 0) of
    mrYes:      ReplaceAction := raReplace;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrNo:       ReplaceAction := raSkip;
    mrCancel:   ReplaceAction := raCancel;
  end;
end;

procedure TSimbaScriptTabsForm.DoOnFindDialog(Sender: TObject);
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

    if (frEntireScope in FindDialog.Options) then
    begin
      if Editor.SearchReplace(FindDialog.FindText, '', Options - [ssoSelectedOnly]) = 0 then
        Editor.SearchReplaceEx(FindDialog.FindText, '', Options - [ssoSelectedOnly], TPoint.Zero);
    end else
      Editor.SearchReplace(FindDialog.FindText, '', Options + [ssoSelectedOnly]);
  end;

  FindDialog.CloseDialog();
  if Editor.CanSetFocus() then
    Editor.SetFocus();
end;

procedure TSimbaScriptTabsForm.DoEditorPopupClick(Sender: TObject);
begin
  if (CurrentEditor = nil) then
    Exit;

  if (Sender = MenuItemFindDeclaration) then CurrentEditor.FindDeclaration(CurrentEditor.CaretXY);
  if (Sender = MenuItemUndo)            then CurrentEditor.Undo();
  if (Sender = MenuItemRedo)         then CurrentEditor.Redo();
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
  i: Int32;
begin
  for i := 0 to High(FileNames) do
    Self.Open(FileNames[i], True);
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
      if (frEntireScope in FindDialog.Options) then
        Editor.SearchReplaceEx(ReplaceDialog.FindText, ReplaceDialog.ReplaceText, Options - [ssoSelectedOnly], TPoint.Zero)
      else
        Editor.SearchReplace(ReplaceDialog.FindText, ReplaceDialog.ReplaceText, Options + [ssoSelectedOnly]);
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
  if CurrentTab.FileName <> '' then
    StatusPanelFileName.Caption := CurrentTab.FileName
  else
    StatusPanelFileName.Caption := CurrentTab.ScriptName;

  StatusPanelCaret.Caption := IntToStr(CurrentEditor.CaretY) + ': ' + IntToStr(CurrentEditor.CaretX);

  SimbaForm.MenuItemSaveAll.Enabled := TabCount > 1;
  SimbaForm.SaveAllButton.Enabled := TabCount > 1;

  SimbaFunctionListForm.State := CurrentTab.FunctionListState;

  if CurrentEditor.CanSetFocus() then
    CurrentEditor.SetFocus();
end;

procedure TSimbaScriptTabsForm.DoOnTabChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if CurrentTab.TabIndex > -1 then
    CurrentTab.FunctionListState := SimbaFunctionListForm.State;

  AllowChange := True;
end;

procedure TSimbaScriptTabsForm.DoTabPopupClick(Sender: TObject);
var
  Tab: Int32;
  Abort: Boolean;
begin
  if (Sender = MenuItemNewTab) then
    AddTab()
  else
  begin
    Tab := Notebook.IndexOfPageAt(NoteBook.ScreenToClient(TabPopupMenu.PopupPoint));

    if (Tab >= 0) and (Tab < TabCount) then
    begin
      if (Sender = MenuItemCloseTab)       then RemoveTab(Tabs[Tab], Abort);
      if (Sender = MenuItemCloseOtherTabs) then RemoveOtherTabs(Tabs[Tab]);
    end;
  end;
end;

procedure TSimbaScriptTabsForm.DoStatusBarPaint(Sender: TObject);
begin
  with TPanel(Sender) do
  begin
    Canvas.Pen.Color := RGBToColor(217, 217, 217);
    Canvas.Line(0, ClientHeight-1, ClientWidth, ClientHeight-1);
  end;
end;

procedure TSimbaScriptTabsForm.DoStatusPanelPaint(Sender: TObject);
begin
  with TPanel(Sender) do
  begin
    Canvas.Pen.Color := RGBToColor(217, 217, 217);
    Canvas.Line(ClientWidth-1, 0, ClientWidth-1, ClientHeight-1);
  end;
end;

procedure TSimbaScriptTabsForm.DoTabPopupOpen(Sender: TObject);
begin
  MenuItemCloseTab.Enabled := Notebook.IndexOfPageAt(NoteBook.ScreenToClient(TabPopupMenu.PopupPoint)) >= 0;
  MenuItemCloseOtherTabs.Enabled := Notebook.IndexOfPageAt(NoteBook.ScreenToClient(TabPopupMenu.PopupPoint)) >= 0;
end;

procedure TSimbaScriptTabsForm.CaretChanged(Sender: TObject);
begin
  with TSynEditCaret(Sender) do
    StatusPanelCaret.Caption := IntToStr(LinePos) + ': ' + IntToStr(CharPos);
end;

procedure TSimbaScriptTabsForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (StatusBar <> nil) and (StatusPanelCaret <> nil) and (StatusPanelState <> nil) then
    with TBitmap.Create() do
    try
      Canvas.Font := Self.Font;

      with Canvas.TextExtent('0') do
      begin
        StatusBar.Height := Round(CY * 1.35);
        StatusPanelCaret.Width := CX * 10;
        StatusPanelState.Width := CX * 15;
      end;
    finally
      Free();
    end;
end;

procedure TSimbaScriptTabsForm.SetCurrentTab(Value: TSimbaScriptTab);
begin
  Notebook.ActivePage := Value;
end;

function TSimbaScriptTabsForm.GetTabCount: Int32;
begin
  Result := Notebook.PageCount;
end;

function TSimbaScriptTabsForm.GetTab(Index: Int32): TSimbaScriptTab;
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
    if CurrentEditor.SelAvail then
      FindDialog.Options := FindDialog.Options - [frEntireScope]
    else
      FindDialog.Options := FindDialog.Options + [frEntireScope];

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

    if (frEntireScope in FindDialog.Options) then
    begin
      if Editor.SearchReplace(FindDialog.FindText, '', Options - [ssoSelectedOnly]) = 0 then
        Editor.SearchReplaceEx(FindDialog.FindText, '', Options - [ssoSelectedOnly], TPoint.Zero);
    end else
      Editor.SearchReplace(FindDialog.FindText, '', Options + [ssoSelectedOnly]);
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

    if (frEntireScope in FindDialog.Options) then
    begin
      if Editor.SearchReplace(FindDialog.FindText, '', Options - [ssoSelectedOnly]) = 0 then
        Editor.SearchReplaceEx(FindDialog.FindText, '', Options - [ssoSelectedOnly], TPoint.Zero);
    end else
      Editor.SearchReplace(FindDialog.FindText, '', Options + [ssoSelectedOnly]);
  end;
end;

function TSimbaScriptTabsForm.AddTab: TSimbaScriptTab;
begin
  Result := TSimbaScriptTab.Create(Notebook);
  Result.Parent := Notebook;
  Result.Editor.PopupMenu := EditorPopupMenu;
  Result.Editor.AddHandlerOnCaretChange(@CaretChanged);

  NoteBook.TabIndex := Result.TabIndex;
end;

procedure TSimbaScriptTabsForm.RemoveAllTabs(out Aborted: Boolean);
var
  i: Int32;
begin
  for i := TabCount - 1 downto 0 do
  begin
    RemoveTab(Tabs[i], Aborted);
    if Aborted then
      Exit;
  end;
end;

procedure TSimbaScriptTabsForm.RemoveTab(ScriptTab: TSimbaScriptTab; out Abort: Boolean);
begin
  Abort := False;

  if (ScriptTab.ScriptInstance <> nil) then
  begin
    ScriptTab.MakeVisible();

    Application.MainForm.Enabled := False;

    try
      if ScriptTab.ScriptInstance.Running then
      begin
        case MessageDlg('Script is still running', 'Do you want to forcefully stop the script?', mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
          mrYes: ScriptTab.ScriptInstance.Kill();
          mrNo: Exit;
          mrAbort:
            begin
              Abort := True;

              Exit;
            end;
        end;
      end;
    finally
      Application.MainForm.Enabled := True;
    end;
  end;

  if ScriptTab.ScriptChanged then
  begin
    ScriptTab.MakeVisible();

    case MessageDlg('Script has been modified.', 'Do you want to save the script?', mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
      mrYes: ScriptTab.Save(ScriptTab.FileName);
      mrNo: { nothing };
      mrAbort:
        begin
          Abort := True;

          Exit;
        end;
    end;
  end;

  if (TabCount = 1) then
  begin
    ScriptTab.Reset();
    if (Notebook.OnChange <> nil) then
      Notebook.OnChange(Self);
  end else
    ScriptTab.Free();
end;

procedure TSimbaScriptTabsForm.RemoveOtherTabs(Exclude: TSimbaScriptTab);
var
  Abort: Boolean;
var
  i: Int32;
begin
  for i := TabCount - 1 downto 0 do
    if (Tabs[i] <> Exclude) then
    begin
      RemoveTab(Tabs[i], Abort);
      if Abort then
        Exit;
    end;

  Exclude.MakeVisible();
end;

procedure TSimbaScriptTabsForm.Open(FileName: String; CheckOtherTabs: Boolean);
var
  I: Int32;
begin
  if FileExists(FileName) then
    FileName := ExpandFileName(FileName);

  if CheckOtherTabs then
  begin
    for I := 0 to TabCount - 1 do
      if Tabs[I].FileName = FileName then
      begin
        CurrentTab := Tabs[i];

        Exit;
      end;
  end;

  if FileExists(FileName) then
  begin
    if (CurrentTab.FileName <> '') or CurrentTab.ScriptChanged then // Use current tab if default
      CurrentTab := AddTab();

    CurrentTab.Load(FileName);

    SimbaForm.AddRecentFile(FileName);
    Notebook.OnChange(Notebook);
  end;
end;

procedure TSimbaScriptTabsForm.Open;
var
  I: Int32;
begin
  try
    OpenDialog.InitialDir := ExtractFileDir(CurrentTab.FileName);
    if OpenDialog.InitialDir = '' then
      OpenDialog.InitialDir := SimbaSettings.Environment.ScriptPath.Value;

    if OpenDialog.Execute() then
      for I := 0 to OpenDialog.Files.Count - 1 do
        Open(OpenDialog.Files[I], True);
  except
    on E: Exception do
      ShowMessage('Exception while opening file: ' + E.Message);
  end;
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

    ScriptTabHistory.Add(CurrentTab);
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

constructor TSimbaScriptTabsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  AddTab();
end;

initialization
  {$I simba.scripttabsform.lrs}

end.

