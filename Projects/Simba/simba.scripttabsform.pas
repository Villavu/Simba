unit simba.scripttabsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtendedNotebook, fptimer, simba.scripttab, StdCtrls, ExtCtrls, simba.scripttabhistory,
  Buttons, Menus, ComCtrls, SynEditTypes,
  simba.editor, simba.codeparser;

type
  TSimbaScriptTabsForm = class(TForm)
    FindPanel_UpButton: TButton;
    FindPanel_DownButton: TButton;
    FindCaseSenstiveButton: TCheckBox;
    FindEdit: TEdit;
    FindPanel: TPanel;
    FindPanel_Label: TLabel;
    EditorPopupMenu: TPopupMenu;
    EditorPopupUndo: TMenuItem;
    EditorPopupCopy: TMenuItem;
    EditorPopupPaste: TMenuItem;
    EditorPopupCut: TMenuItem;
    EditorPopupRedo: TMenuItem;
    EditorPopupFindDeclaration: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    EditorPopupFind: TMenuItem;
    EditorPopupReplace: TMenuItem;
    EditorPopupSelectAll: TMenuItem;
    MenuItem3: TMenuItem;
    EditorPopupDelete: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog: TOpenDialog;
    StatusPanelState: TPanel;
    StatusPanelFileName: TPanel;
    StatusPanelCaret: TPanel;
    StatusBar: TPanel;
    TabPopupItem_NewTab: TMenuItem;
    TabPopupItem_CloseTab: TMenuItem;
    TabPopupItem_CloseOtherTabs: TMenuItem;
    TabPopupMenu: TPopupMenu;
    ReplaceDialog: TReplaceDialog;
    Notebook: TExtendedNotebook;
    FindPanel_CloseButton: TSpeedButton;


    procedure EditorPopupClick(Sender: TObject);
    procedure EditorPopupShow(Sender: TObject);
    procedure FindPanel_DownButtonClick(Sender: TObject);
    procedure FindEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FindPanel_SearchChanged(Sender: TObject);
    procedure FindPanel_UpButtonClick(Sender: TObject);
    procedure HandleDropFiles(Sender: TObject; const FileNames: array of String);
    procedure HandleReplace(Sender: TObject);
    procedure NotebookChange(Sender: TObject);
    procedure NotebookChanging(Sender: TObject; var AllowChange: Boolean);
    procedure Search(SearchOptions: TSynSearchOptions; HighlightAll: Boolean);
    procedure FindPanel_CloseButtonClick(Sender: TObject);
    procedure HandleTabPopupClick(Sender: TObject);
    procedure StatusBarPaint(Sender: TObject);
    procedure StatusPanelPaint(Sender: TObject);
    procedure TabPopupMenuPopup(Sender: TObject);
  protected
    procedure CaretChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject); override;

    procedure SetVisible(Value: boolean); override;
    procedure SetCurrentTab(Value: TSimbaScriptTab);
    procedure SetFindVisible(Value: Boolean);

    function GetTabCount: Int32;
    function GetTab(Index: Int32): TSimbaScriptTab;
    function GetPopupTab: TSimbaScriptTab;
    function GetCurrentTab: TSimbaScriptTab;
    function GetCurrentEditor: TSimbaEditor;
    function GetFindVisible: Boolean;
  public
    property TabCount: Int32 read GetTabCount;
    property Tabs[Index: Int32]: TSimbaScriptTab read GetTab;

    property PopupTab: TSimbaScriptTab read GetPopupTab;

    property CurrentTab: TSimbaScriptTab read GetCurrentTab write SetCurrentTab;
    property CurrentEditor: TSimbaEditor read GetCurrentEditor;

    property FindVisible: Boolean read GetFindVisible write SetFindVisible;

    procedure FindNext;
    procedure FindPrevious;

    procedure Replace;

    function FindTab(FileName: String): TSimbaScriptTab; overload;

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
  syneditpointclasses, lcltype,
  simba.settings, simba.main, simba.codeinsight, simba.debugform, AnchorDocking,
  simba.functionlistform;

procedure TSimbaScriptTabsForm.Search(SearchOptions: TSynSearchOptions; HighlightAll: Boolean);
var
  SearchStart: TPoint;
  Matches: Int32;
begin
  if (CurrentEditor <> nil) and (FindVisible) then
  begin
    if FindEdit.Text <> '' then
    begin
      SearchStart := CurrentEditor.LogicalCaretXY;

      if FindCaseSenstiveButton.Checked then
        SearchOptions := SearchOptions + [ssoMatchCase];

      if (not (ssoFindContinue in SearchOptions)) and CurrentEditor.SelAvail then
        if (ssoBackwards in SearchOptions) then
          CurrentEditor.LogicalCaretXY := CurrentEditor.BlockEnd
        else
          CurrentEditor.LogicalCaretXY := CurrentEditor.BlockBegin;

      Matches := CurrentEditor.SearchReplace(FindEdit.Text, '', SearchOptions);
      if (Matches = 0) then
        Matches := CurrentEditor.SearchReplace(FindEdit.Text, '', SearchOptions + [ssoEntireScope]);
    end;

    if (FindEdit.Text <> '') and (Matches = 0) then
    begin
      FindEdit.Color := clMaroon;
      FindEdit.Font.Color := clWhite;

      CurrentEditor.LogicalCaretXY := SearchStart;
    end else
    begin
      FindEdit.Color := clWindow;
      FindEdit.Font.Color := clWindowText;
    end;
  end;
end;

procedure TSimbaScriptTabsForm.FindPanel_SearchChanged(Sender: TObject);
begin
  Search([], True);
end;

procedure TSimbaScriptTabsForm.FindPanel_DownButtonClick(Sender: TObject);
begin
  Search([ssoFindContinue, ssoBackwards], False);
end;

procedure TSimbaScriptTabsForm.FindEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
    FindVisible := False;
end;

procedure TSimbaScriptTabsForm.EditorPopupShow(Sender: TObject);
var
  CurrentWord: String;
begin
  if (CurrentEditor = nil) then
    EditorPopupMenu.Close()
  else
  begin
    CurrentWord := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);

    if CurrentWord <> '' then
    begin
      EditorPopupFindDeclaration.Caption := 'Find Declaration of ' + CurrentWord;
      EditorPopupFindDeclaration.Enabled := True;
    end else
    begin
      EditorPopupFindDeclaration.Caption := 'Find Declaration';
      EditorPopupFindDeclaration.Enabled := False;
    end;

    EditorPopupUndo.Enabled   := CurrentEditor.CanUndo;
    EditorPopupRedo.Enabled   := CurrentEditor.CanRedo;
    EditorPopupPaste.Enabled  := CurrentEditor.CanPaste;
    EditorPopupCut.Enabled    := CurrentEditor.SelAvail;
    EditorPopupCopy.Enabled   := CurrentEditor.SelAvail;
    EditorPopupDelete.Enabled := CurrentEditor.SelAvail;
  end;
end;

procedure TSimbaScriptTabsForm.EditorPopupClick(Sender: TObject);
begin
  if (CurrentEditor = nil) then
    Exit;

  if (Sender = EditorPopupFindDeclaration) then CurrentEditor.FindDeclaration(CurrentEditor.CaretXY);
  if (Sender = EditorPopupUndo)            then CurrentEditor.Undo();
  if (Sender = EditorPopupRedo)            then CurrentEditor.Redo();
  if (Sender = EditorPopupCut)             then CurrentEditor.CutToClipboard();
  if (Sender = EditorPopupCopy)            then CurrentEditor.CopyToClipboard();
  if (Sender = EditorPopupPaste)           then CurrentEditor.PasteFromClipboard();
  if (Sender = EditorPopupDelete)          then CurrentEditor.ClearSelection();
  if (Sender = EditorPopupSelectAll)       then CurrentEditor.SelectAll();
  if (Sender = EditorPopupFind)            then Self.FindVisible := True;
  if (Sender = EditorPopupReplace)         then Self.Replace();
end;

procedure TSimbaScriptTabsForm.FindPanel_UpButtonClick(Sender: TObject);
begin
  Search([ssoFindContinue], False);
end;

procedure TSimbaScriptTabsForm.HandleDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Int32;
begin
  for i := 0 to High(FileNames) do
    Self.Open(FileNames[i], True);
end;

procedure TSimbaScriptTabsForm.HandleReplace(Sender: TObject);
var
  Options: TSynSearchOptions;
  Start: TPoint;
  Prompt: Boolean;

  procedure Replacement;
  begin
    CurrentEditor.SearchReplaceEx(ReplaceDialog.FindText, ReplaceDialog.ReplaceText, Options + [ssoReplace], Start);
  end;

begin
  if CurrentEditor = nil then
    Exit;

  CurrentEditor.BeginUndoBlock();

  try
    Prompt := frPromptOnReplace in ReplaceDialog.Options;
    Options := [];

    if (frMatchCase in ReplaceDialog.Options) then
      Options := Options + [ssoMatchCase];
    if (frWholeWord in ReplaceDialog.Options) then
      Options := Options + [ssoWholeWord];
    if CurrentEditor.SelAvail then
      Options := Options + [ssoSelectedOnly];

    if (frEntireScope in ReplaceDialog.Options) then
    begin
      Options := Options - [ssoSelectedOnly];

      Start := Point(0, 0)
    end else
      Start := CurrentEditor.CaretXY;

    while CurrentEditor.SearchReplaceEx(ReplaceDialog.FindText, '', Options, Start) > 0 do
    begin
      if Prompt then
      begin
        case MessageDlg('Replace', Format('Replace "%s" with "%s"?', [ReplaceDialog.FindText, ReplaceDialog.ReplaceText]), mtConfirmation, [mbYes, mbNo, mbYesToAll], 0) of
          mrYes:
            begin
              Replacement();
            end;

          mrYesToAll:
            begin
              Replacement();

              Prompt := False;
            end;

          mrCancel:
            Break;
        end;
      end else
        Replacement();

      Start := CurrentEditor.CaretXY;
    end;
  finally
    CurrentEditor.EndUndoBlock();
  end;
end;

procedure TSimbaScriptTabsForm.NotebookChange(Sender: TObject);
begin
  if CurrentTab.FileName <> '' then
    StatusPanelFileName.Caption := CurrentTab.FileName
  else
    StatusPanelFileName.Caption := CurrentTab.ScriptName;

  StatusPanelCaret.Caption := IntToStr(CurrentEditor.CaretY) + ': ' + IntToStr(CurrentEditor.CaretX);

  SimbaForm.MenuItemSaveAll.Enabled := TabCount > 1;
  SimbaForm.SaveAllButton.Enabled := TabCount > 1;

  SimbaFunctionListForm.Fill(CurrentTab.Editor.Text, CurrentTab.Editor.FileName, CurrentTab.Editor.SelStart - 1);
  SimbaFunctionListForm.State := CurrentTab.FunctionListState;

  if CurrentEditor.CanSetFocus() then
    CurrentEditor.SetFocus();
end;

procedure TSimbaScriptTabsForm.NotebookChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if CurrentTab.TabIndex > -1 then
    CurrentTab.FunctionListState := SimbaFunctionListForm.State;

  AllowChange := True;
end;

procedure TSimbaScriptTabsForm.HandleTabPopupClick(Sender: TObject);
var
  Abort: Boolean;
begin
  if (Sender = TabPopupItem_NewTab) then
    AddTab()
  else
  begin
    if PopupTab = nil then
      Exit;

    if (Sender = TabPopupItem_CloseTab) then
      RemoveTab(PopupTab, Abort)
    else
    if (Sender = TabPopupItem_CloseOtherTabs) then
      RemoveOtherTabs(PopupTab);
  end;
end;

procedure TSimbaScriptTabsForm.StatusBarPaint(Sender: TObject);
begin
  with TPanel(Sender) do
  begin
    Canvas.Pen.Color := RGBToColor(217, 217, 217);
    Canvas.Line(0, ClientHeight-1, ClientWidth, ClientHeight-1);
  end;
end;

procedure TSimbaScriptTabsForm.StatusPanelPaint(Sender: TObject);
begin
  with TPanel(Sender) do
  begin
    Canvas.Pen.Color := RGBToColor(217, 217, 217);
    Canvas.Line(ClientWidth-1, 0, ClientWidth-1, ClientHeight-1);
  end;
end;

procedure TSimbaScriptTabsForm.TabPopupMenuPopup(Sender: TObject);
begin
  TabPopupItem_CloseTab.Enabled := (PopupTab <> nil);
  TabPopupItem_CloseOtherTabs.Enabled := (PopupTab <> nil) and (TabCount > 1);
end;

procedure TSimbaScriptTabsForm.CaretChanged(Sender: TObject);
begin
  with TSynEditCaret(Sender) do
    StatusPanelCaret.Caption := IntToStr(LinePos) + ': ' + IntToStr(CharPos);
end;

procedure TSimbaScriptTabsForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (not (csLoading in ComponentState)) then
  begin
    StatusBar.Height := Round(Canvas.TextHeight('Fj') * 1.33);
    if StatusPanelCaret <> nil then
      StatusPanelCaret.Width := Round(Canvas.TextWidth('1000:1000') * 1.5);
    if StatusPanelState <> nil then
      StatusPanelState.Width := Round(Canvas.TextWidth('000:000:000') * 1.5);
  end;
end;

procedure TSimbaScriptTabsForm.SetVisible(Value: boolean);
begin
  inherited SetVisible(Value);

  if DockMaster.GetAnchorSite(Self) <> nil then
    DockMaster.GetAnchorSite(Self).Visible := Value;
end;

procedure TSimbaScriptTabsForm.SetFindVisible(Value: Boolean);
begin
  if Value then
  begin
    if (CurrentEditor <> nil) and CurrentEditor.Focused and (CurrentEditor.SelText = '')  then
      FindEdit.Text := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);

    FindPanel.Visible := True;
    if FindEdit.CanSetFocus() then
      FindEdit.SetFocus();
  end else
  begin
    FindPanel.Visible := False;
    if (CurrentEditor <> nil) and CurrentEditor.CanSetFocus then
      CurrentEditor.SetFocus();
  end;
end;

function TSimbaScriptTabsForm.GetFindVisible: Boolean;
begin
  Result := FindPanel.Visible;
end;

function TSimbaScriptTabsForm.GetPopupTab: TSimbaScriptTab;
begin
  Result := nil;

  with Notebook do
  begin
    if (IndexOfPageAt(ScreenToClient(PopupMenu.PopupPoint)) > -1) then
      Result := Pages[IndexOfPageAt(ScreenToClient(PopupMenu.PopupPoint))] as TSimbaScriptTab;
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

procedure TSimbaScriptTabsForm.FindPanel_CloseButtonClick(Sender: TObject);
begin
  FindVisible := False;
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

procedure TSimbaScriptTabsForm.FindNext;
begin
  Search([ssoFindContinue, ssoBackwards], False);
end;

procedure TSimbaScriptTabsForm.FindPrevious;
begin
  Search([ssoFindContinue], False);
end;

procedure TSimbaScriptTabsForm.Replace;
begin
  if (CurrentEditor <> nil) then
  begin
    ReplaceDialog.FindText := CurrentEditor.GetWordAtRowCol(CurrentEditor.CaretXY);
    if CurrentEditor.SelAvail then
      ReplaceDialog.Options := ReplaceDialog.Options - [frEntireScope]
    else
      ReplaceDialog.Options := ReplaceDialog.Options + [frEntireScope];

    ReplaceDialog.Execute();
  end;
end;

function TSimbaScriptTabsForm.FindTab(FileName: String): TSimbaScriptTab;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to Notebook.PageCount - 1 do
    if TSimbaScriptTab(Notebook.Pages[i]).FileName = FileName then
    begin
      Result := Notebook.Pages[i] as TSimbaScriptTab;

      Break;
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
      if ScriptTab.ScriptInstance.IsRunning then
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
    ScriptTab.Reset()
  else
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
begin
  if FileExists(FileName) then
    FileName := ExpandFileName(FileName);

  if CheckOtherTabs and (FindTab(FileName) <> nil) then
    CurrentTab := FindTab(FileName)
  else
  if FileExists(FileName) then
  begin
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

  addTab();
end;

initialization
  {$I simba.scripttabsform.lrs}

end.

