{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor_findreplace;

{$i simba.inc}

interface

uses
  classes, sysutils, dialogs, synedit, synedittypes, syneditsearch, SynEditMarkupHighAll;

type
  TSimbaEditorFind = class(TComponent)
  protected
    FDialog: TFindDialog;
    FEditor: TSynEdit;
    FMatches: Integer;

    procedure DoFind(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Execute(Editor: TSynEdit);
    procedure ExecuteNoDialog(Editor: TSynEdit; FindText: String; CaseSens, WholeWord: Boolean);

    procedure FindPrev(Editor: TSynEdit);
    procedure FindNext(Editor: TSynEdit);

    property Matches: Integer read FMatches;
  end;

  TSimbaEditorReplace = class(TComponent)
  protected
    FDialog: TReplaceDialog;
    FEditor: TSynEdit;

    procedure DoConfirmReplace(Sender: TObject; const ASearch, AReplace: String; Line, Column: Integer; var ReplaceAction: TSynReplaceAction);
    procedure DoReplace(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Execute(Editor: TSynEdit);
  end;

implementation

uses
  controls, forms, stdctrls, extctrls, graphics,
  simba.ide_editor, simba.ide_events;

type
  TSimbaEditorHelper = class helper for TSimbaEditor
  public
    function AddMarks(ForceEntireScope, CaseSensitive, WholeWorld: Boolean; Pattern: String): Integer;
    procedure ShowMarks;
    procedure HideMarks(Sender: TObject); overload;
    procedure HideMarks; overload;
  end;

procedure TSimbaEditorHelper.HideMarks(Sender: TObject);
begin
  with TSynEditMarkupHighlightAll(MarkupByClass[TSynEditMarkupHighlightAll]) do
    Enabled := False;

  ModifiedLinesGutter.HideMarks();

  if (Sender <> nil) then
    TTimer(Sender).Free();
end;

procedure TSimbaEditorHelper.HideMarks;
begin
  HideMarks(FindComponent('MarksChangedTimer'));
end;

procedure TSimbaEditorHelper.ShowMarks;
var
  Timer: TTimer;
begin
  ModifiedLinesGutter.ShowMarks();

  // In 5 seconds clear marks
  Timer := TTimer(FindComponent('MarksChangedTimer'));
  if (Timer = nil) then
  begin
    Timer := TTimer.Create(Self);
    Timer.Interval := 5000;
    Timer.Name := 'MarksChangedTimer';
    Timer.OnTimer := @HideMarks;
  end else
  begin
    // restart
    Timer.Enabled := False;
    Timer.Enabled := True;
  end;
end;

function TSimbaEditorHelper.AddMarks(ForceEntireScope, CaseSensitive, WholeWorld: Boolean; Pattern: String): Integer;
var
  Search: TSynEditSearch;
  SearchStart, SearchEnd, FoundStart, FoundEnd: TPoint;
begin
  Result := 0;

  with TSynEditMarkupHighlightAll(MarkupByClass[TSynEditMarkupHighlightAll]) do
  begin
    if CaseSensitive then
      SearchOptions := SearchOptions + [ssoMatchCase]
    else
      SearchOptions := SearchOptions - [ssoMatchCase];

    if WholeWorld then
      SearchOptions := SearchOptions + [ssoWholeWord]
    else
      SearchOptions := SearchOptions - [ssoWholeWord];

    SearchString := Pattern;
    Enabled := True;
  end;

  ModifiedLinesGutter.ClearMarks();

  Search := TSynEditSearch.Create();
  try
    Search.Sensitive := CaseSensitive;
    Search.Whole := WholeWorld;
    Search.Pattern := Pattern;
    Search.IdentChars := IdentChars;

    if SelAvail and (not ForceEntireScope) then
    begin
      SearchStart := BlockBegin;
      SearchEnd := BlockEnd;
    end else
    begin
      SearchStart.X := 1;
      SearchStart.Y := 1;

      SearchEnd.Y := TextView.Count;
      SearchEnd.X := Length(TextView[TextView.Count - 1]) + 1;
    end;

    while Search.FindNextOne(Lines, SearchStart, SearchEnd, FoundStart, FoundEnd, True) do
    begin
      ModifiedLinesGutter.AddMark(FoundStart.Y + (FoundEnd.Y - FoundStart.Y), clPurple);
      SearchStart := FoundEnd;
      Inc(Result);
    end;
  finally
    Search.Free();
  end;

  ShowMarks();
end;

type
  TCustomFindDialog = class(TFindDialog)
    function Execute: Boolean; override;
  end;

function TCustomFindDialog.Execute: Boolean;
begin
  Result := False;
  if not Assigned(FFindForm) then
    FFindForm := CreateForm();

  if Assigned(FFindForm) then
  begin
    CalcPosition(FFindForm);

    SetFormValues();

    FFindForm.OnClose := @DoCloseForm;
    FFindForm.OnShow  := @DoShowForm;
    FFindForm.Caption := Title;
    FFindForm.ShowOnTop();

    Height := FFindForm.Height;
    Width  := FFindForm.Width;

    Result := True;
  end;
end;

type
  TCustomReplaceDialog = class(TReplaceDialog)
  public
    function Execute: Boolean; override;
  end;

function TCustomReplaceDialog.Execute: Boolean;
var
  I: Integer;
begin
  Result := False;
  if not Assigned(FFindForm) then
    FFindForm := CreateForm();

  if Assigned(FFindForm) then
  begin
    for I := FFindForm.ComponentCount - 1 downto 0 do
      if (FFindForm.Components[I] is TButton) then
      begin
        if (TButton(FFindForm.Components[I]).Caption = 'Find more') then
          TButton(FFindForm.Components[I]).Free()
        else
        if (TButton(FFindForm.Components[I]).Caption = 'Cancel') then
          TButton(FFindForm.Components[I]).Align := alBottom
        else
          TButton(FFindForm.Components[I]).Align := alTop;
      end;

    CalcPosition(FFindForm);

    SetFormValues();

    FFindForm.OnClose := @DoCloseForm;
    FFindForm.OnShow  := @DoShowForm;
    FFindForm.Caption := Title;
    FFindForm.ShowModal();

    Height := FFindForm.Height;
    Width  := FFindForm.Width;

    Result := True;
  end;
end;

procedure TSimbaEditorFind.DoFind(Sender: TObject);
var
  SearchOptions: TSynSearchOptions;
  Count: Integer;
begin
  with TSimbaEditor(FEditor) do
  begin
    FMatches := AddMarks(False, frMatchCase in FDialog.Options, frWholeWord in FDialog.Options, FDialog.FindText);

    SearchOptions := [];
    if (frMatchCase in FDialog.Options) then
      SearchOptions := SearchOptions + [ssoMatchCase];
    if (frWholeWord in FDialog.Options) then
      SearchOptions := SearchOptions + [ssoWholeWord];

    if (frEntireScope in FDialog.Options) then
    begin
      Count := SearchReplace(FDialog.FindText, '', SearchOptions + [ssoEntireScope]);
      if (Count = 0) then // wrap around
        Count := SearchReplaceEx(FDialog.FindText, '', SearchOptions + [ssoEntireScope], Point(1, 1));
    end else
      Count := SearchReplace(FDialog.FindText, '', SearchOptions + [ssoSelectedOnly]);

    if (Count = 0) then
      MessageDlg('Not found', Format('Search "%s" not found!', [FDialog.FindText]), mtInformation, [mbOK], 0);

    FDialog.CloseDialog();
    if CanSetFocus() then
      SetFocus();
  end;
end;

constructor TSimbaEditorFind.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDialog := TCustomFindDialog.Create(Self);
  FDialog.OnFind := @DoFind;

  // Default options
  FDialog.Options := FDialog.Options + [frHideUpDown];
end;

procedure TSimbaEditorFind.Execute(Editor: TSynEdit);
begin
  FEditor := Editor;
  with TSimbaEditor(FEditor) do
    HideMarks();

  if FEditor.GetWordAtRowCol(FEditor.CaretXY) <> '' then
    FDialog.FindText := FEditor.GetWordAtRowCol(FEditor.CaretXY);

  if FEditor.SelAvail then
    FDialog.Options := FDialog.Options - [frEntireScope, frHideEntireScope]
  else
    FDialog.Options := FDialog.Options + [frEntireScope, frHideEntireScope];

  FDialog.Execute();
end;

procedure TSimbaEditorFind.ExecuteNoDialog(Editor: TSynEdit; FindText: String; CaseSens, WholeWord: Boolean);
var
  SearchOptions: TSynSearchOptions;
begin
  FEditor := Editor;
  with TSimbaEditor(FEditor) do
    HideMarks();

  FDialog.FindText := FindText;
  if CaseSens then
    FDialog.Options := FDialog.Options + [frMatchCase]
  else
    FDialog.Options := FDialog.Options - [frMatchCase];

  if WholeWord then
    FDialog.Options := FDialog.Options + [frWholeWord]
  else
    FDialog.Options := FDialog.Options - [frWholeWord];

  with TSimbaEditor(FEditor) do
  begin
    SearchOptions := [ssoEntireScope];
    if (frMatchCase in FDialog.Options) then
      SearchOptions := SearchOptions + [ssoMatchCase];
    if (frWholeWord in FDialog.Options) then
      SearchOptions := SearchOptions + [ssoWholeWord];

    FMatches := AddMarks(True, frMatchCase in FDialog.Options, frWholeWord in FDialog.Options, FDialog.FindText);

    SearchReplaceEx(FDialog.FindText, '', SearchOptions, Point(1, 1));
  end;

  SimbaIDEEvents.Notify(SimbaIDEEvent.TAB_SEARCH, Self);
end;

procedure TSimbaEditorFind.FindPrev(Editor: TSynEdit);
var
  MaxCaret: TPoint;
  SearchOptions: TSynSearchOptions;
begin
  FEditor := Editor;

  with TSimbaEditor(FEditor) do
  begin
    MaxCaret.Y := TextView.Count;
    MaxCaret.X := Length(TextView[TextView.Count - 1]) + 1;

    SearchOptions := [ssoFindContinue, ssoBackwards];

    if (frMatchCase in FDialog.Options) then
      SearchOptions := SearchOptions + [ssoMatchCase];
    if (frWholeWord in FDialog.Options) then
      SearchOptions := SearchOptions + [ssoWholeWord];

    if SearchReplace(FDialog.FindText, '', SearchOptions) = 0 then
      SearchReplaceEx(FDialog.FindText, '', SearchOptions, MaxCaret);

    ShowMarks();
  end;
end;

procedure TSimbaEditorFind.FindNext(Editor: TSynEdit);
var
  SearchOptions: TSynSearchOptions;
begin
  FEditor := Editor;

  with TSimbaEditor(FEditor) do
  begin
    SearchOptions := [ssoFindContinue];

    if (frMatchCase in FDialog.Options) then SearchOptions := SearchOptions + [ssoMatchCase];
    if (frWholeWord in FDialog.Options) then SearchOptions := SearchOptions + [ssoWholeWord];

    if SearchReplace(FDialog.FindText, '', SearchOptions) = 0 then
      SearchReplaceEx(FDialog.FindText, '', SearchOptions, Point(1, 1));

    ShowMarks();
  end;
end;

constructor TSimbaEditorReplace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDialog := TCustomReplaceDialog.Create(Self);
  FDialog.OnReplace := @DoReplace;

  // Default options
  FDialog.Options := FDialog.Options + [frHideUpDown] - [frHidePromptOnReplace];
end;

procedure TSimbaEditorReplace.DoConfirmReplace(Sender: TObject; const ASearch, AReplace: String; Line, Column: Integer; var ReplaceAction: TSynReplaceAction);
begin
  case MessageDlg('Replace', Format('Replace "%s" with "%s"?', [ASearch, AReplace]), mtConfirmation, [mbYes, mbYesToAll, mbNo, mbCancel], 0) of
    mrYes:      ReplaceAction := raReplace;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrNo:       ReplaceAction := raSkip;
    mrCancel:   ReplaceAction := raCancel;
  end;
end;

procedure TSimbaEditorReplace.DoReplace(Sender: TObject);
var
  SearchOptions: TSynSearchOptions;
begin
  SearchOptions := [];

  if (frReplace in FDialog.Options)         then SearchOptions := SearchOptions + [ssoReplace];
  if (frReplaceAll in FDialog.Options)      then SearchOptions := SearchOptions + [ssoReplaceAll];
  if (frMatchCase in FDialog.Options)       then SearchOptions := SearchOptions + [ssoMatchCase];
  if (frWholeWord in FDialog.Options)       then SearchOptions := SearchOptions + [ssoWholeWord];
  if (frPromptOnReplace in FDialog.Options) then SearchOptions := SearchOptions + [ssoPrompt];

  with TSimbaEditor(FEditor) do
  begin
    BeginUndoBlock();

    OnReplaceText := @DoConfirmReplace;

    try
      if (frEntireScope in FDialog.Options) then
        SearchReplaceEx(FDialog.FindText, FDialog.ReplaceText, SearchOptions, Point(1, 1))
      else
        SearchReplace(FDialog.FindText, FDialog.ReplaceText, SearchOptions + [ssoSelectedOnly]);
    finally
      OnReplaceText := nil;

      EndUndoBlock();
    end;

    FDialog.CloseDialog();
    if CanSetFocus() then
      SetFocus();
  end;
end;

procedure TSimbaEditorReplace.Execute(Editor: TSynEdit);
begin
  FEditor := Editor;

  if FEditor.GetWordAtRowCol(FEditor.CaretXY) <> '' then
    FDialog.FindText := FEditor.GetWordAtRowCol(FEditor.CaretXY);

  if FEditor.SelAvail then
    FDialog.Options := FDialog.Options - [frEntireScope, frHideEntireScope]
  else
    FDialog.Options := FDialog.Options + [frEntireScope, frHideEntireScope];

  FDialog.Execute();
end;

end.

