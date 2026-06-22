{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Find (also replace) in files form
}
unit simba.form_findinfiles;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, LCLType, Menus,
  SynEditSearch,
  SynEditHighlighter,
  simba.ide_events,
  simba.component_synedit,
  simba.component_button,
  simba.component_edit,
  simba.component_treeview,
  simba.component_splitter;

type
  TFindMatch = record
    Line: Integer;
    ColStart: Integer;
    ColEnd: Integer;
    Text: String;
  end;

  TFindFileMatches = record
    FileName: String;
    Matches: array of TFindMatch;
  end;
  TFindFileMatchesArray = array of TFindFileMatches;

  TFindResultNode = class(TTreeNode)
  public
    FileName: String;
    Line: Integer;
    ColStart: Integer;
    ColEnd: Integer;
    HighlightStart: Integer; // 0-based char index
    HighlightLen: Integer;
  end;

  TPreviewMemo = class(TSimbaSynEdit)
  protected
    FFileName: String;
  public
    constructor Create(AOwner: TComponent; HighlighterClass: TSynCustomHighlighterClass); override;

    procedure Reset;
    procedure ShowMatch(const AFileName: String; ALine, AColStart, AColEnd: Integer);
  end;

  TSimbaFindInFilesForm = class(TForm)
  protected
    SearchEdit: TSimbaLabeledEdit;
    ReplaceEdit: TSimbaLabeledEdit;
    FToggleCase: TSimbaToggleButton;
    FToggleWords: TSimbaToggleButton;
    FButtonHistory: TSimbaButton;
    FHistoryPopup: TPopupMenu;

    DirEdit: TSimbaLabeledEdit;
    FButtonSelectDir: TSimbaButton;
    MaskEdit: TSimbaLabeledEdit;

    FResultsList: TSimbaTreeView;
    FResultsHint: TLabel;
    FSplitter: TSimbaSplitter;
    FPreviewBox: TCustomControl;
    FPreview: TPreviewMemo;
    FPreviewShown: Boolean;

    FCountLabel: TLabel;
    FButtonSearch: TSimbaButton;
    FButtonReplace: TSimbaButton;
    FButtonOpen: TSimbaButton;
    FButtonOpenAll: TSimbaButton;

    FSearching: Boolean;
    FSearchFileCount: Integer;
    FSearchedFiles: Integer;

    FSearchString: String;
    FReplaceString: String;
    FSearchLocation: String;
    FSearchCaseSens: Boolean;
    FSearchWholeWords: Boolean;
    FSearchMask: String;

    function CollectMatches(out ATotal, AFiles: Integer): TFindFileMatchesArray;
    function ApplyReplacements(const AResults: TFindFileMatchesArray): Integer;
    procedure DoSearching;
    procedure DoStartSearch;
    procedure DoReplacing;
    procedure DoStartReplace;
    procedure AddHistory;
    procedure StartSeacher(AProc: TThreadMethod);
    procedure DoUpdateProgress(Data: PtrInt);
    procedure DoSearchClick(Sender: TObject);
    procedure DoReplaceClick(Sender: TObject);
    procedure DoSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoHistoryButtonClick(Sender: TObject);
    procedure DoHistoryPopup(Sender: TObject);
    procedure DoHistoryItemClick(Sender: TObject);
    procedure AddSearchToHistory(const ASearch, ALocation, AMask, AReplace: String; ACaseSens, AWholeWords: Boolean);
    procedure ApplyHistoryEntry(const AEntry: String);
    procedure DoSelectDirButtonClick(Sender: TObject);
    procedure DoTogglePanelResize(Sender: TObject);
    procedure DoSelectionChange(Sender: TObject);
    procedure ShowResultsHint(AVisible: Boolean);
    procedure ShowPreview(AVisible: Boolean);
    procedure ClearResults;
    procedure DoDoubleClick(Sender: TObject);
    procedure DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
    procedure DoOpenClick(Sender: TObject);
    procedure DoOpenAllClick(Sender: TObject);
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoShow; override;
  public
    constructor Create; reintroduce;

    procedure OpenSelected;
    procedure OpenAllFiles;
  end;

var
  SimbaFindInFilesForm: TSimbaFindInFilesForm;

implementation

uses
  Masks, SynEdit, SynEditTypes, SynEditMarkupHighAll, ATCanvasPrimitives,
  simba.base,
  simba.initializations,
  simba.ide_docking,
  simba.ide_controller,
  simba.threading,
  simba.component_theme,
  simba.component_images,
  simba.editor_highlighter,
  simba.dialog,
  simba.fs,
  simba.settings;

constructor TPreviewMemo.Create(AOwner: TComponent; HighlighterClass: TSynCustomHighlighterClass);
begin
  inherited Create(AOwner, HighlighterClass);

  ReadOnly := True;
  MaxUndo := 0;

  with TSynEditMarkupHighlightAll(MarkupByClass[TSynEditMarkupHighlightAll]) do
  begin
    MarkupInfo.Background := $004F8FC4;
    MarkupInfo.Foreground := clWhite;
    SearchOptions := [];
  end;
  MarkupByClass[TSynEditMarkupHighlightAllCaret].Enabled := False;

  Gutter.CodeFoldPart().Visible := False;
  Gutter.ChangesPart().Visible := False;
end;

procedure TPreviewMemo.Reset;
begin
  FFileName := '';
  Lines.Clear();
end;

procedure TPreviewMemo.ShowMatch(const AFileName: String; ALine, AColStart, AColEnd: Integer);
var
  Term: String;
begin
  if (FFileName <> AFileName) then
  begin
    FFileName := AFileName;

    BeginUpdate(False);
    try
      try
        Lines.LoadFromFile(AFileName);
      except
        Lines.Text := '';
      end;
    finally
      EndUpdate();
    end;
  end;

  CaretXY := TPoint.Create(AColStart, ALine);

  Term := '';
  if (ALine >= 1) and (ALine <= Lines.Count) and (AColEnd > AColStart) then
    Term := Copy(Lines[ALine - 1], AColStart, AColEnd - AColStart);

  with TSynEditMarkupHighlightAll(MarkupByClass[TSynEditMarkupHighlightAll]) do
  begin
    SearchString := Term;
    Enabled := (Term <> '');
  end;
  TopLine := Max(1, ALine - (LinesInWindow div 2));

  Invalidate();
end;

function TSimbaFindInFilesForm.CollectMatches(out ATotal, AFiles: Integer): TFindFileMatchesArray;
var
  Searcher: TSynEditSearch;
  Lines: TStringList;
  SearchStart, SearchEnd, FoundStart, FoundEnd: TPoint;
  FileName: String;
  Match: TFindMatch;
  FileResult: TFindFileMatches;
  FileList: TStringArray;
  FileIndex: Integer;
  LastTick: QWord;
  MaskList: TMaskList;
begin
  Result := nil;
  ATotal := 0;
  AFiles := 0;
  FSearchedFiles := 0;

  Searcher := TSynEditSearch.Create();
  Searcher.Sensitive := FSearchCaseSens;
  Searcher.Whole := FSearchWholeWords;
  Searcher.Pattern := FSearchString;

  // Build the file mask once (empty = match every file). Commas are also a seperator
  MaskList := nil;
  if (Trim(FSearchMask) <> '') then
    MaskList := TMaskList.Create(StringReplace(FSearchMask, ',', ';', [rfReplaceAll]), ';', False);

  Lines := TStringList.Create();
  try
    FileList := TSimbaDir.DirListFiles(FSearchLocation, True);
    FSearchFileCount := Length(FileList);
    LastTick := 0;

    for FileIndex := 0 to High(FileList) do
    begin
      FileName := FileList[FileIndex];
      if (GetTickCount64() - LastTick >= 500) then
      begin
        LastTick := GetTickCount64();
        Application.RemoveAsyncCalls(Self);
        Application.QueueAsyncCall(@DoUpdateProgress, FileIndex);
      end;

      if (MaskList <> nil) and not MaskList.Matches(ExtractFileName(FileName)) then
        Continue;
      if not TSimbaFile.FileIsText(FileName) then
        Continue;
      Inc(FSearchedFiles);

      try
        Lines.LoadFromFile(FileName);
      except
        Lines.Clear();
      end;
      if (Lines.Count = 0) then
        Continue;

      SearchStart := TPoint.Create(1, 1);
      SearchEnd   := TPoint.Create(Length(Lines[Lines.Count - 1]) + 1, Lines.Count);

      FileResult := Default(TFindFileMatches);
      FileResult.FileName := FileName;

      while Searcher.FindNextOne(Lines, SearchStart, SearchEnd, FoundStart, FoundEnd) do
      begin
        Inc(ATotal);

        Match := Default(TFindMatch);
        Match.Line     := FoundStart.Y;
        Match.ColStart := FoundStart.X;
        if (FoundEnd.Y = FoundStart.Y) then
          Match.ColEnd := FoundEnd.X
        else
          Match.ColEnd := Length(Lines[FoundStart.Y - 1]) + 1;
        Match.Text := Lines[FoundStart.Y - 1];

        FileResult.Matches += [Match];
        SearchStart := FoundEnd;
      end;

      if (Length(FileResult.Matches) > 0) then
      begin
        Inc(AFiles);
        Result += [FileResult];
      end;
    end;
  finally
    Searcher.Free();
    Lines.Free();
    MaskList.Free();
  end;
end;

// Use SynEdit engine to replace text so exact matches from TSynEditSearch are used.
// Must be run on main thread.
function TSimbaFindInFilesForm.ApplyReplacements(const AResults: TFindFileMatchesArray): Integer;
var
  Editor: TSynEdit;
  Options: TSynSearchOptions;
  I: Integer;
begin
  Result := 0;

  Options := [ssoReplaceAll, ssoEntireScope];
  if FSearchCaseSens   then Include(Options, ssoMatchCase);
  if FSearchWholeWords then Include(Options, ssoWholeWord);

  Editor := TSynEdit.Create(nil);
  try
    for I := 0 to High(AResults) do
    begin
      try
        Editor.Lines.LoadFromFile(AResults[I].FileName);
      except
        Continue;
      end;

      if (Editor.SearchReplaceEx(FSearchString, FReplaceString, Options, TPoint.Create(1, 1)) > 0) then
        try
          Editor.Lines.SaveToFile(AResults[I].FileName);
          Inc(Result);
        except
        end;
    end;
  finally
    Editor.Free();
  end;
end;

procedure TSimbaFindInFilesForm.DoSearching;
var
  Total, Files: Integer;
  Results: TFindFileMatchesArray;

  procedure BeginSearch;
  begin
    FCountLabel.Caption := 'Searching...';
    FResultsList.Clear();
    ShowResultsHint(False);
    FPreview.Reset();
    ShowPreview(False);

    Application.ProcessMessages();
  end;

  procedure EndSearch;
  var
    I, J, LeadingWS: Integer;
    Line, Trimmed: String;
    Node, FirstNode: TFindResultNode;
  begin
    FirstNode := nil;

    FResultsList.BeginUpdate();
    try
      for I := 0 to High(Results) do
        for J := 0 to High(Results[I].Matches) do
        begin
          Line := Results[I].Matches[J].Text;

          // Trim leading whitespace for display, shift highlight offset to match.
          LeadingWS := 0;
          while (LeadingWS < Length(Line)) and (Line[LeadingWS + 1] <= #32) do
            Inc(LeadingWS);
          Trimmed := Copy(Line, LeadingWS + 1, Length(Line));

          Node := TFindResultNode(FResultsList.AddNode(Trimmed));
          Node.FileName := Results[I].FileName;
          Node.Line     := Results[I].Matches[J].Line;
          Node.ColStart := Results[I].Matches[J].ColStart;
          Node.ColEnd   := Results[I].Matches[J].ColEnd;
          Node.HighlightStart := Max(0, (Results[I].Matches[J].ColStart - 1) - LeadingWS);
          Node.HighlightLen   := Results[I].Matches[J].ColEnd - Results[I].Matches[J].ColStart;

          if (FirstNode = nil) then
            FirstNode := Node;
        end;
    finally
      FResultsList.EndUpdate();
    end;

    FCountLabel.Caption := Format('%d matches in %d files', [Total, FSearchedFiles]);

    ShowResultsHint(Total = 0);

    FSearching := False;

    // Stop any progress update that's still queued
    Application.RemoveAsyncCalls(Self);
    FButtonSearch.Enabled  := True;
    FButtonReplace.Enabled := True;

    if (FirstNode <> nil) then
      FResultsList.Selected := FirstNode;
  end;

begin
  RunInMainThread(@BeginSearch);
  Results := CollectMatches(Total, Files);
  RunInMainThread(@EndSearch);
end;

procedure TSimbaFindInFilesForm.DoReplacing;
var
  Total, Files, Replaced: Integer;
  Confirmed: Boolean;
  Results: TFindFileMatchesArray;

  procedure ConfirmReplace;
  begin
    if (Total = 0) then
    begin
      FCountLabel.Caption := 'No matches';
      Confirmed := False;
      Exit;
    end;
    Confirmed := ShowQuestionDialog('Replace in Files', 'Replace %d occurrence(s) across %d file(s)?', [Total, Files]) = ESimbaDialogButton.YES;
    if Confirmed then
      FCountLabel.Caption := 'Replacing...';
  end;

  procedure ApplyOnMain;
  begin
    Replaced := ApplyReplacements(Results);
  end;

  procedure EndReplace;
  begin
    FSearching := False;
    Application.RemoveAsyncCalls(Self);
    FButtonSearch.Enabled  := True;
    FButtonReplace.Enabled := True;

    if Confirmed then
    begin
      ClearResults();
      FCountLabel.Caption := Format('Replaced %d occurrence(s) in %d file(s)', [Total, Replaced]);
    end;
  end;

begin
  Results := CollectMatches(Total, Files);

  Confirmed := False;
  RunInMainThread(@ConfirmReplace);

  Replaced := 0;
  if Confirmed then
    RunInMainThread(@ApplyOnMain);

  RunInMainThread(@EndReplace);
end;

procedure TSimbaFindInFilesForm.AddHistory;
begin
  FSearchString     := SearchEdit.Edit.Text;
  FReplaceString    := ReplaceEdit.Edit.Text;
  FSearchLocation   := DirEdit.Edit.Text;
  FSearchCaseSens   := FToggleCase.Down;
  FSearchWholeWords := FToggleWords.Down;
  FSearchMask       := MaskEdit.Edit.Text; // empty = match every file

  AddSearchToHistory(FSearchString, FSearchLocation, FSearchMask, FReplaceString, FSearchCaseSens, FSearchWholeWords);
end;

procedure TSimbaFindInFilesForm.StartSeacher(AProc: TThreadMethod);
begin
  AddHistory();

  FSearching := True;
  FButtonSearch.Enabled  := False;
  FButtonReplace.Enabled := False;
  FCountLabel.Caption := 'Searching...';

  RunInThread(AProc, True);
end;

procedure TSimbaFindInFilesForm.DoStartSearch;
begin
  // Ignore if already running
  if FSearching then
    Exit;

  if (Trim(SearchEdit.Edit.Text) = '') then
  begin
    ClearResults();
    FCountLabel.Caption := '';
    Exit;
  end;

  StartSeacher(@DoSearching);
end;

procedure TSimbaFindInFilesForm.DoStartReplace;
begin
  if FSearching or (Trim(SearchEdit.Edit.Text) = '') then
    Exit;

  StartSeacher(@DoReplacing);
end;

procedure TSimbaFindInFilesForm.DoUpdateProgress(Data: PtrInt);
begin
  FCountLabel.Caption := Format('Searching... %d / %d', [Integer(Data), FSearchFileCount]);
end;

procedure TSimbaFindInFilesForm.DoSearchClick(Sender: TObject);
begin
  DoStartSearch();
end;

procedure TSimbaFindInFilesForm.DoReplaceClick(Sender: TObject);
begin
  DoStartReplace();
end;

procedure TSimbaFindInFilesForm.DoSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    Key := 0;
    DoStartSearch();
  end;
end;

procedure TSimbaFindInFilesForm.AddSearchToHistory(const ASearch, ALocation, AMask, AReplace: String; ACaseSens, AWholeWords: Boolean);
var
  History: TStringList;
  I: Integer;
begin
  if (Trim(ASearch) = '') then
    Exit;

  History := TStringList.Create();
  try
    History.Text := SimbaSettings.General.FindInFilesSearchHistory.Value;

    for I := History.Count - 1 downto 0 do
      if (History[I].Split([#9])[0] = ASearch) then
        History.Delete(I);
    History.Insert(0, ASearch + #9 + ALocation + #9 + AMask + #9 + IntToStr(Ord(ACaseSens)) + #9 + IntToStr(Ord(AWholeWords)) + #9 + AReplace);

    while (History.Count > 15) do
      History.Delete(History.Count - 1);

    SimbaSettings.General.FindInFilesSearchHistory.Value := History.Text;
  finally
    History.Free();
  end;
end;

procedure TSimbaFindInFilesForm.ApplyHistoryEntry(const AEntry: String);
var
  Fields: TStringArray;
begin
  Fields := AEntry.Split([#9]);

  if (Length(Fields) > 0) then SearchEdit.Edit.Text   := Fields[0];
  if (Length(Fields) > 1) then DirEdit.Edit.Text      := Fields[1];
  if (Length(Fields) > 2) then MaskEdit.Edit.Text     := Fields[2];
  if (Length(Fields) > 3) then FToggleCase.Down   := (Fields[3] = '1');
  if (Length(Fields) > 4) then FToggleWords.Down  := (Fields[4] = '1');
  if (Length(Fields) > 5) then ReplaceEdit.Edit.Text  := Fields[5];
end;

procedure TSimbaFindInFilesForm.DoHistoryButtonClick(Sender: TObject);
var
  P: TPoint;
begin
  P := FButtonHistory.ClientToScreen(TPoint.Create(0, FButtonHistory.Height));
  FHistoryPopup.PopUp(P.X, P.Y);
end;

procedure TSimbaFindInFilesForm.DoHistoryPopup(Sender: TObject);
var
  History: TStringList;
  Item: TMenuItem;
  I: Integer;
  Fields: TStringArray;
  Search, Location, Replace, ItemCaption: String;
begin
  FHistoryPopup.Items.Clear();

  History := TStringList.Create();
  try
    History.Text := SimbaSettings.General.FindInFilesSearchHistory.Value;

    if (History.Count = 0) then
    begin
      Item := TMenuItem.Create(FHistoryPopup);
      Item.Caption := 'No recent searches';
      Item.Enabled := False;
      FHistoryPopup.Items.Add(Item);
      Exit;
    end;

    for I := 0 to History.Count - 1 do
    begin
      Fields := History[I].Split([#9]);
      Search := Fields[0];
      Location := '';
      Replace := '';
      if (Length(Fields) > 1) then Location := Fields[1];
      if (Length(Fields) > 5) then Replace := Fields[5];

      if (Replace <> '') then
        ItemCaption := Format('Replace "%s" in "%s"', [Replace, Location])
      else
        ItemCaption := Format('Find "%s" in "%s"', [Search, Location]);

      Item := TMenuItem.Create(FHistoryPopup);
      Item.Caption := StringReplace(ItemCaption, '&', '&&', [rfReplaceAll]);
      Item.Hint := History[I]; // search<TAB>location<TAB>mask<TAB>case<TAB>words<TAB>replace
      Item.OnClick := @DoHistoryItemClick;
      FHistoryPopup.Items.Add(Item);
    end;
  finally
    History.Free();
  end;
end;

procedure TSimbaFindInFilesForm.DoHistoryItemClick(Sender: TObject);
begin
  ApplyHistoryEntry(TMenuItem(Sender).Hint);
  DoStartSearch();
end;

procedure TSimbaFindInFilesForm.DoSelectDirButtonClick(Sender: TObject);
var
  InitialDir, Dir: String;
begin
  InitialDir := DirEdit.Edit.Text;
  if (InitialDir = '') then
    InitialDir := Application.Location;
  if SelectDirectory('Select location', InitialDir, Dir) then
    DirEdit.Edit.Text := Dir;
end;

procedure TSimbaFindInFilesForm.DoTogglePanelResize(Sender: TObject);
begin
  if (ReplaceEdit <> nil) and (SearchEdit <> nil) and (SearchEdit.Button <> nil) then
    ReplaceEdit.Edit.BorderSpacing.Right := SearchEdit.Button.Width + 5;
end;

procedure TSimbaFindInFilesForm.DoSelectionChange(Sender: TObject);
var
  Node: TFindResultNode;
begin
  Node := TFindResultNode(FResultsList.Selected);
  if (Node = nil) then
    Exit;

  ShowPreview(True);
  FPreview.ShowMatch(Node.FileName, Node.Line, Node.ColStart, Node.ColEnd);
end;

procedure TSimbaFindInFilesForm.ShowResultsHint(AVisible: Boolean);
begin
  FResultsHint.Visible := AVisible;
  FResultsList.Visible := not AVisible;
end;

procedure TSimbaFindInFilesForm.ClearResults;
begin
  FResultsList.Clear();
  ShowResultsHint(True);
  FPreview.Reset();
  ShowPreview(False);
end;

procedure TSimbaFindInFilesForm.ShowPreview(AVisible: Boolean);
begin
  if AVisible and (not FPreviewShown) then
  begin
    FPreviewShown := True;
    FPreviewBox.Height := FPreviewBox.Parent.ClientHeight div 2;
  end;

  FPreviewBox.Visible := AVisible;
  FSplitter.Visible := AVisible;
end;

procedure TSimbaFindInFilesForm.DoDoubleClick(Sender: TObject);
begin
  OpenSelected();
end;

procedure TSimbaFindInFilesForm.DoPaintNode(ACanvas: TCanvas; Node: TTreeNode);
var
  ResultNode: TFindResultNode;
  TextRect, ClipRect: TRect;
  Style: TTextStyle;
  Pre, Mid, Info: String;
  X, W, RightEdge, InfoLeft, InfoWidth, ClipRight: Integer;
  BgColor: TColor;
begin
  if not (Node is TFindResultNode) then
    Exit;
  ResultNode := TFindResultNode(Node);

  TextRect := Node.DisplayRect(True);

  if Node.Selected then
    BgColor := SimbaComponentTheme.ColorActive
  else
    BgColor := SimbaComponentTheme.ColorBackground;

  // Make sure to clear the entire row to clear default drawing
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := BgColor;
  ACanvas.FillRect(0, TextRect.Top, FResultsList.Width, TextRect.Bottom);

  Style := ACanvas.TextStyle;
  Style.Layout := tlCenter;

  Info := ExtractFileName(ResultNode.FileName) + '  ' + IntToStr(ResultNode.Line);
  InfoWidth := ACanvas.TextWidth(Info);
  RightEdge := FResultsList.Width - FResultsList.ScrollbarVert.Width - Scale96ToScreen(6);
  InfoLeft  := RightEdge - InfoWidth - Scale96ToScreen(4);
  ClipRight := Max(Node.DisplayTextLeft, InfoLeft - Scale96ToScreen(12));

  if (ResultNode.HighlightLen > 0) then
  begin
    Pre := Copy(Node.Text, 1, ResultNode.HighlightStart);
    Mid := Copy(Node.Text, ResultNode.HighlightStart + 1, ResultNode.HighlightLen);

    X := Node.DisplayTextLeft + ACanvas.TextWidth(Pre);
    W := ACanvas.TextWidth(Mid);

    if (X < ClipRight) then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := ColorBlend($004F8FC4, BgColor, 170);
      ACanvas.FillRect(X, TextRect.Top, Min(X + W, ClipRight), TextRect.Bottom);
    end;
  end;

  ClipRect := TRect.Create(Node.DisplayTextLeft, TextRect.Top, ClipRight, TextRect.Bottom);
  Style.Opaque := False;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := SimbaComponentTheme.ColorFont;
  ACanvas.TextRect(ClipRect, Node.DisplayTextLeft, TextRect.Top, Node.Text, Style);

  Style.Opaque := True;
  ACanvas.Brush.Color := BgColor;
  if Node.Selected then
    ACanvas.Font.Color := SimbaComponentTheme.ColorFont
  else
    ACanvas.Font.Color := SimbaComponentTheme.ColorLine;
  ACanvas.TextRect(TRect.Create(InfoLeft, TextRect.Top, RightEdge + Scale96ToScreen(2), TextRect.Bottom), RightEdge - InfoWidth, TextRect.Top, Info, Style);
end;

procedure TSimbaFindInFilesForm.DoOpenClick(Sender: TObject);
begin
  OpenSelected();
end;

procedure TSimbaFindInFilesForm.OpenSelected;
var
  Node: TFindResultNode;
begin
  Node := TFindResultNode(FResultsList.Selected);
  if (Node <> nil) then
    SimbaController.OpenInTab(Node.FileName, Node.ColStart, Node.Line);
end;

procedure TSimbaFindInFilesForm.DoOpenAllClick(Sender: TObject);
begin
  OpenAllFiles();
end;

procedure TSimbaFindInFilesForm.OpenAllFiles;
var
  I: Integer;
  Node: TFindResultNode;
  Seen: TStringList;
begin
  Seen := TStringList.Create();
  try
    Seen.Sorted := True;

    for I := 0 to FResultsList.Items.Count - 1 do
    begin
      Node := TFindResultNode(FResultsList.Items[I]);
      if (Node <> nil) and (Seen.IndexOf(Node.FileName) < 0) then
      begin
        Seen.Add(Node.FileName);
        SimbaController.OpenInTab(Node.FileName);
      end;
    end;
  finally
    Seen.Free();
  end;
end;

constructor TSimbaFindInFilesForm.Create;
var
  TogglePanel, ButtonRow, LocationRow, MainContainer, Footer, Divider: TCustomControl;
  History: TStringList;

  function CreateRow: TCustomControl;
  begin
    Result := TCustomControl.Create(Self);
    Result.AutoSize := True;
    Result.ControlStyle := Result.ControlStyle + [csOpaque];
    Result.Color := SimbaComponentTheme.ColorFrame;
  end;

  function CreateToggle(AParent: TWinControl; ACaption, AHint: String): TSimbaToggleButton;
  begin
    Result := TSimbaToggleButton.Create(Self);
    Result.Parent := AParent;
    Result.Caption := ACaption;
    Result.Hint := AHint;
    Result.ShowHint := True;
    Result.XPadding := 6;
  end;

  function CreateLabeledEdit(AParent: TWinControl; ACaption, ALabelMeasure: String): TSimbaLabeledEdit;
  begin
    Result := TSimbaLabeledEdit.Create(Self);
    Result.Parent := AParent;
    Result.Caption := ACaption;
    Result.LabelMeasure := ALabelMeasure;
    Result.Color := SimbaComponentTheme.ColorFrame;
    Result.Edit.ColorBorder := SimbaComponentTheme.ColorScrollBarActive;
    Result.Edit.TabStop := True;
  end;

begin
  inherited Create(nil);

  Caption := 'Find in Files';
  Color := SimbaComponentTheme.ColorFrame;
  Font.Color := SimbaComponentTheme.ColorFont;

  Width  := Scale96ToScreen(650);
  Height := Scale96ToScreen(450);

  ButtonRow := CreateRow;
  ButtonRow.Parent := Self;
  ButtonRow.Align := alTop;
  ButtonRow.BorderSpacing.Top   := 8;
  ButtonRow.BorderSpacing.Left  := 5;
  ButtonRow.BorderSpacing.Right := 5;

  FButtonSearch := TSimbaButton.Create(Self);
  FButtonSearch.Parent := ButtonRow;
  FButtonSearch.Caption := 'Find';
  FButtonSearch.XPadding := 15;
  FButtonSearch.OnClick := @DoSearchClick;
  FButtonSearch.AnchorParallel(akLeft, 0, ButtonRow);
  FButtonSearch.AnchorParallel(akTop, 0, ButtonRow);

  FButtonReplace := TSimbaButton.Create(Self);
  FButtonReplace.Parent := ButtonRow;
  FButtonReplace.Caption := 'Replace';
  FButtonReplace.XPadding := 15;
  FButtonReplace.OnClick := @DoReplaceClick;
  FButtonReplace.AnchorToNeighbour(akLeft, 6, FButtonSearch);
  FButtonReplace.AnchorParallel(akTop, 0, ButtonRow);

  LocationRow := CreateRow;
  LocationRow.Parent := Self;
  LocationRow.Align := alTop;
  LocationRow.BorderSpacing.Top   := 8;
  LocationRow.BorderSpacing.Left  := 5;
  LocationRow.BorderSpacing.Right := 5;

  MaskEdit := CreateLabeledEdit(LocationRow, 'Mask', 'Mask:');
  MaskEdit.Edit.Constraints.MinWidth := Scale96ToScreen(150);
  MaskEdit.Anchors := [];
  MaskEdit.AnchorParallel(akTop, 0, LocationRow);
  MaskEdit.AnchorParallel(akRight, 0, LocationRow);
  MaskEdit.Edit.Text := '*.simba';
  MaskEdit.Edit.AddHandlerOnKeyDown(@DoSearchKeyDown);

  FButtonSelectDir := TSimbaButton.Create(Self);
  FButtonSelectDir.Image := ESimbaButtonImage.SELECT_DIR;
  FButtonSelectDir.OnClick := @DoSelectDirButtonClick;

  DirEdit := CreateLabeledEdit(LocationRow, 'Directory', 'Directory:');
  DirEdit.AutoSize := False;
  DirEdit.Button := FButtonSelectDir;
  DirEdit.AnchorParallel(akLeft, 0, LocationRow);
  DirEdit.AnchorParallel(akTop, 0, MaskEdit);
  DirEdit.AnchorToNeighbour(akRight, 8, MaskEdit);
  DirEdit.AnchorParallel(akBottom, 0, MaskEdit);
  DirEdit.Edit.Caption := Application.Location;
  DirEdit.Edit.AddHandlerOnKeyDown(@DoSearchKeyDown);

  TogglePanel := CreateRow;
  TogglePanel.OnResize := @DoTogglePanelResize;

  FToggleWords := CreateToggle(TogglePanel, 'W', 'Whole word only');
  FToggleWords.AnchorParallel(akLeft, 0, TogglePanel);
  FToggleWords.AnchorParallel(akTop, 0, TogglePanel);
  FToggleWords.Font.Bold := True;

  FToggleCase := CreateToggle(TogglePanel, 'Aa', 'Match Case');
  FToggleCase.AnchorToNeighbour(akLeft, 3, FToggleWords);
  FToggleCase.AnchorParallel(akTop, 0, TogglePanel);
  FToggleCase.Font.Bold := True;

  FHistoryPopup := TPopupMenu.Create(Self);
  FHistoryPopup.OnPopup := @DoHistoryPopup;

  FButtonHistory := TSimbaButton.Create(Self);
  FButtonHistory.Parent := TogglePanel;
  FButtonHistory.ImageList := SimbaImages;
  FButtonHistory.ImageIndex := SimbaImages.FOLDER_RECENT;
  FButtonHistory.Hint := 'Recent searches';
  FButtonHistory.ShowHint := True;
  FButtonHistory.XPadding := 6;
  FButtonHistory.OnClick := @DoHistoryButtonClick;
  FButtonHistory.AnchorToNeighbour(akLeft, 3, FToggleCase);
  FButtonHistory.AnchorParallel(akTop, 0, TogglePanel);

  ReplaceEdit := CreateLabeledEdit(Self, 'Replace', 'Directory:');
  ReplaceEdit.Align := alTop;
  ReplaceEdit.BorderSpacing.Top   := 8;
  ReplaceEdit.BorderSpacing.Left  := 5;
  ReplaceEdit.BorderSpacing.Right := 5;
  ReplaceEdit.Edit.AddHandlerOnKeyDown(@DoSearchKeyDown);

  SearchEdit := CreateLabeledEdit(Self, 'Search', 'Directory:');
  SearchEdit.Align := alTop;
  SearchEdit.BorderSpacing.Top   := 8;
  SearchEdit.BorderSpacing.Left  := 5;
  SearchEdit.BorderSpacing.Right := 5;
  SearchEdit.Button := TogglePanel;
  SearchEdit.Edit.AddHandlerOnKeyDown(@DoSearchKeyDown);

  Footer := CreateRow;
  Footer.Parent := Self;
  Footer.Align := alBottom;
  Footer.BorderSpacing.Top   := 8;
  Footer.BorderSpacing.Left  := 5;
  Footer.BorderSpacing.Right := 5;
  Footer.BorderSpacing.Bottom := 5;

  Divider := TCustomControl.Create(Self);
  Divider.Parent := Footer;
  Divider.Align := alTop;
  Divider.Height := Scale96ToScreen(1);
  Divider.ControlStyle := Divider.ControlStyle + [csOpaque];
  Divider.Color := SimbaComponentTheme.ColorScrollBarActive;
  Divider.BorderSpacing.Bottom := 6;

  FButtonOpenAll := TSimbaButton.Create(Self);
  FButtonOpenAll.Parent := Footer;
  FButtonOpenAll.Caption := 'Open All';
  FButtonOpenAll.XPadding := 15;
  FButtonOpenAll.Align := alRight;
  FButtonOpenAll.OnClick := @DoOpenAllClick;

  FButtonOpen := TSimbaButton.Create(Self);
  FButtonOpen.Parent := Footer;
  FButtonOpen.Caption := 'Open';
  FButtonOpen.XPadding := 15;
  FButtonOpen.OnClick := @DoOpenClick;
  FButtonOpen.Anchors := [akRight];
  FButtonOpen.AnchorSide[akRight].Control := FButtonOpenAll;
  FButtonOpen.AnchorSide[akRight].Side := asrLeft;
  FButtonOpen.BorderSpacing.Right := 6;
  FButtonOpen.AnchorVerticalCenterTo(FButtonOpenAll);

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Parent := Footer;
  FCountLabel.Caption := '';
  FCountLabel.Layout := tlCenter;
  FCountLabel.Font.Color := SimbaComponentTheme.ColorFont;
  FCountLabel.Anchors := [akLeft];
  FCountLabel.AnchorSide[akLeft].Control := Footer;
  FCountLabel.AnchorSide[akLeft].Side := asrLeft;
  FCountLabel.BorderSpacing.Left := 6;
  FCountLabel.AnchorVerticalCenterTo(FButtonOpenAll);

  MainContainer := TCustomControl.Create(Self);
  MainContainer.Parent := Self;
  MainContainer.Align := alClient;
  MainContainer.BorderSpacing.Top   := 8;
  MainContainer.BorderSpacing.Left  := 5;
  MainContainer.BorderSpacing.Right := 5;

  FSplitter := TSimbaSplitter.Create(MainContainer);
  FSplitter.Parent := MainContainer;
  FSplitter.Align := alBottom;
  FSplitter.Height := Scale96ToScreen(7);
  FSplitter.MinSize := Scale96ToScreen(80);
  FSplitter.AutoSnap := False;
  FSplitter.Visible := False;

  FPreviewBox := TCustomControl.Create(MainContainer);
  FPreviewBox.Parent := MainContainer;
  FPreviewBox.Align := alBottom;
  FPreviewBox.Visible := False;

  FPreview := TPreviewMemo.Create(FPreviewBox, TSimbaEditorHighlighter);
  FPreview.Parent := FPreviewBox;
  FPreview.Align := alClient;
  FPreview.TabStop := False;

  FResultsList := TSimbaTreeView.Create(MainContainer, TFindResultNode);
  FResultsList.Parent := MainContainer;
  FResultsList.Align := alClient;
  FResultsList.TabStop := False;
  FResultsList.HideRoot();
  FResultsList.FilterVisible := False;
  FResultsList.SetItemHeight(Scale96ToScreen(22));
  FResultsList.ScrollbarHorz.Visible := False;
  FResultsList.OnSelectionChange := @DoSelectionChange;
  FResultsList.OnDoubleClick := @DoDoubleClick;
  FResultsList.OnPaintNode := @DoPaintNode;

  FResultsHint := TLabel.Create(Self);
  FResultsHint.Parent := MainContainer;
  FResultsHint.Align := alClient;
  FResultsHint.Alignment := taCenter;
  FResultsHint.Layout := tlCenter;
  FResultsHint.Caption := '(no search results)';
  FResultsHint.Font.Color := SimbaComponentTheme.ColorLine;
  ShowResultsHint(True);

  SearchEdit.TabOrder  := 0;
  ReplaceEdit.TabOrder := 1;
  LocationRow.TabOrder := 2;
  DirEdit.TabOrder     := 0;
  MaskEdit.TabOrder    := 1;

  History := TStringList.Create();
  try
    History.Text := SimbaSettings.General.FindInFilesSearchHistory.Value;
    if (History.Count > 0) then
      ApplyHistoryEntry(History[0]);
  finally
    History.Free();
  end;

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_FIND_IN_FILES]);
end;

procedure TSimbaFindInFilesForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.ACTION_FIND_IN_FILES:
      SimbaDocking.Show(Self);
  end;
end;

procedure TSimbaFindInFilesForm.DoShow;
begin
  inherited DoShow();

  if SearchEdit.Edit.CanSetFocus() then
    SearchEdit.Edit.SetFocus();
end;

procedure DoCreate;
begin
  SimbaFindInFilesForm := TSimbaFindInFilesForm.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaFindInFilesForm);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaFindInFilesForm');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaFindInFilesForm');

end.
