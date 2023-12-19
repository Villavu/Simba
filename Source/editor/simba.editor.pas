{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, ComCtrls, LCLType,
  SynEdit, SynEditTypes, SynGutterLineOverview, SynEditMouseCmds, SynEditMiscClasses, SynEditKeyCmds, SynEditHighlighter, SynEditMarkupCtrlMouseLink, SynEditMarkupHighAll,
  simba.mufasatypes, simba.settings, simba.editor_autocomplete, simba.editor_paramhint, simba.editor_attributes, simba.editor_modifiedlinegutter, simba.component_synedit;

type
  TSimbaEditorFileNameEvent = function(Sender: TObject): String of object;

  TSimbaEditor = class(TSimbaSynEdit)
  protected
    FMarkupHighlightAllCaret: TSynEditMarkupHighlightAllCaret;
    FMarkupCtrlMouse: TSynEditMarkupCtrlMouseLink;

    FUseSimbaColors: Boolean;
    FAutoComplete: TSimbaAutoComplete;
    FParamHint: TSimbaParamHint;

    FAttributes: TSimbaEditor_Attributes;
    FModifiedLinesGutter: TSimbaEditorModifiedLinesGutter;

    FFocusedLinesUpdating: Boolean;
    FFocusedLinesCount: Integer;
    FFocusedLinesColors: array of record
      Line: Integer;
      Color: TColor;
    end;

    FColorModified: TColor;
    FColorSaved: TColor;

    FLastTextChangeStamp: Int64;
    FModifiedEvent: TNotifyEvent;
    FGetFileNameEvent: TSimbaEditorFileNameEvent;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure FontChanged(Sender: TObject); override;

    procedure MaybeReplaceModifiers;

    procedure DoSettingChanged_Colors(Setting: TSimbaSetting);
    procedure DoSettingChanged_AllowCaretPastEOL(Setting: TSimbaSetting);
    procedure DoSettingChanged_RightMargin(Setting: TSimbaSetting);
    procedure DoSettingChanged_RightMarginVisible(Setting: TSimbaSetting);
    procedure DoSettingChanged_AntiAliased(Setting: TSimbaSetting);
    procedure DoSettingChanged_FontSize(Setting: TSimbaSetting);
    procedure DoSettingChanged_FontName(Setting: TSimbaSetting);

    // Temp line coloring
    procedure DoSpecialLineColor(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);

    procedure StatusChanged(AChanges: TSynStatusChanges); override;
    procedure SetUpdateState(NewUpdating: Boolean; Sender: TObject); override;

    function GetFileName: String;

    procedure SetColorModified(Value: TColor);
    procedure SetColorSaved(Value: TColor);
    procedure SetUseSimbaColors(Value: Boolean);
  public
    property TextView;
    property AutoComplete: TSimbaAutoComplete read FAutoComplete;
    property ParamHint: TSimbaParamHint read FParamHint;
    property ModifiedLinesGutter: TSimbaEditorModifiedLinesGutter read FModifiedLinesGutter;
    property Attributes: TSimbaEditor_Attributes read FAttributes;

    property OnModified: TNotifyEvent read FModifiedEvent write FModifiedEvent;
    property OnGetFileName: TSimbaEditorFileNameEvent read FGetFileNameEvent write FGetFileNameEvent;

    property ColorSaved: TColor read FColorSaved write SetColorSaved;
    property ColorModified: TColor read FColorModified write SetColorModified;
    property UseSimbaColors: Boolean read FUseSimbaColors write SetUseSimbaColors;

    property FileName: String read GetFileName;

    // Accept from TTreeView
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;

    function GetCaretPos(GoBackToWord: Boolean): Integer;

    // Is highlighter attribute at caret
    function IsHighlighterAttribute(Values: TStringArray): Boolean;

    // Is value ahead of caret
    function IsTextAhead(Values: TStringArray): Boolean;
    // Get Expression string at X,Y
    function GetExpression(X, Y: Integer): String;
    // Get word bounds at X,Y
    function GetExpressionEx(X, Y: Integer): String;
    // Execute a command that needs no extra data
    procedure ExecuteSimpleCommand(Command: TSynEditorCommand);
    // Repaint some extra things when saved
    procedure InvalidateGutter; override;

    procedure RegisterCaretMoveHandler(Handler: TNotifyEvent);
    procedure UnRegisterCaretMoveHandler(Handler: TNotifyEvent);

    procedure ClearFocusedLines;
    procedure FocusLine(Line, Column: Integer; AColor: TColor);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SynEditPointClasses, SynGutter, SynHighlighterPas_Simba, LazSynEditMouseCmdsTypes, Forms,
  simba.fonthelpers, simba.editor_blockcompletion,
  simba.editor_docgenerator, simba.editor_commentblock,
  simba.editor_mousewheelzoom, simba.editor_multicaret,
  simba.theme;

function TSimbaEditor.IsHighlighterAttribute(Values: TStringArray): Boolean;
var
  Token: String;
  Attri: TSynHighlighterAttributes;
  P: TPoint;
begin
  P := LogicalCaretXY;
  P.X -= 1;

  Result := GetHighlighterAttriAtRowCol(P, Token, Attri) and Attri.Name.ContainsAny(Values, False);
end;

function TSimbaEditor.IsTextAhead(Values: TStringArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Values) do
    if CompareText(Values[I], TextBetweenPoints[CaretXY, TPoint.Create(CaretX + Length(Values[I]), CaretY)]) = 0 then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

procedure TSimbaEditor.ExecuteSimpleCommand(Command: TSynEditorCommand);
begin
  CommandProcessor(Command, '', Pointer(nil));
end;

procedure TSimbaEditor.InvalidateGutter;
begin
  if Assigned(FModifiedLinesGutter) then
    FModifiedLinesGutter.ReCalc();

  inherited;
end;

procedure TSimbaEditor.RegisterCaretMoveHandler(Handler: TNotifyEvent);
begin
  GetCaretObj().AddChangeHandler(Handler);
end;

procedure TSimbaEditor.UnRegisterCaretMoveHandler(Handler: TNotifyEvent);
begin
  GetCaretObj().RemoveChangeHandler(Handler);
end;

procedure TSimbaEditor.ClearFocusedLines;
var
  I: Integer;
begin
  if (not FFocusedLinesUpdating) and (FFocusedLinesCount > 0) then
  begin
    FFocusedLinesCount := 0;
    for I := 0 to High(FFocusedLinesColors) do
      FFocusedLinesColors[I].Line := -1;

    Invalidate();
  end;
end;

procedure TSimbaEditor.FocusLine(Line, Column: Integer; AColor: TColor);
var
  I: Integer;
begin
  FFocusedLinesUpdating := True;
  try
    CaretX := Column;
    CaretY := Line;
    TopLine := Line - (LinesInWindow div 2);
    if CanSetFocus() then
      SetFocus();

    for I := 0 to High(FFocusedLinesColors) do
      if (FFocusedLinesColors[I].Line = -1) then
      begin
        FFocusedLinesColors[I].Line  := Line;
        FFocusedLinesColors[I].Color := AColor;

        Invalidate();
        Exit;
      end;

    SetLength(FFocusedLinesColors, Length(FFocusedLinesColors) + 1);

    FFocusedLinesColors[High(FFocusedLinesColors)].Line  := Line;
    FFocusedLinesColors[High(FFocusedLinesColors)].Color := AColor;
  finally
    FFocusedLinesCount    := FFocusedLinesCount + 1;
    FFocusedLinesUpdating := False;

    Invalidate();
  end;
end;

procedure TSimbaEditor.SetUpdateState(NewUpdating: Boolean; Sender: TObject);
begin
  inherited SetUpdateState(NewUpdating, Sender);

  if (not NewUpdating) and (FLastTextChangeStamp <> ChangeStamp) then
  begin
    ClearFocusedLines();
    if Assigned(FModifiedEvent) then
      FModifiedEvent(Self);

    FLastTextChangeStamp := ChangeStamp;
  end;
end;

function TSimbaEditor.GetFileName: String;
begin
  if Assigned(FGetFileNameEvent) then
    Result := FGetFileNameEvent(Self)
  else
    Result := '';
end;

function TSimbaEditor.GetCaretPos(GoBackToWord: Boolean): Integer;
var
  TheText: String;
begin
  TheText := TextBetweenPoints[TPoint.Create(1, 1), LogicalCaretXY];

  Result := Length(TheText);
  if GoBackToWord then
    while (Result > 1) and (Result <= Length(TheText)) and (TheText[Result] <= #32) do
      Dec(Result);
end;

procedure TSimbaEditor.SetColorModified(Value: TColor);
begin
  if (FColorModified = Value) then
    Exit;
  FColorModified := Value;

  Gutter.ChangesPart.ModifiedColor := FColorModified;
  ModifiedLinesGutter.Color := FColorModified;
end;

procedure TSimbaEditor.SetColorSaved(Value: TColor);
begin
  if (FColorSaved = Value) then
    Exit;
  FColorSaved := Value;

  Gutter.ChangesPart.SavedColor := FColorSaved;
  ModifiedLinesGutter.ColorSaved := FColorSaved;
end;

procedure TSimbaEditor.SetUseSimbaColors(Value: Boolean);
begin
  if (FUseSimbaColors = Value) then
    Exit;
  FUseSimbaColors := Value;

  if FUseSimbaColors then
    FAttributes.LoadFromFile(SimbaSettings.Editor.CustomColors.Value);
end;

procedure TSimbaEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Mouse link fix
  if FMarkupCtrlMouse.IsMouseOverLink then
    CaretXY := PixelsToRowColumn(Point(X, Y));

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaEditor.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FAutoComplete <> nil) then
    FAutoComplete.Form.Hide();
  if (FParamHint <> nil) then
    FParamHint.Form.Hide();
end;

procedure TSimbaEditor.MaybeReplaceModifiers;
begin
  if Application.HasOption('no-macos-commandkey') then
    Exit;

  ReplaceKeyStrokeModifiers(ssCtrl, ssMeta);
end;

procedure TSimbaEditor.DoSettingChanged_Colors(Setting: TSimbaSetting);
begin
  if FUseSimbaColors then
    FAttributes.LoadFromFile(Setting.Value);
end;

procedure TSimbaEditor.DoSettingChanged_AllowCaretPastEOL(Setting: TSimbaSetting);
begin
  if Setting.Value then
    Options := Options + [eoTrimTrailingSpaces, eoScrollPastEol]
  else
    Options := Options - [eoTrimTrailingSpaces, eoScrollPastEol];
end;

procedure TSimbaEditor.DoSettingChanged_RightMargin(Setting: TSimbaSetting);
begin
  RightEdge := Setting.Value;
end;

procedure TSimbaEditor.DoSettingChanged_RightMarginVisible(Setting: TSimbaSetting);
begin
  if Setting.Value then
    Options := Options - [eoHideRightMargin]
  else
    Options := Options + [eoHideRightMargin];
end;

procedure TSimbaEditor.DoSettingChanged_AntiAliased(Setting: TSimbaSetting);
begin
  FontAntialising := Setting.Value;
end;

procedure TSimbaEditor.DoSettingChanged_FontSize(Setting: TSimbaSetting);
begin
  Font.Size := Setting.Value;
end;

procedure TSimbaEditor.DoSettingChanged_FontName(Setting: TSimbaSetting);
begin
  if IsFontFixed(Setting.Value) then
    Font.Name := Setting.Value;
end;

procedure TSimbaEditor.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);

  if (Source is TTreeView) and (TTreeView(Source).Selected <> nil) then
    TextBetweenPoints[PixelsToRowColumn(TPoint.Create(X, Y)),
                      PixelsToRowColumn(TPoint.Create(X, Y))] := TTreeView(Source).Selected.Text;
end;

procedure TSimbaEditor.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TTreeView;
  if not Accept then
    inherited DragOver(Source, X, Y, State, Accept);
end;

procedure TSimbaEditor.DoSpecialLineColor(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
var
  I: Integer;
begin
  Special := False;
  if (FFocusedLinesCount = 0) then
    Exit;

  for I := 0 to High(FFocusedLinesColors) do
    if (FFocusedLinesColors[I].Line = Line) then
    begin
      AMarkup.BackAlpha  := 128;
      AMarkup.Background := FFocusedLinesColors[I].Color;
      AMarkup.Foreground := clNone;

      Special := True;
      Exit;
    end;
end;

procedure TSimbaEditor.StatusChanged(AChanges: TSynStatusChanges);
begin
  inherited StatusChanged(AChanges);

  if (scSelection in AChanges) then
    FMarkupHighlightAllCaret.Enabled := SelAvail;
end;

function TSimbaEditor.GetExpression(X, Y: Integer): String;
var
  Line: String;
  InRound, InSquare: Integer;
begin
  Result := '';

  Y := Y - 1;
  if (Y < 0) or (Y >= TextView.Count) then
    Exit;
  Line := TextView[Y];
  if (X < 1) or (X > Length(Line)) then
    Exit;

  InRound := 0;
  InSquare := 0;

  for X := X downto 1 do
  begin
    case Line[X] of
      ')': Inc(InRound);
      ']': Inc(InSquare);
      '(':
        begin
          Dec(InRound);
          if (InRound < 0) then
            Break;
        end;
      '[':
        begin
          Dec(InSquare);
          if (InSquare < 0) then
            Break;
        end;

      else
      begin
        if (Line[X] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) then
          // in identifier
        else
        if (InRound <= 0) and (InSquare <= 0) then
          Break;
      end;
    end;

    Result := Line[X] + Result;
  end;
end;

function TSimbaEditor.GetExpressionEx(X, Y: Integer): String;
var
  StartX, EndX: Integer;
begin
  GetWordBoundsAtRowCol(TPoint.Create(X, Y), StartX, EndX);

  Result := GetExpression(EndX - 1, Y);
end;

constructor TSimbaEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMarkupHighlightAllCaret := MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret;
  FMarkupCtrlMouse := MarkupByClass[TSynEditMarkupCtrlMouseLink] as TSynEditMarkupCtrlMouseLink;

  Options := Options + [eoTabIndent, eoKeepCaretX, eoDragDropEditing, eoScrollPastEof] - [eoSmartTabs];
  Options2 := Options2 + [eoCaretSkipsSelection];

  MouseOptions := [emAltSetsColumnMode, emUseMouseActions, emShowCtrlMouseLinks, emCtrlWheelZoom];
  ResetMouseActions();

  TabWidth := 2;
  BlockIndent := 2;

  OnSpecialLineMarkup := @DoSpecialLineColor;

  MouseActions.AddCommand(emcOverViewGutterScrollTo, False, LazSynEditMouseCmdsTypes.mbLeft, ccSingle, cdDown, [], []);

  Highlighter := TSynFreePascalSyn.Create(Self);

  FScreenCaretPainterClass := TSynEditScreenCaretPainterInternal;
  if (FScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterInternal) then
    FScreenCaret.ChangePainter(TSynEditScreenCaretPainterInternal);

  FParamHint := TSimbaParamHint.Create(nil);
  FParamHint.Editor := Self;

  FAutoComplete := TSimbaAutoComplete.Create(nil);
  FAutoComplete.Editor := Self;
  FAutoComplete.AutoUseSingleIdent := False;
  FAutoComplete.ExecCommandID := ecNone;
  FAutoComplete.ShowSizeDrag := True;

  with TSynEditMarkupHighlightAllCaret(MarkupByClass[TSynEditMarkupHighlightAllCaret]) do
  begin
    Enabled := False;
    WaitTime := 1;
    IgnoreKeywords := True;
  end;

  with TSynGutterLineOverview.Create(RightGutter.Parts) do
  begin
    FModifiedLinesGutter := TSimbaEditorModifiedLinesGutter.Create(Providers);
    FModifiedLinesGutter.Priority := 1;
    FModifiedLinesGutter.Color := Gutter.ChangesPart.ModifiedColor;
    FModifiedLinesGutter.ColorSaved := Gutter.ChangesPart.SavedColor;

    AutoSize := False;
    Width := Scale96ToScreen(6);
    //TSynGutterLOvProviderCurrentPage.Create(Providers);
  end;

  TSimbaEditorPlugin_MultiCaret.Create(Self);
  TSimbaEditorPlugin_BlockCompletion.Create(Self);
  TSimbaEditorPlugin_DocGenerator.Create(Self);
  TSimbaEditorPlugin_CommentBlock.Create(Self);
  TSimbaEditorPlugin_MouseWheelZoom.Create(Self);

  Keystrokes.Delete(KeyStrokes.FindCommand(ecInsertLine));
  Keystrokes.Delete(KeyStrokes.FindCommand(ecNormalSelect));
  Keystrokes.Delete(KeyStrokes.FindCommand(ecColumnSelect));

  FAttributes := TSimbaEditor_Attributes.Create(Self);

  Gutter.LeftOffset := Scale96ToScreen(12);
  Gutter.MarksPart.Visible := False;
  Gutter.SeparatorPart.Visible := False;

  with TSynGutterSeparator.Create(Gutter.Parts) do
  begin
    MarkupInfo.Foreground := Gutter.Color;
    AutoSize := False;
    LineWidth := Scale96ToScreen(6);
    Width := Scale96ToScreen(6);
    Index := Gutter.LineNumberPart().Index + 1;
  end;

  {$IFDEF DARWIN}
  MaybeReplaceModifiers();
  {$ENDIF}

  with SimbaSettings do
  begin
    RegisterChangeHandler(Self, Editor.CustomColors, @DoSettingChanged_Colors);
    RegisterChangeHandler(Self, Editor.AllowCaretPastEOL, @DoSettingChanged_AllowCaretPastEOL, True);
    RegisterChangeHandler(Self, Editor.RightMargin, @DoSettingChanged_RightMargin, True);
    RegisterChangeHandler(Self, Editor.RightMarginVisible, @DoSettingChanged_RightMarginVisible, True);
    RegisterChangeHandler(Self, Editor.AntiAliased, @DoSettingChanged_AntiAliased, True);
    RegisterChangeHandler(Self, Editor.FontSize, @DoSettingChanged_FontSize, True);
    RegisterChangeHandler(Self, Editor.FontName, @DoSettingChanged_FontName, True);
  end;
end;

destructor TSimbaEditor.Destroy;
begin
  if (FAttributes <> nil) then
    FreeAndNil(FAttributes);

  inherited Destroy();
end;

end.


