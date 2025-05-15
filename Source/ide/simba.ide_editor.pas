{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, ComCtrls, LCLType,
  SynEdit, SynEditTypes, SynGutterLineOverview, SynEditMouseCmds, SynEditMiscClasses, SynEditKeyCmds, SynEditHighlighter, SynEditMarkupCtrlMouseLink, SynEditMarkupHighAll,
  simba.base, simba.settings,
  simba.ide_editor_completionbox, simba.ide_editor_paramhint, simba.ide_editor_attributes,
  simba.ide_editor_modifiedlinegutter, simba.component_synedit;

type
  TSimbaEditorFileNameEvent = function(Sender: TObject): String of object;

  ESimbaEditorOptions = set of (
    seoColors,      // load colors from settings
    seoKeybindings  // load keybindings from settings
  );

  TSimbaEditor = class(TSimbaSynEdit)
  protected
    FMarkupHighlightAllCaret: TSynEditMarkupHighlightAllCaret;
    FMarkupCtrlMouse: TSynEditMarkupCtrlMouseLink;

    FCompletionBox: TSimbaCompletionBox;
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

    FSimbaOptions: ESimbaEditorOptions;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    procedure FontChanged(Sender: TObject); override;

    procedure DoSettingChanged_Colors(Setting: TSimbaSetting);
    procedure DoSettingChanged_Keystrokes(Setting: TSimbaSetting);
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
  public
    property TextView;
    property CompletionBox: TSimbaCompletionBox read FCompletionBox;
    property ParamHint: TSimbaParamHint read FParamHint;
    property ModifiedLinesGutter: TSimbaEditorModifiedLinesGutter read FModifiedLinesGutter;
    property Attributes: TSimbaEditor_Attributes read FAttributes;

    property OnModified: TNotifyEvent read FModifiedEvent write FModifiedEvent;
    property OnGetFileName: TSimbaEditorFileNameEvent read FGetFileNameEvent write FGetFileNameEvent;

    property ColorSaved: TColor read FColorSaved write SetColorSaved;
    property ColorModified: TColor read FColorModified write SetColorModified;

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

    constructor Create(AOwner: TComponent; SimbaOptions: ESimbaEditorOptions); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  LazSynEditMouseCmdsTypes,
  SynHighlighterPas,
  SynEditPointClasses,
  SynGutter,
  simba.misc,
  simba.vartype_string,
  simba.ide_editor_blockcompletion,
  simba.ide_editor_docgenerator,
  simba.ide_editor_commentblock,
  simba.ide_editor_mousewheelzoom,
  simba.ide_editor_multicaret,
  simba.ide_editor_codecomplete,
  simba.ide_editor_commands;

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

procedure TSimbaEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Mouse link fix
  if FMarkupCtrlMouse.IsMouseOverLink then
    CaretXY := PixelsToRowColumn(Point(X, Y));

  inherited MouseDown(Button, Shift, X, Y);
end;

function TSimbaEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  SCROLL_AMOUNT = 5;
begin
  if (ssShift in Shift) then
  begin
    if (WheelDelta > 0) then
      FScrollbarHorz.Position := FScrollbarHorz.Position - SCROLL_AMOUNT
    else
      FScrollbarHorz.Position := FScrollbarHorz.Position + SCROLL_AMOUNT;

    Result := True;
  end else
    Result := inherited;
end;

procedure TSimbaEditor.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FCompletionBox <> nil) then
    FCompletionBox.Form.Hide();
  if (FParamHint <> nil) then
    FParamHint.Form.Hide();
end;

procedure TSimbaEditor.DoSettingChanged_Colors(Setting: TSimbaSetting);
begin
  if (seoColors in FSimbaOptions) then
    FAttributes.LoadFromFile(Setting.Value);
end;

procedure TSimbaEditor.DoSettingChanged_Keystrokes(Setting: TSimbaSetting);
begin
  SimbaLoadKeystrokes(Keystrokes, seoKeybindings in FSimbaOptions);
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
  I: Integer;
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

  for I := X downto 1 do
  begin
    case Line[I] of
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
        if (Line[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '^']) then
          // in identifier
        else
        if (InRound <= 0) and (InSquare <= 0) then
          Break;
      end;
    end;
  end;

  Result := Copy(Line, I+1, X-I);
end;

function TSimbaEditor.GetExpressionEx(X, Y: Integer): String;
var
  StartX, EndX: Integer;
begin
  GetWordBoundsAtRowCol(TPoint.Create(X, Y), StartX, EndX);

  Result := GetExpression(EndX - 1, Y);
end;

constructor TSimbaEditor.Create(AOwner: TComponent; SimbaOptions: ESimbaEditorOptions);
begin
  inherited Create(AOwner);

  FSimbaOptions := SimbaOptions;

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

  FScreenCaretPainterClass {%H-}:= TSynEditScreenCaretPainterInternal;
  if (FScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterInternal) then
    FScreenCaret.ChangePainter(TSynEditScreenCaretPainterInternal);

  FParamHint := TSimbaParamHint.Create(nil);
  FParamHint.Editor := Self;

  FCompletionBox := TSimbaCompletionBox.Create(nil);
  FCompletionBox.Editor := Self;
  FCompletionBox.AutoUseSingleIdent := False;
  FCompletionBox.ExecCommandID := ecNone;
  FCompletionBox.ShowSizeDrag := True;

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
  TSimbaCodeComplete.Create(Self);

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

  with SimbaSettings do
  begin
    RegisterChangeHandler(Self, Editor.CustomColors,       @DoSettingChanged_Colors,             True);
    RegisterChangeHandler(Self, Editor.Keystrokes,         @DoSettingChanged_Keystrokes,         True);
    RegisterChangeHandler(Self, Editor.AllowCaretPastEOL,  @DoSettingChanged_AllowCaretPastEOL,  True);
    RegisterChangeHandler(Self, Editor.RightMargin,        @DoSettingChanged_RightMargin,        True);
    RegisterChangeHandler(Self, Editor.RightMarginVisible, @DoSettingChanged_RightMarginVisible, True);
    RegisterChangeHandler(Self, Editor.AntiAliased,        @DoSettingChanged_AntiAliased,        True);
    RegisterChangeHandler(Self, Editor.FontSize,           @DoSettingChanged_FontSize,           True);
    RegisterChangeHandler(Self, Editor.FontName,           @DoSettingChanged_FontName,           True);
  end;
end;

destructor TSimbaEditor.Destroy;
begin
  if (FAttributes <> nil) then
    FreeAndNil(FAttributes);

  inherited Destroy();
end;

end.


