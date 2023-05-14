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
  SynEdit, SynEditTypes, SynGutterLineOverview, SynEditMouseCmds, SynEditMiscClasses,
  SynEditKeyCmds, SynEditHighlighter, SynHighlighterPas_Simba, SynEditMarkupHighAll,
  LazSynEditMouseCmdsTypes, LazMethodList,
  simba.mufasatypes, simba.settings, simba.editor_autocomplete, simba.editor_paramhint,
  simba.editor_attributes, simba.editor_modifiedlinegutter, simba.component_synedit;

type
  TSimbaEditor = class(TSimbaSynEdit)
  protected
    FAutoComplete: TSimbaAutoComplete;
    FParamHint: TSimbaParamHint;

    FAttributes: TSimbaEditor_Attributes;
    FModifiedLinesGutter: TSimbaEditorModifiedLinesGutter;
    FFontChangedHandlerList: TMethodList;

    FFocusedLinesUpdating: Boolean;
    FFocusedLinesCount: Integer;
    FFocusedLinesColors: array of record
      Line: Integer;
      Color: TColor;
    end;

    FColorModified: TColor;
    FColorSaved: TColor;

    procedure FontChanged(Sender: TObject); override;
    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

    // Temp line coloring
    procedure DoSpecialLineColor(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    // Enable/Disable TSynEditMarkupHighlightAllCaret depending on has selection
    procedure DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);

    procedure SetColorModified(Value: TColor);
    procedure SetColorSaved(Value: TColor);
  public
    FileName: String;

    property TextView;
    property AutoComplete: TSimbaAutoComplete read FAutoComplete;
    property ParamHint: TSimbaParamHint read FParamHint;
    property ModifiedLinesGutter: TSimbaEditorModifiedLinesGutter read FModifiedLinesGutter;
    property Attributes: TSimbaEditor_Attributes read FAttributes;

    property ColorSaved: TColor read FColorSaved write SetColorSaved;
    property ColorModified: TColor read FColorModified write SetColorModified;

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

    procedure RegisterFontChangedHandler(Handler: TNotifyEvent);
    procedure UnRegisterFontChangedHandler(Handler: TNotifyEvent);
    procedure RegisterCaretMoveHandler(Handler: TNotifyEvent);
    procedure UnRegisterCaretMoveHandler(Handler: TNotifyEvent);

    procedure ClearFocusedLines;
    procedure FocusLine(Line, Column: Integer; AColor: TColor);

    constructor Create(AOwner: TComponent; LoadColors: Boolean = True); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SynEditPointClasses, SynGutterBase, SynEditMarkupWordGroup,
  simba.fonthelpers, simba.editor_blockcompletion,
  simba.editor_docgenerator, simba.editor_commentblock,
  simba.editor_mousewheelzoom, simba.editor_multicaret,
  simba.editor_popupmenu, simba.theme;

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

procedure TSimbaEditor.RegisterFontChangedHandler(Handler: TNotifyEvent);
begin
  FFontChangedHandlerList.Add(TMethod(Handler));
end;

procedure TSimbaEditor.UnRegisterFontChangedHandler(Handler: TNotifyEvent);
begin
  FFontChangedHandlerList.Remove(TMethod(Handler));
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

procedure TSimbaEditor.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source is TTreeView) and (TTreeView(Source).Selected <> nil) then
    TextBetweenPoints[PixelsToRowColumn(TPoint.Create(X, Y)),
                      PixelsToRowColumn(TPoint.Create(X, Y))] := TTreeView(Source).Selected.Text;
end;

procedure TSimbaEditor.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TTreeView;
end;

procedure TSimbaEditor.DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  ClearFocusedLines();

  if (scSelection in Changes) then
    MarkupByClass[TSynEditMarkupHighlightAllCaret].Enabled := SelAvail;
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

procedure TSimbaEditor.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  FFontChangedHandlerList.CallNotifyEvents(Self);
end;

procedure TSimbaEditor.SimbaSettingChanged(Setting: TSimbaSetting);
var
  I: Integer;
begin
  case Setting.Name of
    'Editor.FontSize':
      begin
        Font.Size := Setting.Value;
      end;

    'Editor.FontName':
      begin
        if IsFontFixed(Setting.Value) then
          Font.Name := Setting.Value;
      end;

    'Editor.CustomColors':
      begin
        FAttributes.LoadFromFile(Setting.Value);
      end;

    'Editor.RightMargin':
      begin
        RightEdge := Setting.Value;
      end;

    'Editor.RightMarginVisible':
      begin
        if Setting.Value then
          Options := Options - [eoHideRightMargin]
        else
          Options := Options + [eoHideRightMargin];
      end;

    'Editor.AntiAliased':
      begin
        if Setting.Value then
          Font.Quality := fqCleartypeNatural
        else
          Font.Quality := fqNonAntialiased;
      end;

    'Editor.AllowCaretPastEOL':
      begin
        if Setting.Value then
          Options := Options + [eoTrimTrailingSpaces, eoScrollPastEol]
        else
          Options := Options - [eoTrimTrailingSpaces, eoScrollPastEol];
      end;

    'General.MacOSKeystrokes':
      begin
        if Setting.Value then
        begin
          for I := 0 to Keystrokes.Count - 1 do
            if (ssCtrl in Keystrokes[i].Shift) then
              Keystrokes[I].Shift := Keystrokes[I].Shift - [ssCtrl] + [ssMeta];

          for I := 0 to MouseActions.Count - 1 do
            if (ssCtrl in MouseActions[i].Shift) then
              MouseActions[I].Shift := MouseActions[I].Shift - [ssCtrl] + [ssMeta];
        end else
        begin
          for I := 0 to Keystrokes.Count - 1 do
            if (ssMeta in Keystrokes[i].Shift) then
              Keystrokes[I].Shift := Keystrokes[I].Shift - [ssMeta] + [ssCtrl];

          for I := 0 to MouseActions.Count - 1 do
            if (ssMeta in MouseActions[i].Shift) then
              MouseActions[I].Shift := MouseActions[I].Shift - [ssMeta] + [ssCtrl];
        end;
      end;
  end;
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

constructor TSimbaEditor.Create(AOwner: TComponent; LoadColors: Boolean);
var
  I: Integer;
begin
  inherited Create(AOwner);

  BorderStyle := bsNone;

  OnDragDrop := @DoDragDrop;
  OnDragOver := @DoDragOver;

  FFontChangedHandlerList := TMethodList.Create();

  PopupMenu := GetSimbaEditorPopupMenu();

  Options := Options + [eoTabIndent, eoKeepCaretX, eoDragDropEditing, eoScrollPastEof] - [eoSmartTabs];
  Options2 := Options2 + [eoCaretSkipsSelection];

  MouseOptions := [emAltSetsColumnMode, emUseMouseActions, emShowCtrlMouseLinks, emCtrlWheelZoom];
  ResetMouseActions();

  TabWidth := 2;
  BlockIndent := 2;

  OnSpecialLineMarkup := @DoSpecialLineColor;

  MouseActions.AddCommand(emcOverViewGutterScrollTo, False, LazSynEditMouseCmdsTypes.mbLeft, ccSingle, cdDown, [], []);

  RegisterStatusChangedHandler(@DoStatusChanged, [scCaretX, scCaretY, scModified, scSelection]);

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

  MarkupByClass[TSynEditMarkupWordGroup].Enabled := False;

  with TSynGutterLineOverview.Create(RightGutter.Parts) do
  begin
    FModifiedLinesGutter := TSimbaEditorModifiedLinesGutter.Create(Providers);
    FModifiedLinesGutter.Priority := 1;
    FModifiedLinesGutter.Color := Gutter.ChangesPart.ModifiedColor;
    FModifiedLinesGutter.ColorSaved := Gutter.ChangesPart.SavedColor;

    AutoSize := False;
    Width := Scale96ToScreen(4);
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

  Gutter.LeftOffset := Scale96ToScreen(10);
  for I := 0 to Gutter.Parts.Count - 1 do
    Gutter.Parts[I].Visible := False;
  Gutter.ChangesPart().Visible    := True;
  Gutter.LineNumberPart().Visible := True;
  Gutter.CodeFoldPart().Visible   := True;

  //if LoadColors then
  //  SimbaSettingChanged(SimbaSettings.Editor.CustomColors);
  SimbaSettingChanged(SimbaSettings.Editor.AllowCaretPastEOL);
  SimbaSettingChanged(SimbaSettings.Editor.RightMarginVisible);
  SimbaSettingChanged(SimbaSettings.Editor.AntiAliased);
  SimbaSettingChanged(SimbaSettings.Editor.FontSize);
  SimbaSettingChanged(SimbaSettings.Editor.FontName);
  SimbaSettingChanged(SimbaSettings.General.MacOSKeystrokes);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
end;

destructor TSimbaEditor.Destroy;
begin
  if (SimbaSettings <> nil) then
    SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  if (FAttributes <> nil) then
    FreeAndNil(FAttributes);
  if (FFontChangedHandlerList <> nil) then
    FreeAndNil(FFontChangedHandlerList);

  inherited Destroy();
end;

end.

