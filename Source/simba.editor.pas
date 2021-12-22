{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor;

{$i simba.inc}

interface

uses
  Classes, SysUtils, LazSynEditMouseCmdsTypes, Graphics, Controls, LCLType, fgl,
  SynEdit, SynGutterLineOverview, SynEditMouseCmds,
  SynEditKeyCmds, SynEditHighlighter, SynPluginMultiCaret,
  simba.highlighter, simba.autocomplete, simba.parameterhint, simba.editor_attributes, simba.settings;

const
  ecAutoComplete      = ecUserFirst + 1;
  ecAutoCompleteChar  = ecUserFirst + 2;
  ecParameterHint     = ecUserFirst + 3;
  ecParameterHintChar = ecUserFirst + 4;
  ecCommentBlock      = ecUserFirst + 5;
  ecDocumentation     = ecUserFirst + 6;

  emcJumpForward      = TSynEditorMouseCommand(emcPluginFirst + 1);
  emcJumpBack         = TSynEditorMouseCommand(emcPluginFirst + 2);

type
  TSimbaEditor_LineMark = record
    Line: Integer;
    Color: Integer;

    class operator <> (const Left, Right: TSimbaEditor_LineMark): Boolean;
  end;

  TSimbaEditor_ModifiedLinesGutter = class(TSynGutterLOvProviderModifiedLines)
  protected
  type
    TLineMarkList = specialize TFPGList<TSimbaEditor_LineMark>;
  protected
    FLineMarks: TLineMarkList;
    FPaintMarks: Boolean;

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HideMarks;
    procedure ShowMarks;

    procedure ClearMarks;

    procedure AddMark(ALine, AColor: Integer);
  end;

  TSimbaEditor = class(TSynEdit)
  protected
    FAttributes: TSimbaEditor_Attributes;
    FParameterHint: TSimbaParameterHint;
    FAutoComplete: TSimbaAutoComplete;
    FMultiCaret: TSynPluginMultiCaret;
    FModifiedLinesGutter: TSimbaEditor_ModifiedLinesGutter;

    procedure CommentBlock;

    procedure HandleCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
    procedure HandleZoomInOut(Data: PtrInt);
    function HandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;

    procedure SetMacOSKeystrokes(Value: Boolean);

    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    function GetExpressionAt(X, Y: Integer): String;
  public
    property TextView;
    property ModifiedLinesGutter: TSimbaEditor_ModifiedLinesGutter read FModifiedLinesGutter;
    property AutoComplete: TSimbaAutoComplete read FAutoComplete;
    property ParameterHint: TSimbaParameterHint read FParameterHint;

    property Attributes: TSimbaEditor_Attributes read FAttributes;
    property Expression[X, Y: Integer]: String read GetExpressionAt;

    procedure InsertDocumentation;

    constructor Create(AOwner: TComponent; LoadColors: Boolean = True); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SynEditMarkupHighAll, dialogs, math, menus, SynGutter, SynEditTypes, LazLoggerBase,
  simba.scripttabhistory, Forms, simba.fonthelpers, simba.parser_misc, simba.codeparser;

type
  TSimbaEditor_GutterSeparator = class(TSynGutterSeparator)
  protected
    FWidth: Integer;

    function PreferedWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent; AWidth, APosition: Integer); reintroduce;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
  end;

class operator TSimbaEditor_LineMark.<>(const Left, Right: TSimbaEditor_LineMark): Boolean;
begin
  Result := (Left.Color <> Right.Color) and (Left.Line <> Right.Line);
end;

function TSimbaEditor_GutterSeparator.PreferedWidth: Integer;
begin
  Result := FWidth;
end;

procedure TSimbaEditor_GutterSeparator.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
begin
  Canvas.Brush.Color := Gutter.Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(AClip);
end;

constructor TSimbaEditor_GutterSeparator.Create(AOwner: TComponent; AWidth, APosition: Integer);
begin
  inherited Create(AOwner);

  FWidth := AWidth;
  GutterParts.Move(Index, APosition);
end;

procedure TSimbaEditor_ModifiedLinesGutter.Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer);
var
  I, Y1, Y2: Integer;
begin
  inherited Paint(Canvas, AClip, TopOffset);

  if FPaintMarks then
  begin
    for I := 0 to FLineMarks.Count - 1 do
      with FLineMarks[I] do
      begin
        Y1 := TextLineToPixel(Line) - 2;
        Y2 := TextLineToPixelEnd(Line) + 2;

        Canvas.Brush.Color := Color;
        Canvas.FillRect(AClip.Left, Y1, AClip.Right, Y2);
      end;
  end;
end;

constructor TSimbaEditor_ModifiedLinesGutter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLineMarks := TLineMarkList.Create();
end;

destructor TSimbaEditor_ModifiedLinesGutter.Destroy;
begin
  if (FLineMarks <> nil) then
    FreeAndNil(FLineMarks);

  inherited Destroy();
end;

procedure TSimbaEditor_ModifiedLinesGutter.HideMarks;
begin
  FPaintMarks := False;

  if (SynEdit is TSimbaEditor) then
    TSimbaEditor(SynEdit).InvalidateGutter();
end;

procedure TSimbaEditor_ModifiedLinesGutter.ShowMarks;
begin
  FPaintMarks := True;

  if (SynEdit is TSimbaEditor) then
    TSimbaEditor(SynEdit).InvalidateGutter();
end;

procedure TSimbaEditor_ModifiedLinesGutter.AddMark(ALine, AColor: Integer);
var
  LineMark: TSimbaEditor_LineMark;
begin
  LineMark.Line := ALine;
  LineMark.Color := TColor(ColorToRGB(AColor));

  FLineMarks.Add(LineMark);
end;

procedure TSimbaEditor_ModifiedLinesGutter.ClearMarks;
begin
  FLineMarks.Clear();
end;

procedure TSimbaEditor.InsertDocumentation;
const
  DOC_TEMPLATE =
    '(*'             + LineEnding +
    '%s'             + LineEnding +
    '%s'             + LineEnding +
    '.. pascal:: %s' + LineEnding +
    ''               + LineEnding +
    'DESC'           + LineEnding +
    ''               + LineEnding +
    'Example'        + LineEnding +
    '-------'        + LineEnding +
    ''               + LineEnding +
    '  EXAMPLE'      + LineEnding +
    '*)'             + LineEnding;
var
  Parser: TCodeParser;
  Decl: TDeclaration;
  FullName, Header: String;
begin
  Parser := TCodeParser.Create();
  try
    Parser.Run(Text, '');

    Decl := Parser.Items.GetItemInPosition(SelStart - 1);
    if (Decl <> nil) and Decl.HasOwnerClass(TciProcedureDeclaration, Decl, True) then
    begin
      CaretXY := CharIndexToRowCol(Decl.StartPos);

      Header := TciProcedureDeclaration(Decl).Header;
      if TciProcedureDeclaration(Decl).IsMethodOfType then
        FullName := TciProcedureDeclaration(Decl).ObjectName + '.' + TciProcedureDeclaration(Decl).Name
      else
        FullName := TciProcedureDeclaration(Decl).Name;

      InsertTextAtCaret(Format(DOC_TEMPLATE, [FullName, StringOfChar('~', Length(FullName)), Header]));
    end;
  except
  end;

  if (Parser <> nil) then
    Parser.Free();
end;

// From Lazarus source
procedure TSimbaEditor.CommentBlock;
var
  OldCaretPos, OldBlockStart, OldBlockEnd: TPoint;
  WasSelAvail: Boolean;
  WasSelMode: TSynSelectionMode;
  BlockBeginLine: Integer;
  BlockEndLine: Integer;
  CommonIndent: Integer;

  function FirstNonBlankPos(const Text: String; Start: Integer = 1): Integer;
  var
    i: Integer;
  begin
    for i := Start to Length(Text) do
      if (Text[i] <> #32) and (Text[i] <> #9) then
        Exit(i);
    Result := -1;
  end;

  function MinCommonIndent: Integer;
  var
    i, j: Integer;
  begin
    If CommonIndent = 0 then begin
      CommonIndent := Max(FirstNonBlankPos(Lines[BlockBeginLine - 1]), 1);
      for i := BlockBeginLine + 1 to BlockEndLine do begin
        j := FirstNonBlankPos(Lines[i - 1]);
        if (j < CommonIndent) and (j > 0) then
          CommonIndent := j;
      end;
    end;
    Result := CommonIndent;
  end;

  function InsertPos(ALine: Integer): Integer;
  begin
    if not WasSelAvail then
      Result := MinCommonIndent
    else case WasSelMode of
      smColumn: // CommonIndent is not used otherwise
        begin
          if CommonIndent = 0 then
            CommonIndent := Min(LogicalToPhysicalPos(OldBlockStart).X, LogicalToPhysicalPos(OldBlockEnd).X);
          Result := PhysicalToLogicalPos(Point(CommonIndent, ALine)).X;
        end;
      smNormal:
        begin
          Result := MinCommonIndent;
        end;
       else
         Result := 1;
    end;
  end;

  function DeletePos(ALine: Integer): Integer;
  var
    Line: String;
  begin
    Line := Lines[ALine - 1];
    Result := FirstNonBlankPos(Line, InsertPos(ALine));
    if (WasSelMode = smColumn) and((Result < 1) or (Result > length(Line) - 1)) then
      Result := length(Line) - 1;
    Result := Max(1, Result);
    if (Length(Line) < Result +1) or
       (Line[Result] <> '/') or (Line[Result+1] <> '/') then
      Result := -1;
  end;

var
  i: Integer;
  NonBlankStart: Integer;
  CommentOn: Boolean;
begin
  if ReadOnly then
    Exit;

  OldCaretPos   := CaretXY;
  OldBlockStart := BlockBegin;
  OldBlockEnd   := BlockEnd;
  WasSelAvail := SelAvail;
  WasSelMode  := SelectionMode;
  CommonIndent := 0;

  BlockBeginLine := OldBlockStart.Y;
  BlockEndLine := OldBlockEnd.Y;
  if (OldBlockEnd.X = 1) and (BlockEndLine > BlockBeginLine) and (SelectionMode <> smLine) then
    Dec(BlockEndLine);

  CommentOn := False;
  for i := BlockBeginLine to BlockEndLine do
    if DeletePos(i) < 0 then
    begin
      CommentOn := True;
      Break;
    end;

  BeginUpdate();
  BeginUndoBlock();

  SelectionMode := smNormal;

  if CommentOn then
  begin
    for i := BlockEndLine downto BlockBeginLine do
      TextBetweenPoints[Point(InsertPos(i), i), Point(InsertPos(i), i)] := '//';
    if OldCaretPos.X > InsertPos(OldCaretPos.Y) then
      OldCaretPos.x := OldCaretPos.X + 2;
    if OldBlockStart.X > InsertPos(OldBlockStart.Y) then
      OldBlockStart.X := OldBlockStart.X + 2;
    if OldBlockEnd.X > InsertPos(OldBlockEnd.Y) then
      OldBlockEnd.X := OldBlockEnd.X + 2;
  end else
  begin
    for i := BlockEndLine downto BlockBeginLine do
    begin
      NonBlankStart := DeletePos(i);
      if NonBlankStart < 1 then Continue;
      TextBetweenPoints[Point(NonBlankStart, i), Point(NonBlankStart + 2, i)] := '';
      if (OldCaretPos.Y = i) and (OldCaretPos.X > NonBlankStart) then
        OldCaretPos.x := Max(OldCaretPos.X - 2, NonBlankStart);
      if (OldBlockStart.Y = i) and (OldBlockStart.X > NonBlankStart) then
        OldBlockStart.X := Max(OldBlockStart.X - 2, NonBlankStart);
      if (OldBlockEnd.Y = i) and (OldBlockEnd.X > NonBlankStart) then
        OldBlockEnd.X := Max(OldBlockEnd.X - 2, NonBlankStart);
    end;
  end;

  EndUndoBlock();
  EndUpdate();

  CaretXY := OldCaretPos;
  BlockBegin := OldBlockStart;
  BlockEnd := OldBlockEnd;
  SelectionMode := WasSelMode;
end;

procedure TSimbaEditor.HandleCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
begin
  case Command of
    ecDocumentation: InsertDocumentation();
    ecCommentBlock: CommentBlock();
    ecChar:
      case AChar of
        '(': if SimbaSettings.Editor.AutomaticallyShowParameterHints.Value then
               CommandProcessor(ecParameterHintChar, AChar, Data);
        '.': if SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value then
               CommandProcessor(ecAutoCompleteChar, AChar, Data);
      end;
  end;
end;

procedure TSimbaEditor.HandleZoomInOut(Data: PtrInt);
begin
  SimbaSettings.Editor.FontSize.Value := Font.Size;
end;

procedure TSimbaEditor.SetMacOSKeystrokes(Value: Boolean);
var
  Find: TShiftStateEnum;
  Replace: TShiftStateEnum;
  I: Integer;
begin
  if Value then Find := ssCtrl else Find := ssMeta;
  if Value then Replace := ssMeta else Replace := ssCtrl;

  for I := 0 to Keystrokes.Count - 1 do
    if (Find in Keystrokes[i].Shift) then
      Keystrokes[I].Shift := Keystrokes[I].Shift - [Find] + [Replace];

  for I := 0 to MouseActions.Count - 1 do
    if (Find in MouseActions[i].Shift) then
      MouseActions[I].Shift := MouseActions[I].Shift - [Find] + [Replace];
end;

function TSimbaEditor.HandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;

  case AnAction.Command of
    emcWheelZoomIn, emcWheelZoomOut:
      Application.QueueAsyncCall(@HandleZoomInOut, 0);

    emcJumpForward: SimbaScriptTabHistory.GoForward();
    emcJumpBack: SimbaScriptTabHistory.GoBack();
  end;
end;

procedure TSimbaEditor.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.Editor.FontSize) then
    Font.Size := Setting.Value;

  if (Setting = SimbaSettings.Editor.FontName) then
  begin
    if SimbaFontHelpers.IsFontFixed(Setting.Value) then
      Font.Name := Setting.Value;
  end;

  if (Setting = SimbaSettings.Editor.AntiAliased) then
  begin
    if Setting.Value then
      Font.Quality := fqCleartypeNatural
    else
      Font.Quality := fqNonAntialiased;
  end;

  if (Setting = SimbaSettings.Editor.CustomColors) then
    FAttributes.LoadFromFile(Setting.Value);

  if (Setting = SimbaSettings.Editor.RightMarginVisible) then
  begin
    if Setting.Value then
      Options := Options - [eoHideRightMargin]
    else
      Options := Options + [eoHideRightMargin];
  end;

  if (Setting = SimbaSettings.Editor.AllowCaretPastEOL) then
  begin
    if Setting.Value then
      Options := Options + [eoTrimTrailingSpaces, eoScrollPastEol]
    else
      Options := Options - [eoTrimTrailingSpaces, eoScrollPastEol];
  end;

  if (Setting = SimbaSettings.Editor.RightMargin) then
  begin
    RightEdge := Setting.Value;
  end;

  if (Setting = SimbaSettings.GUI.MacOSKeystrokes) then
    SetMacOSKeystrokes(Setting.Value);
end;

function TSimbaEditor.GetExpressionAt(X, Y: Integer): String;
var
  StartX, EndX: Integer;
begin
  GetWordBoundsAtRowCol(TPoint.Create(X, Y), StartX, EndX);

  Result := GetExpression(Text, RowColToCharIndex(TPoint.Create(EndX - 1, Y)));
end;

constructor TSimbaEditor.Create(AOwner: TComponent; LoadColors: Boolean);
begin
  inherited Create(AOwner);

  MouseOptions := [emAltSetsColumnMode, emUseMouseActions, emShowCtrlMouseLinks, emCtrlWheelZoom];
  ResetMouseActions();

  Options := Options + [eoTabIndent, eoKeepCaretX, eoDragDropEditing, eoScrollPastEof] - [eoSmartTabs, eoHideRightMargin];
  Options2 := Options2 + [eoCaretSkipsSelection];

  TabWidth := 2;
  BlockIndent := 2;

  Highlighter := TSynPasSyn.Create(Self);
  with Highlighter as TSynPasSyn do
  begin
    CommentAttri.Foreground := clBlue;
    CommentAttri.Style := [fsBold];
    IdentifierAttri.Foreground := clDefault;
    NumberAttri.Foreground := clNavy;
    StringAttri.Foreground := clBlue;
    SymbolAttri.Foreground := clRed;
    DirectiveAttri.Foreground := clRed;
    DirectiveAttri.Style := [fsBold];
    NestedComments := True;
    StringKeywordMode := spsmNone;
  end;

  Gutter.MarksPart.Visible := False;
  Gutter.SeparatorPart.Visible := False;
  Gutter.LeftOffset := 10;

  TSimbaEditor_GutterSeparator.Create(Gutter.Parts, 4, 2);
  TSimbaEditor_GutterSeparator.Create(Gutter.Parts, 1, 4);

  FParameterHint := TSimbaParameterHint.Create(nil);
  FParameterHint.Editor := Self;

  FAutoComplete := TSimbaAutoComplete.Create(nil);
  FAutoComplete.Editor := Self;
  FAutoComplete.AutoUseSingleIdent := False;
  FAutoComplete.ExecCommandID := ecNone;
  FAutoComplete.ShowSizeDrag := True;
  FAutoComplete.Width := 500;
  FAutoComplete.LinesInWindow := 8;

  with MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret do
  begin
    Enabled := True;
    FullWord := True;
    FullWordMaxLen := 3;
    WaitTime := 100;
    IgnoreKeywords := True;

    with MarkupInfo do
    begin
      FrameColor := clGray;

      BackAlpha := 125;
      Background := clGray;
    end;
  end;

  FAttributes := TSimbaEditor_Attributes.Create(Self);

  Gutter.ChangesPart.AutoSize := False;
  Gutter.ChangesPart.Width := 2;
  Gutter.ChangesPart.ModifiedColor := RGBToColor(190, 0, 0);
  Gutter.ChangesPart.SavedColor := RGBToColor(2, 100, 64);

  with TSynGutterLineOverview.Create(RightGutter.Parts) do
  begin
    FModifiedLinesGutter := TSimbaEditor_ModifiedLinesGutter.Create(Providers);
    FModifiedLinesGutter.Priority := 1;
    FModifiedLinesGutter.Color := Gutter.ChangesPart.ModifiedColor;
    FModifiedLinesGutter.ColorSaved := Gutter.ChangesPart.SavedColor;

    TSynGutterLOvProviderCurrentPage.Create(Providers);

    AutoSize := False;
    Width := 6;
  end;

  MouseActions.AddCommand(emcOverViewGutterScrollTo, False, LazSynEditMouseCmdsTypes.mbLeft, ccSingle, cdDown,[],[]);
  MouseActions.AddCommand(emcJumpBack, False, LazSynEditMouseCmdsTypes.mbExtra1, ccSingle, cdDown, [], []);
  MouseActions.AddCommand(emcJumpForward, False, LazSynEditMouseCmdsTypes.mbExtra2, ccSingle, cdDown, [], []);

  RegisterCommandHandler(@HandleCommand, nil, [hcfPostExec]);
  RegisterMouseActionExecHandler(@HandleMouseAction);

  Keystrokes.Delete(KeyStrokes.FindCommand(ecInsertLine));
  Keystrokes.Delete(KeyStrokes.FindCommand(ecNormalSelect));
  Keystrokes.Delete(KeyStrokes.FindCommand(ecColumnSelect));

  with KeyStrokes.Add() do
  begin
    Key := VK_SPACE;
    Shift := [ssCtrl];
    Command := ecAutoComplete;
  end;

  with KeyStrokes.Add() do
  begin
    Key := VK_SPACE;
    Shift := [ssCtrl, ssShift];
    Command := ecParameterHint;
  end;

  with Keystrokes.Add() do
  begin
    Key := VK_LCL_SLASH;
    Shift := [ssCtrl];
    Command := ecCommentBlock;
  end;

  with Keystrokes.Add() do
  begin
    Key := VK_D;
    Shift := [ssCtrl];
    Command := ecDocumentation;
  end;

  FMultiCaret := TSynPluginMultiCaret.Create(Self);
  FMultiCaret.MouseActions.Clear();
  FMultiCaret.KeyStrokes.Clear();
  with FMultiCaret.KeyStrokes do
  begin
    with Add() do
    begin
      Command := ecPluginMultiCaretSetCaret;
      Key     := VK_INSERT;
      Shift   := [ssShift, ssCtrl];
    end;

    with Add() do
    begin
      Command := ecPluginMultiCaretClearAll;
      Key     := VK_ESCAPE;
    end;
  end;

  if SimbaSettings.GUI.MacOSKeystrokes.Value then
    SimbaSettingChanged(SimbaSettings.GUI.MacOSKeystrokes);

  if LoadColors then
    SimbaSettingChanged(SimbaSettings.Editor.CustomColors);

  SimbaSettingChanged(SimbaSettings.Editor.AllowCaretPastEOL);
  SimbaSettingChanged(SimbaSettings.Editor.RightMarginVisible);
  SimbaSettingChanged(SimbaSettings.Editor.AntiAliased);
  SimbaSettingChanged(SimbaSettings.Editor.FontSize);
  SimbaSettingChanged(SimbaSettings.Editor.FontName);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
end;

destructor TSimbaEditor.Destroy;
begin
  if (SimbaSettings <> nil) then
    SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  if (FAutoComplete <> nil) then
    FreeAndNil(FAutoComplete);
  if (FParameterHint <> nil) then
    FreeAndNil(FParameterHint);
  if (FAttributes <> nil) then
    FreeAndNil(FAttributes);

  inherited Destroy();
end;

end.

