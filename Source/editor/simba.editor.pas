{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor;

{$i simba.inc}

interface

uses
  classes, sysutils, graphics, controls, lcltype,
  synedit, synedittypes, syngutterlineoverview, lazsyneditmousecmdstypes,
  syneditmousecmds, syneditkeycmds, synpluginmulticaret, synedithighlighter, syneditmarkuphighall,
  simba.autocomplete, simba.parameterhint, simba.settings,
  simba.editor_attributes, simba.editor_modifiedlinegutter;

const
  ecAutoComplete      = ecUserFirst + 1;
  ecAutoCompleteChar  = ecUserFirst + 2;
  ecParameterHint     = ecUserFirst + 3;
  ecParameterHintChar = ecUserFirst + 4;

type
  TSimbaEditor = class(TSynEdit)
  protected
    FAttributes: TSimbaEditor_Attributes;
    FParameterHint: TSimbaParameterHint;
    FAutoComplete: TSimbaAutoComplete;
    FModifiedLinesGutter: TSimbaEditorModifiedLinesGutter;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    // Enable/Disable TSynEditMarkupHighlightAllCaret depending on has selection
    procedure DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    // Handle ecAutoCompleteChar & ecParameterHintChar
    procedure DoAfterCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
  public
    property TextView;
    property ModifiedLinesGutter: TSimbaEditorModifiedLinesGutter read FModifiedLinesGutter;
    property AutoComplete: TSimbaAutoComplete read FAutoComplete;
    property ParameterHint: TSimbaParameterHint read FParameterHint;
    property Attributes: TSimbaEditor_Attributes read FAttributes;

    // Is highlighter attribute at caret
    function IsHighlighterAttribute(Values: TStringArray): Boolean;
    // Is value ahead of caret
    function IsTextAhead(Values: TStringArray): Boolean;
    // Get Expression string at X,Y
    function GetExpression(X, Y: Integer): String;
    // Execute a command that needs no extra data
    procedure ExecuteSimpleCommand(Command: TSynEditorCommand);
    // Repaint some extra things when saved
    procedure MarkTextAsSaved; reintroduce;

    constructor Create(AOwner: TComponent; LoadColors: Boolean = True); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  simba.parser_misc, simba.fonthelpers, simba.helpers_string, simba.highlighter,
  simba.editor_blockcompletion, simba.editor_docgenerator, simba.editor_commentblock,
  simba.editor_mousewheelzoom, simba.editor_history;

function TSimbaEditor.IsHighlighterAttribute(Values: TStringArray): Boolean;
var
  Token: String;
  Attri: TSynHighlighterAttributes;
  P: TPoint;
begin
  P := LogicalCaretXY;
  P.X -= 1;

  Result := GetHighlighterAttriAtRowCol(P, Token, Attri) and Attri.Name.ContainsAny(Values);
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

procedure TSimbaEditor.MarkTextAsSaved;
begin
  inherited MarkTextAsSaved();

  InvalidateGutter();
  ModifiedLinesGutter.ReCalc();
end;

procedure TSimbaEditor.DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  if (scSelection in Changes) then
    MarkupByClass[TSynEditMarkupHighlightAllCaret].Enabled := SelAvail;
end;

procedure TSimbaEditor.DoAfterCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
begin
  case Command of
    ecChar:
      case AChar of
        '(': if SimbaSettings.Editor.AutomaticallyShowParameterHints.Value then
               CommandProcessor(ecParameterHintChar, AChar, Data);
        '.': if SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value then
               CommandProcessor(ecAutoCompleteChar, AChar, Data);
      end;
  end;
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
        if SimbaFontHelpers.IsFontFixed(Setting.Value) then
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

function TSimbaEditor.GetExpression(X, Y: Integer): String;
var
  StartX, EndX: Integer;
begin
  GetWordBoundsAtRowCol(TPoint.Create(X, Y), StartX, EndX);

  Result := simba.parser_misc.GetExpression(Text, RowColToCharIndex(TPoint.Create(EndX - 1, Y)));
end;

constructor TSimbaEditor.Create(AOwner: TComponent; LoadColors: Boolean);
begin
  inherited Create(AOwner);

  Options := Options + [eoTabIndent, eoKeepCaretX, eoDragDropEditing, eoScrollPastEof] - [eoSmartTabs];
  Options2 := Options2 + [eoCaretSkipsSelection];

  MouseOptions := [emAltSetsColumnMode, emUseMouseActions, emShowCtrlMouseLinks, emCtrlWheelZoom];
  ResetMouseActions();

  TabWidth := 2;
  BlockIndent := 2;

  MouseActions.AddCommand(emcOverViewGutterScrollTo, False, LazSynEditMouseCmdsTypes.mbLeft, ccSingle, cdDown, [], []);

  RegisterStatusChangedHandler(@DoStatusChanged, [scSelection]);
  RegisterCommandHandler(@DoAfterCommand, nil, [hcfPostExec]);

  Highlighter := TSynFreePascalSyn.Create(Self);
  with TSynFreePascalSyn(Highlighter) do
  begin
    CommentAttri.Foreground := clBlue;
    CommentAttri.Style := [fsBold];
    IdentifierAttri.Foreground := clDefault;
    NumberAttri.Foreground := clNavy;
    StringAttri.Foreground := clBlue;
    SymbolAttri.Foreground := clRed;
    DirectiveAttri.Foreground := clRed;
    DirectiveAttri.Style := [fsBold];
    BracketMatchColor.Background := clGray;
    BracketMatchColor.BackAlpha := 256 div 2;
    BracketMatchColor.Style := [];

    NestedComments := True;
    StringKeywordMode := spsmNone;
    TypeHelpers := True;
  end;

  FParameterHint := TSimbaParameterHint.Create(nil);
  FParameterHint.Editor := Self;

  FAutoComplete := TSimbaAutoComplete.Create(nil);
  FAutoComplete.Editor := Self;
  FAutoComplete.AutoUseSingleIdent := False;
  FAutoComplete.ExecCommandID := ecNone;
  FAutoComplete.ShowSizeDrag := True;
  FAutoComplete.Width := Scale96ToScreen(500);
  FAutoComplete.LinesInWindow := 8;

  with MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret do
  begin
    Enabled := False;
    WaitTime := 1;
    MarkupInfo.Background := clGray;
    MarkupInfo.BackAlpha := 256 div 2;
  end;

  FAttributes := TSimbaEditor_Attributes.Create(Self);

  Gutter.MarksPart.Visible := False;
  Gutter.SeparatorPart.Visible := False;
  Gutter.LeftOffset := 10;
  Gutter.ChangesPart.ModifiedColor := RGBToColor(255, 192, 0);

  with TSynGutterLineOverview.Create(RightGutter.Parts) do
  begin
    FModifiedLinesGutter := TSimbaEditorModifiedLinesGutter.Create(Providers);
    FModifiedLinesGutter.Priority := 1;
    FModifiedLinesGutter.Color := Gutter.ChangesPart.ModifiedColor;
    FModifiedLinesGutter.ColorSaved := Gutter.ChangesPart.SavedColor;

    TSynGutterLOvProviderCurrentPage.Create(Providers);
  end;

  TSynPluginMultiCaret.Create(Self);

  TSimbaEditorPlugin_BlockCompletion.Create(Self);
  TSimbaEditorPlugin_DocGenerator.Create(Self);
  TSimbaEditorPlugin_CommentBlock.Create(Self);
  TSimbaEditorPlugin_MouseWheelZoom.Create(Self);
  TSimbaEditorPlugin_History.Create(Self);

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

  if LoadColors then
    SimbaSettingChanged(SimbaSettings.Editor.CustomColors);

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

  if (FAutoComplete <> nil) then
    FreeAndNil(FAutoComplete);
  if (FParameterHint <> nil) then
    FreeAndNil(FParameterHint);
  if (FAttributes <> nil) then
    FreeAndNil(FAttributes);

  inherited Destroy();
end;

end.

