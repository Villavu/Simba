unit simba.editor;

// This unit enchances TSynEdit for Simba's usage

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType,
  SynEdit, SynGutterLineOverview, SynEditMarks, SynEditMiscClasses, SynEditMouseCmds, SynEditKeyCmds, SynEditHighlighter, SynEditPointClasses,
  simba.highlighter, simba.generics, simba.autocomplete, simba.parameterhint;

const
  ecAutoComplete  = ecUserFirst + 1;
  ecParameterHint = ecUserFirst + 2;
  ecCommentCode   = ecUserFirst + 3;

type
  TSimbaEditor_AttributeList = class(specialize TSimbaObjectList<TSynHighlighterAttributes>);

  TSimbaEditor = class(TSynEdit)
  protected
    FDividerColor: TColor;
    FIndentColor: TColor;
    FCaretColor: TColor;
    FHighlightAllCaretColor: TColor;
    FHighlightCurrentWordColor: TColor;
    FOnBack: TNotifyEvent;
    FOnForward: TNotifyEvent;
    FOnRedo: TNotifyEvent;
    FOnUndo: TNotifyEvent;
    FAttributes: TSimbaEditor_AttributeList;
    FParameterHint: TSimbaParameterHint;
    FAutoComplete: TSimbaAutoComplete;
    FAntiAliasing: Boolean;
    FFileName: String;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure HandleRightGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
    function HandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;

    function GetFontName: String;

    procedure SetCaretColor(Value: TColor);
    procedure SetIndentColor(Value: TColor);
    procedure SetAntiAliasing(Value: Boolean);
    procedure SetDividerColor(Value: TColor);

    procedure SettingChanged_Colors(Value: String);
    procedure SettingChanged_RightMargin(Value: Int64);
    procedure SettingChanged_RightMarginVisible(Value: Boolean);
    procedure SettingChanged_CaretPastEOL(Value: Boolean);
    procedure SettingChanged_AntiAliasing(Value: Boolean);
    procedure SettingChanged_FontName(Value: String);
    procedure SettingChanged_FontHeight(Value: Int64);
    procedure SettingChanged_DividerVisible(Value: Boolean);

    procedure AddSettingChangeHandlers;
    procedure RemoveSettingChangeHandlers;
  public
    property FileName: String read FFileName;
    property FontName: String read GetFontName;

    property AutoComplete: TSimbaAutoComplete read FAutoComplete;
    property ParameterHint: TSimbaParameterHint read FParameterHint;

    property Attributes: TSimbaEditor_AttributeList read FAttributes;
    property CaretColor: TColor read FCaretColor write SetCaretColor;
    property IndentColor: TColor read FIndentColor write SetIndentColor;
    property AntiAliasing: Boolean read FAntiAliasing write SetAntiAliasing;
    property DividerColor: TColor read FDividerColor write SetDividerColor;

    function Save(AFileName: String): Boolean;
    function Load(AFileName: String): Boolean;

    procedure FindDeclaration(XY: TPoint);

    procedure SaveColors(AFileName: String);
    procedure LoadColors(AFileName: String);

    procedure LoadDefaultScript;

    function GetCaretObj: TSynEditCaret; reintroduce;

    procedure CommentCode;

    procedure CommandProcessor(Command: TSynEditorCommand; AChar: TUTF8Char; Data: Pointer; ASkipHooks: THookedCommandFlags = []); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SynEditMarkupHighAll, SynEditMarkupFoldColoring, SynGutter,
  dialogs, inifiles, math, menus,
  simba.settings, simba.editor_attributes, simba.scripttabhistory;

type
  TSynHighlighterAttributes_Helper = class helper for TSynHighlighterAttributes
  public
    procedure Save(INI: TINIFile);
    procedure Load(INI: TINIFile);
    procedure Changed;
  end;

type
  TSynGutterLineOverview_Helper = class helper for TSynGutterLineOverview
  public
    function PixelToLine(Pixel: Int32): Int32;
  end;

procedure TSynHighlighterAttributes_Helper.Save(INI: TINIFile);
begin
  INI.WriteInteger(StoredName, 'Background', Background);
  INI.WriteInteger(StoredName, 'Foreground', Foreground);
  INI.WriteInteger(StoredName, 'Style', IntegerStyle);
  INI.WriteInteger(StoredName, 'StyleMask', IntegerStyleMask);
  INI.WriteInteger(StoredName, 'Frame', FrameColor);
end;

procedure TSynHighlighterAttributes_Helper.Load(INI: TINIFile);
begin
  if INI.ValueExists(StoredName, 'Background') then
    Background := INI.ReadInteger(StoredName, 'Background', clWindow);
  if INI.ValueExists(StoredName, 'Foreground') then
    Foreground := INI.ReadInteger(StoredName, 'Foreground', clWindowText);
  if INI.ValueExists(StoredName, 'Style') then
    IntegerStyle := INI.ReadInteger(StoredName, 'Style', 0);
  if INI.ValueExists(StoredName, 'StyleMask') then
    IntegerStyleMask := INI.ReadInteger(StoredName, 'StyleMask', 0);
  if INI.ValueExists(StoredName, 'Frame') then
    FrameColor := INI.ReadInteger(StoredName, 'Frame', clRed);
end;

procedure TSynHighlighterAttributes_Helper.Changed;
begin
  DoChange();
end;

function TSynGutterLineOverview_Helper.PixelToLine(Pixel: Int32): Int32;
begin
  Result := PixelLineToText(Pixel);
end;

procedure TSimbaEditor.CommentCode;
var
  i, Start, Stop, ColStart, Temp: integer;
  StartStr, CurrLineStr: String;
  XY: TPoint;
begin
  XY := CaretXY;

  Start := BlockBegin.Y;
  Stop := BlockEnd.Y;

  if (Start > Stop) then
  begin
    Temp := SelStart;

    Start := Stop;
    Stop := Temp;
  end;

  BeginUpdate();

  try
    BeginUndoBlock();

    try
      StartStr := Lines[Start - 1].TrimLeft();
      ColStart := -1;

      for i := Start to Stop do
      begin
        CurrLineStr := Lines[i - 1].TrimLeft();
        Temp := Lines[i - 1].IndexOf(CurrLineStr) + 1;
        if (ColStart < 0) or (ColStart > Temp) then
          ColStart := Temp;
      end;

      if StartStr.StartsWith('//') then
      begin
        for i := Start to Stop do
          if Lines[i - 1].TrimLeft().StartsWith('//') then
            TextBetweenPoints[Point(ColStart, i), Point(ColStart + 2, i)] := '';
      end
      else
        for i := Start to Stop do
          TextBetweenPoints[Point(ColStart, i), Point(ColStart, i)] := '//';
    finally
      EndUndoBlock();
    end;
  finally
    EndUpdate();
  end;

  CaretXY := XY;
end;

procedure TSimbaEditor.SetDividerColor(Value: TColor);
var
  i: Int32;
begin
  FDividerColor := Value;
  if not SimbaSettings.Editor.DividerVisible.Value then
    Value := clNone;

  if (Highlighter <> nil) then
    for i := 0 to Highlighter.DividerDrawConfigCount - 1 do
    begin
      Highlighter.DividerDrawConfig[i].TopColor := Value;
      Highlighter.DividerDrawConfig[i].NestColor := Value;
    end;
end;

function TSimbaEditor.GetFontName: String;
begin
  if Font.Name <> '' then
    Result := Font.Name
  else
    Result := SynDefaultFontName;
end;

procedure TSimbaEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbExtra1: SimbaScriptTabHistory.GoBack();
    mbExtra2: SimbaScriptTabHistory.GoForward();
    else
      inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TSimbaEditor.HandleRightGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
begin
  Self.TopLine := TSynGutterLineOverview(Sender).PixelToLine(Y);
end;

procedure TSimbaEditor.CommandProcessor(Command: TSynEditorCommand; AChar: TUTF8Char; Data: Pointer; ASkipHooks: THookedCommandFlags);
begin
  inherited CommandProcessor(Command, AChar, Data, ASkipHooks);

  case Command of
    ecCommentCode:
      begin
        CommentCode();
      end;
  end;
end;

function TSimbaEditor.HandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;

  case AnAction.Command of
    emcWheelZoomIn:
      SimbaSettings.Editor.FontHeight.Value := Max(10, SimbaSettings.Editor.FontHeight.Value + 1);

    emcWheelZoomOut:
      SimbaSettings.Editor.FontHeight.Value := Max(10, SimbaSettings.Editor.FontHeight.Value - 1);
  end;
end;

procedure TSimbaEditor.SetCaretColor(Value: TColor);
begin
  FCaretColor := Value;

  if (FCaretColor = clDefault) or (FCaretColor = clNone) then
  begin
    FScreenCaretPainterClass := TSynEditScreenCaretPainterSystem;
    if ScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterSystem then
      ScreenCaret.ChangePainter(TSynEditScreenCaretPainterSystem);
  end else
  begin
    FScreenCaretPainterClass := TSynEditScreenCaretPainterInternal;
    if ScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterInternal then
      ScreenCaret.ChangePainter(TSynEditScreenCaretPainterInternal);

    TSynEditScreenCaretPainterInternal(ScreenCaret.Painter).Color := FCaretColor;
  end;
end;

procedure TSimbaEditor.SetIndentColor(Value: TColor);
begin
  FIndentColor := Value;
  with MarkupByClass[TSynEditMarkupFoldColors] as TSynEditMarkupFoldColors do
    LineColor[0].Color := FIndentColor;

  Invalidate();
end;

procedure TSimbaEditor.SetAntiAliasing(Value: Boolean);
begin
  FAntiAliasing := Value;

  if FAntiAliasing then
    Font.Quality := fqDefault
  else
    Font.Quality := fqNonAntialiased;
end;

procedure TSimbaEditor.SettingChanged_Colors(Value: String);
begin
  LoadColors(Value);
end;

procedure TSimbaEditor.SettingChanged_RightMargin(Value: Int64);
begin
  RightEdge := Value;
end;

procedure TSimbaEditor.SettingChanged_RightMarginVisible(Value: Boolean);
begin
  if Value then
    Options := Options - [eoHideRightMargin]
  else
    Options := Options + [eoHideRightMargin];
end;

procedure TSimbaEditor.SettingChanged_CaretPastEOL(Value: Boolean);
begin
  if Value then
    Options := Options + [eoTrimTrailingSpaces, eoScrollPastEol]
  else
    Options := Options - [eoTrimTrailingSpaces, eoScrollPastEol];
end;

procedure TSimbaEditor.SettingChanged_AntiAliasing(Value: Boolean);
begin
  AntiAliasing := Value;
end;

procedure TSimbaEditor.SettingChanged_FontName(Value: String);
begin
  if Value <> '' then
    Font.Name := Value;
end;

procedure TSimbaEditor.SettingChanged_FontHeight(Value: Int64);
begin
  Font.Height := Value;
end;

procedure TSimbaEditor.SettingChanged_DividerVisible(Value: Boolean);
begin
  DividerColor := DividerColor;
end;

procedure TSimbaEditor.AddSettingChangeHandlers;
begin
  SimbaSettings.Editor.ColorsPath.AddHandlerOnChange(@SettingChanged_Colors);
  SimbaSettings.Editor.RightMargin.AddHandlerOnChange(@SettingChanged_RightMargin);
  SimbaSettings.Editor.RightMarginVisible.AddHandlerOnChange(@SettingChanged_RightMarginVisible);
  SimbaSettings.Editor.AllowCaretPastEOL.AddHandlerOnChange(@SettingChanged_CaretPastEOL);
  SimbaSettings.Editor.AntiAliasing.AddHandlerOnChange(@SettingChanged_AntiAliasing);
  SimbaSettings.Editor.FontName.AddHandlerOnChange(@SettingChanged_FontName);
  SimbaSettings.Editor.FontHeight.AddHandlerOnChange(@SettingChanged_FontHeight);
  SimbaSettings.Editor.DividerVisible.AddHandlerOnChange(@SettingChanged_DividerVisible);
end;

procedure TSimbaEditor.RemoveSettingChangeHandlers;
begin
  if SimbaSettings = nil then
    Exit;

  SimbaSettings.Editor.ColorsPath.RemoveHandlerOnChange(@SettingChanged_Colors);
  SimbaSettings.Editor.RightMargin.RemoveHandlerOnChange(@SettingChanged_RightMargin);
  SimbaSettings.Editor.RightMarginVisible.RemoveHandlerOnChange(@SettingChanged_RightMarginVisible);
  SimbaSettings.Editor.AllowCaretPastEOL.RemoveHandlerOnChange(@SettingChanged_CaretPastEOL);
  SimbaSettings.Editor.AntiAliasing.RemoveHandlerOnChange(@SettingChanged_AntiAliasing);
  SimbaSettings.Editor.FontName.RemoveHandlerOnChange(@SettingChanged_FontName);
  SimbaSettings.Editor.FontHeight.RemoveHandlerOnChange(@SettingChanged_FontHeight);
  SimbaSettings.Editor.DividerVisible.RemoveHandlerOnChange(@SettingChanged_DividerVisible);
end;

function TSimbaEditor.Save(AFileName: String): Boolean;
begin
  Result := False;
  if AFileName = '' then
    Exit;

  try
    Lines.SaveToFile(AFileName);
    MarkTextAsSaved();
    FFileName := AFileName;
    Result := True;
  except
    on E: Exception do
      MessageDlg('Unable to save script: ' + E.Message, mtError, [mbOK], 0);
  end;
end;

function TSimbaEditor.Load(AFileName: String): Boolean;
begin
  Result := False;
  if AFileName = '' then
    Exit;

  if FileExists(AFileName) then
  try
    Lines.LoadFromFile(AFileName);
    MarkTextAsSaved();
    FFileName := AFileName;
    Result := True;
  except
    on E: Exception do
      MessageDlg('Unable to load script: ' + E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TSimbaEditor.FindDeclaration(XY: TPoint);
var
  AllowMouseLink: Boolean;
begin
  if (OnMouseLink <> nil) then OnMouseLink(Self, XY.X, XY.Y, AllowMouseLink);
  if (OnClickLink <> nil) then OnClickLink(Self, mbLeft, [], XY.X, XY.Y);
end;

procedure TSimbaEditor.SaveColors(AFileName: String);
var
  INI: TINIFile;
  i: Int32;
begin
  try
    INI := TIniFile.Create(AFileName);
    INI.CacheUpdates := True;

    for i := 0 to FAttributes.Count - 1 do
      FAttributes[i].Save(INI);

    INI.Free();
  except
  end;
end;

procedure TSimbaEditor.LoadColors(AFileName: String);
var
  i: Int32;
  INI: TINIFile;
begin
  if FileExists(AFileName) then
  try
    INI := TIniFile.Create(AFileName);

    for i := 0 to FAttributes.Count - 1 do
      FAttributes[i].Load(INI);

    INI.Free();
  except
  end;
end;

procedure TSimbaEditor.LoadDefaultScript;
begin
  if FileExists(SimbaSettings.Editor.DefaultScriptPath.Value) then
    Load(SimbaSettings.Editor.DefaultScriptPath.Value)
  else
  begin
    Text := 'program new;' + LineEnding +
            'begin' + LineEnding +
            'end.';

    MarkTextAsSaved();
  end;

  FFileName := '';
end;

function TSimbaEditor.GetCaretObj: TSynEditCaret;
begin
  Result := inherited GetCaretObj();
end;

constructor TSimbaEditor.Create(AOwner: TComponent);
var
  I: Int32;
  {$IFDEF DARWIN}
  Key: UInt16;
  Shift: TShiftState;
  {$ENDIF}
begin
  inherited Create(AOwner);

  Highlighter := TSynFreePascalSyn.Create(Self);

  with Highlighter as TSynFreePascalSyn do
  begin
    CommentAttri.Foreground := clBlue;
    CommentAttri.Style := [fsBold];
    IdentifierAttri.Foreground := clDefault;
    NumberAttri.Foreground := clNavy;
    StringAttri.Foreground := clBlue;
    SymbolAttri.Foreground := clRed;
    DirectiveAttri.Foreground := clRed;
    DirectiveAttri.Style := [fsBold];
    NestedComments := False;
    StringKeywordMode := spsmNone;
  end;

  FParameterHint := TSimbaParameterHint.Create(nil);
  FParameterHint.Editor := Self;

  FAutoComplete := TSimbaAutoComplete.Create(nil);
  FAutoComplete.Editor := Self;
  FAutoComplete.AutoUseSingleIdent := False;
  FAutoComplete.ExecCommandID := ecNone;
  FAutoComplete.ShowSizeDrag := True;
  FAutoComplete.Width := 500;
  FAutoComplete.LinesInWindow := 8;

  FAttributes := TSimbaEditor_SimbaAttributesList.Create(Self);

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

  // Indent Line
  MarkupManager.AddMarkUp(TSynEditMarkupFoldColors.Create(Self));

  with MarkupByClass[TSynEditMarkupFoldColors] as TSynEditMarkupFoldColors do
  begin
    ColorCount := 1;

    with Color[0] do
    begin
      Foreground := clNone;
      Background := clNone;
    end;
  end;

  // Right Gutter
  TSynGutterSeparator.Create(RightGutter.Parts).Width := 20;

  with TSynGutterLineOverview.Create(RightGutter.Parts) do
  begin
    OnGutterClick := @HandleRightGutterClick;

    TSynGutterLOvProviderCurrentPage.Create(Providers).Priority := 1;
    TSynGutterLOvProviderModifiedLines.Create(Providers).Priority := 2;
  end;

  // Hooks
  RegisterMouseActionExecHandler(@HandleMouseAction);

  // Defaults
  CaretColor := clDefault;
  IndentColor := clGray;

  MouseOptions := MouseOptions + [emUseMouseActions, emShowCtrlMouseLinks, emCtrlWheelZoom];
  ResetMouseActions();

  // Remapping
  with Keystrokes[KeyStrokes.FindCommand(ecNormalSelect)] do
    Shift := [ssShift, ssAlt];

  with Keystrokes[KeyStrokes.FindCommand(ecColumnSelect)] do
    Shift := [ssShift, ssAlt];

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
    Command := ecCommentCode;
  end;

  {$IFDEF DARWIN}
  for I := 0 to Keystrokes.Count - 1 do
  begin
    ShortCutToKey(Keystrokes[I].ShortCut, Key, Shift);

    if ssCtrl in Shift then
    begin
      Shift := Shift - [ssCtrl] + [ssMeta];

      Keystrokes[I].ShortCut := ShortCut(Key, Shift);
    end;
  end;

  MouseActions.AddCommand(emcWheelZoomOut, False, mbXWheelDown, ccAny,    cdDown, [ssMeta], [ssMeta]);
  MouseActions.AddCommand(emcWheelZoomIn,  False, mbXWheelUp,   ccAny,    cdDown, [ssMeta], [ssMeta]);
  MouseActions.AddCommand(emcMouseLink,    False, mbXLeft,      ccSingle, cdUp,   [ssMeta], [ssShift, ssAlt, ssMeta]);
  {$ENDIF}

  LoadDefaultScript();

  for I := 0 to FAttributes.Count - 1 do
    FAttributes[I].Changed();

  AddSettingChangeHandlers();
end;

destructor TSimbaEditor.Destroy;
begin
  RemoveSettingChangeHandlers();

  FAutoComplete.Free();
  FParameterHint.Free();

  FAttributes.Free();

  inherited Destroy();
end;

end.

