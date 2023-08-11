{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Auto complete form (Ctrl + Space)
}
unit simba.editor_autocomplete;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Controls, Forms, LCLType, Types,
  SynEdit, SynEditTypes, SynCompletion, SynEditKeyCmds, SynEditHighlighter,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_insight,
  simba.component_scrollbar;

type
  TSimbaAutoCompleteSizeDrag = class(TSynBaseCompletionFormSizeDrag)
    procedure Paint; override;
  end;

  TSimbaAutoComplete = class;
  TSimbaAutoComplete_Form = class(TSynCompletionForm)
  protected
    RealScroll: TSimbaScrollBar;

    procedure DoPaintSizeDrag(Sender: TObject);
    procedure DoScrollChange(Sender: TObject);

    procedure FontChanged(Sender: TObject); override;

    procedure Paint; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    AutoComplete: TSimbaAutoComplete;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAutoComplete_Hint = class(TSynBaseCompletionHint)
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  public
    AutoComplete: TSimbaAutoComplete;
    TextWidth: Integer;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAutoComplete = class(TSynCompletion)
  protected
    FForm: TSimbaAutoComplete_Form;
    FHintForm: TSimbaAutoComplete_Hint;
    FCodeinsight: TCodeinsight;
    FDecls: TDeclarationArray;
    FLocalDecls: TDeclarationArray;

    FFilteredDecls: TDeclarationArray;
    FFilteredWeights: TIntegerArray; // cache

    FColumnWidth: Integer;
    FDrawOffsetY: Integer;

    function GetHintText(Decl: TDeclaration; IsHint: Boolean): String;

    function GetDecl(Index: Integer): TDeclaration;
    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;

    procedure ContinueCompletion(Data: PtrInt);

    procedure PaintColumn(Canvas: TCanvas; var X, Y: Integer; Decl: TDeclaration);
    procedure PaintName(Canvas: TCanvas; var X, Y: Integer; AName: String);
    procedure PaintText(Canvas: TCanvas; X, Y: Integer; AText: String);

    function DoPaintItem(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
    function DoMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;

    procedure DoCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure DoFiltering(var NewPosition: Integer);
    procedure DoTabPressed(Sender: TObject);
    procedure DoExecute(Sender: TObject);

    procedure DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoEditorRemoving(Value: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Form: TSimbaAutoComplete_Form read FForm;
  public
    class var AutoCompleteCommand: TSynEditorCommand;
    class function IsAutoCompleteCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
    class constructor Create;
  end;

implementation

uses
  simba.settings, simba.algo_sort, simba.editor, simba.ide_codetools_setup, simba.theme;

{$IFDEF WINDOWS}
function SetClassLong(Handle: HWND; Index: Integer = -26; Value: Integer = 0): UInt32; stdcall; external 'user32' name 'SetClassLongA';
{$ENDIF}

type
  TVirtualStringList = class(TStrings)
  protected
    FCount: Integer;

    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure SetCount(Value: Integer);
  public
    procedure Clear; override;
    property Count: Integer read GetCount write SetCount;
  end;

procedure TSimbaAutoCompleteSizeDrag.Paint;
var
  I: Integer;
begin
  Canvas.Brush.Color := SimbaTheme.ColorScrollBarInActive;
  Canvas.Pen.Color := SimbaTheme.ColorLine;

  Canvas.FillRect(ClientRect);

  I := 2;
  while (I < Height-3) do
  begin
    Canvas.MoveTo(ClientRect.Right-I, ClientRect.Bottom-1-1);
    Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-I-1);
    Inc(I, 3);
  end;
end;

procedure TVirtualStringList.Clear;
begin
  FCount := 0;
end;

function TVirtualStringList.Get(Index: Integer): string;
begin
  Result := '';
end;

function TVirtualStringList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TVirtualStringList.SetCount(Value: Integer);
begin
  FCount := Value;
end;

procedure TSimbaAutoComplete_Hint.Paint;
begin
  if (AutoComplete.Position = Index) then
    Canvas.Brush.Color := AutoComplete.SelectedColor
  else
    Canvas.Brush.Color := AutoComplete.Form.Color;
  Canvas.FillRect(ClientRect);

  AutoComplete.OnPaintItem('', Canvas, AutoComplete.Form.DrawBorderWidth, 0, AutoComplete.Position = Index, Index);
end;

constructor TSimbaAutoComplete_Hint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color := clRed; // disable "UseBGThemes" to stop flickering. We custom draw so this color doesn't matter.

  {$IFDEF WINDOWS}
  SetClassLong(Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

procedure TSimbaAutoComplete_Hint.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (AutoComplete <> nil) then
  begin
    ATop    := HintRect.Top + AutoComplete.Form.DrawBorderWidth;
    AWidth  := TextWidth;
    AHeight := AutoComplete.FontHeight - 3;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TSimbaAutoComplete_Hint.EraseBackground(DC: HDC);
begin
  { nothing }
end;

procedure TSimbaAutoComplete_Form.DoPaintSizeDrag(Sender: TObject);
begin
  Canvas.Brush.Color := 255;
  Canvas.FillRect(ClientRect);
end;

procedure TSimbaAutoComplete_Form.DoScrollChange(Sender: TObject);
begin
  Scroll.Position := RealScroll.Position;
end;

procedure TSimbaAutoComplete_Form.FontChanged(Sender: TObject);
var
  Size: TSize;
begin
  inherited FontChanged(Sender);

  if (FHint <> nil) then
    FHint.Font := Self.Font;
  FFontHeight := FFontHeight + 4;

  if (AutoComplete <> nil) then
  begin
    Size := Canvas.TextExtent('class const ');

    AutoComplete.FColumnWidth := Size.Width;
    AutoComplete.FDrawOffsetY := (((FFontHeight - 2) - Size.Height) div 2) - 1;
  end;
end;

procedure TSimbaAutoComplete_Form.Paint;
begin
  inherited Paint;

  RealScroll.Max := ItemList.Count;
  RealScroll.LargeChange := NbLinesInWindow;
  RealScroll.PageSize := NbLinesInWindow;
  RealScroll.Position := Scroll.Position;
  RealScroll.Update();
end;

procedure TSimbaAutoComplete_Form.DoShow;
begin
  RealScroll.Position := 0;

  Width           := SimbaSettings.Editor.AutoCompleteWidth.Value;
  NbLinesInWindow := SimbaSettings.Editor.AutoCompleteLines.Value;

  inherited DoShow();
end;

procedure TSimbaAutoComplete_Form.DoHide;
begin
  inherited DoHide();

  SimbaSettings.Editor.AutoCompleteWidth.Value := Width;
  SimbaSettings.Editor.AutoCompleteLines.Value := NbLinesInWindow;
end;

procedure TSimbaAutoComplete_Form.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ((Scroll.Visible) and (X > Scroll.Left)) or (Y < DrawBorderWidth) or (Y >= ClientHeight - DrawBorderWidth) then
    Exit;
  if (Scroll.Position + (Y - DrawBorderWidth) div FFontHeight) = FHint.Index then
    Exit;

  inherited MouseMove(Shift, X, Y);
end;

procedure TSimbaAutoComplete_Form.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_OEM_COMMA) then
  begin
    OnValidate(Self, ',', Shift);
    Key := 0;
  end;

  inherited KeyDown(Key, Shift);
end;

constructor TSimbaAutoComplete_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Scroll.Width := 0;

  RealScroll := TSimbaScrollBar.Create(Self);
  RealScroll.Parent := Self;
  RealScroll.Kind := sbVertical;
  RealScroll.OnEnter := @ScrollGetFocus;
  RealScroll.OnChange := @DoScrollChange;
  RealScroll.Anchors := [akTop, akRight, akBottom];
  RealScroll.AnchorSide[akTop].Side := asrTop;
  RealScroll.AnchorSide[akTop].Control := self;
  RealScroll.AnchorSide[akRight].Side := asrBottom;
  RealScroll.AnchorSide[akRight].Control := Self;
  RealScroll.AnchorSide[akBottom].Side := asrTop;

  SizeDrag.Free();
  SizeDrag := TSimbaAutoCompleteSizeDrag.Create(Self);
  SizeDrag.Parent := Self;
  SizeDrag.BevelInner := bvNone;
  SizeDrag.BevelOuter := bvNone;
  SizeDrag.Caption := '';
  SizeDrag.AutoSize := False;
  SizeDrag.BorderStyle := bsNone;
  SizeDrag.Anchors := [akBottom, akRight, akLeft];
  SizeDrag.AnchorSideLeft.Side := asrTop;
  SizeDrag.AnchorSideLeft.Control := RealScroll;
  SizeDrag.AnchorSideRight.Side := asrBottom;
  SizeDrag.AnchorSideRight.Control := Self;
  SizeDrag.AnchorSideBottom.Side := asrBottom;
  SizeDrag.AnchorSideBottom.Control := Self;
  SizeDrag.Cursor := crSizeNWSE;
  SizeDrag.Visible := False;
  SizeDrag.Constraints.MinHeight := RealScroll.Width-DrawBorderWidth;
  SizeDrag.Constraints.MaxHeight := RealScroll.Width-DrawBorderWidth;

  RealScroll.AnchorSide[akBottom].Control := SizeDrag;

  if (FItemList <> nil) then
    FItemList.Free();
  FItemList := TVirtualStringList.Create(){%H-};

  DrawBorderWidth := 4;

  {$IFDEF WINDOWS}
  SetClassLong(FHint.Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

procedure TSimbaAutoComplete.ContinueCompletion(Data: PtrInt);
begin
  Editor.CommandProcessor(ecChar, Char(Byte(Data)), nil);
end;

procedure TSimbaAutoComplete.DoCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
var
  Decl: TDeclaration;
begin
  Decl := FFilteredDecls[Position];
  if Decl.IsName(SourceValue) then
    Value := SourceValue
  else
    Value := Decl.Name;

  case KeyChar of
    '.': Application.QueueAsyncCall(@ContinueCompletion, Ord('.'));
    ',': Application.QueueAsyncCall(@ContinueCompletion, Ord(','));
  end;
end;

function CompareDeclarations(A, B: TDeclaration): Integer;
begin
  Result := CompareText(A.Name, B.Name);
end;

procedure TSimbaAutoComplete.DoFiltering(var NewPosition: Integer);
var
  Filter: String;
  Count: Integer;

  function IgnoreDeclaration(const Decl: TDeclaration): Boolean; inline;
  begin
    Result := (Decl.Name = '') or Decl.isOverrideMethod or Decl.isOperatorMethod;
  end;

  procedure AddSorted(Decls: TDeclarationArray; StartIndex: Integer);
  var
    I: Integer;
  begin
    for I := 0 to High(Decls) do
    begin
      if IgnoreDeclaration(Decls[I]) then
        Continue;

      FFilteredDecls[Count] := Decls[I];
      Inc(Count);
    end;

    specialize QuickSortFunc<TDeclaration>(FFilteredDecls, StartIndex, Count - 1, @CompareDeclarations);
  end;

  procedure AddFiltered(Decls: TDeclarationArray; StartIndex: Integer);
  var
    I: Integer;
    DeclName: String;
  begin
    for I := 0 to High(Decls) do
    begin
      if IgnoreDeclaration(Decls[I]) then
        Continue;

      DeclName := Decls[I].Name.ToUpper();
      if (DeclName.IndexOf(Filter) > 0) then
      begin
        FFilteredDecls[Count] := Decls[I];
        FFilteredWeights[Count] := (100 - Round(Length(Filter) / Length(DeclName) * 100)) + (DeclName.IndexOf(Filter) * 100);

        Inc(Count);
      end;
    end;

    specialize QuickSortWeighted<TDeclaration, Integer>(FFilteredDecls, FFilteredWeights, StartIndex, Count - 1, True);
  end;

var
  NeededLength: Integer;
begin
  NeededLength := Length(FDecls) + Length(FLocalDecls);
  if (Length(FFilteredDecls) < NeededLength) then
    SetLength(FFilteredDecls, NeededLength);
  if (Length(FFilteredWeights) < NeededLength) then
    SetLength(FFilteredWeights, NeededLength);

  Count := 0;
  Filter := CurrentString.ToUpper();
  if (Filter = '') then
  begin
    AddSorted(FLocalDecls, 0);
    AddSorted(FDecls, Count);
  end else
  begin
    AddFiltered(FLocalDecls, 0);
    AddFiltered(FDecls, Count);
  end;

  if (Count > 0) then
    NewPosition := 0
  else
    NewPosition := -1;

  TVirtualStringList(ItemList).Count := Count;
end;

procedure TSimbaAutoComplete.DoTabPressed(Sender: TObject);
begin
  if (OnValidate <> nil) then
    OnValidate(TheForm, '', []);
end;

procedure TSimbaAutoComplete.DoExecute(Sender: TObject);
begin
  if (Editor <> nil) then
  begin
    FForm.TextColor := Editor.Highlighter.IdentifierAttribute.Foreground;
    FForm.TextSelectedColor := Editor.Highlighter.IdentifierAttribute.Foreground;
    FForm.DrawBorderColor := SimbaTheme.ColorScrollBarInActive;
    FForm.BackgroundColor := SimbaTheme.ColorScrollBarInActive;
    Fform.ClSelect := Editor.SelectedColor.Background;
    FForm.Font := Editor.Font;
  end;
end;

function TSimbaAutoComplete.GetHintText(Decl: TDeclaration; IsHint: Boolean): String;

  function GetMethodText(Decl: TDeclaration_Method): String;
  begin
    Result := Decl.ParamString + Decl.ResultString;
  end;

  function GetVarText(Decl: TDeclaration_Var): String;
  begin
    if IsHint then
      Result := Decl.VarTypeString + Decl.VarDefaultString
    else
      Result := Decl.VarTypeString;
  end;

  function GetEnumElementText(Decl: TDeclaration_EnumElement): String;
  begin
    if (Decl.Owner <> nil) then
      Result := ': ' + Decl.Owner.Name
    else
      Result := '';
  end;

  function GetTypeText(Decl: TDeclaration_Type): String;
  begin
    if (Decl is TDeclaration_TypeAlias) and (TDeclaration_TypeAlias(Decl).VarType.Text = Decl.Name) then // BaseType (Integer = Integer)
      Result := ''
    else
      Result := ' = ' + Decl.TextNoCommentsSingleLine
  end;

begin
  Result := '';

  if (Decl is TDeclaration_Method) then
    Result := GetMethodText(Decl as TDeclaration_Method)
  else
  if (Decl is TDeclaration_EnumElement) then
    Result := GetEnumElementText(Decl as TDeclaration_EnumElement)
  else
  if (Decl is TDeclaration_Var) then
    Result := GetVarText(Decl as TDeclaration_Var)
  else
  if (Decl is TDeclaration_Type) then
    Result := GetTypeText(Decl as TDeclaration_Type);

  if (Result = '') then
    Result := #0;
end;

function TSimbaAutoComplete.GetDecl(Index: Integer): TDeclaration;
begin
  if (Index >= 0) and (Index < ItemList.Count) then
    Result := FFilteredDecls[Index]
  else
    Result := nil;
end;

function TSimbaAutoComplete.GetCompletionFormClass: TSynBaseCompletionFormClass;
begin
  Result := TSimbaAutoComplete_Form;
end;

procedure TSimbaAutoComplete.DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
var
  Expression, Filter: String;
  LastDot: Integer;
  StartPoint: TPoint;
begin
  if IsAutoCompleteCommand(Command, AChar) and CodetoolsSetup then
    with TSimbaEditor(Editor) do
    begin
      Handled := True;
      if IsHighlighterAttribute(['Number', 'Comment']) then
        Exit;

      FDecls := [];
      FLocalDecls := [];
      FCodeinsight.SetScript(Text, '', GetCaretPos(True));
      FCodeinsight.Run();

      Expression := GetExpression(CaretX - 1, CaretY);
      if Expression.Contains('.') then
      begin
        if (Expression[Length(Expression)] <> '.') then
        begin
          LastDot := Expression.LastIndexOf('.');

          Filter     := Copy(Expression, LastDot + 1);
          Expression := Copy(Expression, 1, LastDot - 1);
        end else
          Filter := '';

        FDecls := FCodeinsight.GetMembersOfType(FCodeinsight.ParseExpression(Expression, [EParseExpressionFlag.WantMethodResult]));
      end else
      begin
        Filter := Expression;

        FDecls := FCodeinsight.GetGlobals();
        FLocalDecls := FCodeinsight.GetLocals();
      end;

      StartPoint := CaretXY;
      StartPoint.X := StartPoint.X - Length(Filter);

      with ClientToScreen(RowColumnToPixels(StartPoint)) do
        Execute(Filter, X, Y + LineHeight);
    end;
end;

procedure TSimbaAutoComplete.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  if (Value is TSimbaEditor) then
    with TSimbaEditor(Value) do
    begin
      RegisterCommandHandler(@DoEditorCommand, nil, [hcfPostExec]);

      with KeyStrokes.Add() do
      begin
        Key := VK_SPACE;
        Shift := [ssCtrl];
        Command := AutoCompleteCommand;
      end;
    end;
end;

procedure TSimbaAutoComplete.DoEditorRemoving(Value: TCustomSynEdit);
begin
  if (Value is TSimbaEditor) then
    with TSimbaEditor(Value) do
      UnRegisterCommandHandler(@DoEditorCommand);

  inherited DoEditorRemoving(Value);
end;

procedure TSimbaAutoComplete.PaintColumn(Canvas: TCanvas; var X, Y: Integer; Decl: TDeclaration);
type
  TColumnFormat = record
    Text: String;
    Color: TColor;
  end;
const
  COLUMN_VAR:         TColumnFormat = (Text: 'var';         Color: clPurple);
  COLUMN_FUNC:        TColumnFormat = (Text: 'function';    Color: $008CFF);
  COLUMN_PROC:        TColumnFormat = (Text: 'procedure';   Color: clNavy);
  COLUMN_TYPE:        TColumnFormat = (Text: 'type';        Color: clGreen);
  COLUMN_ENUM:        TColumnFormat = (Text: 'enum';        Color: $9314FF);
  COLUMN_CONST:       TColumnFormat = (Text: 'const';       Color: $8B8B00);
  COLUMN_CLASS_VAR:   TColumnFormat = (Text: 'class var';   Color: clPurple);
  COLUMN_CLASS_CONST: TColumnFormat = (Text: 'class const'; Color: $8B8B00);
var
  Column: TColumnFormat;
begin
  if (Decl is TDeclaration_Method) then
  begin
    if Decl.isFunction  then
      Column := COLUMN_FUNC
    else
      Column := COLUMN_PROC;
  end else
  if (Decl.Owner is TDeclaration_TypeRecord) then
  begin
    if (Decl is TDeclaration_Field) then
      Column := COLUMN_VAR
    else
    if (Decl is TDeclaration_Const) then
      Column := COLUMN_CLASS_CONST
    else
    if (Decl is TDeclaration_Var) then
      Column := COLUMN_CLASS_VAR;
  end else
  begin
    if (Decl is TDeclaration_EnumElement) then
      Column := COLUMN_ENUM
    else
    if (Decl is TDeclaration_Const) then
      Column := COLUMN_CONST
    else
    if (Decl is TDeclaration_Var) then
      Column := COLUMN_VAR
    else
    if (Decl is TDeclaration_Type) then
      Column := COLUMN_TYPE;
  end;

  Canvas.Font.Color := Column.Color;
  Canvas.TextOut(X + 2, Y, Column.Text);

  X := FColumnWidth;
end;

procedure TSimbaAutoComplete.PaintName(Canvas: TCanvas; var X, Y: Integer; AName: String);

  procedure DrawText(Str: String; Color: TColor);
  begin
    Canvas.Font.Color := Color;
    Canvas.TextOut(X, Y, Str);

    X := X + Canvas.TextWidth(Str);
  end;

var
  Strings: TStringArray;
begin
  Canvas.Font.Bold := True;

  if (CurrentString = '') then
    DrawText(AName, Form.TextColor)
  else
  begin
    Strings := AName.Partition(CurrentString, False);

    DrawText(Strings[0], Form.TextColor);
    DrawText(Strings[1], $00008B);
    DrawText(Strings[2], Form.TextColor);
  end;

  Canvas.Font.Bold := False;
end;

procedure TSimbaAutoComplete.PaintText(Canvas: TCanvas; X, Y: Integer; AText: String);
var
  Highlighter: TSynCustomHighlighter;
  TokStart: PChar;
  TokLen: Integer;
  TokString: String;
begin
  SetLength(TokString, 64);

  Highlighter := Editor.Highlighter;
  Highlighter.ResetRange();
  Highlighter.SetLine(AText, 0);

  while (not Highlighter.GetEol()) do
  begin
    Highlighter.GetTokenEx(TokStart, TokLen);

    if (TokLen > 0) then
    begin
      if (TokLen > Length(TokString)) then
        SetLength(TokString, (Length(TokString) * 2) + TokLen);
      Move(TokStart^, TokString[1], TokLen);

      with Highlighter.GetTokenAttribute() do
      begin
        if (Foreground = clNone) then
          Canvas.Font.Color := clBlack
        else
          Canvas.Font.Color := ColorToRGB(Foreground);

        Canvas.Font.Style := [];
        Canvas.TextOut(X, Y, TokString.CopyRange(1, TokLen));

        X := X + Canvas.TextWidth(TokString.CopyRange(1, TokLen));
      end;
    end;

    Highlighter.Next();
  end;
end;

function TSimbaAutoComplete.DoPaintItem(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
var
  Decl: TDeclaration;
  Text: String;
begin
  Result := True;

  Decl := GetDecl(Index);
  if (Decl <> nil) then
  begin
    Canvas.Brush.Style := bsClear;

    Text := GetHintText(Decl, Canvas = FHintForm.Canvas);
    if (Canvas = FHintForm.Canvas) then
      FHintForm.Caption := Text;

    Y := Y + FDrawOffsetY;

    PaintColumn(Canvas, X, Y, Decl);
    PaintName(Canvas, X, Y, Decl.Name);
    PaintText(Canvas, X, Y, Text);
  end;
end;

function TSimbaAutoComplete.DoMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;

  function Measure(const Text: String; Bold: Boolean): Integer;
  begin
    ACanvas.Font.Bold := Bold;
    Result := ACanvas.TextWidth(Text);
    ACanvas.Font.Bold := False;
  end;

var
  Decl: TDeclaration;
  MaxRight: Integer;
begin
  Decl := GetDecl(Index);

  if (Decl <> nil) then
  begin
    FHintForm.TextWidth := FColumnWidth + Measure(Decl.Name, True) + Measure(GetHintText(Decl, True), False) + FForm.DrawBorderWidth;

    // Either clip to screen right or main form right, whatever is more right.
    MaxRight := Max(Application.MainForm.BoundsRect.Right, FForm.Monitor.BoundsRect.Right);
    if (FForm.Left + FHintForm.TextWidth > MaxRight) then
      FHintForm.TextWidth := MaxRight - FForm.Left;

    Result := TPoint.Create(10000, FForm.FontHeight); // width=10000, so always show
  end else
    Result := TPoint.Create(0, 0);
end;

constructor TSimbaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCodeinsight := TCodeinsight.Create();
  FForm := TSimbaAutoComplete_Form(TheForm);
  FForm.AutoComplete := Self;

  FHintForm := TSimbaAutoComplete_Hint.Create(TheForm);
  FHintForm.AutoComplete := Self;

  FForm.FHint := FHintForm;

  OnPaintItem := @DoPaintItem;
  OnCodeCompletion := @DoCodeCompletion;
  OnSearchPosition := @DoFiltering;
  OnKeyCompletePrefix := @DoTabPressed;
  OnMeasureItem := @DoMeasureItem;
  OnExecute := @DoExecute;

  LongLineHintType := sclpExtendRightOnly;
end;

destructor TSimbaAutoComplete.Destroy;
begin
  if (FCodeinsight <> nil) then
    FreeAndNil(FCodeinsight);

  inherited Destroy();
end;

class function TSimbaAutoComplete.IsAutoCompleteCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
begin
  Result := ((Command = ecChar) and (AChar = '.')) or (Command = AutoCompleteCommand);
end;

class constructor TSimbaAutoComplete.Create;
begin
  AutoCompleteCommand := AllocatePluginKeyRange(1);
end;

end.
