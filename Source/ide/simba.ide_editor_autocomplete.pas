{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Auto complete form (Ctrl + Space)
}
unit simba.ide_editor_autocomplete;

{$i simba.inc}
{$WARN 4046 OFF} // stop compiling on creating a class with an abstract method

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, LCLType,
  SynEdit, SynEditTypes, SynEditKeyCmds, SynEditHighlighter,
  SynCompletion_Simba,
  simba.base, simba.settings,
  simba.ide_codetools_parser, simba.ide_codetools_insight;

type
  TSimbaAutoComplete = class;
  TSimbaAutoComplete_Form = class(TSynCompletionForm)
  protected
    procedure FontChanged(Sender: TObject); override;

    procedure Paint; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    AutoComplete: TSimbaAutoComplete;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAutoComplete_Hint = class(TSynBaseCompletionHint)
  public
    function UseBGThemes: Boolean; override;
    function UseFGThemes: Boolean; override;
    procedure ActivateSub; override;
  public
    AutoComplete: TSimbaAutoComplete;
    TextWidth: Integer;

    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAutoComplete = class(TSynCompletion)
  public
  type
    TPaintItemEvent = procedure(ACanvas: TCanvas; ItemRect: TRect; Index: Integer; Selected: Boolean) of object;
  protected
    FForm: TSimbaAutoComplete_Form;
    FHintForm: TSimbaAutoComplete_Hint;
    FCodeinsight: TCodeinsight;
    FDecls: TDeclarationArray;
    FLocalDecls: TDeclarationArray;

    FOnPaintListItem: TPaintItemEvent;
    FOnPaintHintItem: TPaintItemEvent;

    FFilteredDecls: TDeclarationArray;
    FFilteredWeights: TIntegerArray; // cache

    FColumnWidth: Integer;

    procedure DoSettingChanged_CompletionKey(Setting: TSimbaSetting);

    function GetHintText(Decl: TDeclaration; IsHint: Boolean): String;

    function GetDecl(Index: Integer): TDeclaration;
    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;

    procedure ContinueCompletion(Data: PtrInt);

    procedure PaintColumn(Canvas: TCanvas; var R: TRect; Decl: TDeclaration);
    procedure PaintName(Canvas: TCanvas; var R: TRect; AName: String);
    procedure PaintText(Canvas: TCanvas; var R: TRect; AText: String);

    procedure DoPaintListItem(ACanvas: TCanvas; ItemRect: TRect; Index: Integer; Selected: Boolean);
    procedure DoPaintHintItem(ACanvas: TCanvas; ItemRect: TRect; Index: Integer; Selected: Boolean);

    function DoMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;

    procedure DoPaintSizeDrag(Sender: TObject);
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

    property OnPaintListItem: TPaintItemEvent read FOnPaintListItem write FOnPaintListItem;
    property OnPaintHintItem: TPaintItemEvent read FOnPaintHintItem write FOnPaintHintItem;
  public
    class var AutoCompleteCommand: TSynEditorCommand;
    class function IsAutoCompleteCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
    class constructor Create;
  end;

implementation

uses
  ATCanvasPrimitives,
  simba.array_algorithm, simba.ide_editor, simba.ide_theme, simba.misc,
  simba.ide_codetools_setup, simba.ide_codetools_keywords, simba.vartype_string;

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
  if Assigned(AutoComplete.OnPaintHintItem) then
    AutoComplete.OnPaintHintItem(Canvas, ClientRect, Index, Index = AutoComplete.Position);
end;

constructor TSimbaAutoComplete_Hint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF WINDOWS}
  SetClassLong(Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

function TSimbaAutoComplete_Hint.UseBGThemes: Boolean;
begin
  Result := False;
end;

function TSimbaAutoComplete_Hint.UseFGThemes: Boolean;
begin
  Result := False;
end;

procedure TSimbaAutoComplete_Hint.ActivateSub;
begin
  Visible := False;

  SetBounds(
    HintRect.Left + AutoComplete.Form.DrawBorderWidth,
    HintRect.Top + AutoComplete.Form.DrawBorderWidth,
    TextWidth,
    (HintRect.Bottom - HintRect.Top) - 2
  );

  Visible := True;
end;

procedure TSimbaAutoComplete_Hint.EraseBackground(DC: HDC);
begin
  { nothing }
end;

procedure TSimbaAutoComplete_Form.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FHint <> nil) then
    FHint.Font := Self.Font;

  if (AutoComplete <> nil) then
    AutoComplete.FColumnWidth := Canvas.TextWidth('procedure  ');

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := GetFontSize(Self, 3);

    FFontHeight := Canvas.TextHeight('TaylorSwift');
  finally
    Free();
  end;
end;

procedure TSimbaAutoComplete_Form.Paint;
var
  I: Integer;
  ItemRect: TRect;
begin
  // update scroll bar
  Scroll.Enabled := ItemList.Count > NbLinesInWindow;
  Scroll.Visible := (ItemList.Count > NbLinesInWindow) or ShowSizeDrag;

  if Scroll.Visible and Scroll.Enabled then
  begin
    Scroll.Max := ItemList.Count - 1;
    Scroll.LargeChange := NbLinesInWindow;
    Scroll.PageSize := NbLinesInWindow;
  end else
  begin
    Scroll.PageSize := 1;
    Scroll.Max := 0;
  end;

  for i := 0 to Min(NbLinesInWindow - 1, ItemList.Count - Scroll.Position - 1) do
  begin
    ItemRect.Left   := DrawBorderWidth;
    ItemRect.Right  := Scroll.Left;
    ItemRect.Top    := DrawBorderWidth + FFontHeight * i;
    ItemRect.Bottom := ItemRect.Top + FontHeight;

    if Assigned(AutoComplete.FOnPaintListItem) then
      AutoComplete.FOnPaintListItem(Canvas, ItemRect, i + Scroll.Position, i + Scroll.Position = Position);
  end;

  // draw a rectangle around the window
  if DrawBorderWidth > 0 then
  begin
    Canvas.Brush.Color := DrawBorderColor;
    Canvas.FillRect(0, 0, Width, DrawBorderWidth);
    Canvas.FillRect(Width-DrawBorderWidth, 0, Width, Height);
    Canvas.FillRect(0, Height-DrawBorderWidth, Width, Height);
    Canvas.FillRect(0, 0, DrawBorderWidth, Height);
  end;
end;

procedure TSimbaAutoComplete_Form.DoShow;
begin
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

procedure TSimbaAutoComplete_Form.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OldPosition: Integer;
begin
  OldPosition := Position;

  Position := Scroll.Position + ((Y - DrawBorderWidth) div FFontHeight);
  if DoubleClickSelects and (ssDouble in Shift) and (Position = OldPosition) and Assigned(OnValidate) then
    OnValidate(Self, '', Shift);
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

  if (FItemList <> nil) then
    FItemList.Free();
  FItemList := TVirtualStringList.Create(){%H-};

  DrawBorderWidth := 5;

  {$IFDEF WINDOWS}
  SetClassLong(FHint.Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

procedure TSimbaAutoComplete.ContinueCompletion(Data: PtrInt);
begin
  Editor.CommandProcessor(TSynEditorCommand(Data), #0, nil);
end;

procedure TSimbaAutoComplete.DoCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
var
  Decl: TDeclaration;
begin
  Decl := GetDecl(Position);
  if (Decl <> nil) then
    if Decl.IsName(SourceValue) then
      Value := SourceValue
    else
      Value := Decl.Name
  else
    Value := SourceValue;

  Value := Value + KeyChar;

  case KeyChar of
    '.':      Application.QueueAsyncCall(@ContinueCompletion, TSimbaEditor(Editor).AutoComplete.AutoCompleteCommand);
    ',', '(': Application.QueueAsyncCall(@ContinueCompletion, TSimbaEditor(Editor).ParamHint.ParamHintCommand);
  end;
end;

function CompareDeclarations(const A, B: TDeclaration): Integer;
begin
  Result := CompareText(A.Name, B.Name);
end;

procedure TSimbaAutoComplete.DoFiltering(var NewPosition: Integer);
var
  Filter: String;
  Count: Integer;

  function IgnoreDeclaration(Decls: TDeclarationArray; Index: Integer): Boolean; inline;
  var
    Decl: TDeclaration;
  begin
    Decl := Decls[Index];

    Result := (Decl.Name = '') or ((Decl is TDeclaration_Method) and (TDeclaration_Method(Decl).isOverride or TDeclaration_Method(Decl).isOperator));
  end;

  procedure AddSorted(Decls: TDeclarationArray; StartIndex: Integer);
  var
    I: Integer;
  begin
    for I := 0 to High(Decls) do
    begin
      if IgnoreDeclaration(Decls, I) then
        Continue;
      FFilteredDecls[Count] := Decls[I];
      Inc(Count);
    end;

    specialize TArraySortFunc<TDeclaration>.QuickSort(FFilteredDecls, StartIndex, Count - 1, @CompareDeclarations);
  end;

  procedure AddFiltered(Decls: TDeclarationArray; StartIndex: Integer);
  var
    I: Integer;
    DeclName: String;
  begin
    for I := 0 to High(Decls) do
    begin
      if IgnoreDeclaration(Decls, I) then
        Continue;
      DeclName := Decls[I].Name.ToUpper();
      if (DeclName.IndexOf(Filter) > 0) then
      begin
        FFilteredDecls[Count] := Decls[I];
        FFilteredWeights[Count] := (100 - Round(Length(Filter) / Length(DeclName) * 100)) + (DeclName.IndexOf(Filter) * 100);

        Inc(Count);
      end;
    end;

    specialize TArraySortWeighted<TDeclaration, Integer>.QuickSort(FFilteredDecls, FFilteredWeights, StartIndex, Count - 1, True);
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
    FForm.ClSelect := Editor.SelectedColor.Background;
    FForm.Font := Editor.Font;
  end;
end;

procedure TSimbaAutoComplete.DoSettingChanged_CompletionKey(Setting: TSimbaSetting);
var
  Index: Integer;
begin
  Index := Editor.Keystrokes.FindCommand(AutoCompleteCommand);
  if (Index > -1) then
  begin
    Editor.Keystrokes[Index].Key := SimbaSettings.CodeTools.CompletionKey.Value;
    Editor.Keystrokes[Index].Shift := TShiftState(Integer(SimbaSettings.CodeTools.CompletionKeyModifiers.Value));
  end;
end;

function TSimbaAutoComplete.GetHintText(Decl: TDeclaration; IsHint: Boolean): String;

  function GetMethodText(Decl: TDeclaration_Method): String;
  begin
    if Decl.isProperty then
      Result := PropertyHeader(Decl as TDeclaration_Property, False)
    else
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
  var
    ParentDecl: TDeclaration;
  begin
    Result := '';

    ParentDecl := nil;
    ParentDecl := Decl.ParentByClass[TDeclaration_TypeSet];
    if (ParentDecl = nil) then
      ParentDecl := Decl.Parent;

    if (ParentDecl <> nil) and (ParentDecl.Name <> '') then
      Result := ': ' + ParentDecl.Name;
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
  d: TDeclaration;
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
      FCodeinsight.SetScript(Text, FileName, GetCaretPos(True));
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

        d := FCodeinsight.ParseExpr(Expression);
        if (d is TDeclaration_Method) then
          d := FCodeinsight.ResolveVarType(TDeclaration_Method(d).ResultType);

        if (d is TDeclaration_Var) then
          d := FCodeinsight.ResolveVarType(TDeclaration_Var(d).VarType);
        if (d is TDeclaration_Type) then
          FDecls := FCodeinsight.GetTypeMembers(d as TDeclaration_Type, True);

        FDecls := RemoveDuplicateProperties(FDecls);
      end else
      begin
        Filter := Expression;

        if SimbaSettings.CodeTools.CompletionAddKeywords.Value then
          FDecls := FCodeinsight.GetGlobals() + GetKeywords()
        else
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
        Key := SimbaSettings.CodeTools.CompletionKey.Value;
        Shift := TShiftState(Int32(SimbaSettings.CodeTools.CompletionKeyModifiers.Value));
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

procedure TSimbaAutoComplete.PaintColumn(Canvas: TCanvas; var R: TRect; Decl: TDeclaration);
type
  TColumnFormat = record
    Text: String;
    Color: TColor;
  end;
const
  COLUMN_VAR:     TColumnFormat = (Text: 'var';         Color: $3cb44b);
  COLUMN_FUNC:    TColumnFormat = (Text: 'function';    Color: $ffe119);
  COLUMN_PROC:    TColumnFormat = (Text: 'procedure';   Color: $e632f0);
  COLUMN_PROP:    TColumnFormat = (Text: 'property';    Color: $ff944d);
  COLUMN_TYPE:    TColumnFormat = (Text: 'type';        Color: $45efbf);
  COLUMN_ENUM:    TColumnFormat = (Text: 'enum';        Color: $3182f5);
  COLUMN_CONST:   TColumnFormat = (Text: 'const';       Color: $b1d8ff);
  COLUMN_KEYWORD: TColumnFormat = (Text: 'keyword';     Color: $BB7DC7);
  COLUMN_UNKNOWN: TColumnFormat = (Text: '';            Color: $000000);
var
  Column: TColumnFormat;
begin
  case DeclarationKind(Decl) of
    'property':    Column := COLUMN_PROP;
    'function':    Column := COLUMN_FUNC;
    'procedure':   Column := COLUMN_PROC;
    'type':        Column := COLUMN_TYPE;
    'const':       Column := COLUMN_CONST;
    'var':         Column := COLUMN_VAR;
    'enumelement': Column := COLUMN_ENUM;
    'keyword':     Column := COLUMN_KEYWORD;
    else
      Column := COLUMN_UNKNOWN;
  end;

  Canvas.Font.Color := Column.Color;
  Canvas.TextRect(R, R.Left, R.Top, Column.Text);

  R.Left := FColumnWidth;
end;

procedure TSimbaAutoComplete.PaintName(Canvas: TCanvas; var R: TRect; AName: String);

  procedure DrawText(const Str: String);
  begin
    Canvas.Font.Color := Form.TextColor;
    Canvas.TextRect(R, R.Left, R.Top, Str);

    R.Left := R.Left + Canvas.TextWidth(Str);
  end;

var
  Strings: TStringArray;
begin
  Canvas.Font.Bold := True;

  if (CurrentString <> '') then
  begin
    Strings := AName.Partition(CurrentString, False);

    DrawText(Strings[0]);

    // Underline matching part
    Canvas.Pen.EndCap := pecFlat;
    Canvas.Pen.Color := Form.TextColor;
    Canvas.Line(
      R.Left + 1,
      R.Top  + FontHeight - 4,
      R.Left + Canvas.TextWidth(Strings[1]) - 1,
      R.Top  + FontHeight - 4
    );

    Canvas.Pen.Color := ColorBlendHalf(Form.BackgroundColor, Form.TextColor);
    Canvas.Line(
      R.Left + 1,
      R.Top  + FontHeight - 3,
      R.Left + Canvas.TextWidth(Strings[1]) - 1,
      R.Top  + FontHeight - 3
    );

    DrawText(Strings[1]);
    DrawText(Strings[2]);
  end else
    DrawText(AName);

  Canvas.Font.Bold := False;
end;

procedure TSimbaAutoComplete.PaintText(Canvas: TCanvas; var R: TRect; AText: String);
var
  Highlighter: TSynCustomHighlighter;
  TokStart: PChar;
  TokLen: Integer;
  TokString: String;
begin
  SetLength(TokString, Length(AText));

  Highlighter := Editor.Highlighter;
  Highlighter.ResetRange();
  Highlighter.SetLine(AText, 0);

  while (not Highlighter.GetEol()) do
  begin
    Highlighter.GetTokenEx(TokStart, TokLen);

    if (TokLen > 0) then
    begin
      Move(TokStart^, TokString[1], TokLen);

      with Highlighter.GetTokenAttribute() do
      begin
        if (Foreground = clNone) then
          Canvas.Font.Color := clBlack
        else
          Canvas.Font.Color := ColorToRGB(Foreground);

        Canvas.Font.Style := [];
        Canvas.TextRect(R, R.Left, R.Top, TokString.CopyRange(1, TokLen));

        R.Left := R.Left + Canvas.TextWidth(TokString.CopyRange(1, TokLen));
      end;
    end;

    Highlighter.Next();
  end;
end;

procedure TSimbaAutoComplete.DoPaintListItem(ACanvas: TCanvas; ItemRect: TRect; Index: Integer; Selected: Boolean);
var
  Decl: TDeclaration;
begin
  Decl := GetDecl(Index);

  if (Decl <> nil) then
  begin
    with ACanvas.TextStyle do
      Layout := tlCenter;

    ACanvas.Brush.Style := bsClear;
    if Selected then
    begin
      ACanvas.Brush.Color := Form.ClSelect;
      ACanvas.FillRect(ItemRect);
    end;

    ItemRect.Bottom := ItemRect.Bottom - 2; // Ensure extra space for underline

    PaintColumn(ACanvas, ItemRect, Decl);
    PaintName(ACanvas, ItemRect, Decl.Name);
    PaintText(ACanvas, ItemRect, GetHintText(Decl, False));
  end;
end;

procedure TSimbaAutoComplete.DoPaintHintItem(ACanvas: TCanvas; ItemRect: TRect; Index: Integer; Selected: Boolean);
var
  Decl: TDeclaration;
begin
  Decl := GetDecl(Index);

  if (Decl <> nil) then
  begin
    with ACanvas.TextStyle do
      Layout := tlCenter;

    if Selected then
      ACanvas.Brush.Color := Form.ClSelect
    else
      ACanvas.Brush.Color := Form.BackgroundColor;
    ACanvas.FillRect(ItemRect);

    ItemRect.Bottom := ItemRect.Bottom - 2; // Ensure extra space for underline

    PaintColumn(ACanvas, ItemRect, Decl);

    // Hint window does not include left border
    ItemRect.Left := FColumnWidth - TheForm.DrawBorderWidth;

    PaintName(ACanvas, ItemRect, Decl.Name);
    PaintText(ACanvas, ItemRect, GetHintText(Decl, True));
  end;
end;

function TSimbaAutoComplete.DoMeasureItem(const AKey: string; ACanvas: TCanvas; Selected: boolean; Index: integer): TPoint;
var
  Decl: TDeclaration;
  MaxRight: Integer;
begin
  Decl := GetDecl(Index);

  if (Decl <> nil) then
  begin
    FHintForm.TextWidth := FColumnWidth + FForm.DrawBorderWidth;

    ACanvas.Font.Bold := True;
    FHintForm.TextWidth += ACanvas.TextWidth(Decl.Name);
    ACanvas.Font.Bold := False;
    FHintForm.TextWidth += ACanvas.TextWidth(GetHintText(Decl, True));

    // Either clip to screen right or main form right, whatever is more right.
    MaxRight := Max(Application.MainForm.BoundsRect.Right, FForm.Monitor.BoundsRect.Right);
    if (FForm.Left + FHintForm.TextWidth > MaxRight) then
      FHintForm.TextWidth := MaxRight - FForm.Left;

    Result := TPoint.Create(10000, FForm.FontHeight); // width=10000, so always show
  end else
    Result := TPoint.Create(0, 0);
end;

procedure TSimbaAutoComplete.DoPaintSizeDrag(Sender: TObject);
var
  I: Integer;
begin
  with TSynBaseCompletionFormSizeDrag(Sender) do
  begin
    Canvas.Brush.Color := SimbaTheme.ColorScrollBarInActive;
    Canvas.Pen.Color := SimbaTheme.ColorLine;

    Canvas.FillRect(ClientRect);

    I := 2;
    while (I < Height - 3) do
    begin
      Canvas.MoveTo(ClientRect.Right-I, ClientRect.Bottom-1-1);
      Canvas.LineTo(ClientRect.Right-1, ClientRect.Bottom-I-1);

      Inc(I, 3);
    end;
  end;
end;

constructor TSimbaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCodeinsight := TCodeinsight.Create();
  FForm := TSimbaAutoComplete_Form(TheForm);
  FForm.AutoComplete := Self;
  FForm.SizeDrag.OnPaint := @DoPaintSizeDrag;

  FHintForm := TSimbaAutoComplete_Hint.Create(TheForm);
  FHintForm.AutoComplete := Self;

  FForm.FHint := FHintForm;

  OnPaintListItem := @DoPaintListItem;
  OnPaintHintItem := @DoPaintHintItem;

  OnCodeCompletion := @DoCodeCompletion;
  OnSearchPosition := @DoFiltering;
  OnKeyCompletePrefix := @DoTabPressed;
  OnMeasureItem := @DoMeasureItem;
  OnExecute := @DoExecute;

  LongLineHintType := sclpExtendRightOnly;

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.CodeTools.CompletionKey, @DoSettingChanged_CompletionKey);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.CodeTools.CompletionKeyModifiers, @DoSettingChanged_CompletionKey);
end;

destructor TSimbaAutoComplete.Destroy;
begin
  if (FCodeinsight <> nil) then
    FreeAndNil(FCodeinsight);

  inherited Destroy();
end;

class function TSimbaAutoComplete.IsAutoCompleteCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
begin
  Result := (SimbaSettings.CodeTools.CompletionOpenAutomatically.Value and (Command = ecChar) and (AChar = '.')) or (Command = AutoCompleteCommand);
end;

class constructor TSimbaAutoComplete.Create;
begin
  AutoCompleteCommand := AllocatePluginKeyRange(1);
end;

end.
