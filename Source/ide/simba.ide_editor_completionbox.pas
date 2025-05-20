{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Auto complete form (Ctrl + Space)
}
unit simba.ide_editor_completionbox;

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
  TSimbaCompletionBox_Form = class(TSynCompletionForm)
  protected
    procedure FontChanged(Sender: TObject); override;

    procedure Paint; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TSimbaCompletionBox_Hint = class(TSynCompletionHint)
  protected
    function UseBGThemes: Boolean; override;
    function UseFGThemes: Boolean; override;
    procedure Paint; override;
  public
    function CalcHintRect: TRect; override;
  end;

  TSimbaCompletionBox = class(TSynCompletion)
  protected
    FCodeinsight: TCodeinsight;
    FDecls: TDeclarationArray;
    FLocalDecls: TDeclarationArray;

    FFilteredDecls: TDeclarationArray;
    FFilteredWeights: TIntegerArray; // cache

    FColumnWidth: Integer;

    function GetHintText(Decl: TDeclaration; IsHint: Boolean): String;

    function GetDecl(Index: Integer): TDeclaration;
    function GetCompletionFormClass: TSynCompletionFormClass; override;
    function GetCompletionHintClass: TSynCompletionHintClass; override;

    procedure ContinueCompletion(Data: PtrInt);

    procedure PaintColumn(Canvas: TCanvas; var R: TRect; Decl: TDeclaration);
    procedure PaintName(Canvas: TCanvas; var R: TRect; AName: String);
    procedure PaintText(Canvas: TCanvas; var R: TRect; AText: String);

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
  public
    class function IsAutoCompleteCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
  end;

implementation

uses
  ATCanvasPrimitives,
  simba.array_algorithm, simba.ide_editor, simba.ide_theme, simba.misc,
  simba.ide_codetools_setup, simba.ide_codetools_keywords, simba.vartype_string,
  simba.ide_editor_commands;

{$IFDEF WINDOWS}
function SetClassLong(Handle: HWND; Index: Integer = -26; Value: Integer = 0): UInt32; stdcall; external 'user32' name 'SetClassLongA';
{$ENDIF}

procedure TSimbaCompletionBox_Hint.Paint;
var
  AutoComplete: TSimbaCompletionBox;
  Decl: TDeclaration;
  R: TRect;
begin
  AutoComplete := Completion as TSimbaCompletionBox;

  Decl := AutoComplete.GetDecl(Index);
  if (Decl = nil) then
    Exit;

  R := ClientRect;
  with Canvas, Canvas.TextStyle do
  begin
    Layout := tlCenter;
    if (AutoComplete.Position = Index) then
    begin
      Brush.Color := AutoComplete.SelectedColor;
      FillRect(R);
    end;

    AutoComplete.PaintName(Canvas, R, Decl.Name);
    AutoComplete.PaintText(Canvas, R, AutoComplete.GetHintText(Decl, True));
  end;
end;

function TSimbaCompletionBox_Hint.UseBGThemes: Boolean;
begin
  Result := False;
end;

function TSimbaCompletionBox_Hint.UseFGThemes: Boolean;
begin
  Result := False;
end;

function TSimbaCompletionBox_Hint.CalcHintRect: TRect;
var
  AutoComplete: TSimbaCompletionBox;
  Decl: TDeclaration;
  HintWidth: Integer;
begin
  AutoComplete := Completion as TSimbaCompletionBox;

  Decl := AutoComplete.GetDecl(Index);
  if (Decl = nil) then
    Exit(TRect.Empty);

  Canvas.Font.Bold := True;
  HintWidth := Canvas.TextWidth(Decl.Name);
  Canvas.Font.Bold := False;
  HintWidth := HintWidth + Canvas.TextWidth(AutoComplete.GetHintText(Decl, True));

  with AutoComplete do
    Result.TopLeft := Form.ClientToScreen(
      TPoint.Create(
        Form.DrawBorderWidth + FColumnWidth,
        Form.DrawBorderWidth + FontHeight * (Index - Form.ScrollBar.Position)
      )
    );
  Result.Height := AutoComplete.FontHeight;
  Result.Width := HintWidth;
end;

procedure TSimbaCompletionBox_Form.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FHint <> nil) then
    FHint.Font := Self.Font;

  if (Completion is TSimbaCompletionBox) then
    TSimbaCompletionBox(Completion).FColumnWidth := Canvas.TextWidth('procedure ');

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := GetFontSize(Self, 3);

    FFontHeight := Canvas.TextHeight('Tay');
  finally
    Free();
  end;
end;

procedure TSimbaCompletionBox_Form.Paint;
var
  AutoComplete: TSimbaCompletionBox;
  I: Integer;
  ItemIndex: Integer;
  ItemRect: TRect;
  Decl: TDeclaration;
begin
  AutoComplete := Completion as TSimbaCompletionBox;

  // update ScrollBar bar
  ScrollBar.Enabled := FItemCount > NbLinesInWindow;
  ScrollBar.Visible := (FItemCount > NbLinesInWindow) or ShowSizeDrag;

  if ScrollBar.Visible and ScrollBar.Enabled then
  begin
    ScrollBar.Max := FItemCount - 1;
    ScrollBar.LargeChange := NbLinesInWindow;
    ScrollBar.PageSize := NbLinesInWindow;
  end else
  begin
    ScrollBar.PageSize := 1;
    ScrollBar.Max := 0;
  end;

  Canvas.Brush.Color := BackgroundColor;
  Canvas.FillRect(ClientRect);

  for i := 0 to Min(NbLinesInWindow - 1, FItemCount - ScrollBar.Position - 1) do
  begin
    ItemIndex := i + ScrollBar.Position;
    Decl := AutoComplete.GetDecl(ItemIndex);
    if (Decl = nil) then
      Continue;

    ItemRect.Left   := DrawBorderWidth;
    ItemRect.Right  := ScrollBar.Left;
    ItemRect.Top    := DrawBorderWidth + FFontHeight * i;
    ItemRect.Bottom := ItemRect.Top + FontHeight;

    with Canvas, Canvas.TextStyle do
    begin
      Layout := tlCenter;

      // Selected highlight
      if (AutoComplete.Position = ItemIndex) then
      begin
        Brush.Color := AutoComplete.Form.SelectedColor;
        FillRect(ItemRect);
      end;

      AutoComplete.PaintColumn(Canvas, ItemRect, Decl);
      AutoComplete.PaintName(Canvas, ItemRect, Decl.Name);
      AutoComplete.PaintText(Canvas, ItemRect, AutoComplete.GetHintText(Decl, False));
    end;
  end;

  // draw a rectangle around the window
  if (DrawBorderWidth > 0) then
  begin
    Canvas.Brush.Color := DrawBorderColor;
    Canvas.FillRect(0, 0, Width, DrawBorderWidth);
    Canvas.FillRect(Width-DrawBorderWidth, 0, Width, Height);
    Canvas.FillRect(0, Height-DrawBorderWidth, Width, Height);
    Canvas.FillRect(0, 0, DrawBorderWidth, Height);
  end;
end;

procedure TSimbaCompletionBox_Form.DoShow;
begin
  Width           := SimbaSettings.Editor.AutoCompleteWidth.Value;
  NbLinesInWindow := SimbaSettings.Editor.AutoCompleteLines.Value;

  inherited DoShow();
end;

procedure TSimbaCompletionBox_Form.DoHide;
begin
  inherited DoHide();

  SimbaSettings.Editor.AutoCompleteWidth.Value := Width;
  SimbaSettings.Editor.AutoCompleteLines.Value := NbLinesInWindow;
end;

procedure TSimbaCompletionBox_Form.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OldPosition: Integer;
begin
  OldPosition := Position;

  Position := ScrollBar.Position + ((Y - DrawBorderWidth) div FFontHeight);
  if DoubleClickSelects and (ssDouble in Shift) and (Position = OldPosition) and Assigned(OnValidate) then
    OnValidate(Self, '', Shift);
end;

procedure TSimbaCompletionBox_Form.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ((ScrollBar.Visible) and (X > ScrollBar.Left)) or (Y < DrawBorderWidth) or (Y >= ClientHeight - DrawBorderWidth) then
    Exit;
  if (ScrollBar.Position + (Y - DrawBorderWidth) div FFontHeight) = FHint.Index then
    Exit;

  inherited MouseMove(Shift, X, Y);
end;

procedure TSimbaCompletionBox_Form.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_OEM_COMMA) then
  begin
    OnValidate(Self, ',', Shift);
    Key := 0;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TSimbaCompletionBox.ContinueCompletion(Data: PtrInt);
begin
  Editor.CommandProcessor(TSynEditorCommand(Data), #0, nil);
end;

procedure TSimbaCompletionBox.DoCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);

  function isObjectConstructor(Decl: TDeclaration): Boolean;
  var
    Decls: TDeclarationArray;
  begin
    if (Decl <> nil) and Decl.IsName('Construct') and (Decl is TDeclaration_MethodOfType) then
    begin
      Decls := FCodeinsight.Get(TDeclaration_MethodOfType(Decl).ObjectName);
      Result := (Length(Decls) > 0) and (FCodeinsight.ResolveVarType(Decls[0]) is TDeclaration_TypeObject);
    end else
      Result := False;
  end;

var
  Decl: TDeclaration;
begin
  Decl := GetDecl(Position);

  // special handling for object constructor insert new statement
  if isObjectConstructor(Decl) then
  begin
    Value := '';

    if Length(Editor.LineText) > 0 then
    begin
      // go back until white space
      while (SourceStart.X-1 > 1) and (SourceStart.X-1 <= Length(Editor.LineText)) and (Editor.LineText[SourceStart.X - 1] <> ' ') do
        Dec(SourceStart.X);

      // ensure we dont duplicate new keyword
      if not Copy(Editor.LineText, 1, SourceStart.X-2).EndsWith('new', False) then
        Value := 'new ';
    end;
    Value := Value + TDeclaration_MethodOfType(Decl).ObjectName;
  end else
  begin
    if (Decl <> nil) then
      if Decl.IsName(SourceValue) then
        Value := SourceValue
      else
        Value := Decl.Name
    else
      Value := SourceValue;
  end;

  Value := Value + KeyChar;
  case KeyChar of
    '.':      Application.QueueAsyncCall(@ContinueCompletion, ecCompletionBox);
    ',', '(': Application.QueueAsyncCall(@ContinueCompletion, ecParamHint);
  end;
end;

function CompareDeclarations(const A, B: TDeclaration): Integer;
begin
  Result := CompareText(A.Name, B.Name);
end;

procedure TSimbaCompletionBox.DoFiltering(var NewPosition: Integer);
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

  ItemCount := Count;
end;

procedure TSimbaCompletionBox.DoTabPressed(Sender: TObject);
begin
  if (OnValidate <> nil) then
    OnValidate(Form, '', []);
end;

procedure TSimbaCompletionBox.DoExecute(Sender: TObject);
begin
  if (Editor <> nil) then
  begin
    Form.TextColor := Editor.Highlighter.IdentifierAttribute.Foreground;
    Form.TextSelectedColor := Editor.Highlighter.IdentifierAttribute.Foreground;
    Form.DrawBorderColor := SimbaTheme.ColorScrollBarInActive;
    Form.BackgroundColor := SimbaTheme.ColorScrollBarInActive;
    Form.SelectedColor := Editor.SelectedColor.Background;
    Form.Font := Editor.Font;
    Form.ScrollBar.Position := 0;
  end;
end;

function TSimbaCompletionBox.GetHintText(Decl: TDeclaration; IsHint: Boolean): String;

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

function TSimbaCompletionBox.GetDecl(Index: Integer): TDeclaration;
begin
  if (Index >= 0) and (Index < ItemCount) then
    Result := FFilteredDecls[Index]
  else
    Result := nil;
end;

function TSimbaCompletionBox.GetCompletionFormClass: TSynCompletionFormClass;
begin
  Result := TSimbaCompletionBox_Form;
end;

function TSimbaCompletionBox.GetCompletionHintClass: TSynCompletionHintClass;
begin
  Result := TSimbaCompletionBox_Hint;
end;

procedure TSimbaCompletionBox.DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
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

procedure TSimbaCompletionBox.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  Value.RegisterCommandHandler(@DoEditorCommand, nil, [hcfPostExec]);
end;

procedure TSimbaCompletionBox.DoEditorRemoving(Value: TCustomSynEdit);
begin
  Value.UnRegisterCommandHandler(@DoEditorCommand);

  inherited DoEditorRemoving(Value);
end;

procedure TSimbaCompletionBox.PaintColumn(Canvas: TCanvas; var R: TRect; Decl: TDeclaration);
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

  R.Left += FColumnWidth;
end;

procedure TSimbaCompletionBox.PaintName(Canvas: TCanvas; var R: TRect; AName: String);

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

procedure TSimbaCompletionBox.PaintText(Canvas: TCanvas; var R: TRect; AText: String);
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

procedure TSimbaCompletionBox.DoPaintSizeDrag(Sender: TObject);
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

constructor TSimbaCompletionBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // On Windows remove shadows CS_DROPSHADOW
  {$IFDEF WINDOWS}
  SetClassLong(Form.Handle);
  SetClassLong(Form.Hint.Handle);
  {$ENDIF}

  Form.SizeDrag.OnPaint := @DoPaintSizeDrag;
  Form.DrawBorderWidth := 5;

  FCodeinsight := TCodeinsight.Create();

  OnCodeCompletion := @DoCodeCompletion;
  OnSearchPosition := @DoFiltering;
  OnKeyCompletePrefix := @DoTabPressed;
  OnExecute := @DoExecute;
end;

destructor TSimbaCompletionBox.Destroy;
begin
  if (FCodeinsight <> nil) then
    FreeAndNil(FCodeinsight);

  inherited Destroy();
end;

class function TSimbaCompletionBox.IsAutoCompleteCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
begin
  Result := (SimbaSettings.CodeTools.CompletionOpenAutomatically.Value and (Command = ecChar) and (AChar = '.')) or (Command = ecCompletionBox);
end;

end.
