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
  Classes, SysUtils, Graphics, Controls, Forms, LCLType,
  SynEdit, SynEditTypes, SynCompletion, SynEditKeyCmds,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_insight;

type
  TSimbaAutoComplete = class;
  TSimbaAutoComplete_Form = class(TSynCompletionForm)
  protected
    FAutoComplete: TSimbaAutoComplete;

    procedure DoShow; override;
    procedure DoHide; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAutoComplete_Hint = class(THintWindow)
  protected
    FAutoComplete: TSimbaAutoComplete;
    FName: String;
    FText: String;
    FItemIndex: Integer;

    procedure DoHide; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  end;

  TSimbaAutoComplete = class(TSynCompletion)
  protected
    FForm: TSimbaAutoComplete_Form;
    FHintForm: TSimbaAutoComplete_Hint;
    FCodeinsight: TCodeinsight;
    FDecls: TDeclarationArray;

    FFilteredDecls: TDeclarationArray;
    FFilteredWeights: TIntegerArray; // cache

    FColumnWidth: Integer;

    function GetDecl(Index: Integer): TDeclaration;
    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;

    procedure ShowHint(Index: Integer);
    procedure ShowHintForm(Data: PtrInt);
    procedure ContinueCompletion(Data: PtrInt);

    procedure PaintColumn(Canvas: TCanvas; X, Y: Integer; Decl: TDeclaration);
    procedure PaintName(Canvas: TCanvas; X, Y: Integer; AName: String);

    function DoPaintItem(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
    procedure DoPositionChanged(Sender: TObject);
    procedure DoCodeCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure DoFiltering(var NewPosition: Integer);
    procedure DoTabPressed(Sender: TObject);

    procedure DoEditorFontChanged(Sender: TObject);
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
  simba.settings, simba.algo_sort, simba.editor, simba.ide_codetools_setup;

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
  Canvas.Brush.Color := FAutoComplete.SelectedColor;
  Canvas.Font.Color := FAutoComplete.Form.TextColor;
  Canvas.FillRect(ClientRect);
  Canvas.TextOut(FAutoComplete.Form.DrawBorderWidth + Canvas.TextWidth(FName), 0, FText);

  FAutoComplete.PaintName(Canvas, FAutoComplete.Form.DrawBorderWidth, 0, FName);
end;

procedure TSimbaAutoComplete_Hint.DoHide;
begin
  FItemIndex := -1;

  inherited DoHide();
end;

constructor TSimbaAutoComplete_Hint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItemIndex := -1;

  {$IFDEF WINDOWS}
  SetClassLong(Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

procedure TSimbaAutoComplete_Hint.EraseBackground(DC: HDC);
begin
  { nothing }
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

  FAutoComplete.FHintForm.Hide();

  SimbaSettings.Editor.AutoCompleteWidth.Value := Width;
  SimbaSettings.Editor.AutoCompleteLines.Value := NbLinesInWindow;
end;

procedure TSimbaAutoComplete_Form.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if ((Scroll.Visible) and (X > Scroll.Left)) or (Y < DrawBorderWidth) or (Y >= ClientHeight - DrawBorderWidth) then
    Exit;

  FAutoComplete.ShowHint(Scroll.Position + (Y - DrawBorderWidth) div FFontHeight);
end;

constructor TSimbaAutoComplete_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (FItemList <> nil) then
    FItemList.Free();
  FItemList := TVirtualStringList.Create();

  DrawBorderWidth := 4;
  DrawBorderColor := RGBToColor(240, 240, 240);
  BackgroundColor := RGBToColor(240, 240, 240);
  ClSelect        := RGBToColor(159, 180, 208);

  TextColor         := clBlack;
  TextSelectedColor := clBlack;
end;

procedure TSimbaAutoComplete.ContinueCompletion(Data: PtrInt);
begin
  Editor.CommandProcessor(ecChar, '.', nil);
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

  if (KeyChar = '.') then
    Application.QueueAsyncCall(@ContinueCompletion, 0);
end;

function CompareDeclarations(A, B: TDeclaration): Integer;
begin
  Result := CompareText(A.Name, B.Name);
end;

procedure TSimbaAutoComplete.DoFiltering(var NewPosition: Integer);
var
  Count: Integer;

  procedure AddSorted;
  var
    Item: TDeclaration;
  begin
    for Item in FDecls do
    begin
      if (Item is TDeclaration_Method) and ((TDeclaration_Method(Item).MethodType = mtOperator) or TDeclaration_Method(Item).IsOverride()) then
        Continue;

      FFilteredDecls[Count] := Item;

      Inc(Count);
    end;

    if (Count > 1) then
      specialize QuickSortFunc<TDeclaration>(FFilteredDecls, 0, Count - 1, @CompareDeclarations);
  end;

  procedure AddFiltered(Filter: String; DefaultWeight: Integer = 0);
  var
    Decl: TDeclaration;
    DeclName: String;
  begin
    Filter := Filter.ToUpper();

    for Decl in FDecls do
    begin
      DeclName := Decl.Name.ToUpper();
      if (DeclName.IndexOf(Filter) > 0) then
      begin
        FFilteredDecls[Count] := Decl;
        FFilteredWeights[Count] := DefaultWeight + (100 - Round(Length(Filter) / Length(DeclName) * 100)) + (DeclName.IndexOf(Filter) * 100);

        Inc(Count);
      end;
    end;

    if (Count > 1) then
      specialize QuickSortWeighted<TDeclaration, Integer>(FFilteredDecls, FFilteredWeights, 0, Count - 1, True);
  end;

begin
  if (Length(FFilteredDecls) < Length(FDecls)) then
    SetLength(FFilteredDecls, Length(FDecls));
  if (Length(FFilteredWeights) < Length(FDecls)) then
    SetLength(FFilteredWeights, Length(FDecls));

  Count := 0;

  if (CurrentString = '') then
    AddSorted()
  else
    AddFiltered(CurrentString);

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

procedure TSimbaAutoComplete.DoEditorFontChanged(Sender: TObject);
begin
  FForm.Font := TSynEdit(Sender).Font;
  FHintForm.Font := TSynEdit(Sender).Font;

  with TBitmap.Create() do
  try
    Canvas.Font := TSynEdit(Sender).Font;

    FColumnWidth := Canvas.TextWidth('class const ');
  finally
    Free();
  end;
end;

procedure TSimbaAutoComplete.ShowHint(Index: Integer);

  function GetMethodText(Decl: TDeclaration_Method): String;
  begin
    Result := Decl.ParamString + Decl.ResultString;
  end;

  function GetVarText(Decl: TDeclaration_Var): String;
  begin
    Result := Decl.VarTypeString + Decl.VarDefaultString;
  end;

  function GetEnumElementText(Decl: TDeclaration): String;
  begin
    if (Decl.Owner <> nil) then
      Result := ': ' + Decl.Owner.Name
    else
      Result := '';
  end;

  function GetRecordText(Decl: TDeclaration): String;
  begin
    Result := ' = record';
  end;

  function GetTypeText(Decl: TDeclaration): String;
  begin
    Result := ' = ' + Decl.TextNoCommentsSingleLine;
  end;

var
  Decl: TDeclaration;
  R: TRect;
  MaxRight: Integer;
begin
  Decl := GetDecl(Index);
  if (Decl = nil) or (not FForm.Showing) then
  begin
    FHintForm.Visible := False;
    Exit;
  end;

  if FHintForm.Visible and (FHintForm.FItemIndex = Index) then
    Exit;

  with FHintForm do
  begin
    FItemIndex := Index;
    FName := Decl.Name;

    if (Decl is TDeclaration_Method)      then FText := GetMethodText(Decl as TDeclaration_Method) else
    if (Decl is TDeclaration_Var)         then FText := GetVarText(Decl as TDeclaration_Var)       else
    if (Decl is TDeclaration_EnumElement) then FText := GetEnumElementText(Decl)                   else
    if (Decl is TDeclaration_TypeRecord)  then FText := GetRecordText(Decl)                        else
    if (Decl is TDeclaration_Type)        then FText := GetTypeText(Decl)                          else
                                               FText := '';

    R := TRect.Create(Form.ClientToScreen(TPoint.Create(FColumnWidth, Form.DrawBorderWidth + (Index - Form.Scroll.Position) * FontHeight)));
    R.Width := Canvas.TextWidth(FName + FText);
    R.Height := FontHeight - 3;
    R.Inflate(Form.DrawBorderWidth, 0);

    // Either clip to screen right or main form right, whatever is more right.
    MaxRight := Max(Application.MainForm.BoundsRect.Right, Monitor.BoundsRect.Right);
    if (R.Right > MaxRight) then
      R.Right := MaxRight;

    HintRect := R;

    Application.QueueAsyncCall(@ShowHintForm, 0);
  end;
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
  begin
    Handled := True;

    FForm.Font := TSynEdit(Sender).Font;
    FHintForm.Font := TSynEdit(Sender).Font;

    FDecls := [];
    FCodeinsight.SetScript(Editor.Text, '', Editor.SelStart, Editor.SelStart);
    FCodeinsight.Run();

    Expression := TSimbaEditor(Editor).GetExpression(Editor.CaretX - 1, Editor.CaretY);
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
    end;

    StartPoint := Editor.CaretXY;
    StartPoint.X := StartPoint.X - Length(Filter);

    with Editor.ClientToScreen(Editor.RowColumnToPixels(StartPoint)) do
      Execute(Filter, X, Y + Editor.LineHeight);
  end;
end;

procedure TSimbaAutoComplete.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  if (Value is TSimbaEditor) then
    with TSimbaEditor(Value) do
    begin
      RegisterFontChangedHandler(@DoEditorFontChanged);
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
    begin
      UnRegisterFontChangedHandler(@DoEditorFontChanged);
      UnRegisterCommandHandler(@DoEditorCommand);
    end;

  inherited DoEditorRemoving(Value);
end;

procedure TSimbaAutoComplete.PaintColumn(Canvas: TCanvas; X, Y: Integer; Decl: TDeclaration);
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
  COLUMN_CLASS_CONST: TColumnFormat = (Text: 'class const'; Color: clOlive);
var
  Column: TColumnFormat;
begin
  if (Decl is TDeclaration_Method) then
  begin
    if TDeclaration_Method(Decl).MethodType in [mtFunction, mtObjectFunction] then
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

  Canvas.Font.Bold := True;
  Canvas.Font.Color := Column.Color;
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(X + 2, Y, Column.Text);
  Canvas.Font.Bold := False;
end;

procedure TSimbaAutoComplete.PaintName(Canvas: TCanvas; X, Y: Integer; AName: String);

  procedure DrawText(Str: String; Color: TColor);
  begin
    Canvas.Font.Color := Color;
    Canvas.TextOut(X, Y, Str);

    X := X + Canvas.TextWidth(Str);
  end;

var
  Strings: TStringArray;
begin
  if (CurrentString = '') then
    DrawText(AName, Form.TextColor)
  else
  begin
    Strings := AName.Partition(CurrentString, False);

    DrawText(Strings[0], Form.TextColor);
    DrawText(Strings[1], $00008B);
    DrawText(Strings[2], Form.TextColor);
  end;
end;

function TSimbaAutoComplete.DoPaintItem(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
var
  Decl: TDeclaration;
begin
  Result := True;

  Decl := GetDecl(Index);
  if (Decl = nil) then
    Exit;

  PaintColumn(Canvas, X, Y, Decl);
  PaintName(Canvas, FColumnWidth, Y, Decl.Name);
end;

procedure TSimbaAutoComplete.DoPositionChanged(Sender: TObject);
begin
  ShowHint(Position);
end;

procedure TSimbaAutoComplete.ShowHintForm(Data: PtrInt);
begin
  FHintForm.Paint();
  FHintForm.ActivateHint('');
end;

constructor TSimbaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCodeinsight := TCodeinsight.Create();
  FForm := TSimbaAutoComplete_Form(TheForm);
  FForm.FAutoComplete := Self;
  FHintForm := TSimbaAutoComplete_Hint.Create(TheForm);
  FHintForm.FAutoComplete := Self;

  OnPaintItem := @DoPaintItem;
  OnCodeCompletion := @DoCodeCompletion;
  OnSearchPosition := @DoFiltering;
  OnKeyCompletePrefix := @DoTabPressed;
  OnPositionChanged := @DoPositionChanged;

  LongLineHintType := sclpNone;
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
