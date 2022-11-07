{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.autocomplete;

{$i simba.inc}

interface

uses
  classes, sysutils, graphics, controls, lcltype,
  synedit, syncompletion, syneditkeycmds,
  simba.mufasatypes, simba.codeparser, simba.codeinsight;

type
  TSimbaAutoComplete_Form = class(TSynCompletionForm)
  protected
    FColumnWidth: Integer;

    procedure DoShow; override;
    procedure DoHide; override;
    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAutoComplete_Hint = class(TSynBaseCompletionHint)
  protected
    FForm: TSimbaAutoComplete_Form;
    FText: String;
    FTextWidth: Integer;
  public
    constructor Create(AOwner: TComponent); override;

    function CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect; override;

    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TSimbaAutoComplete = class(TSynCompletion)
  protected
    FParser: TCodeInsight;
    FGlobals: TDeclarationArray;
    FLocals: TDeclarationArray;

    function HandlePaintList(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;

    procedure HandleCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure HandleFiltering(var NewPosition: Integer);
    procedure HandleTab(Sender: TObject);

    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;

    procedure SetParser(Value: TCodeInsight);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FillGlobals;
    procedure FillMembers(TypeDeclaration: TDeclaration; IsStatic: Boolean);

    property Parser: TCodeInsight read FParser write SetParser;
  end;

implementation

uses
  castaliapaslextypes,
  simba.settings, simba.algo_sort;

procedure TSimbaAutoComplete_Hint.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (FForm <> nil) then
  begin
    ALeft   := HintRect.Left + FForm.FColumnWidth;
    ATop    := HintRect.Top + FForm.DrawBorderWidth;
    AWidth  := FTextWidth + (FForm.DrawBorderWidth * 2) + 4;
    AHeight := FForm.FontHeight - 3;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TSimbaAutoComplete_Hint.CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect;

  function GetProcedureText(Decl: TciProcedureDeclaration): String;
  begin
    Result := Decl.Name + Decl.Items.GetShortText(TciParameterList);
    Result := Result.Replace('constref ', '', [rfReplaceAll]);
    Result := Result.Replace('const ', '', [rfReplaceAll]);

    if (Decl.ReturnType <> nil) then
      Result := Result + ': ' + Decl.Items.GetShortText(TciReturnType);
  end;

  function GetVariableText(Decl: TciVarDeclaration): String;
  begin
    Result := Decl.Name;
    if (Decl.VarType <> nil) then
      Result := Result + ': ' + Decl.VarType.ShortText;
  end;

  function GetTypeText(Decl: TciTypeDeclaration): String;
  begin
    Result := Decl.Name + ' = ' + Decl.Items.GetShortText(TciTypeKind);
  end;

  function GetEnumText(Decl: TciEnumElement): String;
  var
    TypeDecl: TDeclaration;
  begin
    Result := Decl.Name;
    if Decl.HasOwnerClass(TciTypeDeclaration, TypeDecl, True) then
      Result := Result + ': ' + TypeDecl.Name;
  end;

var
  Decl: TDeclaration;
begin
  Decl := FForm.ItemList.Objects[Index] as TDeclaration;
  if Decl is TciProcedureDeclaration then
    FText := GetProcedureText(Decl as TciProcedureDeclaration)
  else
  if Decl is TciVarDeclaration then
    FText := GetVariableText(Decl as TciVarDeclaration)
  else
  if Decl is TciTypeDeclaration then
    FText := GetTypeText(Decl as TciTypeDeclaration)
  else
  if Decl is TciEnumElement then
    FText := GetEnumText(Decl as TciEnumElement)
  else
    FText := '';

  FTextWidth := Canvas.TextWidth(FText);
  if (FTextWidth > 0) then
    Result := TRect.Create(0,0,100000,0) // Always show
  else
    Result := TRect.Create(0,0,0,0);
end;

procedure TSimbaAutoComplete_Hint.Paint;
begin
  Canvas.Font.Color := FForm.TextColor;
  Canvas.Brush.Color := FForm.ClSelect;
  Canvas.FillRect(ClientRect);
  Canvas.TextOut(FForm.DrawBorderWidth + 2, 0, FText);
end;

{$IFDEF WINDOWS}
function SetClassLong(Handle: HWND; Index: Integer = -26; Value: Integer = 0): UInt32; stdcall; external 'user32' name 'SetClassLongA';
{$ENDIF}

constructor TSimbaAutoComplete_Hint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForm := AOwner as TSimbaAutoComplete_Form;

  {$IFDEF WINDOWS}
  SetClassLong(Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

procedure TSimbaAutoComplete_Form.DoShow;
begin
  Font := CurrentEditor.Font;

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

procedure TSimbaAutoComplete_Form.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  if (FHint <> nil) then
  begin
    FHint.Hide();
    FHint.Font := Self.Font;
  end;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    FColumnWidth := Canvas.TextWidth('class const ');
  finally
    Free();
  end;

  FFontHeight := FFontHeight + 2;
end;

constructor TSimbaAutoComplete_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (FHint <> nil) then
    FreeAndNil(FHint);

  FHint := TSimbaAutoComplete_Hint.Create(Self);

  DrawBorderWidth := 4;
  DrawBorderColor := RGBToColor(240, 240, 240);
  BackgroundColor := RGBToColor(240, 240, 240);
  ClSelect        := RGBToColor(159, 180, 208);

  TextColor         := clBlack;
  TextSelectedColor := clBlack;
end;

procedure TSimbaAutoComplete.HandleCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
begin
  if UpperCase(Value) <> UpperCase(SourceValue) then
    Editor.TextBetweenPointsEx[SourceStart, SourceEnd, scamEnd] := Value;

  Editor.CommandProcessor(ecChar, KeyChar, nil);
  if (KeyChar <> '.') and Editor.CanSetFocus() then
    Editor.SetFocus();

  SourceStart := Point(0, 0);
  SourceEnd := Point(0, 0);

  Value := '';
end;

procedure TSimbaAutoComplete.HandleFiltering(var NewPosition: Integer);

  procedure AddSorted(Decls: TDeclarationArray);
  var
    I: Integer;
    List: TStringList;
  begin
    List := TStringList.Create();
    List.UseLocale := False;

    for I := 0 to High(Decls) do
      List.AddObject(Decls[I].Name, Decls[I]);
    List.Sort();
    for I := 0 to List.Count - 1 do
      ItemList.AddObject(List[I], List.Objects[I]);

    List.Free();
  end;

  procedure AddFiltered(Filter: String; Decls: TDeclarationArray; DefaultWeight: Integer = 0);
  var
    I, Count: Integer;
    FilteredDecls: TDeclarationArray;
    Weights: TIntegerArray;
  begin
    SetLength(FilteredDecls, Length(Decls));
    SetLength(Weights, Length(Decls));

    Count := 0;
    for I := 0 to High(Decls) do
      if Decls[I].NameUpper.Contains(Filter) then
      begin
        FilteredDecls[Count] := Decls[i];
        Weights[Count] := DefaultWeight + (100 - Round(Length(Filter) / Length(Decls[I].Name) * 100)) + (Pos(Filter, Decls[I].NameUpper) * 100);

        Inc(Count);
      end;

    if (Count > 0) then
    begin
      if (Count > 1) then
        specialize QuickSortWeighted<TDeclaration, Integer>(FilteredDecls, Weights, 0, Count - 1, True);

      for I := 0 to Count - 1 do
        ItemList.AddObject(FilteredDecls[I].Name, FilteredDecls[I]);
    end;
  end;

begin
  ItemList.BeginUpdate();
  ItemList.Clear();

  if (Self.CurrentString = '') then
  begin
    AddSorted(FLocals);
    AddSorted(FGlobals);
  end else
  begin
    AddFiltered(Self.CurrentString.ToUpper(), FLocals, -$FFFF);
    AddFiltered(Self.CurrentString.ToUpper(), FGlobals);
  end;

  ItemList.EndUpdate();

  if (ItemList.Count > 0) then
    NewPosition := 0
  else
    NewPosition := -1;
end;

procedure TSimbaAutoComplete.HandleTab(Sender: TObject);
begin
  if (OnValidate <> nil) then
    OnValidate(TheForm, '', []);
end;

procedure TSimbaAutoComplete.FillGlobals;
var
  I, Count: Integer;
  Method: TciProcedureDeclaration;
begin
  Count := 0;

  FLocals := FParser.Locals;
  FGlobals := FParser.Globals;
  for I := 0 to High(FGlobals) do
  begin
    if FGlobals[i].ClassType = TciProcedureDeclaration then
    begin
      Method := FGlobals[i] as TciProcedureDeclaration;
      if Method.IsOperator or Method.IsMethodOfType or (tokOverride in Method.Directives) then
        Continue;
    end;

    FGlobals[Count] := FGlobals[I];
    Inc(Count);
  end;
  SetLength(FGlobals, Count);
end;

procedure TSimbaAutoComplete.FillMembers(TypeDeclaration: TDeclaration; IsStatic: Boolean);
var
  I, Count: Integer;
begin
  Count := 0;

  FLocals := nil;
  FGlobals := FParser.GetMembersOfType(TypeDeclaration);
  for I := 0 to High(FGlobals) do
  begin
    if (FGlobals[I].ClassType = TciProcedureDeclaration) and (tokOverride in TciProcedureDeclaration(FGlobals[I]).Directives) then
      Continue;

    if IsStatic then
    begin
      if (FGlobals[I].Owner is TciRecordType) and (FGlobals[I].ClassType = TciClassField) then
        Continue;
      if (FGlobals[I].ClassType = TciProcedureDeclaration) and (not (tokStatic in TciProcedureDeclaration(FGlobals[I]).Directives)) then
        Continue;
    end;

    FGlobals[Count] := FGlobals[I];
    Inc(Count);
  end;
  SetLength(FGlobals, Count);
end;

function TSimbaAutoComplete.GetCompletionFormClass: TSynBaseCompletionFormClass;
begin
  Result := TSimbaAutoComplete_Form;
end;

procedure TSimbaAutoComplete.SetParser(Value: TCodeInsight);
begin
  FGlobals := nil;
  FLocals := nil;

  if (FParser <> nil) then
    FParser.Free();
  FParser := Value;
end;

constructor TSimbaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnPaintItem := @HandlePaintList;
  OnCodeCompletion := @HandleCompletion;
  OnSearchPosition := @HandleFiltering;
  OnKeyCompletePrefix := @HandleTab;
end;

destructor TSimbaAutoComplete.Destroy;
begin
  SetParser(nil);

  inherited Destroy();
end;

function TSimbaAutoComplete.HandlePaintList(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
type
  TColumnFormat = record
    Text: String;
    Color: TColor;
  end;

const
  COLUMN_VAR:         TColumnFormat = (Text: 'var';         Color: clPurple);
  COLUMN_FUNC:        TColumnFormat = (Text: 'function';    Color: clTeal);
  COLUMN_PROC:        TColumnFormat = (Text: 'procedure';   Color: clNavy);
  COLUMN_TYPE:        TColumnFormat = (Text: 'type';        Color: clMaroon);
  COLUMN_ENUM:        TColumnFormat = (Text: 'enum';        Color: clGreen);
  COLUMN_CONST:       TColumnFormat = (Text: 'const';       Color: clOlive);
  COLUMN_CLASS_VAR:   TColumnFormat = (Text: 'class var';   Color: clPurple);
  COLUMN_CLASS_CONST: TColumnFormat = (Text: 'class const'; Color: clOlive);
var
  Declaration: TDeclaration;
  Column: TColumnFormat;
begin
  Column := Default(TColumnFormat);

  Declaration := ItemList.Objects[Index] as TDeclaration;
  if (Declaration is TciProcedureDeclaration) then
  begin
    if TciProcedureDeclaration(Declaration).IsFunction then
      Column := COLUMN_FUNC
    else
      Column := COLUMN_PROC;
  end else
  if (Declaration.Owner is TciRecordType) then
  begin
    if (Declaration is TciClassField) then
      Column := COLUMN_VAR
    else
    if (Declaration is TciConstantDeclaration) then
      Column := COLUMN_CLASS_CONST
    else
    if (Declaration is TciVarDeclaration) then
      Column := COLUMN_CLASS_VAR;
  end else
  begin
    if (Declaration is TciTypeDeclaration) then
      Column := COLUMN_TYPE
    else
    if (Declaration is TciEnumElement) then
      Column := COLUMN_ENUM
    else
    if (Declaration is TciConstantDeclaration) then
      Column := COLUMN_CONST
    else
    if (Declaration is TciVarDeclaration) then
      Column := COLUMN_VAR
    else
    if (Declaration is TciProcedureClassName) then
      Column := COLUMN_VAR;
  end;

  Canvas.Brush.Style := bsClear;

  Canvas.Font.Color := Column.Color;
  Canvas.TextOut(X + 2, Y, Column.Text);

  Canvas.Font.Color := TheForm.TextColor;
  Canvas.TextOut(X + 2 + TSimbaAutoComplete_Form(TheForm).FColumnWidth, Y, Key);

  Result := True;
end;

end.
