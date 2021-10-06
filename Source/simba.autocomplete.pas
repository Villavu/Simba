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
  simba.codeparser, simba.codeinsight, simba.settings;

type
  TSimbaAutoComplete_Form = class;

  TSimbaAutoComplete_ItemList = class(TStringList)
  public
    CurrentString: String;

    constructor Create;
  end;

  TSimbaAutoComplete_Hint = class(TSynBaseCompletionHint)
  protected
    FCompletionForm: TSimbaAutoComplete_Form;
    FText: String;
  public
    constructor Create(AOwner: TComponent); override;

    function CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect; override;

    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  TSimbaAutoComplete_Form = class(TSynCompletionForm)
  protected
    FColumnWidth: Integer;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);

    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ColumnWidth: Integer read FColumnWidth;
  end;

  TSimbaAutoComplete = class(TSynCompletion)
  protected
    FParser: TCodeInsight;
    FDeclarations: TDeclarationList;

    function HandlePaintList(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;

    procedure HandleCompletion(var Value: String; SourceValue: String; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure HandleFiltering(var NewPosition: Integer);
    procedure HandleTab(Sender: TObject);

    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;
    function GetColumnWidth: Integer;

    procedure SetParser(Value: TCodeInsight);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FillGlobals;
    procedure FillMembers(TypeDeclaration: TDeclaration);

    property ColumnWidth: Integer read GetColumnWidth;
    property Parser: TCodeInsight read FParser write SetParser;
  end;

implementation

uses
  castaliapaslextypes, simba.fonthelpers;

procedure TSimbaAutoComplete_Hint.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (FCompletionForm <> nil) then
    with FCompletionForm do
    begin
      if (FText <> '') then
      begin
        ALeft := HintRect.Left + DrawBorderWidth + ColumnWidth - 2;
        ATop := HintRect.Top + DrawBorderWidth;
        AWidth := Self.Canvas.TextWidth(FText) + 4;
        AHeight := FontHeight - 3;
      end;
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
  Result := TRect.Empty;

  Decl := FCompletionForm.ItemList.Objects[Index] as TDeclaration;
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

  if (FText <> '') then
    Result.Right := 100000; // Always show
end;

procedure TSimbaAutoComplete_Hint.Paint;
begin
  Canvas.Brush.Color := FCompletionForm.ClSelect;
  Canvas.FillRect(ClientRect);

  Canvas.Font.Color := FCompletionForm.TextColor;
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(2, 0, FText);
end;

{$IFDEF WINDOWS}
function SetClassLong(hWnd:HWND; nIndex:longint=-26; dwNewLong:Integer=0):DWORD; stdcall; external 'user32' name 'SetClassLongA';
{$ENDIF}

constructor TSimbaAutoComplete_Hint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCompletionForm := AOwner as TSimbaAutoComplete_Form;

  Color := clForm;

  {$IFDEF WINDOWS}
  SetClassLong(Handle); // Clear CS_DROPSHADOW
  {$ENDIF}
end;

constructor TSimbaAutoComplete_ItemList.Create;
begin
  inherited Create();

  OwnsObjects := False;
  UseLocale := False;
  CaseSensitive := False;
  Duplicates := dupAccept;
end;

destructor TSimbaAutoComplete_Form.Destroy;
begin
  SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  inherited Destroy();
end;

procedure TSimbaAutoComplete_Form.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.Editor.FontSize) then
    Font.Size := Setting.Value;

  if (Setting = SimbaSettings.Editor.FontName) then
  begin
    if SimbaFontHelpers.IsFontFixed(Setting.Value) then
      Font.Name := Setting.Value;
  end;

  if SimbaSettings.Editor.AntiAliased.Value then
    Font.Quality := fqCleartypeNatural
  else
    Font.Quality := fqNonAntialiased;
end;

procedure TSimbaAutoComplete_Form.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  FFontHeight := FFontHeight + 2;
  if (FHint <> nil) then
    FHint.Font := Font;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    FColumnWidth := Canvas.TextWidth('class const ');
  finally
    Free();
  end;
end;

constructor TSimbaAutoComplete_Form.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (FItemList <> nil) then
    FreeAndNil(FItemList);
  FItemList := TSimbaAutoComplete_ItemList.Create();

  if (FHint <> nil) then
    FreeAndNil(FHint);
  FHint := TSimbaAutoComplete_Hint.Create(Self);

  DrawBorderWidth := 4;
  DrawBorderColor := RGBToColor(240, 240, 240);
  BackgroundColor := RGBToColor(240, 240, 240);
  ClSelect := RGBToColor(159, 180, 208);

  TextColor := clBlack;
  TextSelectedColor := clBlack;

  SimbaSettingChanged(SimbaSettings.Editor.FontSize);
  SimbaSettingChanged(SimbaSettings.Editor.FontName);
  SimbaSettingChanged(SimbaSettings.Editor.AntiAliased);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
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

function CustomSortCallback(List: TSimbaAutoComplete_ItemList; Left, Right: Integer): Integer;
var
  LeftName, RightName: String;
  LeftDeclaration, RightDeclaration, Declaration: TDeclaration;
  LeftWeight, RightWeight: Integer;
begin
  Result := 0;

  LeftDeclaration := TDeclaration(List.Objects[Left]);
  LeftName := LeftDeclaration.NameUpper;
  LeftWeight := ((100 - Round(Length(List.CurrentString) / Length(LeftName) * 100)) + (Pos(List.CurrentString, LeftName) * 100));

  RightDeclaration := TDeclaration(List.Objects[Right]);
  RightName := RightDeclaration.NameUpper;
  RightWeight := ((100 - Round(Length(List.CurrentString) / Length(RightName) * 100)) + (Pos(List.CurrentString, RightName) * 100));

  // Locals always first
  if LeftDeclaration.HasOwnerClass(TciProcedureDeclaration, Declaration) then
    LeftWeight -= $FFFF;
  if RightDeclaration.HasOwnerClass(TciProcedureDeclaration, Declaration) then
    RightWeight -= $FFFF;

  Result := LeftWeight - RightWeight;
end;

procedure TSimbaAutoComplete.HandleFiltering(var NewPosition: Integer);
var
  I: Integer;
begin
  with ItemList as TSimbaAutoComplete_ItemList do
  begin
    CurrentString := UpperCase(Self.CurrentString);

    BeginUpdate();
    Clear();

    for I := 0 to FDeclarations.Count - 1 do
      if (CurrentString = '') or FDeclarations[I].NameUpper.Contains(CurrentString) then
        AddObject(FDeclarations[i].Name, FDeclarations[i]);

    if (Count > 0) then
    begin
      if (CurrentString <> '') then
        CustomSort(TStringListSortCompare(@CustomSortCallback))
      else
        Sort();

      NewPosition := 0
    end else
      NewPosition := -1;
  end;
end;

procedure TSimbaAutoComplete.HandleTab(Sender: TObject);
begin
  if (OnValidate <> nil) then
    OnValidate(TheForm, '', []);
end;

procedure TSimbaAutoComplete.FillGlobals;
var
  Declaration: TDeclaration;
  Method: TciProcedureDeclaration;
begin
  FDeclarations.Clear();

  for Declaration in FParser.Globals do
  begin
    if Declaration.ClassType = TciProcedureDeclaration then
    begin
      Method := Declaration as TciProcedureDeclaration;
      if Method.IsOperator or Method.IsMethodOfType or (tokOverride in Method.Directives) then
        Continue;
    end;

    FDeclarations.Add(Declaration);
  end;

  FDeclarations.AddRange(FParser.Locals);
end;

procedure TSimbaAutoComplete.FillMembers(TypeDeclaration: TDeclaration);
var
  Declaration: TDeclaration;
begin
  FDeclarations.Clear();

  for Declaration in FParser.GetMembersOfType(TypeDeclaration) do
  begin
    if (Declaration.ClassType = TciProcedureDeclaration) and (tokOverride in TciProcedureDeclaration(Declaration).Directives) then
      Continue;

    FDeclarations.Add(Declaration);
  end;
end;

function TSimbaAutoComplete.GetCompletionFormClass: TSynBaseCompletionFormClass;
begin
  Result := TSimbaAutoComplete_Form;
end;

function TSimbaAutoComplete.GetColumnWidth: Integer;
begin
  Result := TSimbaAutoComplete_Form(TheForm).ColumnWidth;
end;

procedure TSimbaAutoComplete.SetParser(Value: TCodeInsight);
begin
  FDeclarations.Clear();

  if (FParser <> nil) then
    FParser.Free();
  FParser := Value;
end;

constructor TSimbaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDeclarations := TDeclarationList.Create(False);

  OnPaintItem := @HandlePaintList;
  OnCodeCompletion := @HandleCompletion;
  OnSearchPosition := @HandleFiltering;
  OnKeyCompletePrefix := @HandleTab;
end;

destructor TSimbaAutoComplete.Destroy;
begin
  SetParser(nil);

  if (FDeclarations <> nil) then
    FreeAndNil(FDeclarations);

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
      Column := COLUMN_VAR;
  end;

  Canvas.Brush.Style := bsClear;

  Canvas.Font.Color := Column.Color;
  Canvas.TextOut(X, Y, Column.Text);

  Canvas.Font.Color := TheForm.TextColor;
  Canvas.TextOut(X + ColumnWidth, Y, Key);

  Result := True;
end;

end.
