unit simba.autocomplete;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, graphics, lcltype, synedit, syncompletion, synedithighlighter, syneditkeycmds,
  simba.codeparser, simba.codeinsight;

type
  TSimbaAutoComplete = class(TSynCompletion)
  protected
    FSynEdit: TSynEdit;
    FParser: TCodeInsight;
    FDeclarations: TDeclarationList;
    FColumnWidth: Int32;
    FIdentifierColor: TColor;
    FFilterColor: TColor;
    FAlternatingColor: TColor;
    FBackgroundColor: TColor;
    FSelectedColor: TColor;

    procedure SetEditor(const Value: TCustomSynEdit); override;
    procedure SetParser(Value: TCodeInsight);

    procedure PaintName(Canvas: TCanvas; var X, Y: Int32; AName: String);
    procedure PaintColumn(Canvas: TCanvas; var X, Y: Int32; Column: String; Color: Int32); overload;
    procedure PaintText(Canvas: TCanvas; var X, Y: Int32; AText: String); overload;

    function HandlePainting(const AKey: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;

    procedure HandleCompletion(var Value: string; SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure HandleFiltering(var APosition: Int32);
    procedure HandleExecute(Sender: TObject);
  public
    property Parser: TCodeInsight read FParser write SetParser;

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property AlternatingColor: TColor read FAlternatingColor write FAlternatingColor;
    property IdentifierColor: TColor read FIdentifierColor write FIdentifierColor;
    property FilterColor: TColor read FFilterColor write FFilterColor;

    procedure FillGlobalDeclarations;
    procedure FillTypeDeclarations(Declaration: TDeclaration);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

procedure TSimbaAutoComplete.PaintName(Canvas: TCanvas; var X, Y: Int32; AName: String);
var
  i: Int32;
  Start, Stop: Int32;
begin
  Start := Pos(UpperCase(CurrentString), UpperCase(AName));
  Stop := Start + Length(CurrentString) - 1;

  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Style := bsClear;

  for i := 1 to Length(AName) do
  begin
    if (i >= Start) and (i <= Stop) then
      Canvas.Font.Color := FFilterColor
    else
      Canvas.Font.Color := FIdentifierColor;

    Canvas.TextOut(X, Y, AName[i]);

    Inc(X, Canvas.TextWidth(AName[i]));
  end;

  Inc(X);
end;

procedure TSimbaAutoComplete.PaintColumn(Canvas: TCanvas; var X, Y: Int32; Column: String; Color: Int32);
begin
  Canvas.Font.Color := Color;
  Canvas.Font.Style := [];
  Canvas.TextOut(X, Y, Column);

  X := FColumnWidth;
end;

procedure TSimbaAutoComplete.PaintText(Canvas: TCanvas; var X, Y: Int32; AText: String);
var
  Highlighter: TSynCustomHighlighter;
  TokStart: PChar;
  TokLen: Int32;
  TokString: String;
begin
  Highlighter := FSynEdit.Highlighter;
  Highlighter.ResetRange();
  Highlighter.SetLine(AText, 0);

  while (not Highlighter.GetEol()) do
  begin
    Highlighter.GetTokenEx(TokStart, TokLen);

    if (TokLen > 0) then
    begin
      SetLength(TokString, TokLen);

      Move(TokStart^, TokString[1], TokLen);

      with Highlighter.GetTokenAttribute() do
      begin
        if (Foreground = clNone) then
          Canvas.Font.Color := clBlack
        else
          Canvas.Font.Color := ColorToRGB(Foreground);

        Canvas.Font.Style := [];
        Canvas.TextOut(X, Y, TokString);

        X := X + Canvas.TextWidth(TokString);
      end;
    end;

    Highlighter.Next();
  end;
end;

procedure TSimbaAutoComplete.HandleCompletion(var Value: string; SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
begin
  if FParser <> nil then
  begin
    FParser.Free();
    FParser := nil;
  end;

  FSynEdit.TextBetweenPointsEx[SourceStart, SourceEnd, scamEnd] := Value;
  FSynEdit.CommandProcessor(ecChar, KeyChar, nil);

  if KeyChar <> '.' then
  begin
    if FSynEdit.CanFocus then
      FSynEdit.SetFocus();
  end;

  SourceStart := Point(0, 0);
  SourceEnd := Point(0, 0);

  Value := '';
end;

procedure TSimbaAutoComplete.HandleFiltering(var APosition: Int32);
var
  Filter: String;
  i: Int32;
begin
  Filter := UpperCase(CurrentString);

  ItemList.BeginUpdate();
  ItemList.Clear();

  for i := 0 to FDeclarations.Count - 1 do
  begin
    if (Filter = '') or UpperCase(FDeclarations[i].Name).Contains(Filter) then
      ItemList.AddObject(FDeclarations[i].Name, FDeclarations[i]);
  end;

  ItemList.EndUpdate();

  if ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;
end;

procedure TSimbaAutoComplete.HandleExecute(Sender: TObject);
begin
  with TheForm do
  begin
    Font := FSynEdit.Font;

    FColumnWidth := Canvas.TextWidth('procedure  ');
  end;
end;

procedure TSimbaAutoComplete.FillGlobalDeclarations;

  procedure Add(constref Declarations: TDeclarationArray; MethodOfTypes: Boolean);
  var
    i: Int32;
  begin
    for i := 0 to High(Declarations) do
    begin
      if Declarations[i].Name = '' then
        Continue;

      if (Declarations[i] is TciProcedureDeclaration) then
        with Declarations[i] as TciProcedureDeclaration do
        begin
          if (not MethodOfTypes) and IsMethodOfType then
            Continue;
          if IsOperator then
            Continue;
        end;

      if (Declarations[i] is TciProcedureDeclaration) or (Declarations[i] is TciVarDeclaration) or
         (Declarations[i] is TciTypeDeclaration) or (Declarations[i] is TciEnumElement) then
        FDeclarations.Add(Declarations[i]);
    end;
  end;

begin
  FDeclarations.Clear();

  Add(FParser.getLocals(), True);
  Add(FParser.getGlobals(), False);
end;

procedure TSimbaAutoComplete.FillTypeDeclarations(Declaration: TDeclaration);
begin
  FDeclarations.Clear();

  if Declaration is TciTypeDeclaration then
    FDeclarations.Extend(FParser.getGlobalTypeMembers(Declaration as TciTypeDeclaration))
  else
  if Declaration is TciRecordType then
    FDeclarations.Extend(Declaration.Items.GetItemsOfClass(TciClassField));
end;

procedure TSimbaAutoComplete.SetEditor(const Value: TCustomSynEdit);
begin
  inherited SetEditor(Value);

  FSynEdit := Value as TSynEdit;
end;

procedure TSimbaAutoComplete.SetParser(Value: TCodeInsight);
begin
  FDeclarations.Clear();

  if (FParser <> nil) then
  begin
    FParser.Free();
    FParser := nil;
  end;

  FParser := Value;
end;

constructor TSimbaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDeclarations := TDeclarationList.Create(False);
  FIdentifierColor := clBlack;
  FFilterColor := clMaroon;

  OnPaintItem := @HandlePainting;
  OnCodeCompletion := @HandleCompletion;
  OnSearchPosition := @HandleFiltering;
  OnExecute := @HandleExecute;

  TStringList(ItemList).Sorted := True;
  TStringList(ItemList).Duplicates := dupAccept;
  TStringList(ItemList).OwnsObjects := False;
end;

destructor TSimbaAutoComplete.Destroy;
begin
  SetParser(nil);

  FDeclarations.Free();

  inherited Destroy();
end;

function TSimbaAutoComplete.HandlePainting(const AKey: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;

  procedure PaintType(Declaration: TciTypeDeclaration);
  begin
    PaintColumn(Canvas, X, Y, 'type', clMaroon);
    PaintName(Canvas, X, Y, Declaration.Name);
    PaintText(Canvas, X, Y, ': ' + Declaration.Items.GetShortText(TciTypeKind));
  end;

  procedure PaintProcedure(Declaration: TciProcedureDeclaration);
  begin
    if Declaration.IsFunction then
      PaintColumn(Canvas, X, Y, 'function', clTeal)
    else
      PaintColumn(Canvas, X, Y, 'procedure', clNavy);

    PaintName(Canvas, X, Y, Declaration.Name);
    PaintText(Canvas, X, Y, Declaration.Items.GetShortText(TciParameterList));
    if Declaration.ReturnType <> nil then
      PaintText(Canvas, X, Y, ': ' + Declaration.Items.GetShortText(TciReturnType));
  end;

  procedure PaintVariable(Declaration: TciVarDeclaration);
  begin
    if Declaration is TciConstantDeclaration then
      PaintColumn(Canvas, X, Y, 'constant', $0F8CA8)
    else
      PaintColumn(Canvas, X, Y, 'variable', clPurple);

    PaintName(Canvas, X, Y, Declaration.Name);

    if Declaration.VarType <> nil then
      PaintText(Canvas, X, Y, ': ' + Declaration.VarType.ShortText);
  end;

  procedure PaintProcedureVariable(Declaration: TciTypeKind);
  begin
    PaintColumn(Canvas, X, Y, 'variable', clPurple);
    PaintName(Canvas, X, Y, Declaration.Name);
    PaintText(Canvas, X, Y, ': ' + Declaration.ShortText);
  end;

  procedure PaintEnumElement(Declaration: TciEnumElement);
  var
    TypeDeclaration: TDeclaration;
  begin
    PaintColumn(Canvas, X, Y, 'enum', $0094E5);
    PaintName(Canvas, X, Y, Declaration.Name);
    if Declaration.HasOwnerClass(TciTypeDeclaration, TypeDeclaration, True) then
      PaintText(Canvas, X, Y, ': ' + TypeDeclaration.Name);
  end;

var
  Declaration: TDeclaration;
begin
  Declaration := ItemList.Objects[Index] as TDeclaration;

  if Selected then
    Canvas.Brush.Color := SelectedColor
  else
  if Odd(Index) then
    Canvas.Brush.Color := AlternatingColor
  else
    Canvas.Brush.Color := BackgroundColor;

  {
  if Selected then
    Canvas.Brush.Color := $BE9270
  else
  if Odd(Index) then
    Canvas.Brush.Color := $F0F0F0
  else
    Canvas.Brush.Color := $FFFFFF;
  }
  Canvas.FillRect(X, Y, TheForm.Width, Y + FontHeight);

  X := 3;

  if (Declaration is TciProcedureDeclaration) then
    PaintProcedure(Declaration as TciProcedureDeclaration)
  else
  if (Declaration is TciTypeDeclaration) then
    PaintType(Declaration as TciTypeDeclaration)
  else
  if (Declaration is TciVarDeclaration) then
    PaintVariable(Declaration as TciVarDeclaration)
  else
  if (Declaration is TciEnumElement) then
    PaintEnumElement(Declaration as TciEnumElement)
  else
  if (Declaration is TciReturnType) or (Declaration is TciProcedureClassName) then
    PaintProcedureVariable(Declaration as TciTypeKind);

  Result := True;
end;

end.

