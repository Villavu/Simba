{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_paramhint;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LCLType,
  SynEdit, SynEditTypes, SynEditKeyCmds,
  simba.ide_codetools_insight, simba.ide_codetools_parser;

type
  TSimbaParamHintForm = class(THintWindow)
  protected
    FMethods: TDeclarationArray;
    FBoldIndex: Integer;
    FNeededWidth: Integer;
    FNeededHeight: Integer;
    FMeasuring: Boolean;
    FLineHeight: Integer;

    procedure DoHide; override;
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    procedure DrawMethod(var X, Y: Integer; Method: TDeclaration);
    procedure SetBoldIndex(AValue: Integer);
  public
    procedure Show(ScreenPoint: TPoint; Decls: TDeclarationArray);

    property BoldIndex: Integer read FBoldIndex write SetBoldIndex;
  end;

  TSimbaParamHint = class(TLazSynEditPlugin)
  protected
    FHintForm: TSimbaParamHintForm;
    FParenthesesPoint: TPoint;
    FDisplayPoint: TPoint;
    FCodeinsight: TCodeinsight;

    function IsShowing: Boolean;
    function GetParameterIndexAtCaret: Integer;

    procedure DoEditorTopLineChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoEditorFontChanged(Sender: TObject);
    procedure DoEditorCaretMove(Sender: TObject);
    procedure DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
    procedure DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoEditorRemoving(Value: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    class var ParamHintCommand: TSynEditorCommand;
    class function IsParamHintCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
    class constructor Create;
  end;

implementation

uses
  mPasLexTypes, mPasLex,
  simba.editor, simba.mufasatypes, simba.ide_codetools_setup;

procedure TSimbaParamHintForm.SetBoldIndex(AValue: Integer);
begin
  FBoldIndex := AValue;
  if (FBoldIndex = -1) then
    Hide();
end;

procedure TSimbaParamHintForm.DoHide;
begin
  FMethods := nil;

  inherited DoHide();
end;

procedure TSimbaParamHintForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    FLineHeight := Canvas.TextHeight('TaylorSwift');
  finally
    Free();
  end;
end;

procedure TSimbaParamHintForm.Paint;
var
  I: Integer;
  X, Y: Integer;
begin
  if (not FMeasuring) then
  begin
    Canvas.Brush.Color := cl3DLight;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(ClientRect);
  end;

  Y := 2;
  X := 0;

  for I := 0 to High(FMethods) do
  begin
    X := 4;
    DrawMethod(X, Y, FMethods[I]);
    if (X > FNeededWidth) then
      FNeededWidth := X;
  end;

  FNeededWidth := FNeededWidth + 4;
  FNeededHeight := Y + 2;
end;

procedure TSimbaParamHintForm.DrawMethod(var X, Y: Integer; Method: TDeclaration);
var
  ParamIndex: Integer;

  procedure DrawText(Str: String; Bold: Boolean = False);
  begin
    Canvas.Font.Bold := Bold;
    if not FMeasuring then
      Canvas.TextOut(X, Y, Str);
    Inc(X, Canvas.TextWidth(Str));
  end;

  procedure DrawGroup(Group: TDeclaration);
  var
    Decls: TDeclarationArray;
    I: Integer;
    NeedBold: Boolean;
  begin
    Decls := Group.Items.GetItemsOfClass(TDeclaration_Parameter);
    if (Length(Decls) = 0) then
      Exit;

    if FMeasuring then
      NeedBold := True
    else
      NeedBold := (FBoldIndex >= ParamIndex) and (FBoldIndex < ParamIndex + Length(Decls));

    with TDeclaration_Parameter(Decls[0]) do
    begin
      case ParamType of
        tokVar: DrawText('var ', NeedBold);
        tokOut: DrawText('out ', NeedBold);
        tokConst: if (VarTypeString = '') then DrawText('const ', NeedBold);
        tokConstRef: if (VarTypeString = '') then DrawText('constref ', NeedBold);
      end;
    end;

    for I := 0 to High(Decls) do
      with TDeclaration_Parameter(Decls[I]) do
      begin
        if (I > 0) then
          DrawText(', ');
        DrawText(Name, ParamIndex=FBoldIndex);

        Inc(ParamIndex);
      end;

    with TDeclaration_Parameter(Decls[0]) do
    begin
      if (VarTypeString <> '') then
        DrawText(VarTypeString, NeedBold);
      if (VarDefaultString <> '') then
        DrawText(VarDefaultString, NeedBold);
    end;
  end;

var
  NameString, ResultString: String;
  Decl: TDeclaration;
  Decls: TDeclarationArray;
  I: Integer;
begin
  NameString := '';
  ResultString := '';

  if (Method is TDeclaration_Method) then
  begin
    NameString := TDeclaration_Method(Method).Name;
    ResultString := TDeclaration_Method(Method).ResultString;
  end;
  if (Method is TDeclaration_TypeMethod) then
    ResultString := TDeclaration_TypeMethod(Method).ResultString;

  Decl := Method.Items.GetFirstItemOfClass(TDeclaration_ParamList);
  if (Decl = nil) then
    DrawText(NameString + '()' + ResultString + ';')
  else
  begin
    ParamIndex := 0;

    Decls := Decl.Items.GetItemsOfClass(TDeclaration_ParamGroup);
    for I := 0 to High(Decls) do
    begin
      if (I = 0) then
        DrawText(NameString + '(');

      DrawGroup(Decls[I]);

      if (I < High(Decls)) then
        DrawText('; ')
      else
        DrawText(')' + ResultString + ';');
    end;
  end;

  Y := Y + FLineHeight;
end;

procedure TSimbaParamHintForm.Show(ScreenPoint: TPoint; Decls: TDeclarationArray);
var
  ScreenRect: TRect;
  MaxRight: Integer;
  I: Integer;
begin
  FMethods := [];
  for I := 0 to High(Decls) do
  begin
    if Decls[I].isOverrideMethod then
      Continue;

    FMethods := FMethods + [Decls[I]];
  end;

  if (Length(FMethods) = 0) then
  begin
    Hide();
    Exit;
  end;

  FNeededHeight := 0;
  FNeededWidth := 0;
  FMeasuring := True;
  Paint();
  FMeasuring := False;

  ScreenRect.Top := ScreenPoint.Y;
  ScreenRect.Left := ScreenPoint.X;
  ScreenRect.Width := FNeededWidth;
  ScreenRect.Height := FNeededHeight;
  ScreenRect.Offset(-4, -ScreenRect.Height);

  // Either clip to screen right or main form right, whatever is more right.
  MaxRight := Max(Application.MainForm.BoundsRect.Right, Monitor.BoundsRect.Right);
  if (ScreenRect.Right > MaxRight) then
    ScreenRect.Right := MaxRight;

  ActivateWithBounds(ScreenRect, '');
end;

function TSimbaParamHint.IsShowing: Boolean;
begin
  Result := FHintForm.Visible;
end;

function TSimbaParamHint.GetParameterIndexAtCaret: Integer;
var
  Lexer: TmwPasLex;
  BracketCount: Integer;
begin
  Result := -1;
  if ((Editor.CaretX < FParenthesesPoint.X) and (Editor.CaretY < FParenthesesPoint.Y)) or
     ((Editor.CaretX < FParenthesesPoint.X) and (Editor.CaretY = FParenthesesPoint.Y)) then
    Exit;

  Lexer := TmwPasLex.Create(Editor.TextBetweenPoints[FParenthesesPoint, TPoint.Create(Editor.CaretX, Editor.CaretY)]);
  Lexer.NextNoJunk();
  try
    if (Lexer.TokenID <> tokRoundOpen) then
      Exit;

    Result := 0;
    BracketCount := 0;

    while (Lexer.TokenID <> tokNull) do
    begin
      case Lexer.TokenID of
        tokRoundOpen, tokSquareOpen:
          begin
            Inc(BracketCount);
            if (BracketCount = 1) then
              Result := 0;
          end;

        tokRoundClose, tokSquareClose:
          begin
            Dec(BracketCount);
            if (BracketCount = 0) then
            begin
              Result := -1;
              Exit;
            end;
          end;

        tokComma:
          begin
            if (BracketCount = 1) then
              Inc(Result);
          end;
        end;

      Lexer.NextNoJunk();
    end;
  finally
    Lexer.Free();
  end;
end;

procedure TSimbaParamHint.DoEditorTopLineChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  if IsShowing then
  begin
    if (Editor.CaretY < Editor.TopLine) or (Editor.CaretY > Editor.ScreenRowToRow(Max(0, Editor.LinesInWindow - 1))) then
      FHintForm.Visible := False
    else
      FHintForm.Show(Editor.ClientToScreen(Editor.RowColumnToPixels(Editor.LogicalToPhysicalPos(FDisplayPoint))), TDeclarationArray(FHintForm.FMethods));
  end;
end;

procedure TSimbaParamHint.DoEditorFontChanged(Sender: TObject);
begin
  if IsShowing then
    FHintForm.Hide();
end;

procedure TSimbaParamHint.DoEditorCaretMove(Sender: TObject);
begin
  if IsShowing then
  begin
    FHintForm.BoldIndex := GetParameterIndexAtCaret();
    FHintForm.Invalidate();
  end;
end;

procedure TSimbaParamHint.DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);

  // Go back until we find an unclosed (
  function FindParenthesesPoint: TPoint;
  var
    Text: String;
    I, InRound: Integer;
  begin
    Result.X := -1;
    Result.Y := -1;

    Text := Editor.Text;
    if (Editor.SelStart > Length(Text)) then
      Exit;

    InRound := 1;
    for I := Editor.SelStart - 1 downto 1 do
      case Text[I] of
        ')': Inc(InRound);
        '(':
          begin
            Dec(InRound);
            if (InRound = 0) then
            begin
              Result := Editor.CharIndexToRowCol(I-1);
              Exit;
            end;
          end;
      end;
  end;

var
  Text: String;
  I: Integer;
  Decl: TDeclaration;
  Decls: TDeclarationArray;
begin
  if IsParamHintCommand(Command, AChar) and CodetoolsSetup then
  begin
    Handled := True;

    Text := Editor.Text;
    if (Editor.SelStart > Length(Text)) then
      Exit;

    for I := Editor.SelStart - 1 downto 1 do
      if (Text[I] = '(') then
        Break;

    FParenthesesPoint := FindParenthesesPoint();

    FCodeinsight.SetScript(Editor.Text, '', TSimbaEditor(Editor).GetCaretPos(True));
    FCodeinsight.Run();

    Decl := FCodeinsight.ParseExpression(TSimbaEditor(Editor).GetExpression(FParenthesesPoint.X -1, FParenthesesPoint.Y), [EParseExpressionFlag.WantVarType]);
    Decls := [];

    if (Decl is TDeclaration_TypeMethod) then
    begin
      FDisplayPoint := FParenthesesPoint;
      Decls := [Decl];
    end
    else
    if (Decl is TDeclaration_Method) then
    begin
      FDisplayPoint := FParenthesesPoint.Offset(-Length(Decl.Name), 0);
      Decls := FCodeinsight.GetOverloads(Decl);
    end;

    if (Length(Decls) > 0) then
    begin
      FHintForm.Font := Editor.Font;
      FHintForm.BoldIndex := GetParameterIndexAtCaret();
      FHintForm.Show(Editor.ClientToScreen(Editor.RowColumnToPixels(Editor.LogicalToPhysicalPos(FDisplayPoint))), Decls);
    end else
      FHintForm.Hide();
  end;
end;

procedure TSimbaParamHint.DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if IsShowing() and (Key = VK_ESCAPE) then
    Self.FHintForm.Hide();
end;

procedure TSimbaParamHint.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  if (Value is TSimbaEditor) then
    with TSimbaEditor(Value) do
    begin
      RegisterFontChangedHandler(@DoEditorFontChanged);
      RegisterCaretMoveHandler(@DoEditorCaretMove);
      RegisterBeforeKeyDownHandler(@DoEditorKeyDown);
      RegisterCommandHandler(@DoEditorCommand, nil, [hcfPostExec]);
      RegisterStatusChangedHandler(@DoEditorTopLineChanged, [scTopLine]);

      with KeyStrokes.Add() do
      begin
        Key := VK_SPACE;
        Shift := [ssCtrl, ssShift];
        Command := ParamHintCommand;
      end;
    end;
end;

procedure TSimbaParamHint.DoEditorRemoving(Value: TCustomSynEdit);
begin
  if (Value is TSimbaEditor) then
    with TSimbaEditor(Value) do
    begin
      UnRegisterFontChangedHandler(@DoEditorFontChanged);
      UnRegisterCaretMoveHandler(@DoEditorCaretMove);
      UnRegisterBeforeKeyDownHandler(@DoEditorKeyDown);
      UnRegisterCommandHandler(@DoEditorCommand);
      UnRegisterStatusChangedHandler(@DoEditorTopLineChanged);
    end;

  inherited DoEditorRemoving(Value);
end;

destructor TSimbaParamHint.Destroy;
begin
  if (FCodeinsight <> nil) then
    FreeAndNil(FCodeinsight);

  inherited Destroy();
end;

class function TSimbaParamHint.IsParamHintCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
begin
  Result := ((Command = ecChar) and (AChar = '(')) or (Command = ParamHintCommand);
end;

class constructor TSimbaParamHint.Create;
begin
  ParamHintCommand := AllocatePluginKeyRange(1);
end;

constructor TSimbaParamHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCodeinsight := TCodeinsight.Create();
  FHintForm := TSimbaParamHintForm.Create(Self);
end;

end.

