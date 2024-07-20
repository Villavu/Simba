{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Displays method parameters at the caret.
}
unit simba.ide_editor_paramhint;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LCLType,
  SynEdit, SynEditTypes, SynEditKeyCmds,
  simba.base,
  simba.settings,
  simba.ide_codetools_insight,
  simba.ide_codetools_parser;

type
  TSimbaParamHintForm = class(THintWindow)
  protected const
    BORDER_SIZE = 4;
  protected
    FMethods: TDeclarationArray;
    FBoldIndex: Integer;
    FNeededWidth: Integer;
    FNeededHeight: Integer;
    FMeasuring: Boolean;
    FLineHeight: Integer;
    FIndexing: Boolean;

    procedure DoHide; override;
    procedure Paint; override;
    procedure FontChanged(Sender: TObject); override;

    // Returns the width drawn
    function DrawMethod(var Y: Integer; Method: TDeclaration): Integer;
    function DrawProperty(var Y: Integer; Method: TDeclaration): Integer;

    procedure SetBoldIndex(AValue: Integer);
  public
    procedure Show(ScreenPoint: TPoint; Decls: TDeclarationArray; IsIndexing: Boolean);

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

    procedure DoSettingChanged_ParamHintKey(Setting: TSimbaSetting);

    procedure DoEditorTopLineChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoEditorCaretMove(Sender: TObject);
    procedure DoEditorCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer; HandlerData: Pointer);
    procedure DoEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    procedure DoEditorRemoving(Value: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Form: TSimbaParamHintForm read FHintForm;
  public
    class var ParamHintCommand: TSynEditorCommand;
    class function IsParamHintCommand(Command: TSynEditorCommand; AChar: TUTF8Char): Boolean;
    class constructor Create;
  end;

implementation

uses
  simba.ide_codetools_setup,
  simba.ide_codetools_paslexer,
  simba.ide_editor,
  simba.ide_theme,
  simba.misc;

procedure TSimbaParamHintForm.SetBoldIndex(AValue: Integer);
begin
  FBoldIndex := AValue;
  if (FBoldIndex = -1) then
    Hide();
end;

procedure TSimbaParamHintForm.DoHide;
begin
  FMethods := [];

  inherited DoHide();
end;

procedure TSimbaParamHintForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Size := GetFontSize(Self, 1);

    FLineHeight := Canvas.TextHeight('Fj');
  finally
    Free();
  end;
end;

procedure TSimbaParamHintForm.Paint;
var
  I, Y: Integer;
begin
  if (not FMeasuring) then
  begin
    Canvas.Brush.Color := SimbaTheme.ColorScrollBarActive;
    Canvas.Pen.Color := SimbaTheme.ColorLine;
    Canvas.Rectangle(ClientRect);
  end;

  Y := BORDER_SIZE div 2;
  case FIndexing of
    True:
      for I := 0 to High(FMethods) do
        FNeededWidth := Max(FNeededWidth, DrawProperty(Y, FMethods[I]));
    False:
      for I := 0 to High(FMethods) do
        FNeededWidth := Max(FNeededWidth, DrawMethod(Y, FMethods[I]));
  end;

  FNeededWidth := FNeededWidth + BORDER_SIZE;
  FNeededHeight := Y + BORDER_SIZE div 2;
end;

function TSimbaParamHintForm.DrawMethod(var Y: Integer; Method: TDeclaration): Integer;
var
  ParamIndex: Integer;

  procedure DrawText(Str: String; Bold: Boolean = False);
  begin
    Canvas.Font.Bold := Bold;
    if not FMeasuring then
      Canvas.TextOut(Result, Y, Str);
    Inc(Result, Canvas.TextWidth(Str));
  end;

  procedure DrawGroup(Group: TDeclaration);
  var
    Decls: TDeclarationArray;
    I: Integer;
    NeedBold: Boolean;
  begin
    Decls := Group.Items.GetByClass(TDeclaration_Parameter);
    if (Length(Decls) = 0) then
      Exit;

    if FMeasuring then
      NeedBold := True
    else
      NeedBold := (FBoldIndex >= ParamIndex) and (FBoldIndex < ParamIndex + Length(Decls));

    // Dont clutter with const/constref when it doesn't change anything
    case TDeclaration_Parameter(Decls[0]).ParamType of
      tokVar: DrawText('var ', NeedBold);
      tokOut: DrawText('out ', NeedBold);
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
  Result := BORDER_SIZE;

  NameString := '';
  ResultString := '';

  if (Method is TDeclaration_Method) then
  begin
    NameString := TDeclaration_Method(Method).Name;
    ResultString := TDeclaration_Method(Method).ResultString;
  end;

  Decl := Method.Items.GetByClassFirst(TDeclaration_ParamList);
  if (Decl = nil) then
    DrawText(NameString + '()' + ResultString + ';')
  else
  begin
    ParamIndex := 0;

    Decls := Decl.Items.GetByClass(TDeclaration_ParamGroup);
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

function TSimbaParamHintForm.DrawProperty(var Y: Integer; Method: TDeclaration): Integer;
var
  ParamIndex, ParamCount: Integer;

  procedure DrawText(Str: String; Bold: Boolean = False);
  begin
    Canvas.Font.Bold := Bold;
    if not FMeasuring then
      Canvas.TextOut(Result, Y, Str);
    Inc(Result, Canvas.TextWidth(Str));
  end;

  procedure DrawParam(Name, VarType: String);
  var
    NeedBold: Boolean;
  begin
    if FMeasuring then
      NeedBold := True
    else
      NeedBold := (FBoldIndex >= ParamIndex) and (FBoldIndex < ParamIndex + ParamCount);

    DrawText(Name, NeedBold);
    DrawText(VarType, NeedBold);

    Inc(ParamIndex);
  end;

var
  I: Integer;
  Params: TDeclarationArray;
begin
  Result := BORDER_SIZE;

  Params := Method.Items.GetByClass(TDeclaration_Parameter, True, True);
  ParamCount := Length(Params);
  ParamIndex := 0;
  if (ParamCount = 0) then
    Exit;

  // build header from getter
  if Assigned(TDeclaration_Method(Method).ResultType) then
  begin
    DrawText(TDeclaration_Method(Method).Name + '[');
    for I := 0 to ParamCount - 1 do
    begin
      DrawParam(Params[I].Name, Params[I].Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': '));
      if (I < ParamCount-1) then
        DrawText('; ');
    end;
    DrawText(']' + TDeclaration_Method(Method).ResultString);
  end else
  // build header from setter
  begin
    DrawText(TDeclaration_Method(Method).Name + '[');
    for I := 0 to ParamCount - 2 do
    begin
      DrawParam(Params[I].Name, Params[I].Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': '));
      if (I < ParamCount-2) then
        DrawText('; ');
    end;
    DrawText(']' + Params[ParamCount - 1].Items.GetTextOfClassNoCommentsSingleLine(TDeclaration_VarType, ': '));
  end;

  Y := Y + FLineHeight;
end;

procedure TSimbaParamHintForm.Show(ScreenPoint: TPoint; Decls: TDeclarationArray; IsIndexing: Boolean);
var
  ScreenRect: TRect;
  MaxRight: Integer;
begin
  FIndexing := IsIndexing;
  FMethods := Decls;

  FNeededHeight := 0;
  FNeededWidth := 0;
  FMeasuring := True;
  Paint();
  FMeasuring := False;

  ScreenRect.Top := ScreenPoint.Y;
  ScreenRect.Left := ScreenPoint.X;
  ScreenRect.Width := FNeededWidth;
  ScreenRect.Height := FNeededHeight;
  ScreenRect.Offset(-BORDER_SIZE, -ScreenRect.Height);

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
  Lexer: TPasLexer;
  BracketCount: Integer;
begin
  Result := -1;
  if ((Editor.CaretX < FParenthesesPoint.X) and (Editor.CaretY < FParenthesesPoint.Y)) or
     ((Editor.CaretX < FParenthesesPoint.X) and (Editor.CaretY = FParenthesesPoint.Y)) then
    Exit;

  Lexer := TPasLexer.Create(Editor.TextBetweenPoints[FParenthesesPoint, TPoint.Create(Editor.CaretX, Editor.CaretY)]);
  Lexer.NextNoJunk();
  try
    if (not (Lexer.TokenID in [tokRoundOpen, tokSquareOpen])) then
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

procedure TSimbaParamHint.DoSettingChanged_ParamHintKey(Setting: TSimbaSetting);
var
  Index: Integer;
begin
  Index := Editor.Keystrokes.FindCommand(ParamHintCommand);
  if (Index > -1) then
  begin
    Editor.Keystrokes[Index].Key := SimbaSettings.CodeTools.ParamHintKey.Value;
    Editor.Keystrokes[Index].Shift := TShiftState(Integer(SimbaSettings.CodeTools.ParamHintKeyModifiers.Value));
  end;
end;

procedure TSimbaParamHint.DoEditorTopLineChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  if IsShowing then
    FHintForm.Hide;
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

  // Go back until we find an unclosed `(` or `[` if indexing
  function FindParenthesesPoint(out IsIndexing: Boolean): TPoint;
  var
    Text: String;
    I, InRound: Integer;
  begin
    IsIndexing := False;
    Result.X := -1;
    Result.Y := -1;

    Text := Editor.Text;
    if (Editor.SelStart > Length(Text)) then
      Exit;

    InRound := 1;
    for I := Editor.SelStart - 1 downto 1 do
      case Text[I] of
        ')', ']': Inc(InRound);
        '(', '[':
          begin
            Dec(InRound);
            if (InRound = 0) then
            begin
              Result := Editor.CharIndexToRowCol(I-1){%H-};
              IsIndexing := Text[I] = '[';
              Exit;
            end;
          end;
      end;
  end;

  function FilterProperties(Decls: TDeclarationArray; IsIndexing: Boolean): TDeclarationArray;
  var
    I: Integer;
  begin
    if IsIndexing then
      Result := RemoveDuplicateProperties(Decls)
    else
    begin
      // for `obj.Prop(` to be valid syntax it must return a method
      Result := [];
      for I := 0 to High(Decls) do
        if FCodeinsight.ResolveVarType(TDeclaration_Property(Decls[I]).ResultType) is TDeclaration_TypeMethod then
          Result.Add(Decls[I]);
      Result := RemoveDuplicateProperties(Result);
    end;
  end;

var
  Decl: TDeclaration;
  Members, Decls: TDeclarationArray;
  IsIndexing: Boolean;
  P: TPoint;
begin
  if IsParamHintCommand(Command, AChar) and CodetoolsSetup then
    with TSimbaEditor(Editor) do
    begin
      Handled := True;
      if IsHighlighterAttribute(['Number', 'Comment']) then
        Exit;

      P := FindParenthesesPoint(IsIndexing);
      if ((P.X = -1) and (P.Y = -1)) or (IsShowing and IsIndexing) then
        Exit;

      FParenthesesPoint := P;
      FDisplayPoint := P;

      FCodeinsight.SetScript(Text, FileName, GetCaretPos(True));
      FCodeinsight.Run();

      Decls := [];
      Decl := FCodeinsight.ParseExpr(GetExpression(FParenthesesPoint.X - 1, FParenthesesPoint.Y), Members);

      if (Decl <> nil) then
      begin
        // properties
        if IsIndexing then
        begin
          if (Decl is TDeclaration_Property) then
          begin
            Decls := Members.GetByClassAndName(Decl.Name, TDeclaration_Property);
            Decls := FilterProperties(Decls, IsIndexing);
            Decls := IndexableProperties(Decls);
          end;
        end else
        begin
          // function pointer
          if (Decl is TDeclaration_Var) then
          begin
            Decl := FCodeinsight.ResolveVarType(TDeclaration_Var(Decl).VarType);
            if (Decl is TDeclaration_TypeMethod) then
              Decls := [TDeclaration_TypeMethod(Decl).Method];
          end else
          // properties
          if (Decl is TDeclaration_Property) then
          begin
            Decls := Members.GetByClassAndName(Decl.Name, TDeclaration_Property);
            //Decls := FilterProperties(Decls, IsIndexing);
          end else
          // method of type
          if (Decl is TDeclaration_MethodOfType) then
          begin
            Decls := Members.GetByClassAndName(Decl.Name, TDeclaration_MethodOfType, True);
          end else
          // regular methods
          if (Decl is TDeclaration_Method) then
          begin
            Decls := FCodeinsight.SymbolTable.Get(Decl.Name).GetByClassAndName(Decl.Name, TDeclaration_Method, True);
          end;
        end;

        // remove overrides
        Decls := RemoveOverridenMethods(Decls);
      end;

      if (Length(Decls) > 0) then
      begin
        if (Decl is TDeclaration_Method) then
          FDisplayPoint.X := FDisplayPoint.X - Length(Decl.Name);

        FHintForm.Font := Font;
        FHintForm.Font.Color := Editor.Highlighter.IdentifierAttribute.Foreground;
        FHintForm.BoldIndex := GetParameterIndexAtCaret();
        FHintForm.Show(ClientToScreen(RowColumnToPixels(LogicalToPhysicalPos(FDisplayPoint))), Decls, IsIndexing);
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
      RegisterCaretMoveHandler(@DoEditorCaretMove);
      RegisterBeforeKeyDownHandler(@DoEditorKeyDown);
      RegisterCommandHandler(@DoEditorCommand, nil, [hcfPostExec]);
      RegisterStatusChangedHandler(@DoEditorTopLineChanged, [scTopLine]);

      with KeyStrokes.Add() do
      begin
        Key := SimbaSettings.CodeTools.ParamHintKey.Value;
        Shift := TShiftState(Int32(SimbaSettings.CodeTools.ParamHintKeyModifiers.Value));
        Command := ParamHintCommand;
      end;
    end;
end;

procedure TSimbaParamHint.DoEditorRemoving(Value: TCustomSynEdit);
begin
  if (Value is TSimbaEditor) then
    with TSimbaEditor(Value) do
    begin
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
  Result := (SimbaSettings.CodeTools.ParamHintOpenAutomatically.Value and (Command = ecChar) and ((AChar = '(') or (AChar = '['))) or (Command = ParamHintCommand);
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

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.CodeTools.ParamHintKey, @DoSettingChanged_ParamHintKey);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.CodeTools.ParamHintKeyModifiers, @DoSettingChanged_ParamHintKey);
end;

end.

