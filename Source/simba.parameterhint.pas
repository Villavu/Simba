{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.parameterhint;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, extctrls, synedit,
  simba.codeinsight, simba.codeparser;

type
  TSimbaParameterHint = class(THintWindow)
  protected const
    FBorderX = 4;
    FBorderY = 2;
  protected
    LastParameterIndex: Int32;
    FSynEdit: TSynEdit;
    FStartPoint: TPoint;
    FBracketPoint: TPoint;
    FParser: TCodeInsight;
    FDeclarations: array of TciProcedureDeclaration;
    FParameters: array of TDeclarationArray;

    procedure Paint; override;

    procedure HandleEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleEditorCaretChange(Sender: TObject);

    procedure DrawHints(var Bounds: TRect; Measure: Boolean);
    function PrepareParamString(Index: Int32; out Str: String): Integer;

    procedure SetEditor(Value: TSynEdit);
    procedure SetParser(Value: TCodeInsight);
  public
    property Editor: TSynEdit read FSynEdit write SetEditor;
    property Parser: TCodeInsight read FParser write SetParser;

    procedure CalculateBounds;
    procedure Execute(BracketPoint: TPoint; Methods: TDeclarationArray);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  lclintf, lcltype, castaliapaslex, castaliapaslextypes, types;

type
  TSynEdit_Helper = class helper for TSynEdit
  public
    procedure AddHandlerOnCaretChange(Handler: TNotifyEvent);
  end;

procedure TSynEdit_Helper.AddHandlerOnCaretChange(Handler: TNotifyEvent);
begin
  GetCaretObj().AddChangeHandler(Handler);
end;

function TSimbaParameterHint.PrepareParamString(Index: Int32; out Str: String): Integer;
var
  Lexer: TmwPasLex;
  BracketCount, ParameterIndex, ParamC: Int32;
  ParamNames: TStringArray;
  TypeDecl: TDeclaration;
  s, TypeStr, Params: string;
  i, ii, Group: Int32;
begin
  Result := -1;

  if (FSynEdit = nil) then
    Exit;
  if (Index < 0) or (Index >= Length(FParameters)) then
    Exit;

  Lexer := TmwPasLex.Create();

  try
    Lexer.Origin := PChar(FSynEdit.TextBetweenPoints[FBracketPoint, FSynEdit.CaretXY]);

    BracketCount := 0;
    ParameterIndex := -1;

    while (Lexer.TokenID <> tokNull) do
    begin
      case Lexer.TokenID of
        tokRoundOpen, tokSquareOpen:
          begin
            Inc(BracketCount);
            if BracketCount = 1 then
              ParameterIndex := 0;
          end;

        tokRoundClose, tokSquareClose:
          begin
            Dec(BracketCount);
            if BracketCount =0 then
              Exit;
          end;

        tokComma:
          begin
            if BracketCount = 1 then
              Inc(ParameterIndex);
          end;
        end;

      Lexer.NextNoJunk();
    end;
  finally
    Lexer.Free();
  end;

  if ParameterIndex = -1 then
    Exit;

  if ParameterIndex = LastParameterIndex then
  begin
    Result := ParameterIndex;
    Exit;
  end;

  str := '';
  ParamC := 0;

  group := 0;
  i := 0;
  while (i < Length(FParameters[Index])) do
  begin
    // Seems pointless to show these. Just clutter.
    //if (FParameters[Index][i] is TciConstRefParameter) then
    //  s := 'constref '
    //else if (FParameters[Index][i] is TciConstParameter) then
    //  s := 'const '
    if (FParameters[Index][i] is TciOutParameter) then
      s := 'out '
    //else if (FParameters[Index][i] is TciInParameter) then
    //  s := 'in '
    else if (FParameters[Index][i] is TciVarParameter) then
      s := 'var '
    else
      s := '';

    group := TciParameter(FParameters[Index][i]).Group;
    SetLength(ParamNames, 0);

    TypeDecl := FParameters[Index][i].Items.GetFirstItemOfClass(TciTypeKind);
    while (i < Length(FParameters[Index])) and (TciParameter(FParameters[Index][i]).Group = group) do
    begin
      SetLength(ParamNames, Length(ParamNames) + 1);
      ParamNames[High(ParamNames)] := TciParameter(FParameters[Index][i]).Name;
      Inc(i);
    end;

    if TypeDecl <> nil then
      TypeStr := ': ' + TypeDecl.ShortText
    else
      TypeStr := '';

    Params := '';
    for ii := 0 to high(ParamNames) do
    begin
      if ParameterIndex = ParamC then //Found the current parameter index in the parameterdecl!
      begin
        if s <> '' then
          s := '\' + s + '\'; //If it has a const/var/in/out thingy, bold this as well
        if TypeStr <> '' then        //If has a type then bold the type
          TypeStr := '\' + TypeStr + '\';
        if Params <> '' then
          Params := Params +', \' + ParamNames[ii] + '\'
        else
          Params := '\' + ParamNames[ii] + '\';
      end else
      begin
        if Params <> '' then
          Params := Params +', ' +  ParamNames[ii]
        else
          Params := ParamNames[ii]
      end;
      inc(ParamC);
    end;

    if str <> '' then
      str := str + '; ' + s + Params + TypeStr
    else
      str := s + params + TypeStr;
  end;

  if (FDeclarations[Index] <> nil) then
    TypeDecl := FDeclarations[Index].ReturnType
  else
    TypeDecl := nil;

  if TypeDecl <> nil then
    TypeStr := ': ' + TypeDecl.ShortText
  else
    TypeStr := '';

  str :=  '(' +  str + ')' + TypeStr + ';';

  if (FDeclarations[Index] <> nil) and (FDeclarations[Index].Name <> '') then
    str := FDeclarations[Index].Name + str;
  str := StringReplace(str, '\\', '', [rfReplaceAll]); //Delete all the \\, something like \const \\x\ is the same as \const x\

  Result := parameterindex;
end;

procedure TSimbaParameterHint.SetEditor(Value: TSynEdit);
begin
  if FSynEdit = Value then
    Exit;

  FSynEdit := Value;
  FSynEdit.AddHandlerOnCaretChange(@HandleEditorCaretChange);
  FSynEdit.AddHandlerOnKeyDown(@HandleEditorKeyDown);
end;

procedure TSimbaParameterHint.SetParser(Value: TCodeInsight);
begin
  if (FParser <> nil) then
    FParser.Free();

  SetLength(FParameters, 0);
  SetLength(FDeclarations, 0);

  FParser := Value;
end;

destructor TSimbaParameterHint.Destroy;
begin
  SetParser(nil);

  inherited Destroy();
end;

constructor TSimbaParameterHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoHide := False;
  LastParameterIndex := -1;
end;

procedure TSimbaParameterHint.CalculateBounds;
var
  MonitorRect: TRect;
  ScreenPoint: TPoint;
  R: TRect;
begin
  Font := FSynEdit.Font;

  ScreenPoint := FSynEdit.ClientToScreen(FSynEdit.RowColumnToPixels(FSynEdit.LogicalToPhysicalPos(FStartPoint)));
  MonitorRect := Screen.MonitorFromPoint(ScreenPoint).BoundsRect;

  R := MonitorRect;
  R.Left := ScreenPoint.X - FBorderX;
  R.Top := ScreenPoint.Y;

  DrawHints(R, True);

  if R.Left < MonitorRect.Left then
    R.Left := MonitorRect.Left;
  if R.Top < MonitorRect.Top then
    R.Top := MonitorRect.Top;
  if R.Right > MonitorRect.Right then
    R.Right := MonitorRect.Right;
  if R.Bottom > MonitorRect.Bottom then
    R.Bottom := MonitorRect.Bottom;

  BoundsRect := Rect(R.Left, R.Top - R.Height, R.Right, R.Bottom - R.Height);
end;

procedure TSimbaParameterHint.HandleEditorCaretChange(Sender: TObject);
var
  MustHide: Boolean;
  CursorXY: TPoint;
  Line: string;
  i: Int32;
begin
  if not self.Visible then
    Exit;

  try
    MustHide := True;

    if not Assigned(FSynEdit) then
      Exit;
    if FSynEdit.Focused = False then
      Exit;
    CursorXY := FSynEdit.CaretXY;
    if (CursorXY.y < FBracketPoint.y) or ((CursorXY.x <= FBracketPoint.x) and (CursorXY.y <= FBracketPoint.y)) then //Cursor moved in front of the bracket
      Exit;
    if (FBracketPoint.Y <= 0) or (FBracketPoint.Y > FSynEdit.Lines.Count) then
      Line := ''
    else
      Line:= FSynEdit.Lines[FBracketPoint.Y - 1];
    if (FBracketPoint.X > Length(Line)) or (not (Line[FBracketPoint.X] in ['(','['])) then
      Exit;

    for i := 0 to High(FDeclarations) do
    begin
      if PrepareParamString(i, Line) <> LastParameterIndex then
      begin
        MustHide := False;
        Exit;
      end;
    end;
  finally
    if MustHide then
      Self.Hide()
    else
      Self.Invalidate();
  end;
end;

procedure TSimbaParameterHint.HandleEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Self.Visible and (Key = VK_ESCAPE) then
    Self.Hide();
end;

procedure TSimbaParameterHint.DrawHints(var Bounds: TRect; Measure: Boolean);
var
  CharacterSize: TSize;

  function MeasureWord(constref Text: String): Int32;
  var
    i: Int32;
  begin
    Result := 0;
    for i := 1 to Length(Text) do
      if (Text[i] <> '\') then
        Inc(Result);

    Result *= CharacterSize.CX;
  end;

  procedure DrawWord(constref Text: String; X, Y: Int32);
  var
    i: Int32;
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clBlack;

    for i := 1 to Length(Text) do
    begin
      if Text[i] = '\' then
        Canvas.Font.Bold := not Canvas.Font.Bold
      else
      begin
        Canvas.TextOut(X, Y, Text[i]);

        X := X + CharacterSize.CX;
      end;
    end;
  end;

  procedure DrawHint(Index: Int32; Offset: Int32; out Size: TSize);
  var
    i, X, Y: Int32;
    Str: String;
    Words: TStringArray;
  begin
    PrepareParamString(Index, Str);

    X := FBorderX;
    Y := FBorderY;

    Size.CX := 0;
    Size.CY := CharacterSize.CY;

    Words := Str.Split(' ');
    for i := 0 to High(Words) - 1 do
      Words[i] := Words[i] + ' ';

    for i := 0 to High(Words) do
    begin
      if (Bounds.Left + X + MeasureWord(Words[i]) > Bounds.Right) then
      begin
        if (X > Size.CX) then
          Size.CX := X;
        Size.CY := Size.CY + CharacterSize.CY;

        X := FBorderX;
        Y := Y + CharacterSize.CY;
      end;

      if not Measure then
        DrawWord(Words[i], X, Offset + Y);

      X := X + MeasureWord(Words[i]);
    end;

    if (X > Size.CX) then
      Size.CX := X;
  end;

var
  i, W, H: Int32;
  Size: TSize;
begin
  CharacterSize := Canvas.TextExtent(' ');

  W := 0;
  H := 0;

  for i := 0 to High(FParameters) do
  begin
    DrawHint(i, H, Size);

    if (Size.CX > W) then
      W := Size.CX;
    H := H + Size.CY;
  end;

  Bounds.Right := Bounds.Left + W + (FBorderX * 2);
  Bounds.Bottom := Bounds.Top + H + (FBorderY * 2);
end;

procedure TSimbaParameterHint.Paint;
var
  R: TRect;
begin
  R := ClientRect;

  Canvas.Brush.Color := $F0F0F0;
  Canvas.Pen.Color := clBlack;
  Canvas.Rectangle(R);

  DrawHints(R, False);
end;

procedure TSimbaParameterHint.Execute(BracketPoint: TPoint; Methods: TDeclarationArray);
var
  I: Int32;
begin
  LastParameterIndex := -1;

  SetLength(FDeclarations, 0);
  SetLength(FParameters, 0);

  for I := 0 to High(Methods) do
  begin
    if (tokOverride in TciProcedureDeclaration(Methods[I]).Directives) then
      Continue;

    FDeclarations += [TciProcedureDeclaration(Methods[I])];
    FParameters += [TciProcedureDeclaration(Methods[I]).GetParamDeclarations()];
  end;

  if (Length(FDeclarations) > 0) and (Length(FParameters) > 0) then
  begin
    FStartPoint := Point(BracketPoint.X - Length(Methods[0].Name), BracketPoint.Y);
    FBracketPoint := BracketPoint;

    CalculateBounds();
  end;

  Self.Visible := (Length(FDeclarations) > 0) and (Length(FParameters) > 0);

  if FSynEdit.CanFocus then
    FSynEdit.SetFocus();
end;

end.

