unit simba.scripttab;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, comctrls, controls, syneditmiscclasses, syneditkeycmds, lcltype, dialogs, simba.functionlistform,
  simba.editor, simba.scriptinstance, simba.codeinsight, simba.codeparser, simba.parameterhint, simba.autocomplete;

type
  TSimbaScriptTab = class(TTabSheet)
  private
    procedure SetFunctionListState(Value: TSimbaFunctionList_State);
    procedure SetScriptChanged(Value: Boolean);
  protected
    FEditor: TSimbaEditor;
    FScriptChanged: Boolean;
    FSavedText: String;
    FScriptName: String;
    FScriptFile: String;
    FScriptInstance: TSimbaScriptInstance;
    FScriptErrorLine: Int32;
    FMouseLinkXY: TPoint;
    FScriptIsDefault: Boolean;
    FFunctionListState: TSimbaFunctionList_State;

    procedure HandleEditorClick(Sender: TObject);
    procedure HandleEditorChange(Sender: TObject);
    procedure HandleEditorLinkMouse(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure HandleEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleEditorSpecialLine(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    procedure HandleEditorUserCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
    procedure HandleEditorCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);

    procedure HandleAutoComplete;
    procedure HandleParameterHints;

    function GetFileName: String;
    function GetScript: String;
    procedure SetScript(Value: String);

    procedure SetScriptErrorLine(Value: Int32);
  public
    property FileName: String read GetFileName;
    property ScriptChanged: Boolean read FScriptChanged write SetScriptChanged;
    property ScriptInstance: TSimbaScriptInstance read FScriptInstance write FScriptInstance;
    property ScriptName: String read FScriptName;
    property Script: String read GetScript write SetScript;
    property ScriptErrorLine: Int32 read FScriptErrorLine write SetScriptErrorLine;
    property Editor: TSimbaEditor read FEditor;
    property ScriptIsDefault: Boolean read FScriptIsDefault;
    property MouseLinkXY: TPoint read FMouseLinkXY write FMouseLinkXY;
    property FunctionListState: TSimbaFunctionList_State read FFunctionListState write SetFunctionListState;

    procedure HandleCodeJump(Data: PtrInt);

    function Save(AFileName: String = ''): Boolean;
    function Load(AFileName: String): Boolean;

    function ParseScript: TCodeInsight;

    procedure MakeVisible;

    procedure Undo;
    procedure Redo;

    procedure Reset;

    constructor Create(APageControl: TPageControl); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  synedit, lazfileutils, SynEditMouseCmds, InterfaceBase, forms, simba.scripttabsform,
  simba.settings, simba.scripttabhistory, simba.main, simba.parser_misc, simba.debugform;


procedure TSimbaScriptTab.HandleCodeJump(Data: PtrInt);
var
  Expression: String;
  Parser: TCodeInsight;
  Declarations: TDeclarationArray;
  Buttons: TDialogButtons;
  i: Int32;
begin
  Expression := getExpression(FEditor.Text, FEditor.RowColToCharIndex(FMouseLinkXY));

  if Expression = '' then
    Exit;

  Parser := Self.ParseScript();
  Declarations := Parser.FindDeclarations(Expression);

  if Length(Declarations) = 1 then
    SimbaScriptTabsForm.OpenDeclaration(Declarations[0])
  else
  if Length(Declarations) > 1 then
  begin
    Buttons := TDialogButtons.Create(TDialogButton);

    for i := 0 to High(Declarations) do
    begin
      with Buttons.Add() do
      begin
        Caption := ExtractFileName(Declarations[i].Lexer.FileName) + ' (Line ' + IntToStr(Declarations[i].Line + 1) + ')';
        ModalResult := 1000 + i;
      end;
    end;

    i := DefaultQuestionDialog('Multiple declarations found', 'Choose the declaration to show', Ord(mtInformation), Buttons , 0);
    if (i >= 1000) then
      SimbaScriptTabsForm.OpenDeclaration(Declarations[i - 1000]);

    Buttons.Free();
  end;

  Parser.Free();
end;

procedure TSimbaScriptTab.HandleAutoComplete;
var
  Expression, Filter: String;
  Declaration: TDeclaration;
  P: TPoint;
begin
  Filter := '';
  Expression := '';

  if FEditor.CaretX <= Length(FEditor.LineText) + 1 then
    Expression := GetExpression(FEditor.Text, FEditor.SelStart - 1);

  FEditor.AutoComplete.Parser := Self.ParseScript();

  if Expression.Contains('.') then
  begin
    if not Expression.EndsWith('.') then
    begin
      Filter := Copy(Expression, LastDelimiter('.', Expression) + 1, $FFFFFF);
      Expression := Copy(Expression, 1, LastDelimiter('.', Expression) - 1);
    end;

    Declaration := FEditor.AutoComplete.Parser.ParseExpression(Expression);
    if Declaration <> nil then
      FEditor.AutoComplete.FillTypeDeclarations(Declaration);
  end else
  begin
    Filter := Expression;

    FEditor.AutoComplete.FillGlobalDeclarations();
  end;

  if Filter = '' then
    P := FEditor.CaretXY
  else
    P := FEditor.CharIndexToRowCol(FEditor.SelStart - Length(Filter) - 1);

  P := FEditor.RowColumnToPixels(P);
  P := FEditor.ClientToScreen(P);

  FEditor.AutoComplete.Execute(Filter, P.X, P.Y + FEditor.LineHeight);
end;

procedure TSimbaScriptTab.HandleParameterHints;
var
  BracketPos, InParameters: Int32;
  Expression, ScriptText: String;
  Methods: TDeclarationArray;
begin
  ScriptText := Script;

  if (FEditor.SelStart <= Length(ScriptText)) then
  begin
    InParameters := 0;

    for BracketPos := FEditor.SelStart - 1 downto 1 do
    begin
      if (ScriptText[BracketPos] = ')') then
        Inc(InParameters);

      if (ScriptText[BracketPos] = '(') then
      begin
        if (InParameters = 0) then
          Break;

        Dec(InParameters);
      end;
    end;

    Expression := GetExpression(ScriptText, BracketPos - 1);
  end else
    Exit;

  FEditor.ParameterHint.Parser := Self.ParseScript();
  with FEditor.ParameterHint.Parser do
    Methods := FindMethods(Expression);

  FEditor.ParameterHint.Execute(FEditor.CharIndexToRowCol(BracketPos - 1), Methods);
end;

function TSimbaScriptTab.GetFileName: String;
begin
  Result := FEditor.FileName;
end;

function TSimbaScriptTab.GetScript: String;
begin
  Result := FEditor.Text;
end;

procedure TSimbaScriptTab.SetScript(Value: String);
begin
  FEditor.Text := Value;
end;

procedure TSimbaScriptTab.SetScriptErrorLine(Value: Int32);
begin
  if (Value = FScriptErrorLine) then
    Exit;

  FScriptErrorLine := Value;

  Invalidate();
end;



procedure TSimbaScriptTab.SetFunctionListState(Value: TSimbaFunctionList_State);
begin
  FFunctionListState.Free();

  FFunctionListState := Value;
end;

procedure TSimbaScriptTab.SetScriptChanged(Value: Boolean);
begin
  FScriptChanged := Value;

  if FScriptChanged then
    Caption := '*' + FScriptName
  else
    Caption := FScriptName;

  SimbaForm.MenuItemSave.Enabled := FScriptChanged;
  SimbaForm.SaveButton.Enabled := FScriptChanged;
end;

procedure TSimbaScriptTab.HandleEditorClick(Sender: TObject);
begin
  ScriptTabHistory.Add(Self);
end;

procedure TSimbaScriptTab.HandleEditorChange(Sender: TObject);
begin
  ScriptChanged := True;

  if FScriptErrorLine > -1 then
    ScriptErrorLine := -1;
end;

procedure TSimbaScriptTab.HandleEditorLinkMouse(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
var
  StartX, EndX: Int32;
begin
  AllowMouseLink := True;

  Editor.GetWordBoundsAtRowCol(Point(X, Y), StartX, EndX);

  FMouseLinkXY.X := EndX - 1;
  FMouseLinkXY.Y := Y;
end;

procedure TSimbaScriptTab.HandleEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Application.QueueASyncCall(@HandleCodeJump, 0); // SynEdit is paint locked... queue it so it's prettier
end;

procedure TSimbaScriptTab.HandleEditorSpecialLine(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
begin
  Special := Line = FScriptErrorLine;

  if Special then
  begin
    AMarkup.BackAlpha := 200;
    AMarkup.Background := $0000CC;
    AMarkup.Foreground := $000000;
  end;
end;

procedure TSimbaScriptTab.HandleEditorUserCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin
  case Command of
    ecParameterHint: HandleParameterHints();
    ecAutoComplete: HandleAutoComplete();
  end;
end;

procedure TSimbaScriptTab.HandleEditorCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: Pointer);
begin
  if (Command = ecUndo) then
    ScriptChanged := FEditor.Lines.Text <> FSavedText;
  if (Command = ecChar) and (AChar = '(') and SimbaSettings.Editor.AutomaticallyShowParameterHints.Value then
    HandleParameterHints();
  if (Command = ecChar) and (AChar = '.') and SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value then
    HandleAutoComplete();
end;

function TSimbaScriptTab.Save(AFileName: String): Boolean;
begin
  Result := False;

  if AFileName = '' then
  begin
    with TSaveDialog.Create(nil) do
    try
      InitialDir := FScriptFile;
      if (FScriptFile = '') then
        InitialDir := SimbaSettings.Environment.ScriptPath.Value;

      Filter := 'Simba Files|*.simba;*.txt;*.pas;*.inc;|Any files|*.*';

      if Execute and (FileName <> '') and (ExtractFileExt(FileName) = '') then
        FileName := FileName + '.simba';

      AFileName := FileName;
    finally
      Free();
    end;
  end;

  Result := FEditor.Save(AFileName);

  if Result then
  begin
    FScriptName := ExtractFileNameOnly(AFileName);
    FScriptFile := AFileName;

    FSavedText := FEditor.Text;

    ScriptChanged := False;
  end;
end;

function TSimbaScriptTab.Load(AFileName: String): Boolean;
begin
  Result := FEditor.Load(AFileName);

  if Result then
  begin
    FScriptName := ExtractFileNameOnly(AFileName);
    FScriptFile := AFileName;

    FSavedText := FEditor.Text;

    ScriptChanged := False;
  end;
end;

function TSimbaScriptTab.ParseScript: TCodeInsight;
begin
  Result := TCodeInsight.Create();
  Result.Lexer.FileName := FileName;
  if Result.Lexer.FileName = '' then
    Result.Lexer.FileName := ScriptName;
  Result.OnMessage := @SimbaForm.OnCCMessage;
  Result.OnFindInclude := @SimbaForm.OnCCFindInclude;
  Result.OnFindLibrary := @SimbaForm.OnCCFindLibrary;
  Result.OnLoadLibrary := @SimbaForm.OnCCLoadLibrary;
  Result.Lexer.CaretPos := FEditor.SelStart - 1;
  Result.Lexer.MaxPos := FEditor.SelStart - 1;
  Result.Run(Script, Result.Lexer.FileName);
end;

procedure TSimbaScriptTab.MakeVisible;
begin
  PageControl.ActivePage := Self;
end;

procedure TSimbaScriptTab.Undo;
begin
  FEditor.Undo();
end;

procedure TSimbaScriptTab.Redo;
begin
  FEditor.Redo();
end;

procedure TSimbaScriptTab.Reset;
begin
  if (FEditor <> nil) then
    FEditor.Free();

  FEditor := TSimbaEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.OnClick := @HandleEditorClick;
  FEditor.OnChange := @HandleEditorChange;
  FEditor.OnClickLink := @HandleEditorLinkClick;
  FEditor.OnMouseLink := @HandleEditorLinkMouse;
  FEditor.OnSpecialLineMarkup := @HandleEditorSpecialLine;
  FEditor.OnProcessUserCommand := @HandleEditorUserCommand;
  FEditor.OnCommandProcessed := @HandleEditorCommandProcessed;
  FEditor.Options := FEditor.Options + [eoTabIndent, eoKeepCaretX, eoDragDropEditing, eoScrollPastEof] - [eoSmartTabs];
  FEditor.Options2 := FEditor.Options2 + [eoCaretSkipsSelection];
  FEditor.MouseOptions := FEditor.MouseOptions + [emShowCtrlMouseLinks, emCtrlWheelZoom];
  FEditor.TabWidth := 2;
  FEditor.BlockIndent := 2;
  FEditor.BorderStyle := bsNone;

  FScriptIsDefault := True;

  FunctionListState := nil;
end;

constructor TSimbaScriptTab.Create(APageControl: TPageControl);
begin
  inherited Create(PageControl);

  Reset();

  FScriptName := 'Untitled';
  FSavedText := Editor.Text;

  Caption := FScriptName;
  ImageIndex := IMAGE_SIMBA;
end;

destructor TSimbaScriptTab.Destroy;
begin
  FunctionListState := nil;

  inherited Destroy();
end;

end.

