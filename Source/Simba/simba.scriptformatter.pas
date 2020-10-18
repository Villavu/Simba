unit simba.scriptformatter;

{
  Original Code Thread: http://villavu.com/forum/showthread.php?t=35513
  by Nielsie95
}

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.tokenizer;

type
  TSimbaCodeFormatter = class
  protected
  type
    EInType = (None, InFunc, InFuncDecl, InVar);
  protected
    FOutput: String;
    FTokenizer: TSimbaTokenizerString;

    FInType: EInType;

    FIndents: array[0..1000] of Int32;
    FIndent: Int32;
    FProcIndents: array[0..1000] of Int32;
    FProcIndent: Int32;
    FElseIndent: Int32;

    FBeginCount: Int32;

    FFirstInLine: Boolean;

    FLastAsSpace: Boolean;
    FLastWasKeyword: Boolean;
    FLastWasComment: Boolean;

    procedure Add;
    procedure AddLine;
    procedure AddDirective;
    procedure AddComment;
  public
    function Format(constref Script: String): String;

    constructor Create;
    destructor Destroy; override;
  end;

function FormatScript(constref Script: String): String;

implementation

const
  Formatter_Keywords = ParserToken_Keywords + ParserToken_Operators - [tk_op_Dot, tk_op_Index];

type
  TSimbaFormatter_Tokenizer = class(TSimbaTokenizerString)
    function HandleDirective: Boolean; override;
 end;

function TSimbaFormatter_Tokenizer.HandleDirective: Boolean;
begin
  Result := True;

  while (not (getChar(1) in ['}', #0])) do
  begin
    Inc(FPos);

    if (CurChar in [#10, #13]) then
    begin
      if (CurChar = #13) and (getChar(1) = #10) then
        Inc(FPos);

      Inc(FDocPos.Line);
    end;
  end;

  Inc(FPos);
end;

procedure TSimbaCodeFormatter.AddLine();
begin
  if (FOutput <> '') then
    FOutput := FOutput + LineEnding;

  FFirstInLine := True;
end;

procedure TSimbaCodeFormatter.AddDirective;
begin
  FOutput := FOutput + FTokenizer.TokString;

  FFirstInLine := False;
  FLastAsSpace := False;
  FLastWasKeyword := False;

  AddLine();
end;

procedure TSimbaCodeFormatter.AddComment;
begin
  case FTokenizer.TokString.StartsWith('//') of
    False:
      begin
        if (not FLastWasComment) and (FInType <> InFunc) and (not FFirstInLine) then
          AddLine();
        if (FInType <> InFunc) then
          AddLine()
        else if (not FFirstInLine) then
          AddLine();

        FOutput := FOutput + FTokenizer.TokString;
      end;

    True:
      begin
        if (not FFirstInLine) then
          AddLine();

        FOutput := FOutput + StringOfChar(' ', 2 * (FIndents[FIndent] + FElseIndent)) + FTokenizer.TokString;
      end;
  end;

  FFirstInLine := False;
  FLastAsSpace := False;
  FLastWasKeyword := False;
  FLastWasComment := True;
end;

procedure TSimbaCodeFormatter.Add;
var
  IsKeyword: Boolean;
begin
  IsKeyword := FTokenizer.Tok in Formatter_Keywords;
  if FLastWasComment and (not FFirstInLine) then
    AddLine();

  FLastWasComment := False;

  if FFirstInLine then
    if (FTokenizer.Tok in [tk_kw_End, tk_kw_Until]) then
      FOutput := FOutput + StringOfChar(' ', 2 * (FIndents[FIndent] + FElseIndent - 1))
    else
      FOutput := FOutput + StringOfChar(' ', 2 * (FIndents[FIndent] + FElseIndent));

  if IsKeyWord then
  begin
    if (FFirstInLine or FLastAsSpace) and (not (FTokenizer.Tok in [tk_op_Assign, tk_op_AssignDiv, tk_op_AssignMinus, tk_op_AssignMul, tk_op_AssignPlus])) then  // =+ etc
      FOutput := FOutput + LowerCase(FTokenizer.TokString)
    else
      FOutput := FOutput + ' ' + LowerCase(FTokenizer.TokString);

    FLastAsSpace := False;
  end else
  begin
    case FTokenizer.Tok of
      tk_sym_ParenthesisOpen, tk_sym_BracketOpen:
        begin
          if FLastWasKeyword then
            FOutput := FOutput + ' ' + FTokenizer.TokString
          else
            FOutput := FOutput + FTokenizer.TokString;

          FLastAsSpace := True;
        end;

      tk_sym_Dot:
        begin
          FOutput := FOutput + FTokenizer.TokString;

          FLastAsSpace := True;
        end;

      tk_sym_Comma:
        begin
          FOutput := FOutput + FTokenizer.TokString;

          FFirstInLine := False;
          FLastAsSpace := False;
          FLastWasKeyword := True;

          Exit;
        end;

      tk_sym_Colon, tk_sym_SemiColon, tk_sym_ParenthesisClose, tk_sym_BracketClose:
        begin
          FOutput := FOutput + FTokenizer.TokString;

          FLastAsSpace := False;
        end;

      tk_op_Assign, tk_op_AssignDiv, tk_op_AssignMinus, tk_op_AssignMul, tk_op_AssignPlus,
      tk_op_Divide, tk_op_Multiply, tk_op_Minus, tk_op_Plus:
        begin
          FOutput := FOutput + ' ' + FTokenizer.TokString;
          FFirstInLine := False;
          FLastAsSpace := False;
          FLastWasKeyword := True;

          Exit;
        end;
      else
      begin
        if FFirstInLine or FLastAsSpace then
          FOutput := FOutput + FTokenizer.TokString
        else
          FOutput := FOutput + ' ' + FTokenizer.TokString;

        FLastAsSpace := False;
      end;
    end;
  end;

  FFirstInLine := False;
  FLastWasKeyword := IsKeyword;
end;

function TSimbaCodeFormatter.Format(constref Script: String): String;

  procedure Reset;
  begin
    FOutput := '';

    FLastAsSpace := False;
    FLastWasKeyword := False;
    FLastWasComment := False;

    FBeginCount := 0;
    FInType := None;
    FFirstInLine := True;

    FElseIndent := 0;
    FProcIndent := 0;
    FIndent := 0;

    FillDWord(FIndents[0], Length(FIndents), 0);
    FillDWord(FProcIndents[0], Length(FProcIndents), 0);
  end;

  procedure Next;
  begin
    repeat
      FTokenizer.Next();
    until (not (FTokenizer.Tok in [tk_NewLine, tk_WhiteSpace]));
  end;

var
  BraceCount: Integer;
begin
  FTokenizer.Doc := Script;

  Reset();
  Next();

  while (FTokenizer.Tok <> tk_NULL) do
  begin
    case FTokenizer.Tok of
      tk_Directive:
        AddDirective();

      tk_Comment:
        AddComment();

      tk_kw_Procedure, tk_kw_Function, tk_kw_Operator:
        begin
          if (FInType = InVar) and (FIndent > 0) then
            Dec(FIndent);

          FElseIndent := 0;

          if FProcIndent = 0 then
            FIndents[FIndent] := 0
          else
            FIndents[FIndent] := FIndents[FIndent] + 1;

          Inc(FProcIndent);
          FProcIndents[FProcIndent] := FIndents[FIndent];

          AddLine();
          Add();
          Next();
          Add();

          FInType := InFuncDecl;
        end;

      tk_sym_ParenthesisOpen:
        begin
          if (FInType = InVar) then
            FLastWasKeyword := True;

          Add();
          Next();
          BraceCount := 1;

          while (FTokenizer.Tok <> tk_NULL) and (BraceCount > 0) do
          begin
            if (FTokenizer.Tok = tk_sym_ParenthesisOpen) then
              Inc(BraceCount)
            else if (FTokenizer.Tok = tk_sym_ParenthesisClose) then
              Dec(BraceCount);

            Add();
            Next();
          end;

          Continue;
        end;

      tk_kw_Do, tk_kw_Then:
        begin
          Add();
          Next();

          if (FTokenizer.Tok = tk_kw_Begin) then
            Continue
          else if (FTokenizer.Tok in [tk_kw_Try, tk_kw_Repeat]) then
          begin
            Inc(FElseIndent);
            Continue;
          end else
          begin
            Inc(FElseIndent);

            AddLine();
            Add();
          end;
        end;

      tk_kw_Else:
        begin
          if (not FFirstInLine) then
            AddLine();
          if (FElseIndent > 0) then
            Dec(FElseIndent);

          Add();
          Next();

          if (FTokenizer.Tok in [tk_kw_Begin, tk_kw_If]) then
            Continue
          else if (FTokenizer.Tok in [tk_kw_Try, tk_kw_Repeat]) then
          begin
            Inc(FElseIndent);
            Continue;
          end else
          begin
            Inc(FElseIndent);
            AddLine();
            Add();
          end;
        end;

      tk_sym_Colon:
        begin
          Add();

          if (FInType = InFunc) and (FBeginCount > 1) then
          begin
            Next();
            if (FTokenizer.Tok in Formatter_Keywords) then
            begin
              Inc(FElseIndent);
              AddLine();
            end;

            Continue;
          end;
        end;

      tk_kw_Of:
        begin
          if (FInType = InVar) or (FInType = InFuncDecl) then
            FLastAsSpace := False;

          Add();

          if (FInType <> InVar) and (FInType <> InFuncDecl) then
          begin
            Inc(FBeginCount);
            Inc(FIndent);
            FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
            AddLine();
            FElseIndent := 0;
          end;
        end;

      tk_kw_Begin, tk_kw_Repeat:
        begin
          if (FInType <> InFunc) or (FBeginCount = 0) then
          begin
            FIndent := 0;
            FElseIndent := 0;
          end;

          if (not FFirstInLine) or (FInType = None) then
            AddLine();

          Add();
          Inc(FBeginCount);
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
          FElseIndent := 0;
          FInType := InFunc;
        end;

      tk_kw_Try:
        begin
          if (not FFirstInLine) then
            AddLine();

          Add();
          Inc(FBeginCount);
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
          FElseIndent := 0;
        end;

      tk_kw_Except, tk_kw_Finally:
        begin
          if (not FFirstInLine) then
            AddLine();

          FElseIndent := -1;
          Add();
          FElseIndent := 0;
          AddLine();
        end;

      tk_kw_End, tk_kw_Until:
        begin
          if (FBeginCount > 0) then
            Dec(FBeginCount);
          if (FBeginCount = 0) and (FInType = InFunc) then
            FInType := None;

          if (not FFirstInLine) then
            AddLine();

          FElseIndent := 0;

          Add();
          if (FIndent > 0) then
            Dec(FIndent);

          if (FProcIndent > 0) and (FIndents[FIndent] = FProcIndents[FProcIndent]) then
          begin
            if FIndents[FIndent] > 0 then
              FIndents[FIndent] := FIndents[FIndent] - 1;
            Dec(FProcIndent);
          end;

          if (FTokenizer.Tok = tk_kw_End) then
          begin
            Next();
            if (FTokenizer.Tok <> tk_sym_SemiColon) and (FTokenizer.Tok <> tk_sym_Dot) then
              AddLine();

            Continue;
          end;
        end;

      tk_kw_Var, tk_kw_Type, tk_kw_Label, tk_kw_Const:
        begin
          FIndent := 0;
          FElseIndent := 0;

          if (FInType <> InFuncDecl) then
            AddLine()
          else if (not FFirstInLine) then
            AddLine();

          FInType := InVar;
          Add();
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
        end;

      tk_kw_Record:
        begin
          FElseIndent := 0;
          FInType := InVar;
          Add();
          Inc(FIndent);
          FIndents[FIndent] := FIndents[FIndent - 1] + FElseIndent + 1;
          AddLine();
        end;

      tk_sym_SemiColon:
        begin
          if (not FFirstInLine) then
          begin
            Add();
            Next();

            if (FTokenizer.Tok in [tk_kw_External, tk_kw_Forward, tk_kw_Overload, tk_kw_Override, tk_kw_Static, tk_kw_ConstRef]) then
            begin
              if (FTokenizer.Tok in [tk_kw_External, tk_kw_Forward]) then
              begin
                FInType := None;

                if (FIndents[FIndent] > 0) then
                  FIndents[FIndent] := FIndents[FIndent] - 1;

                if (FProcIndent > 0) then
                  Dec(FProcIndent);
              end else
                FInType := InFuncDecl;
            end else
            begin
              FElseIndent := 0;

              AddLine();
            end;

            Continue;
          end;
        end;
      else
        Add();
    end;

    Next();
  end;

  Result := FOutput;
end;

constructor TSimbaCodeFormatter.Create;
begin
  inherited Create();

  FTokenizer := TSimbaFormatter_Tokenizer.Create('');
end;

destructor TSimbaCodeFormatter.Destroy;
begin
  FTokenizer.Free();

  inherited Destroy();
end;

function FormatScript(constref Script: String): String;
var
  Formatter: TSimbaCodeFormatter;
begin
  Result := '';

  Formatter := TSimbaCodeFormatter.Create();

  try
    Result := Formatter.Format(Script);
  finally
    Formatter.Free();
  end;
end;

end.

