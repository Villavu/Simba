{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_pasparser;

{$i simba.inc}

interface

uses
  SysUtils, Classes,
  simba.ide_codetools_base, simba.ide_codetools_paslexer;

type
  TPasParser = class(TObject)
  protected
    fLexer: TPasLexer;
    fLexers: TLexerList;
    fLexerStack: TLexerStack;

    fLastNoJunkTok: ELexerToken;
    fLastNoJunkPos: Integer;

    fInRound: Boolean;

    function getLexer: TPasLexer;

    procedure PushLexer(ALexer: TPasLexer); virtual;
    procedure PopLexer; virtual;

    procedure OnErrorMessage(Sender: TPasLexer; Message: String); virtual; abstract;
    procedure OnLibraryDirect(Sender: TPasLexer); virtual;
    procedure OnIncludeDirect(Sender: TPasLexer); virtual;
    procedure OnCompilerDirective(Sender: TPasLexer); virtual;
    procedure OnIDEDirective(Sender: TPasLexer); virtual;

    procedure Expected(Sym: ELexerToken); virtual;

    procedure Anchor; virtual;
    procedure NextToken; virtual;
    procedure SkipJunk; virtual;
    procedure SemiColon; virtual;
    procedure AdditiveOperator; virtual;
    procedure AncestorId; virtual;
    procedure ArrayType; virtual;
    procedure ArrayTypeStatic; virtual;
    procedure Block; virtual;
    procedure CaseLabel; virtual;
    procedure CaseSelector; virtual;
    procedure CaseStatement; virtual;
    procedure CharString; virtual;
    procedure RecordField; virtual;
    procedure CompoundStatement; virtual;
    procedure ConstantColon; virtual;
    procedure ConstantDeclaration; virtual;
    procedure ConstantAssign; virtual;
    procedure ConstantEqual; virtual;
    procedure ConstantExpression; virtual;
    procedure ConstantName; virtual;
    procedure ConstantType; virtual;
    procedure ConstantValue; virtual;
    procedure ConstantValueTyped; virtual;
    procedure ConstSection; virtual;
    procedure DeclarationSection; virtual;
    procedure Designator; virtual;
    procedure DirectiveDeprecated; virtual;
    procedure EmptyStatement; virtual;
    procedure EnumElementName; virtual;
    procedure EnumElement; virtual;
    procedure EnumType; virtual;
    procedure EnumTypeScoped; virtual;
    procedure ExceptBlock; virtual;
    procedure FinallyBlock; virtual;
    procedure TypeCopy; virtual;
    procedure Expression; virtual;
    procedure ExpressionList; virtual;
    procedure ExternalDirective; virtual;
    procedure Factor; virtual;
    procedure FieldDeclaration; virtual;
    procedure FieldList; virtual;
    procedure FieldNameList; virtual;
    procedure FieldName; virtual;
    procedure ForStatement; virtual;
    procedure ForwardDeclaration; virtual;
    procedure Identifier; virtual;
    procedure IdentifierList; virtual;
    procedure IfStatement; virtual;
    procedure LabelDeclarationSection; virtual;
    procedure LabeledStatement; virtual;
    procedure LabelId; virtual;
    procedure MultiplicativeOperator; virtual;
    procedure Number; virtual;
    procedure NativeType; virtual;
    procedure OrdinalType; virtual;
    procedure ParseFile; virtual;
    procedure PointerType; virtual;
    procedure FakeGenericType; virtual;

    procedure Method; virtual;
    procedure MethodOfType; virtual;
    procedure MethodDirective; virtual;
    procedure MethodTypeName; virtual;
    procedure MethodDirectives; virtual;
    procedure MethodBlock; virtual;
    procedure MethodName; virtual;
    procedure MethodResultType; virtual;

    procedure ParameterSection; virtual;
    procedure Parameters; virtual;
    procedure ParameterName; virtual;
    procedure ParameterNameList; virtual;
    procedure ParameterVarType; virtual;

    procedure ProceduralType; virtual;
    procedure QualifiedIdentifier; virtual;
    procedure QualifiedIdentifierList; virtual;
    procedure RaiseStatement; virtual;
    procedure FloatType; virtual;
    procedure RecordFields; virtual;
    procedure RecordType; virtual;
    procedure UnionType; virtual;
    procedure RelativeOperator; virtual;
    procedure RepeatStatement; virtual;
    procedure SetConstructor; virtual;
    procedure SetElement; virtual;
    procedure SetType; virtual;
    procedure SimpleExpression; virtual;
    procedure SimpleStatement; virtual;
    procedure SimpleType; virtual;
    procedure SkipAnsiComment; virtual;
    procedure SkipBorComment; virtual;
    procedure SkipSlashesComment; virtual;
    procedure SkipSpace; virtual;
    procedure Statement; virtual;
    procedure StatementList; virtual;
    procedure StringType; virtual;
    procedure StructuredType; virtual;
    procedure SubrangeType; virtual;
    procedure Term; virtual;
    procedure TryStatement; virtual;
    procedure TypedConstant; virtual;
    procedure TypeDeclaration; virtual;
    procedure TypeIdentifer; virtual;
    procedure TypeAlias; virtual;
    procedure TypeKind; virtual;
    procedure TypeName; virtual;
    procedure TypeSection; virtual;
    procedure VarEqual; virtual;
    procedure VarAssign; virtual;
    procedure VarDeclaration; virtual;
    procedure Variable; virtual;
    procedure VariableList; virtual;
    procedure VariableReference; virtual;
    procedure VariableTwo; virtual;
    procedure VarName; virtual;
    procedure VarNameList; virtual;
    procedure VarSection; virtual;
    procedure WhileStatement; virtual;
    procedure WithStatement; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(From: TObject); virtual;

    procedure SetScript(Script: String; FileName: String = ''); virtual;
    procedure SetFile(FileName: String); virtual;

    procedure Reset; virtual;
    procedure Run; virtual;

    property Lexer: TPasLexer read getLexer;
    property LastNoJunkPos: Integer read fLastNoJunkPos;
  end;

implementation

procedure TPasParser.ForwardDeclaration;
begin
  NextToken;
  SemiColon;
end;

procedure TPasParser.Run;
begin
  try
    ParseFile();
  except
    on E: Exception do
      OnErrorMessage(fLexer, E.Message);
  end;
end;

constructor TPasParser.Create;
begin
  inherited Create();

  fLexers := TLexerList.Create(True);
  fLexerStack := TLexerStack.Create();
end;

destructor TPasParser.Destroy;
begin
  FreeAndNil(fLexerStack);
  FreeAndNil(fLexers);

  inherited Destroy();
end;

procedure TPasParser.Assign(From: TObject);
begin
  if (From is TPasParser) then
    with From as TPasParser do
      Self.Lexer.CloneDefinesFrom(Lexer);
end;

procedure TPasParser.Expected(Sym: ELexerToken);
begin
  if (Sym <> fLexer.TokenID) then
  begin
    if (fLexer.TokenID = tokNull) then
      OnErrorMessage(fLexer, Format('"%s" expected but end of file reached', [TokenName(Sym)]))
    else
      OnErrorMessage(fLexer, Format('"%s" expected but found "%s"', [TokenName(Sym), fLexer.Token]))
  end else
    NextToken;
end;

procedure TPasParser.Anchor;
begin
  fLexer.Next();
end;

procedure TPasParser.NextToken;
begin
  fLastNoJunkPos := -1;
  fLastNoJunkTok := fLexer.TokenID;

  repeat
    fLexer.Next();

    if (fLastNoJunkPos = -1) then
      fLastNoJunkPos := fLexer.TokenPos;

    if (fLexer.TokenID = tokNull) and (fLexerStack.Count > 1) then
    begin
      PopLexer();

      Continue;
    end;
  until (not fLexer.IsJunk);
end;

procedure TPasParser.SkipJunk;
begin
  if fLexer.IsJunk then
  begin
    case fLexer.TokenID of
      tokAnsiComment:
        begin
          SkipAnsiComment;
        end;
      tokBorComment:
        begin
          SkipBorComment;
        end;
      tokSlashesComment:
        begin
          SkipSlashesComment;
        end;
      tokWhiteSpace:
        begin
          SkipSpace;
        end;
    else
      begin
        fLexer.Next;
      end;
    end;
  end;
  fLastNoJunkPos := fLexer.TokenPos;
end;

procedure TPasParser.SkipAnsiComment;
begin
  Expected(tokAnsiComment);
  while fLexer.TokenID in [tokAnsiComment] do
    fLexer.Next;
end;

procedure TPasParser.SkipBorComment;
begin
  Expected(tokBorComment);
  while fLexer.TokenID in [tokBorComment] do
    fLexer.Next;
end;

procedure TPasParser.SkipSlashesComment;
begin
  Expected(tokSlashesComment);
end;

procedure TPasParser.SemiColon;
begin
  if (not (fLexer.TokenID in [tokElse, tokEnd, tokExcept, tokFinally, tokRoundClose, tokUntil])) then
    Expected(tokSemiColon);
end;

procedure TPasParser.Reset;
begin
  fLexer := nil;
  fLexers.Clear();
  fLexerStack.Clear();
end;

procedure TPasParser.SetScript(Script: String; FileName: String);
begin
  Reset();
  PushLexer(TPasLexer.Create(Script, FileName));
end;

procedure TPasParser.SetFile(FileName: String);
begin
  Reset();
  PushLexer(TPasLexer.CreateFromFile(FileName));
end;

procedure TPasParser.ParseFile;
begin
  if (fLexer = nil) then
    Exit;
  fLexer.Next();

  SkipJunk();
  if (fLexer.TokenID = tokProgram) then
  begin
    NextToken();
    Expected(tokIdentifier);
    SemiColon();
  end;

  while (fLexer.TokenID in [tokBegin, tokType, tokConst, tokVar, tokFunction, tokProcedure, tokOperator, tokProperty, tokLabel]) do
  begin
    if (fLexer.TokenID = tokBegin) then
    begin
      CompoundStatement();
      if (fLexer.TokenID = tokSemiColon) then
        Expected(tokSemiColon);
    end else
      DeclarationSection();
  end;
end;

procedure TPasParser.Block;
begin
  while (fLexer.TokenID in [tokConst, tokFunction, tokLabel, tokProcedure, tokType, tokVar]) do
    DeclarationSection;
  CompoundStatement;
end;

procedure TPasParser.DeclarationSection;
begin
  case fLexer.TokenID of
    tokConst:
      begin
        ConstSection;
      end;
    tokFunction, tokProcedure, tokOperator, tokProperty:
      begin
        fLexer.InitAhead;
        fLexer.AheadNext;
        if (fLexer.AheadTokenID = tokPoint) then
          MethodOfType()
        else
          Method();
      end;
    tokLabel:
      begin
        LabelDeclarationSection;
      end;
    tokType:
      begin
        TypeSection;
      end;
    tokVar:
      begin
        VarSection;
      end;
  end;
end;

procedure TPasParser.Designator;
begin
  VariableReference;
end;

procedure TPasParser.DirectiveDeprecated;
begin
  Expected(tokDeprecated);
  if fLexer.TokenID = tokStringConst then
    NextToken;
end;

procedure TPasParser.MethodDirective;
begin
  case fLexer.TokenID of
    tokExternal:
      ExternalDirective;

    tokDeprecated:
      DirectiveDeprecated;

    tokOverload, tokOverride, tokStatic:
      NextToken;
  end;
end;

procedure TPasParser.MethodResultType;
begin
  TypeKind;
end;

procedure TPasParser.Parameters;
begin
  Expected(tokRoundOpen);
  if fLexer.TokenID <> tokRoundClose then
  begin
    ParameterSection;
    while fLexer.TokenID = tokSemiColon do
    begin
      SemiColon;
      ParameterSection;
    end;
  end;
  Expected(tokRoundClose);
end;

procedure TPasParser.ParameterNameList;
begin
  ParameterName;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    ParameterName;
  end;
end;

procedure TPasParser.ParameterName;
begin
  Expected(tokIdentifier);
end;

procedure TPasParser.ParameterVarType;
begin
  TypeKind();
end;

procedure TPasParser.MethodName;
begin
  NextToken;
end;

procedure TPasParser.MethodTypeName;
begin
  NextToken;
end;

procedure TPasParser.MethodDirectives;
var
  HasMethodBlock: Boolean = True;
begin
  SemiColon;

  if (fLexer.TokenID <> tokForward) then
    while (fLexer.TokenID in [tokOverload, tokOverride, tokDeprecated, tokStatic, tokExternal]) do
    begin
      if (fLexer.TokenID = tokExternal) then
        HasMethodBlock := False;

      MethodDirective;
      if fLexer.TokenID = tokSemiColon then
        SemiColon;
    end;

  if (fLexer.TokenID = tokForward) then
    ForwardDeclaration
  else if HasMethodBlock then
    MethodBlock();
end;

procedure TPasParser.MethodBlock;
begin
  Block;
  SemiColon;
end;

procedure TPasParser.ExternalDirective;
begin
  Expected(tokExternal);
  if (fLexer.TokenID = tokSemiColon) then
    SemiColon
  else
    Expression;
end;

procedure TPasParser.ForStatement;
var
  typ: ELexerToken;
begin
  Expected(tokFor);

  if (fLexer.TokenID = tokIdentifier) then
  begin
    QualifiedIdentifier;
    typ := fLexer.TokenID;
    Expected(typ);
  end;

  Expression;

  if (typ <> tokIn) then
  begin
    case fLexer.TokenID of
      tokTo, tokDownTo: NextToken;
    end;
    Expression;

    if (fLexer.TokenID = tokWith) then
    begin
      NextToken;
      Expression;
    end;
  end;

  Expected(tokDo);
  Statement;
end;

procedure TPasParser.WhileStatement;
begin
  Expected(tokWhile);
  Expression;
  Expected(tokDo);
  Statement;
end;

procedure TPasParser.RepeatStatement;
begin
  Expected(tokRepeat);
  StatementList;
  Expected(tokUntil);
  Expression;
end;

procedure TPasParser.CaseStatement;
begin
  Expected(tokCase);
  Expression;
  Expected(tokOf);
  CaseSelector;
  while fLexer.TokenID = tokSemiColon do
  begin
    SemiColon;
    case fLexer.TokenID of
      tokElse, tokEnd: ;
    else
      CaseSelector;
    end;
  end;
  if fLexer.TokenID = tokElse then
  begin
    NextToken;
    StatementList;
    SemiColon;
  end;
  Expected(tokEnd);
end;

procedure TPasParser.CaseSelector;
begin
  CaseLabel;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    CaseLabel;
  end;
  Expected(tokColon);
  case fLexer.TokenID of
    tokSemiColon: ;
  else
    Statement;
  end;
end;

procedure TPasParser.CaseLabel;
begin
  ConstantExpression;
  if fLexer.TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TPasParser.IfStatement;
begin
  Expected(tokIf);
  Expression();
  while (fLexer.TokenID = tokAssign) do
  begin
    NextToken();
    Expression();
  end;
  Expected(tokThen);
  Statement();
  if (fLexer.TokenID = tokElse) then
  begin
    NextToken();
    Statement();
  end;
end;

procedure TPasParser.ExceptBlock;
begin
  StatementList;
end;

procedure TPasParser.FinallyBlock;
begin
  StatementList;
end;

procedure TPasParser.RaiseStatement;
begin
  Expected(tokRaise);
  while (not (fLexer.TokenID in [tokSemiColon, tokNull])) do
    NextToken();
end;

procedure TPasParser.TryStatement;
begin
  Expected(tokTry);
  StatementList;
  case fLexer.TokenID of
    tokExcept:
      begin
        NextToken;
        ExceptBlock;

        if (fLexer.TokenID = tokFinally) then
        begin
          NextToken;
          FinallyBlock;
          StatementList;
        end;

        Expected(tokEnd);
      end;

    tokFinally:
      begin
        NextToken;
        FinallyBlock;
        Expected(tokEnd);
      end;
  end;
end;

procedure TPasParser.WithStatement;
begin
  Expected(tokWith);
  VariableList;
  Expected(tokDo);
  Statement;
end;

procedure TPasParser.VariableList;
begin
  VariableReference;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    VariableReference;
  end;
end;

procedure TPasParser.StatementList;
begin
  while fLexer.TokenID in [tokAddressOp, tokBegin, tokCase, tokDoubleAddressOp,
    tokFor, tokGoTo, tokIdentifier, tokIf, tokIntegerConst, tokStringConst,
    tokPointerSymbol, tokRaise, tokRoundOpen, tokRepeat, tokSemiColon,
    tokTry, tokWhile, tokWith, tokAssign] do
  begin
    Statement();

    if (fLexer.TokenID = tokAssign) then
    begin
      NextToken();
      Statement();
    end else
      SemiColon();
  end;
end;

procedure TPasParser.SimpleStatement;
begin
  case fLexer.TokenID of
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokPointerSymbol, tokRoundOpen, tokStringConst:
      begin
        Designator;
        if fLexer.TokenID in [tokAssign, tokMulAsgn, tokDivAsgn, tokPlusAsgn, tokMinusAsgn, tokPowAsgn] then
        begin
          NextToken;
          Expression;
        end;
      end;
    tokGoTo:
      begin
        NextToken;
        LabelId;
      end;
  end;
end;

procedure TPasParser.Statement;
begin
  case fLexer.TokenID of
    tokBegin:
      begin
        CompoundStatement;
      end;
    tokCase:
      begin
        CaseStatement;
      end;
    tokFor:
      begin
        ForStatement;
      end;
    tokIf:
      begin
        IfStatement;
      end;
    tokIdentifier:
      begin
        fLexer.InitAhead;
        if fLexer.AheadTokenID = tokColon then
          LabeledStatement
        else
          SimpleStatement;
      end;
    tokFloat:
      NextToken();
    tokIntegerConst:
      begin
        fLexer.InitAhead;
        if fLexer.AheadTokenID = tokColon then
          LabeledStatement
        else
          NextToken();
      end;
    tokRepeat:
      begin
        RepeatStatement;
      end;
    tokRaise:
      begin
        RaiseStatement;
      end;
    tokSemiColon:
      begin
        EmptyStatement;
      end;
    tokTry:
      begin
        TryStatement;
      end;
    tokWhile:
      begin
        WhileStatement;
      end;
    tokWith:
      begin
        WithStatement;
      end;
  else
    begin
      SimpleStatement;
    end;
  end;
end;

procedure TPasParser.EmptyStatement;
begin
end;

procedure TPasParser.LabeledStatement;
begin
  case fLexer.TokenID of
    tokIdentifier:
      begin
        NextToken;
        Expected(tokColon);
        Statement;
      end;
    tokIntegerConst:
      begin
        NextToken;
        Expected(tokColon);
        Statement;
      end;
  end;
end;

procedure TPasParser.SetElement;
begin
  Expression;
  if fLexer.TokenID = tokDotDot then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TPasParser.QualifiedIdentifier;
begin
  if (fLexer.TokenID = tokType) then
  begin
    TypeDeclaration;
    Exit;
  end;

  Expected(tokIdentifier);
  case fLexer.TokenID of
    tokPoint:
      begin
        while fLexer.TokenID = tokPoint do
        begin
          NextToken;
          if fLexer.TokenID in [tokAnd, tokArray, tokAs, tokBegin, tokCase,
            tokConst, tokConstRef, tokDiv, tokDo,
            tokDownto, tokElse, tokEnd, tokExcept,
            tokFinally, tokFor, tokFunction, tokGoto, tokIf,
            tokIn, tokIs, tokLabel, tokMod, tokNot, tokObject,
            tokOf, tokOr, tokOut, tokPacked, tokProcedure, tokProgram, tokProperty,
            tokRaise, tokRecord, tokUnion, tokRepeat, tokSet,
            tokShl, tokShr, tokStatic, tokThen, tokTo, tokTry,
            tokType, tokUntil, tokVar, tokWhile, tokWith,
            tokXor] then
              NextToken
          else
          Expected(tokIdentifier);
          if (fLexer.TokenID = tokSquareOpen) then
          begin
            ConstantExpression;
          end;
        end;
      end;
    tokSquareOpen:
      begin
        ConstantExpression;
      end;
    tokStringConst, tokIntegerConst, tokFloat, tokIdentifier:
      ConstantExpression;
  end;
end;

procedure TPasParser.SetConstructor;
begin
  Expected(tokSquareOpen);
  SetElement;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    SetElement;
  end;
  Expected(tokSquareClose);
end;

procedure TPasParser.Number;
begin
  case fLexer.TokenID of
    tokFloat:
      begin
        NextToken;
      end;
    tokIntegerConst:
      begin
        NextToken;
      end;
    tokIdentifier:
      begin
        NextToken;
      end;
  end;
end;

procedure TPasParser.NativeType;
begin
  NextToken;
  if (fLexer.TokenID = tokIdentifier) then
    TypeIdentifer
  else
  begin
    Expected(tokRoundOpen);

    if (fLexer.TokenID = tokType) then
      TypeDeclaration
    else
      TypeIdentifer;

    while (not (fLexer.TokenID in [tokRoundClose, tokNull])) do
      NextToken;
    Expected(tokRoundClose);
  end;
end;

procedure TPasParser.ExpressionList;
begin
  Expression;
  if fLexer.TokenID = tokAssign then
    begin
      Expected(tokAssign);
      Expression;
    end;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    Expression;
    if fLexer.TokenID = tokAssign then
    begin
      Expected(tokAssign);
      Expression;
    end;
  end;
end;

procedure TPasParser.MultiplicativeOperator;
begin
  case fLexer.TokenID of
    tokAnd: NextToken;
    tokDiv: NextToken;
    tokMod: NextToken;
    tokShl: NextToken;
    tokShr: NextToken;
    tokSlash: NextToken;
    tokStar:  NextToken;
    tokStarStar: NextToken;
  end;
end;

procedure TPasParser.Factor;
begin
  case fLexer.TokenID of
    tokAsciiChar, tokStringConst:
      begin
        CharString;
      end;
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokPointerSymbol, tokRoundOpen:
      begin
        Designator;
      end;
    tokIntegerConst, tokFloat:
      begin
        Number;
      end;
    tokMinus:
      begin
        NextToken;
        Factor;
      end;
    tokNot:
      begin
        NextToken;
        Factor;
      end;
    tokPlus:
      begin
        NextToken;
        Factor;
      end;
    tokSquareOpen:
      begin
        SetConstructor;
      end;
  end;
end;

procedure TPasParser.AdditiveOperator;
begin
  if fLexer.TokenID in [tokMinus, tokOr, tokPlus, tokXor] then
    NextToken;
end;

procedure TPasParser.Term;
begin
  Factor;
  while fLexer.TokenID in [tokAnd, tokDiv, tokMod, tokShl, tokShr, tokSlash, tokStar, tokStarStar] do
  begin
    MultiplicativeOperator;
    Factor;
  end;
end;

procedure TPasParser.RelativeOperator;
begin
  case fLexer.TokenID of
    tokAs:
      begin
        NextToken;
      end;
    tokEqual:
      begin
        NextToken;
      end;
    tokGreater:
      begin
        NextToken;
      end;
    tokGreaterEqual:
      begin
        NextToken;
      end;
    tokIn:
      begin
        NextToken;
      end;
    tokIs:
      begin
        NextToken;
      end;
    tokLower:
      begin
        NextToken;
      end;
    tokLowerEqual:
      begin
        NextToken;
      end;
    tokNotEqual:
      begin
        NextToken;
      end;
  end;
end;

procedure TPasParser.SimpleExpression;
begin
  Term;
  while fLexer.TokenID in [tokMinus, tokOr, tokPlus, tokXor] do
  begin
    AdditiveOperator;
    Term;
  end;
end;

procedure TPasParser.Expression;
begin
  if (fLexer.TokenID = tokType) then
  begin
    TypeDeclaration;
    Exit;
  end;

  SimpleExpression;

  case fLexer.TokenID of
    tokEqual, tokGreater, tokGreaterEqual, tokLower, tokLowerEqual, tokIn, tokIs, tokNotEqual:
      begin
        while fLexer.TokenID in [tokEqual, tokGreater, tokGreaterEqual, tokLower, tokLowerEqual, tokIn, tokIs, tokNotEqual] do
        begin
          RelativeOperator;
          SimpleExpression;
        end;
      end;

  tokColon:
    begin
      if fInRound then
        while fLexer.TokenID = tokColon do
        begin
          NextToken;
          SimpleExpression;
        end;
    end;
  end;
end;

procedure TPasParser.VarDeclaration;
begin
  VarNameList;
  if (fLexer.TokenID = tokColon) then
  begin
    Expected(tokColon);
    TypeKind;
  end;

  case fLexer.TokenID of
    tokEqual: VarEqual;
    tokAssign: VarAssign;
  end;
end;

procedure TPasParser.VarEqual;
begin
  Expected(tokEqual);
  ConstantValue;
end;

procedure TPasParser.VarAssign;
begin
  Expected(tokAssign);
  ConstantValue;
end;

procedure TPasParser.VarNameList;
begin
  VarName;
  while fLexer.TokenID = tokComma do
    begin
      NextToken;
      VarName;
    end;
end;

procedure TPasParser.VarName;
begin
  Expected(tokIdentifier);
end;

procedure TPasParser.FieldDeclaration;
begin
  FieldNameList;
  Expected(tokColon);
  TypeKind;
end;

procedure TPasParser.FieldList;
begin
  while fLexer.TokenID = tokIdentifier do
  begin
    FieldDeclaration;
    SemiColon;
  end;
end;

procedure TPasParser.FieldName;
begin
  Expected(tokIdentifier);
end;

procedure TPasParser.FieldNameList;
begin
  FieldName;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    FieldName;
  end;
end;

procedure TPasParser.RecordType;
begin
  Expected(tokRecord);

  if fLexer.TokenID = tokSemicolon then
    Exit;

  if fLexer.TokenID = tokRoundOpen then
  begin
    AncestorId;
    if fLexer.TokenID = tokSemicolon then
      Exit;
  end;
  RecordFields;

  Expected(tokEnd);
end;

procedure TPasParser.UnionType;
begin
  Expected(tokUnion);
  if fLexer.TokenID = tokSemicolon then
    Exit;
  FieldList;
  Expected(tokEnd);
end;

procedure TPasParser.SetType;
begin
  Expected(tokSet);
  Expected(tokOf);
  if (fLexer.TokenID = tokEnum) then
  begin
    NextToken;
    EnumTypeScoped;
  end else
    OrdinalType;
end;

procedure TPasParser.ArrayType;
begin
  Expected(tokArray);

  // support parsing `a: array` for magic methods
  if (fLexer.TokenID = tokOf) then
  begin
    Expected(tokOf);
    TypeKind;
  end;
end;

procedure TPasParser.ArrayTypeStatic;
begin
  Expected(tokArray);
  Expected(tokSquareOpen);

  OrdinalType;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    OrdinalType;
  end;
  Expected(tokSquareClose);
  Expected(tokOf);

  TypeKind;
end;

procedure TPasParser.EnumElementName;
begin
  NextToken;
end;

procedure TPasParser.EnumElement;
begin
  EnumElementName;
  if fLexer.TokenID = tokEqual then
  begin
    Expected(tokEqual);
    ConstantExpression;
  end;
end;

procedure TPasParser.EnumType;
begin
  Expected(tokRoundOpen);
  EnumElement;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    EnumElement;
  end;
  Expected(tokRoundClose);
end;

procedure TPasParser.EnumTypeScoped;
begin
  Expected(tokRoundOpen);
  EnumElement;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    EnumElement;
  end;
  Expected(tokRoundClose);
end;

procedure TPasParser.SubrangeType;
begin
  ConstantExpression;
  if fLexer.TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TPasParser.FloatType;
begin
  case fLexer.TokenID of
    tokMinus:
      begin
        NextToken;
      end;
    tokPlus:
      begin
        NextToken;
      end;
  end;
  case fLexer.TokenID of
    tokFloat:
      begin
        NextToken;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TPasParser.OrdinalType;
begin
  case fLexer.TokenID of
    tokIdentifier:
      begin
        fLexer.InitAhead;
        case fLexer.AheadTokenID of
          tokPoint:
            begin
              Expression;
            end;
          tokRoundOpen:
            begin
              ConstantExpression;
            end;
        else
          begin
            TypeIdentifer;
          end;
        end;
      end;
    tokRoundOpen:
      begin
        EnumType;
      end;
    tokSquareOpen:
      begin
        NextToken;
        SubrangeType;
        Expected(tokSquareClose);
      end;
  else
    begin
      Expression;
    end;
  end;
  if fLexer.TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TPasParser.VariableReference;
begin
  case fLexer.TokenID of
    tokStringConst, tokIntegerConst:
      begin
        NextToken;
        Variable;
      end;
    tokAddressOp:
      begin
        NextToken;
        Variable;
      end;
    tokDoubleAddressOp:
      begin
        NextToken;
        Variable;
      end;
    tokPointerSymbol:
      begin
        NextToken;
        case fLexer.TokenID of
          tokRoundClose, tokSquareClose: ;
        else
          begin
            Variable;
          end;
        end;
      end;
  else
    Variable;
  end;
end;

procedure TPasParser.Variable;
begin
  case fLexer.TokenID of
    tokPoint:
      begin
        VariableTwo;
      end;
    tokPointerSymbol:
      begin
        VariableTwo;
      end;
    tokRoundOpen:
      begin
        VariableTwo;
      end;
    tokSquareOpen:
      begin
        VariableTwo;
      end;
  else
    QualifiedIdentifier;
  end;
  VariableTwo;
  case fLexer.TokenID of
    tokAs:
      begin
        NextToken;
        QualifiedIdentifier;
      end;
  end;
end;

procedure TPasParser.VariableTwo;
begin
  case fLexer.TokenID of
    tokPoint:
      begin
        NextToken;
        case fLexer.TokenID of
          tokAddressOp, tokDoubleAddressOp, tokIdentifier:
            begin
              VariableReference;
            end;
          tokPointerSymbol, tokRoundOpen, tokSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    tokPointerSymbol:
      begin
        NextToken;
        case fLexer.TokenID of
          tokAddressOp, tokDoubleAddressOp, tokIdentifier:
            begin
              VariableReference;
            end;
          tokPoint, tokPointerSymbol, tokRoundOpen, tokSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    tokRoundOpen:
      begin
        NextToken;
        fInRound := True;
        case fLexer.TokenID of
          tokRoundClose:
            begin
              NextToken;
              fInRound := False;
            end;
        else
          begin
            case fLexer.TokenID of
              tokAddressOp, tokDoubleAddressOp:
                begin
                  VariableReference;
                end;
              tokPoint, tokPointerSymbol, tokRoundOpen, tokSquareOpen:
                begin
                  VariableTwo;
                end;
            end;
            fInRound := True;
            ExpressionList;
            fInRound := True;
            Expected(tokRoundClose);
            fInRound := False;
          end;
        end;
        case fLexer.TokenID of
          tokAddressOp, tokDoubleAddressOp:
            begin
              VariableReference;
            end;
          tokPoint, tokPointerSymbol, tokRoundOpen, tokSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    tokSquareOpen:
      begin
        NextToken;
        case fLexer.TokenID of
          tokSquareClose:
            begin
              NextToken;
            end;
        else
          begin
            case fLexer.TokenID of
              tokAddressOp, tokDoubleAddressOp:
                begin
                  VariableReference;
                end;
              tokPoint, tokPointerSymbol, tokRoundOpen, tokSquareOpen:
                begin
                  VariableTwo;
                end;
            end;
            ExpressionList;
            Expected(tokSquareClose);
          end;
        end;
        case fLexer.TokenID of
          tokAddressOp, tokDoubleAddressOp:
            begin
              VariableReference;
            end;
          tokPoint, tokPointerSymbol, tokRoundOpen, tokSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
  end;
end;

procedure TPasParser.RecordField;
begin
  FieldNameList;
  Expected(tokColon);
  TypeKind;
  while fLexer.TokenID in [tokDeprecated] do
    case fLexer.TokenID of
      tokDeprecated: DirectiveDeprecated;
    end;
end;

procedure TPasParser.ProceduralType;
begin
  case fLexer.TokenID of
    tokFunction:
      begin
        NextToken;
        if fLexer.TokenID = tokRoundOpen then
        begin
          Parameters;
        end;
        Expected(tokColon);
        MethodResultType;
      end;
    tokProcedure:
      begin
        NextToken;
        if fLexer.TokenID = tokRoundOpen then
        begin
          Parameters;
        end;
      end;
  end;

  if fLexer.TokenID = tokOf then
  begin
    NextToken;
    Expected(tokObject);
  end;
end;

procedure TPasParser.StringType;
begin
  VariableReference;
end;

procedure TPasParser.PointerType;
begin
  Expected(tokPointerSymbol);
  TypeIdentifer;
end;

procedure TPasParser.FakeGenericType;
begin
  TypeIdentifer;
  Parameters;
end;

procedure TPasParser.Method;
var
  Typ: ELexerToken;
begin
  Typ := fLexer.TokenID;
  NextToken();

  MethodName;
  if (fLexer.TokenID = tokRoundOpen) then
    Parameters;

  case Typ of
    tokFunction:
      begin
        Expected(tokColon);
        MethodResultType;
      end;

    tokProcedure:
      begin
        if (fLexer.TokenID <> tokSemiColon) then
          Expected(tokSemiColon);
      end;

    tokOperator, tokProperty: // properties/operators can or can not
      if (fLexer.TokenID = tokColon) then
      begin
        NextToken;
        MethodResultType;
      end;
  end;

  MethodDirectives;
end;

procedure TPasParser.MethodOfType;
var
  Typ: ELexerToken;
begin
  Typ := fLexer.TokenID;
  NextToken();

  MethodTypeName;
  Expected(tokPoint);
  MethodName;
  if (fLexer.TokenID = tokRoundOpen) then
    Parameters;

  case Typ of
    tokFunction:
      begin
        Expected(tokColon);
        MethodResultType;
      end;

    tokProcedure:
      begin
        if (fLexer.TokenID <> tokSemiColon) then
          Expected(tokSemiColon);
      end;

    tokOperator, tokProperty: // properties/operators can or can not
      if (fLexer.TokenID = tokColon) then
      begin
        NextToken;
        MethodResultType;
      end;
  end;

  MethodDirectives;
end;

procedure TPasParser.StructuredType;
begin
  if fLexer.TokenID = tokPacked then
    NextToken;

  case fLexer.TokenID of
    tokArray:
      begin
        fLexer.InitAhead;
        if (fLexer.AheadTokenID = tokSquareOpen) then
          ArrayTypeStatic
        else
          ArrayType;
      end;

    tokRecord: RecordType;
    tokUnion: UnionType;
    tokSet: SetType;
  end;
end;

procedure TPasParser.SimpleType;
begin
  case fLexer.TokenID of
    tokMinus:
      begin
        NextToken;
      end;
    tokPlus:
      begin
        NextToken;
      end;
  end;

  case fLexer.TokenID of
    tokAsciiChar, tokIntegerConst:
      begin
        OrdinalType;
      end;
    tokFloat:
      begin
        FloatType;
      end;
    tokIdentifier:
      begin
        fLexer.InitAhead;
        if (fLexer.AheadTokenID in [tokPoint, tokSemiColon]) then
          TypeIdentifer
        else
        begin
          SimpleExpression;
          if fLexer.TokenID = tokDotDot then
          begin
            NextToken;
            SimpleExpression;
          end;
        end;
      end;
    tokEnum:
      begin
        NextToken;
        EnumTypeScoped;
      end;
    tokRoundOpen:
      begin
        if fLexer.IsDefined('!SCOPEDENUMS') then
          EnumTypeScoped
        else
          EnumType;
      end;
    tokSquareOpen:
      begin
        SubrangeType;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TPasParser.RecordFields;
begin
  while (fLexer.TokenID in [tokIdentifier, tokConst]) do
  begin
    if (fLexer.TokenID = tokConst) then
    begin
      NextToken;
      ConstantDeclaration;
    end else
      RecordField;

    Expected(tokSemiColon);
  end;
end;

procedure TPasParser.TypeDeclaration;
begin
  if (fLexer.TokenID <> tokType) then
  begin
    TypeName;
    Expected(tokEqual);
  end else
    NextToken();

  if (fLexer.TokenID = tokIdentifier) then
  begin
    fLexer.InitAhead;
    if fLexer.AheadTokenID = tokSemiColon then
    begin
      TypeAlias;
      Exit;
    end;
  end;

  TypeKind;

  while fLexer.TokenID in [tokDeprecated] do
    case fLexer.TokenID of
      tokDeprecated: DirectiveDeprecated;
    end;
end;

procedure TPasParser.TypeName;
begin
  Expected(tokIdentifier);
end;

procedure TPasParser.TypeCopy;
begin
  NextToken;
  TypeIdentifer;
end;

procedure TPasParser.TypeKind;

  function isMaybeFakeGeneric: Boolean;
  var
    S: String;
  begin
    S := UpperCase(fLexer.Token);
    Result := (S = 'STRINGMAP') or (S = 'MAP') or (S = 'HEAP');
  end;

begin
  if (fLexer.TokenID = tokIdentifier) and (fLexer.TokenID = tokPrivate) then
    NextToken;

  case fLexer.TokenID of
    tokNative:
      begin
        NativeType;
      end;
    tokType:
      begin
        TypeCopy;
      end;
    tokStrict:
      begin
        TypeCopy;
      end;
    tokIdentifier:
      begin
        if isMaybeFakeGeneric() then
        begin
          fLexer.InitAhead;
          if fLexer.AheadTokenID = tokRoundOpen then
            FakeGenericType
          else
            TypeIdentifer;
        end else
          TypeIdentifer;
      end;
    tokAsciiChar, tokFloat, tokIntegerConst, tokMinus, tokPlus, tokSquareOpen, tokStringConst, tokRoundOpen, tokEnum:
      begin
        SimpleType;
      end;
    tokArray, tokPacked, tokRecord, tokUnion, tokSet:
      begin
        StructuredType;
      end;
    tokFunction, tokProcedure:
      begin
        ProceduralType;
      end;
    tokPointerSymbol:
      begin
        fLexer.InitAhead;
        if fLexer.AheadTokenID = tokConst then
        begin
          NextToken;
          NextToken;
          TypeKind;
        end else
          PointerType;
      end;
  end;
end;

procedure TPasParser.TypedConstant;
begin
  ConstantExpression;
end;

procedure TPasParser.TypeIdentifer;
begin
  NextToken;
end;

procedure TPasParser.TypeAlias;
begin
  TypeIdentifer;
end;

procedure TPasParser.ConstantExpression;
begin
  Expression;
end;

procedure TPasParser.ConstantDeclaration;
begin
  ConstantName;
  case fLexer.TokenID of
    tokAssign:
      begin
        ConstantAssign;
      end;
    tokEqual:
      begin
        ConstantEqual;
      end;
    tokColon:
      begin
        ConstantColon;
      end;
  end;
  while fLexer.TokenID in [tokDeprecated] do
    case fLexer.TokenID of
      tokDeprecated: DirectiveDeprecated;
    end;
end;

procedure TPasParser.ConstantColon;
begin
  Expected(tokColon);
  ConstantType;
  if (not (fLexer.TokenID in [tokAssign, tokEqual])) then
    Expected(tokEqual);
  NextToken;
  ConstantValueTyped;
end;

procedure TPasParser.ConstantAssign;
begin
  Expected(tokAssign);
  if (fLexer.TokenID = tokIdentifier) then
  begin
    fLexer.InitAhead;
    if (fLexer.AheadTokenID = tokRoundOpen) then
      ConstantType();
  end;
  ConstantValueTyped;
end;

procedure TPasParser.ConstantEqual;
begin
  Expected(tokEqual);
  if (fLexer.TokenID = tokIdentifier) then
  begin
    fLexer.InitAhead;
    if (fLexer.AheadTokenID = tokRoundOpen) then
      ConstantType();
  end;
  ConstantValue;
end;

procedure TPasParser.ConstantValue;
begin
  ConstantExpression;
end;

procedure TPasParser.ConstantValueTyped;
begin
  ConstantValue;
end;

procedure TPasParser.ParameterSection;
begin
  if (fLexer.TokenID in [tokOut, tokConst, tokConstRef, tokVar]) then
    NextToken;

  ParameterNameList;
  if fLexer.TokenID = tokColon then
  begin
    NextToken;
    ParameterVarType;
    if fLexer.TokenID = tokEqual then
    begin
      NextToken;
      TypedConstant;
    end;
  end;
end;

procedure TPasParser.ConstantName;
begin
  Expected(tokIdentifier);
end;

procedure TPasParser.ConstantType;
begin
  TypeKind;
end;

procedure TPasParser.LabelId;
begin
  case fLexer.TokenID of
    tokIntegerConst:
      begin
        NextToken;
      end;
    tokIdentifier:
      begin
        NextToken;
      end;
  end;
end;

procedure TPasParser.LabelDeclarationSection;
begin
  Expected(tokLabel);
  LabelId;
  while (fLexer.TokenID = tokComma) do
  begin
    NextToken;
    LabelId;
  end;
  SemiColon;
end;



procedure TPasParser.VarSection;
begin
  case fLexer.TokenID of
    tokVar:
      begin
        NextToken;
      end;
  end;
  while fLexer.TokenID in [tokIdentifier] do
  begin
    VarDeclaration;
    SemiColon;
  end;
end;

procedure TPasParser.TypeSection;
begin
  Expected(tokType);

  while (fLexer.TokenID = tokIdentifier) do
  begin
    TypeDeclaration;
    if fLexer.TokenID = tokEqual then
      TypedConstant;
    SemiColon;
  end;
end;

procedure TPasParser.ConstSection;
begin
  case fLexer.TokenID of
    tokConst:
      begin
        NextToken;
        while fLexer.TokenID in [tokIdentifier] do
        begin
            ConstantDeclaration;
            SemiColon;
        end;
      end;
  end;
end;

procedure TPasParser.CompoundStatement;
begin
  Expected(tokBegin);
  StatementList;
  Expected(tokEnd);
end;

procedure TPasParser.IdentifierList;
begin
  Identifier;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    Identifier;
  end;
end;

procedure TPasParser.QualifiedIdentifierList;
begin
  QualifiedIdentifier;
  while (fLexer.TokenID = tokComma) do
  begin
    NextToken;
    QualifiedIdentifier;
  end;
end;

procedure TPasParser.CharString;
begin
  case fLexer.TokenID of
    tokAsciiChar, tokIdentifier, tokRoundOpen, tokStringConst:
      while fLexer.TokenID in
        [tokAsciiChar, tokIdentifier, tokPlus, tokRoundOpen, tokStringConst] do
      begin
        case fLexer.TokenID of
          tokIdentifier, tokRoundOpen:
            begin
              VariableReference;
            end;
        else
          NextToken;
        end;
        if fLexer.TokenID = tokPoint then
        begin
          NextToken;
          VariableReference;
        end;
      end;
  end;
end;

procedure TPasParser.SkipSpace;
begin
  Expected(tokWhiteSpace);
  while fLexer.TokenID in [tokWhiteSpace] do
    fLexer.Next;
end;

procedure TPasParser.Identifier;
begin
  Expected(tokIdentifier);
end;

procedure TPasParser.AncestorId;
begin
  QualifiedIdentifier;
end;

function TPasParser.getLexer: TPasLexer;
begin
  if (fLexer <> nil) then
    Result := fLexer
  else
    raise Exception.Create('Parser has a nil lexer');
end;

procedure TPasParser.PushLexer(ALexer: TPasLexer);
begin
  if (fLexer <> nil) then
    ALexer.CloneDefinesFrom(fLexer);

  fLexer := ALexer;
  fLexers.Add(fLexer);
  fLexerStack.Push(fLexer);

  if (fLexerStack.Count > 100) then
    raise Exception.Create('Recursive include');

  fLexer.OnIncludeDirect := @OnIncludeDirect;
  fLexer.OnLibraryDirect := @OnLibraryDirect;
  fLexer.OnCompilerDirective := @OnCompilerDirective;
  fLexer.OnIDEDirective := @OnIDEDirective;

  if (fLexerStack.Count > 1) then
    fLexer.Next();
end;

procedure TPasParser.PopLexer;
begin
  fLexerStack.Pop();
  fLexerStack.Top.CloneDefinesFrom(fLexer);
  fLexer := fLexerStack.Top;
end;

procedure TPasParser.OnLibraryDirect(Sender: TPasLexer);
begin
end;

procedure TPasParser.OnIncludeDirect(Sender: TPasLexer);
begin
end;

procedure TPasParser.OnCompilerDirective(Sender: TPasLexer);
begin
end;

procedure TPasParser.OnIDEDirective(Sender: TPasLexer);
begin
  if (Sender.CompilerDirective = 'ANCHOR') then
    Anchor();
end;

end.

