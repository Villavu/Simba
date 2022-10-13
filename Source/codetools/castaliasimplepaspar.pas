{
  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with the
  License. You may obtain a copy of the License at
  http://www.mozilla.org/NPL/NPL-1_1Final.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.
}
unit castaliasimplepaspar;

{$MODE DELPHI}

interface

uses
  sysutils,
  classes,
  castaliapaslextypes,
  castaliapaslex,
  castaliasimplepaspartypes;

resourcestring
  rsExpected = '''%s'' expected found ''%s''';
  rsEndOfFile = 'end of file';

const
 ClassMethodDirectiveEnum = [
    tokAbstract, tokCdecl, tokMessage, tokOverride,  tokOverload, tokRegister,
    tokReintroduce, tokSafeCall, tokStdCall, tokVirtual, tokDeprecated,
    tokLibrary, tokPlatform, tokStatic, tokInline
 ];

type
  ESyntaxError = class(Exception)
  private
    FPosXY: TTokenPoint;
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreatePos(const Msg: string; aPosXY: TTokenPoint);
    property PosXY: TTokenPoint read FPosXY write FPosXY;
  end;

type
  TmwSimplePasPar = class(TObject)
  protected
    fOnMessage: TMessageEvent;
    fLexer: TmwPasLex;

    fInterfaceOnly: Boolean;
    fLastNoJunkPos: Integer;
    fLastNoJunkLen: Integer;

    AheadParse: TmwSimplePasPar;

    fInRound: Boolean;
    procedure setOnMessage(Value: TMessageEvent);
    procedure InitAhead;

    procedure Expected(Sym: TptTokenKind); virtual;
    procedure ExpectedEx(Sym: TptTokenKind); virtual;
    procedure ExpectedFatal(Sym: TptTokenKind); virtual;
    procedure HandlePtCompDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtDefineDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtEndIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfNDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfOptDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIncludeDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtResourceDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtUndefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfEndDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseIfDirect(Sender: TmwBasePasLex); virtual;
    procedure NextToken; virtual;
    procedure SkipJunk; virtual;
    procedure TerminateStream(Stream: TCustomMemoryStream); virtual;
    procedure SemiColon; virtual;
    function GetExID: TptTokenKind; virtual;
    function GetTokenID: TptTokenKind; virtual;
    function GetGenID: TptTokenKind; virtual;
    procedure AccessSpecifier; virtual;
    procedure AdditiveOperator; virtual;
    procedure AncestorIdList; virtual; // !! Added ancestorIdList back in...
    procedure AncestorId; virtual; // !! Added ancestorId back in...
    procedure ArrayConstant; virtual;
    procedure ArrayType; virtual;
    procedure AsmStatement; virtual;
    procedure Block; virtual;
    procedure CaseLabel; virtual;
    procedure CaseSelector; virtual;
    procedure CaseStatement; virtual;
    procedure CharString; virtual;
    procedure ClassField; virtual;
    procedure ClassForward; virtual;
    procedure ClassFunctionHeading; virtual;
    procedure ClassHeritage; virtual;
    procedure ClassMemberList; virtual;
    procedure ClassMethodDirective; virtual;
    procedure ClassMethodHeading; virtual;
    procedure ClassMethodOrProperty; virtual;
    procedure ClassMethodResolution; virtual;
    procedure ClassProcedureHeading; virtual;
    procedure ClassClass; virtual;
    procedure ClassProperty; virtual;
    procedure ClassReferenceType; virtual;
    procedure ClassType; virtual;
    procedure ClassTypeEnd; virtual; // DR 2001-07-31
    procedure ClassVisibility; virtual;
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
    procedure ConstRefParameter; virtual;
    procedure ConstParameter; virtual;
    procedure ConstructorHeading; virtual;
    procedure ConstructorName; virtual;
    procedure ConstSection; virtual;
    procedure CustomAttribute; virtual;
    procedure DeclarationSection; virtual;
    procedure Designator; virtual;
    procedure DestructorHeading; virtual;
    procedure DestructorName; virtual;
    procedure Directive16Bit; virtual;
    procedure DirectiveBinding; virtual;
    procedure DirectiveCalling; virtual;
    procedure DirectiveDeprecated; virtual; // DR 2001-10-20
    procedure DirectiveLibrary; virtual; // DR 2001-10-20
    procedure DirectivePlatform; virtual; // DR 2001-10-20
    procedure EmptyStatement; virtual;
    procedure EnumeratedType; virtual;
    procedure EnumeratedScopedType; virtual;
    procedure EnumeratedTypeItem; virtual; // DR 2001-10-29
    procedure ExceptBlock; virtual;
    procedure ExceptionBlockElseBranch; virtual;
    procedure ExceptionClassTypeIdentifier; virtual;
    procedure ExceptionHandler; virtual;
    procedure ExceptionHandlerList; virtual;
    procedure ExceptionIdentifier; virtual;
    procedure ExceptionVariable; virtual;
    procedure ExplicitType; virtual; // !! changed spelling to "Explicit"
    procedure ExportedHeading; virtual;
    procedure ExportsClause; virtual;
    procedure ExportsElement; virtual;
    procedure Expression; virtual;
    procedure ExpressionList; virtual;
    procedure ExternalDirective; virtual;
    procedure ExternalDirectiveThree; virtual;
    procedure ExternalDirectiveTwo; virtual;
    procedure Factor; virtual;
    procedure FieldDeclaration; virtual;
    procedure FieldList; virtual;
    procedure FieldNameList; virtual;
    procedure FieldName; virtual;
    procedure FormalParameterList; virtual;
    procedure FormalParameterSection; virtual;
    procedure ForStatement; virtual;
    procedure ForwardDeclaration; virtual; {GLC: corrected spelling}
    procedure FunctionHeading; virtual;
    procedure FunctionMethodDeclaration; virtual;
    procedure FunctionMethodName; virtual;
    procedure FunctionProcedureBlock; virtual;
    procedure FunctionProcedureName; virtual;
    procedure Identifier; virtual;
    procedure IdentifierList; virtual;
    procedure IfStatement; virtual;
    procedure ImplementationSection; virtual;
    procedure IncludeFile; virtual;
    procedure IndexSpecifier; virtual; // DR 2001-07-26
    procedure InitializationSection; virtual;
    procedure InlineStatement; virtual;
    procedure InParameter; virtual;
    procedure InterfaceDeclaration; virtual;
    procedure InterfaceForward; virtual;
    procedure InterfaceGUID; virtual;
    procedure InterfaceHeritage; virtual;
    procedure InterfaceMemberList; virtual;
    procedure InterfaceSection; virtual;
    procedure InterfaceType; virtual;
    procedure LabelDeclarationSection; virtual;
    procedure LabeledStatement; virtual;
    procedure LabelId; virtual;
    procedure LibraryFile; virtual;
    procedure MainUsedUnitExpression; virtual;
    procedure MainUsedUnitName; virtual;
    procedure MainUsedUnitStatement; virtual;
    procedure MainUsesClause; virtual;
    procedure MultiplicativeOperator; virtual;
    procedure Number; virtual;
    procedure NativeType; virtual;
    procedure ObjectConstructorHeading; virtual;
    procedure ObjectDestructorHeading; virtual;
    procedure ObjectField; virtual;
    procedure ObjectForward; virtual;
    procedure ObjectFunctionHeading; virtual;
    procedure ObjectHeritage; virtual;
    procedure ObjectMemberList; virtual;
    procedure ObjectMethodDirective; virtual;
    procedure ObjectMethodHeading; virtual;
    procedure ObjectNameOfMethod; virtual;
    procedure ObjectProperty; virtual;
    procedure ObjectPropertySpecifiers; virtual;
    procedure ObjectProcedureHeading; virtual;
    procedure ObjectType; virtual;
    procedure ObjectTypeEnd; virtual; // DR 2001-08-07
    procedure ObjectVisibility; virtual;
    procedure OldFormalParameterType; virtual;
    procedure OrdinalType; virtual;
    procedure OutParameter; virtual;
    procedure PackageFile; virtual;
    procedure ParameterFormal; virtual;
    procedure ParameterName; virtual;
    procedure ParameterNameList; virtual;
    procedure ParseFile; virtual;
    procedure PointerType; virtual;
    procedure ProceduralDirective; virtual;
    procedure ProceduralType; virtual;
    procedure ProcedureDeclarationSection; virtual;
    procedure ProcedureHeading; virtual;
    procedure ProcedureMethodDeclaration; virtual;
    procedure ProcedureMethodName; virtual;
    procedure ProgramBlock; virtual;
    procedure ProgramFile; virtual;
    procedure PropertyDefault; virtual;
    procedure PropertyInterface; virtual;
    procedure PropertyName; virtual;
    procedure PropertyParameterConst; virtual;
    procedure PropertyParameterList; virtual;
    procedure PropertySpecifiers; virtual;
    procedure QualifiedIdentifier; virtual;
    procedure QualifiedIdentifierList; virtual;
    procedure RaiseStatement; virtual;
    procedure ReadAccessIdentifier; virtual;
    procedure RealType; virtual;
    procedure RecordConstant; virtual;
    procedure RecordFieldConstant; virtual;
    procedure RecordType; virtual;
    procedure UnionType; virtual;
    procedure RecordVariant; virtual;
    procedure RelativeOperator; virtual;
    procedure RepeatStatement; virtual;
    procedure RequiresClause; virtual;
    procedure RequiresIdentifier; virtual;
    procedure ResolutionInterfaceName; virtual;
    procedure ResourceDeclaration; virtual;
    procedure ReturnType; virtual;
    procedure SetConstructor; virtual;
    procedure SetElement; virtual;
    procedure SetType; virtual;
    procedure SimpleExpression; virtual;
    procedure SimpleStatement; virtual;
    procedure SimpleType; virtual;
    procedure SkipAnsiComment; virtual;
    procedure SkipBorComment; virtual;
    procedure SkipSlashesComment; virtual;
    procedure SkipSpace; virtual; //XM Jul-2000
    procedure SkipCRLFco; virtual; //XM Jul-2000
    procedure SkipCRLF; virtual; //XM Jul-2000
    procedure Statement; virtual;
    procedure StatementList; virtual;
    procedure StorageExpression; virtual;
    procedure StorageIdentifier; virtual;
    procedure StorageDefault; virtual;
    procedure StorageNoDefault; virtual;
    procedure StorageSpecifier; virtual;
    procedure StorageStored; virtual;
    procedure StringStatement; virtual;
    procedure StringType; virtual;
    procedure StructuredType; virtual;
    procedure SubrangeType; virtual;
    procedure TagField; virtual;
    procedure TagFieldName; virtual;
    procedure TagFieldTypeName; virtual;
    procedure Term; virtual;
    procedure TryStatement; virtual;
    procedure TypedConstant; virtual;
    procedure TypeDeclaration; virtual;
    procedure TypeIdentifer; virtual;
    procedure TypeAlias; virtual;
    procedure TypeKind; virtual;
    procedure TypeName; virtual;
    //generics
    procedure TypeArgs; virtual;
    procedure TypeParams; virtual;
    procedure TypeParamDecl; virtual;
    procedure TypeParamDeclList; virtual;
    procedure TypeParamList; virtual;
    procedure ConstraintList; virtual;
    procedure Constraint; virtual;
    //end generics
    procedure TypeSection; virtual;
    procedure UnitFile; virtual;
    procedure UnitId; virtual;
    procedure UnitName; virtual;
    procedure UsedUnitName; virtual;
    procedure UsedUnitsList; virtual;
    procedure UsesClause; virtual;
    procedure VarAbsolute; virtual;
    procedure VarEqual; virtual;
    procedure VarAssign; virtual;
    procedure VarDeclaration; virtual;
    procedure Variable; virtual;
    procedure VariableList; virtual;
    procedure VariableReference; virtual;
    procedure VariableTwo; virtual;
    procedure VariantSection; virtual;
    procedure VarParameter; virtual;
    procedure VarName; virtual; //!! Added VarName and VarNameList back in...
    procedure VarNameList; virtual;
    procedure VarSection; virtual;
    procedure VisibilityPrivate; virtual;
    procedure VisibilityProtected; virtual;
    procedure VisibilityPublic; virtual;
    procedure VisibilityPublished; virtual;
    procedure VisibilityUnknown; virtual;
    procedure WhileStatement; virtual;
    procedure WithStatement; virtual;
    procedure WriteAccessIdentifier; virtual;
    procedure GlobalAttributes;
    procedure GlobalAttributeSections;
    procedure GlobalAttributeSection;
    procedure GlobalAttributeTargetSpecifier;
    procedure GlobalAttributeTarget;
    procedure Attributes;
    procedure AttributeSections;
    procedure AttributeSection;
    procedure AttributeTargetSpecifier;
    procedure AttributeTarget;
    procedure AttributeList;
    procedure Attribute;
    procedure AttributeName;
    procedure AttributeArguments;
    procedure PositionalArgumentList;
    procedure PositionalArgument;
    procedure NamedArgumentList;
    procedure NamedArgument;
    procedure AttributeArgumentExpression;

    property ExID: TptTokenKind read GetExID;
    property GenID: TptTokenKind read GetGenID;
    property TokenID: TptTokenKind read GetTokenID;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(From: TObject); virtual;
    procedure SynError(Error: TmwParseError); virtual;
    procedure DefaultOnMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: String; X, Y: Integer);

    procedure Run; virtual; overload;
    procedure Run(Script: String; FileName: String); virtual; overload;
    procedure Run(FileName: String); virtual; overload;

    property InterfaceOnly: Boolean read fInterfaceOnly write fInterfaceOnly;
    property Lexer: TmwPasLex read fLexer;
    property OnMessage: TMessageEvent read FOnMessage write setOnMessage;
    property LastNoJunkPos: Integer read fLastNoJunkPos;
    property LastNoJunkLen: Integer read fLastNoJunkLen;
  end;

implementation

uses
  LazLoggerBase;

constructor ESyntaxError.Create(const Msg: string);
begin
  // !! changed initialization for TTokenPoint
  FPosXY.X:= -1;
  FPosXY.Y:= -1;
  inherited Create(Msg);
end;

constructor ESyntaxError.CreateFmt(const Msg: string; const Args: array of const);
begin
  // !! changed initialization for TTokenPoint
  FPosXY.X:= -1;
  FPosXY.Y:= -1;
  inherited CreateFmt(Msg, Args);
end;

constructor ESyntaxError.CreatePos(const Msg: string; aPosXY: TTokenPoint);
begin
  Message := Msg;
  FPosXY := aPosXY;
end;

{ TmwSimplePasPar }
(* DR 2002-01-16
const
  cnExpected = 'Expected ''%s'' found ''%s''';
//  cnOrExpected = 'Expected ''%s'' or ''%s'' found ''%s''';
  cnEndOfFile = 'end of file'; {jdj 7/22/1999}
//  cnIntegerOverflow = 'Integer constant too large'; {jdj 7/22/1999}
*)

 {range checks a tokIntegerConst-slightly faster than StrToInt}
{function IsValidInteger(const S: string): Boolean; jdj 7/22/1999
var jdj removed 02/07/2001
  C: Integer;
  N: Integer;
begin
  Val(S, N, C);
  Result := (C = 0);
end;}

procedure TmwSimplePasPar.ForwardDeclaration;
begin {jdj added method 02/07/2001}
  NextToken;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ObjectProperty;
begin {jdj added method 02/07/2001}
 // DR 2001-08-07 -> changed. for array-property override failure
  Expected(tokProperty);
  PropertyName;
  case TokenID of
    tokColon, tokSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  ObjectPropertySpecifiers;
  case ExID of
    tokDefault:
      begin
        PropertyDefault; //DR 2001-07-16
        SEMICOLON;
      end;
  end;
end;

procedure TmwSimplePasPar.ObjectPropertySpecifiers;
begin {jdj added method 02/07/2001}
  if ExID = tokIndex then
  begin
    IndexSpecifier; // DR 2001-08-07
  end;
  while ExID in [tokRead, tokWrite] do
  begin
    AccessSpecifier;
  end;
  while ExID in [tokDefault, tokNoDefault, tokStored] do
  begin
    StorageSpecifier;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.Run;
begin
  try
    ParseFile();
  except
    on E: Exception do
    begin
      if Assigned(fOnMessage) then
        fOnMessage(Self, meError, E.Message, Lexer.PosXY.X, LExer.PosXY.Y);
    end;
  end;
end;

procedure TmwSimplePasPar.Run(Script: String; FileName: String);
begin
  fLexer.FileName := FileName;
  fLexer.Script := Script;

  Run();
end;

procedure TmwSimplePasPar.Run(FileName: String);
var
  Script: String;
begin
  Script := '';

  try
    with TStringList.Create() do
    try
      LoadFromFile(FileName);

      Script := Text;
    finally
      Free();
    end;
  except
  end;

  Run(Script, FileName);
end;

constructor TmwSimplePasPar.Create;
begin
  inherited Create;
  fLexer := TmwPasLex.Create;
  fOnMessage := DefaultOnMessage;
end;

destructor TmwSimplePasPar.Destroy;
begin
  FreeAndNil(AheadParse);
  FreeAndNil(fLexer);

  inherited;
end;

procedure TmwSimplePasPar.Assign(From: TObject);
begin
  if (From is TmwSimplePasPar) then
    with From as TmwSimplePasPar do
    begin
      Self.Lexer.CloneDefinesFrom(Lexer);
      Self.OnMessage := OnMessage;
    end;
end;

procedure TmwSimplePasPar.Expected(Sym: TptTokenKind);
begin
  if Sym <> Lexer.TokenID then
  begin
    if TokenID = tokNull then
      ExpectedFatal(Sym)
    else
    begin
      if Assigned(FOnMessage) then
        FOnMessage(Self, meError, Format(rsExpected, [TokenName(Sym), fLexer.Token]),
          fLexer.PosXY.X, fLexer.PosXY.Y);
    end;
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ExpectedEx(Sym: TptTokenKind);
begin
  if Sym <> Lexer.ExID then
  begin
    if Lexer.TokenID = tokNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
    else if Assigned(FOnMessage) then
      FOnMessage(Self, meError, Format(rsExpected, ['EX:' + TokenName(Sym), fLexer.Token]),
        fLexer.PosXY.X, fLexer.PosXY.Y);
  end
  else
    NextToken;
end;

{Replace Token with cnEndOfFile if TokenId = toknull}

procedure TmwSimplePasPar.ExpectedFatal(Sym: TptTokenKind);
var
  tS: string;
begin
  if Sym <> Lexer.TokenID then
  begin
    {--jdj 7/22/1999--}
    if Lexer.TokenId = tokNull then
      tS := rsEndOfFile
    else
      tS := fLexer.Token;
    {--jdj 7/22/1999--}
    raise ESyntaxError.CreatePos(Format(rsExpected, [TokenName(Sym), tS]), fLexer.PosXY);
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

  Sender.Next; //XM Jul-2000
  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtDefineDirect(Sender: TmwBasePasLex);
begin
  Sender.Next; //XM Jul-2000
end;

procedure TmwSimplePasPar.HandlePtElseDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000
  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtElseIfDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtEndIfDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfDefDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfEndDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfNDefDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfOptDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIncludeDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtResourceDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtUndefDirect(Sender: TmwBasePasLex);
begin
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.NextToken;
begin
  FLexer.NextNoJunk;
end;

procedure TmwSimplePasPar.SkipJunk;
begin
  if Lexer.IsJunk then
  begin
    case TokenID of
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
      tokSpace:
        begin
          SkipSpace; //XM Jul-2000
        end;
      tokCRLFCo:
        begin
          SkipCRLFco;
        end;
      tokCRLF:
        begin
          SkipCRLF;
        end;
      tokSquareOpen:
        begin
          CustomAttribute;
        end;
    else
      begin
        Lexer.Next;
      end;
    end;
  end;
  fLastNoJunkPos := Lexer.TokenPos;
  fLastNoJunkLen := Lexer.TokenLen;
end;

procedure TmwSimplePasPar.SkipAnsiComment;
begin
  Expected(tokAnsiComment);
  while TokenID in [tokAnsiComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipBorComment;
begin
  Expected(tokBorComment);
  while TokenID in [tokBorComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipSlashesComment;
begin
  Expected(tokSlashesComment);
end;

procedure TmwSimplePasPar.TerminateStream(Stream: TCustomMemoryStream);
var
  aChar: Char;
begin
  Stream.Position := Stream.Size;
  aChar := #0;
  Stream.Write(aChar, SizeOf(char));
end;

procedure TmwSimplePasPar.SemiColon;
begin
  case Lexer.TokenID of
    tokElse, tokEnd, tokExcept, tokfinally, tokFinalization, tokRoundClose, tokUntil: // jdj 2.23.20001 added tokFinalization
      ;
  else
    Expected(tokSemiColon);
    //Check for semicolon before else - common syntax error - JT 11.10.2007
    //Doesn't work here - it fails a CASE statement
//    if Lexer.TokenID = tokElse then
//    begin
//      if Assigned(FOnMessage) then
//      begin
//        FOnMessage(Self, meError, ''';'' not allowed before ''ELSE''',
//          FLexer.PosXY.X, FLexer.PosXY.Y);
//      end;
//    end;
  end;
end;

function TmwSimplePasPar.GetExID: TptTokenKind;
begin
  Result := fLexer.ExID;
end;

function TmwSimplePasPar.GetTokenID: TptTokenKind;
begin
  Result := fLexer.TokenID;
end;

function TmwSimplePasPar.GetGenID: TptTokenKind;
begin
  Result := fLexer.GenID;
end;

procedure TmwSimplePasPar.SynError(Error: TmwParseError);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meError, ParserErrorName(Error) + ' found ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
end;

procedure TmwSimplePasPar.DefaultOnMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: String; X, Y: Integer);
begin
  if Assigned(fLexer) then
  begin
    if (fLexer.MaxPos > -1) and (fLexer.TokenPos > fLexer.MaxPos) then
      Exit;

    if (fLexer.FileName <> '') then
      DebugLn('"%s" at line %d, column %d in file "%s"', [Msg, Y + 1, X, fLexer.FileName])
    else
      DebugLn('"%s" at line %d, column %d', [Msg, Y + 1, X]);
  end else
    DebugLn('"%s" at line %d, column %d', [Msg, Y + 1, X]);
end;

procedure TmwSimplePasPar.ParseFile;
begin
  SkipJunk;
  case GenID of
    tokLibrary:
      begin
        LibraryFile;
      end;
    tokPackage:
      begin
        PackageFile;
      end;
    tokProgram:
      begin
        ProgramFile;
      end;
    tokUnit:
      begin
        UnitFile;
      end;
  else
    begin
      IncludeFile;
    end;
  end;
end;

procedure TmwSimplePasPar.LibraryFile;
begin
  Expected(tokLibrary);
  Expected(tokIdentifier);
  SEMICOLON;
  ProgramBlock;
  Expected(tokPoint);
end;

procedure TmwSimplePasPar.PackageFile;
begin
  ExpectedEx(tokPackage);
  Expected(tokIdentifier);

  while Lexer.TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;

  SEMICOLON;
  case ExID of
    tokRequires:
      begin
        RequiresClause;
      end;
  end;

  while Lexer.TokenID = tokSquareOpen do
  begin
    CustomAttribute;
  end;

  Expected(tokEnd);
  Expected(tokPoint);
end;

procedure TmwSimplePasPar.ProgramFile;
begin
  Expected(tokProgram);
  QualifiedIdentifier;
  if TokenID = tokRoundOpen then
  begin
    NextToken;
    IdentifierList;
    Expected(tokRoundClose);
  end;
  if not InterfaceOnly then
  begin
    SEMICOLON;
    ProgramBlock;
    Expected(tokPoint);
  end;
end;

procedure TmwSimplePasPar.UnitFile;
begin
  Expected(tokUnit);
  UnitName;

  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;

  SEMICOLON;
  InterfaceSection;
  if not InterfaceOnly then
  begin
    ImplementationSection;
    InitializationSection;
    Expected(tokPoint);
  end;
end;

procedure TmwSimplePasPar.ProgramBlock;
begin
  if TokenID = tokUses then
  begin
    MainUsesClause;
  end;
  Block;
end;

procedure TmwSimplePasPar.MainUsesClause;
begin
  Expected(tokUses);
  MainUsedUnitStatement;
  while TokenID = tokComma do
  begin
    NextToken;
    MainUsedUnitStatement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.MainUsedUnitStatement;
begin
  MainUsedUnitName;
  if Lexer.TokenID = tokIn then
  begin
    NextToken;
    MainUsedUnitExpression;
  end;
end;

procedure TmwSimplePasPar.MainUsedUnitName;
begin
  UsedUnitName;
end;

procedure TmwSimplePasPar.MainUsedUnitExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.UsesClause;
begin
  Expected(tokUses);
  UsedUnitsList;
  SEMICOLON;
end;

procedure TmwSimplePasPar.UsedUnitsList;
begin
  UsedUnitName;
  while TokenID = tokComma do
  begin
    NextToken;
    UsedUnitName;
  end;
end;

procedure TmwSimplePasPar.UsedUnitName;
begin
  Expected(tokIdentifier);
  while TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;
end;

procedure TmwSimplePasPar.Block;
begin
  while (TokenID in [tokClass, tokConst, tokConstructor, tokDestructor, tokExports,
    tokFunction, tokLabel, tokProcedure, tokResourceString, tokThreadVar, tokType,
    tokVar, tokSquareOpen]) do
  begin
    DeclarationSection;
  end;
  case TokenID of
    tokAsm:
      begin
        AsmStatement;
      end;
  else
    begin
      CompoundStatement;
    end;
  end;
end;

procedure TmwSimplePasPar.DeclarationSection;
begin
  case TokenID of
    tokClass:
      begin
        ProcedureDeclarationSection;
      end;
    tokConst:
      begin
        ConstSection;
      end;
    tokConstructor:
      begin
        ProcedureDeclarationSection;
      end;
    tokDestructor:
      begin
        ProcedureDeclarationSection;
      end;
    tokExports:
      begin
        ExportsClause;
      end;
    tokFunction, tokProcedure, tokOperator:
      begin
        ProcedureDeclarationSection;
      end;
    tokLabel:
      begin
        LabelDeclarationSection;
      end;
    tokResourceString:
      begin
        ConstSection;
      end;
    tokType:
      begin
        TypeSection;
      end;
    tokThreadVar:
      begin
        VarSection;
      end;
    tokVar:
      begin
        VarSection;
      end;
    tokSquareOpen:
      begin
        CustomAttribute;
      end;
  else
    begin
      SynError(InvalidDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.UnitId;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.UnitName;
begin
  Expected(tokIdentifier);
  while Lexer.TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;
end;

procedure TmwSimplePasPar.InterfaceHeritage;
begin
  Expected(tokRoundOpen);
  AncestorIdList; // JR moved qualified check into ancestorIdList // DR 2001-11-01 can also be qualified!
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.InterfaceGUID;
begin
  Expected(tokSquareOpen);
  CharString;
  Expected(tokSquareClose);
end;

procedure TmwSimplePasPar.AccessSpecifier;
begin
  case ExID of
    tokRead:
      begin
        NextToken;
        ReadAccessIdentifier;
      end;
    tokWrite:
      begin
        NextToken;
        WriteAccessIdentifier;
      end;
    tokAdd:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: AddAccessIdentifier
      end;
  else
    begin
      SynError(InvalidAccessSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ReadAccessIdentifier;
begin
  QualifiedIdentifier;
  (* XM removed at Martin suggestion. Martin send a more general fix in QualifiedIdentifier
    //jdj 12/05/2000
    if (TokenID =  tokSquareOpen) then
      begin
        ConstantExpression;
      end;
    //jdj 12/05/2000*)
end;

procedure TmwSimplePasPar.WriteAccessIdentifier;
begin
  QualifiedIdentifier;
  (* XM removed at Martin suggestion. Martin send a more general fix in QualifiedIdentifier
   //jdj 12/05/2000
    if (TokenID =  tokSquareOpen) then
      begin
        ConstantExpression;
      end;
    //jdj 12/05/2000*)
end;

procedure TmwSimplePasPar.StorageSpecifier;
begin
  case ExID of
    tokStored:
      begin
        StorageStored;
      end;
    tokDefault:
      begin
        StorageDefault;
      end;
    tokNoDefault:
      begin
        StorageNoDefault;
      end
  else
    begin
      SynError(InvalidStorageSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StorageDefault;
begin
  ExpectedEx(tokDefault);
  StorageExpression;
end;

procedure TmwSimplePasPar.StorageNoDefault;
begin
  ExpectedEx(tokNoDefault);
end;

procedure TmwSimplePasPar.StorageStored;
begin
  ExpectedEx(tokStored);
  case TokenID of
    tokIdentifier:
      begin
        StorageIdentifier;
      end;
  else
    if TokenID <> tokSemiColon then
    begin
      StorageExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.StorageExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.StorageIdentifier;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.PropertyParameterList;
//changed James Jacobson on 20001221
begin
  Expected(tokSquareOpen);
  if TokenID = tokConst then
  begin
    PropertyParameterConst;
  end;
  IdentifierList;
  Expected(tokColon);
  TypeIdentifer;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    if TokenID = tokConst then
    begin //jdj 12-21-2000
      PropertyParameterConst;
    end;
    IdentifierList;
    Expected(tokColon);
    TypeIdentifer;
  end;
  Expected(tokSquareClose);
end;

(*begin
  Expected(tokSquareOpen);
  if TokenID = tokConst then
  begin
    PropertyParameterConst;
  end;
  IdentifierList;
  Expected(tokColon);
  TypeIdentifer;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    IdentifierList;
    Expected(tokColon);
    TypeIdentifer;
  end;
  Expected(tokSquareClose);
end;*)

procedure TmwSimplePasPar.PropertyParameterConst;
begin
  Expected(tokConst);
end;

procedure TmwSimplePasPar.PropertySpecifiers;
begin
  if ExID = tokIndex then
  begin
    IndexSpecifier; // DR 2001-07-26
  end;
  while ExID in [tokRead, tokWrite, tokAdd] do
  begin
    AccessSpecifier;
  end;
  while ExID in [tokDefault, tokNoDefault, tokStored] do
  begin
    StorageSpecifier;
  end;
  if ExID = tokImplements then
  begin
    NextToken;
    QualifiedIdentifierList;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.PropertyInterface;
begin
  if TokenID = tokSquareOpen then
  begin
    PropertyParameterList;
  end;
  Expected(tokColon);
  TypeIdentifer;
end;

procedure TmwSimplePasPar.ClassMethodHeading;
begin
  case TokenID of
    tokConstructor:
      begin
        ConstructorHeading;
      end;
    tokDestructor:
      begin
        DestructorHeading;
      end;
    tokFunction:
      begin
        Lexer.InitAhead;
        Lexer.AheadNext;
        case Lexer.AheadTokenID of
          tokPoint:
            begin
              ClassMethodResolution;
            end;
        else
          begin
            ClassFunctionHeading;
          end;
        end;
      end;
    tokProcedure:
      begin
        Lexer.InitAhead;
        Lexer.AheadNext;
        case Lexer.AheadTokenID of
          tokPoint:
            begin
              ClassMethodResolution;
            end;
        else
          begin
            ClassProcedureHeading;
          end;
        end;
      end;
  else
    SynError(InvalidClassMethodHeading);
  end;
end;

procedure TmwSimplePasPar.ClassFunctionHeading;
begin
  Expected(tokFunction);
  FunctionMethodName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(tokColon);
  ReturnType;
  if TokenId = tokSemicolon then // DR 2002-01-14
    SEMICOLON;
  if ExID in ClassMethodDirectiveEnum then
    ClassMethodDirective; //XM 2002-01-26
end;

procedure TmwSimplePasPar.FunctionMethodName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ClassProcedureHeading;
begin
  Expected(tokProcedure);
  ProcedureMethodName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;

  if TokenId = tokSemicolon then // DR 2002-01-14
    SEMICOLON;

  if exID in ClassMethodDirectiveEnum then // XM 2002-01-29
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ProcedureMethodName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ClassMethodResolution;
begin
  case TokenID of
    tokFunction:
      begin
        NextToken;
      end;
    tokProcedure:
      begin
        NextToken;
      end;
  end;
  ResolutionInterfaceName;
  Expected(tokPoint);
  Expected(tokIdentifier);
  Expected(tokEqual);
  Expected(tokIdentifier);
  SEMICOLON;
end;

procedure TmwSimplePasPar.ResolutionInterfaceName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.Constraint;
begin
  while TokenId in [tokConstructor, tokRecord, tokUnion, tokClass, tokIdentifier] do
  begin
    case TokenId of
      tokConstructor, tokRecord, tokUnion, tokClass: NextToken;
      tokIdentifier: TypeIdentifer;
    end;
    if TokenId = tokComma then
      NextToken;
  end;
end;

procedure TmwSimplePasPar.ConstraintList;
begin
  Constraint;
  while TokenId = tokComma do
  begin
    Constraint;
  end;
end;

procedure TmwSimplePasPar.ConstructorHeading;
begin
  Expected(tokConstructor);
  ConstructorName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  SEMICOLON;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ConstructorName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.DestructorHeading;
begin
  Expected(tokDestructor);
  DestructorName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  SEMICOLON;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.DestructorName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ClassMethodDirective;
begin
  while ExId in ClassMethodDirectiveEnum do
  begin
    ProceduralDirective;
    if TokenId = tokSemicolon then // DR 2002-01-14
      SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.ObjectMethodHeading;
begin
  case TokenID of
    tokConstructor:
      begin
        ObjectConstructorHeading;
      end;
    tokDestructor:
      begin
        ObjectDestructorHeading;
      end;
    tokFunction:
      begin
        ObjectFunctionHeading;
      end;
    tokProcedure:
      begin
        ObjectProcedureHeading;
      end;
  else
    begin
      SynError(InvalidMethodHeading);
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectFunctionHeading;
begin
  Expected(tokFunction);
  FunctionMethodName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(tokColon);
  ReturnType;
  if TokenID = tokSemiColon then  SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectProcedureHeading;
begin
  Expected(tokProcedure);
  ProcedureMethodName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = tokSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectConstructorHeading;
begin
  Expected(tokConstructor);
  ConstructorName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = tokSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectDestructorHeading;
begin
  Expected(tokDestructor);
  DestructorName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = tokSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectMethodDirective;
begin
  while ExID in [tokAbstract, tokCdecl, tokExport, tokExternal,
    tokMessage,
    tokOverload, // DR 2001-08-07
    tokRegister, tokSafeCall, tokStdCall, tokVirtual,
    tokDeprecated, tokLibrary, tokPlatform, tokStatic, tokInline] do
  begin
    ProceduralDirective;
    if TokenID = tokSemiColon then SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.Directive16Bit;
begin
  case ExID of
    tokExport:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirective16Bit);
    end;
  end;
end;

procedure TmwSimplePasPar.DirectiveBinding;
begin
  case ExID of
    tokVirtual:
      begin
        NextToken;
      end;
    tokMessage:
      begin
        NextToken;
        ConstantExpression;
      end;
    tokOverride:
      begin
        NextToken;
      end;
    tokOverload:
      begin
        NextToken;
      end;
    tokReintroduce:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirectiveBinding);
    end;
  end;
end;

procedure TmwSimplePasPar.ReturnType;
begin
  TypeKind;
end;

procedure TmwSimplePasPar.FormalParameterList;
begin
  Expected(tokRoundOpen);
  FormalParameterSection;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    FormalParameterSection;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.FormalParameterSection;
begin
  while TokenID = tokSquareOpen do
    CustomAttribute;
  case TokenID of
    tokConstRef:
      begin
        ConstRefParameter;
      end;
    tokConst:
      begin
        ConstParameter;
      end;
    tokIdentifier:
      case ExID of
        tokOut: OutParameter;
      else
        ParameterFormal;
      end;
    tokIn:
      begin
        InParameter;
      end;
    tokVar:
      begin
        VarParameter;
      end;
  end;
end;

procedure TmwSimplePasPar.ConstRefParameter;
begin
  Expected(tokConstRef);
  ParameterNameList;
  case TokenID of
    tokColon:
      begin
        NextToken;
        OldFormalParameterType;
        if TokenID = tokEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.ConstParameter;
begin
  Expected(tokConst);
  ParameterNameList;
  case TokenID of
    tokColon:
      begin
        NextToken;
        OldFormalParameterType;
        if TokenID = tokEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.VarParameter;
begin
  Expected(tokVar);
  ParameterNameList;
  case TokenID of
    tokColon:
      begin
        NextToken;
        OldFormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.OutParameter;
begin
  ExpectedEx(tokOut);
  ParameterNameList;
  case TokenID of
    tokColon:
      begin
        NextToken;
        OldFormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.ParameterFormal;
begin
  case TokenID of
    tokIdentifier:
      begin
        ParameterNameList;

        case TokenID of
          tokColon:
            begin
              NextToken;
              OldFormalParameterType;
              if TokenID = tokEqual then
              begin
                NextToken;
                TypedConstant;
              end;
            end
        end;

        {Expected(tokColon);
        OldFormalParameterType;
        if TokenID = tokEqual then
        begin
          NextToken;
          TypedConstant;
        end;}
      end;
  else
    begin
      SynError(InvalidParameter);
    end;
  end;
end;

procedure TmwSimplePasPar.ParameterNameList;
begin
  ParameterName;
  while TokenID = tokComma do
  begin
    NextToken;
    ParameterName;
  end;
end;

procedure TmwSimplePasPar.ParameterName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.OldFormalParameterType;
begin
  TypeIdentifer;
end;

procedure TmwSimplePasPar.FunctionMethodDeclaration;
begin
  if (TokenID = tokOperator) then
    NextToken()
  else
    Expected(tokFunction);

  Lexer.InitAhead;
  if Lexer.AheadTokenID in [tokPoint, tokLower] then
  begin
    ObjectNameOfMethod;
    Expected(tokPoint);
  end;
  FunctionProcedureName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;

  Expected(tokColon);
  ReturnType;

  FunctionProcedureBlock;
end;

procedure TmwSimplePasPar.ProcedureMethodDeclaration;
begin
  case TokenID of
    tokConstructor:
      begin
        NextToken;
      end;
    tokDestructor:
      begin
        NextToken;
      end;
    tokProcedure:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidProcedureMethodDeclaration);
    end;
  end;
  Lexer.InitAhead;
  if Lexer.AheadTokenID in [tokPoint, tokLower] then
  begin
    ObjectNameOfMethod;
    Expected(tokPoint);
  end;
  FunctionProcedureName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;

  FunctionProcedureBlock;
end;

procedure TmwSimplePasPar.FunctionProcedureName;
begin
  if not (Lexer.TokenID in [tokIdentifier,

    //Operators =)
    tokMinus, tokOr, tokPlus, tokXor,
    tokAnd, tokAs, tokDiv, tokMod, tokShl, tokShr, tokSlash, tokStar, tokStarStar,
    tokEqual, tokGreater, tokGreaterEqual, tokLower, tokLowerEqual,
    tokIn, tokIs, tokNotEqual,
    tokAssign,
    tokDivAsgn,
    tokMulAsgn,
    tokPlusAsgn,
    tokMinusAsgn,
    tokPowAsgn]) then
  begin
    if TokenID = tokNull then
      ExpectedFatal(tokIdentifier) {jdj 7/22/1999}
    else
    begin
      if Assigned(FOnMessage) then
        FOnMessage(Self, meError, Format(rsExpected, [TokenName(tokIdentifier), fLexer.Token]),
          fLexer.PosXY.X, fLexer.PosXY.Y);
    end;
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ObjectNameOfMethod;
begin
  NextToken;
end;

procedure TmwSimplePasPar.FunctionProcedureBlock;
var
  NoExternal: Boolean;
begin
  NoExternal := True;
  if TokenID = tokSemiColon then SEMICOLON;
  case ExID of
    tokForward:
      ForwardDeclaration;
  else
    while (ExID in [tokAbstract, tokCdecl, tokExport, tokExternal,
      tokMessage, tokOverload, tokOverride, tokRegister,
      tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
      tokDeprecated, tokLibrary, tokPlatform,
      tokAssembler, tokNative, tokStatic, tokInline, tokConst
       ]) or (TokenID = tokConstRef)
    do
      begin
        case TokenID of
          tokConstRef:
            begin
              NextToken;
              if (TokenID = tokSemiColon) then SEMICOLON;
            end
        else
          case ExId of
            tokExternal:
              begin
                ProceduralDirective;
                if TokenID = tokSemiColon then SEMICOLON;
                NoExternal := False;
              end;
          else
            begin
              ProceduralDirective;
              if TokenID = tokSemiColon then SEMICOLON;
            end;
          end;
        end;
      end;
    if ExID = tokForward then
      ForwardDeclaration // DR 2001-07-23
    else if NoExternal then
    begin
      if ExId = tokAssembler then
      begin
        NextToken;
        SEMICOLON;
      end;
      case TokenID of
        tokAsm:
          begin
            AsmStatement;
          end;
      else
        begin
          Block;
        end;
      end;
      SEMICOLON;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirective;
begin
  ExpectedEx(tokExternal);
  case TokenID of
    tokSemiColon:
      begin
        SEMICOLON;
      end;
  else
    begin
      SimpleExpression;
      ExternalDirectiveTwo;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirectiveTwo;
begin
  case fLexer.ExID of
    tokIndex:
      begin
        NextToken;
      end;
    tokName:
      begin
        NextToken;
        SimpleExpression;
      end;
    tokSemiColon:
      begin
        SEMICOLON;
        ExternalDirectiveThree;
      end;
  end
end;

procedure TmwSimplePasPar.ExternalDirectiveThree;
begin
  case TokenID of
    tokMinus:
      begin
        NextToken;
      end;
  end;
  case TokenID of
    tokIdentifier, tokIntegerConst:
      begin
        NextToken;
      end;
  end;
end;

procedure TmwSimplePasPar.ForStatement;
var
  typ: TptTokenKind;
begin
  Expected(tokFor);

  if (Lexer.TokenID = tokIdentifier) then
  begin
    QualifiedIdentifier;
    typ := Lexer.TokenID;  //Should be tokIn or tokAssign =)
    Expected(typ);
  end;

  Expression;

  if (typ <> tokIn) then
  begin
    case TokenID of
      tokTo, tokDownTo: NextToken;
      else
        SynError(InvalidForStatement);
    end;
    Expression;

    if (TokenID = tokWith) then
    begin
      NextToken;
      Expression;
    end;
  end;

  Expected(tokDo);
  Statement;
end;

procedure TmwSimplePasPar.WhileStatement;
begin
  Expected(tokWhile);
  Expression;
  Expected(tokDo);
  Statement;
end;

procedure TmwSimplePasPar.RepeatStatement;
begin
  Expected(tokRepeat);
  StatementList;
  Expected(tokUntil);
  Expression;
end;

procedure TmwSimplePasPar.CaseStatement;
begin
  Expected(tokCase);
  Expression;
  Expected(tokOf);
  CaseSelector;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    case TokenID of
      tokElse, tokEnd: ;
    else
      CaseSelector;
    end;
  end;
  if TokenID = tokElse then
  begin
    NextToken;
    StatementList;
    SEMICOLON;
  end;
  Expected(tokEnd);
end;

procedure TmwSimplePasPar.CaseSelector;
begin
  CaseLabel;
  while TokenID = tokComma do
  begin
    NextToken;
    CaseLabel;
  end;
  Expected(tokColon);
  case TokenID of
    tokSemiColon: ;
  else
    Statement;
  end;
end;

procedure TmwSimplePasPar.CaseLabel;
begin
  ConstantExpression;
  if TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.IfStatement;
begin
  Expected(tokIf);
  Expression;
  Expected(tokThen);
  Statement;
  //This breaks if you have an if statement immediately preceding the else 
  //clause of a case statement
{  Lexer.InitAhead;
  if (TokenID = tokSemicolon) and (Lexer.AheadTokenID = tokElse) then
  begin
    if Assigned(FOnMessage) then
    begin
      FOnMessage(Self, meError, ''';'' not allowed before ''ELSE''',
        FLexer.PosXY.X, FLexer.PosXY.Y);
    end;
  end;}
  if TokenID = tokElse then
  begin
    NextToken;
    Statement;
  end;
end;

procedure TmwSimplePasPar.ExceptBlock;
begin
  case ExID of
    tokOn:
      begin
        ExceptionHandlerList;
        ExceptionBlockElseBranch
      end;
  else
    begin
      StatementList;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandlerList;
begin
  while fLexer.ExID = tokOn do
  begin
    ExceptionHandler;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandler;
begin
  ExpectedEx(tokOn);
  ExceptionIdentifier;
  Expected(tokDo);
  Statement;
end;

procedure TmwSimplePasPar.ExceptionBlockElseBranch;
begin
  case TokenID of
    tokElse:
      begin
        NextToken;
        StatementList;
      end;
  end;
end;

procedure TmwSimplePasPar.ExceptionIdentifier;
begin
  Lexer.InitAhead;
  case Lexer.AheadTokenID of
    tokPoint:
      begin
        ExceptionClassTypeIdentifier;
      end;
  else
    begin
      ExceptionVariable;
      case Lexer.TokenID of
        tokColon:
          begin
            NextToken;
            ExceptionClassTypeIdentifier;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionClassTypeIdentifier;
begin
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.ExceptionVariable;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.InlineStatement;
begin
  Expected(tokInline);
  Expected(tokRoundOpen);
  Expected(tokIntegerConst);
  while (TokenID = tokSlash) do
  begin
    NextToken;
    Expected(tokIntegerConst);
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.InParameter;
begin
  Expected(tokIn);
  ParameterNameList;
  case TokenID of
    tokColon:
      begin
        NextToken;
        OldFormalParameterType;
        if TokenID = tokEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.AsmStatement;
begin
  Lexer.AsmCode := True;
  Expected(tokAsm);
  { should be replaced with a Assembler lexer }
  while (not (TokenID in [tokEnd, tok_Done])) do
    case fLexer.TokenID of
      tokBegin, tokCase, tokEnd, tokIf, tokFunction, tokProcedure, tokRepeat, tokwhile: break;
      tokAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      tokDoubleAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      tokNull: //JThurman 10-26-2004.  Need another way out of this.
        begin
          Expected(tokEnd);
          Exit;
        end;
    else
      NextToken;
    end;
  Lexer.AsmCode := False;
  Expected(tokEnd);
end;

procedure TmwSimplePasPar.RaiseStatement;
begin
  Expected(tokRaise);
  while (not (TokenID in [tokSemiColon, tokNull, tok_DONE])) do
    NextToken();
end;

procedure TmwSimplePasPar.TryStatement;
begin
  Expected(tokTry);
  StatementList;
  case TokenID of
    tokExcept:
      begin
        NextToken;
        ExceptBlock;

        if (TokenID = tokFinally) then
        begin
          NextToken;
          StateMentList;
        end;

        Expected(tokEnd);
      end;
    tokFinally:
      begin
        NextToken;
        StatementList;

        if (TokenID = tokExcept) then
        begin
          NextToken;
          StateMentList;
        end;

        Expected(tokEnd);
      end;
  else
    begin
      SynError(InvalidTryStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.WithStatement;
begin
  Expected(tokWith);
  VariableList;
  Expected(tokDo);
  Statement;
end;

procedure TmwSimplePasPar.VariableList;
begin
  VariableReference;
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    VariableReference;
  end;
end;

procedure TmwSimplePasPar.StatementList;
begin
  while TokenID in [tokAddressOp, tokAsm, tokBegin, tokCase, tokDoubleAddressOp,
    tokFor, tokGoTo, tokIdentifier, tokIf, tokInline, tokIntegerConst, tokStringConst,
    tokPointerSymbol, tokRaise, tokRoundOpen, tokRepeat, tokSemiColon,
    tokTry, tokWhile, tokWith, tokAssign] do
  begin
    Statement();

    if (TokenID = tokAssign) then
    begin
      NextToken();
      Statement();
    end else
      SemiColon();
  end;
end;

procedure TmwSimplePasPar.SimpleStatement;
begin
  case TokenID of
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokPointerSymbol, tokRoundOpen, tokStringConst:
      begin
        Designator;
        if TokenID in [tokAssign, tokMulAsgn, tokDivAsgn, tokPlusAsgn, tokMinusAsgn, tokPowAsgn] then
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

procedure TmwSimplePasPar.Statement;
begin
  case TokenID of
    tokAsm:
      begin
        AsmStatement;
      end;
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
        case Lexer.AheadTokenID of
          tokColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SimpleStatement;
          end;
        end;
      end;
    tokInLine:
      begin
        InlineStatement;
      end;
    tokFloat:
      NextToken();
    tokIntegerConst:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          tokColon:
            begin
              LabeledStatement;
            end;
          else
            NextToken();
        end;
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

procedure TmwSimplePasPar.EmptyStatement;
begin
  { Nothing to do here.
    The semicolon will be removed in StatementList }
end;

procedure TmwSimplePasPar.LabeledStatement;
begin
  case TokenID of
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
  else
    begin
      SynError(InvalidLabeledStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.StringStatement;
begin
  Statement;
end;

procedure TmwSimplePasPar.SetElement;
begin
  Expression;
  if TokenID = tokDotDot then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.QualifiedIdentifier;
begin //mw 12/7/2000
  if (TokenID = tokType) then
  begin
    TypeDeclaration;
    Exit;
  end;

  Expected(tokIdentifier);
  case TokenID of
    tokPoint:
      begin
        while TokenID = tokPoint do
        begin
          NextToken;
          if TokenID in [tokAnd, tokArray, tokAs, tokASM, tokBegin, tokCase, tokClass,
            tokConst, tokConstRef, tokConstructor, tokDestructor, tokDiv, tokDo,
            tokDOwnto, tokElse, tokEnd, tokExcept, tokExports, tokFinal,
            tokFinalization, tokFinally, tokFor, tokFunction, tokGoto, tokIf,
            tokImplementation, tokIn, tokInitialization, tokInline,
            tokInterface, tokIs, tokLabel, tokLibrary, tokMod, tokNot, tokObject,
            tokOf, tokOr, tokOut, tokPacked, tokProcedure, tokProgram, tokProperty,
            tokRaise, tokRecord, tokUnion, tokRepeat, tokResourceString, tokSealed, tokSet,
            tokShl, tokShr, tokStatic, tokThen, tokThreadVar, tokTo, tokTry,
            tokType, tokUnit, tokUnsafe, tokUntil, tokUses, tokVar, tokWhile, tokWith,
            tokXor] then
              NextToken
          else
          Expected(tokIdentifier);
          if (TokenID = tokSquareOpen) then
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
(*  Expected(tokIdentifier); // old code for information removed in next versions
  case TokenID of
    tokPoint:
      begin
        NextToken;
        Expected(tokIdentifier);
        if (TokenID = tokSquareOpen) then
        begin
          ConstantExpression;
        end;
      end;
    tokSquareOpen: {MW 20001207}
      begin
        ConstantExpression;
      end;
  end;*)

end;

procedure TmwSimplePasPar.SetConstructor;
begin
  Expected(tokSquareOpen);
  SetElement;
  while TokenID = tokComma do
  begin
    NextToken;
    SetElement;
  end;
  Expected(tokSquareClose);
end;

procedure TmwSimplePasPar.Number;
begin
  case TokenID of
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
  else
    begin
      SynError(InvalidNumber);
    end;
  end;
end;

procedure TmwSimplePasPar.NativeType;
begin
  NextToken;

  if TokenID = tokIdentifier then
  begin
    AncestorId;
  end else
  begin
    Expected(tokRoundOpen);

    AncestorId;
    while (not (TokenID in [tokRoundClose, tokNull, tok_DONE])) do
      NextToken;

    Expected(tokRoundClose);
  end;
end;

procedure TmwSimplePasPar.ExpressionList;
begin
  Expression;
  if TokenID = tokAssign then //JT Nov 26, 2004 - supporting ole automation syntax
    begin
      Expected(tokAssign);
      Expression;
    end;
  while TokenID = tokComma do
  begin
    NextToken;
    Expression;
    if TokenID = tokAssign then //JT Nov 26, 2004 - supporting ole automation syntax
    begin
      Expected(tokAssign);
      Expression;
    end;
  end;
end;

procedure TmwSimplePasPar.Designator;
begin
  VariableReference;
end;

procedure TmwSimplePasPar.MultiplicativeOperator;
begin
  case TokenID of
    tokAnd: NextToken;
    tokDiv: NextToken;
    tokMod: NextToken;
    tokShl: NextToken;
    tokShr: NextToken;
    tokSlash: NextToken;
    tokStar:  NextToken;
    tokStarStar: NextToken;
  else
    begin SynError(InvalidMultiplicativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.Factor;
begin
  case TokenID of
    tokAsciiChar, tokStringConst:
      begin
        CharString;
      end;
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokPointerSymbol,
      tokRoundOpen:
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

procedure TmwSimplePasPar.AdditiveOperator;
begin
  if TokenID in [tokMinus, tokOr, tokPlus, tokXor] then
    NextToken
  else
    SynError(InvalidAdditiveOperator);
end;

procedure TmwSimplePasPar.Term;
begin
  Factor;
  while TokenID in [tokAnd, tokDiv, tokMod, tokShl, tokShr, tokSlash, tokStar, tokStarStar] do
  begin
    MultiplicativeOperator;
    Factor;
  end;
end;

procedure TmwSimplePasPar.RelativeOperator;
begin
  case TokenID of
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
  else
    begin
      SynError(InvalidRelativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleExpression;
begin
  Term;
  while TokenID in [tokMinus, tokOr, tokPlus, tokXor] do
  begin
    AdditiveOperator;
    Term;
  end;
end;

procedure TmwSimplePasPar.Expression;
begin
  if (TokenID = tokType) then
  begin
    TypeDeclaration;
    Exit;
  end;

  SimpleExpression;

  //JT 2006-07-17 The Delphi language guide has this as
  //Expression -> SimpleExpression [RelOp SimpleExpression]...
  //So this needs to be able to repeat itself.
  case TokenID of
  tokEqual, tokGreater, tokGreaterEqual, tokLower, tokLowerEqual, tokIn, tokIs,
    tokNotEqual:
    begin
      while TokenID in [tokEqual, tokGreater, tokGreaterEqual, tokLower, tokLowerEqual,
        tokIn, tokIs, tokNotEqual{, tokColon}] do
      begin
        RelativeOperator;
        SimpleExpression;
      end;
    end;
  tokColon:
    begin
      case fInRound of
        False: ;
        True:
          while TokenID = tokColon do
          begin
            NextToken;
            SimpleExpression;
          end;
      end;
    end;
  tokAssign:
    begin
      StatementList;
      Expression;
    end;
  end;
end;

procedure TmwSimplePasPar.VarDeclaration;
begin
  // !! Changed back to var name list from IdentifierList
  VarNameList;
  if (TokenID = tokColon) then
  begin
    Expected(tokColon);
    TypeKind;
  end;

  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
  case GenID of
    tokAbsolute: VarAbsolute;
    tokEqual: VarEqual;
    tokAssign: VarAssign;
  end;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.VarAbsolute;
begin
  ExpectedEx(tokAbsolute);
  ConstantValue;
end;

procedure TmwSimplePasPar.VarEqual;
begin
  Expected(tokEqual);
  ConstantValue;
end;

procedure TmwSimplePasPar.VarAssign;
begin
  Expected(tokAssign);
  ConstantValue;
end;

procedure TmwSimplePasPar.VarNameList;
begin
  VarName;
  while TokenID = tokComma do
    begin
      NextToken;
      VarName;
    end;
end;

procedure TmwSimplePasPar.VarName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.DirectiveCalling;
begin
  case ExID of
    tokCdecl:
      begin
        NextToken;
      end;
    tokRegister:
      begin
        NextToken;
      end;
    tokSafeCall:
      begin
        NextToken;
      end;
    tokStdCall:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirectiveCalling);
    end;
  end;
end;

procedure TmwSimplePasPar.RecordVariant;
begin
  ConstantExpression;
  while (TokenID = tokComma) do
  begin
    NextToken;
    ConstantExpression;
  end;
  Expected(tokColon);
  Expected(tokRoundOpen);
  if TokenID <> tokRoundClose then
  begin
    FieldList;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.VariantSection;
begin
  Expected(tokCase);
  TagField;
  Expected(tokOf);
  RecordVariant;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    case TokenID of //DR 2001-12-11
      tokEnd, tokRoundClose: Break;
    else
      RecordVariant;
    end;
  end;
end;

procedure TmwSimplePasPar.TagField;
begin
  TagFieldName;
  case fLexer.TokenID of
    tokColon:
      begin
        NextToken;
        TagFieldTypeName;
      end;
  end;
end;

procedure TmwSimplePasPar.TagFieldName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.TagFieldTypeName;
begin
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.FieldDeclaration;
begin
  //IdentifierList;
  FieldNameList;
  Expected(tokColon);
  TypeKind;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2002-01-09
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.FieldList;
begin
  while TokenID = tokIdentifier do
  begin
    FieldDeclaration;
    SEMICOLON;
  end;
  if TokenID = tokCase then
  begin
    VariantSection;
  end;
end;

procedure TmwSimplePasPar.FieldName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.FieldNameList;
begin
  FieldName;
  while TokenID = tokComma do
  begin
    NextToken;
    FieldName;
  end;
end;

procedure TmwSimplePasPar.RecordType;
begin
  Expected(tokRecord);

  if TokenID = tokSemicolon then
    Exit;

  if TokenID = tokRoundOpen then
  begin
    ClassHeritage;
    if TokenID = tokSemicolon then
      Exit;
  end;
  ClassMemberList;

  Expected(tokEnd);
end;

procedure TmwSimplePasPar.UnionType;
begin
  Expected(tokUnion);
  if TokenID = tokSemicolon then
    Exit;
  FieldList;
  Expected(tokEnd);
end;

procedure TmwSimplePasPar.SetType;
begin
  Expected(tokSet);
  Expected(tokOf);
  OrdinalType;
end;

procedure TmwSimplePasPar.ArrayType;
begin
  Expected(tokArray);
  if TokenID = tokSquareOpen then
  begin
    NextToken;
    OrdinalType;
    while TokenID = tokComma do
    begin
      NextToken;
      OrdinalType;
    end;
    Expected(tokSquareClose);
  end;

  if (TokenID = tokOf) then
  begin
    Expected(tokOf);
    TypeKind;
  end;
end;

procedure TmwSimplePasPar.EnumeratedType;
begin
  Expected(tokRoundOpen);
  EnumeratedTypeItem;
  while TokenID = tokComma do
  begin
    NextToken;
    EnumeratedTypeItem;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.EnumeratedScopedType;
begin
  NextToken();

  Expected(tokRoundOpen);
  EnumeratedTypeItem;
  while TokenID = tokComma do
  begin
    NextToken;
    EnumeratedTypeItem;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.SubrangeType;
begin
  ConstantExpression;
  if TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.RealType;
begin
  case TokenID of
    tokMinus:
      begin
        NextToken;
      end;
    tokPlus:
      begin
        NextToken;
      end;
  end;
  case TokenId of
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

procedure TmwSimplePasPar.OrdinalType;
begin
  case TokenID of
    tokIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          tokPoint:
            begin
              Expression;
            end;
          tokRoundOpen:
            begin //jdj
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
        EnumeratedType;
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
  if TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.VariableReference;
begin
  case TokenID of
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
        case TokenID of
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

procedure TmwSimplePasPar.Variable; (* Attention: could also came from proc_call ! ! *)
begin
  case TokenID of
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
  case TokenID of
    tokAs:
      begin
        NextToken;
        QualifiedIdentifier;
      end;
  end;
end;

procedure TmwSimplePasPar.VariableTwo;
begin
  case TokenID of
    tokPoint:
      begin
        NextToken;
        case TokenID of
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
        case TokenID of
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
        case TokenID of
          tokRoundClose:
            begin
              NextToken;
              //Expected(tokRoundClose);
              fInRound := False;
            end;
        else
          begin
            case TokenID of
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
        case TokenID of
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
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> tokSemiColon do
        begin
          case Lexer.AheadTokenID of
            tokBegin, tokClass, tokConst, tokEnd, tokDotDot, tokIn, tokNull, tok_Done, tokThreadVar, tokType,
              tokVar: break;
          else
            Lexer.AheadNext;
          end;
        end;
        case Lexer.AheadTokenID of
          tokDotDot:
            begin
              SubrangeType;
            end;
        else
          begin
            NextToken;
            case TokenID of
              tokSquareClose:
                begin
                  NextToken;
                end;
            else
              begin
                case TokenID of
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
            case TokenID of
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
  end;
end;

procedure TmwSimplePasPar.InterfaceType;
begin
  case TokenID of
    tokInterface:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidInterfaceType);
    end;
  end;
  case TokenID of
    tokEnd:
      begin
        NextToken; { Direct descendant without new members }
      end;
    tokRoundOpen:
      begin
        InterfaceHeritage;
        case TokenID of
          tokEnd:
            begin
              NextToken; { No new members }
            end;
          tokSemiColon: ; { No new members }
        else
          begin
            if TokenID = tokSquareOpen then
            begin
              InterfaceGUID;
            end;
            InterfaceMemberList;
            Expected(tokEnd);
          end;
        end;
      end;
  else
    begin
      if TokenID = tokSquareOpen then
      begin
        InterfaceGUID;
      end;
      InterfaceMemberList; { Direct descendant }
      Expected(tokEnd);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceMemberList;
begin
  while TokenID in [tokFunction, tokProcedure, tokProperty] do
  begin
    ClassMethodOrProperty;
  end;
end;

procedure TmwSimplePasPar.ClassType;
begin
  Expected(tokClass);

  case TokenID of
    tokIdentifier:
      case Lexer.ExID of
        tokAbstract: ExpectedEx(tokAbstract);
        tokHelper:
          begin
            ExpectedEx(tokHelper);
            Expected(tokFor);
            Expected(tokIdentifier);
          end;
      end;
    tokSealed:
      Expected(tokSealed);
  end;

  case TokenID of
    tokEnd:
      begin
        ClassTypeEnd; // DR 2001-07-31
        NextToken; { Direct descendant of TObject without new members }
      end;
    tokRoundOpen:
      begin
        ClassHeritage;
        case TokenID of
          tokEnd:
            begin
              Expected(tokEnd); // DR 2001-07-31
              ClassTypeEnd; // DR 2001-07-31
            end;
          tokSemiColon: ClassTypeEnd; // DR 2001-07-31
        else
          begin
            ClassMemberList; { Direct descendant of TObject }
            Expected(tokEnd); // DR 2001-07-31
            ClassTypeEnd; // DR 2001-07-31
          end;
        end;
      end;
  else
    begin
      ClassMemberList; { Direct descendant of TObject }
      Expected(tokEnd); // DR 2001-07-31
      ClassTypeEnd; // DR 2001-07-31
    end;
  end;
end;

procedure TmwSimplePasPar.ClassHeritage;
begin
  Expected(tokRoundOpen);
  AncestorIdList;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.ClassVisibility;
begin
  if TokenID = tokStrict then
    Expected(tokStrict);

  while ExID in [tokPrivate, tokProtected, tokPublic, tokPublished] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      tokColon, tokComma: ;
    else
      case ExID of
        tokPrivate:
          begin
            VisibilityPrivate;
          end;
        tokProtected:
          begin
            VisibilityProtected;
          end;
        tokPublic:
          begin
            VisibilityPublic;
          end;
        tokPublished:
          begin
            VisibilityPublished;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VisibilityPrivate;
begin
  ExpectedEx(tokPrivate);
end;

procedure TmwSimplePasPar.VisibilityProtected;
begin
  ExpectedEx(tokProtected);
end;

procedure TmwSimplePasPar.VisibilityPublic;
begin
  ExpectedEx(tokPublic);
end;

procedure TmwSimplePasPar.VisibilityPublished;
begin
  ExpectedEx(tokPublished);
end;

procedure TmwSimplePasPar.VisibilityUnknown;
begin
  //
end;

procedure TmwSimplePasPar.ClassMemberList;
begin
  ClassVisibility;
  while TokenID in [tokClass, tokConstructor, tokDestructor, tokFunction,
    tokIdentifier, tokProcedure, tokProperty, tokType, tokSquareOpen, tokVar, tokConst, tokStrict,
     tokCase] do
  begin
    while (TokenID = tokIdentifier) and
      not (ExID in [tokPrivate, tokProtected, tokPublished, tokPublic]) do
    begin
      ClassField;
      SEMICOLON;
      ClassVisibility;
    end;
    while TokenID in [tokClass, tokConstructor, tokDestructor, tokFunction,
      tokProcedure, tokProperty, tokSquareOpen, tokVar, tokConst] do
    begin
      if (TokenID = tokVar) then
      begin
        NextToken;
        ClassField;
        SEMICOLON;
      end
      else
        ClassMethodOrProperty;
    end;

    while TokenID = tokType do
      TypeSection;
    while TokenID = tokCase do
    begin
      VariantSection;
    end;

    ClassVisibility;
  end;
end;

procedure TmwSimplePasPar.ClassMethodOrProperty;
begin
  if TokenID = tokSquareOpen then
    CustomAttribute;

  if TokenID = tokClass
    then ClassClass; //DR 2001-07-16
  case TokenID of
    tokProperty:
      begin
        ClassProperty;
      end;

    tokVar:
      begin
        NextToken;
        while (TokenID = tokIdentifier) and (ExID = tokUnknown) do
        begin
          VarDeclaration;
          NextToken;
        end;
      end;
    tokConst:
      begin
        NextToken;
        while (TokenID = tokIdentifier) and (ExID = tokUnknown) do
        begin
          ConstantDeclaration;
          NextToken;
        end;
      end;
  else
    begin
      ClassMethodHeading;
    end;
  end;
end;

procedure TmwSimplePasPar.ClassProperty;
begin
 // DR 2001-07-19 -> changed. for array-property override failure
  Expected(tokProperty);
  PropertyName;
  case TokenID of
    tokColon, tokSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  PropertySpecifiers;
  case ExID of
    tokDefault:
      begin
        PropertyDefault; //DR 2001-07-16
        SEMICOLON;
      end;
  end;
end;

procedure TmwSimplePasPar.PropertyName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ClassField;
begin
  //IdentifierList;
  FieldNameList;
  Expected(tokColon);
  TypeKind;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ObjectType;
begin
  Expected(tokObject);
  case TokenID of
    tokEnd:
      begin
        ObjectTypeEnd; // DR 2001-07-31
        NextToken; { Direct descendant without new members }
      end;
    tokRoundOpen:
      begin
        ObjectHeritage;
        case TokenID of
          tokEnd:
            begin
              Expected(tokEnd); // DR 2001-07-31
              ObjectTypeEnd; // DR 2001-07-31
            end;
          tokSemiColon: ObjectTypeEnd; // DR 2001-07-31
        else
          begin
            ObjectMemberList; { Direct descendant }
            Expected(tokEnd); // DR 2001-07-31
            ObjectTypeEnd; // DR 2001-07-31
          end;
        end;
      end;
  else
    begin
      ObjectMemberList; { Direct descendant }
      Expected(tokEnd); // DR 2001-07-31
      ObjectTypeEnd; // DR 2001-07-31
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectHeritage;
begin
  Expected(tokRoundOpen);
  AncestorIdList;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.ObjectMemberList;
begin {jdj added tokProperty-call to ObjectProperty 02/07/2001}
  ObjectVisibility;
  while TokenID in [tokConstructor, tokDestructor, tokFunction, tokIdentifier,
    tokProcedure, tokProperty] do
  begin
    while TokenID = tokIdentifier do
    begin
      ObjectField;
      SEMICOLON;
      ObjectVisibility;
    end;
    while TokenID in [tokConstructor, tokDestructor, tokFunction, tokProcedure, tokProperty] do
    begin
      case TokenID of
        tokConstructor, tokDestructor, tokFunction, tokProcedure:
          ObjectMethodHeading;
        tokProperty:
          ObjectProperty;
      end;
    end;
    ObjectVisibility;
  end;
end;

procedure TmwSimplePasPar.ObjectVisibility;
begin
  while ExID in [tokPrivate, tokProtected, tokPublic] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      tokColon, tokComma: ;
    else
      case ExID of
        tokPrivate:
          begin
            VisibilityPrivate;
          end;
        tokProtected:
          begin
            VisibilityProtected;
          end;
        tokPublic:
          begin
            VisibilityPublic;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectField;
begin
  IdentifierList;
  Expected(tokColon);
  TypeKind;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ClassReferenceType;
begin
  Expected(tokClass);
  Expected(tokOf);
  TypeIdentifer;
end;

procedure TmwSimplePasPar.ProceduralType;
var
  TheTokenID: TptTokenKind;
begin
  case TokenID of
    tokFunction:
      begin
        NextToken;
        if TokenID = tokRoundOpen then
        begin
          FormalParameterList;
        end;
        Expected(tokColon);
        ReturnType;
      end;
    tokProcedure:
      begin
        NextToken;
        if TokenID = tokRoundOpen then
        begin
          FormalParameterList;
        end;
      end;
  else
    begin
      SynError(InvalidProceduralType);
    end;
  end;
  if TokenID = tokOf then
  begin
    NextToken;
    Expected(tokObject);
  end;
  Lexer.InitAhead;
  case TokenID of
    tokSemiColon: TheTokenID := Lexer.AheadExID;
  else
    TheTokenID := ExID;
  end;
  while TheTokenID in [tokAbstract, tokCdecl, tokExport, tokExternal,
    tokMessage, tokOverload, tokOverride, tokRegister,
    tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
    tokStatic, tokInline
    ] do
 // DR 2001-11-14 no checking for deprecated etc. since it's captured by the typedecl
  begin
    if TokenID = tokSemiColon then SEMICOLON;
    ProceduralDirective;
    Lexer.InitAhead;
    case TokenID of
      tokSemiColon: TheTokenID := Lexer.AheadExID;
    else
      TheTokenID := ExID;
    end;
  end;
end;

procedure TmwSimplePasPar.StringType;
begin
  VariableReference;
end;

procedure TmwSimplePasPar.PointerType;
begin
  Expected(tokPointerSymbol);
  TypeIdentifer;
end;

procedure TmwSimplePasPar.StructuredType;
begin
  if TokenID = tokPacked then
    NextToken;

  case TokenID of
    tokArray: ArrayType;
    tokRecord: RecordType;
    tokUnion: UnionType;
    tokSet: SetType;
  else
    SynError(InvalidStructuredType);
  end;
end;

procedure TmwSimplePasPar.SimpleType;
begin
  case TokenID of
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
        RealType;
      end;
    tokIdentifier:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          tokPoint, tokSemiColon:
            begin
              TypeIdentifer;
            end;
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
      end;
    tokEnum:
      begin
        EnumeratedScopedType;
      end;
    tokRoundOpen:
      begin
        EnumeratedType;
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

procedure TmwSimplePasPar.RecordFieldConstant;
begin
  Expected(tokIdentifier);
  Expected(tokColon);
  TypedConstant;
end;

procedure TmwSimplePasPar.RecordConstant;
begin
  Expected(tokRoundOpen);
  RecordFieldConstant;
  while (TokenID = tokSemiColon) do
  begin
    SEMICOLON;
    if TokenId <> tokRoundClose then //jdj 2.23.2001
      RecordFieldConstant;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.ArrayConstant;
begin
  Expected(tokRoundOpen);
  TypedConstant;
  while (TokenID = tokComma) do
  begin
    NextToken;
    TypedConstant;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.ClassForward;
begin
  Expected(tokClass);
end;

procedure TmwSimplePasPar.InterfaceForward;
begin
  Expected(tokInterface);
end;

procedure TmwSimplePasPar.ObjectForward;
begin
  Expected(tokObject);
end;

procedure TmwSimplePasPar.TypeDeclaration;
begin
  if (TokenID <> tokType) then
  begin
    TypeName;
    Expected(tokEqual);
  end else
    NextToken();

  Lexer.InitAhead;

  // Some types have their own tokens which messes when declarating base types: `type Currency = Currency`
  if (TokenID = tokIdentifier) and (Lexer.AheadTokenID = tokSemiColon) then
  begin
    TypeAlias;
    Exit;
  end;
  if TokenID = tokType then
  begin
    ExplicitType;
    Exit;
  end;

  case TokenID of
    tokClass:
      begin
        case Lexer.AheadTokenID of
          tokOf:
            begin
              ClassReferenceType;
            end;
          tokSemiColon:
            begin
              ClassForward;
            end;
        else
          begin
            ClassType;
          end;
        end;
      end;
    tokInterface:
      begin
        case Lexer.AheadTokenID of
          tokSemiColon:
            begin
              InterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    tokObject:
      begin
        case Lexer.AheadTokenID of
          tokSemiColon:
            begin
              ObjectForward;
            end;
        else
          begin
            ObjectType;
          end;
        end;
      end;
  else
    TypeKind;
  end;

  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.TypeName;
begin
  Expected(tokIdentifier);
  if TokenId = tokLower then
    TypeParams;
end;

procedure TmwSimplePasPar.ExplicitType;
begin
  Expected(tokType);
  TypeIdentifer;
end;

procedure TmwSimplePasPar.TypeKind;
begin
  if ExID = tokNative then
  begin
    NativeType;
    Exit;
  end;

  if (TokenID = tokIdentifier) and (GenID = tokPrivate) then
    NextToken;

  case TokenID of
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
    tokIdentifier:
      begin
        TypeIdentifer;
      end;
    tokPointerSymbol:
      begin
        Lexer.InitAhead;
        if Lexer.AheadTokenID = tokConst then
        begin
          NextToken;
          NextToken;
          TypeKind;
        end else
          PointerType;
      end;
  else
    begin
      SynError(InvalidTypeKind);
    end;
  end;
end;

procedure TmwSimplePasPar.TypeArgs;
begin
  Expected(tokLower);
  TypeIdentifer;
  while TokenId = tokComma do
  begin
    NextToken;
    TypeIdentifer;
  end;
  Expected(tokGreater);
end;

procedure TmwSimplePasPar.TypedConstant;
begin
  case TokenID of
    tokRoundOpen:
      begin
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> tokSemiColon do
          case Lexer.AheadTokenID of
            tokAnd, tokBegin, tokCase, tokColon, tokEnd, tokElse, tokIf, tokMinus, tokNull, tok_Done,
              tokOr, tokPlus, tokShl, tokShr, tokSlash, tokStar, tokStarStar, tokWhile, tokWith,
              tokXor: break;
            tokRoundOpen:
              begin
                repeat
                  case Lexer.AheadTokenID of
                    tokBegin, tokCase, tokEnd, tokElse, tokIf, tokNull, tok_Done, tokWhile, tokWith: break;
                  else
                    begin
                      case Lexer.AheadTokenID of
                        tokRoundClose:
                          begin
                            NextToken;
                            break;
                          end;
                      else
                        Lexer.AheadNext;
                      end;
                    end;
                  end;
                until Lexer.AheadTokenID = tokRoundClose;
              end;
          else
            Lexer.AheadNext;
          end;
        case Lexer.AheadTokenID of
          tokColon: RecordConstant;
          tokNull, tok_Done: ;
          tokAnd, tokMinus, tokOr, tokPlus, tokShl, tokShr, tokSlash, tokStar, tokStarStar, tokXor:
            begin
              ConstantExpression;
            end;
        else
          begin
            ArrayConstant;
          end;
        end;
      end;
    tokSquareOpen:
      ConstantExpression; // DR 2002-01-11

 { DR: fails with constructed set constants like
    WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];

 (*empty; there mustn't be all fields of a record mentioned*)
   begin
  NextToken;
  if TokenID <> tokSquareClose then
    begin
   case TokenID of
     tokDotDot:
    begin
      NextToken;
      NextToken;
    end;
     else
    NextToken;
    case TokenID of
      tokDotDot:
     begin
       NextToken;
       NextToken;
     end;
    end;
   end;
   while TokenID = tokComma do
     begin
    NextToken;
    NextToken;
    case TokenID of
      tokDotDot:
     begin
       NextToken;
       NextToken;
     end;
    end;
     end;
   Expected(tokSquareClose);
    end
  else NextToken;
   end;}
  else
    begin
      ConstantExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeIdentifer;
begin
  NextToken;
end;

procedure TmwSimplePasPar.TypeAlias;
begin
  TypeIdentifer;
end;

procedure TmwSimplePasPar.ConstantExpression;
begin
  Expression;
end;

procedure TmwSimplePasPar.ResourceDeclaration;
begin
  Identifier;
  Expected(tokEqual);
  CharString;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2002-01-10
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantDeclaration;
begin
  ConstantName;
  case TokenID of
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
  else
    begin
      SynError(InvalidConstantDeclaration);
    end;
  end;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantColon;
begin
  Expected(tokColon);
//JR changed to constant Type
  ConstantType;
  if (not (Lexer.TokenID in [tokAssign, tokEqual])) then
    Expected(tokEqual);
  NextToken;
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.ConstantAssign;
begin
  Expected(tokAssign);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.ConstantEqual;
begin
  Expected(tokEqual);
  ConstantValue;
end;

procedure TmwSimplePasPar.ConstantValue;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.ConstantValueTyped;
begin
  ConstantValue;
end;

procedure TmwSimplePasPar.ConstantName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ConstantType;
begin
  TypeKind;
end;
procedure TmwSimplePasPar.LabelId;
begin
  case TokenID of
    tokIntegerConst:
      begin
        NextToken;
      end;
    tokIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidLabelId);
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureDeclarationSection;
begin
  if (TokenID = tokClass) then
    NextToken();

  case TokenID of
    tokConstructor, tokDestructor, tokProcedure:
      begin
        ProcedureMethodDeclaration();
      end;
    tokFunction, tokOperator:
      begin
        FunctionMethodDeclaration();
      end;
  else
    begin
      SynError(InvalidProcedureDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.LabelDeclarationSection;
begin
  Expected(tokLabel);
  LabelId;
  while (TokenID = tokComma) do
  begin
    NextToken;
    LabelId;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ProceduralDirective; //TODO: Add STATIC and FINAL
begin
  case ExID of
    tokAbstract:
      begin
        NextToken;
      end;
    tokCdecl, tokRegister, tokSafeCall, tokStdCall:
      begin
        DirectiveCalling;
      end;
    tokExport:
      begin
        Directive16Bit;
      end;
    tokExternal:
      begin
        ExternalDirective;
      end;
    tokMessage, tokOverload, tokOverride, tokReintroduce, tokVirtual:
      begin
        DirectiveBinding;
      end;
    tokAssembler:
      begin
        NextToken;
      end;
    tokStatic:
      begin
        NextToken;
      end;
     tokInline:
       begin
         NextToken;
       end;
    tokDeprecated:
      DirectiveDeprecated;
    tokLibrary:
      DirectiveLibrary;
    tokPlatform:
      DirectivePlatform;
    tokNative:
      NextToken;
  else
    begin
      SynError(InvalidProceduralDirective);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportedHeading;
begin
  case TokenID of
    tokFunction:
      begin
        FunctionHeading;
      end;
    tokProcedure:
      begin
        ProcedureHeading;
      end;
  else
    begin
      SynError(InvalidExportedHeading);
    end;
  end;
  if TokenID = tokSemiColon then SEMICOLON;
  case ExID of
    tokForward:
      begin
        ForwardDeclaration; //jdj added 02/07/2001
//        NextToken;
//        SEMICOLON;
      end;
    tokAssembler:
      begin
        NextToken;
        SEMICOLON;
        if Exid = tokForward then
          ForwardDeclaration; //jdj added 02/07/2001
      end;
  else  //TODO: Add STATIC and FINAL
    while ExID in [tokAbstract, tokCdecl, tokExport, tokExternal,
      tokMessage, tokOverload, tokOverride, tokRegister,
      tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
      tokDeprecated, tokLibrary, tokPlatform, // DR 2001-10-20
      tokStatic, tokInline, tokConst] do
    begin
      ProceduralDirective;
      if TokenID = tokSemiColon then SEMICOLON;
    end;
    if ExId = tokForward then
      ForwardDeclaration; //jdj added 02/07/2001
  end;
end;

procedure TmwSimplePasPar.FunctionHeading;
begin
  Expected(tokFunction);
  FunctionProcedureName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(tokColon);
  ReturnType;
end;

procedure TmwSimplePasPar.ProcedureHeading;
begin
  Expected(tokProcedure);
  FunctionProcedureName;
  if TokenID = tokRoundOpen then
  begin
    FormalParameterList;
  end;

end;

procedure TmwSimplePasPar.VarSection;
begin
  case TokenID of
    tokThreadVar:
      begin
        NextToken;
      end;
    tokVar:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidVarSection);
    end;
  end;
  while TokenID in [tokIdentifier, tokSquareOpen] do
  begin
    if TokenID = tokSquareOpen then
      CustomAttribute
    else
    begin
      VarDeclaration;
      SEMICOLON;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeSection;
begin
  Expected(tokType);

  while (TokenID = tokIdentifier) do
  begin
    TypeDeclaration;
    if TokenID = tokEqual then
      TypedConstant;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.TypeParamDecl;
begin
  TypeParamList;
  if TokenId = tokColon then
  begin
    NextToken;
    ConstraintList;
  end;
end;

procedure TmwSimplePasPar.TypeParamDeclList;
begin
  TypeParamDecl;
  while TokenId = tokSemicolon do
  begin
    NextToken;
    TypeParamDecl;
  end;
end;

procedure TmwSimplePasPar.TypeParamList;
begin
  if TokenId = tokSquareOpen then
    AttributeSection;
  Identifier;
  while TokenId = tokComma do
  begin
    NextToken;
    if TokenId = tokSquareOpen then
      AttributeSection;
    Identifier;
  end;
end;

procedure TmwSimplePasPar.TypeParams;
begin
  Expected(tokLower);
  TypeParamDeclList;
  Expected(tokGreater);
end;

procedure TmwSimplePasPar.ConstSection;
begin
  case TokenID of
    tokConst:
      begin
        NextToken;
        while TokenID in [tokIdentifier, tokSquareOpen] do
        begin
          if TokenID = tokSquareOpen then
            CustomAttribute
          else
          begin
            ConstantDeclaration;
            SEMICOLON;
          end;
        end;
      end;
    tokResourceString:
      begin
        NextToken;
        while (TokenID = tokIdentifier) do
        begin
          ResourceDeclaration;
          SEMICOLON;
        end;
      end
  else
    begin
      SynError(InvalidConstSection);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceDeclaration;
begin
  case TokenID of
    tokConst:
      begin
        ConstSection;
      end;
    tokFunction:
      begin
        ExportedHeading;
      end;
    tokProcedure:
      begin
        ExportedHeading;
      end;
    tokResourceString:
      begin
        ConstSection;
      end;
    tokType:
      begin
        TypeSection;
      end;
    tokThreadVar:
      begin
        VarSection;
      end;
    tokVar:
      begin
        VarSection;
      end;
    tokExports:
      begin
        ExportsClause;
      end;
    tokSquareOpen:
      begin
        CustomAttribute;
      end;
  else
    begin
      SynError(InvalidInterfaceDeclaration);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportsElement;
begin
  Expected(tokIdentifier);
  //  if TokenID = tokIndex then
  if FLexer.ExID = tokIndex then //jdj 20001207
  begin
    NextToken;
    Expected(tokIntegerConst);
  end;
  //  if TokenID = tokName then
  if FLexer.ExID = tokName then //jdj 20001207
  begin
    NextToken;
    CharString;
  end;
end;

procedure TmwSimplePasPar.CompoundStatement;
begin
  Expected(tokBegin);
  StatementList;
  Expected(tokEnd);
end;

procedure TmwSimplePasPar.ExportsClause;
begin
  Expected(tokExports);
  ExportsElement;
  while TokenID = tokComma do
  begin
    NextToken;
    ExportsElement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.RequiresClause;
begin
  ExpectedEx(tokRequires);
  RequiresIdentifier;
  while TokenID = tokComma do
  begin
    NextToken;
    RequiresIdentifier;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.RequiresIdentifier;
begin
  Expected(tokIdentifier);
  while Lexer.TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;
end;

procedure TmwSimplePasPar.InitializationSection;
begin
  case TokenID of
    tokInitialization:
      begin
        NextToken;
        StatementList;
        if TokenID = tokFinalization then
        begin
          NextToken;
          StatementList;
        end;
        Expected(tokEnd);
      end;
    tokBegin:
      begin
        CompoundStatement;
      end;
    tokEnd:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidInitializationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.ImplementationSection;
begin
  Expected(tokImplementation);
  if TokenID = tokUses then
  begin
    UsesClause;
  end;
  while TokenID in [tokClass, tokConst, tokConstructor, tokDestructor, tokFunction,
    tokLabel, tokProcedure, tokResourceString, tokThreadVar, tokType, tokVar,
    tokExports, tokSquareOpen] do
  begin
    DeclarationSection;
  end;
end;

procedure TmwSimplePasPar.InterfaceSection;
begin
  Expected(tokInterface);
  if TokenID = tokUses then
  begin
    UsesClause;
  end;
  while TokenID in [tokConst, tokFunction, tokResourceString, tokProcedure,
    tokThreadVar, tokType, tokVar, tokExports, tokSquareOpen] do
  begin
    InterfaceDeclaration;
  end;
end;

procedure TmwSimplePasPar.IdentifierList;
begin
  Identifier; // DR 2001-10-20
  while TokenID = tokComma do
  begin
    NextToken;
    Identifier;
  end;
end;

procedure TmwSimplePasPar.QualifiedIdentifierList;
begin
  QualifiedIdentifier;
  while (TokenID = tokComma) do
  begin
    NextToken;
    QualifiedIdentifier;
  end;
end;

procedure TmwSimplePasPar.CharString;
begin //updated mw 2/22/00, JThurman 6/24/2004
  case TokenID of
    tokAsciiChar, tokIdentifier, tokRoundOpen, tokStringConst:
      while TokenID in
        [tokAsciiChar, tokIdentifier, tokPlus, tokRoundOpen, tokStringConst] do
      begin
        case TokenID of
          tokIdentifier, tokRoundOpen:
            begin
              VariableReference;
            end;
        else
          NextToken;
        end;
        if Lexer.TokenID = tokPoint then
        begin
          NextToken;
          VariableReference;
        end;
      end;
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;

(*procedure TmwSimplePasPar.CharString;
begin //updated mw 2/22/00
  case TokenID of
    tokAsciiChar, tokIdentifier, tokRoundOpen, tokStringConst:
      while TokenID in
        [tokAsciiChar, tokIdentifier, tokPlus, tokRoundOpen, tokStringConst] do
      begin
        case TokenID of
          tokIdentifier, tokRoundOpen:
            begin
              VariableReference;
            end;
        else
          NextToken;
        end;
      end;
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;*)

(*procedure TmwSimplePasPar.CharString;
begin
  case TokenID of
    tokAsciiChar, tokStringConst:
      while TokenID in [tokAsciiChar, tokPlus, tokStringConst] do
      begin
        case TokenID of
          tokPlus:
            begin
              NextToken;
              if TokenID = tokIdentifier then
              begin
                VariableReference;
              end;
            end;
        else
          begin
            NextToken;
          end;
        end;
      end;
    tokIdentifier:
      begin
        VariableReference;
        case TokenID of
          tokPlus:
            begin
              NextToken;
              while TokenID in [tokAsciiChar, tokPlus, tokStringConst] do
              begin
                case TokenID of
                  tokPlus:
                    begin
                      NextToken;
                      if TokenID = tokIdentifier then
                      begin
                        VariableReference;
                      end;
                    end;
                else
                  begin
                    NextToken;
                  end;
                end;
              end;
   end;
        end;
      end
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;*)

procedure TmwSimplePasPar.IncludeFile;
begin
  while (not (TokenID in [tokNull, tok_DONE])) do
    case TokenID of
      tokClass:
        begin
          ProcedureDeclarationSection;
        end;
      tokConst:
        begin
          ConstSection;
        end;
      tokConstructor:
        begin
          ProcedureDeclarationSection;
        end;
      tokDestructor:
        begin
          ProcedureDeclarationSection;
        end;
      tokExports:
        begin
          ExportsClause;
        end;
      tokFunction:
        begin
          ProcedureDeclarationSection;
        end;
      tokIdentifier:
        begin
          Lexer.InitAhead;
          if Lexer.AheadTokenID in [tokColon, tokEqual] then
          begin
            ConstantDeclaration;
            if TokenID = tokSemiColon then SEMICOLON;
          end
          else
            NextToken;
        end;
      tokLabel:
        begin
          LabelDeclarationSection;
        end;
      tokOperator:
        begin
          ProcedureDeclarationSection();
        end;
      tokProcedure:
        begin
          ProcedureDeclarationSection;
        end;
      tokResourceString:
        begin
          ConstSection;
        end;
      tokType:
        begin
          TypeSection;
        end;
      tokThreadVar:
        begin
          VarSection;
        end;
      tokVar:
        begin
          VarSection;
        end;
    else
      begin
        NextToken;
      end;
    end;
end;

procedure TmwSimplePasPar.SkipSpace; //XM Jul-2000
begin
  Expected(tokSpace);
  while TokenID in [tokSpace] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLFco; //XM Jul-2000
begin
  Expected(tokCRLFCo);
  while TokenID in [tokCRLFCo] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLF; //XM Jul-2000
begin
  Expected(tokCRLF);
  while TokenID in [tokCRLF] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.ClassClass;
begin
  Expected(tokClass);
end;

procedure TmwSimplePasPar.PropertyDefault;
begin
  ExpectedEx(tokDefault);
end;

procedure TmwSimplePasPar.IndexSpecifier;
begin
  ExpectedEx(tokIndex);
  ConstantExpression;
end;

procedure TmwSimplePasPar.ClassTypeEnd;
begin
end;

procedure TmwSimplePasPar.ObjectTypeEnd;
begin
end;

procedure TmwSimplePasPar.DirectiveDeprecated;
begin
  ExpectedEx(tokDeprecated);
  if TokenID = tokStringConst then
    NextToken;
end;

procedure TmwSimplePasPar.DirectiveLibrary;
begin
  ExpectedEx(tokLibrary);
end;

procedure TmwSimplePasPar.DirectivePlatform;
begin
  ExpectedEx(tokPlatform);
end;

procedure TmwSimplePasPar.EnumeratedTypeItem;
begin
  QualifiedIdentifier;
  if TokenID = tokEqual then
  begin
    Expected(tokEqual);
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.Identifier;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.AncestorId;
begin
  // !! Although I re-added this function I modified it
  // so that it now calls QualifiedIdentifier, per DR's change
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.AncestorIdList;
begin
  // !! Added this function back in
  AncestorId;
  while(TokenID = tokComma) do
    begin
      NextToken;
      AncestorId;
    end;
end;

procedure TmwSimplePasPar.setOnMessage(Value: TMessageEvent);
begin
  fOnMessage := Value;
  if Assigned(fLexer) then
    fLexer.OnMessage := Value;
end;

procedure TmwSimplePasPar.InitAhead;
begin
  if AheadParse = nil then
    AheadParse := TmwSimplePasPar.Create;
  AheadParse.Lexer.InitFrom(Lexer);
end;

procedure TmwSimplePasPar.GlobalAttributes;
begin
  GlobalAttributeSections;
end;

procedure TmwSimplePasPar.GlobalAttributeSections;
begin
  while TokenID = tokSquareOpen do
    GlobalAttributeSection;
end;

procedure TmwSimplePasPar.GlobalAttributeSection;
begin
  Expected(tokSquareOpen);
  GlobalAttributeTargetSpecifier;
  AttributeList;
  while TokenID = tokComma do
  begin
    Expected(tokComma);
    GlobalAttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(tokSquareClose);
end;

procedure TmwSimplePasPar.GlobalAttributeTargetSpecifier;
begin
  GlobalAttributeTarget;
  Expected(tokColon);
end;

procedure TmwSimplePasPar.GlobalAttributeTarget;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.Attributes;
begin
  AttributeSections;
end;

procedure TmwSimplePasPar.AttributeSections;
begin
  while TokenID = tokSquareOpen do
    AttributeSection;
end;

procedure TmwSimplePasPar.AttributeSection;
begin
  Expected(tokSquareOpen);
  Lexer.InitAhead;
  if Lexer.AheadTokenID = tokColon then
    AttributeTargetSpecifier;
  AttributeList;
  while TokenID = tokComma do
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = tokColon then
      AttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(tokSquareClose);
end;

procedure TmwSimplePasPar.AttributeTargetSpecifier;
begin
  AttributeTarget;
  Expected(tokColon);
end;

procedure TmwSimplePasPar.AttributeTarget;
begin
  case TokenID of
    tokProperty:
      Expected(tokProperty);
    tokType:
      Expected(tokType);
    else
      Expected(tokIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeList;
begin
  Attribute;
  while TokenID = tokComma do
  begin
    Expected(tokComma);
    AttributeList;
  end;
end;

procedure TmwSimplePasPar.Attribute;
begin
  AttributeName;
  if TokenID = tokRoundOpen then
    AttributeArguments;
end;

procedure TmwSimplePasPar.AttributeName;
begin
  case TokenID of
    tokIn, tokOut, tokConst, tokConstRef, tokVar:
      NextToken;
  else
    Expected(tokIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeArguments;
begin
  Expected(tokRoundOpen);
  if TokenID <> tokRoundClose then
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = tokEqual then
      NamedArgumentList
    else
      PositionalArgumentList;
    if Lexer.TokenID = tokEqual then
      NamedArgumentList;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.PositionalArgumentList;
begin
  PositionalArgument;
  while TokenID = tokComma do
  begin
    Expected(tokComma);
    PositionalArgument;
  end;
end;

procedure TmwSimplePasPar.PositionalArgument;
begin
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.NamedArgumentList;
begin
  NamedArgument;
  while TokenID = tokComma do
  begin
    Expected(tokComma);
    NamedArgument;
  end;
end;

procedure TmwSimplePasPar.NamedArgument;
begin
  Expected(tokIdentifier);
  Expected(tokEqual);
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.AttributeArgumentExpression;
begin
  Expression;
end;

procedure TmwSimplePasPar.CustomAttribute;
begin
  AttributeSection;
  AttributeSections;
end;

end.

