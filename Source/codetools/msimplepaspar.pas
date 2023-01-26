{ + --------------------------------------------------------------------------+
  | Class:       TmwPasLex
  | Created:     07.98 - 10.98
  | Author:      Martin Waldenburg
  | Description: A very fast Pascal tokenizer.
  | Version:     1.32
  | Copyright (c) 1998, 1999 Martin Waldenburg
  | All rights reserved.
  |
  | DISCLAIMER:
  |
  | THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS'.
  |
  | ALL EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  | THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
  | PARTICULAR PURPOSE ARE DISCLAIMED.
  |
  | IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
  | INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  | (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  | OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  | INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  | WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  | NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  | THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  |
  |  Martin.Waldenburg@T-Online.de
  +--------------------------------------------------------------------------+ }

// Heavily modified over the years for Simba

unit mSimplePasPar;

{$i simba.inc}

interface

uses
  SysUtils, Classes,
  mPasLexTypes, mPasLex;

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
    fLexer: TmwPasLex;

    fInterfaceOnly: Boolean;
    fLastNoJunkPos: Integer;
    fLastNoJunkLen: Integer;

    AheadParse: TmwSimplePasPar;

    fInRound: Boolean;

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
    procedure SemiColon; virtual;
    function GetExID: TptTokenKind; virtual;
    function GetTokenID: TptTokenKind; virtual;
    procedure AccessSpecifier; virtual;
    procedure AdditiveOperator; virtual;
    procedure AncestorIdList; virtual;
    procedure AncestorId; virtual;
    procedure ArrayConstant; virtual;
    procedure ArrayType; virtual;
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
    procedure ClassTypeEnd; virtual;
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
    procedure DirectiveDeprecated; virtual;
    procedure DirectiveLibrary; virtual;
    procedure DirectivePlatform; virtual;
    procedure EmptyStatement; virtual;
    procedure EnumeratedType; virtual;
    procedure EnumeratedScopedType; virtual;
    procedure EnumeratedTypeItem; virtual;
    procedure ExceptBlock; virtual;
    procedure ExceptionBlockElseBranch; virtual;
    procedure ExceptionClassTypeIdentifier; virtual;
    procedure ExceptionHandler; virtual;
    procedure ExceptionHandlerList; virtual;
    procedure ExceptionIdentifier; virtual;
    procedure ExceptionVariable; virtual;
    procedure ExplicitType; virtual;
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
    procedure IndexSpecifier; virtual;
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
    procedure ObjectTypeEnd; virtual;
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
    procedure SkipSpace; virtual;
    procedure SkipCRLFco; virtual;
    procedure SkipCRLF; virtual;
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
    procedure TypeArgs; virtual;
    procedure TypeParams; virtual;
    procedure TypeParamDecl; virtual;
    procedure TypeParamDeclList; virtual;
    procedure TypeParamList; virtual;
    procedure ConstraintList; virtual;
    procedure Constraint; virtual;
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
    procedure VarName; virtual;
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
    property TokenID: TptTokenKind read GetTokenID;
  public
    NoErrorMessages: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Assign(From: TObject); virtual;
    procedure ErrorMessage(const Message: String); virtual;

    procedure Run; virtual; overload;
    procedure Run(Script: String; FileName: String); virtual; overload;
    procedure Run(FileName: String); virtual; overload;

    property InterfaceOnly: Boolean read fInterfaceOnly write fInterfaceOnly;
    property Lexer: TmwPasLex read fLexer;
    property LastNoJunkPos: Integer read fLastNoJunkPos;
    property LastNoJunkLen: Integer read fLastNoJunkLen;
  end;

implementation

uses
  LazLoggerBase
  {$IFDEF PARSER_BENCHMARK},
  simba.datetime
  {$ENDIF};

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

procedure TmwSimplePasPar.ForwardDeclaration;
begin
  NextToken;
  SemiColon;
end;

procedure TmwSimplePasPar.ObjectProperty;
begin
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
        PropertyDefault;
        SemiColon;
      end;
  end;
end;

procedure TmwSimplePasPar.ObjectPropertySpecifiers;
begin
  if ExID = tokIndex then
  begin
    IndexSpecifier;
  end;
  while ExID in [tokRead, tokWrite] do
  begin
    AccessSpecifier;
  end;
  while ExID in [tokDefault, tokNoDefault, tokStored] do
  begin
    StorageSpecifier;
  end;
  SemiColon;
end;

procedure TmwSimplePasPar.Run;
{$IFDEF PARSER_BENCHMARK}
var T: Double;
{$ENDIF}
begin
  try
    {$IFDEF PARSER_BENCHMARK}
    T := HighResolutionTime();
    {$ENDIF}

    ParseFile();

    {$IFDEF PARSER_BENCHMARK}
    Writeln(Lexer.FileName, ' -> ', FormatFloat('0.00', HighResolutionTime()-T));
    {$ENDIF}
  except
    on E: Exception do
      ErrorMessage(E.Message);
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
      Self.Lexer.CloneDefinesFrom(Lexer);
end;

procedure TmwSimplePasPar.Expected(Sym: TptTokenKind);
begin
  if Sym <> Lexer.TokenID then
  begin
    if TokenID = tokNull then
      ExpectedFatal(Sym)
    else
      ErrorMessage(Format('"%s" expected but found "%s"', [TokenName(Sym), FLexer.Token]));
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ExpectedEx(Sym: TptTokenKind);
begin
  if Sym <> Lexer.ExID then
  begin
    if Lexer.TokenID = tokNull then
      ExpectedFatal(Sym)
    else
      ErrorMessage(Format('"%s" expected but found "%s"', [TokenName(Sym), FLexer.Token]))
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ExpectedFatal(Sym: TptTokenKind);
var
  tS: string;
begin
  if Sym <> Lexer.TokenID then
  begin
    if Lexer.TokenId = tokNull then
      tS := rsEndOfFile
    else
      tS := fLexer.Token;

    raise ESyntaxError.CreatePos(Format(rsExpected, [TokenName(Sym), tS]), fLexer.PosXY);
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtDefineDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtElseDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
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
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfDefDirect(Sender: TmwBasePasLex);
begin
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
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
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfOptDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIncludeDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtResourceDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtUndefDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
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
          SkipSpace;
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

procedure TmwSimplePasPar.SemiColon;
begin
  if (not (Lexer.TokenID in [tokElse, tokEnd, tokExcept, tokFinally, tokFinalization, tokRoundClose, tokUntil])) then
    Expected(tokSemiColon);
end;

function TmwSimplePasPar.GetExID: TptTokenKind;
begin
  Result := fLexer.ExID;
end;

function TmwSimplePasPar.GetTokenID: TptTokenKind;
begin
  Result := fLexer.TokenID;
end;

procedure TmwSimplePasPar.ErrorMessage(const Message: String);
begin
  if NoErrorMessages then
    Exit;

  if (FLexer <> nil) then
  begin
    if (fLexer.MaxPos = -1) or (fLexer.TokenPos < fLexer.MaxPos) then
    begin
      if (fLexer.FileName <> '') then
        DebugLn('[Codetools]: "%s" at line %d, column %d in file "%s"', [Message, FLexer.PosXY.Y + 1, FLexer.PosXY.X, FLexer.FileName])
      else
        DebugLn('[Codetools]: "%s" at line %d, column %d', [Message, FLexer.PosXY.Y + 1, FLexer.PosXY.X]);
    end;
  end else
    DebugLn('[Codetools]: ' + Message);
end;

procedure TmwSimplePasPar.ParseFile;
begin
  SkipJunk;
  case ExID of
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
  SemiColon;
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

  SemiColon;
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
    SemiColon;
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

  SemiColon;
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
  SemiColon;
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
  SemiColon;
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
  CompoundStatement;
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
  AncestorIdList;
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
        QualifiedIdentifier;
      end;
  end;
end;

procedure TmwSimplePasPar.ReadAccessIdentifier;
begin
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.WriteAccessIdentifier;
begin
  QualifiedIdentifier;
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
    SemiColon;
    if TokenID = tokConst then
    begin
      PropertyParameterConst;
    end;
    IdentifierList;
    Expected(tokColon);
    TypeIdentifer;
  end;
  Expected(tokSquareClose);
end;

procedure TmwSimplePasPar.PropertyParameterConst;
begin
  Expected(tokConst);
end;

procedure TmwSimplePasPar.PropertySpecifiers;
begin
  if ExID = tokIndex then
  begin
    IndexSpecifier;
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
  SemiColon;
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
  if TokenId = tokSemicolon then
    SemiColon;
  if ExID in ClassMethodDirectiveEnum then
    ClassMethodDirective;
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

  if TokenId = tokSemicolon then
    SemiColon;

  if exID in ClassMethodDirectiveEnum then
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
  SemiColon;
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
  SemiColon;
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
  SemiColon;
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
    if TokenId = tokSemicolon then
      SemiColon;
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
  if TokenID = tokSemiColon then  SemiColon;
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
  if TokenID = tokSemiColon then SemiColon;
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
  if TokenID = tokSemiColon then SemiColon;
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
  if TokenID = tokSemiColon then SemiColon;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectMethodDirective;
begin
  while ExID in [tokAbstract, tokCdecl, tokExport, tokExternal,
    tokMessage,
    tokOverload,
    tokRegister, tokSafeCall, tokStdCall, tokVirtual,
    tokDeprecated, tokLibrary, tokPlatform, tokStatic, tokInline] do
  begin
    ProceduralDirective;
    if TokenID = tokSemiColon then SemiColon;
  end;
end;

procedure TmwSimplePasPar.Directive16Bit;
begin
  case ExID of
    tokExport:
      begin
        NextToken;
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
    SemiColon;
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
      ErrorMessage(Format('"%s" expected but found "%s"', [TokenName(tokIdentifier), FLexer.Token]));
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
  if TokenID = tokSemiColon then SemiColon;
  case ExID of
    tokForward:
      ForwardDeclaration;
  else
    while (ExID in [tokAbstract, tokCdecl, tokExport, tokExternal,
      tokMessage, tokOverload, tokOverride, tokRegister,
      tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
      tokDeprecated, tokLibrary, tokPlatform,
      tokNative, tokStatic, tokInline, tokConst
       ]) or (TokenID = tokConstRef)
    do
      begin
        case TokenID of
          tokConstRef:
            begin
              NextToken;
              if (TokenID = tokSemiColon) then SemiColon;
            end
        else
          case ExId of
            tokExternal:
              begin
                ProceduralDirective;
                if TokenID = tokSemiColon then SemiColon;
                NoExternal := False;
              end;
          else
            begin
              ProceduralDirective;
              if TokenID = tokSemiColon then SemiColon;
            end;
          end;
        end;
      end;
    if ExID = tokForward then
      ForwardDeclaration
    else if NoExternal then
    begin
      Block;
      SemiColon;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirective;
begin
  ExpectedEx(tokExternal);
  case TokenID of
    tokSemiColon:
      begin
        SemiColon;
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
        SemiColon;
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
    typ := Lexer.TokenID;
    Expected(typ);
  end;

  Expression;

  if (typ <> tokIn) then
  begin
    case TokenID of
      tokTo, tokDownTo: NextToken;
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
    SemiColon;
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
    SemiColon;
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
  Expression();
  while (TokenID = tokAssign) do
  begin
    NextToken();
    Expression();
  end;
  Expected(tokThen);
  Statement();
  if (TokenID = tokElse) then
  begin
    NextToken();
    Statement();
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
    SemiColon;
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
  while TokenID in [tokAddressOp, tokBegin, tokCase, tokDoubleAddressOp,
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
begin
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
          if TokenID in [tokAnd, tokArray, tokAs, tokBegin, tokCase, tokClass,
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
  if TokenID = tokAssign then
    begin
      Expected(tokAssign);
      Expression;
    end;
  while TokenID = tokComma do
  begin
    NextToken;
    Expression;
    if TokenID = tokAssign then
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
    NextToken;
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
  end;
end;

procedure TmwSimplePasPar.VarDeclaration;
begin
  VarNameList;
  if (TokenID = tokColon) then
  begin
    Expected(tokColon);
    TypeKind;
  end;

  case TokenID of
    tokAbsolute: VarAbsolute;
    tokEqual: VarEqual;
    tokAssign: VarAssign;
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
    SemiColon;
    case TokenID of
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
  FieldNameList;
  Expected(tokColon);
  TypeKind;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
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
    SemiColon;
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

procedure TmwSimplePasPar.Variable;
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

procedure TmwSimplePasPar.InterfaceType;
begin
  case TokenID of
    tokInterface:
      begin
        NextToken;
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
        ClassTypeEnd;
        NextToken; { Direct descendant of TObject without new members }
      end;
    tokRoundOpen:
      begin
        ClassHeritage;
        case TokenID of
          tokEnd:
            begin
              Expected(tokEnd);
              ClassTypeEnd;
            end;
          tokSemiColon: ClassTypeEnd;
        else
          begin
            ClassMemberList; { Direct descendant of TObject }
            Expected(tokEnd);
            ClassTypeEnd;
          end;
        end;
      end;
  else
    begin
      ClassMemberList; { Direct descendant of TObject }
      Expected(tokEnd);
      ClassTypeEnd;
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
      SemiColon;
      ClassVisibility;
    end;
    while TokenID in [tokClass, tokConstructor, tokDestructor, tokFunction,
      tokProcedure, tokProperty, tokSquareOpen, tokVar, tokConst] do
    begin
      if (TokenID = tokVar) then
      begin
        NextToken;
        ClassField;
        SemiColon;
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
    then ClassClass;
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
        PropertyDefault;
        SemiColon;
      end;
  end;
end;

procedure TmwSimplePasPar.PropertyName;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ClassField;
begin
  FieldNameList;
  Expected(tokColon);
  TypeKind;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
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
        ObjectTypeEnd;
        NextToken; { Direct descendant without new members }
      end;
    tokRoundOpen:
      begin
        ObjectHeritage;
        case TokenID of
          tokEnd:
            begin
              Expected(tokEnd);
              ObjectTypeEnd;
            end;
          tokSemiColon: ObjectTypeEnd;
        else
          begin
            ObjectMemberList; { Direct descendant }
            Expected(tokEnd);
            ObjectTypeEnd;
          end;
        end;
      end;
  else
    begin
      ObjectMemberList; { Direct descendant }
      Expected(tokEnd);
      ObjectTypeEnd;
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
begin
  ObjectVisibility;
  while TokenID in [tokConstructor, tokDestructor, tokFunction, tokIdentifier,
    tokProcedure, tokProperty] do
  begin
    while TokenID = tokIdentifier do
    begin
      ObjectField;
      SemiColon;
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
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
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
  begin
    if TokenID = tokSemiColon then SemiColon;
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
    SemiColon;
    if TokenId <> tokRoundClose then
      RecordFieldConstant;
  end;
  Expected(tokRoundClose);
end;

procedure TmwSimplePasPar.ArrayConstant;
begin
  Expected(tokRoundOpen);

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

  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
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

  if (TokenID = tokIdentifier) and (ExID = tokPrivate) then
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
  ConstantExpression;
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
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
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
  end;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantColon;
begin
  Expected(tokColon);
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
  SemiColon;
end;

procedure TmwSimplePasPar.ProceduralDirective;
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
  end;
  if TokenID = tokSemiColon then SemiColon;
  case ExID of
    tokForward:
      begin
        ForwardDeclaration;
      end;
  else
    while ExID in [tokAbstract, tokCdecl, tokExport, tokExternal,
      tokMessage, tokOverload, tokOverride, tokRegister,
      tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
      tokDeprecated, tokLibrary, tokPlatform,
      tokStatic, tokInline, tokConst] do
    begin
      ProceduralDirective;
      if TokenID = tokSemiColon then SemiColon;
    end;
    if ExId = tokForward then
      ForwardDeclaration;
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
  end;
  while TokenID in [tokIdentifier, tokSquareOpen] do
  begin
    if TokenID = tokSquareOpen then
      CustomAttribute
    else
    begin
      VarDeclaration;
      SemiColon;
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
    SemiColon;
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
            SemiColon;
          end;
        end;
      end;
    tokResourceString:
      begin
        NextToken;
        while (TokenID = tokIdentifier) do
        begin
          ResourceDeclaration;
          SemiColon;
        end;
      end
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
  end;
end;

procedure TmwSimplePasPar.ExportsElement;
begin
  Expected(tokIdentifier);
  if FLexer.ExID = tokIndex then
  begin
    NextToken;
    Expected(tokIntegerConst);
  end;
  if FLexer.ExID = tokName then
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
  SemiColon;
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
  SemiColon;
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
  Identifier;
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
begin
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
  end;
end;

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
            if TokenID = tokSemiColon then SemiColon;
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

procedure TmwSimplePasPar.SkipSpace;
begin
  Expected(tokSpace);
  while TokenID in [tokSpace] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLFco;
begin
  Expected(tokCRLFCo);
  while TokenID in [tokCRLFCo] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLF;
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
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.AncestorIdList;
begin
  AncestorId;
  while (TokenID = tokComma) do
  begin
    NextToken;
    AncestorId;
  end;
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

