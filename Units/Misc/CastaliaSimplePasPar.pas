{---------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwSimplePasPar.pas, released November 14, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.
Portions CopyRight by Robert Zierer.

Contributor(s): James Jacobson, Dean Hill, Vladimir Churbanov___________________.

Last Modified: 2002/01/16
Current Version: 1.02

Notes: This program is an early beginning of a Pascal parser.
I'd like to invite the Delphi community to develop it further and to create
a fully featured Object Pascal parser.

Modification history:

Jacob Thurman between 20040301 and 20020401

Made ready for Delphi 8:

Added new directives and keywords: static, sealed, final, operator, unsafe.

Added parsing for custom attributes (based on ECMA C# specification).

Added support for nested types in class declarations.

Jeff Rafter between 20020116 and 20020302

Added AncestorId and AncestorIdList back in, but now treat them as Qualified
Identifiers per Daniel Rolf's fix. The separation from QualifiedIdentifierList
is need for descendent classes.

Added VarName and VarNameList back in for descendent classes, fixed to correctly
use Identifiers as in Daniel's verison

Removed fInJunk flags (they were never used, only set)

Pruned uses clause to remove windows dependency. This required changing
"TPoint" to "TTokenPoint". TTokenPoint was declared in mwPasLexTypes

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

ciClassClass for "class function" etc.
ciClassTypeEnd marks end of a class declaration (I needed that for the delphi-objectif-connector)
ciEnumeratedTypeItem for items of enumerations
ciDirectiveXXX for the platform, deprecated, varargs, local
ciForwardDeclaration for "forward" (until now it has been read but no event)
ciIndexSpecifier for properties
ciObjectTypeEnd marks end of an object declaration
ciObjectProperty property for objects
ciObjectPropertySpecifiers property for objects
ciPropertyDefault marking default of property
ciDispIDSpecifier for dispid

patched some functions for implementing the above things and patching the following bugs/improv.:

ObjectProperty handling overriden properties
ProgramFile, UnitFile getting Identifier instead of dropping it
InterfaceHeritage: Qualified identifiers
bugs in variant records
typedconstant failed with complex set constants. simple patch using ConstantExpression

German localization for the two string constants. Define GERMAN for german string constants.

Greg Chapman on 20010522
Better handling of defaut array property
Separate handling of X and Y in property Pixels[X, Y: Integer through identifier "event"
corrected spelling of "ForwardDeclaration"

James Jacobson on 20010223
semi colon before finalization fix

James Jacobson on 20010223
RecordConstant Fix

Martin waldenburg on 2000107
Even Faster lexer implementation !!!!

James Jacobson on 20010107
  Improper handling of the construct
      property TheName: Integer read FTheRecord.One.Two; (stop at second point)
      where one and two are "qualifiable" structures.

James Jacobson on 20001221
   Stops at the second const.
   property Anchor[const Section: string; const Ident:string]: string read
   changed TmwSimplePasPar.PropertyParameterList

On behalf of  Martin Waldenburg and James Jacobson
 Correction in array property Handling (Matin and James) 07/12/2000
 Use of ExId instead of TokenId in ExportsElements (James) 07/12/2000
 Reverting to old behavior in Statementlist [tokintegerConst put back in] (James) 07/12/2000

Xavier Masson InnerCircleProject : XM : 08/11/2000
  Integration of the new version delivered by Martin Waldenburg with the modification I made described just below

Xavier Masson InnerCircleProject : XM : 07/15/2000
  Added "states/events " for      spaces( SkipSpace;) CRLFco (SkipCRLFco) and
    CRLF (SkipCRLF) this way the parser can give a complete view on code allowing
    "perfect" code reconstruction.
    (I fully now that this is not what a standard parser will do but I think it is more usefull this way ;) )
    go to www.innercircleproject.com for more explanations or express your critisism ;)

previous modifications not logged sorry ;)

Known Issues:
-----------------------------------------------------------------------------}
{----------------------------------------------------------------------------
 Last Modified: 05/22/2001
 Current Version: 1.1
 official version
   Maintained by InnerCircle

   http://www.innercircleproject.org

 02/07/2001
   added property handling in Object types
   changed handling of forward declarations in ExportedHeading method
-----------------------------------------------------------------------------}
unit CastaliaSimplePasPar;

{$include ValistusDefines.inc}

interface

uses
  //!! pruned uses
  SysUtils,
  Classes,
  CastaliaPasLexTypes,
  CastaliaPasLex,
  CastaliaSimplePasParTypes;

resourcestring
  rsExpected = '''%s'' expected found ''%s''';
  rsEndOfFile = 'end of file';

const

 ClassMethodDirectiveEnum = [tokAbstract, tokCdecl, tokDynamic, tokMessage, tokOverride,
    tokOverload, tokPascal, tokRegister, tokReintroduce, tokSafeCall, tokStdCall,
    tokVirtual,
    tokDeprecated, tokLibrary, tokPlatform // DR 2001-10-20
    {$IFDEF D8_NEWER}
    , tokStatic //JThurman 2004-11-10
    {$ENDIF}
    {$IFDEF D9_NEWER}
    , tokInline
    {$ENDIF}
    ];  //XM 2002-01-29

type
  ESyntaxError = class(Exception)
  private //jdj 7/18/1999
    FPosXY: TTokenPoint;
  protected

  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreatePos(const Msg: string; aPosXY: TTokenPoint);
    property PosXY: TTokenPoint read FPosXY write FPosXY;
  end;

type
  TmwSimplePasPar = class(TObject)
  private
    fOnMessage: TMessageEvent;
    fLexer: TmwPasLex;
    fInterfaceOnly: Boolean;
    fLastNoJunkPos: Integer;
    fLastNoJunkLen: Integer;

    AheadParse: TmwSimplePasPar;
    procedure InitAhead;
  protected
    fInRound: Boolean;

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
    procedure SEMICOLON; virtual;
    function GetExID: TptTokenKind; virtual;
    function GetTokenID: TptTokenKind; virtual;
    function GetGenID: TptTokenKind; virtual;
    procedure AccessSpecifier; virtual;
    procedure AdditiveOperator; virtual;
    procedure AncestorIdList; virtual; // !! Added ancestorIdList back in...
    procedure AncestorId; virtual; // !! Added ancestorId back in...
    procedure AnonymousMethod; virtual;
    procedure AnonymousMethodType; virtual;
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
    procedure ConstantEqual; virtual;
    procedure ConstantExpression; virtual;
    procedure ConstantName; virtual;
//JR added constant type
    procedure ConstantType; virtual;
    procedure ConstantValue; virtual;
    procedure ConstantValueTyped; virtual;
    procedure ConstParameter; virtual;
    procedure ConstructorHeading; virtual;
    procedure ConstructorName; virtual;
    procedure ConstSection; virtual;
    procedure ContainsClause; virtual;
    procedure ContainsExpression; virtual;
    procedure ContainsIdentifier; virtual;
    procedure ContainsStatement; virtual;
    {$IFDEF D8_NEWER}
    procedure CustomAttribute; virtual; //JThurman 2004-03-03
    {$ENDIF}
    procedure DeclarationSection; virtual;
    procedure Designator; virtual;
    procedure DestructorHeading; virtual;
    procedure DestructorName; virtual;
    procedure Directive16Bit; virtual;
    procedure DirectiveBinding; virtual;
    procedure DirectiveCalling; virtual;
    procedure DirectiveDeprecated; virtual; // DR 2001-10-20
    procedure DirectiveLibrary; virtual; // DR 2001-10-20
    procedure DirectiveLocal; virtual; // DR 2001-11-14
    procedure DirectivePlatform; virtual; // DR 2001-10-20
    procedure DirectiveVarargs; virtual; // DR 2001-11-14
    procedure DispInterfaceForward; virtual;
    procedure DispIDSpecifier; virtual; // DR 2001-07-26
    procedure EmptyStatement; virtual;
    procedure EnumeratedType; virtual;
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
    procedure FileType; virtual;
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
    procedure InheritedStatement; virtual;
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
    procedure NewFormalParameterType; virtual;
    procedure Number; virtual;
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
    procedure OrdinalIdentifier; virtual;
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
    procedure RealIdentifier; virtual;
    procedure RealType; virtual;
    procedure RecordConstant; virtual;
    procedure RecordFieldConstant; virtual;
    procedure RecordType; virtual;
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
    procedure StringIdentifier; virtual;
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
    procedure TypeId; virtual;
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
    procedure VarDeclaration; virtual;
    procedure Variable; virtual;
    procedure VariableList; virtual;
    procedure VariableReference; virtual;
    procedure VariableTwo; virtual;
    procedure VariantIdentifier; virtual;
    procedure VariantSection; virtual;
    procedure VarParameter; virtual;
    procedure VarName; virtual; //!! Added VarName and VarNameList back in...
    procedure VarNameList; virtual;
    procedure VarSection; virtual;
    procedure VisibilityAutomated; virtual;
    procedure VisibilityPrivate; virtual;
    procedure VisibilityProtected; virtual;
    procedure VisibilityPublic; virtual;
    procedure VisibilityPublished; virtual;
    procedure VisibilityUnknown; virtual;
    procedure WhileStatement; virtual;
    procedure WithStatement; virtual;
    procedure WriteAccessIdentifier; virtual;
    {$IFDEF D8_NEWER}//JThurman 2004-03-21
    {This is the syntax for custom attributes, based quite strictly on the
    ECMA syntax specifications for C#, but with a Delphi expression being
    used at the bottom as opposed to a C# expression}
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
    {$ENDIF}
    property ExID: TptTokenKind read GetExID;
    property GenID: TptTokenKind read GetGenID;
    property TokenID: TptTokenKind read GetTokenID;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(From: TObject); virtual;
    procedure SynError(Error: TmwParseError); virtual;
    procedure Run(SourceStream: TCustomMemoryStream; MaxPos: Integer = -1); virtual;

    property InterfaceOnly: Boolean read fInterfaceOnly write fInterfaceOnly;
    property Lexer: TmwPasLex read fLexer;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property LastNoJunkPos: Integer read fLastNoJunkPos;
    property LastNoJunkLen: Integer read fLastNoJunkLen;
  published
  end;

implementation

{ ESyntaxError }

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
  while ExID in [tokRead, tokReadOnly, tokWrite, tokWriteOnly] do
  begin
    AccessSpecifier;
  end;
  while ExID in [tokDefault, tokNoDefault, tokStored] do
  begin
    StorageSpecifier;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.Run(SourceStream: TCustomMemoryStream; MaxPos: Integer = -1);
begin
  TerminateStream(SourceStream);
  fLexer.Origin := SourceStream.Memory;
  fLexer.MaxPos := MaxPos;

  ParseFile;
end;

constructor TmwSimplePasPar.Create;
begin
  inherited Create;
  fLexer := TmwPasLex.Create;
  fLexer.OnCompDirect := {$IFDEF FPC}@{$ENDIF}HandlePtCompDirect;
  fLexer.OnDefineDirect := {$IFDEF FPC}@{$ENDIF}HandlePtDefineDirect;
  fLexer.OnElseDirect := {$IFDEF FPC}@{$ENDIF}HandlePtElseDirect;
  fLexer.OnEndIfDirect := {$IFDEF FPC}@{$ENDIF}HandlePtEndIfDirect;
  fLexer.OnIfDefDirect := {$IFDEF FPC}@{$ENDIF}HandlePtIfDefDirect;
  fLexer.OnIfNDefDirect := {$IFDEF FPC}@{$ENDIF}HandlePtIfNDefDirect;
  fLexer.OnIfOptDirect := {$IFDEF FPC}@{$ENDIF}HandlePtIfOptDirect;
  fLexer.OnIncludeDirect := {$IFDEF FPC}@{$ENDIF}HandlePtIncludeDirect;
  fLexer.OnResourceDirect := {$IFDEF FPC}@{$ENDIF}HandlePtResourceDirect;
  fLexer.OnUnDefDirect := {$IFDEF FPC}@{$ENDIF}HandlePtUndefDirect;
  fLexer.OnIfDirect := {$IFDEF FPC}@{$ENDIF}HandlePtIfDirect;
  fLexer.OnIfEndDirect := {$IFDEF FPC}@{$ENDIF}HandlePtIfEndDirect;
  fLexer.OnElseIfDirect := {$IFDEF FPC}@{$ENDIF}HandlePtElseIfDirect;
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

{next two check for tokNull and ExpectedFatal for an EOF Error}

procedure TmwSimplePasPar.Expected(Sym: TptTokenKind);
begin
  if Sym <> Lexer.TokenID then
  begin
    if TokenID = tokNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
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
      {$IFDEF D8_NEWER} //JThurman 2004-3-19
      tokSquareOpen:
        begin
          CustomAttribute;
        end;
      {$ENDIF}
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

procedure TmwSimplePasPar.SEMICOLON;
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
    FOnMessage(Self, meError, ParserErrorName(Error) + ' found ' + fLexer.Token, fLexer.PosXY.X,
      fLexer.PosXY.Y);

end;

(******************************************************************************
 This part is oriented at the official grammar of Delphi 4
 and parialy based on Robert Zierers Delphi grammar.
 For more information about Delphi grammars take a look at:
 http://www.stud.mw.tu-muenchen.de/~rz1/Grammar.html
******************************************************************************)

procedure TmwSimplePasPar.ParseFile;
//var
//  I: Integer;
begin
//  OutputDebugString('ParseFile');

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

  {$IFDEF D8_NEWER}
  while Lexer.TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;
  {$ENDIF}

  SEMICOLON;
  case ExID of
    tokRequires:
      begin
        RequiresClause;
      end;
  end;
  case ExID of
    tokContains:
      begin
        ContainsClause;
      end;
  end;

  {$IFDEF D8_NEWER}
  while Lexer.TokenID = tokSquareOpen do
  begin
    CustomAttribute;
  end;
  {$ENDIF}

  Expected(tokEnd);
  Expected(tokPoint);
end;

procedure TmwSimplePasPar.ProgramFile;
begin
 // DR 2002-01-11
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
 // DR 2002-01-11

//??
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
//  Expected(tokIdentifier);
  UsedUnitName; //JThurman 2004-11-10
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
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  Expected(tokIdentifier);
  while TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;
  {$ELSE}
  Expected(tokIdentifier);
  {$ENDIF}
end;

procedure TmwSimplePasPar.Block;
begin
  while TokenID in [tokClass, tokConst, tokConstructor, tokDestructor, tokExports,
    tokFunction, tokLabel, tokProcedure, tokResourceString, tokThreadVar, tokType,
    tokVar{$IFDEF D8_NEWER}, tokSquareOpen{$ENDIF}] do
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
    tokFunction:
      begin
        ProcedureDeclarationSection;
      end;
    tokLabel:
      begin
        LabelDeclarationSection;
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
    {$IFDEF D8_NEWER} //JThurman
    tokSquareOpen:
      begin
        CustomAttribute;
      end;
    {$ENDIF}
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
    tokReadOnly:
      begin
        NextToken;
      end;
    tokWriteOnly:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER}
    tokAdd:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: AddAccessIdentifier
      end;
    tokRemove:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: RemoveAccessIdentifier
      end;
    {$ENDIF}
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
  TypeId;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    if TokenID = tokConst then
    begin //jdj 12-21-2000
      PropertyParameterConst;
    end;
    IdentifierList;
    Expected(tokColon);
    TypeId;
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
  TypeId;
  while TokenID = tokSemiColon do
  begin
    SEMICOLON;
    IdentifierList;
    Expected(tokColon);
    TypeId;
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
  while ExID in [tokRead, tokReadOnly, tokWrite, tokWriteOnly
    {$IFDEF D8_NEWER}, tokAdd, tokRemove{$ENDIF}] do
  begin
    AccessSpecifier;
  end;
  if ExID = tokDispId then
  begin
    DispIDSpecifier; // DR 2001-07-26
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
  TypeID;
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
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    tokFunction, tokIdentifier:
      begin
        if (TokenID = tokIdentifier) and (Lexer.ExID <> tokOperator) then
          Expected(tokOperator);
    {$ELSE}
    tokFunction:
      begin
    {$ENDIF}
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
  {$IFDEF D8_NEWER} //JThurman 2004-03-2003
  if (TokenID = tokIdentifier) and (Lexer.ExID = tokOperator) then
    Expected(tokIdentifier) else
  {$ENDIF}
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
  if ExID = tokDispId then
  begin
    DispIDSpecifier; // DR 2001-07-26
    if TokenId = tokSemicolon then // DR 2002-01-14
      SEMICOLON;
  end;
  if ExID in ClassMethodDirectiveEnum     //XM 2002-01-29
   then ClassMethodDirective; //XM 2002-01-26
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

  if ExID = tokDispId then
  begin
    DispIDSpecifier; // DR 2001-07-26
    if TokenId = tokSemicolon then // DR 2002-01-14
      SEMICOLON;
  end;
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
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    tokIdentifier:
      begin
        if Lexer.ExID = tokOperator then
          NextToken;
      end;
    {$ENDIF}
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
  while TokenId in [tokConstructor, tokRecord, tokClass, tokIdentifier] do
  begin
    case TokenId of
      tokConstructor, tokRecord, tokClass: NextToken;
      tokIdentifier: TypeId;
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
  while ExID in [tokAbstract, tokCdecl, tokDynamic, tokExport, tokExternal, tokFar,
    tokMessage, tokNear,
    tokOverload, // DR 2001-08-07
    tokPascal, tokRegister, tokSafeCall, tokStdCall, tokVirtual,
    tokDeprecated, tokLibrary, tokPlatform // DR 2001-10-20
    {$IFDEF D8_NEWER}
    , tokStatic
    {$ENDIF}
    {$IFDEF D9_NEWER}
    , tokInline
    {$ENDIF}
    ] do
  begin
    ProceduralDirective;
    if TokenID = tokSemiColon then SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.Directive16Bit;
begin
  case ExID of
    tokNear:
      begin
        NextToken;
      end;
    tokFar:
      begin
        NextToken;
      end;
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
    tokDynamic:
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
  {$IFDEF D8_NEWER}
  while TokenID = tokSquareOpen do
    CustomAttribute;
  {$ENDIF}
  case TokenID of
    tokString:
      begin
        StringType;
      end;
  else
    begin
      TypeID;
    end;
  end;
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
  {$IFDEF D8_NEWER}//JThurman 2004-03-23
  while TokenID = tokSquareOpen do
    CustomAttribute;
  {$ENDIF}
  case TokenID of
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

procedure TmwSimplePasPar.ConstParameter;
begin
  Expected(tokConst);
  ParameterNameList;
  case TokenID of
    tokColon:
      begin
        NextToken;
        NewFormalParameterType;
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
        NewFormalParameterType;
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
        NewFormalParameterType;
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
              NewFormalParameterType;
              if TokenID = tokEqual then
              begin
                NextToken;
                TypedConstant;
              end;
            end
        end;

        {Expected(tokColon);
        NewFormalParameterType;
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

procedure TmwSimplePasPar.NewFormalParameterType;
begin
  case TokenID of
    tokArray:
      begin
        NextToken;
        Expected(tokOf);
        case TokenID of
          tokConst: (*new in ObjectPascal80*)
            begin
              NextToken;
            end;
        else
          begin
            OldFormalParameterType;
          end;
        end;
      end;
  else
    begin
      OldFormalParameterType;
    end;
  end;
end;

procedure TmwSimplePasPar.OldFormalParameterType;
begin
  case TokenID of
    tokString:
      begin
        NextToken;
      end;
    tokFile:
      begin
        FileType;
      end;
  else
    begin
      TypeID;
    end;
  end;
end;

procedure TmwSimplePasPar.FunctionMethodDeclaration;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-2003
  if (TokenID = tokIdentifier) and (Lexer.ExID = tokOperator) then
    NextToken else
  {$ENDIF}
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
  case TokenID of
    tokSemiColon:
      begin
        FunctionProcedureBlock;
      end;
  else
    begin
      Expected(tokColon);
      ReturnType;
      FunctionProcedureBlock;
    end;
  end;
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
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ObjectNameOfMethod;
begin
  Expected(tokIdentifier);
  {$IFDEF D8_NEWER} //JThurman 2004-03-22
  if TokenId = tokLower then
    TypeParams;
  Lexer.InitAHead;
  Lexer.AheadNext;
  if Lexer.AheadTokenID = tokPoint then
  begin
    Expected(tokPoint);
    ObjectNameOfMethod;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.FunctionProcedureBlock;
var
  NoExternal: Boolean;
begin
  NoExternal := True;
  if TokenID = tokSemiColon
    then SEMICOLON;
  case ExID of
    tokForward:
      ForwardDeclaration; // DR 2001-07-23
  else
    while ExID in [tokAbstract, tokCdecl, tokDynamic, tokExport, tokExternal, tokFar,
      tokMessage, tokNear, tokOverload, tokOverride, tokPascal, tokRegister,
      tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
      tokDeprecated, tokLibrary, tokPlatform, // DR 2001-10-20
      tokLocal, tokVarargs,
      tokAssembler //JT 2004-10-29
      {$IFDEF D8_NEWER}
      , tokStatic
      {$ENDIF}
      {$IFDEF D9_NEWER}
      , tokInline
      {$ENDIF}
       ] // DR 2001-11-14
    do
      begin
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
begin
  Expected(tokFor);
  QualifiedIdentifier;
  {$IFDEF D8_NEWER}
  if Lexer.TokenID = tokAssign then
  begin
    Expected(tokAssign);
    Expression;
    case TokenID of
      tokTo:
        begin
          NextToken;
        end;
      tokDownTo:
        begin
          NextToken;
        end;
    else
      begin
        SynError(InvalidForStatement);
      end;
    end;
    Expression;
  end else
  if Lexer.TokenID = tokIn then
  begin
    Expected(tokIn);
    //QualifiedIdentifier;
    Expression;
  end;
  {$ELSE}
  Expected(tokAssign);
  Expression;
  case TokenID of
    tokTo:
      begin
        NextToken;
      end;
    tokDownTo:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidForStatement);
    end;
  end;
  Expression;
  {$ENDIF}
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
        NewFormalParameterType;
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
  while TokenID <> tokEnd do
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
  case TokenID of
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokPointerSymbol, tokRoundOpen:
      begin
        Designator;
      end;
  end;
  if ExID = tokAt then
  begin
    NextToken;
    Expression;
  end;
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
  VariableReference; (* acessing func.recordfield not allowed here;as well as UNITNAMEID *)
  while fLexer.TokenID = tokComma do
  begin
    NextToken;
    VariableReference;
  end;
end;

procedure TmwSimplePasPar.StatementList;
begin {removed tokIntegerConst jdj-Put back in for labels}
  while TokenID in [tokAddressOp, tokAsm, tokBegin, tokCase, tokDoubleAddressOp,
    tokFor, tokGoTo, tokIdentifier, tokIf, tokInherited, tokInline, tokIntegerConst,
    tokPointerSymbol, tokRaise, tokRoundOpen, tokRepeat, tokSemiColon, tokString,
    tokTry, tokWhile, tokWith] do
  begin
    Statement;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.SimpleStatement;
begin
  case TokenID of
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokPointerSymbol, tokRoundOpen:
      begin
        Designator;
        if TokenID = tokAssign then
        begin
          NextToken;
          if TokenID = tokInherited then
          begin
            NextToken;
          end;
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
    tokInherited:
      begin
        InheritedStatement;
      end;
    tokInLine:
      begin
        InlineStatement;
      end;
    tokIntegerConst:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          tokColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SynError(InvalidLabeledStatement);
            NextToken;
          end;
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
    tokString:
      begin
        StringStatement;
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

procedure TmwSimplePasPar.InheritedStatement;
begin
  Expected(tokInherited);
  case TokenID of
    tokSemiColon: ;
  else
    begin
      Statement;
    end;
  end;
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
  Expected(tokString);
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
  Expected(tokIdentifier);
  case TokenID of
    tokPoint:
      begin
        while TokenID = tokPoint do
        begin //jdj 1/7/2001
          NextToken;
          {$IFDEF D8_NEWER}
          if TokenID in [tokAnd, tokArray, tokAs, tokASM, tokBegin, tokCase, tokClass,
            tokConst, tokConstructor, tokDestructor, tokDispInterface, tokDiv, tokDo,
            tokDOwnto, tokElse, tokEnd, tokExcept, tokExports, tokFile, tokFinal,
            tokFinalization, tokFinally, tokFor, tokFunction, tokGoto, tokIf,
            tokImplementation, tokIn, tokInherited, tokInitialization, tokInline,
            tokInterface, tokIs, tokLabel, tokLibrary, tokMod, tokNil, tokNot, tokObject,
            tokOf, tokOr, tokOut, tokPacked, tokProcedure, tokProgram, tokProperty,
            tokRaise, tokRecord, tokRepeat, tokResourceString, tokSealed, tokSet,
            tokShl, tokShr, tokStatic, tokString, tokThen, tokThreadVar, tokTo, tokTry,
            tokType, tokUnit, tokUnsafe, tokUntil, tokUses, tokVar, tokWhile, tokWith,
            tokXor] then
              NextToken
          else
          {$ENDIF}
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
    tokAnd:
      begin
        NextToken;
      end;
    tokDiv:
      begin
        NextToken;
      end;
    tokMod:
      begin
        NextToken;
      end;
    tokShl:
      begin
        NextToken;
      end;
    tokShr:
      begin
        NextToken;
      end;
    tokSlash:
      begin
        NextToken;
      end;
    tokStar:
      begin
        NextToken;
      end;
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
    tokAddressOp, tokDoubleAddressOp, tokIdentifier, tokInherited, tokPointerSymbol,
      tokRoundOpen:
      begin
        Designator;
      end;
    tokIntegerConst, tokFloat:
      begin
        Number;
      end;
    tokNil:
      begin
        NextToken;
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
    tokString:
      begin
        NextToken;
        Factor;
      end;
    tokFunction, tokProcedure:
      AnonymousMethod;
  end;
end;

procedure TmwSimplePasPar.AdditiveOperator;
begin
  if TokenID in [tokMinus, tokOr, tokPlus, tokXor] then
  begin
    NextToken; // DR 2001-12-19
 {
   case TokenID of
  tokMinus, tokPlus:
    begin
   while TokenID in [tokMinus, tokPlus] do
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
    end;
  tokOr:
    begin
   NextToken;
    end;
  tokXor:
    begin
   NextToken;
    end;
   end;}
  end
  else
  begin
    SynError(InvalidAdditiveOperator);
  end;
end;

procedure TmwSimplePasPar.Term;
begin
  Factor;
  while TokenID in [tokAnd, tokDiv, tokMod, tokShl, tokShr, tokSlash, tokStar] do
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
(*  while TokenID in [tokMinus, tokPlus] do
 begin
   NextToken;								// DR 2001-12-19
 end;
*)
  Term;
  while TokenID in [tokMinus, tokOr, tokPlus, tokXor] do
  begin
    AdditiveOperator;
    Term;
  end;
end;

procedure TmwSimplePasPar.Expression;
begin
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
  end;
end;

procedure TmwSimplePasPar.VarDeclaration;
begin
  // !! Changed back to var name list from IdentifierList
  VarNameList;
  Expected(tokColon);
  TypeKind;
  while ExID in [tokDeprecated, tokLibrary, tokPlatform] do // DR 2001-10-20
    case ExID of
      tokDeprecated: DirectiveDeprecated;
      tokLibrary: DirectiveLibrary;
      tokPlatform: DirectivePlatform;
    end;
  case GenID of
    tokAbsolute:
      begin
        VarAbsolute;
      end;
    tokEqual:
      begin
        VarEqual;
      end;
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
  ConstantValueTyped;
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
    tokPascal:
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
  {$IFDEF D8_NEWER1}
  if TokenID = tokRoundOpen then
  begin
    ClassHeritage;
    if TokenID = tokSemicolon then
      Exit;
  end;
  ClassMemberList;
  {$ELSE}
  FieldList;
  {$ENDIF}
  Expected(tokEnd);
end;

procedure TmwSimplePasPar.FileType;
begin
  Expected(tokFile);
  if TokenID = tokOf then
  begin
    NextToken;
    TypeId;
  end;
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
  Expected(tokOf);
  TypeKind;
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

procedure TmwSimplePasPar.SubrangeType;
begin
  ConstantExpression;
  if TokenID = tokDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.RealIdentifier;
begin
  case ExID of
    tokReal48:
      begin
        NextToken;
      end;
    tokReal:
      begin
        NextToken;
      end;
    tokSingle:
      begin
        NextToken;
      end;
    tokDouble:
      begin
        NextToken;
      end;
    tokExtended:
      begin
        NextToken;
      end;
    tokCurrency:
      begin
        NextToken;
      end;
    tokComp:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidRealIdentifier);
    end;
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

procedure TmwSimplePasPar.OrdinalIdentifier;
begin
  case ExID of
    tokBoolean:
      begin
        NextToken;
      end;
    tokByte:
      begin
        NextToken;
      end;
    tokBytebool:
      begin
        NextToken;
      end;
    tokCardinal:
      begin
        NextToken;
      end;
    tokChar:
      begin
        NextToken;
      end;
    tokDWord:
      begin
        NextToken;
      end;
    tokInt64:
      begin
        NextToken;
      end;
    tokInteger:
      begin
        NextToken;
      end;
    tokLongBool:
      begin
        NextToken;
      end;
    tokLongInt:
      begin
        NextToken;
      end;
    tokLongWord:
      begin
        NextToken;
      end;
    tokPChar:
      begin
        NextToken;
      end;
    tokShortInt:
      begin
        NextToken;
      end;
    tokSmallInt:
      begin
        NextToken;
      end;
    tokWideChar:
      begin
        NextToken;
      end;
    tokWord:
      begin
        NextToken;
      end;
    tokWordbool:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidOrdinalIdentifier);
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
            TypeID;
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
    tokAddressOp:
      begin
        NextToken;
        variable;
      end;
    tokDoubleAddressOp:
      begin
        NextToken;
        variable;
      end;
    tokPointerSymbol:
      begin
        NextToken;
        case TokenID of
          tokRoundClose, tokSquareClose: ;
        else
          begin
            variable;
          end;
        end;
      end;
  else
    variable;
  end;
end;

procedure TmwSimplePasPar.Variable; (* Attention: could also came from proc_call ! ! *)
begin
  case TokenID of
    tokInherited:
      begin
        NextToken;
        QualifiedIdentifier;
      end;
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
//{$IFDEF D11_NEWER}
//    tokLower:
//      begin
//        VariableTwo;
//      end;
//{$ENDIF}
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
            tokBegin, tokClass, tokConst, tokEnd, tokDotDot, tokIn, tokNull, tokThreadVar, tokType,
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
    {$IFDEF D11_NEWER}
    tokLower:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.TypeKind;

        if AheadParse.TokenId = tokGreater then
        begin
          NextToken;
          TypeKind;
          Expected(tokGreater);
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
      end;
    {$ENDIF}
  end;
end;

procedure TmwSimplePasPar.InterfaceType;
begin
  case TokenID of
    tokInterface:
      begin
        NextToken;
      end;
    tokDispInterface:
      begin
        NextToken;
      end
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
  {$IFDEF D8_NEWER} //JThurman 2004-03-19
  case TokenID of
    tokIdentifier: //NASTY hack because Abstract is generally an ExID, except in this case when it should be a keyword.
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
  {$ENDIF}
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
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  if TokenID = tokStrict then
    Expected(tokStrict);
  {$ENDIF}
  while ExID in [tokAutomated, tokPrivate, tokProtected, tokPublic, tokPublished] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      tokColon, tokComma: ;
    else
      case ExID of
        tokAutomated:
          begin
            VisibilityAutomated;
          end;
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

procedure TmwSimplePasPar.VisibilityAutomated;
begin
  ExpectedEx(tokAutomated);
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
    tokIdentifier, tokProcedure, tokProperty
    {$IFDEF D8_NEWER}, tokType, tokSquareOpen, tokVar, tokConst, tokStrict,
     tokCase{$ENDIF}] do
  begin
    while (TokenID = tokIdentifier) and
      not (ExID in [tokPrivate, tokProtected, tokPublished, tokPublic]) do
    begin
      ClassField;
      SEMICOLON;
      ClassVisibility;
    end;
    while TokenID in [tokClass, tokConstructor, tokDestructor, tokFunction,
      tokProcedure, tokProperty{$IFDEF D8_NEWER}, tokSquareOpen, tokVar, tokConst{$ENDIF}] do
    begin
      ClassMethodOrProperty;
    end;
    {$IFDEF D8_NEWER}//JThurman 2004-03-22
    {Nested types for D8}
    while TokenID = tokType do
      TypeSection;
    while TokenID = tokCase do
    begin
      VariantSection;
    end;
    {$ENDIF}
    ClassVisibility;
  end;
end;

procedure TmwSimplePasPar.ClassMethodOrProperty;
begin
  {$IFDEF D8_NEWER}
  if TokenID = tokSquareOpen then
    CustomAttribute;
  {$ENDIF}
  if TokenID = tokClass
    then ClassClass; //DR 2001-07-16
  case TokenID of
    tokProperty:
      begin
        ClassProperty;
      end;
    {$IFDEF D8_NEWER}
    tokVar:
      begin
        NextToken;
        while (TokenID = tokIdentifier) and (ExID = tokUnknown) do
        begin
          ClassField;
          SemiColon;
        end;
      end;
    tokConst:
      begin
        NextToken;
        while (TokenID = tokIdentifier) and (ExID = tokUnknown) do
        begin
          ConstantDeclaration;
          SemiColon;
        end;
      end;
    {$ENDIF}
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
  TypeId;
end;

procedure TmwSimplePasPar.VariantIdentifier;
begin
  case ExID of
    tokOleVariant:
      begin
        NextToken;
      end;
    tokVariant:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidVariantIdentifier);
    end;
  end;
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
  while TheTokenID in [tokAbstract, tokCdecl, tokDynamic, tokExport, tokExternal, tokFar,
    tokMessage, tokNear, tokOverload, tokOverride, tokPascal, tokRegister,
    tokReintroduce, tokSafeCall, tokStdCall, tokVirtual
    {$IFDEF D8_NEWER}, tokStatic{$ENDIF}{$IFDEF D9_NEWER}, tokInline{$ENDIF}
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

procedure TmwSimplePasPar.StringIdentifier;
begin
  case ExID of
    tokAnsiString:
      begin
        NextToken;
      end;
    tokShortString:
      begin
        NextToken;
      end;
    tokWideString:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidStringIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StringType;
begin
  case TokenID of
    tokString:
      begin
        NextToken;
        if TokenID = tokSquareOpen then
        begin
          NextToken;
          ConstantExpression;
          Expected(tokSquareClose);
        end;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.PointerType;
begin
  Expected(tokPointerSymbol);
  TypeId;
end;

procedure TmwSimplePasPar.StructuredType;
begin
  if TokenID = tokPacked then
  begin
    NextToken;
  end;
  case TokenID of
    tokArray:
      begin
        ArrayType;
      end;
    tokFile:
      begin
        FileType;
      end;
    tokRecord:
      begin
        RecordType;
      end;
    tokSet:
      begin
        SetType;
      end;
  else
    begin
      SynError(InvalidStructuredType);
    end;
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
              TypeID;
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

procedure TmwSimplePasPar.DispInterfaceForward;
begin
  Expected(tokDispInterface);
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
  TypeName;
  //For generics
//  if TokenId = tokLower then
//    TypeParams;
  //end generics
  Expected(tokEqual);
  if TokenID = tokType then
  begin
    ExplicitType;
  end;
  Lexer.InitAhead;
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
    tokDispInterface:
      begin
        case Lexer.AheadTokenID of
          tokSemiColon:
            begin
              DispInterfaceForward;
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
    begin
      {$IFDEF D12_NEWER}
      if ExID = tokReference then
        AnonymousMethodType
      else
      {$ENDIF}
      TypeKind;
    end;
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
  if (TokenID = tokString) then
    NextToken
  else
  begin
    Expected(tokIdentifier);
    if TokenId = tokLower then
      TypeParams;
  end;
end;

procedure TmwSimplePasPar.ExplicitType;
begin
  Expected(tokType);
end;

procedure TmwSimplePasPar.TypeKind;
begin
  case TokenID of
    tokAsciiChar, tokFloat, tokIntegerConst, tokMinus, tokNil, tokPlus, tokRoundOpen,
      tokSquareOpen, tokStringConst:
      begin
        SimpleType;
      end;
    tokArray, tokFile, tokPacked, tokRecord, tokSet:
      begin
        StructuredType;
      end;
    tokFunction, tokProcedure:
      begin
        ProceduralType;
      end;
    tokIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          tokPoint, tokSemiColon, tokLower:
            begin
              TypeId;
            end;
        else
          begin
            SimpleExpression;
            if Lexer.TokenID = tokDotDot then
            begin
              NextToken;
              SimpleExpression;
            end;
          end;
        end;
      end;
    tokPointerSymbol:
      begin
        PointerType;
      end;
    tokString:
      begin
        StringType;
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
  TypeId;
  while TokenId = tokComma do
  begin
    NextToken;
    TypeId;
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
            tokAnd, tokBegin, tokCase, tokColon, tokEnd, tokElse, tokIf, tokMinus, tokNull,
              tokOr, tokPlus, tokShl, tokShr, tokSlash, tokStar, tokWhile, tokWith,
              tokXor: break;
            tokRoundOpen:
              begin
                repeat
                  case Lexer.AheadTokenID of
                    tokBegin, tokCase, tokEnd, tokElse, tokIf, tokNull, tokWhile, tokWith: break;
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
          tokColon:
            begin
              RecordConstant;
            end;
          tokNull: ;
          tokAnd, tokMinus, tokOr, tokPlus, tokShl, tokShr, tokSlash, tokStar, tokXor:
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

procedure TmwSimplePasPar.TypeId;
begin
  Lexer.InitAhead;
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  while Lexer.AheadTokenID = tokPoint do
  begin
    //UnitId;
    NextToken;
    Expected(tokPoint);
    Lexer.InitAhead;
  end;
  {$ELSE}
  if Lexer.AheadTokenID = tokPoint then
  begin
    UnitId;
    Expected(tokPoint);
  end;
  {$ENDIF}
  case GenID of
    tokBoolean, tokByte, tokChar, tokDWord, tokInt64, tokInteger, tokLongInt,
      tokLongWord, tokPChar, tokShortInt, tokSmallInt, tokWideChar, tokWord:
      begin
        OrdinalIdentifier;
      end;
    tokComp, tokCurrency, tokDouble, tokExtended, tokReal, tokReal48, tokSingle:
      begin
        RealIdentifier;
      end;
    tokAnsiString, tokShortString, tokWideString:
      begin
        StringIdentifier;
      end;
    tokOleVariant, tokVariant:
      begin
        VariantIdentifier;
      end;
    tokString:
      begin
        StringType;
      end;
  else
    begin
      //Problem: Delphi 8 allows things like 'Object' to be types
      //when they are fully qualified (as System.Object, etc...), so
      //tokIdentifier doesn't quite work right in this context
      //TODO: Come up with a more elegant solution to the 'Object' dilemna
      {$IFDEF D8_NEWER}//JThurman 2004-03-03
      NextToken;
      {$ELSE}
      Expected(tokIdentifier);
      {$ENDIF}
      if TokenId = tokLower then
        TypeArgs;
    end;
  end;
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
  Expected(tokEqual);
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
  TypedConstant;
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
  if TokenID = tokClass then
  begin
    NextToken;
  end;
  case TokenID of
    tokConstructor:
      begin
        ProcedureMethodDeclaration;
      end;
    tokDestructor:
      begin
        ProcedureMethodDeclaration;
      end;
    tokProcedure:
      begin
        ProcedureMethodDeclaration;
      end;
    tokFunction:
      begin
        FunctionMethodDeclaration;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    tokIdentifier:
      begin
        if Lexer.ExID = tokOperator then
        begin
          FunctionMethodDeclaration;
        end
        else
          SynError(InvalidProcedureDeclarationSection);
      end;
    {$ENDIF}
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
    tokCdecl, tokPascal, tokRegister, tokSafeCall, tokStdCall:
      begin
        DirectiveCalling;
      end;
    tokExport, tokFar, tokNear:
      begin
        Directive16Bit;
      end;
    tokExternal:
      begin
        ExternalDirective;
      end;
    tokDynamic, tokMessage, tokOverload, tokOverride, tokReintroduce, tokVirtual:
      begin
        DirectiveBinding;
      end;
    tokAssembler:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER}
    tokStatic:
      begin
        NextToken;
      end;
    {$ENDIF}
    {$IFDEF D9_NEWER}
     tokInline:
       begin
         NextToken;
       end;
    {$ENDIF}
    tokDeprecated:
      DirectiveDeprecated; // DR 2001-10-20
    tokLibrary:
      DirectiveLibrary; // DR 2001-10-20
    tokPlatform:
      DirectivePlatform; // DR 2001-10-20
    tokLocal:
      DirectiveLocal; // DR 2001-11-14
    tokVarargs:
      DirectiveVarargs; // DR 2001-11-14
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
    while ExID in [tokAbstract, tokCdecl, tokDynamic, tokExport, tokExternal, tokFar,
      tokMessage, tokNear, tokOverload, tokOverride, tokPascal, tokRegister,
      tokReintroduce, tokSafeCall, tokStdCall, tokVirtual,
      tokDeprecated, tokLibrary, tokPlatform, // DR 2001-10-20
      tokLocal, tokVarargs // DR 2001-11-14
      {$IFDEF D8_NEWER}, tokStatic{$ENDIF}{$IFDEF D9_NEWER}, tokInline{$ENDIF}
      ] do
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
  {$IFDEF D8_NEWER}//JThurman 2004-03-22
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
  {$ELSE}
  while TokenID = tokIdentifier do
  begin
    VarDeclaration;
    SEMICOLON;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.TypeSection;
begin
  Expected(tokType);
  {$IFDEF D8_NEWER}
  while ((TokenID = tokIdentifier) and (Lexer.ExID in ExTypes)) or
        (Lexer.TokenID = tokSquareOpen) or (Lexer.TokenID = tokString) do
  begin
    if TokenID = tokSquareOpen then
      CustomAttribute
    else
    begin
      TypeDeclaration;
      if TokenID = tokEqual then
        TypedConstant;
      SEMICOLON;
    end;
  end;
  {$ELSE}
  while (TokenID = tokIdentifier) or (TokenID = tokString) do
  begin
    TypeDeclaration;
    if TokenId = tokEqual then //jdj 8/2/00
      TypedConstant;
    SEMICOLON;
  end;
 {$ENDIF}
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
  {$IFDEF D8_NEWER}
  if TokenId = tokSquareOpen then
    AttributeSection;
  {$ENDIF}
  Identifier;
  while TokenId = tokComma do
  begin
    NextToken;
    {$IFDEF D8_NEWER}
    if TokenId = tokSquareOpen then
      AttributeSection;
    {$ENDIF}
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
        {$IFDEF D8_NEWER} //JThurman 2004-03-22
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
        {$ELSE}
        while (TokenID = tokIdentifier) do
        begin
          ConstantDeclaration;
          SEMICOLON;
        end;
        {$ENDIF}
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
    {$IFDEF D8_NEWER} //JThurman 2004-03-03
    tokSquareOpen:
      begin
        CustomAttribute;
      end;
    {$ENDIF}
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
  //  if TokenID = tokResident then
  if FLexer.ExID = tokResident then //jdj 20001207
  begin
    NextToken;
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

procedure TmwSimplePasPar.ContainsClause;
begin
  ExpectedEx(tokContains);
  ContainsStatement;
  while TokenID = tokComma do
  begin
    NextToken;
    ContainsStatement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ContainsStatement;
begin
  ContainsIdentifier;
  if fLexer.TokenID = tokIn then
  begin
    NextToken;
    ContainsExpression;
  end;
end;

procedure TmwSimplePasPar.ContainsIdentifier;
begin
  Expected(tokIdentifier);
end;

procedure TmwSimplePasPar.ContainsExpression;
begin
  ConstantExpression;
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
  {$IFDEF D8_NEWER}
  while Lexer.TokenID = tokPoint do
  begin
    NextToken;
    Expected(tokIdentifier);
  end;
  {$ENDIF}
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
    tokExports
    {$IFDEF D8_NEWER}//JThurman 2004-03-22
    , tokSquareOpen
    {$ENDIF}
    ] do //tokResourceString added jdj
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
    tokThreadVar, tokType, tokVar, tokExports
    {$IFDEF D8_NEWER} //JThurman 2004-03-03
    , tokSquareOpen
    {$ENDIF}
    ] do
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
        [tokAsciiChar, tokIdentifier, tokPlus, tokRoundOpen, tokStringConst,
        tokString] do
      begin
        case TokenID of
          tokIdentifier, tokRoundOpen:
            begin
              VariableReference;
            end;
          tokString: //JT
            begin
              StringStatement;
            end;
        else
          NextToken;
        end;
        {$IFDEF D8_NEWER}
        if Lexer.TokenID = tokPoint then
        begin
          NextToken;
          VariableReference;
        end;
        {$ENDIF}
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
  while TokenID <> tokNull do
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

procedure TmwSimplePasPar.DispIDSpecifier; // DR 2001-07-26
begin
  ExpectedEx(tokDispid);
  ConstantExpression;
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

procedure TmwSimplePasPar.DirectiveLocal;
begin
  ExpectedEx(tokLocal);
end;

procedure TmwSimplePasPar.DirectiveVarargs;
begin
  ExpectedEx(tokVarargs);
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



procedure TmwSimplePasPar.AnonymousMethod;
begin
  case TokenID of
    tokFunction:
      begin
        NextToken;
        if TokenID = tokRoundOpen then
          FormalParameterList;
        Expected(tokColon);
        ReturnType;
      end;
    tokProcedure:
      begin
        NextToken;
        if TokenId = tokRoundOpen then
          FormalParameterList;
      end;
  end;
  Block;
end;

procedure TmwSimplePasPar.AnonymousMethodType;
begin
{$IFDEF D11_NEWER}
  ExpectedEx(tokReference); //ExID = tokReference
  Expected(tokTo);
  case TokenID of
    tokProcedure:
      begin
        NextToken;
        if TokenID = tokRoundOpen then
          FormalParameterList;
      end;
    tokFunction:
      begin
        NextToken;
        if TokenID = tokRoundOpen then
          FormalParameterList;
        Expected(tokColon);
        ReturnType;
      end;
  end;
{$ENDIF}
end;

procedure TmwSimplePasPar.InitAhead;
begin
  if AheadParse = nil then
    AheadParse := TmwSimplePasPar.Create;
  AheadParse.Lexer.InitFrom(Lexer);
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-03

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
    tokIn, tokOut, tokConst, tokVar:
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
  AttributeSection;//TODO: Global vs. Local attributes
{  Lexer.InitAhead;
  if (Lexer.AheadToken = 'assembly') or (Lexer.AheadToken = 'module') then
    GlobalAttributeSections
  else}
    AttributeSections;

end;
{$ENDIF}

end.

