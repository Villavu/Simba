{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Script compiler, which turns a script into a syntax tree.
}
unit lpcompiler;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpparser, lptree;

type
  TLapeTreeMethodMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapeTree_Method>;
  TLapeInternalMethodMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapeTree_InternalMethodClass>;
  TLapeTypeForwards = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapeType>;
  TLapeFuncForwards = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeGlobalVar>;
  TLapeTree_NodeStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeTree_ExprBase>;
  TLapeTree_OpStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeTree_Operator>;

  TLapeCompiler = class;
  TLapeHandleDirective = function(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek: Boolean): Boolean of object;
  TLapeFindFile = function(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase of object;
  TLapeTokenizerArray = array of TLapeTokenizerBase;

  TLapeConditional = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Eval: Boolean;
    Pos: TDocPos;
  end;
  TLapeConditionalStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeConditional>;

  PCompilerState = ^TCompilerState;
  TCompilerState = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Tokenizer: Integer;
    Tokenizers: array of Pointer;
    Defines: lpString;
    Conditionals: TLapeConditionalStack.TTArray;
  end;

  PTempTokenizerState = ^TTempTokenizerState;
  TTempTokenizerState = record
    OldStackInfo: TLapeStackInfo;
    OldTokenizer: TLapeTokenizerBase;
    OldTokenizerIndex: Integer;
    OldState: Pointer;
  end;

  TLapeCompiler = class(TLapeCompilerBase)
  private
    __tmp: TDocPos;
    function getPDocPos: PDocPos; inline;
    function hasTokenizer: Boolean; inline;
    function hasMoreTokenizers: Boolean; inline;
    procedure setTokenizersPeek(Peek: Boolean);
  protected
    FTokenizers: TLapeTokenizerArray;
    FTokenizer: Integer;
    FTreeMethodMap: TLapeTreeMethodMap;
    FInternalMethodMap: TLapeInternalMethodMap;
    FTree: TLapeTree_Base;
    FDelayedTree: TLapeTree_DelayedStatementList;
    FImporting: Pointer;

    FIncludes: TStringList;
    FBaseDefines: TStringList;
    FDefines: TStringList;
    FConditionalStack: TLapeConditionalStack;

    FOnHandleDirective: TLapeHandleDirective;
    FOnFindFile: TLapeFindFile;

    function getDocPos: TDocPos; override;
    procedure Reset; override;
    function getImporting: Boolean; virtual;
    procedure setImporting(Import: Boolean); virtual;
    procedure setBaseDefines(Defines: TStringList); virtual;
    function getTokenizer: TLapeTokenizerBase; virtual;
    procedure setTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    function popTokenizer: TLapeTokenizerBase; virtual;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); virtual;
    function popConditional: TDocPos; virtual;

    function GetToStringMethod(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    procedure InitBaseDefinitions; virtual;

    function EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase; virtual;
    function EnsureTypeExpression(Node: TLapeTree_Base): TLapeTree_Base; virtual;
    function EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range; overload; virtual;
    function EnsureRange(Node: TLapeTree_Base): TLapeTree_Range; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base): TLapeRange; overload; virtual;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; virtual;
    function InIgnore: Boolean; virtual;
    function Next: EParserToken; virtual;
    function isNext(Tokens: EParserTokenSet; out Token: EParserToken): Boolean; overload; virtual;
    function isNext(Tokens: EParserTokenSet): Boolean; overload; virtual;
    function Peek: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    function ParseIdentifierList: TStringArray; virtual;
    function ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList; virtual;
    function ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method; virtual;
    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method; overload; virtual;
    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString): TLapeTree_Method; overload; virtual;
    function ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method; overload; virtual;
    function ParseType(TypeForwards: TLapeTypeForwards): TLapeType; virtual;
    procedure ParseTypeBlock; virtual;
    function ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList; virtual;

    function ParseExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_ExprBase; virtual;
    function ParseTypeExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_Base; virtual;
    function ParseStatement: TLapeTree_Base; virtual;
    function ParseStatementList: TLapeTree_StatementList; virtual;
    function ParseBeginEnd(AllowDot: Boolean = False): TLapeTree_StatementList; virtual;
    function ParseCase: TLapeTree_Case; virtual;
    function ParseFor: TLapeTree_For; virtual;
    function ParseIf: TLapeTree_If; virtual;
    function ParseRepeat: TLapeTree_Repeat; virtual;
    function ParseTry: TLapeTree_Try; virtual;
    function ParseWhile: TLapeTree_While; virtual;
    function ParseWith: TLapeTree_With; virtual;
  public
    FreeTokenizer: Boolean;
    FreeTree: Boolean;

    constructor Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    ); reintroduce; virtual;
    destructor Destroy; override;

    function getState: Pointer; virtual;
    procedure setState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeState(const State: Pointer); virtual;

    function getTempTokenizerState(const ATokenizer: TLapeTokenizerBase): Pointer; overload; virtual;
    function getTempTokenizerState(const AStr: lpString; const AFileName: lpString = ''): Pointer; overload; virtual;
    procedure resetTokenizerState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeTempTokenizerState(const State: Pointer); virtual;

    procedure StartImporting; virtual;
    procedure EndImporting; virtual;

    function addDelayedExpression(Node: TLapeTree_Base; AfterCompilation: Boolean = True; IsGlobal: Boolean = False): TLapeTree_Base; virtual;
    function ParseFile: TLapeTree_Base; virtual;
    function Compile: Boolean; virtual;
    procedure CheckAfterCompile; virtual;

    function getDeclaration(Name: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration; override;
    function getDeclarationNoWith(Name: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration; overload; virtual;
    function getDeclarationNoWith(Name: lpString; LocalOnly: Boolean = False): TLapeDeclaration; overload; virtual;
    function getExpression(AName: lpString; AStackInfo: TLapeStackInfo; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase; overload; virtual;
    function getExpression(AName: lpString; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase; overload; virtual;

    function addLocalDecl(Decl: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration; override;
    function addLocalVar(AVar: TLapeType; Name: lpString = ''): TLapeVar; virtual;

    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: TLapeType; AName: lpString; Value: lpString = ''): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: ELapeBaseType; AName: lpString; Value: lpString = ''): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: ELapeBaseType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: ShortString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; virtual;
	{$IFNDEF Lape_NoWideString}
    function addGlobalVar(Value: WideString; AName: lpString): TLapeGlobalVar; overload; virtual;
	{$ENDIF}
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: AnsiChar; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: WideChar; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addGlobalType(Typ: TLapeType; AName: lpString = ''): TLapeType; overload; virtual;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; virtual;

    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AHeader: TLapeType_Method; AName, Body: lpString): TLapeTree_Method; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addDelayedCode(ACode: lpString; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; virtual;

    property InternalMethodMap: TLapeInternalMethodMap read FInternalMethodMap;
    property Tree: TLapeTree_Base read FTree;
    property DelayedTree: TLapeTree_DelayedStatementList read FDelayedTree;
    property Importing: Boolean read getImporting write setImporting;
  published
    property Tokenizer: TLapeTokenizerBase read getTokenizer write setTokenizer;
    property BaseDefines: TStringList read FBaseDefines write setBaseDefines;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
    property OnFindFile: TLapeFindFile read FOnFindFile write FOnFindFile;
  end;

implementation

uses
  Variants,
  lpexceptions, lpinterpreter, lpeval;

function TLapeCompiler.getPDocPos: PDocPos;
begin
  __tmp := DocPos;
  Result := @__tmp;
end;

function TLapeCompiler.hasTokenizer: Boolean;
begin
  Result := FTokenizer >= 0;
end;

function TLapeCompiler.hasMoreTokenizers: Boolean;
begin
  Result := FTokenizer > 0;
end;

type
  __LapeTokenizerBase = class(TLapeTokenizerBase);
procedure TLapeCompiler.setTokenizersPeek(Peek: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(FTokenizers) do
    if (FTokenizers[i] <> nil) then
      __LapeTokenizerBase(FTokenizers[i]).FInPeek := Peek;
end;

function TLapeCompiler.getDocPos: TDocPos;
begin
  if hasTokenizer() then
    Result := Tokenizer.DocPos
  else
    Result := NullDocPos;
end;

procedure TLapeCompiler.Reset;
begin
  inherited;
  EndImporting();

  FTokenizer := High(FTokenizers);
  while hasMoreTokenizers() do
    popTokenizer();

  if (FTokenizer = 0) and (FTokenizers[0] <> nil) then
    FTokenizers[0].Reset();

  if FreeTree and (FTree <> nil) then
    FTree.Free();
  FTree := nil;

  if (FDelayedTree <> nil) then
    FDelayedTree.Clean();
  if (FTreeMethodMap <> nil) then
    FTreeMethodMap.Clear();

  if (FIncludes <> nil) then
    FIncludes.Clear();
  if (FConditionalStack <> nil) then
    FConditionalStack.Reset();

  if (FDefines <> nil) then
    if (FBaseDefines <> nil) then
      FDefines.Assign(FBaseDefines)
    else
      FDefines.Clear();
end;

function TLapeCompiler.getImporting: Boolean;
begin
  Result := FImporting <> nil;
end;

procedure TLapeCompiler.setImporting(Import: Boolean);
begin
  if Import then
    StartImporting
  else
    EndImporting;
end;

procedure TLapeCompiler.setBaseDefines(Defines: TStringList);
begin
  Assert(FBaseDefines <> nil);
  FBaseDefines.Assign(Defines);
  Reset();
end;

function TLapeCompiler.getTokenizer: TLapeTokenizerBase;
begin
  if hasTokenizer() then
    Result := FTokenizers[FTokenizer]
  else
    Result := nil;
end;

procedure TLapeCompiler.setTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  if hasTokenizer() and (FreeTokenizer or hasMoreTokenizers()) and (FTokenizer < Length(FTokenizers)) and
     (FTokenizers[FTokenizer] <> nil) and (FTokenizers[FTokenizer] <> ATokenizer)
  then
    FTokenizers[FTokenizer].Free();

  if (not hasTokenizer()) then
    FTokenizer := 0;
  if (Length(FTokenizers) <= FTokenizer) then
    SetLength(FTokenizers, FTokenizer + 1);
  FTokenizers[FTokenizer] := ATokenizer;

  if (ATokenizer <> nil) then
  begin
    ATokenizer.OnHandleDirective := {$IFDEF FPC}@{$ENDIF}HandleDirective;
    ATokenizer.Reset();
  end;
end;

procedure TLapeCompiler.pushTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  if (Tokenizer <> nil) and (ATokenizer <> nil) then
    __LapeTokenizerBase(ATokenizer).FInPeek := Tokenizer.InPeek;
  Inc(FTokenizer);
  setTokenizer(ATokenizer);
end;

function TLapeCompiler.popTokenizer: TLapeTokenizerBase;
begin
  Result := Tokenizer;
  setTokenizer(nil);
  Dec(FTokenizer);
end;

procedure TLapeCompiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
var
  Conditional: TLapeConditional;
begin
  Assert(FConditionalStack <> nil);
  with Conditional do
  begin
    Eval := AEval;
    Pos := ADocPos;
  end;
  FConditionalStack.Push(Conditional);
end;

function TLapeCompiler.popConditional: TDocPos;
begin
  Assert(FConditionalStack <> nil);
  if (FConditionalStack.Size > 0) then
    Result := FConditionalStack.Pop().Pos
  else
    LapeException(lpeLostConditional, Tokenizer.DocPos);
end;

function TLapeCompiler.GetToStringMethod(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar;
var
  Body: lpString;
begin
  Result := nil;
  if (Sender <> nil) and (AType <> nil) and (AType.Params.Count = 1) and (AType.Params[0].VarType <> nil) then
  begin
    SetLength(AParams, 1);
    AParams[0] := AType.Params[0].VarType;
    AResult := AType.Res;
  end;
  if (Sender = nil) or (Length(AParams) <> 1) or (AParams[0] = nil) or ((AResult <> nil) and (AResult.BaseType <> ltString)) then
    Exit;

  Body := AParams[0].VarToStringBody(Sender);
  if (Body <> '') then
  begin
    if (AResult = nil) and (AType = nil) then
      AResult := getBaseType(ltString);
    if (AType = nil) then
      AType := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptConst], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;

    Sender.addMethod(AType.NewGlobalVarP());
    Result := addGlobalFunc(AType, 'ToString', 'override;' + Body).Method;
  end;
end;

procedure TLapeCompiler.InitBaseDefinitions;
  procedure addToString;
  var
    OLMethod: TLapeType_OverloadedMethod;
    BaseType: ELapeBaseType;
  begin
    OLMethod := TLapeType_OverloadedMethod.Create(Self, nil);
    OLMethod.OnFunctionNotFound := {$IFDEF FPC}@{$ENDIF}GetToStringMethod;
    OLMethod.NeedFullMatch := True;
    addManagedDecl(OLMethod);

    for BaseType := Low(ELapeBaseType) to High(ELapeBaseType) do
      if ({$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType] <> nil) then
        OLMethod.addMethod(
          TLapeType_Method(addManagedType(
            TLapeType_Method.Create(
              Self,
              [getBaseType(BaseType)],
              [lptConst],
              [TLapeGlobalVar(nil)],
              getBaseType(ltString)
            )
          )).NewGlobalVar(
            {$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType]
          )
        );

    addGlobalVar(OLMethod.NewGlobalVar('ToString'));
  end;
begin
  with FBaseDefines do
  begin
    add('Lape');
    add('Sesquipedalian');
  end;

  addGlobalType(getBaseType(ltString).createCopy(), 'String');
  addGlobalType(getBaseType(ltChar).createCopy(), 'Char');
  addGlobalType(getBaseType(ltEvalBool).createCopy(), 'EvalBool');
  addGlobalVar(True, 'True').isConstant := True;
  addGlobalVar(False, 'False').isConstant := True;
  addGlobalVar(nil, 'nil').isConstant := True;
  addGlobalVar(Null, 'Null').isConstant := True;
  addGlobalVar(Unassigned, 'Unassigned').isConstant := True;

  addToString();
  addDelayedCode(
    _LapeToString_Enum +
    Format(_LapeToString_Set, ['Small', Ord(High(ELapeSmallEnum))]) +
    Format(_LapeToString_Set, ['Large', Ord(High(ELapeLargeEnum))]) +
    _LapeToString_Array
  );
end;

function TLapeCompiler.EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
begin
 if (Node <> nil) then
   Result := Node
 else
   Result := TLapeTree_ExprBase.Create(Self, getPDocPos());
end;

function TLapeCompiler.EnsureTypeExpression(Node: TLapeTree_Base): TLapeTree_Base;
begin
 if (Node <> nil) then
   Result := Node
 else
   Result := TLapeTree_Base.Create(Self, getPDocPos());
end;

function TLapeCompiler.EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range;
begin
  VarType := nil;
  if (Node <> nil) and (Node is TLapeTree_Range) then
    Result := TLapeTree_Range(Node)
  else if (Node <> nil) and (Node is TLapeTree_VarType) and (TLapeTree_VarType(Node).VarType <> nil) then
  begin
    Result := TLapeTree_Range.Create(Self, @Node._DocPos);
    with TLapeTree_VarType(Node).VarType do
    begin
      Result.Lo := TLapeTree_GlobalVar.Create(VarLo(), Self, @Node._DocPos);
      Result.Hi := TLapeTree_GlobalVar.Create(VarHi(), Self, @Node._DocPos);
    end;
  end
  else
    LapeException(lpeInvalidRange, [Node, Self]);

  if (Result.Lo <> nil) and (Result.Hi <> nil) then
    VarType := Result.Hi.resType();
  if (VarType = nil) or (not VarType.CompatibleWith(Result.Lo.resType())) then
    LapeException(lpeInvalidRange, Node.DocPos);
end;

function TLapeCompiler.EnsureRange(Node: TLapeTree_Base): TLapeTree_Range;
var
  VarType: TLapeType;
begin
  Result := EnsureRange(Node, VarType);
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange;
var
  Range: TLapeTree_Range;
  Lo, Hi: TLapeGlobalVar;
begin
  Range := EnsureRange(Node, VarType);
  try
    if (Node = nil) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);

    Lo := Range.Lo.Evaluate();
    Hi := Range.Hi.Evaluate();
    if (Lo = nil) or (Lo.VarType = nil) or (Lo.VarType.BaseIntType = ltUnknown) or
       (Hi = nil) or (Hi.VarType = nil) or (Hi.VarType.BaseIntType = ltUnknown)
    then
      LapeException(lpeInvalidRange, Node.DocPos)
    else if (not Lo.isConstant) or (not Hi.isConstant) then
      LapeException(lpeConstantExpected, Node.DocPos);

    Result.Lo := Lo.AsInteger;
    Result.Hi := Hi.AsInteger;
    if (Result.Hi < Result.Lo) then
      LapeException(lpeInvalidRange, Node.DocPos);
  finally
    if (Range <> Node) then
      Range.Free();
  end;
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base): TLapeRange;
var
  VarType: TLapeType;
begin
  Result := EnsureConstantRange(Node, VarType);
end;

function TLapeCompiler.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
var
  NewTokenizer: TLapeTokenizerBase;
  Pos: TDocPos;

  procedure switchConditional;
  var
    Conditional: TLapeConditional;
  begin
    if (FConditionalStack.Size <= 0) then
      LapeException(lpeLostConditional, Sender.DocPos)
    else
    begin
      Conditional := FConditionalStack.Pop();
      Conditional.Eval := not Conditional.Eval;
      FConditionalStack.Push(Conditional);
    end;
  end;

  procedure RemoveFromStringList(l: TStringList; s: lpString);
  var
    i: Integer;
  begin
    i := l.IndexOf(s);
    while (i > -1) do
    begin
      l.Delete(i);
      i := l.IndexOf(s);
    end;
  end;

begin
  Assert(Sender = Tokenizer);
  Result := True;
  NewTokenizer := nil;

  if ({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil) then
    if FOnHandleDirective(Self, Directive, Argument, Sender.InPeek) then
      Exit;

  Directive := LowerCase(Directive);
  if (Directive = 'ifdef') or (Directive = 'ifndef') then
    pushConditional((not InIgnore()) and ((FDefines.IndexOf(Trim(Argument)) > -1) xor (Directive = 'ifndef')), Sender.DocPos)
  else if (Directive = 'else') then
    switchConditional()
  else if (Directive = 'endif') then
    Pos := popConditional() //Assign to a variable to work around FPC internal compiler error
  else if InIgnore() then
    {nothing}
  else if (Directive = 'define') then
    FDefines.add(Trim(Argument))
  else if (Directive = 'undef') then
    RemoveFromStringList(FDefines, Trim(Argument))
  else if (Directive = 'i') or (Directive = 'include') or (Directive = 'include_once') then
  begin
    if ({$IFNDEF FPC}@{$ENDIF}FOnFindFile <> nil) then
      NewTokenizer := FOnFindFile(Self, Argument);

    if (not Sender.InPeek) then
      if (Directive = 'include_once') and (FIncludes.IndexOf(Argument) > -1) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [Argument], Sender.DocPos)
      else
        FIncludes.add(Argument);

    if (NewTokenizer = nil) then
      if (FTokenizer + 1 < Length(FTokenizers)) and (FTokenizers[FTokenizer + 1] <> nil) and (FTokenizers[FTokenizer + 1].FileName = Argument) then
      begin
        NewTokenizer := FTokenizers[FTokenizer + 1];
        NewTokenizer.Reset();
      end
      else if (Argument = '') or (not FileExists(Argument)) then
        LapeExceptionFmt(lpeFileNotFound, [Argument], Sender.DocPos)
      else
        NewTokenizer := TLapeTokenizerFile.Create(Argument);

    pushTokenizer(NewTokenizer);
  end
  else if Sender.InPeek then
    {nothing}
  else
    Result := False;

  WriteLn('DIRECTIVE: '+Directive);
end;

function TLapeCompiler.InIgnore: Boolean;
begin
  Result := (FConditionalStack.Cur >= 0) and (not FConditionalStack.Top.Eval);
end;

function TLapeCompiler.Next: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := Tokenizer.Tok;
  repeat
    Result := Tokenizer.Next{NoWhiteSpace}();
    if (Result = tk_NULL) and hasMoreTokenizers() then
    begin
      if Tokenizer.InPeek then
        Dec(FTokenizer)
      else
        popTokenizer();
      Result := Tokenizer.Next{NoWhiteSpace}();
    end;
  until (Result = tk_NULL) or ((not (Result in TokJunk)) and (not InIgnore()));
  __LapeTokenizerBase(Tokenizer).FLastTok := PrevTok;
end;

function TLapeCompiler.isNext(Tokens: EParserTokenSet; out Token: EParserToken): Boolean;
var
  OldState: Pointer;
begin
  Result := False;
  OldState := getState();
  try
    setTokenizersPeek(True);
    Token := Next();
    Result := Token in Tokens;
  finally
    if (not Result) then
      setState(OldState)
    else
    begin
      freeState(OldState);
      setTokenizersPeek(False);
    end;
  end;
end;

function TLapeCompiler.isNext(Tokens: EParserTokenSet): Boolean;
var
  Token: EParserToken;
begin
  Result := isNext(Tokens, Token);
end;

function TLapeCompiler.Peek: EParserToken;
var
  OldState: Pointer;
begin
  OldState := getState();
  try
    setTokenizersPeek(True);
    Result := Next();
  finally
    setState(OldState);
  end;
end;

function TLapeCompiler.Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    Next();
  Result := Tokenizer.Tok;
  if (Result <> Token) then
    LapeExceptionFmt(lpeExpectedOther, [Tokenizer.TokString, LapeTokenToString(Token)], Tokenizer.DocPos);
  if NextAfter then
    Next();
end;

function TLapeCompiler.Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    Next();
  Result := Tokenizer.Tok;
  if (not (Result in Tokens)) then
    LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(Result)], Tokenizer.DocPos);
  if NextAfter then
    Next();
end;

function TLapeCompiler.ParseIdentifierList: TStringArray;
begin
  SetLength(Result, 0);
  repeat
    Expect(tk_Identifier, True, False);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Tokenizer.TokString;
  until (Next() <> tk_sym_Comma);
end;

function TLapeCompiler.ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList;
var
  FuncForwards: TLapeFuncForwards;
  Statement: TLapeTree_Base;
  Token: EParserToken;
  DoBreak: Boolean;
begin

  Result := TLapeTree_StatementList.Create(Self, getPDocPos());
  try
    FuncForwards := TLapeFuncForwards.Create(nil, dupError);

    try
      DoBreak := False;
      repeat
        Statement := nil;
        if isNext([tk_NULL, tk_kw_Begin, tk_kw_Const, tk_kw_Var, tk_kw_Function, tk_kw_Procedure, tk_kw_Type], Token) then
          case Token of
            tk_NULL: Break;
            tk_kw_Begin:
              begin
                Statement := ParseBeginEnd(not StopAfterBeginEnd);
                DoBreak := (Tokenizer.Tok = tk_sym_Dot) or StopAfterBeginEnd;
              end;
            tk_kw_Const, tk_kw_Var: Statement := ParseVarBlock();
            tk_kw_Function, tk_kw_Procedure: addDelayedExpression(ParseMethod(FuncForwards));
            tk_kw_Type: ParseTypeBlock();
          end
        {$IFNDEF Lape_ForceBlock}
        else if (not StopAfterBeginEnd) then
          Statement := ParseStatement()
        {$ENDIF}
        else
          LapeException(lpeBlockExpected, Tokenizer.DocPos);

        if (Statement <> nil) then
          Result.addStatement(Statement);
        Expect([tk_sym_Dot, tk_sym_SemiColon], False, False);
      until DoBreak;

      if (FuncForwards.Count > 0) then
        LapeExceptionFmt(lpeInvalidForward, [FuncForwards[0].Name], FuncForwards[0].VarType.DocPos);
    finally
      while (FuncForwards.Count > 0) do
        FuncForwards.Delete(0).Free();
      FuncForwards.Free();
    end;

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method;
var
  i: Integer;
  isFunction: Boolean;
  Identifiers: TStringArray;
  Param: TLapeParameter;
  Token: EParserToken;
  Default: TLapeTree_ExprBase;
begin
  //Expect([tk_kw_Function, tk_kw_Procedure], True, False);
  isFunction := (Tokenizer.Tok = tk_kw_Function);
  Result := TLapeType_Method.Create(Self, nil, nil, '', getPDocPos());

  try

    if isNext([tk_Identifier, tk_sym_ParenthesisOpen], Token) and (Token = tk_Identifier) then
    begin
      Name := Tokenizer.TokString;
      Token := tk_NULL;
    end;

    if (Token = tk_sym_ParenthesisOpen) or ((Token = tk_NULL) and isNext([tk_sym_ParenthesisOpen])) then
      repeat
        Param := NullParameter;
        if isNext([tk_NULL, tk_sym_ParenthesisClose, tk_kw_Const, tk_kw_Out, tk_kw_Var], Token) then
          case Token of
            tk_NULL: Break;
            tk_sym_ParenthesisClose:
              begin
                if (Tokenizer.LastTok <> tk_sym_ParenthesisOpen) then
                  Expect(tk_sym_SemiColon, False, False);
                Break;
              end;
            tk_kw_Const: Param.ParType := lptConst;
            tk_kw_Out:   Param.ParType := lptOut;
            tk_kw_Var:   Param.ParType := lptVar;
          end;

        Identifiers := ParseIdentifierList();
        Expect([tk_sym_Colon, tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
        if (Tokenizer.Tok = tk_sym_Colon) then
        begin
          Param.VarType := ParseType(nil);
          if (Param.VarType = nil) then
            LapeException(lpeTypeExpected, Tokenizer.DocPos);
          Expect([tk_sym_Equals, tk_sym_SemiColon, tk_sym_ParenthesisClose], True, False);

          if (Tokenizer.Tok = tk_sym_Equals) then
          begin
            Default := ParseExpression([tk_sym_ParenthesisClose], True, False);
            try
              Default := setExpectedType(Default, Param.VarType) as TLapeTree_ExprBase;
              Param.Default := Default.Evaluate();
              if (not (Param.ParType in Lape_ValParams)) and ((Param.Default = nil) or Param.Default.isConstant) then
                LapeException(lpeVariableExpected, Default.DocPos);
            finally
              Default.Free();
            end;
            Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
          end;
        end
        else if (not (Param.ParType in Lape_RefParams)) then
          Expect(tk_sym_Colon, False, False);

        for i := 0 to High(Identifiers) do
        begin
          if addToScope then
            if (FStackInfo = nil) then
              LapeException(lpeImpossible, Tokenizer.DocPos)
            else if (LapeCase(Identifiers[i]) = LapeCase(Name)) or hasDeclaration(Identifiers[i], True) then
              LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], Tokenizer.DocPos)
            else
              FStackInfo.addVar(Param.ParType, Param.VarType, Identifiers[i]);

          Result.addParam(Param);
        end;
      until (Tokenizer.Tok = tk_sym_ParenthesisClose);

    if isFunction then
    begin
      Expect(tk_sym_Colon, True, False);
      Result.Res := ParseType(nil);
      if (Result.Res = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);

      if addToScope then
        if (FStackInfo = nil) then
          LapeException(lpeImpossible, Tokenizer.DocPos)
        else if (LapeCase(Name) = LapeCase('Result')) or hasDeclaration('Result', True) then
          LapeExceptionFmt(lpeDuplicateDeclaration, ['Result'], Tokenizer.DocPos)
        else
          FStackInfo.addVar(lptOut, Result.Res, 'Result');
    end;

    Result := TLapeType_Method(addManagedType(Result));

  except
    Result.Free();
    raise;
  end;
end;

type
  __LapeTree_Method = class(TLapeTree_Method);
  __LapeType = class(TLapeType);
  __LapeVar = class(TLapeVar);
function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method;
var
  Pos: TDocPos;
  OldDeclaration: TLapeDeclaration;

  procedure swapMethodTree(varFrom, varTo: TLapeGlobalVar);
  var
    Method: TLapeTree_Method;
  begin
    Method := FTreeMethodMap[IntToStr(PtrUInt(varFrom))];
    if (Method <> nil) then
    begin
      varTo.isConstant := True;
      __LapeTree_Method(Method).FMethod := varTo;
      FTreeMethodMap[IntToStr(PtrUInt(varTo))] := Method;
    end;
  end;

  procedure setMethodDefaults(AVar: TLapeGlobalVar; AMethod: TLapeType_Method);
  type
    TLapeClassType = class of TLapeType_Method;
  var
    i, ii: Integer;
    Params: array of TLapeParameter;
    NewMethod: TLapeType_Method;
  begin
    if (AMethod = nil) or (FStackInfo = nil) or (AMethod.Params.Count <= 0) or (FStackInfo.TotalParamSize <> AMethod.ParamSize) then
      Exit;

    NewMethod := TLapeClassType(AMethod.ClassType).Create(Self, nil, AMethod.Res, AMethod.Name, @AMethod._DocPos);
    __LapeType(NewMethod).FBaseType := AMethod.BaseType;

    ii := 0;
    Params := AMethod.Params.ExportToArray();
    for i := 0 to High(Params) do
    begin
      while (ii < FStackInfo.Declarations.Count) do
      begin
        if (FStackInfo.Declarations[ii] is TLapeParameterVar) then
          Break;
        Inc(ii);
      end;
      if (ii >= FStackInfo.Declarations.Count) then
        LapeException(lpeImpossible);

      Params[i].Default := FStackInfo.Declarations[ii] as TLapeVar;
      NewMethod.addParam(Params[i]);
      Inc(ii);
    end;

    __LapeVar(AVar).FVarType := addManagedType(NewMethod);
  end;

begin
  Result := nil;
  Pos := Tokenizer.DocPos;
  if (FuncHeader = nil) or (FuncName = '') then
    LapeException(lpeBlockExpected, Tokenizer.DocPos);

  isNext([tk_kw_Forward, tk_kw_Overload, tk_kw_Override]);

  OldDeclaration := getDeclarationNoWith(FuncName, FStackInfo.Owner);
  if isExternal then
    Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(FuncHeader.NewGlobalVar(nil), FStackInfo.Owner)), FStackInfo, Self, @Pos)
  else
    Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(FuncHeader.NewGlobalVar(EndJump), FStackInfo.Owner)), FStackInfo, Self, @Pos);

  try
    if (Tokenizer.Tok = tk_kw_Overload) then
    begin
      Expect(tk_sym_SemiColon, True, False);

      if (OldDeclaration = nil) or ((OldDeclaration is TLapeGlobalVar) and (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_Method)) then
        with TLapeType_OverloadedMethod(addLocalDecl(TLapeType_OverloadedMethod.Create(Self, nil, '', @Pos), FStackInfo.Owner)) do
        begin
          if (OldDeclaration <> nil) then
            addMethod(OldDeclaration as TLapeGlobalVar);
          OldDeclaration := addLocalDecl(NewGlobalVar(FuncName, @_DocPos), FStackInfo.Owner);
        end
      else if (not (OldDeclaration is TLapeGlobalVar)) or (not (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_OverloadedMethod)) or (TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).getMethod(FuncHeader) <> nil) then
        LapeException(lpeCannotOverload, Tokenizer.DocPos);

      try
        TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).addMethod(Result.Method);
      except on E: lpException do
        LapeException(E.Message, Tokenizer.DocPos);
      end;

      isNext([tk_kw_Forward]);
    end
    else if (Tokenizer.Tok = tk_kw_Override) then
    begin
      Expect(tk_sym_SemiColon, True, False);

      if (OldDeclaration <> nil) and (OldDeclaration is TLapeGlobalVar) and (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_OverloadedMethod) then
        OldDeclaration := TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).getMethod(FuncHeader);
      if (OldDeclaration = nil) or (not (OldDeclaration is TLapeGlobalVar)) or (not TLapeGlobalVar(OldDeclaration).isConstant) or (not (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_Method)) then
        LapeException(lpeUnknownParent, Tokenizer.DocPos);
      if (not TLapeType_Method(TLapeGlobalVar(OldDeclaration).VarType).EqualParams(FuncHeader, False)) then
        LapeException(lpeNoForwardMatch, Tokenizer.DocPos);
      if hasDeclaration('inherited', FStackInfo, True) then
        LapeExceptionFmt(lpeDuplicateDeclaration, ['inherited'], Tokenizer.DocPos);

      if (TLapeType_Method(TLapeGlobalVar(OldDeclaration).VarType).BaseType = ltScriptMethod) then
        swapMethodTree(TLapeGlobalVar(OldDeclaration), Result.Method);

      Result.Method.Name := 'inherited';
      TLapeType_Method(Result.Method.VarType).setImported(Result.Method, TLapeType_Method(TLapeGlobalVar(OldDeclaration).VarType).BaseType = ltImportedMethod);
      Move(TLapeGlobalVar(OldDeclaration).Ptr^, Result.Method.Ptr^, FuncHeader.Size);
      setMethodDefaults(Result.Method, Result.Method.VarType as TLapeType_Method);
      addLocalDecl(Result.Method, FStackInfo);

      __LapeTree_Method(Result).FMethod := TLapeGlobalVar(OldDeclaration);
      TLapeType_Method(Result.Method.VarType).setImported(Result.Method, isExternal);
    end
    else
    begin
      OldDeclaration := getDeclarationNoWith(FuncName, FStackInfo.Owner, True);
      if (OldDeclaration <> nil) and (OldDeclaration is TLapeGlobalVar) and (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_OverloadedMethod) then
      begin
        OldDeclaration := TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).getMethod(FuncHeader);
        if (OldDeclaration = nil) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [FuncName], Tokenizer.DocPos)
      end;
      if (OldDeclaration <> nil) then
        if (FuncForwards <> nil) and FuncForwards.ExistsItem(OldDeclaration as TLapeGlobalVar) then
        begin
          if (not TLapeGlobalVar(OldDeclaration).VarType.Equals(FuncHeader)) then
            LapeException(lpeNoForwardMatch, Tokenizer.DocPos);
          Result.FreeStackInfo := False;
          Result.Free();

          Result := TLapeTree_Method.Create(TLapeGlobalVar(OldDeclaration), FStackInfo, Self, @Pos);
        end
        else
          LapeExceptionFmt(lpeDuplicateDeclaration, [FuncName], Tokenizer.DocPos);

      Result.Method.Name := FuncName;
    end;

    if (Tokenizer.Tok = tk_kw_Forward) then
    begin
      Expect(tk_sym_SemiColon, True, False);
      if (FuncForwards = nil) then
        LapeException(lpeBlockExpected, Tokenizer.DocPos)
      else if FuncForwards.ExistsItem(Result.Method) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [FuncName], Tokenizer.DocPos)
      else
        FuncForwards.add(Result.Method);

      Result.FreeStackInfo := False;
      FreeAndNil(Result);
      Exit;
    end;

    Result.Method.isConstant := True;
    if isExternal then
      Exit;

    if (FuncForwards <> nil) and (OldDeclaration is TLapeGlobalVar) then
      FuncForwards.DeleteItem(TLapeGlobalVar(OldDeclaration));

    Result.Statements := ParseBlockList();
    FTreeMethodMap[IntToStr(PtrUInt(Result.Method))] := Result;

    if (Result.Statements = nil) or (Result.Statements.Statements.Count < 1) or (not (Result.Statements.Statements[Result.Statements.Statements.Count - 1] is TLapeTree_StatementList)) then
      Expect(tk_kw_Begin, False, False)
    else
      Expect(tk_sym_SemiColon, False, False);
  except
    Result.FreeStackInfo := False;
    FreeAndNil(Result);
    raise;
  end;
end;

function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString): TLapeTree_Method;
var
  i: Integer;
begin
  Result := nil;
  IncStackInfo();

  try
    if (FuncHeader <> nil) then
    begin
      for i := 0 to FuncHeader.Params.Count - 1 do
        FStackInfo.addVar(FuncHeader.Params[i].ParType, FuncHeader.Params[i].VarType, 'Param'+IntToStr(i));
      if (FuncHeader.Res <> nil) then
        FStackInfo.addVar(lptOut, FuncHeader.Res, 'Result');
    end;
    Result := ParseMethod(FuncForwards, FuncHeader, FuncName, False);
  finally
    DecStackInfo(True, False, (Result = nil));
  end;
end;

function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method;
var
  FuncHeader: TLapeType_Method;
  FuncName: lpString;
begin
  Result := nil;
  IncStackInfo();

  try
    FuncHeader := ParseMethodHeader(FuncName, not isExternal);
    Expect(tk_sym_SemiColon, True, False);
    Result := ParseMethod(FuncForwards, FuncHeader, FuncName, isExternal);
  finally
    DecStackInfo(True, False, (Result = nil));
  end;
end;

function TLapeCompiler.ParseType(TypeForwards: TLapeTypeForwards): TLapeType;
  procedure ParseArray;
  var
    TypeExpr: TLapeTree_Base;
    Range: TLapeRange;
    DocPos: TDocPos;
  begin
    //Expect(tk_kw_Array, True, False);
    DocPos := Tokenizer.DocPos;

    Expect([tk_sym_BracketOpen, tk_kw_Of], True, False);
    if (Tokenizer.Tok = tk_sym_BracketOpen) then
    begin
      TypeExpr := ParseTypeExpression();
      try
        Range := EnsureConstantRange(TypeExpr);
        Expect(tk_sym_BracketClose, False, False);
        Expect(tk_kw_Of, True, False);
        Result := addManagedType(TLapeType_StaticArray.Create(Range, ParseType(nil), Self, '', @DocPos));
      finally
        if (TypeExpr <> nil) then
          TypeExpr.Free();
      end;
    end
    else
      Result := addManagedType(TLapeType_DynArray.Create(ParseType(nil), Self, '', @DocPos));
  end;

  procedure ParseRecord(IsPacked: Boolean = False);
  var
    i: Integer;
    Rec: TLapeType_Record absolute Result;
    FieldType: TLapeType;
    Identifiers: TStringArray;
  begin
    //Expect([tk_kw_Record, tk_kw_Union], True, False);
    if (Tokenizer.Tok = tk_kw_Record) then
      Rec := TLapeType_Record.Create(Self, nil, '', getPDocPos())
    else
      Rec := TLapeType_Union.Create(Self, nil, '', getPDocPos());

    repeat
      Identifiers := ParseIdentifierList();
      Expect(tk_sym_Colon, False, False);
      FieldType := ParseType(nil);
      Expect(tk_sym_SemiColon, True, False);
      for i := 0 to High(Identifiers) do
        if IsPacked then
          Rec.addField(FieldType, Identifiers[i], 1)
        else
          Rec.addField(FieldType, Identifiers[i], Options_PackRecords);

      if isNext([tk_kw_End]) then
        Break;
    until False;

    Result := addManagedType(Rec);
  end;

  procedure ParseSet;
  var
    SetType: TLapeType;
  begin
    //Expect(tk_kw_Set, True, False);
    Expect(tk_kw_Of, True, False);
    SetType := ParseType(nil);
    if (not (SetType is TLapeType_SubRange)) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);

    try
      Result := addManagedType(TLapeType_Set.Create(TLapeType_SubRange(SetType), Self, '', GetPDocPos));
    except on E: lpException do
      LapeException(E.Message, Tokenizer.DocPos);
    end;
  end;

  procedure ParsePointer;
  var
    PointerType: TLapeType;
    DocPos: TDocPos;
  begin
    //Expect(tk_sym_Caret, True, False);
    DocPos := Tokenizer.DocPos;

    Expect(tk_Identifier, True, False);
    PointerType := TLapeType(getDeclarationNoWith(Tokenizer.TokString));
    if ((PointerType = nil) and (TypeForwards = nil)) or ((PointerType <> nil) and (not (PointerType is TLapeType))) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    if (PointerType <> nil) then
      Result := addManagedType(TLapeType_Pointer.Create(Self, PointerType, '', @DocPos))
    else
    begin
      Result := TLapeType_Pointer.Create(Self, PointerType, '', @DocPos);
      TypeForwards.add(Tokenizer.TokString, Result);
    end;
  end;

  procedure ParseEnum;
  var
    Enum: TLapeType_Enum absolute Result;
    Name: lpString;
    Member: TLapeTree_ExprBase;
    Default: TLapeGlobalVar;
    StackOwner: TLapeStackInfo;
  begin
    //Expect(tk_sym_ParenthesisOpen, True, False);
    Enum := TLapeType_Enum.Create(Self, nil, '', getPDocPos());
    if (FStackInfo = nil) then
      StackOwner := nil
    else
      StackOwner := FStackInfo.Owner;

    repeat
      Expect(tk_Identifier, True, False);
      Name := Tokenizer.TokString;
      if hasDeclaration(Name, StackOwner, True) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [Name], Tokenizer.DocPos);

      Expect([tk_sym_Comma, tk_sym_ParenthesisClose, tk_sym_Equals], True, False);
      if (Tokenizer.Tok = tk_cmp_Equal) then
      try
        Member := ParseExpression([tk_sym_Comma, tk_sym_ParenthesisClose], True, False);
        try
          if (Member <> nil) then
            Default := Member.Evaluate()
          else
            Default := nil;

          if (Default = nil) or (Default.VarType = nil) or (Default.VarType.BaseIntType = ltUnknown) or (not Default.isConstant) then
            LapeException(lpeExpressionExpected, Tokenizer.DocPos);
          TLapeGlobalVar(addLocalDecl(Enum.NewGlobalVar(Enum.addMember(Default.AsInteger, Name), Name), StackOwner)).isConstant := True;
        finally
          Member.Free();
        end;
      except on E: lpException do
        LapeException(E.Message, Tokenizer.DocPos);
      end
      else
        TLapeGlobalVar(addLocalDecl(Enum.NewGlobalVar(Enum.addMember(Name), Name), StackOwner)).isConstant := True;
    until (Tokenizer.Tok in [tk_NULL, tk_sym_ParenthesisClose]);
    Result := addManagedType(Enum);
  end;

  procedure ParseMethodType;
  var
    BaseType: ELapeBaseType;
    Name: lpString;
  begin
    //Expect([tk_kw_Function, tk_kw_Procedure, tk_kw_External, tk_kw_Export, tk_kw_Private], True, False);
    BaseType := ltPointer;
    if (Tokenizer.Tok in [tk_kw_External, {tk_kw_Export,} tk_kw_Private]) then
    begin
      if (Tokenizer.Tok = tk_kw_External) then
        BaseType := ltImportedMethod
      //else if (Tokenizer.Tok = tk_kw_Export) then
      else if (Tokenizer.Tok = tk_kw_Private) then
        BaseType := ltScriptMethod;

      Expect([tk_kw_Function, tk_kw_Procedure], True, False);
    end;

    Result := ParseMethodHeader(Name, False);
    if (Name <> '') or (Result = nil) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    Result := Result.CreateCopy();
    __LapeType(Result).FBaseType := BaseType;
    Result := addManagedType(Result);
  end;

  procedure ParseDef;
  var
    TypeExpr: TlapeTree_Base;
    Range: TLapeRange;
    RangeType: TLapeType;
  begin
    TypeExpr := ParseTypeExpression([tk_sym_Equals, tk_op_Assign, tk_sym_ParenthesisClose], False);
    try
      if (TypeExpr <> nil) and (TypeExpr is TLapeTree_Range) then
      begin
        Range := EnsureConstantRange(TypeExpr, RangeType);
        Result := addManagedType(TLapeType_SubRange.Create(Range, Self, RangeType, '', getPDocPos()))
      end
      else if (TypeExpr <> nil) and (TypeExpr is TLapeTree_Operator) and (TLapeTree_Operator(TypeExpr).OperatorType = op_Index) then
        with TLapeTree_Operator(TypeExpr) do
        begin
          Range.Hi := -1;
          try
            if (Right <> nil) and (Right is TLapeTree_ExprBase) then
              if TLapeTree_ExprBase(Right).isConstant() then
                Range.Hi := TLapeTree_ExprBase(Right).Evaluate().AsInteger
              else
                LapeException(lpeConstantExpected, Right.DocPos);
          finally
            if (Range.Hi < 0) or (Range.Hi > High(UInt8)) then
              LapeException(lpeInvalidRange, [Right, Self]);
          end;

          if (Left <> nil) and (Left is TLapeTree_VarType) and
            (TLapeTree_VarType(Left).VarType <> nil) and
            (TLapeTree_VarType(Left).VarType.BaseType = ltAnsiString)
          then
            Result := addManagedType(TLapeType_ShortString.Create(Self, Range.Hi, '', @Left._DocPos))
          else
            LapeException(lpeOutOfTypeRange, Tokenizer.DocPos);
        end
      else if (TypeExpr <> nil) and (TypeExpr is TLapeTree_VarType) and (TLapeTree_VarType(TypeExpr).VarType <> nil) then
        Result := TLapeTree_VarType(TypeExpr).VarType
      else
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
    finally
      if (TypeExpr <> nil) then
      begin
        Tokenizer.tempRollBack();
        TypeExpr.Free();
      end;
    end;
  end;

begin
  Result := nil;
  try

    case Next() of
      tk_kw_Array: ParseArray();
      tk_kw_Record, tk_kw_Union: ParseRecord();
      tk_kw_Set: ParseSet();
      tk_kw_Packed:
        begin
          Expect(tk_kw_Record, True, False);
          ParseRecord(True);
        end;
      tk_sym_Caret: ParsePointer();
      tk_sym_ParenthesisOpen: ParseEnum();
      tk_kw_Function, tk_kw_Procedure,
      tk_kw_External, {tk_kw_Export,} tk_kw_Private: ParseMethodType();
      else ParseDef();
    end;

  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

type
  __LapeType_Pointer = class(TLapeType_Pointer);
procedure TLapeCompiler.ParseTypeBlock;
var
  TypeForwards: TLapeTypeForwards;
  Typ: TLapeType;
  Name: lpString;
begin
  TypeForwards := TLapeTypeForwards.Create(nil, {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF}, dupIgnore);
  try
    //Expect(tk_kw_Type, True, False);
    repeat
      Expect(tk_Identifier, True, False);
      Name := Tokenizer.TokString;
      if hasDeclaration(Name, True) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [Name], Tokenizer.DocPos);
      Expect(tk_sym_Equals, True, False);

      Typ := ParseType(TypeForwards).CreateCopy();
      Typ.Name := Name;
      addLocalDecl(Typ);

      Expect(tk_sym_SemiColon, True, False);
    until (Peek() <> tk_Identifier);

    while (TypeForwards.Count > 0) do
      with __LapeType_Pointer(TypeForwards.ItemsI[0]) do
      begin
        FPType := TLapeType(getDeclarationNoWith(TypeForwards.Key[0]));
        if (PType = nil) then
          LapeExceptionFmt(lpeInvalidForward, [TypeForwards.Key[0]], DocPos);
        TypeForwards.Delete(0);
      end;
  finally
    while (TypeForwards.Count > 0) do
      TypeForwards.Delete(0).Free();
    TypeForwards.Free();
  end;
end;

function TLapeCompiler.ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList;
var
  i: Integer;
  isConst: Boolean;
  Identifiers: TStringArray;
  VarType: TLapeType;
  Default: TLapeTree_ExprBase;
  DefVal: TLapeVar;
  DefConstVal: TLapeGlobalVar;
  VarDecl: TLapeVarDecl;
begin
  Result := TLapeTree_VarList.Create(Self, getPDocPos());
  try

    //Expect([tk_kw_Const, tk_kw_Var], True, False);
    isConst := (Tokenizer.Tok = tk_kw_Const);
    repeat
      VarType := nil;
      Default := nil;
      DefConstVal := nil;

      Identifiers := ParseIdentifierList();
      Expect([tk_sym_Colon, tk_op_Assign, tk_sym_Equals], False, False);

      if (Tokenizer.Tok = tk_sym_Colon) then
      begin
        VarType := ParseType(nil);
        if isConst then
          Expect([tk_op_Assign, tk_sym_Equals], True, False)
        else
          Expect([tk_op_Assign, tk_sym_Equals] + ValidEnd, True, False);
      end;

      if (Tokenizer.Tok = tk_sym_Equals) then
      begin
        Default := setExpectedType(ParseExpression([], True, False), VarType) as TLapeTree_ExprBase;
        if (Default <> nil) and (not Default.isConstant()) then
          LapeException(lpeConstantExpected, Default.DocPos);

        try
          Expect(ValidEnd, False, False);
          DefConstVal := Default.Evaluate();
        finally
          if (Default <> nil) then
            FreeAndNil(Default);
        end;
      end
      else if (Tokenizer.Tok = tk_op_Assign) then
      begin
        if (Length(Identifiers) <> 1) then
          LapeException(lpeDefaultToMoreThanOne, Tokenizer.DocPos);

        Default := ParseExpression();
        Expect(ValidEnd, False, False);
      end;

      if (VarType = nil) then
        if (DefConstVal <> nil) then
          VarType := DefConstVal.VarType
        else if (Default <> nil) then
          VarType := Default.resType()
        else
          LapeException(lpeCannotAssign, Tokenizer.DocPos);
      if (VarType = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);

      for i := 0 to High(Identifiers) do
      begin
        if hasDeclaration(Identifiers[i], True) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], Tokenizer.DocPos);

        if isConst then
          DefVal := TLapeVar(addLocalDecl(VarType.NewGlobalVarP(nil, Identifiers[i])))
        else
          DefVal := addLocalVar(VarType, Identifiers[i]);

        if (DefConstVal <> nil) then
          if (not (DefVal is TLapeGlobalVar)) then
          begin
            VarDecl.VarDecl := DefVal;
            VarDecl.Default := TLapeTree_GlobalVar.Create(DefConstVal, Self, GetPDocPos());
            Result.addVar(VarDecl);
          end
          else
            DefVal.VarType.EvalConst(op_Assign, TLapeGlobalVar(DefVal), DefConstVal)
        else if (Default <> nil) then
        begin
          VarDecl.VarDecl := DefVal;
          VarDecl.Default := Default;
          Result.addVar(VarDecl);
          Default := nil;
        end;

        if (DefVal is TLapeVar) then
          TLapeVar(DefVal).isConstant := isConst;
      end;
    until OneOnly or (Peek() <> tk_Identifier);

  except
    Result.Free();
    if (Default <> nil) then
      Default.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_ExprBase;
const
  ParenthesisOpen = Pointer(-1);
var
  VarStack: TLapeTree_NodeStack;
  OpStack: TLapeTree_OpStack;
  Precedence: Byte;
  Expr: TLapeTree_ExprBase;
  Method: TLapeTree_Invoke;
  _LastNode: (_None, _Var, _Op);
  InExpr: Integer;
  DoNext: Boolean;

  procedure PopOpNode;
  var
    OpNode: TLapeTree_Operator;
    DocPos: TDocPos;
  begin
    OpNode := OpStack.Pop();
    try
      with OpNode do
      begin
        if (not (OperatorType in UnaryOperators)) then
          Right := VarStack.Pop();
        Left := VarStack.Pop();
      end;
      VarStack.Push(OpNode);
    except
      DocPos := OpNode._DocPos;
      OpNode.Free();
      LapeException(lpeInvalidEvaluation, DocPos);
    end;
  end;

  procedure PopOpStack(op: EOperator = op_Unknown);
  var
    Associative: ShortInt;
  begin
    if (OpStack.Cur >= 0) and (op <> op_Unknown) then
    begin
      case OperatorAssociative[op] of
        assocLeft: Associative := 0;
        assocRight: Associative := 1;
        assocNone: LapeException(lpeImpossible, Tokenizer.DocPos);
      end;
      Precedence := OperatorPrecedence[op];

      while (OpStack.Cur >= 0) and (OpStack.Top <> TLapeTree_Operator(ParenthesisOpen)) do
        if (Precedence >= OperatorPrecedence[OpStack.Top.OperatorType] + Associative) then
          PopOpNode()
        else
          Break;
    end;
  end;

  function PushVarStack(Item: TLapeTree_ExprBase): Integer;
  begin
    try
      if (_LastNode = _Var) then
        LapeException(lpeOperatorExpected, Tokenizer.DocPos)
      else
        _LastNode := _Var;
      Result := VarStack.Push(Item);
    except
      Item.Free();
      raise;
    end;
  end;

  function PushOpStack(Item: TLapeTree_Operator): Integer;
  begin
    try
      if (Item = TLapeTree_Operator(ParenthesisOpen)) or (Item.OperatorType = op_Assign) then
        _LastNode := _None
      else if (_LastNode <> _Var) and (not (Item.OperatorType in UnaryOperators)) then
        LapeException(lpeExpressionExpected, Tokenizer.DocPos)
      else if (Item.OperatorType = op_Deref) then
        _LastNode := _Var
      else
        _LastNode := _Op;
      Result := OpStack.Push(Item);
    except
      if (Item <> TLapeTree_Operator(ParenthesisOpen)) then
        Item.Free();
      raise;
    end;
  end;

  function getString: lpString;
  begin
    Result := Tokenizer.TokString;
    if (Length(Result) > 1) then
    begin
      Delete(Result, 1, 1);
      Delete(Result, Length(Result), 1);
    end;
  end;

  procedure ParseAndPushString(ForceString: Boolean = False);
  var
    Str: lpString;
    DocPos: TDocPos;
    Token: EParserToken;
  begin
    DocPos := Tokenizer.DocPos;
    case Tokenizer.Tok of
      tk_typ_String: Str := getString();
      tk_typ_Char: Str := Tokenizer.TokChar;
      else LapeException(lpeImpossible);
    end;
    while isNext([tk_typ_String, tk_typ_Char], Token) do
    begin
      case Token of
        tk_typ_String:
          if (Tokenizer.LastTok = tk_typ_String) then
            Str := Str + #39 + getString()
          else
            Str := Str + getString();
        tk_typ_Char: Str := Str + Tokenizer.TokChar;
        else LapeException(lpeImpossible);
      end;
      ForceString := True;
    end;

    if (Length(Str) = 1) and (not ForceString) then
      PushVarStack(TLapeTree_Char.Create(WideChar(Str[1]), Self, @DocPos))
    else
      PushVarStack(TLapeTree_String.Create(Str, Self, @DocPos));
  end;

  procedure ParseAndPushArray;
  var
    OpenArr: TLapeTree_OpenArray;
  begin
    OpenArr := TLapeTree_OpenArray.Create(Self, getPDocPos());
    try
      if (Next() <> tk_sym_BracketClose) then
      begin
        OpenArr.addValue(EnsureTypeExpression(ParseTypeExpression([tk_sym_BracketClose, tk_sym_Comma], False)));
        while True do
          case Tokenizer.Tok of
            tk_sym_BracketClose: Break;
            tk_sym_Comma: OpenArr.addValue(EnsureTypeExpression(ParseTypeExpression([tk_sym_BracketClose, tk_sym_Comma])));
            else Expect(tk_sym_BracketClose, False, False);
          end;
      end;
    except
      OpenArr.Free();
      raise;
    end;
    PushVarStack(OpenArr);
  end;

  procedure ParseOperator(op: EOperator = op_Unknown);
  begin
    if (op = op_Unknown) and (_LastNode = _Var) then
      if (Tokenizer.Tok = tk_sym_ParenthesisOpen) then
      begin
        PushOpStack(TLapeTree_Operator(ParenthesisOpen));
        Exit;
      end;
    if (op = op_Unknown) then
      op := ParserTokenToOperator(Tokenizer.Tok);

    if (op = op_Index) and (_LastNode <> _Var) then
    begin
      ParseAndPushArray();
      Exit;
    end;

    //Unary minus and double negation
    if (op in [op_Minus, op_Plus]) then
      if (_LastNode = _None) or
           ((Tokenizer.LastTok in ParserToken_Operators) and
           (OperatorPrecedence[ParserTokenToOperator(Tokenizer.LastTok)] <> OperatorPrecedence[op]))
      then
        if (op = op_Minus) then
          op := op_UnaryMinus
        else
          Exit
      else if (op = op_Minus) and (Tokenizer.LastTok in [tk_op_Plus, tk_op_Minus]) then
      begin
        case opStack.Top.OperatorType of
          op_Plus: opStack.Top.OperatorType := op_Minus;
          op_Minus: opStack.Top.OperatorType := op_Plus;
          op_UnaryPlus: opStack.Top.OperatorType := op_UnaryMinus;
          op_UnaryMinus: opStack.Top.OperatorType := op_UnaryPlus;
        end;
        Exit;
      end;

    if (op = op_UnaryPlus) then
      Exit;

    if (_LastNode <> _Var) and (not (op in UnaryOperators)) then
      LapeException(lpeExpressionExpected, Tokenizer.DocPos);

    PopOpStack(op);
    PushOpStack(TLapeTree_Operator.Create(op, Self, getPDocPos()));

    if (op = op_Dot) then
    begin
      if (Next() <> tk_Identifier) then
        LapeExceptionFmt(lpeExpected, [LapeTokenToString(tk_Identifier)]);
      PushVarStack(TLapeTree_Field.Create(Tokenizer.TokString, Self, getPDocPos()));
    end
    else if (op = op_Index) then
    begin
      PushVarStack(ParseExpression());
      if (Tokenizer.Tok = tk_sym_Comma) then
        ParseOperator(op_Index)
      else
        Expect(tk_sym_BracketClose, False, False);
    end;
  end;

begin
  Result := nil;
  Method := nil;
  VarStack := TLapeTree_NodeStack.Create(8);
  OpStack := TLapeTree_OpStack.Create(16);
  _LastNode := _None;
  InExpr := 0;
  DoNext := FirstNext;

  try
    while True do
    begin
      if ((DoNext and (Next() in ReturnOn)) or ((not DoNext) and (Tokenizer.Tok in ReturnOn))) and (InExpr <= 0) then
        Break;
      DoNext := True;

      case Tokenizer.Tok of
        tk_typ_Integer: PushVarStack(TLapeTree_Integer.Create(Tokenizer.TokString, Self, getPDocPos()));
        tk_typ_Integer_Hex: PushVarStack(TLapeTree_Integer.Create(IntToStr(Tokenizer.TokInt64), Self, getPDocPos()));
        tk_typ_Integer_Bin: PushVarStack(TLapeTree_Integer.Create(IntToStr(Tokenizer.TokInt64), Self, getPDocPos()));
        tk_typ_Float: PushVarStack(TLapeTree_Float.Create(Tokenizer.TokString, Self, getPDocPos()));
        tk_typ_String: ParseAndPushString();
        tk_typ_Char:
          begin
            //if (Peek() in [tk_typ_String, tk_typ_Char]) then
              ParseAndPushString()
            //else
            //  PushVarStack(TLapeTree_Char.Create(Tokenizer.TokChar, Self, getPDocPos()));
          end;

        tk_Identifier:
          begin
            Expr := getExpression(Tokenizer.TokString, getPDocPos());
            if (Expr <> nil) then
              PushVarStack(Expr)
            else if (FInternalMethodMap[Tokenizer.TokString] <> nil) then
            begin
              Method := FInternalMethodMap[Tokenizer.TokString].Create(Self, getPDocPos());
              DoNext := False;
              if (Next() = tk_sym_ParenthesisOpen) then
                _LastNode := _Var
              else
              begin
                VarStack.Push(Method);
                Method := nil;
              end;
            end
            else
              LapeExceptionFmt(lpeUnknownDeclaration, [Tokenizer.TokString], Tokenizer.DocPos);
          end;

        tk_sym_ParenthesisOpen:
          begin
            if (_LastNode = _Var) then
            begin
              PopOpStack(op_Invoke);
              if (Method = nil) then
                Method := TLapeTree_Invoke.Create(FoldConstants(VarStack.Pop()) as TLapeTree_ExprBase, Self, getPDocPos());
              if (Next() <> tk_sym_ParenthesisClose) then
              begin
                Method.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], False)));
                while True do
                  case Tokenizer.Tok of
                    tk_sym_ParenthesisClose: Break;
                    tk_sym_Comma: Method.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma])));
                    else
                      LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
                  end;
              end;
              VarStack.Push(Method);
              Method := nil;
            end
            else
            begin
              PushOpStack(TLapeTree_Operator(ParenthesisOpen));
              Inc(InExpr);
            end;
          end;
        tk_sym_ParenthesisClose:
          begin
            while (OpStack.Cur >= 0) and (OpStack.Top <> TLapeTree_Operator(ParenthesisOpen)) do
              PopOpNode();
            if (OpStack.Cur < 0) or (OpStack.Pop() <> TLapeTree_Operator(ParenthesisOpen)) then
              LapeException(lpeLostClosingParenthesis, Tokenizer.DocPos);
            Dec(InExpr);
          end;

        ParserToken_FirstOperator..ParserToken_LastOperator: ParseOperator();
        else
          Break;
      end;
    end;

    while (OpStack.Cur >= 0) do
    begin
      if (OpStack.Top = TLapeTree_Operator(ParenthesisOpen)) then
        LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);

      PopOpNode();
    end;

    if (VarStack.Cur <> 0) then
      if (VarStack.Cur < 0) and (Tokenizer.Tok in ReturnOn) then
        Exit(nil)
      else
        LapeException(lpeInvalidEvaluation, Tokenizer.DocPos);

    Result := VarStack.Pop();
    if DoFold then
      Result := TLapeTree_ExprBase(FoldConstants(Result));
    PrintTree(Result);
  finally
    if (Method <> nil) then
      Method.Free();
    while (VarStack.Cur >= 0) do
      VarStack.Pop().Free();
    while (OpStack.Cur >= 0) do
      if (OpStack.Top = TLapeTree_Operator(ParenthesisOpen)) then
        OpStack.Pop()
      else
        OpStack.Pop().Free();

    VarStack.Free();
    OpStack.Free();
  end;
end;

function TLapeCompiler.ParseTypeExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_Base;
var
  ExprLo: TLapeTree_ExprBase;
  TypeHi: TLapeType;
begin
  Result := nil;
  TypeHi := nil;

  ExprLo := ParseExpression(ReturnOn, FirstNext, DoFold);
  if (Tokenizer.Tok <> tk_sym_DotDot) then
    Result := ExprLo
  else
  try
    Result := TLapeTree_Range.Create(Self, getPDocPos());
    TLapeTree_Range(Result).Lo := ExprLo;
    TLapeTree_Range(Result).Hi := ParseExpression(ReturnOn, True, DoFold);

    if (ExprLo <> nil) and (TLapeTree_Range(Result).Hi <> nil) then
      TypeHi := TLapeTree_Range(Result).Hi.resType();
    if (TypeHi = nil) or (not TypeHi.CompatibleWith(ExprLo.resType())) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseStatement: TLapeTree_Base;
var
  Token: EParserToken;
begin
  Result := nil;

  if isNext([tk_NULL, tk_Identifier, tk_kw_Begin, tk_kw_Case {$IFNDEF Lape_ForceBlock}, tk_kw_Const, tk_kw_Var {$ENDIF}, tk_kw_For, tk_kw_If, tk_kw_Repeat, tk_kw_While, tk_kw_With, tk_kw_Try, tk_sym_SemiColon, tk_kw_Else], Token) then
    case Token of
      tk_Identifier: Result := ParseExpression([], False);
      tk_kw_Begin: Result := ParseBeginEnd();
      tk_kw_Case: Result := ParseCase();
      tk_kw_Const, tk_kw_Var: Result := ParseVarBlock(True);
      tk_kw_For: Result := ParseFor();
      tk_kw_If: Result := ParseIf();
      tk_kw_Repeat: Result := ParseRepeat();
      tk_kw_While: Result := ParseWhile();
      tk_kw_With: Result := ParseWith();
      tk_kw_Try: Result := ParseTry();
    end
  else if (not (Token in [tk_kw_End, tk_kw_Finally, tk_kw_Except, tk_kw_Until])) then
  begin
    Result := ParseExpression();
    try
      Expect([tk_sym_SemiColon, tk_kw_Else], False, False);
    except
      Result.Free();
      raise;
    end;
  end;
end;

function TLapeCompiler.ParseStatementList: TLapeTree_StatementList;
var
  Statement: TLapeTree_Base;
begin
  Result := TLapeTree_StatementList.Create(Self, getPDocPos());
  try

    repeat
      Statement := ParseStatement();
      if (Statement <> nil) then
      begin
        Result.addStatement(Statement);
        Expect(tk_sym_SemiColon, False, False);
      end;
    until (Statement = nil);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseBeginEnd(AllowDot: Boolean = False): TLapeTree_StatementList;
begin
  //Expect(tk_kw_Begin, True, False);
  Result := ParseStatementList();

  try

    Expect(tk_kw_End, True, False);
    if AllowDot then
      Expect([tk_sym_SemiColon, tk_kw_Else, tk_sym_Dot], True, False)
    else
      Expect([tk_sym_SemiColon, tk_kw_Else], True, False);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseCase: TLapeTree_Case;
var
  Expr: TLapeTree_Base;
  Field: TLapeTree_MultiIf;
begin
  //Expect(tk_kw_Case, True, False);
  Result := TLapeTree_Case.Create(Self, getPDocPos());
  Expr := nil;
  Field := nil;
  try
    Result.Condition := ParseExpression();
    Expect(tk_kw_Of, False, False);

    while (not (Next() in [tk_Null, tk_kw_Else, tk_kw_End])) do
    begin
      Expr := ParseTypeExpression([], False);
      Field := TLapeTree_MultiIf.Create(nil, Self, @Expr._DocPos);
      repeat
        Field.addValue(Expr);
        Expr := nil;
        Expect([tk_sym_Comma, tk_sym_Colon], False, False);
        if (Tokenizer.Tok = tk_sym_Colon) then
          Break
        else
          Expr := ParseTypeExpression();
      until False;
      Field.Body := ParseStatement();
      Result.addField(Field);
      Field := nil;
    end;

    Expect([tk_kw_Else, tk_kw_End], False, False);
    if (Tokenizer.Tok = tk_kw_Else) then
    begin
      Result.ElseBody := ParseStatement();
      Expect(tk_kw_End, True, False);
    end;

    Expect([tk_sym_SemiColon, tk_kw_Else], True, False);
  except
    if (Expr <> nil) then
      Expr.Free();
    if (Field <> nil) then
      Field.Free();
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseFor: TLapeTree_For;
begin
  //Expect(tk_kw_For, True, False);
  Result := TLapeTree_For.Create(Self, getPDocPos());
  try

    {$IFNDEF Lape_ForceBlock}
    if (Peek() = tk_kw_Var) then
      with ParseVarBlock(True, [tk_kw_To]) do
      try
        if (Vars.Count <> 1) then
          LapeException(lpeVariableExpected, DocPos);

        if (Vars[0].Default <> nil) then
        begin
          Result.Counter := TLapeTree_Operator.Create(op_Assign, Compiler, @_DocPos);
          with TLapeTree_Operator(Result.Counter) do
          begin
            Left := TLapeTree_ResVar.Create(getResVar(Vars[0].VarDecl), Compiler, @_DocPos);
            Right := Vars[0].Default;
          end;
        end
        else
          Result.Counter := TLapeTree_ResVar.Create(getResVar(Vars[0].VarDecl), Compiler, @_DocPos);
      finally
        Free();
      end
    else
    {$ENDIF}
      Result.Counter := ParseExpression();
    Expect([tk_kw_To, tk_kw_DownTo], False, False);
    if (Tokenizer.Tok = tk_kw_DownTo) then
      Result.WalkDown := True;
    Result.Limit := ParseExpression();
    Expect([tk_kw_Step, tk_kw_Do], False, False);
    if (Tokenizer.Tok = tk_kw_Step) then
    begin
      Result.Step := ParseExpression();
      Expect(tk_kw_Do, False, False);
    end;
    Result.Body := ParseStatement();
    if (Tokenizer.Tok = tk_kw_Else) then
      Result.ElseBody := ParseStatement();

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseIf: TLapeTree_If;
begin
  //Expect(tk_kw_If, True, False);
  Result := TLapeTree_If.Create(Self, getPDocPos());

  try

    Result.Condition := ParseExpression();
    Expect(tk_kw_Then, False, False);
    Result.Body := ParseStatement();
    if (Tokenizer.Tok = tk_kw_Else) then
      Result.ElseBody := ParseStatement();

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseRepeat: TLapeTree_Repeat;
begin
  Expect(tk_kw_Repeat, True, False);
  Result := TLapeTree_Repeat.Create(Self, getPDocPos());

  try

    Result.Body := ParseStatementList();
    Expect(tk_kw_Until, True, False);
    Result.Condition := ParseExpression();
    Expect([tk_sym_SemiColon, tk_kw_Else], False, False);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseTry: TLapeTree_Try;
begin
  //Expect(tk_kw_Try, True, False);
  Result := TLapeTree_Try.Create(Self, getPDocPos());

  try

    Result.Body := ParseStatementList();
    Expect([tk_kw_Except, tk_kw_Finally], True, False);
    if (Tokenizer.Tok = tk_kw_Except) then
    begin
      Result.ExceptBody := ParseStatementList();
      Expect([tk_kw_Finally, tk_kw_End], True, False);
    end;
    if (Tokenizer.Tok = tk_kw_Finally) then
    begin
      Result.FinallyBody := ParseStatementList();
      Expect(tk_kw_End, True, False);
    end;
    Expect([tk_sym_SemiColon, tk_kw_Else], True, False);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseWhile: TLapeTree_While;
begin
  //Expect(tk_kw_While, True, False);
  Result := TLapeTree_While.Create(Self, getPDocPos());

  try

    Result.Condition := ParseExpression();
    Expect(tk_kw_Do, False, False);
    Result.Body := ParseStatement();
    if (Tokenizer.Tok = tk_kw_Else) then
      Result.ElseBody := ParseStatement();

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseWith: TLapeTree_With;
var
  Count: Integer;
begin
  //Expect(tk_kw_With, True, False);
  Result := TLapeTree_With.Create(Self, getPDocPos());
  Count := 0;

  try

    repeat
      if (Tokenizer.Tok in [tk_kw_With, tk_sym_Comma]) then
      begin
        FStackInfo.addWith(Result.addWith(ParseExpression([tk_sym_Comma])));
        Inc(Count);
      end
      else
      begin
        Expect(tk_kw_Do, False, False);
        Break;
      end;
    until False;

    Result.Body := ParseStatement();
    FStackInfo.delWith(Count);

  except
    Result.Free();
    raise;
  end;
end;

constructor TLapeCompiler.Create(
  ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
  AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True);
begin
  inherited Create(AEmitter, ManageEmitter);

  FTokenizer := -1;
  FImporting := nil;
  FDelayedTree := TLapeTree_DelayedStatementList.Create(Self);
  FreeTokenizer := ManageTokenizer;
  FreeTree := True;

  FIncludes := TStringList.Create();
  FIncludes.Duplicates := dupIgnore;
  FIncludes.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  FDefines := TStringList.Create();
  FDefines.Duplicates := dupIgnore;
  FDefines.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  FConditionalStack := TLapeConditionalStack.Create(0);

  FOnHandleDirective := nil;
  FOnFindFile := nil;

  FBaseDefines := TStringList.Create();
  FBaseDefines.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};

  FTreeMethodMap := TLapeTreeMethodMap.Create(nil);
  FInternalMethodMap := TLapeInternalMethodMap.Create(nil);
  FInternalMethodMap['Write'] := TLapeTree_InternalMethod_Write;
  FInternalMethodMap['WriteLn'] := TLapeTree_InternalMethod_WriteLn;
  FInternalMethodMap['IsScriptMethod'] := TLapeTree_InternalMethod_IsScriptMethod;
  FInternalMethodMap['Break'] := TLapeTree_InternalMethod_Break;
  FInternalMethodMap['Continue'] := TLapeTree_InternalMethod_Continue;
  FInternalMethodMap['Exit'] := TLapeTree_InternalMethod_Exit;
  FInternalMethodMap['SizeOf'] := TLapeTree_InternalMethod_SizeOf;
  FInternalMethodMap['Ord'] := TLapeTree_InternalMethod_Ord;
  FInternalMethodMap['Low'] := TLapeTree_InternalMethod_Low;
  FInternalMethodMap['High'] := TLapeTree_InternalMethod_High;
  FInternalMethodMap['Length'] := TLapeTree_InternalMethod_Length;
  FInternalMethodMap['Succ'] := TLapeTree_InternalMethod_Succ;
  FInternalMethodMap['Pred'] := TLapeTree_InternalMethod_Pred;
  FInternalMethodMap['Inc'] := TLapeTree_InternalMethod_Inc;
  FInternalMethodMap['Dec'] := TLapeTree_InternalMethod_Dec;

  addGlobalFunc([getBaseType(ltString)], [lptNormal], [TLapeGlobalVar(nil)], @_LapeWrite, '_write').isConstant := True;
  addGlobalFunc([], [], [], @_LapeWriteLn, '_writeln').isConstant := True;

  addGlobalFunc([getBaseType(ltInt32)],   [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltPointer), @_LapeGetMem, 'GetMem').isConstant := True;
  addGlobalFunc([getBaseType(ltPointer)], [lptNormal], [TLapeGlobalVar(nil)], @_LapeFreeMem, 'FreeMem').isConstant := True;
  addGlobalFunc([getBaseType(ltPointer), getBaseType(ltInt32)], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], @_LapeFreeMemSize, 'FreeMemSize').isConstant := True;
  addGlobalFunc([getBaseType(ltPointer), getBaseType(ltInt32)], [lptVar,    lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], @_LapeReallocMem, 'ReallocMem').isConstant := True;

  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeHigh, '!high').isConstant := True;
  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeLength, '!length').isConstant := True;
  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeAStrLen, '!astrlen').isConstant := True;
  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeWStrLen, '!wstrlen').isConstant := True;
  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeUStrLen, '!ustrlen').isConstant := True;

  setTokenizer(ATokenizer);
  Reset();

  StartImporting();
  InitBaseDefinitions();
end;

destructor TLapeCompiler.Destroy;
begin
  setTokenizer(nil);
  EndImporting();
  if FreeTree and (FDelayedTree <> nil) then
    FreeAndNil(FDelayedTree);
  FreeAndNil(FIncludes);
  FreeAndNil(FDefines);
  FreeAndNil(FBaseDefines);
  FreeAndNil(FConditionalStack);
  FreeAndNil(FTreeMethodMap);
  FreeAndNil(FInternalMethodMap);
  inherited;
end;

function TLapeCompiler.getState: Pointer;
var
  i: Integer;
begin
  New(PCompilerState(Result));
  with PCompilerState(Result)^ do
  begin
    Tokenizer := FTokenizer;

    SetLength(Tokenizers, Length(FTokenizers));
    for i := 0 to High(FTokenizers) do
      if (FTokenizers[i] <> nil) then
        Tokenizers[i] := FTokenizers[i].getState()
      else
        Tokenizers[i] := nil;

    Defines := FDefines.Text;
    Conditionals := FConditionalStack.ExportToArray();
  end;
end;

procedure TLapeCompiler.setState(const State: Pointer; DoFreeState: Boolean = True);
var
  i: Integer;
begin
  with PCompilerState(State)^ do
  begin
    Assert(Length(FTokenizers) >= Length(Tokenizers));
    FTokenizer := Tokenizer;

    for i := 0 to High(Tokenizers) do
      if (Tokenizers[i] <> nil) and (FTokenizers[i] <> nil) then
        FTokenizers[i].setState(Tokenizers[i], False);

    FDefines.Text := Defines;
    FConditionalStack.ImportFromArray(Conditionals);
  end;
  if DoFreeState then
    freeState(State);
end;

procedure TLapeCompiler.freeState(const State: Pointer);
var
  i: Integer;
begin
  with PCompilerState(State)^ do
    for i := 0 to High(Tokenizers) do
      if (Tokenizers[i] <> nil) and (FTokenizers[i] <> nil) then
        FTokenizers[i].freeState(Tokenizers[i]);
  Dispose(PCompilerState(State));
end;

function TLapeCompiler.getTempTokenizerState(const ATokenizer: TLapeTokenizerBase): Pointer;
begin
  if (not hasTokenizer()) then
    SetLength(FTokenizers, 1);

  if (FImporting <> nil) then
    Result := nil
  else
  begin
    New(PTempTokenizerState(Result));
    with PTempTokenizerState(Result)^ do
    begin
      OldStackInfo := FStackInfo;
      OldTokenizerIndex := FTokenizer;
      OldTokenizer := FTokenizers[0];
      OldState := getState();
    end;
  end;

  FTokenizer := 0;
  FTokenizers[0] := ATokenizer;

  while (FStackInfo <> nil) and (FStackInfo.Owner <> nil) do
    FStackInfo := FStackInfo.Owner;
  if (FStackInfo = nil) then
    FStackInfo := EmptyStackInfo;
end;

function TLapeCompiler.getTempTokenizerState(const AStr: lpString; const AFileName: lpString = ''): Pointer;
begin
  Result := getTempTokenizerState(TLapeTokenizerString.Create(AStr, AFileName));
end;

procedure TLapeCompiler.resetTokenizerState(const State: Pointer; DoFreeState: Boolean = True);
begin
  if (State <> nil) then
    with PTempTokenizerState(State)^ do
    begin
      Swap(Pointer(FTokenizers[0]), Pointer(OldTokenizer));
      FTokenizer := OldTokenizerIndex;
      FStackInfo := OldStackInfo;
      setState(OldState, False);
    end;
  if DoFreeState then
    freeTempTokenizerState(State);
end;

procedure TLapeCompiler.freeTempTokenizerState(const State: Pointer);
begin
  if (State = nil) then
  begin
    if (FImporting <> nil) and (FTokenizers[0] <> nil) then
      FreeAndNil(FTokenizers[0]);
    Exit;
  end;

  with PTempTokenizerState(State)^ do
  begin
    if (OldTokenizer <> nil) then
      OldTokenizer.Free();
    freeState(OldState);
  end;
  Dispose(PTempTokenizerState(State));
end;

procedure TLapeCompiler.StartImporting;
begin
  if (FImporting = nil) then
    FImporting := getTempTokenizerState(nil);
end;

procedure TLapeCompiler.EndImporting;
begin
  if (FImporting <> nil) then
  begin
    resetTokenizerState(FImporting);
    FImporting := nil;
  end;
end;

function TLapeCompiler.addDelayedExpression(Node: TLapeTree_Base; AfterCompilation: Boolean = True; IsGlobal: Boolean = False): TLapeTree_Base;
begin
  Result := Node;
  if (Node = nil) or (FDelayedTree = nil) then
    Exit;
  FDelayedTree.addStatement(Node, AfterCompilation, IsGlobal);
end;

function TLapeCompiler.ParseFile: TLapeTree_Base;
begin
  Result := nil;
  Assert(Tokenizer <> nil);

  try
    if (FDefines <> nil) and (FBaseDefines <> nil) then
      FDefines.Assign(FBaseDefines);

    if isNext([tk_kw_Program]) then
    begin
      Expect(tk_Identifier, True, False);
      Expect(tk_sym_SemiColon, True, False);
      Result := ParseBlockList(False);
    end
    else
      Result := ParseBlockList(False);

    CheckAfterCompile();
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.Compile: Boolean;
begin
  Result := False;
  try

    Reset();
    IncStackInfo(True);
    FTree := ParseFile();
    if (FTree = nil) and (FDelayedTree.GlobalCount(False) <= 0) then
      LapeException(lpeExpressionExpected);
    FDelayedTree.Compile(False);
    FTree.Compile();
    FDelayedTree.Compile(True);
    DecStackInfo(False, True, True);
    FEmitter._op(ocNone);
    Result := True;

  except
    Reset();
    raise;
  end;
end;

procedure TLapeCompiler.CheckAfterCompile;
begin
  Assert(Tokenizer <> nil);

  if (FConditionalStack.Cur >= 0) then
    LapeException(lpeConditionalNotClosed, popConditional());
end;

function TLapeCompiler.getDeclaration(Name: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration;
begin
  Result := inherited;
  if (Result = nil) and LocalOnly and (AStackInfo <> nil) and (AStackInfo.Owner = nil) then
    Result := inherited getDeclaration(Name, nil, Localonly);
end;

function TLapeCompiler.getDeclarationNoWith(Name: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration;
begin
  Result := getDeclaration(Name, AStackInfo, LocalOnly);
  if (Result is TLapeWithDeclaration) then
    with TLapeWithDeclaration(Result).WithDeclRec do
      try
        if (WithVar <> nil) and (WithVar^ <> nil) and (WithVar^ is TLapeGlobalVar) and TLapeGlobalVar(WithVar^).isConstant then
          Result := WithVar^ as TLapeGlobalVar
        else
          Result := nil;
      finally
        Free();
      end;
end;

function TLapeCompiler.getDeclarationNoWith(Name: lpString; LocalOnly: Boolean = False): TLapeDeclaration;
begin
  Result := getDeclarationNoWith(Name, FStackInfo, LocalOnly);
end;

function TLapeCompiler.getExpression(AName: lpString; AStackInfo: TLapeStackInfo; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase;
var
  Decl: TLapeDeclaration;
begin
  Result := nil;
  Decl := getDeclaration(AName, AStackInfo, LocalOnly);

  if (Decl <> nil) then
    if (Decl is TLapeWithDeclaration) then
      with TLapeWithDeclaration(Decl) do
      try
        Result := TLapeTree_Operator.Create(op_Dot, Self, Pos);
        TLapeTree_Operator(Result).Left := TLapeTree_WithVar.Create(WithDeclRec, Self, Pos);
        TLapeTree_Operator(Result).Right := TLapeTree_Field.Create(AName, Self, Pos);
        Result := FoldConstants(Result) as TLapeTree_ExprBase;
      finally
        Free();
      end
    else if (Decl is TLapeGlobalVar) then
      Result := TLapeTree_GlobalVar.Create(TLapeGlobalVar(Decl), Self, Pos)
    else if (Decl is TLapeVar) then
      Result := TlapeTree_ResVar.Create(getResVar(TLapeVar(Decl)), Self, Pos)
    else if (Decl is TLapeType) then
      Result := TLapeTree_VarType.Create(TLapeType(Decl), Self, Pos);
end;

function TLapeCompiler.getExpression(AName: lpString; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase;
begin
  Result := getExpression(AName, FStackInfo, Pos, LocalOnly);
end;

function TLapeCompiler.addLocalDecl(Decl: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration;
begin
  if (Decl = nil) then
    Exit(nil);
  if (Decl.Name <> '') and hasDeclaration(Decl.Name, AStackInfo, True) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Decl.Name]);

  Result := Decl;
  if (AStackInfo = nil) or (AStackInfo.Owner = nil) then
    FGlobalDeclarations.addDeclaration(Decl)
  else
    AStackInfo.addDeclaration(addManagedDecl(Decl));
end;

function TLapeCompiler.addLocalVar(AVar: TLapeType; Name: lpString = ''): TLapeVar;
begin
  if (AVar = nil) then
    Exit(nil);
  if (Name <> '') and hasDeclaration(Name, True) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Name]);

  if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
    Result := addGlobalVar(AVar.NewGlobalVarP(), Name)
  else
    Result := addStackVar(AVar, Name);
end;

function TLapeCompiler.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar;
begin
  if (AVar <> nil) then
  begin
    if (AName <> '') then
      AVar.Name := AName;
    if (Length(FGlobalDeclarations.getByName(AVar.Name)) > 0) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [AVar.Name]);
    AVar.isConstant := False;
    FGlobalDeclarations.addDeclaration(AVar);
  end;
  Result := AVar;
end;

function TLapeCompiler.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
var
  OldState: Pointer;
begin
  Typ := AName + ': ' + Typ;
  if (Value <> '') then
    Typ := Typ + ' = ' + Value;
  OldState := getTempTokenizerState(Typ + ';', '!import');

  try
    ParseVarBlock().Free();
    Result := FGlobalDeclarations.Items[FGlobalDeclarations.Items.Count - 1] as TLapeGlobalVar;
    CheckAfterCompile();
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalVar(Typ: TLapeType; AName: lpString; Value: lpString = ''): TLapeGlobalVar;
begin
  if (Typ.Name <> '') then
    Result := addGlobalVar(Typ.Name, Value, AName)
  else
    Result := addGlobalVar(Typ.AsString, Value, AName);
end;

function TLapeCompiler.addGlobalVar(Typ: ELapeBaseType; AName: lpString; Value: lpString = ''): TLapeGlobalVar;
begin
  Result := addGlobalVar(getBaseType(Typ), Value, AName);
end;

function TLapeCompiler.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  with addGlobalVar(Typ, '', AName) do
  begin
    Name := '';
    Result := AddGlobalVar(VarType.NewGlobalVarP(Value), AName);
    Free();
  end;
end;

function TLapeCompiler.addGlobalVar(Typ: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  if (Typ.Name <> '') then
    Result := addGlobalVar(Typ.Name, Value, AName)
  else
    Result := addGlobalVar(Typ.AsString, Value, AName);
end;

function TLapeCompiler.addGlobalVar(Typ: ELapeBaseType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(getBaseType(Typ), Value, AName);
end;

function TLapeCompiler.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Int32(FBaseTypes[ltInt32]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_UInt32(FBaseTypes[ltUInt32]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Int64(FBaseTypes[ltInt64]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_UInt64(FBaseTypes[ltUInt64]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Extended(FBaseTypes[ltExtended]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_EvalBool(FBaseTypes[ltEvalBool]).NewGlobalVar(Ord(Boolean(Value))), AName);
end;

function TLapeCompiler.addGlobalVar(Value: ShortString; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_ShortString(FBaseTypes[ltShortString]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_AnsiString(FBaseTypes[ltAnsiString]).NewGlobalVar(Value), AName);
end;

{$IFNDEF Lape_NoWideString}
function TLapeCompiler.addGlobalVar(Value: WideString; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_WideString(FBaseTypes[ltWideString]).NewGlobalVar(Value), AName);
end;
{$ENDIF}

function TLapeCompiler.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_UnicodeString(FBaseTypes[ltUnicodeString]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: AnsiChar; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_AnsiChar(FBaseTypes[ltAnsiChar]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: WideChar; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_WideChar(FBaseTypes[ltWideChar]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Variant(FBaseTypes[ltVariant]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Pointer(FBaseTypes[ltPointer]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalType(Typ: TLapeType; AName: lpString = ''): TLapeType;
begin
  if (Typ <> nil) then
  begin
    if (AName <> '') then
      Typ.Name := AName;
    if (Length(FGlobalDeclarations.getByName(Typ.Name)) > 0) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Typ.Name]);
    FGlobalDeclarations.addDeclaration(Typ);
  end;
  Result := Typ;
end;

function TLapeCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
var
  OldState: Pointer;
begin
  Result := nil;
  OldState := getTempTokenizerState(AName + ' = ' + Str + ';', '!import');

  try
    ParseTypeBlock();
    Result := FGlobalDeclarations.Items[FGlobalDeclarations.Items.Count - 1] as TLapeType;
    CheckAfterCompile();
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  OldState: Pointer;
begin
  Result := nil;
  OldState := getTempTokenizerState(AHeader + ';', '!import');

  try
    Expect([tk_kw_Function, tk_kw_Procedure]);
    Method := ParseMethod(nil, True);
    CheckAfterCompile();

    try
      if (Method.Method = nil) or (Method.Method.VarType = nil) or
         (Method.Method.VarType.BaseType <> ltImportedMethod)
      then
        LapeException(lpeInvalidEvaluation);

      Result := Method.Method;
      PPointer(Result.Ptr)^ := Value;
    finally
      FreeAndNil(Method);
    end;
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalFunc(AHeader: TLapeType_Method; AName, Body: lpString): TLapeTree_Method;
var
  OldState: Pointer;
begin
  OldState := getTempTokenizerState(Body);
  try
    Result := ParseMethod(nil, AHeader, AName);
    CheckAfterCompile();
    addDelayedExpression(Result, True, True);
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Method(addManagedType(TLapeType_Method.Create(Self, AParams, AParTypes, AParDefaults, ARes))).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalFunc(AParams, AParTypes, AParDefaults, nil, Value, AName);
end;

function TLapeCompiler.addDelayedCode(ACode: lpString; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base;
var
  Index: Integer;
  OldState: Pointer;
begin
  Index := FDelayedTree.Statements.Count;
  OldState := getTempTokenizerState(ACode);

  try
    Result := ParseFile();
    CheckAfterCompile();

    addDelayedExpression(Result);
    FDelayedTree.OverrideDelayGlobal(AfterCompilation, IsGlobal, Index, -1);
  finally
    resetTokenizerState(OldState);
  end;
end;

end.

