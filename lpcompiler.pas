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

  TLapeCompiler = class(TLapeCompilerBase)
  private
    __tmp: TDocPos;
    function getPDocPos: PDocPos; inline;
  protected
    FTokenizers: TLapeTokenizerArray;
    FTokenizer: Integer;
    FInternalMethodMap: TLapeInternalMethodMap;
    FTree: TLapeTree_Base;

    FIncludes: TStringList;
    FBaseDefines: TStringList;
    FDefines: TStringList;
    FConditionalStack: TLapeConditionalStack;

    FOnHandleDirective: TLapeHandleDirective;
    FOnFindFile: TLapeFindFile;

    procedure Reset; override;
    procedure setBaseDefines(Defines: TStringList); virtual;
    function getTokenizer: TLapeTokenizerBase; virtual;
    procedure setTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    function popTokenizer: TLapeTokenizerBase; virtual;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); virtual;
    function popConditional: TDocPos; virtual;

    function EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase; virtual;
    function EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range; overload; virtual;
    function EnsureRange(Node: TLapeTree_Base): TLapeTree_Range; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base): TLapeRange; overload; virtual;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; virtual;
    function InIgnore: Boolean; virtual;
    function Next: EParserToken; virtual;
    function Peek: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    function ParseIdentifierList: TStringArray; virtual;
    function ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList; virtual;
    function ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method;
    function ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method;
    function ParseType(TypeForwards: TLapeTypeForwards): TLapeType; virtual;
    procedure ParseTypeBlock; virtual;
    function ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList; virtual;

    function ParseExpression(ReturnOn: EParserTokenSet = []): TLapeTree_ExprBase; virtual;
    function ParseTypeExpression(ReturnOn: EParserTokenSet = []): TLapeTree_Base; virtual;
    function ParseStatement: TLapeTree_Base; virtual;
    function ParseStatementList: TLapeTree_StatementList; virtual;
    function ParseBeginEnd(AllowDot: Boolean = False): TLapeTree_StatementList; virtual;
    function ParseCase: TLapeTree_Case; virtual;
    function ParseFor: TLapeTree_For; virtual;
    function ParseIf: TLapeTree_If; virtual;
    function ParseRepeat: TLapeTree_Repeat; virtual;
    function ParseTry: TLapeTree_Try; virtual;
    function ParseWhile: TLapeTree_While; virtual;
  public
    FreeTokenizer: Boolean;
    FreeTree: Boolean;

    constructor Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    ); reintroduce; virtual;
    destructor Destroy; override;

    function getState: Pointer; virtual;
    procedure setState(const State: Pointer; FreeState: Boolean = True); virtual;
    function ParseFile: TLapeTree_Base; virtual;
    function Compile: Boolean; virtual;
    procedure CheckAfterCompile; virtual;

    function getDeclaration(Name: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration; override;
    function addLocalDecl(v: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration; override;
    function addLocalVar(v: TLapeType; Name: lpString = ''): TLapeVar; virtual;

    function addGlobalVar(v: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; overload; virtual;
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

    function addGlobalType(t: TLapeType; AName: lpString = ''): TLapeType; overload; virtual;
    function addGlobalType(s: lpString; AName: lpString): TLapeType; overload; virtual;

    function addGlobalFunc(s: lpString; Value: Pointer): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    property InternalMethodMap: TLapeInternalMethodMap read FInternalMethodMap;
    property Tree: TLapeTree_Base read FTree;
  published
    property Tokenizer: TLapeTokenizerBase read getTokenizer write setTokenizer;
    property BaseDefines: TStringList read FBaseDefines write setBaseDefines;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
    property OnFindFile: TLapeFindFile read FOnFindFile write FOnFindFile;
  end;

implementation

uses
  Variants,
  lpexceptions, lpinterpreter;

function TLapeCompiler.getPDocPos: PDocPos;
begin
  __tmp := Tokenizer.DocPos;
  Result := @__tmp;
end;

procedure TLapeCompiler.Reset;
begin
  inherited;

  FTokenizer := High(FTokenizers);
  while (FTokenizer > 0) do
    popTokenizer();

  if (FTokenizer = 0) and (FTokenizers[0] <> nil) then
    FTokenizers[0].Reset();

  if FreeTree and (FTree <> nil) then
    FTree.Free();
  FTree := nil;

  if (FIncludes <> nil) then
    FIncludes.Clear();
  if (FConditionalStack <> nil) then
    FConditionalStack.Reset();

  if (getDeclaration('String') = nil) then
    addGlobalType(getBaseType(ltString).createCopy(), 'String');
  if (getDeclaration('Char') = nil) then
    addGlobalType(getBaseType(ltChar).createCopy(), 'Char');
  if (getDeclaration('True') = nil) then
    addGlobalVar(True, 'True').isConstant := True;
  if (getDeclaration('False') = nil) then
    addGlobalVar(False, 'False').isConstant := True;
  if (getDeclaration('nil') = nil) then
    addGlobalVar(nil, 'nil').isConstant := True;
  if (getDeclaration('Null') = nil) then
    addGlobalVar(Null, 'Null').isConstant := True;

  if (FBaseDefines <> nil) then
  with FBaseDefines do
  begin
    add('Lape');
    add('Sesquipedalian');
    if (FDefines <> nil) then
      FDefines.Assign(FBaseDefines);
  end
  else if (FDefines <> nil) then
    FDefines.Clear();
end;

procedure TLapeCompiler.setBaseDefines(Defines: TStringList);
begin
  Assert(FBaseDefines <> nil);
  FBaseDefines.Assign(Defines);
  Reset();
end;

type
  __LapeTokenizerBase = class(TLapeTokenizerBase);
function TLapeCompiler.getTokenizer: TLapeTokenizerBase;
begin
  if (FTokenizer >= 0) then
    Result := FTokenizers[FTokenizer]
  else
    Result := nil;
end;

procedure TLapeCompiler.setTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  if (FreeTokenizer or (FTokenizer > 0)) and (FTokenizer >= 0) and (FTokenizer < Length(FTokenizers)) and
     (FTokenizers[FTokenizer] <> nil) and (FTokenizers[FTokenizer] <> ATokenizer)
  then
    FTokenizers[FTokenizer].Free();

  if (FTokenizer < 0) then
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
  setTokenizer(nil);
  Dec(FTokenizer);
end;

procedure TLapeCompiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
var
  r: TLapeConditional;
begin
  Assert(FConditionalStack <> nil);
  with r do
  begin
    Eval := AEval;
    Pos := ADocPos;
  end;
  FConditionalStack.Push(r);
end;

function TLapeCompiler.popConditional: TDocPos;
begin
  Assert(FConditionalStack <> nil);
  if (FConditionalStack.Size > 0) then
    Result := FConditionalStack.Pop().Pos
  else
    LapeException(lpeLostConditional, Tokenizer.DocPos);
end;

function TLapeCompiler.EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
begin
 if (Node <> nil) then
   Result := Node
 else
   Result := TLapeTree_ExprBase.Create(Self, getPDocPos());
end;

function TLapeCompiler.EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range;
begin
  VarType := nil;
  if (Node <> nil) and (Node is TLapeTree_Range) then
    Result := TLapeTree_Range(Node)
  else if (Node <> nil) and (Node is TLapeTree_ExprBase) then
  begin
    if (not (Node is TLapeTree_VarType)) or (TLapeTree_VarType(Node).VarType = nil) or (TLapeTree_VarType(Node).VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidRange, Node.DocPos);

    Result := TLapeTree_Range.Create(Self, @Node.DocPos);
    if (TLapeTree_VarType(Node).VarType is TLapeType_SubRange) then
      with TLapeType_SubRange(TLapeTree_VarType(Node).VarType) do
      begin
        Result.Lo := TLapeTree_GlobalVar.Create(TLapeGlobalVar(addManagedVar(getBaseType(BaseIntType).NewGlobalVarStr(IntToStr(Range.Lo)))), Self, @Node.DocPos);
        Result.Hi := TLapeTree_GlobalVar.Create(TLapeGlobalVar(addManagedVar(getBaseType(BaseIntType).NewGlobalVarStr(IntToStr(Range.Hi)))), Self, @Node.DocPos);
      end
    else
      with TLapeTree_VarType(Node).VarType do
      begin
        Result.Lo := TLapeTree_GlobalVar.Create(TLapeGlobalVar(addManagedVar(getBaseType(BaseIntType).NewGlobalVarP(LapeTypeLow[BaseIntType]))), Self, @Node.DocPos);
        Result.Hi := TLapeTree_GlobalVar.Create(TLapeGlobalVar(addManagedVar(getBaseType(BaseIntType).NewGlobalVarP(LapeTypeHigh[BaseIntType]))), Self, @Node.DocPos);
      end;
  end
  else if (Node <> nil) then
    LapeException(lpeInvalidRange, Node.DocPos)
  else
    LapeException(lpeInvalidRange, Tokenizer.DocPos);

  if (Result.Lo <> nil) and (Result.Hi <> nil) then
    VarType := Result.Hi.resType();
  if (VarType = nil) or (not (VarType.CompatibleWith(Result.Lo.resType()))) then
    LapeException(lpeInvalidRange, Node.DocPos);
end;

function TLapeCompiler.EnsureRange(Node: TLapeTree_Base): TLapeTree_Range;
var v: TLapeType;
begin
  Result := EnsureRange(Node, v);
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange;
var
  res: TlapeTree_Range;
  l, r: TLapeGlobalVar;
begin
  res := EnsureRange(Node, VarType);
  try
    if (Node = nil) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);

    l := res.Lo.Evaluate();
    r := res.Hi.Evaluate();
    if (l = nil) or (l.VarType = nil) or (l.VarType.BaseIntType = ltUnknown) or
       (r = nil) or (r.VarType = nil) or (r.VarType.BaseIntType = ltUnknown)
    then
      LapeException(lpeInvalidRange, Node.DocPos)
    else if (not l.isConstant) or (not r.isConstant) then
      LapeException(lpeConstantExpected, Node.DocPos);
    Result.Lo := l.AsInteger;
    Result.Hi := r.AsInteger;
    if (Result.Hi < Result.Lo) then
      LapeException(lpeInvalidRange, Node.DocPos);
  finally
    if (res <> Node) then
      res.Free();
  end;
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base): TLapeRange;
var v: TLapeType;
begin
  Result := EnsureConstantRange(Node, v);
end;

function TLapeCompiler.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
var
  t: TLapeTokenizerBase;

  procedure switchConditional;
  var
    r: TLapeConditional;
  begin
    if (FConditionalStack.Size <= 0) then
      LapeException(lpeLostConditional, Sender.DocPos)
    else
    begin
      r := FConditionalStack.Pop();
      r.Eval := not r.Eval;
      FConditionalStack.Push(r);
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
  t := nil;

  if ({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil) then
    if FOnHandleDirective(Self, Directive, Argument, Sender.InPeek) then
      Exit;

  Directive := LowerCase(Directive);
  if (Directive = 'ifdef') or (Directive = 'ifndef') then
    pushConditional((not InIgnore()) and ((FDefines.IndexOf(Trim(Argument)) > -1) xor (Directive = 'ifndef')), Sender.DocPos)
  else if (Directive = 'else') then
    switchConditional()
  else if (Directive = 'endif') then
    popConditional()
  else if InIgnore() then
    {nothing}
  else if (Directive = 'define') then
    FDefines.add(Trim(Argument))
  else if (Directive = 'undef') then
    RemoveFromStringList(FDefines, Trim(Argument))
  else if (Directive = 'i') or (Directive = 'include') or (Directive = 'include_once') then
  begin
    if ({$IFNDEF FPC}@{$ENDIF}FOnFindFile <> nil) then
      t := FOnFindFile(Self, Argument);

    if (not Sender.InPeek) then
      if (Directive = 'include_once') and (FIncludes.IndexOf(Argument) > -1) then
        LapeException(lpeDuplicateDeclaration, [Argument], Sender.DocPos)
      else
        FIncludes.add(Argument);

    if (t = nil) then
      if (FTokenizer + 1 < Length(FTokenizers)) and (FTokenizers[FTokenizer + 1] <> nil) and (FTokenizers[FTokenizer + 1].FileName = Argument) then
      begin
        t := FTokenizers[FTokenizer + 1];
        t.Reset();
      end
      else if ((Argument = '') or (not FileExists(Argument))) then
        LapeException(lpeFileNotFound, [Argument], Sender.DocPos)
      else
        t := TLapeTokenizerFile.Create(Argument);

    pushTokenizer(t);
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
  lTok: EParserToken;
begin
  lTok := Tokenizer.Tok;
  repeat
    Result := Tokenizer.Next{NoWhiteSpace}();
    if (Result = tk_NULL) and (FTokenizer > 0) then
    begin
      if Tokenizer.InPeek then
        Dec(FTokenizer)
      else
        popTokenizer();
      Result := Tokenizer.Next{NoWhiteSpace}();
    end;
  until (Result = tk_NULL) or ((not (Result in TokJunk)) and (not InIgnore()));
  __LapeTokenizerBase(Tokenizer).FLastTok := lTok;
end;

function TLapeCompiler.Peek: EParserToken;
var
  i: Integer;
  p: Pointer;
begin
  p := getState();
  try
    for i := 0 to High(FTokenizers) do
      if (FTokenizers[i] <> nil) then
        __LapeTokenizerBase(FTokenizers[i]).FInPeek := True;
    Result := Next();
  finally
    setState(p);
  end;
end;

function TLapeCompiler.Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    Next();
  Result := Tokenizer.Tok;
  if (Result <> Token) then
    LapeException(lpeExpectedOther, [LapeTokenToString(Result), LapeTokenToString(Token)], Tokenizer.DocPos);
  if NextAfter then
    Next();
end;

function TLapeCompiler.Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    Next();
  Result := Tokenizer.Tok;
  if (not (Result in Tokens)) then
    LapeException(lpeUnexpectedToken, [LapeTokenToString(Result)], Tokenizer.DocPos);
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
  f: TLapeFuncForwards;
  t: TLapeTree_Base;
  b: Boolean;
begin

  Result := TLapeTree_StatementList.Create(Self, getPDocPos());
  try
    f := TLapeFuncForwards.Create(nil, dupError);

    try
      b := False;
      repeat
        t := nil;
        case Peek() of
          tk_NULL: Break;
          tk_kw_Begin:
            begin
              t := ParseBeginEnd(not StopAfterBeginEnd);
              b := (Tokenizer.Tok = tk_sym_Dot) or StopAfterBeginEnd;
            end;
          tk_kw_Const, tk_kw_Var: t := ParseVarBlock();
          tk_kw_Function, tk_kw_Procedure: t := ParseMethod(f);
          tk_kw_Type: ParseTypeBlock();
          {$IFNDEF Lape_ForceBlock}
          else if (not StopAfterBeginEnd) then t := ParseStatement()
          {$ENDIF}
          else LapeException(lpeBlockExpected, Tokenizer.DocPos);
        end;
        if (t <> nil) then
          Result.addStatement(t);
        Expect([tk_sym_Dot, tk_sym_SemiColon], False, False);
      until b;

      if (f.Count > 0) then
        LapeException(lpeInvalidForward, [f[0].Name], f[0].VarType.DocPos);
    finally
      while (f.Count > 0) do
        f.Delete(0).Free();
      f.Free();
    end;

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method;
var
  isFunction: Boolean;
  a: TStringArray;
  i: Integer;
  p: TLapeParameter;
begin
  Expect([tk_kw_Function, tk_kw_Procedure], True, False);
  isFunction := (Tokenizer.Tok = tk_kw_Function);
  if addToScope then
    Result := TLapeType_ScriptMethod.Create(Self, nil, nil, '', getPDocPos())
  else
    Result := TLapeType_ImportedMethod.Create(Self, nil, nil, '', getPDocPos());

  try

    if (Peek() = tk_Identifier) then
    begin
      Next();
      Name := Tokenizer.TokString;
    end;

    if (Peek() = tk_sym_ParenthesisOpen) then
    begin
      Next();
      repeat
        case Peek() of
          tk_NULL: Break;
          tk_sym_ParenthesisClose:
            begin
              if (Tokenizer.Tok <> tk_sym_ParenthesisOpen) then
                Expect(tk_sym_SemiColon, True, False);
              Next();
              Break;
            end;
          tk_kw_Const: begin p.ParType := lptConst; Next(); end;
          tk_kw_Out:   begin p.ParType := lptOut;   Next(); end;
          tk_kw_Var:   begin p.ParType := lptVar;   Next(); end;
          else p.ParType := lptNormal;
        end;

        a := ParseIdentifierList();
        Expect(tk_sym_Colon, False, False);
        p.VarType := ParseType(nil);
        if (p.VarType = nil) then
          LapeException(lpeTypeExpected, Tokenizer.DocPos);
        Expect([tk_sym_Equals, tk_sym_SemiColon, tk_sym_ParenthesisClose], True, False);

        if (Tokenizer.Tok = tk_sym_Equals) then
        begin
          with ParseExpression([tk_sym_ParenthesisClose]) do
          try
            p.Default := Evaluate();
            if (p.ParType in [lptVar, lptOut]) and p.Default.isConstant then
              LapeException(lpeVariableExpected, DocPos);
          finally
            Free();
          end;
          Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
        end
        else
          p.Default := nil;

        for i := 0 to High(a) do
        begin
          if addToScope then
            if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
              LapeException(lpeImpossible, Tokenizer.DocPos)
            else if (LapeCase(a[i]) = LapeCase(Name)) or (getDeclaration(a[i], True) <> nil) then
              LapeException(lpeDuplicateDeclaration, [a[i]], Tokenizer.DocPos)
            else
              FStackInfo.addVar(p.ParType, p.VarType, a[i]);

          Result.addParam(p);
        end;
      until (Tokenizer.Tok = tk_sym_ParenthesisClose);
    end;

    if isFunction then
    begin
      Expect(tk_sym_Colon, True, False);
      Result.Res := ParseType(nil);
      if (Result.Res = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);

      if addToScope then
        if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
          LapeException(lpeImpossible, Tokenizer.DocPos)
        else if (LapeCase(Name) = LapeCase('Result')) or (getDeclaration('Result', True) <> nil) then
          LapeException(lpeDuplicateDeclaration, ['Result'], Tokenizer.DocPos)
        else
          FStackInfo.addVar(lptOut, Result.Res, 'Result');
    end;

    Result := TLapeType_Method(addManagedType(Result));

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method;
var
  t: TLapeType_Method;
  n: lpString;
  d: TDocPos;
  decl: TLapeDeclaration;
begin
  Result := nil;
  IncStackInfo();
  d := Tokenizer.DocPos;
  try
    if isExternal then
      t := TLapeType_ImportedMethod(ParseMethodHeader(n, False))
    else
      t := TLapeType_ScriptMethod(ParseMethodHeader(n));
    if (n = '') then
      LapeException(lpeBlockExpected, Tokenizer.DocPos);

    Expect(tk_sym_SemiColon, True, False);
    if (Peek() in [tk_kw_Forward, tk_kw_Overload{, tk_kw_Override}]) then
      Next();

    decl := getDeclaration(n, FStackInfo.Owner);
    if isExternal then
      Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(TLapeType_ImportedMethod(t).NewGlobalVar(nil), FStackInfo.Owner)), FStackInfo, Self, @d)
    else
      Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(TLapeType_ScriptMethod(t).NewGlobalVar(0), FStackInfo.Owner)), FStackInfo, Self, @d);

    try
      if (Tokenizer.Tok = tk_kw_Overload) then
      begin
        Expect(tk_sym_SemiColon, True, False);

        if (decl = nil) or ((decl is TLapeGlobalVar) and (TLapeGlobalVar(decl).VarType is TLapeType_Method)) then
          with TLapeType_OverloadedMethod(addLocalDecl(TLapeType_OverloadedMethod.Create(Self, nil, '', @d), FStackInfo.Owner)) do
          begin
            if (decl <> nil) then
              addMethod(TLapeGlobalVar(decl));
            decl := addLocalDecl(NewGlobalVar(n, @d), FStackInfo.Owner);
          end
        else if (not (decl is TLapeGlobalVar)) or (not (TLapeGlobalVar(decl).VarType is TLapeType_OverloadedMethod)) or (TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).getMethod(t) <> nil) then
          LapeException(lpeCannotOverload, Tokenizer.DocPos);

        try
          TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).addMethod(Result.Method);
        except on E: lpException do
          LapeException(E.Message, Tokenizer.DocPos);
        end;

        if (Peek() = tk_kw_Forward) then
          Next();
      end
      {else if (Tokenizer.Tok = tk_kw_Override) then
      begin
        Expect(tk_sym_SemiColon, True, False);

        if (decl <> nil) and (decl is TLapeGlobalVar) and (TLapeGlobalVar(decl).VarType is TLapeType_OverloadedMethod) then
          decl := TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).getMethod(t);
        if (decl = nil) or (not (decl is TLapeGlobalVar)) or (not (TLapeGlobalVar(decl).VarType is TLapeType_Method)) then
          LapeException(lpeUnknownParent, Tokenizer.DocPos);
        if (getDeclaration('inherited', FStackInfo, True) <> nil) then
          LapeException(lpeDuplicateDeclaration, ['inherited'], Tokenizer.DocPos);

        decl.Name := 'inherited';
        addLocalDecl(decl, FStackInfo);
        Result.Method.Name := n;
      end}
      else
      begin
        decl := getDeclaration(n, FStackInfo.Owner, True);
        if (decl <> nil) and (decl is TLapeGlobalVar) and (TLapeGlobalVar(decl).VarType is TLapeType_OverloadedMethod) then
        begin
          decl := TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).getMethod(t);
          if (decl = nil) then
            LapeException(lpeDuplicateDeclaration, [n], Tokenizer.DocPos)
        end;
        if (decl <> nil) then
          if (FuncForwards <> nil) and FuncForwards.ExistsItem(TLapeGlobalVar(decl)) then
          begin
            if (not TLapeGlobalVar(decl).VarType.Equals(t)) then
              LapeException(lpeNoForwardMatch, Tokenizer.DocPos);
            Result.FreeStackInfo := False;
            Result.Free();

            Result := TLapeTree_Method.Create(TLapeGlobalVar(decl), FStackInfo, Self, @d);
          end
          else
            LapeException(lpeDuplicateDeclaration, [n], Tokenizer.DocPos);

        Result.Method.Name := n;
      end;

      if (Tokenizer.Tok = tk_kw_Forward) then
      begin
        Expect(tk_sym_SemiColon, True, False);
        if (FuncForwards = nil) then
          LapeException(lpeBlockExpected, Tokenizer.DocPos)
        else if FuncForwards.ExistsItem(Result.Method) then
          LapeException(lpeDuplicateDeclaration, [n], Tokenizer.DocPos)
        else
          FuncForwards.add(Result.Method);

        Result.FreeStackInfo := False;
        FreeAndNil(Result);
        Exit;
      end;

      if isExternal then
        Exit;

      if (FuncForwards <> nil) and (decl is TLapeGlobalVar) then
        FuncForwards.DeleteItem(TLapeGlobalVar(decl));

      Result.Statements := ParseBlockList();
      if (Result.Statements = nil) or (Result.Statements.Statements.Count < 1) or (not (Result.Statements.Statements[Result.Statements.Statements.Count - 1] is TLapeTree_StatementList)) then
        Expect(tk_kw_Begin, False, False)
      else
        Expect(tk_sym_SemiColon, False, False);
    except
      Result.FreeStackInfo := False;
      FreeAndNil(Result);
      raise;
    end;
  finally
    DecStackInfo(True, False, (Result = nil));
  end;
end;

function TLapeCompiler.ParseType(TypeForwards: TLapeTypeForwards): TLapeType;

  procedure ParseArray;
  var
    t: TLapeTree_Base;
    r: TLapeRange;
    d: TDocPos;
  begin
    Expect(tk_kw_Array, True, False);
    d := Tokenizer.DocPos;

    Expect([tk_sym_BracketOpen, tk_kw_Of], True, False);
    if (Tokenizer.Tok = tk_sym_BracketOpen) then
    begin
      t := ParseTypeExpression();
      try
        r := EnsureConstantRange(t);
        Expect(tk_sym_BracketClose, False, False);
        Expect(tk_kw_Of, True, False);
        Result := addManagedType(TLapeType_StaticArray.Create(r, ParseType(nil), Self, '', @d));
      finally
        if (t <> nil) then
          t.Free();
      end;
    end
    else
      Result := addManagedType(TLapeType_DynArray.Create(ParseType(nil), Self, '', @d));
  end;

  procedure ParseRecord(IsPacked: Boolean = False);
  var
    rr: TLapeType_Record absolute Result;
    x: TLapeType;
    a: TStringArray;
    i: Integer;
  begin
    Expect([tk_kw_Record, tk_kw_Union], True, False);
    if (Tokenizer.Tok = tk_kw_Record) then
      rr := TLapeType_Record.Create(Self, nil, '', getPDocPos())
    else
      rr := TLapeType_Union.Create(Self, nil, '', getPDocPos());

    repeat
      a := ParseIdentifierList();
      Expect(tk_sym_Colon, False, False);
      x := ParseType(nil);
      Expect(tk_sym_SemiColon, True, False);
      for i := 0 to High(a) do
        if IsPacked then
          rr.addField(x, a[i], 1)
        else
          rr.addField(x, a[i], Options_PackRecords);

      if (Peek() = tk_kw_End) then
      begin
        Expect(tk_kw_End, True, False);
        Break;
      end;
    until False;

    Result := addManagedType(rr);
  end;

  procedure ParseSet;
  var
    t: TLapeType;
  begin
    Expect(tk_kw_Set, True, False);
    Expect(tk_kw_Of, True, False);
    t := ParseType(nil);
    if (not (t is TLapeType_SubRange)) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);

    try
      Result := addManagedType(TLapeType_Set.Create(TLapeType_SubRange(t), Self, '', GetPDocPos));
    except on E: lpException do
      LapeException(E.Message, Tokenizer.DocPos);
    end;
  end;

  procedure ParsePointer;
  var
    x: TLapeType;
    d: TDocPos;
  begin
    Expect(tk_sym_Caret, True, False);
    d := Tokenizer.DocPos;

    Expect(tk_Identifier, True, False);
    x := TLapeType(getDeclaration(Tokenizer.TokString));
    if ((x = nil) and (TypeForwards = nil)) or ((x <> nil) and (not (x is TLapeType))) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    if (x <> nil) then
      Result := addManagedType(TLapeType_Pointer.Create(Self, x, '', @d))
    else
    begin
      Result := TLapeType_Pointer.Create(Self, x, '', @d);
      TypeForwards.add(Tokenizer.TokString, Result);
    end;
  end;

  procedure ParseEnum;
  var
    re: TLapeType_Enum absolute Result;
    n: lpString;
    t: TlapeTree_ExprBase;
    v: TLapeGlobalVar;
    so: TLapeStackInfo;
  begin
    Expect(tk_sym_ParenthesisOpen, True, False);
    re := TLapeType_Enum.Create(Self, nil, '', getPDocPos());
    if (FStackInfo = nil) then
      so := nil
    else
      so := FStackInfo.Owner;

    repeat
      Expect(tk_Identifier, True, False);
      n := Tokenizer.TokString;
      if (getDeclaration(n, so, True) <> nil) then
        LapeException(lpeDuplicateDeclaration, [n], Tokenizer.DocPos);

      Expect([tk_sym_Comma, tk_sym_ParenthesisClose, tk_sym_Equals], True, False);
      if (Tokenizer.Tok = tk_cmp_Equal) then
      try
        t := ParseExpression([tk_sym_Comma, tk_sym_ParenthesisClose]);
        try
          if (t <> nil) then
            v := t.Evaluate()
          else
            v := nil;

          if (v = nil) or (v.VarType = nil) or (v.VarType.BaseIntType = ltUnknown) or (not v.isConstant) then
            LapeException(lpeExpressionExpected, Tokenizer.DocPos);
          TLapeGlobalVar(addLocalDecl(re.NewGlobalVar(re.addMember(v.AsInteger, n), n), so)).isConstant := True;
        finally
          t.Free();
        end;
      except on E: lpException do
        LapeException(E.Message, Tokenizer.DocPos);
      end
      else
        TLapeGlobalVar(addLocalDecl(re.NewGlobalVar(re.addMember(n), n), so)).isConstant := True;
    until (Tokenizer.Tok in [tk_NULL, tk_sym_ParenthesisClose]);
    Result := addManagedType(re);
  end;

  procedure ParseDef;
  var
    t: TlapeTree_Base;
    r: TLapeRange;
    v: TLapeType;
  begin
    t := ParseTypeExpression([tk_sym_Equals, tk_op_Assign, tk_sym_ParenthesisClose]);
    try
      if (t <> nil) and (t is TLapeTree_Range) then
      begin
        r := EnsureConstantRange(t, v);
        Result := addManagedType(TLapeType_SubRange.Create(r, Self, v, '', getPDocPos()))
      end
      else if (t <> nil) and (t is TLapeTree_Operator) and (TLapeTree_Operator(t).OperatorType = op_Index) then
        with TLapeTree_Operator(t) do
        begin
          r.Hi := -1;
          try
            if (Right <> nil) and (Right is TLapeTree_ExprBase) then
              if TLapeTree_ExprBase(Right).isConstant() then
                r.Hi := TLapeTree_ExprBase(Right).Evaluate().AsInteger
              else
                LapeException(lpeConstantExpected, Right.DocPos);
          finally
            if (r.Hi < 0) or (r.Hi > High(UInt8)) then
              if (Right <> nil) then
                LapeException(lpeInvalidRange, DocPos)
              else
                LapeException(lpeInvalidRange, Right.DocPos);
          end;

          if (left <> nil) and (Left is TLapeTree_VarType) and (TLapeTree_VarType(Left).VarType <> nil) and (TLapeTree_VarType(Left).VarType.BaseType = ltAnsiString) then
            Result := addManagedType(TLapeType_ShortString.Create(Self, r.Hi, '', @Left.DocPos))
          else
            LapeException(lpeOutOfTypeRange, Tokenizer.DocPos);
        end
      else if (t <> nil) and (t is TLapeTree_VarType) and (TLapeTree_VarType(t).VarType <> nil) then
        Result := TLapeTree_VarType(t).VarType
      else
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
    finally
      if (t <> nil) then
      begin
        Tokenizer.tempRollBack();
        t.Free();
      end;
    end;
  end;

begin
  Result := nil;
  try

    case Peek() of
      tk_kw_Array: ParseArray();
      tk_kw_Record, tk_kw_Union: ParseRecord();
      tk_kw_Set: ParseSet();
      tk_kw_Packed:
        begin
          Next();
          ParseRecord(True);
        end;
      tk_sym_Caret: ParsePointer();
      tk_sym_ParenthesisOpen: ParseEnum();
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
  f: TLapeTypeForwards;
  t: TLapeType;
  s: lpString;
begin
  f := TLapeTypeForwards.Create(nil, {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF}, dupIgnore);
  try
    Expect(tk_kw_Type, True, False);
    repeat
      Expect(tk_Identifier, True, False);
      s := Tokenizer.TokString;
      if (getDeclaration(s, True) <> nil) then
        LapeException(lpeDuplicateDeclaration, [s], Tokenizer.DocPos);
      Expect(tk_sym_Equals, True, False);

      t := ParseType(f).CreateCopy();
      t.Name := s;
      addLocalDecl(t);

      Expect(tk_sym_SemiColon, True, False);
    until (Peek() <> tk_Identifier);

    while (f.Count > 0) do
      with __LapeType_Pointer(f.ItemsI[0]) do
      begin
        FPType := TLapeType(getDeclaration(f.Index[0]));
        if (PType = nil) then
          LapeException(lpeInvalidForward, [f.Index[0]], DocPos);
        f.Delete(0);
      end;
  finally
    while (f.Count > 0) do
      f.Delete(0).Free();
    f.Free();
  end;
end;

function TLapeCompiler.ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList;
var
  isConst: Boolean;
  a: TStringArray;
  i: Integer;
  t: TLapeType;
  b: TLapeTree_ExprBase;
  v: TLapeVar;
  v2: TLapeGlobalVar;
  vd: TLapeVarDecl;
begin
  Result := TLapeTree_VarList.Create(Self, getPDocPos());
  try

    Expect([tk_kw_Const, tk_kw_Var], True, False);
    isConst := (Tokenizer.Tok = tk_kw_Const);
    repeat
      t := nil;
      b := nil;
      v2 := nil;

      a := ParseIdentifierList();
      Expect([tk_sym_Colon, tk_op_Assign, tk_sym_Equals], False, False);

      if (Tokenizer.Tok = tk_sym_Colon) then
      begin
        t := ParseType(nil);
        if isConst then
          Expect([tk_op_Assign, tk_sym_Equals], True, False)
        else
          Expect([tk_op_Assign, tk_sym_Equals] + ValidEnd, True, False);
      end;

      if (Tokenizer.Tok = tk_sym_Equals) then
      begin
        b := ParseExpression();
        if (b <> nil) and (not b.isConstant()) then
          LapeException(lpeConstantExpected, b.DocPos);
        if (t <> nil) and (b is TLapeTree_OpenArray) then
          TLapeTree_OpenArray(b).ToType := t;

        try
          Expect(ValidEnd, False, False);
          v2 := b.Evaluate();
        finally
          if (b <> nil) then
            FreeAndNil(b);
        end;
      end
      else if (Tokenizer.Tok = tk_op_Assign) then
      begin
        if (Length(a) <> 1) then
          LapeException(lpeDefaultToMoreThanOne, Tokenizer.DocPos);

        b := ParseExpression();
        Expect(ValidEnd, False, False);
      end;

      if (t = nil) then
        if (v2 <> nil) then
          t := v2.VarType
        else if (b <> nil) then
          t := b.resType()
        else
          LapeException(lpeCannotAssign, Tokenizer.DocPos);
      if (t = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);

      for i := 0 to High(a) do
      begin
        if (getDeclaration(a[i], True) <> nil) then
          LapeException(lpeDuplicateDeclaration, [a[i]], Tokenizer.DocPos);

        if isConst then
          v := TLapeVar(addLocalDecl(t.NewGlobalVarP(nil, a[i])))
        else
          v := addLocalVar(t, a[i]);

        if (v2 <> nil) then
          if (not (v is TLapeGlobalVar)) then
          begin
            vd.VarDecl := v;
            vd.Default := TLapeTree_GlobalVar.Create(v2, Self, GetPDocPos());
            Result.addVar(vd);
          end
          else
            v.VarType.EvalConst(op_Assign, TLapeGlobalVar(v), v2)
        else if (b <> nil) then
        begin
          vd.VarDecl := v;
          vd.Default := b;
          Result.addVar(vd);
          b := nil;
        end;

        if (v is TLapeVar) then
          TLapeVar(v).isConstant := isConst;
      end;
    until OneOnly or (Peek() <> tk_Identifier);

  except
    Result.Free();
    if (b <> nil) then
      b.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseExpression(ReturnOn: EParserTokenSet = []): TLapeTree_ExprBase;
const
  ParenthesisOpen = Pointer(-1);
var
  VarStack: TLapeTree_NodeStack;
  OpStack: TLapeTree_OpStack;
  Precedence: Byte;
  v: TLapeDeclaration;
  f: TLapeTree_Invoke;
  _LastNode: (_None, _Var, _Op);
  InExpr: Integer;

  procedure PopOpNode;
  var
    OpNode: TLapeTree_Operator;
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
      OpNode.Free();
      LapeException(lpeInvalidEvaluation, OpNode.DocPos);
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
    s: lpString;
    d: TDocPos;
  begin
    d := Tokenizer.DocPos;
    case Tokenizer.Tok of
      tk_typ_String: s := getString();
      tk_typ_Char: s := Tokenizer.TokChar;
      else LapeException(lpeImpossible);
    end;
    while (Peek() in [tk_typ_String, tk_typ_Char]) do
    begin
      case Next() of
        tk_typ_String:
          if (Tokenizer.LastTok = tk_typ_String) then
            s := s + #39 + getString()
          else
            s := s + getString();
        tk_typ_Char: s := s + Tokenizer.TokChar;
        else LapeException(lpeImpossible);
      end;
      ForceString := True;
    end;

    if (Length(s) = 1) and (not ForceString) then
      PushVarStack(TLapeTree_Char.Create(WideChar(s[1]), Self, @d))
    else
      PushVarStack(TLapeTree_String.Create(s, Self, @d));
  end;

  procedure ParseAndPushArray;
  var
    r: TLapeTree_OpenArray;
  begin
    r := TLapeTree_OpenArray.Create(Self, getPDocPos());
    try
      while (not (Tokenizer.Tok in [tk_NULL, tk_sym_BracketClose])) do
      begin
        r.addValue(ParseTypeExpression());
        Expect([tk_sym_Comma, tk_sym_BracketClose], False, False);
      end;
    except
      r.Free();
      raise;
    end;
    PushVarStack(r);
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
        LapeException(lpeExpected, [LapeTokenToString(tk_Identifier)]);
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
  f := nil;
  VarStack := TLapeTree_NodeStack.Create(8);
  OpStack := TLapeTree_OpStack.Create(16);
  _LastNode := _None;
  InExpr := 0;

  try
    while True do
    begin
      if (Next() in ReturnOn) and (InExpr <= 0) then
        Break;
      case Tokenizer.Tok of
        tk_typ_Integer: PushVarStack(TLapeTree_Integer.Create(Tokenizer.TokString, Self, getPDocPos()));
        tk_typ_Integer_Hex: PushVarStack(TLapeTree_Integer.Create(IntToStr(Tokenizer.TokInt64), Self, getPDocPos()));
        tk_typ_Integer_Bin: PushVarStack(TLapeTree_Integer.Create(IntToStr(Tokenizer.TokInt64), Self, getPDocPos()));
        tk_typ_Float: PushVarStack(TLapeTree_Float.Create(Tokenizer.TokString, Self, getPDocPos()));
        tk_typ_String: ParseAndPushString();
        tk_typ_Char:
          begin
            if (Peek() in [tk_typ_String, tk_typ_Char]) then
              ParseAndPushString()
            else
              PushVarStack(TLapeTree_Char.Create(Tokenizer.TokChar, Self, getPDocPos()));
          end;

        tk_Identifier:
          begin
            v := getDeclaration(Tokenizer.TokString);

            if (v <> nil) and (v is TLapeGlobalVar) then
              PushVarStack(TLapeTree_GlobalVar.Create(TLapeGlobalVar(v), Self, getPDocPos()))
            else if (v <> nil) and (v is TLapeVar) then
              PushVarStack(TlapeTree_ResVar.Create(getResVar(TLapeVar(v)), Self, getPDocPos()))
            else if (v <> nil) and (v is TLapeType) then
              PushVarStack(TLapeTree_VarType.Create(TLapeType(v), Self, getPDocPos()))
            else if (FInternalMethodMap[Tokenizer.TokString] <> nil) then
            begin
              f := FInternalMethodMap[Tokenizer.TokString].Create(Self, getPDocPos());
              if (Peek() = tk_sym_ParenthesisOpen) then
                _LastNode := _Var
              else
              begin
                VarStack.Push(f);
                f := nil;
              end;
            end
            else
              LapeException(lpeUnknownDeclaration, [Tokenizer.TokString], Tokenizer.DocPos);
          end;

        tk_sym_ParenthesisOpen:
          begin
            if (_LastNode = _Var) then
            begin
              PopOpStack(op_Invoke);
              if (f = nil) then
                f := TLapeTree_Invoke.Create(VarStack.Pop(), Self, getPDocPos());
              if (Peek() = tk_sym_ParenthesisClose) then
                Next()
              else
              begin
                f.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma])));
                while True do
                  case Tokenizer.Tok of
                    tk_sym_ParenthesisClose: Break;
                    tk_sym_Comma: f.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma])));
                    else
                      LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
                  end;
              end;
              VarStack.Push(f);
              f := nil;
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

    Result := TLapeTree_ExprBase(FoldConstants(VarStack.Pop()));
    PrintTree(Result);
  finally
    if (f <> nil) then
      f.Free();
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

function TLapeCompiler.ParseTypeExpression(ReturnOn: EParserTokenSet = []): TLapeTree_Base;
var
  r: TLapeTree_ExprBase;
  v: TLapeType;
begin
  Result := nil;
  v := nil;

  r := ParseExpression(ReturnOn);
  if (Tokenizer.Tok <> tk_sym_DotDot) then
    Result := r
  else
  try
    Result := TLapeTree_Range.Create(Self, getPDocPos());
    TLapeTree_Range(Result).Lo := r;
    TLapeTree_Range(Result).Hi := ParseExpression(ReturnOn);

    if (r <> nil) and (TLapeTree_Range(Result).Hi <> nil) then
      v := TLapeTree_Range(Result).Hi.resType();
    if (v = nil) or (not (v.CompatibleWith(r.resType()))) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseStatement: TLapeTree_Base;
begin
  case Peek() of
    tk_NULL, tk_kw_End, tk_kw_Finally, tk_kw_Except, tk_kw_Until: Result := nil;

    tk_kw_Begin: Result := ParseBeginEnd();
    tk_kw_Case: Result := ParseCase();
    {$IFNDEF Lape_ForceBlock}
    tk_kw_Const, tk_kw_Var: Result := ParseVarBlock(True);
    {$ENDIF}
    tk_kw_For: Result := ParseFor();
    tk_kw_If: Result := ParseIf();
    tk_kw_Repeat: Result := ParseRepeat();
    tk_kw_While: Result := ParseWhile();
    tk_kw_Try: Result := ParseTry();

    tk_sym_SemiColon, tk_kw_Else:
      begin
        //if (not (Tokenizer.Tok in [tk_sym_SemiColon, tk_kw_Else])) then
          Next();
        Result := nil;
      end;
    else
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
end;

function TLapeCompiler.ParseStatementList: TLapeTree_StatementList;
var
  t: TLapeTree_Base;
begin
  Result := TLapeTree_StatementList.Create(Self, getPDocPos());
  try

    repeat
      t := ParseStatement();
      if (t <> nil) then
      begin
        Result.addStatement(t);
        Expect(tk_sym_SemiColon, False, False);
      end;
    until (t = nil);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseBeginEnd(AllowDot: Boolean = False): TLapeTree_StatementList;
begin
  Expect(tk_kw_Begin, True, False);
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
  t: TLapeTree_Base;
  Field: TLapeTree_MultiIf;
begin
  Expect(tk_kw_Case, True, False);
  Result := TLapeTree_Case.Create(Self, getPDocPos());
  t := nil;
  Field := nil;
  try
    Result.Condition := ParseExpression();
    Expect(tk_kw_Of, False, False);

    while (not (Peek() in [tk_Null, tk_kw_Else, tk_kw_End])) do
    begin
      t := ParseTypeExpression();
      Field := TLapeTree_MultiIf.Create(nil, Self, @t.DocPos);
      repeat
        Field.addValue(t);
        t := nil;
        Expect([tk_sym_Comma, tk_sym_Colon], False, False);
        if (Tokenizer.Tok = tk_sym_Colon) then
          Break
        else
          t := ParseTypeExpression();
      until False;
      Field.Body := ParseStatement();
      Result.addField(Field);
      Field := nil;
    end;

    Expect([tk_kw_Else, tk_kw_End], True, False);
    if (Tokenizer.Tok = tk_kw_Else) then
    begin
      Result.ElseBody := ParseStatement();
      Expect(tk_kw_End, True, False);
    end;

    Expect([tk_sym_SemiColon, tk_kw_Else], True, False);
  except
    if (t <> nil) then
      t.Free();
    if (Field <> nil) then
      Field.Free();
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseFor: TLapeTree_For;
begin
  Expect(tk_kw_For, True, False);
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
          Result.Counter := TLapeTree_Operator.Create(op_Assign, Compiler, @DocPos);
          with TLapeTree_Operator(Result.Counter) do
          begin
            Left := TLapeTree_ResVar.Create(getResVar(Vars[0].VarDecl), Compiler, @DocPos);
            Right := Vars[0].Default;
          end;
        end
        else
          Result.Counter := TLapeTree_ResVar.Create(getResVar(Vars[0].VarDecl), Compiler, @DocPos);
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
  Expect(tk_kw_If, True, False);
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
  Expect(tk_kw_Try, True, False);
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
  Expect(tk_kw_While, True, False);
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

constructor TLapeCompiler.Create(
  ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
  AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True);
begin
  inherited Create(AEmitter, ManageEmitter);

  FTokenizer := -1;
  FreeTokenizer := ManageTokenizer;
  FreeTree := True;

  FIncludes := TStringList.Create();
  FIncludes.Duplicates := dupIgnore;
  FIncludes.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  FDefines := TStringList.Create();
  FDefines.Duplicates := dupIgnore;
  FDefines.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  FBaseDefines := TStringList.Create();
  FBaseDefines.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  FConditionalStack := TLapeConditionalStack.Create(0);

  FOnHandleDirective := nil;
  FOnFindFile := nil;

  FInternalMethodMap := TLapeInternalMethodMap.Create(nil);
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

  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeHigh, '!high');
  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeLength, '!length');
  addGlobalFunc([TLapeType(nil)], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltInt32), @_LapeStrLen, '!strlen');

  setTokenizer(ATokenizer);
  Reset();
end;

destructor TLapeCompiler.Destroy;
begin
  setTokenizer(nil);
  FreeAndNil(FIncludes);
  FreeAndNil(FDefines);
  FreeAndNil(FBaseDefines);
  FreeAndNil(FConditionalStack);
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

procedure TLapeCompiler.setState(const State: Pointer; FreeState: Boolean = True);
var
  i: Integer;
begin
  with PCompilerState(State)^ do
  begin
    Assert(Length(FTokenizers) >= Length(Tokenizers));
    FTokenizer := Tokenizer;

    for i := 0 to High(Tokenizers) do
      if (Tokenizers[i] <> nil) and (FTokenizers[i] <> nil) then
        FTokenizers[i].setState(Tokenizers[i]);

    FDefines.Text := Defines;
    FConditionalStack.ImportFromArray(Conditionals);
  end;
  if FreeState then
    Dispose(PCompilerState(State));
end;

function TLapeCompiler.ParseFile: TLapeTree_Base;
begin
  Result := nil;
  Assert(Tokenizer <> nil);

  try
    if (FDefines <> nil) and (FBaseDefines <> nil) then
      FDefines.Assign(FBaseDefines);

    case Peek() of
      tk_kw_Program:
        begin
          Expect(tk_kw_Program, True, False);
          Expect(tk_Identifier, True, False);
          Expect(tk_sym_SemiColon, True, False);
          Result := ParseBlockList(False);
        end
      else Result := ParseBlockList(False);
    end;

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
    if (FTree = nil) then
      LapeException(lpeExpressionExpected);
    FTree.Compile();
    FEmitter._op(ocNone);
    FreeAndNil(FTree);
    DecStackInfo(False, True, True);
    Result := True;

  except
    Reset();
    raise;
  end;
end;

procedure TLapeCompiler.CheckAfterCompile;
begin
  Assert(Tokenizer <> nil);

  if (FTokenizer > 0) then
    LapeException(lpeImpossible, Tokenizer.DocPos);
  if (FConditionalStack.Cur >= 0) then
    LapeException(lpeConditionalNotClosed, popConditional());
end;

function TLapeCompiler.getDeclaration(Name: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration;
begin
  Result := inherited;
  if (Result = nil) and LocalOnly and (AStackInfo <> nil) and (AStackInfo.Owner = nil) then
    Result := inherited getDeclaration(Name, nil, Localonly);
end;

function TLapeCompiler.addLocalDecl(v: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration;
begin
  if (v = nil) then
    Exit(nil);
  if (v.Name <> '') and (getDeclaration(v.Name, AStackInfo, True) <> nil) then
    LapeException(lpeDuplicateDeclaration, [v.Name]);

  Result := v;
  if (AStackInfo = nil) or (AStackInfo.Owner = nil) then
    FGlobalDeclarations.addDeclaration(v)
  else
    AStackInfo.addDeclaration(addManagedDecl(v));
end;

function TLapeCompiler.addLocalVar(v: TLapeType; Name: lpString = ''): TLapeVar;
begin
  if (v = nil) then
    Exit(nil);
  if (Name <> '') and (getDeclaration(Name, True) <> nil) then
    LapeException(lpeDuplicateDeclaration, [Name]);

  if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
    Result := addGlobalVar(v.NewGlobalVarP(), Name)
  else
    Result := addStackVar(v, Name);
end;

function TLapeCompiler.addGlobalVar(v: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar;
begin
  if (v <> nil) then
  begin
    if (AName <> '') then
      v.Name := AName;
    if (Length(FGlobalDeclarations.getByName(v.Name)) > 0) then
      LapeException(lpeDuplicateDeclaration, [v.Name]);
    v.isConstant := False;
    FGlobalDeclarations.addDeclaration(v);
  end;
  Result := v;
end;

function TLapeCompiler.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
var
  s: lpString;
  f: TLapeStackInfo;
  t: TLapeTokenizerBase;
  p: Pointer;
  i: Integer;
begin
  s := 'var ' + AName + ': ' + Typ;
  if (Value <> '') then
   s := s + ' = ' + Value;

  if (FTokenizer < 0) then
    SetLength(FTokenizers, 1);
  f := FStackInfo;
  i := FTokenizer;
  t := FTokenizers[0];
  p := getState();

  FTokenizer := 0;
  FTokenizers[0] := TLapeTokenizerString.Create(s + ';');
  try
    FStackInfo := nil;
    ParseVarBlock().Free();
    Result := FGlobalDeclarations.Items[FGlobalDeclarations.Items.Count - 1] as TLapeGlobalVar;
    CheckAfterCompile();
  finally
    FTokenizers[0].Free();
    FTokenizers[0] := t;
    FTokenizer := i;
    FStackInfo := f;
    setState(p);
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

function TLapeCompiler.addGlobalType(t: TLapeType; AName: lpString = ''): TLapeType;
begin
  if (t <> nil) then
  begin
    if (AName <> '') then
      t.Name := AName;
    if (Length(FGlobalDeclarations.getByName(t.Name)) > 0) then
      LapeException(lpeDuplicateDeclaration, [t.Name]);
    FGlobalDeclarations.addDeclaration(t);
  end;
  Result := t;
end;

function TLapeCompiler.addGlobalType(s: lpString; AName: lpString): TLapeType;
var
  f: TLapeStackInfo;
  t: TLapeTokenizerBase;
  p: Pointer;
  i: Integer;
begin
  Result := nil;
  if (FTokenizer < 0) then
    SetLength(FTokenizers, 1);
  f := FStackInfo;
  i := FTokenizer;
  t := FTokenizers[0];
  p := getState();

  FTokenizer := 0;
  FTokenizers[0] := TLapeTokenizerString.Create('type ' + AName + ' = ' + s + ';');
  try
    FStackInfo := nil;
    ParseTypeBlock();
    Result := FGlobalDeclarations.Items[FGlobalDeclarations.Items.Count - 1] as TLapeType;
    CheckAfterCompile();
  finally
    FTokenizers[0].Free();
    FTokenizers[0] := t;
    FTokenizer := i;
    FStackInfo := f;
    setState(p);
  end;
end;

function TLapeCompiler.addGlobalFunc(s: lpString; Value: Pointer): TLapeGlobalVar;
var
  f: TLapeStackInfo;
  t: TLapeTokenizerBase;
  p: Pointer;
  i: Integer;
  m: TLapeTree_Method;
begin
  Result := nil;
  if (FTokenizer < 0) then
    SetLength(FTokenizers, 1);
  f := FStackInfo;
  i := FTokenizer;
  t := FTokenizers[0];
  p := getState();

  FTokenizer := 0;
  FTokenizers[0] := TLapeTokenizerString.Create(s + ';');
  try
    FStackInfo := nil;
    m := ParseMethod(nil, True);
    CheckAfterCompile();

    try
      if (m.Method = nil) or (m.Method.VarType = nil) or (not (m.Method.VarType is TLapeType_ImportedMethod)) then
        LapeException(lpeInvalidEvaluation);

      Result := m.Method;
      PPointer(Result.Ptr)^ := Value;
    finally
      FreeAndNil(m);
    end;
  finally
    FTokenizers[0].Free();
    FTokenizers[0] := t;
    FTokenizer := i;
    FStackInfo := f;
    setState(p);
  end;
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_ImportedMethod(addManagedType(TLapeType_ImportedMethod.Create(Self, AParams, AParTypes, AParDefaults, ARes))).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalFunc(AParams, AParTypes, AParDefaults, nil, Value, AName);
end;

end.

