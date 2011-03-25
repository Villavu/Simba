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

  TLapeCompiler = class(TLapeCompilerBase)
  private
    __tmp: TDocPos;
    function getPDocPos: PDocPos; inline;
  protected
    FTokenizer: TLapeTokenizerBase;
    FInternalMethodMap: TLapeInternalMethodMap;
    FTree: TLapeTree_Base;

    procedure Reset; override;
    procedure setTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    function EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase; virtual;
    function EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range; overload; virtual;
    function EnsureRange(Node: TLapeTree_Base): TLapeTree_Range; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base): TLapeRange; overload; virtual;
   public
    FreeTokenizer: Boolean;
    FreeTree: Boolean;

    constructor Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    ); reintroduce; virtual;
    destructor Destroy; override;

    function ParseIdentifierList: TStringArray; virtual;
    function ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList; virtual;
    function ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method;
    function ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method;
    function ParseType(TypeForwards: TLapeTypeForwards): TLapeType; virtual;
    procedure ParseTypeBlock; virtual;
    function ParseVarBlock: TLapeTree_VarList; virtual;

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

    function Compile: Boolean; virtual;

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
    function addGlobalVar(Value: Boolean; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: ShortString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; virtual;
	{$IFNDEF Lape_NoWideString}
    function addGlobalVar(Value: WideString; AName: lpString): TLapeGlobalVar; overload; virtual;
	{$ENDIF}
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: AnsiChar; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: WideChar; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addGlobalType(t: TLapeType; AName: lpString = ''): TLapeType; overload; virtual;
    function addGlobalType(s: lpString; AName: lpString): TLapeType; overload; virtual;

    function addGlobalFunc(s: lpString; Value: Pointer): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of TLapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of TLapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    property Tokenizer: TLapeTokenizerBase read FTokenizer write setTokenizer;
    property InternalMethodMap: TLapeInternalMethodMap read FInternalMethodMap;
    property Tree: TLapeTree_Base read FTree;
  end;

implementation

uses
  lpexceptions, lpinterpreter;

procedure TLapeCompiler.Reset;
begin
  inherited;
  if (FTokenizer <> nil) then
    FTokenizer.Reset();
  if FreeTree and (FTree <> nil) then
    FTree.Free();
  FTree := nil;

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
end;

function TLapeCompiler.getPDocPos: PDocPos;
begin
  __tmp := FTokenizer.DocPos;
  Result := @__tmp;
end;

procedure TLapeCompiler.setTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  if FreeTokenizer and (FTokenizer <> nil) then
    FTokenizer.Free();
  FTokenizer := ATokenizer;
  Reset();
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
    LapeException(lpeInvalidRange, FTokenizer.DocPos);

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
      LapeException(lpeInvalidRange, FTokenizer.DocPos);

    l := res.Lo.Evaluate();
    r := res.Hi.Evaluate();
    if (l = nil) or (l.VarType = nil) or (l.VarType.BaseIntType = ltUnknown) or
       (r = nil) or (r.VarType = nil) or (r.VarType.BaseIntType = ltUnknown)
    then
      LapeException(lpeInvalidRange, Node.DocPos)
    else if (not l.isConstant) or (r.isConstant) then
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

constructor TLapeCompiler.Create(
  ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
  AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True);
begin
  inherited Create(AEmitter, ManageEmitter);

  FreeTokenizer := ManageTokenizer;
  FreeTree := True;

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

  setTokenizer(ATokenizer); //Calls Reset(), which initializes the FCode array
end;

destructor TLapeCompiler.Destroy;
begin
  if FreeTokenizer and (FTokenizer <> nil) then
    FreeAndNil(FTokenizer);
  FInternalMethodMap.Free();
  inherited;
end;

function TLapeCompiler.ParseIdentifierList: TStringArray;
begin
  SetLength(Result, 0);
  repeat
    FTokenizer.Expect(tk_Identifier, True, False);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := FTokenizer.TokString;
  until (FTokenizer.NextNoJunk() <> tk_sym_Comma);
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
        case FTokenizer.PeekNoJunk() of
          tk_NULL: Break;
          tk_kw_Begin:
            begin
              t := ParseBeginEnd(not StopAfterBeginEnd);
              b := (FTokenizer.Tok = tk_sym_Dot) or StopAfterBeginEnd;
            end;
          tk_kw_Const, tk_kw_Var: t := ParseVarBlock();
          tk_kw_Function, tk_kw_Procedure: t := ParseMethod(f);
          tk_kw_Type: ParseTypeBlock();
          else LapeException(lpeBlockExpected, FTokenizer.DocPos);
        end;
        if (t <> nil) then
          Result.addStatement(t);
        FTokenizer.Expect([tk_sym_Dot, tk_sym_SemiColon], False, False);
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
  FTokenizer.Expect([tk_kw_Function, tk_kw_Procedure], True, False);
  isFunction := (FTokenizer.Tok = tk_kw_Function);
  if addToScope then
    Result := TLapeType_ScriptMethod.Create(Self, nil, nil, '', getPDocPos())
  else
    Result := TLapeType_ImportedMethod.Create(Self, nil, nil, '', getPDocPos());

  try

    if (FTokenizer.PeekNoJunk() = tk_Identifier) then
    begin
      FTokenizer.NextNoJunk();
      Name := FTokenizer.TokString;
    end;

    if (FTokenizer.PeekNoJunk() = tk_sym_ParenthesisOpen) then
    begin
      FTokenizer.NextNoJunk();
      repeat
        case FTokenizer.PeekNoJunk() of
          tk_NULL: Break;
          tk_sym_ParenthesisClose:
            begin
              if (FTokenizer.Tok <> tk_sym_ParenthesisOpen) then
                FTokenizer.Expect(tk_sym_SemiColon, True, False);
              FTokenizer.NextNoJunk();
              Break;
            end;
          tk_kw_Const: begin p.ParType := lptConst; FTokenizer.NextNoJunk(); end;
          tk_kw_Out:   begin p.ParType := lptOut;   FTokenizer.NextNoJunk(); end;
          tk_kw_Var:   begin p.ParType := lptVar;   FTokenizer.NextNoJunk(); end;
          else p.ParType := lptNormal;
        end;

        a := ParseIdentifierList();
        FTokenizer.Expect(tk_sym_Colon, False, False);
        p.VarType := ParseType(nil);
        if (p.VarType = nil) then
          LapeException(lpeTypeExpected, FTokenizer.DocPos);
        FTokenizer.Expect([tk_sym_Equals, tk_sym_SemiColon, tk_sym_ParenthesisClose], True, False);

        if (FTokenizer.Tok = tk_sym_Equals) then
        begin
          with ParseExpression([tk_sym_ParenthesisClose]) do
          try
            p.Default := Evaluate();
            if (p.ParType in [lptVar, lptOut]) and p.Default.isConstant then
              LapeException(lpeVariableExpected, DocPos);
          finally
            Free();
          end;
          FTokenizer.Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
        end
        else
          p.Default := nil;

        for i := 0 to High(a) do
        begin
          if addToScope then
            if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
              LapeException(lpeImpossible, FTokenizer.DocPos)
            else if (LapeCase(a[i]) = LapeCase(Name)) or (getDeclaration(a[i], True) <> nil) then
              LapeException(lpeDuplicateDeclaration, [a[i]], FTokenizer.DocPos)
            else
              FStackInfo.addVar(p.ParType, p.VarType, a[i]);

          Result.addParam(p);
        end;
      until (FTokenizer.Tok = tk_sym_ParenthesisClose);
    end;

    if isFunction then
    begin
      FTokenizer.Expect(tk_sym_Colon, True, False);
      Result.Res := ParseType(nil);
      if (Result.Res = nil) then
        LapeException(lpeTypeExpected, FTokenizer.DocPos);

      if addToScope then
        if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
          LapeException(lpeImpossible, FTokenizer.DocPos)
        else if (LapeCase(Name) = LapeCase('Result')) or (getDeclaration('Result', True) <> nil) then
          LapeException(lpeDuplicateDeclaration, ['Result'], FTokenizer.DocPos)
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
  d := FTokenizer.DocPos;
  try
    if isExternal then
      t := TLapeType_ImportedMethod(ParseMethodHeader(n, False))
    else
      t := TLapeType_ScriptMethod(ParseMethodHeader(n));
    if (n = '') then
      LapeException(lpeBlockExpected, FTokenizer.DocPos);

    FTokenizer.Expect(tk_sym_SemiColon, True, False);
    if (FTokenizer.PeekNoJunk() in [tk_kw_Forward, tk_kw_Overload{, tk_kw_Override}]) then
      FTokenizer.NextNoJunk();

    decl := getDeclaration(n, FStackInfo.Owner);
    if isExternal then
      Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(TLapeType_ImportedMethod(t).NewGlobalVar(nil), FStackInfo.Owner)), FStackInfo, Self, @d)
    else
      Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(TLapeType_ScriptMethod(t).NewGlobalVar(0), FStackInfo.Owner)), FStackInfo, Self, @d);

    try
      if (FTokenizer.Tok = tk_kw_Overload) then
      begin
        FTokenizer.Expect(tk_sym_SemiColon, True, False);

        if (decl = nil) or ((decl is TLapeGlobalVar) and (TLapeGlobalVar(decl).VarType is TLapeType_Method)) then
          with TLapeType_OverloadedMethod(addLocalDecl(TLapeType_OverloadedMethod.Create(Self, nil, '', @d), FStackInfo.Owner)) do
          begin
            if (decl <> nil) then
              addMethod(TLapeGlobalVar(decl));
            decl := addLocalDecl(NewGlobalVar(n, @d), FStackInfo.Owner);
          end
        else if (not (decl is TLapeGlobalVar)) or (not (TLapeGlobalVar(decl).VarType is TLapeType_OverloadedMethod)) or (TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).getMethod(t) <> nil) then
          LapeException(lpeCannotOverload, FTokenizer.DocPos);

        try
          TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).addMethod(Result.Method);
        except on E: lpException do
          LapeException(E.Message, FTokenizer.DocPos);
        end;

        if (FTokenizer.PeekNoJunk() = tk_kw_Forward) then
          FTokenizer.NextNoJunk();
      end
      {else if (FTokenizer.Tok = tk_kw_Override) then
      begin
        FTokenizer.Expect(tk_sym_SemiColon, True, False);

        if (decl <> nil) and (decl is TLapeGlobalVar) and (TLapeGlobalVar(decl).VarType is TLapeType_OverloadedMethod) then
          decl := TLapeType_OverloadedMethod(TLapeGlobalVar(decl).VarType).getMethod(t);
        if (decl = nil) or (not (decl is TLapeGlobalVar)) or (not (TLapeGlobalVar(decl).VarType is TLapeType_Method)) then
          LapeException(lpeUnknownParent, FTokenizer.DocPos);
        if (getDeclaration('inherited', FStackInfo, True) <> nil) then
          LapeException(lpeDuplicateDeclaration, ['inherited'], FTokenizer.DocPos);

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
            LapeException(lpeDuplicateDeclaration, [n], FTokenizer.DocPos)
        end;
        if (decl <> nil) then
          if (FuncForwards <> nil) and FuncForwards.ExistsItem(TLapeGlobalVar(decl)) then
          begin
            if (not TLapeGlobalVar(decl).VarType.Equals(t)) then
              LapeException(lpeNoForwardMatch, FTokenizer.DocPos);
            Result.FreeStackInfo := False;
            Result.Free();

            Result := TLapeTree_Method.Create(TLapeGlobalVar(decl), FStackInfo, Self, @d);
          end
          else
            LapeException(lpeDuplicateDeclaration, [n], FTokenizer.DocPos);

        Result.Method.Name := n;
      end;

      if (FTokenizer.Tok = tk_kw_Forward) then
      begin
        FTokenizer.Expect(tk_sym_SemiColon, True, False);
        if (FuncForwards = nil) then
          LapeException(lpeBlockExpected, FTokenizer.DocPos)
        else if FuncForwards.ExistsItem(Result.Method) then
          LapeException(lpeDuplicateDeclaration, [n], FTokenizer.DocPos)
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
        FTokenizer.Expect(tk_kw_Begin, False, False)
      else
        FTokenizer.Expect(tk_sym_SemiColon, False, False);
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

  procedure ParseArray; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    t: TLapeTree_Base;
    r: TLapeRange;
    d: TDocPos;
  begin
    FTokenizer.Expect(tk_kw_Array, True, False);
    d := FTokenizer.DocPos;

    FTokenizer.Expect([tk_sym_BracketOpen, tk_kw_Of], True, False);
    if (FTokenizer.Tok = tk_sym_BracketOpen) then
    begin
      t := ParseTypeExpression();
      try
        r := EnsureConstantRange(t);
        FTokenizer.Expect(tk_sym_BracketClose, False, False);
        FTokenizer.Expect(tk_kw_Of, True, False);
        Result := addManagedType(TLapeType_StaticArray.Create(r, ParseType(nil), Self, '', @d));
      finally
        if (t <> nil) then
          t.Free();
      end;
    end
    else
      Result := addManagedType(TLapeType_DynArray.Create(ParseType(nil), Self, '', @d));
  end;

  procedure ParseRecord(IsPacked: Boolean = False); {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    rr: TLapeType_Record absolute Result;
    x: TLapeType;
    a: TStringArray;
    i: Integer;
  begin
    FTokenizer.Expect([tk_kw_Record, tk_kw_Union], True, False);
    if (FTokenizer.Tok = tk_kw_Record) then
      rr := TLapeType_Record.Create(Self, nil, '', getPDocPos())
    else
      rr := TLapeType_Union.Create(Self, nil, '', getPDocPos());

    repeat
      a := ParseIdentifierList();
      FTokenizer.Expect(tk_sym_Colon, False, False);
      x := ParseType(nil);
      FTokenizer.Expect(tk_sym_SemiColon, True, False);
      for i := 0 to High(a) do
        if IsPacked then
          rr.addField(x, a[i], 1)
        else
          rr.addField(x, a[i], Options_PackRecords);

      if (FTokenizer.PeekNoJunk() = tk_kw_End) then
      begin
        FTokenizer.Expect(tk_kw_End, True, False);
        Break;
      end;
    until False;

    Result := addManagedType(rr);
  end;

  procedure ParseSet; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    t: TLapeType;
  begin
    FTokenizer.Expect(tk_kw_Set, True, False);
    FTokenizer.Expect(tk_kw_Of, True, False);
    t := ParseType(nil);
    if (not (t is TLapeType_SubRange)) then
      LapeException(lpeInvalidRange, FTokenizer.DocPos);

    try
      Result := addManagedType(TLapeType_Set.Create(TLapeType_SubRange(t), Self, '', GetPDocPos));
    except on E: lpException do
      LapeException(E.Message, FTokenizer.DocPos);
    end;
  end;

  procedure ParsePointer; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    x: TLapeType;
    d: TDocPos;
  begin
    FTokenizer.Expect(tk_sym_Caret, True, False);
    d := FTokenizer.DocPos;

    FTokenizer.Expect(tk_Identifier, True, False);
    x := TLapeType(getDeclaration(FTokenizer.TokString));
    if ((x = nil) and (TypeForwards = nil)) or ((x <> nil) and (not (x is TLapeType))) then
      LapeException(lpeTypeExpected, FTokenizer.DocPos);

    if (x <> nil) then
      Result := addManagedType(TLapeType_Pointer.Create(Self, x, '', @d))
    else
    begin
      Result := TLapeType_Pointer.Create(Self, x, '', @d);
      TypeForwards.add(FTokenizer.TokString, Result);
    end;
  end;

  procedure ParseEnum; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    re: TLapeType_Enum absolute Result;
    n: lpString;
    t: TlapeTree_ExprBase;
    v: TLapeGlobalVar;
  begin
    FTokenizer.Expect(tk_sym_ParenthesisOpen, True, False);
    re := TLapeType_Enum.Create(Self, nil, '', getPDocPos());

    repeat
      FTokenizer.Expect(tk_Identifier, True, False);
      n := FTokenizer.TokString;
      if (getDeclaration(n, FStackInfo.Owner, True) <> nil) then
        LapeException(lpeDuplicateDeclaration, [n], FTokenizer.DocPos);

      FTokenizer.Expect([tk_sym_Comma, tk_sym_ParenthesisClose, tk_sym_Equals], True, False);
      if (FTokenizer.Tok = tk_cmp_Equal) then
      try
        t := ParseExpression([tk_sym_Comma, tk_sym_ParenthesisClose]);
        try
          if (t <> nil) then
            v := t.Evaluate()
          else
            v := nil;

          if (v = nil) or (v.VarType = nil) or (v.VarType.BaseIntType = ltUnknown) or (not v.isConstant) then
            LapeException(lpeExpressionExpected, FTokenizer.DocPos);
          TLapeGlobalVar(addLocalDecl(re.NewGlobalVar(re.addMember(v.AsInteger, n), n), FStackInfo.Owner)).isConstant := True;
        finally
          t.Free();
        end;
      except on E: lpException do
        LapeException(E.Message, FTokenizer.DocPos);
      end
      else
        TLapeGlobalVar(addLocalDecl(re.NewGlobalVar(re.addMember(n), n), FStackInfo.Owner)).isConstant := True;
    until (FTokenizer.Tok in [tk_NULL, tk_sym_ParenthesisClose]);
    Result := addManagedType(re);
  end;

  procedure ParseDef; {$IFDEF Lape_Inline}inline;{$ENDIF}
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
            LapeException(lpeOutOfTypeRange, FTokenizer.DocPos);
        end
      else if (t <> nil) and (t is TLapeTree_VarType) and (TLapeTree_VarType(t).VarType <> nil) then
        Result := TLapeTree_VarType(t).VarType
      else
        LapeException(lpeTypeExpected, FTokenizer.DocPos);
    finally
      if (t <> nil) then
      begin
        FTokenizer.Pos := FTokenizer.TokStart - 1;
        t.Free();
      end;
    end;
  end;

begin
  Result := nil;
  try

    case FTokenizer.PeekNoJunk() of
      tk_kw_Array: ParseArray();
      tk_kw_Record, tk_kw_Union: ParseRecord();
      tk_kw_Set: ParseSet();
      tk_kw_Packed:
        begin
          FTokenizer.NextNoJunk();
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
  TLapeType_Pointer2 = class(TLapeType_Pointer);
procedure TLapeCompiler.ParseTypeBlock;
var
  f: TLapeTypeForwards;
  t: TLapeType;
  s: lpString;
begin
  f := TLapeTypeForwards.Create(nil, {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF}, dupIgnore);
  try
    FTokenizer.Expect(tk_kw_Type, True, False);
    repeat
      FTokenizer.Expect(tk_Identifier, True, False);
      s := FTokenizer.TokString;
      if (getDeclaration(s, True) <> nil) then
        LapeException(lpeDuplicateDeclaration, [s], FTokenizer.DocPos);
      FTokenizer.Expect(tk_sym_Equals, True, False);

      t := ParseType(f).CreateCopy();
      t.Name := s;
      addLocalDecl(t);

      FTokenizer.Expect(tk_sym_SemiColon, True, False);
    until (FTokenizer.PeekNoJunk() <> tk_Identifier);

    while (f.Count > 0) do
      with TLapeType_Pointer2(f.ItemsI[0]) do
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

function TLapeCompiler.ParseVarBlock: TLapeTree_VarList;
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

    FTokenizer.Expect([tk_kw_Const, tk_kw_Var], True, False);
    isConst := (FTokenizer.Tok = tk_kw_Const);
    repeat
      t := nil;
      b := nil;
      v2 := nil;

      a := ParseIdentifierList();
      FTokenizer.Expect([tk_sym_Colon, tk_op_Assign, tk_sym_Equals], False, False);

      if (FTokenizer.Tok = tk_sym_Colon) then
      begin
        t := ParseType(nil);
        if isConst then
          FTokenizer.Expect([tk_op_Assign, tk_sym_Equals], True, False)
        else
          FTokenizer.Expect([tk_op_Assign, tk_sym_Equals, tk_sym_SemiColon], True, False);
      end;

      if (FTokenizer.Tok = tk_sym_Equals) then
      begin
        b := ParseExpression();
        if (b <> nil) and (not b.isConstant()) then
          LapeException(lpeConstantExpected, b.DocPos);
        if (t <> nil) and (b is TLapeTree_OpenArray) then
          TLapeTree_OpenArray(b).ToType := t;

        try
          FTokenizer.Expect(tk_sym_SemiColon, False, False);
          v2 := b.Evaluate();
        finally
          if (b <> nil) then
            FreeAndNil(b);
        end;
      end
      else if (FTokenizer.Tok = tk_op_Assign) then
      begin
        if (Length(a) <> 1) then
          LapeException(lpeDefaultToMoreThanOne, FTokenizer.DocPos);

        b := ParseExpression();
        FTokenizer.Expect(tk_sym_SemiColon, False, False);
      end;

      if (t = nil) then
        if (v2 <> nil) then
          t := v2.VarType
        else if (b <> nil) then
          t := b.resType()
        else
          LapeException(lpeCannotAssign, FTokenizer.DocPos);
      if (t = nil) then
        LapeException(lpeTypeExpected, FTokenizer.DocPos);

      for i := 0 to High(a) do
      begin
        if (getDeclaration(a[i], True) <> nil) then
          LapeException(lpeDuplicateDeclaration, [a[i]], FTokenizer.DocPos);

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

        if isConst and (v is TLapeVar) then
          TLapeVar(v).isConstant := True;
      end;
    until (FTokenizer.PeekNoJunk() <> tk_Identifier);

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

  procedure PopOpNode; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    OpNode: TLapeTree_Operator;
  begin
    OpNode := OpStack.Pop();
    try
      with OpNode do
      begin
        if (OperatorType = op_UnaryPlus) then
          Exit;
        if (not (OperatorType in UnaryOperators)) then
          Right := VarStack.Pop();
        Left := VarStack.Pop();
      end;
      VarStack.Push(OpNode);
    except
      LapeException(lpeInvalidEvaluation, OpNode.DocPos);
    end;
  end;

  procedure PopOpStack(op: EOperator = op_Unknown); {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    Associative: ShortInt;
  begin
    if (OpStack.Cur >= 0) and (op <> op_Unknown) then
    begin
      case OperatorAssociative[op] of
        assocLeft: Associative := 0;
        assocRight: Associative := 1;
        assocNone: LapeException(lpeImpossible, FTokenizer.DocPos);
      end;
      Precedence := OperatorPrecedence[op];

      while (OpStack.Cur >= 0) and (OpStack.Top <> TLapeTree_Operator(ParenthesisOpen)) do
        if (Precedence >= OperatorPrecedence[OpStack.Top.OperatorType] + Associative) then
          PopOpNode()
        else
          Break;
    end;
  end;

  function PushVarStack(Item: TLapeTree_ExprBase): Integer; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (_LastNode = _Var) then
      LapeException(lpeOperatorExpected, FTokenizer.DocPos)
    else
      _LastNode := _Var;
    Result := VarStack.Push(Item);
  end;

  function PushOpStack(Item: TLapeTree_Operator): Integer; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (Item = TLapeTree_Operator(ParenthesisOpen)) or (Item.OperatorType = op_Assign) then
      _LastNode := _None
    else if (_LastNode <> _Var) and (not (Item.OperatorType in UnaryOperators)) then
      LapeException(lpeExpressionExpected, FTokenizer.DocPos)
    else if (Item.OperatorType = op_Deref) then
      _LastNode := _Var
    else
      _LastNode := _Op;
    Result := OpStack.Push(Item);
  end;

  function getString: lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := FTokenizer.TokString;
    if (Length(Result) > 1) then
    begin
      Delete(Result, 1, 1);
      Delete(Result, Length(Result), 1);
    end;
  end;

  procedure ParseAndPushString(ForceString: Boolean = False); {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    s: lpString;
    d: TDocPos;
  begin
    d := FTokenizer.DocPos;
    case FTokenizer.Tok of
      tk_typ_String: s := getString();
      tk_typ_Char: s := FTokenizer.TokChar;
      else LapeException(lpeImpossible);
    end;
    while (FTokenizer.Peek() in [tk_typ_String, tk_typ_Char]) do
    begin
      case FTokenizer.Next() of
        tk_typ_String:
          if (FTokenizer.LastTok = tk_typ_String) then
            s := s + #39 + getString()
          else
            s := s + getString();
        tk_typ_Char: s := s + FTokenizer.TokChar;
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
      while (not (FTokenizer.Tok in [tk_NULL, tk_sym_BracketClose])) do
      begin
        r.addValue(ParseTypeExpression());
        FTokenizer.Expect([tk_sym_Comma, tk_sym_BracketClose], False, False);
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
      if (FTokenizer.Tok = tk_sym_ParenthesisOpen) then
      begin
        PushOpStack(TLapeTree_Operator(ParenthesisOpen));
        Exit;
      end;
    if (op = op_Unknown) then
      op := ParserTokenToOperator(FTokenizer.Tok);

    if (op = op_Index) and (_LastNode <> _Var) then
    begin
      ParseAndPushArray();
      Exit;
    end;

    //Unary minus and double negation
    if (op = op_Minus) then
      if (_LastNode = _None) or
           ((FTokenizer.LastTok in ParserToken_Operators) and
           (OperatorPrecedence[ParserTokenToOperator(FTokenizer.LastTok)] < OperatorPrecedence[op]))
      then
        op := op_UnaryMinus
      else if (FTokenizer.LastTok in [tk_op_Plus, tk_op_Minus]) then
      begin
        case opStack.Top.OperatorType of
          op_Plus: opStack.Top.OperatorType := op_Minus;
          op_Minus: opStack.Top.OperatorType := op_Plus;
          op_UnaryPlus: opStack.Top.OperatorType := op_UnaryMinus;
          op_UnaryMinus: opStack.Top.OperatorType := op_UnaryPlus;
        end;
        Exit;
      end;
    if (op = op_Plus) then
      if (FTokenizer.LastTok in [tk_NULL, tk_sym_ParenthesisOpen, tk_op_Plus, tk_op_Minus]) then
        Exit;

    if (_LastNode <> _Var) and (not (op in UnaryOperators)) then
      LapeException(lpeExpressionExpected, FTokenizer.DocPos);

    PopOpStack(op);
    PushOpStack(TLapeTree_Operator.Create(op, Self, getPDocPos()));

    if (op = op_Dot) then
    begin
      if (FTokenizer.NextNoJunk() <> tk_Identifier) then
        LapeException(lpeExpected, [LapeTokenToString(tk_Identifier)]);
      PushVarStack(TLapeTree_Field.Create(FTokenizer.TokString, Self, getPDocPos()));
    end
    else if (op = op_Index) then
    begin
      PushVarStack(ParseExpression());
      if (FTokenizer.Tok = tk_sym_Comma) then
        ParseOperator(op_Index)
      else
        FTokenizer.Expect(tk_sym_BracketClose, False, False);
    end;
  end;

begin
  Assert(FTokenizer <> nil);

  Result := nil;
  f := nil;
  VarStack := TLapeTree_NodeStack.Create(8);
  OpStack := TLapeTree_OpStack.Create(16);
  _LastNode := _None;
  InExpr := 0;

  try
    while True do
    begin
      if (FTokenizer.NextNoJunk() in ReturnOn) and (InExpr <= 0) then
        Break;
      case FTokenizer.Tok of
        tk_typ_Integer: PushVarStack(TLapeTree_Integer.Create(FTokenizer.TokString, Self, getPDocPos()));
        tk_typ_Integer_Hex: PushVarStack(TLapeTree_Integer.Create(IntToStr(FTokenizer.TokInt64), Self, getPDocPos()));
        tk_typ_Integer_Bin: PushVarStack(TLapeTree_Integer.Create(IntToStr(FTokenizer.TokInt64), Self, getPDocPos()));
        tk_typ_Float: PushVarStack(TLapeTree_Float.Create(FTokenizer.TokString, Self, getPDocPos()));
        tk_typ_String: ParseAndPushString();
        tk_typ_Char:
          begin
            if (FTokenizer.Peek() in [tk_typ_String, tk_typ_Char]) then
              ParseAndPushString()
            else
              PushVarStack(TLapeTree_Char.Create(FTokenizer.TokChar, Self, getPDocPos()));
          end;

        tk_Identifier:
          begin
            v := getDeclaration(FTokenizer.TokString);

            if (v <> nil) and (v is TLapeGlobalVar) then
              PushVarStack(TLapeTree_GlobalVar.Create(TLapeGlobalVar(v), Self, getPDocPos()))
            else if (v <> nil) and (v is TLapeVar) then
              PushVarStack(TlapeTree_ResVar.Create(getResVar(TLapeVar(v)), Self, getPDocPos()))
            else if (v <> nil) and (v is TLapeType) then
              PushVarStack(TLapeTree_VarType.Create(TLapeType(v), Self, getPDocPos()))
            else if (FInternalMethodMap[FTokenizer.TokString] <> nil) then
            begin
              f := FInternalMethodMap[FTokenizer.TokString].Create(Self, getPDocPos());
              if (FTokenizer.PeekNoJunk() = tk_sym_ParenthesisOpen) then
                _LastNode := _Var
              else
              begin
                VarStack.Push(f);
                f := nil;
              end;
            end
            else
              LapeException(lpeUnknownDeclaration, [FTokenizer.TokString], FTokenizer.DocPos);
          end;

        tk_sym_ParenthesisOpen:
          begin
            if (_LastNode = _Var) then
            begin
              PopOpStack(op_Invoke);
              if (f = nil) then
                f := TLapeTree_Invoke.Create(VarStack.Pop(), Self, getPDocPos());
              if (FTokenizer.PeekNoJunk() = tk_sym_ParenthesisClose) then
                FTokenizer.NextNoJunk()
              else
              begin
                f.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma])));
                while True do
                  case FTokenizer.Tok of
                    tk_sym_ParenthesisClose: Break;
                    tk_sym_Comma: f.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma])));
                    else
                      LapeException(lpeClosingParenthesisExpected, FTokenizer.DocPos);
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
              LapeException(lpeLostClosingParenthesis, FTokenizer.DocPos);
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
        LapeException(lpeClosingParenthesisExpected, FTokenizer.DocPos);

      PopOpNode();
    end;

    if (VarStack.Cur <> 0) then
      if (VarStack.Cur < 0) and (FTokenizer.Tok in ReturnOn) then
        Exit(nil)
      else
        LapeException(lpeInvalidEvaluation, FTokenizer.DocPos);

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
  if (FTokenizer.Tok <> tk_sym_DotDot) then
    Result := r
  else
  try
    Result := TLapeTree_Range.Create(Self, getPDocPos());
    TLapeTree_Range(Result).Lo := r;
    TLapeTree_Range(Result).Hi := ParseExpression(ReturnOn);

    if (r <> nil) and (TLapeTree_Range(Result).Hi <> nil) then
      v := TLapeTree_Range(Result).Hi.resType();
    if (v = nil) or (not (v.CompatibleWith(r.resType()))) then
      LapeException(lpeInvalidRange, FTokenizer.DocPos);
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseStatement: TLapeTree_Base;
begin
  case FTokenizer.PeekNoJunk() of
    tk_NULL, tk_kw_End, tk_kw_Finally, tk_kw_Except, tk_kw_Until: Result := nil;

    tk_kw_Begin: Result := ParseBeginEnd();
    tk_kw_Case: Result := ParseCase();
    tk_kw_For: Result := ParseFor();
    tk_kw_If: Result := ParseIf();
    tk_kw_Repeat: Result := ParseRepeat();
    tk_kw_While: Result := ParseWhile();
    tk_kw_Try: Result := ParseTry();

    tk_sym_SemiColon, tk_kw_Else:
      begin
        //if (not (FTokenizer.Tok in [tk_sym_SemiColon, tk_kw_Else])) then
          FTokenizer.NextNoJunk();
        Result := nil;
      end;
    else
    begin
      Result := ParseExpression();
      try
        FTokenizer.Expect([tk_sym_SemiColon, tk_kw_Else], False, False);
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
        FTokenizer.Expect(tk_sym_SemiColon, False, False);
      end;
    until (t = nil);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseBeginEnd(AllowDot: Boolean = False): TLapeTree_StatementList;
begin
  FTokenizer.Expect(tk_kw_Begin, True, False);
  Result := ParseStatementList();
  FTokenizer.Expect(tk_kw_End, True, False);

  if AllowDot then
    FTokenizer.Expect([tk_sym_SemiColon, tk_kw_Else, tk_sym_Dot], True, False)
  else
    FTokenizer.Expect([tk_sym_SemiColon, tk_kw_Else], True, False);
end;

function TLapeCompiler.ParseCase: TLapeTree_Case;
var
  t: TLapeTree_Base;
  Field: TLapeTree_MultiIf;
begin
  FTokenizer.Expect(tk_kw_Case, True, False);
  Result := TLapeTree_Case.Create(Self, getPDocPos());
  t := nil;
  Field := nil;
  try
    Result.Condition := ParseExpression();
    FTokenizer.Expect(tk_kw_Of, False, False);

    while (not (FTokenizer.PeekNoJunk() in [tk_Null, tk_kw_Else, tk_kw_End])) do
    begin
      t := ParseTypeExpression();
      Field := TLapeTree_MultiIf.Create(nil, Self, @t.DocPos);
      repeat
        Field.addValue(t);
        t := nil;
        FTokenizer.Expect([tk_sym_Comma, tk_sym_Colon], False, False);
        if (FTokenizer.Tok = tk_sym_Colon) then
          Break
        else
          t := ParseTypeExpression();
      until False;
      Field.Body := ParseStatement();
      Result.addField(Field);
      Field := nil;
    end;

    FTokenizer.Expect([tk_kw_Else, tk_kw_End], True, False);
    if (FTokenizer.Tok = tk_kw_Else) then
    begin
      Result.ElseBody := ParseStatement();
      FTokenizer.Expect(tk_kw_End, True, False);
    end;

    FTokenizer.Expect([tk_sym_SemiColon, tk_kw_Else], True, False);
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
  FTokenizer.Expect(tk_kw_For, True, False);
  Result := TLapeTree_For.Create(Self, getPDocPos());
  try

    Result.Counter := ParseExpression();
    FTokenizer.Expect([tk_kw_To, tk_kw_DownTo], False, False);
    if (FTokenizer.Tok = tk_kw_DownTo) then
      Result.WalkDown := True;
    Result.Limit := ParseExpression();
    FTokenizer.Expect([tk_kw_Step, tk_kw_Do], False, False);
    if (FTokenizer.Tok = tk_kw_Step) then
    begin
      Result.Step := ParseExpression();
      FTokenizer.Expect(tk_kw_Do, False, False);
    end;
    Result.Body := ParseStatement();
    if (FTokenizer.Tok = tk_kw_Else) then
      Result.ElseBody := ParseStatement();

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseIf: TLapeTree_If;
begin
  FTokenizer.Expect(tk_kw_If, True, False);
  Result := TLapeTree_If.Create(Self, getPDocPos());

  try

    Result.Condition := ParseExpression();
    FTokenizer.Expect(tk_kw_Then, False, False);
    Result.Body := ParseStatement();
    if (FTokenizer.Tok = tk_kw_Else) then
      Result.ElseBody := ParseStatement();

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseRepeat: TLapeTree_Repeat;
begin
  FTokenizer.Expect(tk_kw_Repeat, True, False);
  Result := TLapeTree_Repeat.Create(Self, getPDocPos());

  try

    Result.Body := ParseStatementList();
    FTokenizer.Expect(tk_kw_Until, True, False);
    Result.Condition := ParseExpression();
    FTokenizer.Expect([tk_sym_SemiColon, tk_kw_Else], False, False);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseTry: TLapeTree_Try;
begin
  FTokenizer.Expect(tk_kw_Try, True, False);
  Result := TLapeTree_Try.Create(Self, getPDocPos());
  try

    Result.Body := ParseStatementList();
    FTokenizer.Expect([tk_kw_Except, tk_kw_Finally], True, False);
    if (FTokenizer.Tok = tk_kw_Except) then
    begin
      Result.ExceptBody := ParseStatementList();
      FTokenizer.Expect([tk_kw_Finally, tk_kw_End], True, False);
    end;
    if (FTokenizer.Tok = tk_kw_Finally) then
    begin
      Result.FinallyBody := ParseStatementList();
      FTokenizer.Expect(tk_kw_End, True, False);
    end;
    FTokenizer.Expect([tk_sym_SemiColon, tk_kw_Else], True, False);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseWhile: TLapeTree_While;
begin
  FTokenizer.Expect(tk_kw_While, True, False);
  Result := TLapeTree_While.Create(Self, getPDocPos());

  try

    Result.Condition := ParseExpression();
    FTokenizer.Expect(tk_kw_Do, False, False);
    Result.Body := ParseStatement();
    if (FTokenizer.Tok = tk_kw_Else) then
      Result.ElseBody := ParseStatement();

  except
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
    FTree := ParseBlockList(False);
    FTree.Compile();
    DecStackInfo(False, True, True);
    FEmitter._op(ocNone);
    Result := True;

  except
    Reset();
    raise;
  end;
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
  t: TLapeStackInfo;
  p: TLapeTokenizerBase;
begin
  s := 'var ' + AName + ': ' + Typ;
  if (Value <> '') then
   s := s + ' = ' + Value;

  t := FStackInfo;
  p := FTokenizer;
  FTokenizer := TLapeTokenizerString.Create(s + ';');
  try
    FStackInfo := nil;
    ParseVarBlock().Free();
    Result := FGlobalDeclarations.Items[FGlobalDeclarations.Items.Count - 1] as TLapeGlobalVar;
  finally
    FTokenizer.Free();
    FTokenizer := p;
    FStackInfo := t;
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

function TLapeCompiler.addGlobalVar(Value: Boolean; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Boolean(FBaseTypes[ltBoolean]).NewGlobalVar(Ord(Value)), AName);
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
  t: TLapeStackInfo;
  p: TLapeTokenizerBase;
begin
  t := FStackInfo;
  p := FTokenizer;
  FTokenizer := TLapeTokenizerString.Create('type ' + AName + ' = ' + s + ';');
  try
    FStackInfo := nil;
    ParseTypeBlock();
    Result := FGlobalDeclarations.Items[FGlobalDeclarations.Items.Count - 1] as TLapeType;
  finally
    FTokenizer.Free();
    FTokenizer := p;
    FStackInfo := t;
  end;
end;

function TLapeCompiler.addGlobalFunc(s: lpString; Value: Pointer): TLapeGlobalVar;
var
  t: TLapeStackInfo;
  p: TLapeTokenizerBase;
  f: TLapeTree_Method;
begin
  Result := nil;
  t := FStackInfo;
  p := FTokenizer;
  FTokenizer := TLapeTokenizerString.Create(s + ';');
  try
    FStackInfo := nil;
    f := ParseMethod(nil, True);
    try
      if (f.Method = nil) or (f.Method.VarType = nil) or (not (f.Method.VarType is TLapeType_ImportedMethod)) then
        LapeException(lpeInvalidEvaluation);

      Result := f.Method;
      PPointer(Result.Ptr)^ := Value;
    finally
      FreeAndNil(f);
    end;
  finally
    FTokenizer.Free();
    FTokenizer := p;
    FStackInfo := t;
  end;
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of TLapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_ImportedMethod(addManagedType(TLapeType_ImportedMethod.Create(Self, AParams, AParTypes, AParDefaults, ARes))).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of TLapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalFunc(AParams, AParTypes, AParDefaults, nil, Value, AName);
end;

end.

