{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Syntax tree objects (and turning it into bytecode).
}
unit lptree;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpparser;

type
  TLapeTree_Base = class;
  TLapeTree_ExprBase = class;
  TLapeExpressionList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_ExprBase>;
  TLapeStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_Base>;

  TLapeTree_Base = class(TLapeBaseClass)
  protected
    FParent: TLapeTree_Base;
    FCompiler: TLapeCompilerBase;

    procedure setParent(Parent: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); virtual;
  public
    DocPos: TDocPos;

    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload; virtual;
    destructor Destroy; override;

    function Compile(var Offset: Integer): TResVar; overload; virtual; abstract;
    function Compile: TResVar; overload; virtual;

    property Parent: TLapeTree_Base read FParent write setParent;
    property Compiler: TLapeCompilerBase read FCompiler;
  end;

  TLapeTree_ExprBase = class(TLapeTree_Base)
  public
    function isConstant: Boolean; virtual;
    function resType: TLapeType; virtual;
    function Evaluate: TLapeGlobalVar; virtual; abstract;
  end;

  TLapeTree_DestExprBase = class(TLapeTree_ExprBase)
  protected
    FDest: TResVar;
    procedure setDest(ResVar: TResVar); virtual;
  public
    property Dest: TResVar read FDest write setDest;
  end;

  TLapeTree_OpenArray = class(TLapeTree_DestExprBase)
  protected
    FValues: TLapeStatementList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    ToType: TLapeType;

    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    destructor Destroy; override;
    function addValue(p: TLapeTree_Base): Integer; virtual;

    function canCast: Boolean; virtual;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Values: TLapeStatementList read FValues;
  end;

  TLapeTree_Invoke = class(TLapeTree_DestExprBase)
  protected
    FIdent: TLapeTree_ExprBase;
    FParams: TLapeExpressionList;
    procedure setIdent(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
    function getParamTypes: TLapeTypeArray; virtual;
    function getParamTypesStr: lpString; virtual;
  public
    constructor Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    destructor Destroy; override;

    function addParam(p: TLapeTree_ExprBase): Integer; virtual;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Ident: TLapeTree_ExprBase read FIdent write setIdent;
    property Params: TLapeExpressionList read FParams;
  end;

  TLapeTree_Operator = class(TLapeTree_DestExprBase)
  protected
    FOperatorType: EOperator;
    FLeft: TLapeTree_ExprBase;
    FRight: TLapeTree_ExprBase;

    procedure setLeft(Node: TLapeTree_ExprBase); virtual;
    procedure setRight(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    destructor Destroy; override;

    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property OperatorType: EOperator read FOperatorType write FOperatorType;
    property Left: TLapeTree_ExprBase read FLeft write setLeft;
    property Right: TLapeTree_ExprBase read FRight write setRight;
  end;

  TLapeTree_ResVar = class(TLapeTree_ExprBase)
  protected
    FResVar: TResVar;
  public
    constructor Create(AResVar: TResVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;

    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property ResVar: TResVar read FResVar;
  end;

  TLapeTree_GlobalVar = class(TLapeTree_ExprBase)
  protected
    FGlobalVar: TLapeGlobalVar;
    function getVarAsString: lpString; virtual;
  public
    constructor Create(AGlobalVar: TLapeGlobalVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;

    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property GlobalVar: TLapeGlobalVar read FGlobalVar;
    property VarAsString: lpString read getVarAsString;
  end;

  TLapeTree_VarType  = class(TLapeTree_GlobalVar)
  protected
    FVarType: TLapeType;
  public
    constructor Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    property VarType: TLapeType read FVarType;
  end;

  TLapeTree_Integer = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: Integer; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload;
  end;

  TLapeTree_Float = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: Extended; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload;
  end;

  TLapeTree_String = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: AnsiString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    {$IFNDEF Lape_NoWideString}
    constructor Create(AValue: WideString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload;
	{$ENDIF}
    constructor Create(AValue: UnicodeString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload;
  end;
  TLapeTree_Field = class(TLapeTree_String);

  TLapeTree_Char = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: WideChar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
  end;

  TLapeTree_Pointer = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: Pointer; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
  end;

  TLapeTree_Range = class(TLapeTree_Base)
  protected
    FLo: TLapeTree_ExprBase;
    FHi: TLapeTree_ExprBase;

    procedure setLo(Node: TLapeTree_ExprBase); virtual;
    procedure setHi(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    property Lo: TLapeTree_ExprBase read FLo write setLo;
    property Hi: TLapeTree_ExprBase read FHi write setHi;
  end;

  TLapeTree_StatementList = class(TLapeTree_Base)
  protected
    FStatements: TLapeStatementList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addStatement(Statement: TLapeTree_Base): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    property Statements: TLapeStatementList read FStatements;
  end;

  TLapeTree_Method = class(TLapeTree_Base)
  protected
    FMethod: TLapeGlobalVar;
    FStackInfo: TLapeStackInfo;
    FStatements: TLapeTree_StatementList;
    procedure setStatements(Node: TLapeTree_StatementList); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    FreeStackInfo: Boolean;

    constructor Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Method: TLapeGlobalVar read FMethod;
    property StackInfo: TLapeStackInfo read FStackInfo;
    property Statements: TLapeTree_StatementList read FStatements write setStatements;
  end;

  TLapeVarDecl = record
    VarDecl: TLapeVar;
    Default: TLapeTree_ExprBase;
  end;
  TLapeVarList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeVarDecl>;

  TLapeTree_VarList = class(TLapeTree_Base)
  protected
    FVars: TLapeVarList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addVar(AVar: TLapeVarDecl): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    property Vars: TLapeVarList read FVars;
  end;

  TLapeTree_If = class(TLapeTree_Base)
  protected
    FCondition: TLapeTree_ExprBase;
    FBody: TLapeTree_Base;
    FElse: TLapeTree_Base;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure setElse(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
    function CompileBody(var Offset: Integer): TResVar; virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property Body: TLapeTree_Base read FBody write setBody;
    property ElseBody: TLapeTree_Base read FElse write setElse;
  end;

  TLapeTree_MultiIf = class(TLapeTree_If)
  protected
    FValues: TLapeStatementList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(Ident: TLapeTree_ExprBase; OpenArray: TLapeTree_OpenArray; FreeArray: Boolean = True); overload; virtual;
    destructor Destroy; override;

    function addValue(p: TLapeTree_Base): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;
    property Values: TLapeStatementList read FValues;
  end;

  TLapeCaseFieldList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_MultiIf>;
  TLapeTree_Case = class(TLapeTree_Base)
  protected
    FCondition: TLapeTree_ExprBase;
    FElse: TLapeTree_Base;
    FFields: TLapeCaseFieldList;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setElse(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addField(p: TLapeTree_MultiIf): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property ElseBody: TLapeTree_Base read FElse write setElse;
    property Fields: TLapeCaseFieldList read FFields;
  end;

  TLapeTree_While = class(TLapeTree_If)
  protected
    function CompileBody(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_For = class(TLapeTree_While)
  protected
    FCounter: TLapeTree_ExprBase;
    FLimit: TLapeTree_ExprBase;
    FStep: TLapeTree_ExprBase;

    procedure setCounter(Node: TLapeTree_ExprBase); virtual;
    procedure setLimit(Node: TLapeTree_ExprBase); virtual;
    procedure setStep(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    WalkDown: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Counter: TLapeTree_ExprBase read FCounter write setCounter;
    property Limit: TLapeTree_ExprBase read FLimit write setLimit;
    property Step: TLapeTree_ExprBase read FStep write setStep;
  end;

  TLapeTree_Repeat = class(TLapeTree_Base)
  protected
    FCondition: TLapeTree_ExprBase;
    FBody: TLapeTree_Base;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property Body: TLapeTree_Base read FBody write setBody;
  end;

  TLapeTree_Try = class(TLapeTree_Base)
  protected
    FBody: TLapeTree_Base;
    FExcept: TLapeTree_Base;
    FFinally: TLapeTree_Base;

    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure setExcept(Node: TLapeTree_Base); virtual;
    procedure setFinally(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Body: TLapeTree_Base read FBody write setBody;
    property ExceptBody: TLapeTree_Base read FExcept write setExcept;
    property FinallyBody: TLapeTree_Base read FFinally write setFinally;
  end;

  function FoldConstants(Root: TLapeTree_Base): TLapeTree_Base;
  procedure PrintTree(Root: TLapeTree_Base; Indent: Integer = 0);
  function getTempVar(Node: TLapeTree_Base; var Offset: Integer; out v: TresVar; Lock: Integer = 1): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}

implementation

uses
  lpexceptions, lpeval;

function FoldConstants(Root: TLapeTree_Base): TLapeTree_Base;
var
  Replacement: TLapeTree_GlobalVar;
  t: TLapeGlobalVar;
begin
  Result := Root;

  if (Root <> nil) and (Root is TLapeTree_ExprBase) then
  try
    if (Root is TLapeTree_Operator) then
      with TLapeTree_Operator(Root) do
      begin
        FLeft := TLapeTree_ExprBase(FoldConstants(FLeft));
        if (FRight is TLapeTree_OpenArray) then
          TLapeTree_OpenArray(FRight).ToType := FLeft.resType();
        FRight := TLapeTree_ExprBase(FoldConstants(FRight));
      end;

    if (not (Root is TLapeTree_ResVar)) and (not (Root is TLapeTree_GlobalVar)) and TLapeTree_ExprBase(Root).isConstant() then
    begin
      try
        t := TLapeTree_ExprBase(Root).Evaluate();
      except
        t := nil;
      end;

      if (t <> nil) then
      begin
        //Ensure smallest possible integer type
        if t.isConstant and (t.BaseType in LapeIntegerTypes) then
          Replacement := TLapeTree_Integer.Create(t.AsString, Root.Compiler)
        else
          Replacement := TLapeTree_GlobalVar.Create(t, Root.Compiler);
        Replacement.Parent := Root.FParent;
        Root.Free();
        Result := Replacement;
      end;
    end;
  except
  end;
end;

procedure PrintTree(Root: TLapeTree_Base; Indent: Integer = 0);
begin
  Write(StringOfChar('-', Indent)+Root.ClassName);
  if (Root is TLapeTree_GlobalVar) then
    WriteLn('('+TLapeTree_GlobalVar(Root).GlobalVar.VarType.AsString+': '+TLapeTree_GlobalVar(Root).VarAsString+')')
  else
    WriteLn('');
  if (Root is TLapeTree_Operator) then
  begin
    WriteLn(StringOfChar('-', Indent)+'Operation: '+LapeOperatorToString(TLapeTree_Operator(Root).OperatorType));
    if (TLapeTree_Operator(Root).Left <> nil) then
    begin
      WriteLn(StringOfChar('-', Indent)+'Left:');
      PrintTree(TLapeTree_Operator(Root).Left, Indent + 1);
    end;
    if (TLapeTree_Operator(Root).Right <> nil) then
    begin
      WriteLn(StringOfChar('-', Indent)+'Right:');
      PrintTree(TLapeTree_Operator(Root).Right, Indent + 1);
    end;
  end;
end;

function getTempVar(Node: TLapeTree_Base; var Offset: Integer; out v: TresVar; Lock: Integer = 1): Boolean;
begin
  if (Node is TLapeTree_DestExprBase) then
    TLapeTree_DestExprBase(Node).Dest := VarResVar;
  v := Node.Compile(Offset);
  Result := (v.VarPos.MemPos <> mpStack) and (v.VarType <> nil);
  if Result and (Lock > 0) and (v.VarPos.MemPos = mpVar) and (v.VarPos.StackVar <> nil) and (v.VarPos.StackVar is TLapeStackTempVar) then
    TLapeStackTempVar(v.VarPos.StackVar).IncLock(Lock);
end;

procedure TLapeTree_Base.setParent(Parent: TLapeTree_Base);
begin
  Assert((Parent = nil) or (Parent.Compiler = FCompiler));
  if (FParent <> nil) then
    FParent.DeleteChild(Self);
  FParent := Parent;
end;

procedure TLapeTree_Base.DeleteChild(Node: TLapeTree_Base);
begin
  {nothing}
end;

constructor TLapeTree_Base.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create();
  Assert(ACompiler <> nil);

  FParent := nil;
  FCompiler := ACompiler;
  if (ADocPos <> nil) then
    DocPos := ADocPos^
  else
    FillChar(DocPos, SizeOf(TDocPos), 0);
end;

destructor TLapeTree_Base.Destroy;
begin
  setParent(nil);
  inherited;
end;

function TLapeTree_Base.Compile: TResVar;
var o: Integer;
begin
  o := -1;
  Result := Compile(o);
end;

function TLapeTree_ExprBase.isConstant: Boolean;
begin
  Result := False;
end;

function TLapeTree_ExprBase.resType: TLapeType;
begin
  Result := nil;
end;

procedure TLapeTree_DestExprBase.setDest(ResVar: TResVar);
begin
  setNullResVar(FDest, 1);
  FDest := ResVar;
end;

procedure TLapeTree_OpenArray.DeleteChild(Node: TLapeTree_Base);
begin
  inherited;
  if (FValues <> nil) then
    FValues.DeleteItem(Node);
end;

constructor TLapeTree_OpenArray.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  ToType := nil;
  FValues := TLapeStatementList.Create(nil, dupIgnore);
end;

destructor TLapeTree_OpenArray.Destroy;
var
  i: Integer;
begin
  for i := FValues.Count - 1 downto 0 do
    if (FValues[i] <> nil) and (FValues[i].Parent = Self) then
      FValues[i].Free();
  FreeAndNil(FValues);
  inherited;
end;

function TLapeTree_OpenArray.addValue(p: TLapeTree_Base): Integer;
begin
  if (p <> nil) then
  begin
    Result := FValues.add(p);
    p.Parent := Self;
  end;
end;

function TLapeTree_OpenArray.canCast: Boolean;
var
  i: Integer;
  t: TLapeType;
begin
  if (ToType = nil) and (FDest.VarType <> nil) then
    ToType := FDest.VarType;
  if (ToType = nil) then
    ToType := resType();
  if (ToType = nil) then
    Exit(False);

  t := nil;
  if (ToType is TLapeType_Set) then
    t := TLapeType_Set(ToType).Range
  else if (ToType is TLapeType_DynArray) then
    t := TLapeType_DynArray(ToType).PType;
  if (t = nil) then
    Exit(False);

  for i := 0 to FValues.Count - 1 do
  begin
    if (FValues[i] is TLapeTree_OpenArray) then
      TLapeTree_OpenArray(FValues[i]).ToType := t;

    if (FValues[i] is TLapeTree_ExprBase) and t.CompatibleWith(TLapeTree_ExprBase(FValues[i]).resType()) then
      {nothing}
    else if (FValues[i] is TLapeTree_Range) and t.CompatibleWith(TLapeTree_Range(FValues[i]).Lo.resType()) and t.CompatibleWith(TLapeTree_Range(FValues[i]).Hi.resType()) then
      {nothing}
    else
      Exit(False);
  end;
  Result := True;
end;

function TLapeTree_OpenArray.isConstant: Boolean;
var
  i: Integer;
begin
  if (not canCast()) then
    Exit(False);
  for i := 0 to FValues.Count - 1 do
    if (FValues[i] is TLapeTree_ExprBase) and TLapeTree_ExprBase(FValues[i]).isConstant() then
      {nothing}
    else if (FValues[i] is TLapeTree_Range) and TLapeTree_Range(FValues[i]).Lo.isConstant() and TLapeTree_Range(FValues[i]).Hi.isConstant() then
      {nothing}
    else
      Exit(False);
  Result := True;
end;

function TLapeTree_OpenArray.resType: TLapeType;
var
  i: Integer;
  t: TLapeType;
  r: TLapeRange;

  function determineArrType(v: TLapeType): TLapeType;
  begin
    if (v = nil) then
      Exit(nil)
    else if (v.BaseType in LapeIntegerTypes) then
      Result := FCompiler.getBaseType(ltInt32)
    else if (v.BaseType in LapeRealTypes) then
      Result := FCompiler.getBaseType(ltExtended)
    else if (v.BaseType in LapeBoolTypes) then
      Result := FCompiler.getBaseType(ltBoolean)
    else if (v.BaseType in LapeStringTypes) then
      Result := FCompiler.getBaseType(ltString)
    else
      Result := v;
  end;

begin
  Result := ToType;
  if (Result = nil) and (FValues.Count > 0) then
  begin
    r.Lo := 0;
    r.Hi := FValues.Count - 1;

    for i := 0 to r.Hi do
    begin
      if (FValues[i] is TLapeTree_Range) and (TLapeTree_Range(FValues[i]).Hi <> nil) then
      begin
        t := TLapeTree_Range(FValues[i]).Hi.resType();
        if (t = nil) or (not (t is TLapeType_SubRange)) or ((Result <> nil) and (not t.CompatibleWith(Result))) then
          Exit(nil);
        Exit(FCompiler.addManagedType(TLapeType_Set.Create(TLapeType_SubRange(t), FCompiler, '', @DocPos)))
      end
      else if (not (FValues[i] is TLapeTree_ExprBase)) then
        Exit(nil)
      else
        t := determineArrType(TLapeTree_ExprBase(FValues[i]).resType());

      if (Result = nil) then
        Result := t
      else if (t <> nil) and t.Equals(Result) then
        {nothing}
      else if (t <> nil) and (t.BaseType > Result.BaseType) and t.CompatibleWith(Result) then
        Result := t
      else if (t <> nil) and (Result.BaseType >= t.BaseType) and Result.CompatibleWith(t) then
        {nothing}
      else
      begin
        Result := nil;
        Break;
      end;
    end;

    if (Result <> nil) then
      if (Result is TLapeType_Enum) then
        Result := FCompiler.addManagedType(TLapeType_Set.Create(TLapeType_Enum(Result), FCompiler, '', @DocPos))
      else
        Result := FCompiler.addManagedType(TLapeType_StaticArray.Create(r, Result, FCompiler, '', @DocPos));
  end;
end;

function TLapeTree_OpenArray.Evaluate: TLapeGlobalVar;
var
  i, c, tmp: Integer;
  t: TLapeType;
  counter, v, r: TLapeGlobalVar;
begin
  if (not canCast()) then
    LapeException(lpeInvalidEvaluation, DocPos);

  Result := ToType.NewGlobalVarP();
  v := nil;

  try
    if (ToType is TLapeType_Set) then
    begin
      for i := 0 to FValues.Count - 1 do
        if (FValues[i] is TLapeTree_ExprBase) then
        begin
          r := Result;
          Result := ToType.EvalConst(op_Plus, r, TLapeTree_ExprBase(FValues[i]).Evaluate());
          r.Free();
        end
        else if (FValues[i] is TLapeTree_Range) then
        try
          v := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@c);
          for c := TLapeTree_Range(FValues[i]).Lo.Evaluate().AsInteger to TLapeTree_Range(FValues[i]).Hi.Evaluate().AsInteger do
          begin
            r := Result;
            Result := ToType.EvalConst(op_Plus, Result, v);
            r.Free();
          end;
        finally
          v.Free();
        end
        else
          LapeException(lpeInvalidCast, DocPos);
    end
    else if (ToType is TLapeType_StaticArray) then
    try
      tmp := TLapeType_StaticArray(ToType).Range.Lo;
      counter := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@tmp);

      for i := 0 to FValues.Count - 1 do
      begin
        if (FValues[i] is TLapeTree_ExprBase) then
        try
          try
            v := ToType.EvalConst(op_Index, Result, counter);
            v.VarType.EvalConst(op_Assign, v, TLapeTree_ExprBase(FValues[i]).Evaluate())
          except on E: lpException do
            LapeException(E.Message, FValues[i].DocPos);
          end;
        finally
          if (v <> nil) then
            FreeAndNil(v);
        end
        else if (FValues[i] is TLapeTree_Range) then
        try
          r := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@c);
          for c := TLapeTree_Range(FValues[i]).Lo.Evaluate().AsInteger to TLapeTree_Range(FValues[i]).Hi.Evaluate().AsInteger do
          try
            try
              v := ToType.EvalConst(op_Index, Result, counter);
              v.VarType.EvalConst(op_Assign, v, r);
            except on E: lpException do
              LapeException(E.Message, FValues[i].DocPos);
            end;
          finally
            Inc(tmp);
            if (v <> nil) then
              FreeAndNil(v);
          end;
        finally
          Dec(tmp);
          r.Free();
        end
        else
          LapeException(lpeInvalidCast, FValues[i].DocPos);
        Inc(tmp);
      end;
    finally
      counter.Free();
      if (tmp <> TLapeType_StaticArray(ToType).Range.Hi - TLapeType_StaticArray(ToType).Range.Lo + 1) then
        LapeException(lpeInvalidRange, DocPos);
    end
    else
      LapeException(lpeInvalidCast, DocPos);

    Result := TLapeGlobalVar(FCompiler.AddManagedVar(Result));
  except
    Result.Free();
    raise;
  end;
end;

function TLapeTree_OpenArray.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  t: TLapeType;
  v: TResVar;
  c: TLapeVar;
begin
  if (not canCast()) then
    LapeException(lpeInvalidEvaluation, DocPos);

  v := NullResVar;
  if (FDest.VarType <> nil) and (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and ToType.Equals(FDest.VarType) then
    Result := FDest
  else
  begin
    FDest := NullResVar;
    if (ToType is TLapeType_DynArray) then
    begin
      Result := getResVar(FCompiler.getTempVar(ToType));
      Result.VarPos.StackVar.isConstant := False;
    end
    else
    begin
      Result := StackResVar;
      Result.VarType := ToType;
    end;
  end;

  if (ToType is TLapeType_Set) then
  begin
    for i := FValues.Count - 1 downto 0 do
      if (FValues[i] is TLapeTree_ExprBase) then
        with TLapeTree_Operator.Create(op_Plus, FCompiler, @FValues[i].DocPos) do
        try
          Dest := Result;
          Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i].DocPos);
          Right := TLapeTree_ExprBase(FValues[i]);
          Result := Compile(Offset);
        finally
          Free();
        end
      else if (FValues[i] is TLapeTree_Range) then
      begin
        c := FCompiler.getTempVar(TLapeType_Set(ToType).Range, 2);
        c.isConstant := False;
        v := c.VarType.Eval(op_Assign, v, GetResVar(c), TLapeTree_Range(FValues[i]).Lo.Compile(Offset), Offset, @FValues[i].DocPos);

        with TLapeTree_For.Create(FCompiler, @FValues[i].DocPos) do
        try
          Counter := TLapeTree_ResVar.Create(v, FCompiler, @FValues[i].DocPos);
          Limit := TLapeTree_Range(FValues[i]).Hi;
          Body := TLapeTree_Operator.Create(op_Plus, FCompiler, @FValues[i].DocPos);
          with TLapeTree_Operator(Body) do
          begin
            Dest := Result;
            Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i].DocPos);
            Right := TLapeTree_ResVar.Create(v, FCompiler, @FValues[i].DocPos);
          end;
          Compile(Offset);
        finally
          Free();
          c.isConstant := True;
          SetNullResVar(v, 2);
        end;
      end
      else
        LapeException(lpeInvalidCast, FValues[i].DocPos);
  end
  else if (ToType is TLapeType_StaticArray) then
  begin
    if (FValues.Count <> TLapeType_StaticArray(ToType).Range.Hi - TLapeType_StaticArray(ToType).Range.Lo + 1) then
      LapeException(lpeInvalidRange, DocPos);

    for i := FValues.Count - 1 downto 0 do
      if (not (FValues[i] is TLapeTree_ExprBase)) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else
      with TLapeTree_Operator.Create(op_Assign, FCompiler, @FValues[i].DocPos) do
      try
        Left := TLapeTree_Operator.Create(op_Index, FCompiler, @FValues[i].DocPos);
        with TLapeTree_Operator(Left) do
        begin
          Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i].DocPos);
          Right := TLapeTree_Operator.Create(op_Minus, FCompiler, @FValues[i].DocPos);
          with TLapeTree_Operator(Right) do
          begin
            Left := TLapeTree_ResVar.Create(getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(DetermineIntType(i)).NewGlobalVarStr(IntToStr(i)))), FCompiler, @FValues[i].DocPos);
            Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(DetermineIntType(TLapeType_StaticArray(ToType).Range.Lo)).NewGlobalVarStr(IntToStr(TLapeType_StaticArray(ToType).Range.Lo)))), FCompiler, @FValues[i].DocPos);
          end;
        end;
        Right := TLapeTree_ExprBase(FValues[i]);
        Compile(Offset);
      finally
        Free();
      end;
  end
  else
    LapeException(lpeInvalidCast, DocPos);

  if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) and (Result.VarPos.MemPos = mpVar) then
    Result.VarPos.StackVar.isConstant := (Result.VarPos.StackVar is TLapeStackTempVar);
end;

procedure TLapeTree_Invoke.setIdent(Node: TLapeTree_ExprBase);
begin
  if (FIdent <> nil) and (FIdent <> Node) then
    FIdent.Free();
  FIdent := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Invoke.DeleteChild(Node: TLapeTree_Base);
begin
  if (FIdent = Node) then
    FIdent := nil
  else
    FParams.DeleteItem(TLapeTree_ExprBase(Node));
end;

function TLapeTree_Invoke.getParamTypes: TLapeTypeArray;
var
  i: Integer;
begin
  SetLength(Result, FParams.Count);
  for i := 0 to FParams.Count - 1 do
    if (FParams[i] = nil) or (FParams[i].ClassType = TLapeTree_ExprBase) then
      Result[i] := nil
    else
      Result[i] := FParams[i].resType();
end;

function TLapeTree_Invoke.getParamTypesStr: lpString;
var
  i: Integer;
  v: TLapeType;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    if (FParams[i] = nil) or (FParams[i].ClassType = TLapeTree_ExprBase) then
      v := nil
    else
      v := FParams[i].resType();
    if (v <> nil) then
      Result := Result + v.AsString
    else
      Result := Result + '*unknown*';
  end;
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  setIdent(Ident);
  FParams := TLapeExpressionList.Create(nil, dupIgnore);
end;

destructor TLapeTree_Invoke.Destroy;
var
  i: Integer;
begin
  for i := FParams.Count - 1 downto 0 do
    if (FParams[i] <> nil) and (FParams[i].Parent = Self) then
      FParams[i].Free();
  FreeAndNil(FParams);
  setIdent(nil);

  inherited;
end;

function TLapeTree_Invoke.addParam(p: TLapeTree_ExprBase): Integer;
begin
  Result := FParams.add(p);
  if (p <> nil) then
    p.Parent := Self;
end;

function TLapeTree_Invoke.resType: TLapeType;
var
  f: TLapeGlobalVar;
begin
  if (FIdent is TLapeTree_VarType) then
    Result := TLapeTree_VarType(FIdent).VarType
  else
  begin
    f := FIdent.Evaluate();
    if (f <> nil) and (f.VarType <> nil) and (f.VarType is TLapeType_OverloadedMethod) then
      f := TLapeType_OverloadedMethod(f.VarType).getMethod(getParamTypes());
    if (f = nil) or (f.VarType = nil) or (not (f.VarType.BaseType in [ltProc, ltExternalProc])) then
      Result := nil
    else
      Result := TLapeType_Method(f.VarType).Res;
  end;
end;

function TLapeTree_Invoke.Evaluate: TLapeGlobalVar;
var
  f: TLapeGlobalVar;

  function DoCast: TLapeGlobalVar;
  var
    t: TLapeGlobalVar;
  begin
    Result := nil;
    Assert(FIdent is TLapeTree_VarType);

    with TLapeTree_VarType(FIdent) do
    try
      if (FParams.Count <> 1) or (VarType = nil) then
        LapeException(lpeInvalidCast);
      if (FParams[0] is TLapeTree_OpenArray) then
        TLapeTree_OpenArray(FParams[0]).ToType := VarType;

      Result := FParams[0].Evaluate();
      if VarType.Equals(Result.VarType) or ((Result.VarType <> nil) and (VarType.Size = Result.VarType.Size)) then
      begin
        t := Result.VarType.EvalConst(op_Assign, Result.VarType.NewGlobalVarP(), Result);
        try
          t.DoManage := False;
          Result := TLapeGlobalVar(FCompiler.addManagedVar(VarType.NewGlobalVarP(t.Ptr)));
          Result.DoManage := True;
        finally
          t.Free();
        end;
      end
      else if VarType.CompatibleWith(Result.VarType) then
        Result := TLapeGlobalVar(FCompiler.addManagedVar(VarType.EvalConst(op_Assign, VarType.NewGlobalVarP(), Result)))
      else
        LapeException(lpeInvalidCast);
    except on E: lpException do
      LapeException(E.Message, Self.DocPos);
    end;
  end;

  function DoExternalMethod(f: TLapeGlobalVar): TLapeGlobalVar;
  var
    i: Integer;
    t: TLapeGlobalVar;
    a: array of Pointer;
  begin
    Assert(f <> nil);
    Assert(f.VarType is TLapeType_ExternalMethod);
    Result := nil;

    with TLapeType_ExternalMethod(f.VarType) do
    begin
      if (Res = nil) then
        LapeException(lpeCannotEvalConstProc, FIdent.DocPos);
      if (FParams.Count > Params.Count) then
        if (FParams.Count > 0) then
          LapeException(lpeTooMuchParameters, FParams[Params.Count].DocPos)
        else
          LapeException(lpeTooMuchParameters, Self.DocPos);

      SetLength(a, FParams.Count);
      for i := 0 to FParams.Count - 1 do
      begin
        if (i > FParams.Count) or (FParams[i] = nil) or (FParams[i].ClassType = TLapeTree_ExprBase) then
          if (Params[i].Default <> nil) then
            t := Params[i].Default
          else if (FParams[i] <> nil) then
            LapeException(lpeNoDefaultForParam, [i + 1], FParams[i].DocPos)
          else
            LapeException(lpeNoDefaultForParam, [i + 1], Self.DocPos)
        else
        begin
          if (FParams[i] is TLapeTree_OpenArray) then
            TLapeTree_OpenArray(FParams[i]).ToType := Params[i].VarType;
          t := FParams[i].Evaluate();
        end;

        if (t = nil) or (Params[i].ParType in [lptVar, lptOut]) then
          LapeException(lpeCannotInvoke, FParams[i].DocPos);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(t.VarType)) then
          if (FCompiler <> nil) and Params[i].VarType.CompatibleWith(t.VarType) then
          try
            t := TLapeGlobalVar(FCompiler.addManagedVar(Params[i].VarType.EvalConst(op_Assign, Params[i].VarType.NewGlobalVarP(), t)));
          except on E: lpException do
            LapeException(E.Message, FParams[i].DocPos);
          end
          else
            LapeException(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, t.VarType.AsString], FParams[i].DocPos);

        a[i] := t.Ptr;
      end;

      Result := Res.NewGlobalVarP();
      TLapeCallbackFunc(f.Ptr^)(@a[0], Result.Ptr);
      Result := TLapeGlobalVar(FCompiler.addManagedVar(Result));
    end;
  end;

begin
  Result := nil;
  Assert(FIdent <> nil);

  if (FIdent is TLapeTree_VarType) then
    Result := DoCast()
  else
  begin
    f := FIdent.Evaluate();
    if (f = nil) or (f.VarType = nil) then
      LapeException(lpeCannotInvoke, DocPos);

    if (f.VarType is TLapeType_OverloadedMethod) then
    begin
      f := TLapeType_OverloadedMethod(f.VarType).getMethod(getParamTypes());
      if (f = nil) then
        LapeException(lpeNoOverloadedMethod, [getParamTypesStr()], FIdent.DocPos);
    end
    else if (f.Ptr = nil) or (not (f.VarType.BaseType in [ltExternalProc])) then
      LapeException(lpeCannotInvoke, FIdent.DocPos);

    Result := DoExternalMethod(f);
  end;
end;

function TLapeTree_Invoke.Compile(var Offset: Integer): TResVar;
type
  TResVarArray = array of TResVar;
var
  i: Integer;
  a: TResVar;
  c: TResVarArray;

  function DoCast: TResVar;
  var
    a, b, tmp: TResVar;
  begin
    Assert(FIdent is TLapeTree_VarType);
    Result := NullResVar;
    a := NullResVar;
    b := NullResVar;

    with TLapeTree_VarType(FIdent) do
    try
      if (FParams.Count <> 1) or (VarType = nil) then
        LapeException(lpeInvalidCast);
      if (FParams[0] is TLapeTree_OpenArray) then
        TLapeTree_OpenArray(FParams[0]).ToType := VarType;

      Result := FParams[0].Compile(Offset);
      if VarType.Equals(Result.VarType) or ((Result.VarType <> nil) and (VarType.Size = Result.VarType.Size)) then
      begin
        setNullResVar(FDest);
        Result.VarType := VarType;
      end
      else if VarType.CompatibleWith(Result.VarType) then
      begin
        if (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
          b := FDest
        else
        begin
          b.VarPos.MemPos := mpVar;
          b.VarType := VarType;
          b.VarPos.StackVar := Compiler.getTempVar(b.VarType);
        end;

        tmp := Result;
        Result := VarType.Eval(op_Assign, a, b, Result, Offset, @Self.DocPos);
        setNullResVar(tmp, 1);
      end
      else
        LapeException(lpeInvalidCast);
    except on E: lpException do
      LapeException(E.Message, Self.DocPos);
    end;
  end;

  function DoInternalMethod(a: TResVar; c: TResVarArray): TResVar;
  var
    i: Integer;
    b, e, tmp: TResVar;

    function getStackVar(Node: TLapeTree_Base; var Offset: Integer): TResVar;
    begin
      if (Node is TLapeTree_DestExprBase) then
        TLapeTree_DestExprBase(Node).Dest := StackResVar;
      Result := Node.Compile(Offset);
    end;

  begin
    Assert(a.VarType is TLapeType_InternalMethod);
    Assert(Length(c) = TLapeType_InternalMethod(a.VarType).Params.Count);
    Result := NullResVar;
    e := NullResVar;

    with TLapeType_InternalMethod(a.VarType) do
    begin
      FCompiler.Emitter._InitStack(ParamSize, Offset, @Self.DocPos);
      for i := 0 to Params.Count - 1 do
      try
        b := NullResVar;
        if (c[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          c[i] := getStackVar(FParams[i], Offset);

        if (Params[i].ParType in [lptVar, lptOut]) then
          if (not isVariable(c[i])) then
            LapeException(lpeVariableExpected)
          else if c[i].VarPos.isPointer then
          begin
            b.VarPos.MemPos := mpStack;
            b.VarType := FCompiler.getBaseType(ltPointer);
            c[i].VarType := b.VarType;

            tmp := c[i];
            c[i] := b.VarType.Eval(op_Assign, e, b, c[i], Offset, @Self.DocPos);
            setNullResVar(tmp, 1);
          end
          else
          begin
            b.VarPos.MemPos := mpStack;
            b.VarType := FCompiler.getBaseType(ltPointer);
            c[i] := c[i].VarType.Eval(op_Addr, b, c[i], e, Offset, @Self.DocPos);
          end
        else if (c[i].VarPos.MemPos <> mpStack) or ((Params[i].VarType <> nil) and (not Params[i].VarType.Equals(c[i].VarType))) then
          if Params[i].VarType.CompatibleWith(c[i].VarType) then
          begin
            b.VarPos.MemPos := mpStack;
            b.VarType := Params[i].VarType;

            tmp := c[i];
            c[i] := Params[i].VarType.Eval(op_Assign, e, b, c[i], Offset, @Self.DocPos);
            setNullResVar(tmp, 1);
          end
          else
            LapeException(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, c[i].VarType.AsString]);

        if (c[i].VarPos.MemPos <> mpStack) or (c[i].VarType = nil) then
          LapeException(lpeCannotInvoke);
      except on E: lpException do
        LapeException(E.Message, FParams[i].DocPos);
      end;

      setNullResVar(FDest);
      if (Res <> nil) then
      begin
        Result.VarPos.MemPos := mpVar;
        Result.VarType := Res;
        Result.VarPos.StackVar := FCompiler.getTempVar(Res);

        b := NullResVar;
        b.VarPos.MemPos := mpStack;
        b.VarType := FCompiler.getBaseType(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), b, Result, NullResVar, Offset, @Self.DocPos);
      end;

      FCompiler.Emitter._IncCall(ResVarToIMemPos(a), ParamSize, Offset, @Self.DocPos);
    end;
  end;

  function DoExternalMethod(a: TResVar; c: TResVarArray): TResVar;
  var
    i: Integer;
    b, e, tmp: TResVar;
  begin
    Assert(a.VarType is TLapeType_ExternalMethod);
    Assert(Length(c) = TLapeType_ExternalMethod(a.VarType).Params.Count);
    Result := NullResVar;
    e := NullResVar;

    with TLapeType_ExternalMethod(a.VarType) do
    begin
      for i := 0 to Params.Count - 1 do
      begin
        b := NullResVar;
        if (c[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          getTempVar(FParams[i], Offset, c[i], 0);

        if (c[i].VarPos.MemPos = mpStack) or (c[i].VarType = nil) then
          LapeException(lpeCannotInvoke, FParams[i].DocPos)
        else if (Params[i].ParType in [lptVar, lptOut]) and (not isVariable(c[i])) then
          LapeException(lpeVariableExpected, FParams[i].DocPos);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(c[i].VarType)) then
          if (not (Params[i].ParType in [lptVar, lptOut])) and Params[i].VarType.CompatibleWith(c[i].VarType) then
          try
            b.VarPos.MemPos := mpVar;
            b.VarType := Params[i].VarType;
            b.VarPos.StackVar := Compiler.getTempVar(b.VarType);

            tmp := c[i];
            c[i] := Params[i].VarType.Eval(op_Assign, e, b, c[i], Offset, @Self.DocPos);
            setNullResVar(tmp, 1);
          except on E: lpException do
            LapeException(E.Message, FParams[i].DocPos);
          end
          else
            LapeException(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, c[i].VarType.AsString], FParams[i].DocPos);

        b.VarPos.MemPos := mpStack;
        b.VarType := Compiler.getBaseType(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), b, c[i], NullResVar, Offset, @Self.DocPos);
      end;

      if (Res = nil) then
      begin
        setNullResVar(FDest);
        FCompiler.Emitter._InvokeExternalProc(ResVarToIMemPos(a), Params.Count * LapeTypeSize[ltPointer], Offset, @Self.DocPos)
      end
      else
      begin
        Result.VarType := Res;
        if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          FDest := VarResVar;
        getDestVar(FDest, Result, op_Unknown, FCompiler);
        FCompiler.Emitter._InvokeExternalFunc(ResVarToIMemPos(a), ResVarToIMemPos(Result), Params.Count * LapeTypeSize[ltPointer], Offset, @Self.DocPos)
      end;
    end;
  end;

begin
  Result := NullResVar;
  Assert(FIdent <> nil);

  if (FIdent is TLapeTree_VarType) then
    Result := DoCast()
  else
  begin
    a := FIdent.Compile(Offset);

    if (a.VarType <> nil) and (a.VarType is TLapeType_OverloadedMethod) then
    begin
      a := getResVar(TLapeType_OverloadedMethod(a.VarType).getMethod(getParamTypes(), FDest.VarType));
      if (a.VarType = nil) then
        LapeException(lpeNoOverloadedMethod, [getParamTypesStr()], FIdent.DocPos);
    end
    else if (a.VarType = nil) or (not (a.VarType.BaseType in [ltProc, ltExternalProc])) then
      LapeException(lpeCannotInvoke, FIdent.DocPos);

    with TLapeType_Method(a.VarType) do
    begin
      if (FParams.Count > Params.Count) then
        if (FParams.Count > 0) then
          LapeException(lpeTooMuchParameters, FParams[Params.Count].DocPos)
        else
          LapeException(lpeTooMuchParameters, Self.DocPos);

      SetLength(c, Params.Count);
      for i := 0 to Params.Count - 1 do
        if (i >= FParams.Count) or (FParams[i] = nil) or (FParams[i].ClassType = TLapeTree_ExprBase) then
          if (Params[i].Default <> nil) then
          begin
            c[i] := getResVar(Params[i].Default);
            if (Params[i].ParType in [lptVar, lptOut]) and (not isVariable(c[i])) then
              if (FParams[i] <> nil) then
                LapeException(lpeVariableExpected, FParams[i].DocPos)
              else
                LapeException(lpeVariableExpected, Self.DocPos);
          end
          else if (FParams[i] <> nil) then
            LapeException(lpeNoDefaultForParam, [i + 1], FParams[i].DocPos)
          else
            LapeException(lpeNoDefaultForParam, [i + 1], Self.DocPos)
        else
        begin
          if (FParams[i] is TLapeTree_OpenArray) then
            TLapeTree_OpenArray(FParams[i]).ToType := Params[i].VarType;
          c[i] := NullResVar;
        end;

      if (a.VarType.BaseType = ltProc) then
        Result := DoInternalMethod(a, c)
      else
        Result := DoExternalMethod(a, c);
    end;

    setNullResVar(a, 1);
    for i := 0 to FParams.Count - 1 do
      setNullResVar(c[i], 1);
  end;
end;

procedure TLapeTree_Operator.setLeft(Node: TLapeTree_ExprBase);
begin
  if (FLeft <> nil) and (FLeft <> Node) then
    FLeft.Free();
  FLeft := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Operator.setRight(Node: TLapeTree_ExprBase);
begin
  if (FRight <> nil) and (FRight <> Node) then
    FRight.Free();
  FRight := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Operator.DeleteChild(Node: TLapeTree_Base);
begin
  if (Left = Node) then
    FLeft := nil
  else if (Right = Node) then
    FRight := nil;
end;

constructor TLapeTree_Operator.Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  FLeft := nil;
  FRight := nil;
  FDest := NullResVar;
  FOperatorType := AOperatorType;
end;

destructor TLapeTree_Operator.Destroy;
begin
  setLeft(nil);
  setRight(nil);

  inherited;
end;

function TLapeTree_Operator.isConstant: Boolean;
begin
  Result := ((FLeft = nil) or (not (TLapeTree_Base(FLeft) is TLapeTree_MultiIf))) and (
    ((FOperatorType = op_Dot) and (FRight <> nil) and FRight.isConstant()) or
    ((FLeft = nil) or FLeft.isConstant()) and ((FRight = nil) or FRight.isConstant())
  );
end;

function TLapeTree_Operator.resType: TLapeType;
var
  l, r: TLapeType;
  v: TResVar;
begin
  Result := nil;
  if (FLeft <> nil) then
    if (TLapeTree_Base(FLeft) is TLapeTree_MultiIf) then
      Exit(FCompiler.getBaseType(ltBoolean))
    else
      l := FLeft.resType()
  else
    l := nil;

  if (l <> nil) and (FRight is TLapeTree_OpenArray) then
    TLapeTree_OpenArray(FRight).ToType := l;

  if (FRight <> nil) then
    r := FRight.resType()
  else
    r := nil;
  if (l = nil) and (r = nil) then
    Exit
  else if (l = nil) then
  begin
    l := r;
    r := nil;
  end;

  if (FRight <> nil) and (FRight is TLapeTree_GlobalVar) then
    Result := l.EvalRes(FOperatorType, TLapeTree_GlobalVar(FRight).GlobalVar)
  else
    Result := l.EvalRes(FOperatorType, r);

  if (Result = nil) and (FOperatorType = op_IN) and (FRight <> nil) and (FRight is TLapeTree_OpenArray) then
  begin
    TLapeTree_Base(FLeft) := TLapeTree_MultiIf.Create(FLeft, FRight as TLapeTree_OpenArray);
    with TLapeTree_MultiIf(TLapeTree_Base(FLeft)) do
    begin
      v := NullResVar;
      v.VarType := FCompiler.getBaseType(ltBoolean);
      Result := v.VarType;

      if (FDest.VarType <> nil) and (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and v.VarType.Equals(FDest.VarType) then
        v := FDest
      else
      begin
        FDest := NullResVar;
        v := getResVar(FCompiler.getTempVar(ltBoolean));
        if (v.VarPos.MemPos = mpVar) then
          v.VarPos.StackVar.isConstant := False;
      end;

      Body := TLapeTree_Operator.Create(op_Assign, FCompiler, @DocPos);
      with TLapeTree_Operator(Body) do
      begin
        Left := TLapeTree_ResVar.Create(v, FCompiler, @DocPos);
        Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(v.VarType.NewGlobalVarStr('1'))), FCompiler, @DocPos);
      end;

      ElseBody := TLapeTree_Operator.Create(op_Assign, FCompiler, @DocPos);
      with TLapeTree_Operator(ElseBody) do
      begin
        Left := TLapeTree_ResVar.Create(v, FCompiler, @DocPos);
        Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(v.VarType.NewGlobalVarStr('0'))), FCompiler, @DocPos);
     end;
    end;
  end;
end;

function TLapeTree_Operator.Evaluate: TLapeGlobalVar;
var
  l, r: TLapeGlobalVar;
begin
  Result := nil;

  if (FLeft <> nil) then
    if (TLapeTree_Base(FLeft) is TLapeTree_MultiIf) then
      LapeException(lpeInvalidEvaluation, DocPos)
    else
      l := FLeft.Evaluate()
  else
    l := nil;

  if (l <> nil) and (l.VarType <> nil) and (FRight is TLapeTree_OpenArray) then
    TLapeTree_OpenArray(FRight).ToType := l.VarType;

  if (FRight <> nil) then
    r := FRight.Evaluate()
  else
    r := nil;

  if (l = nil) and (r = nil) then
    LapeException(lpeInvalidEvaluation, DocPos)
  else if (l = nil) then
  begin
    l := r;
    r := nil;
  end;
  if (l.VarType = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  try
    Result := TLapeGlobalVar(FCompiler.addManagedVar(l.VarType.EvalConst(FOperatorType, l, r)));
  except on E: lpException do
    LapeException(E.Message, DocPos);
  end;
end;

function TLapeTree_Operator.Compile(var Offset: Integer): TResVar;
var
  l, r: TResVar;
  DoneAssignment: Boolean;

  function doMultiIf: TResVar;
  begin
    with TLapeTree_MultiIf(TLapeTree_Base(FLeft)) do
    begin
      if (FDest.VarType <> nil) and (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and TLapeTree_Operator(Body).Left.resType().Equals(FDest.VarType) then
      begin
        TLapeTree_Operator(Body).Left.Free();
        TLapeTree_Operator(Body).Left := TLapeTree_ResVar.Create(FDest, FCompiler, @DocPos);
        TLapeTree_Operator(ElseBody).Left.Free();
        TLapeTree_Operator(ElseBody).Left := TLapeTree_ResVar.Create(FDest, FCompiler, @DocPos);
      end
      else
        FDest := NullResVar;

      Result := Compile(Offset);
    end;
  end;

begin
  Result := NullResVar;
  DoneAssignment := False;

  if (FLeft <> nil) then
    if (resType() <> nil) and (TLapeTree_Base(FLeft) is TLapeTree_MultiIf) then
      Exit(doMultiIf())
    else
    begin
      l := FLeft.Compile(Offset);
      if (FOperatorType = op_Assign) and (not isVariable(l)) then
        LapeException(lpeCannotAssign, FLeft.DocPos);
    end;

  if (FRight <> nil) then
  begin
    if (l.VarType <> nil) and (FRight is TLapeTree_OpenArray) then
      TLapeTree_OpenArray(FRight).ToType := l.VarType;

    if (FOperatorType = op_Assign) and
      (FLeft <> nil) and isVariable(l) and
      (FRight is TLapeTree_DestExprBase) {and (not (TLapeTree_Operator(FRight).OperatorType in [op_Assign, op_Deref]))} and
      (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos)
    then
    begin
      TLapeTree_DestExprBase(FRight).Dest := l;
      DoneAssignment := True;
    end;

    r := FRight.Compile(Offset);
    if DoneAssignment then
      if (TLapeTree_Operator(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
        DoneAssignment := False
      else
        Result := l;
  end
  else
    r := NullResVar;

  try
    if (not DoneAssignment) then
    begin
      if (l.VarType = nil) and (r.VarType = nil) then
        LapeException(lpeInvalidEvaluation, DocPos)
      else if (l.VarType = nil) then
      begin
        l := r;
        r := NullResVar;
      end;
      if (l.VarType = nil) then
        LapeException(lpeInvalidEvaluation, DocPos);

      try
        Result := l.VarType.Eval(FOperatorType, FDest, l, r, Offset, @DocPos);
      except on E: lpException do
        LapeException(E.Message, DocPos);
      end;
    end;
  finally
    if ((FLeft <> nil) or (FRight <> nil)) then
      setNullResVar(l, 1);
    if ((FLeft <> nil) and (FRight <> nil)) then
      setNullResVar(r, 1);
  end;
end;

constructor TLapeTree_ResVar.Create(AResVar: TResVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  FResVar := AResVar;
end;

function TLapeTree_ResVar.resType: TLapeType;
begin
  Result := FResVar.VarType;
end;

function TLapeTree_ResVar.Evaluate: TLapeGlobalVar;
begin
  if (FResVar.VarPos.MemPos = mpMem) then
    Result := FResVar.VarPos.GlobalVar
  else
    LapeException(lpeInvalidEvaluation, DocPos);
end;

function TLapeTree_ResVar.Compile(var Offset: Integer): TResVar;
begin
  Result := FResVar;
end;

function TLapeTree_GlobalVar.getVarAsString: lpString;
begin
  if (FGlobalVar <> nil) then
    Result := FGlobalVar.AsString
  else
    Result := '';
end;

constructor TLapeTree_GlobalVar.Create(AGlobalVar: TLapeGlobalVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  FGlobalVar := AGlobalVar;
  if (ACompiler <> nil) and (FGlobalVar.DeclarationList = nil) then
    FGlobalVar := TLapeGlobalVar(FCompiler.addManagedVar(FGlobalVar));
end;

function TLapeTree_GlobalVar.isConstant: Boolean;
begin
  Result := FGlobalVar.isConstant;
end;

function TLapeTree_GlobalVar.resType: TLapeType;
begin
  Result := FGlobalVar.VarType;
end;

function TLapeTree_GlobalVar.Evaluate: TLapeGlobalVar;
begin
  Result := FGlobalVar;
end;

function TLapeTree_GlobalVar.Compile(var Offset: Integer): TResVar;
begin
  Result := getResVar(FGlobalVar);
end;

constructor TLapeTree_VarType.Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
var
  t: TLapeType_Type;
begin
  Assert(ACompiler <> nil);
  t := TLapeType_Type(ACompiler.addManagedType(TLapeType_Type.Create(AVarType, ACompiler, '', ADocPos)));
  inherited Create(t.NewGlobalVarP(), ACompiler, ADocPos);
  FVarType := AVarType;
end;

constructor TLapeTree_Integer.Create(AValue: Integer; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(TLapeType_UInt32(ACompiler.getBaseType(ltUInt32)).NewGlobalVar(AValue), ACompiler, ADocPos);
end;

constructor TLapeTree_Integer.Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(DetermineIntType(AStr)).NewGlobalVarStr(AStr), ACompiler, ADocPos);
end;

constructor TLapeTree_Float.Create(AValue: Extended; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(TLapeType_Extended(ACompiler.getBaseType(ltExtended)).NewGlobalVar(AValue), ACompiler, ADocPos);
end;

constructor TLapeTree_Float.Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Create(StrToFloatDot(AStr), ACompiler, ADocPos);
end;

constructor TLapeTree_String.Create(AValue: AnsiString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltAnsiString).NewGlobalVarStr(AValue), ACompiler, ADocPos);
end;

{$IFNDEF Lape_NoWideString}
constructor TLapeTree_String.Create(AValue: WideString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideString).NewGlobalVarStr(AValue), ACompiler, ADocPos);
end;
{$ENDIF}

constructor TLapeTree_String.Create(AValue: UnicodeString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltUnicodeString).NewGlobalVarStr(AValue), ACompiler, ADocPos);
end;

constructor TLapeTree_Char.Create(AValue: WideChar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  if (AValue > #255) then
    inherited Create(ACompiler.getBaseType(ltWideChar).NewGlobalVarStr(UnicodeString(AValue)), ACompiler, ADocPos)
  else
    inherited Create(ACompiler.getBaseType(ltChar).NewGlobalVarStr(UnicodeString(AValue)), ACompiler, ADocPos);
end;

constructor TLapeTree_Pointer.Create(AValue: Pointer; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(TLapeType_Pointer(ACompiler.getBaseType(ltPointer)).NewGlobalVar(AValue), ACompiler, ADocPos);
end;

procedure TLapeTree_Range.setLo(Node: TLapeTree_ExprBase);
begin
  if (FLo <> nil) and (FLo <> Node) then
    FLo.Free();
  FLo := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Range.setHi(Node: TLapeTree_ExprBase);
begin
  if (FHi <> nil) and (FHi <> Node) then
    FHi.Free();
  FHi := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Range.DeleteChild(Node: TLapeTree_Base);
begin
  if (FLo = Node) then
    FLo := nil
  else if (FHi = Node) then
    FHi := nil;
end;

constructor TLapeTree_Range.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  setLo(nil);
  setHi(nil);
end;

destructor TLapeTree_Range.Destroy;
begin
  setLo(nil);
  setHi(nil);
  inherited;
end;

procedure TLapeTree_StatementList.DeleteChild(Node: TLapeTree_Base);
begin
  FStatements.DeleteItem(Node);
end;

constructor TLapeTree_StatementList.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FStatements := TLapeStatementList.Create(nil);
end;

destructor TLapeTree_StatementList.Destroy;
var
  i: Integer;
begin
  for i := FStatements.Count - 1 downto 0 do
    if (FStatements[i] <> nil) and (FStatements[i].Parent = Self) then
      FStatements[i].Free();
  FreeAndNil(FStatements);

  inherited;
end;

function TLapeTree_StatementList.addStatement(Statement: TLapeTree_Base): Integer;
begin
  Result := FStatements.add(Statement);
  if (Statement <> nil) then
    Statement.Parent := Self;
end;

function TLapeTree_StatementList.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
begin
  Result := NullResVar;
  for i := 0 to FStatements.Count - 1 do
    Result := FStatements[i].Compile(Offset);
end;

procedure TLapeTree_Method.setStatements(Node: TLapeTree_StatementList);
begin
  if (FStatements <> nil) and (FStatements <> Node) then
    FStatements.Free();
  FStatements := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Method.DeleteChild(Node: TLapeTree_Base);
begin
  if (Node = FStatements) then
    FStatements := nil;
end;

constructor TLapeTree_Method.Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(AMethod <> nil);
  Assert(AMethod.VarType is TLapeType_InternalMethod);
  Assert(AStackInfo <> nil);
  inherited Create(ACompiler, ADocPos);

  FreeStackInfo := True;
  FMethod := AMethod;
  FStackInfo := AStackInfo;
end;

destructor TLapeTree_Method.Destroy;
begin
  setStatements(nil);
  if FreeStackInfo then
    FStackInfo.Free();
  inherited;
end;

function TLapeTree_Method.Compile(var Offset: Integer): TResVar;
var
  if_o: Integer;
begin
  Assert(FCompiler <> nil);
  Assert(FMethod <> nil);
  if_o := FCompiler.Emitter._JmpR(0, Offset, @DocPos);
  PUInt32(FMethod.Ptr)^ := FCompiler.Emitter.getCodeOffset(Offset);
  FCompiler.IncStackInfo(FStackInfo, Offset, True, @DocPos);
  Result := FStatements.Compile(Offset);
  FCompiler.DecStackInfo(Offset, True, True, False, @DocPos);
  FCompiler.Emitter._JmpR(Offset - if_o, if_o, @DocPos);
end;

procedure TLapeTree_VarList.DeleteChild(Node: TLapeTree_Base);
var
  i: Integer;
begin
  for i := FVars.Count - 1 downto 0 do
    if (FVars[i].Default = Node) then
      FVars.Delete(i);
end;

constructor TLapeTree_VarList.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
const
  NullVar: TLapeVarDecl = (VarDecl: nil; Default: nil);
begin
  inherited;
  FVars := TLapeVarList.Create(NullVar);
end;

destructor TLapeTree_VarList.Destroy;
var
  i: Integer;
begin
  for i := FVars.Count - 1 downto 0 do
    if (FVars[i].Default <> nil) and (FVars[i].Default.Parent = Self) then
      FVars[i].Default.Free();
  FreeAndNil(FVars);

  inherited;
end;

function TLapeTree_VarList.addVar(AVar: TLapeVarDecl): Integer;
begin
  Result := FVars.add(AVar);
  if (AVar.Default <> nil) then
    AVar.Default.Parent := Self;
end;

function TLapeTree_VarList.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  b: Boolean;
begin
  Result := NullResVar;
  i := 0;
  while (i < FVars.Count) do
    if (FVars[i].VarDecl <> nil) and (FVars[i].Default <> nil) then
      with FVars[i], TLapeTree_Operator.Create(op_Assign, Compiler, @Default.DocPos) do
      try
        b := (VarDecl is TLapeVar) and TLapeVar(VarDecl).isConstant;
        if b then
          TLapeVar(VarDecl).isConstant := False;

        Left := TLapeTree_ResVar.Create(getResVar(VarDecl), Compiler, @Default.DocPos);
        Right := Default;
        Result := Compile(Offset);

        if b then
          TLapeVar(VarDecl).isConstant := True;
      finally
        Free();
      end
    else
      Inc(i);
end;

procedure TLapeTree_If.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_If.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_If.setElse(Node: TLapeTree_Base);
begin
  if (FElse <> nil) and (FElse <> Node) then
    FElse.Free();
  FElse := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_If.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FBody = Node) then
    FBody := nil
  else if (FElse = Node) then
    FElse := nil;
end;

function TLapeTree_If.CompileBody(var Offset: Integer): TResVar;
begin
  Assert(FBody <> nil);
  Result := FBody.Compile(Offset);
end;

constructor TLapeTree_If.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FBody := nil;
  FElse := nil;
end;

destructor TLapeTree_If.Destroy;
begin
  setCondition(nil);
  setBody(nil);
  setElse(nil);
  inherited;
end;

function TLapeTree_If.Compile(var Offset: Integer): TResVar;
var
  cnd, tmp, e: TResVar;
  if_o, if_e: Integer;
begin
  Result := NullResVar;
  Assert(FCondition <> nil);

  e := NullResVar;
  cnd := FCondition.Compile(Offset);
  if (cnd.VarType = nil) or (not (cnd.VarType.BaseType in LapeIfTypes)) then
    LapeException(lpeInvalidCondition, FCondition.DocPos);

  if (not (cnd.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmp := cnd;
    if (cnd.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition.DocPos)
    else
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition.DocPos);
    setNullResVar(tmp);
  end;

  if (FBody <> nil) or (FElse <> nil) then
  begin
    if_o := FCompiler.Emitter._JmpRIfNot(0, cnd, Offset, @DocPos);
    if (FBody <> nil) then
    begin
      Result := CompileBody(Offset);

      if (FElse <> nil) then
        if_e := FCompiler.Emitter._JmpR(0, Offset, @DocPos);
      FCompiler.Emitter._JmpRIfNot(Offset - if_o, cnd, if_o, @DocPos);
    end;

    if (FElse <> nil) then
    begin
      Result := FElse.Compile(Offset);

      if (FBody <> nil) then
        FCompiler.Emitter._JmpR(Offset - if_e, if_e, @DocPos)
      else
        FCompiler.Emitter._JmpRIf(Offset - if_o, cnd, if_o, @DocPos);
    end;
  end;

  setNullResVar(cnd, 1);
end;

procedure TLapeTree_MultiIf.DeleteChild(Node: TLapeTree_Base);
begin
  inherited;
  if (FValues <> nil) then
    FValues.DeleteItem(Node);
end;

constructor TLapeTree_MultiIf.Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  setCondition(Ident);
  FValues := TLapeStatementList.Create(nil, dupIgnore);
end;

constructor TLapeTree_MultiIf.Create(Ident: TLapeTree_ExprBase; OpenArray: TLapeTree_OpenArray; FreeArray: Boolean = True);
begin
  Assert(OpenArray <> nil);
  Create(Ident, OpenArray.Compiler, @OpenArray.DocPos);
  while (OpenArray.Values.Count > 0) do
    addValue(OpenArray.Values[0]);
  if FreeArray then
    OpenArray.Free();
end;

destructor TLapeTree_MultiIf.Destroy;
var
  i: Integer;
begin
  for i := FValues.Count - 1 downto 0 do
    if (FValues[i] <> nil) and (FValues[i].Parent = Self) then
      FValues[i].Free();
  FreeAndNil(FValues);
  inherited;
end;

function TLapeTree_MultiIf.addValue(p: TLapeTree_Base): Integer;
begin
  Result := FValues.add(p);
  if (p <> nil) then
    p.Parent := Self;
end;

function TLapeTree_MultiIf.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  c: TResVar;
  a, b: TLapeTree_Operator;
  t: TLapeTree_ExprBase;
begin
  Result := NullResVar;
  Assert(FValues.Count > 0);
  Assert(FCondition <> nil);
  Assert((FBody <> nil) or (FElse <> nil));

  a := nil;
  b := nil;
  if (not getTempVar(FCondition, Offset, c)) then
    LapeException(lpeInvalidCondition, DocPos);

  try

    for i := FValues.Count - 1 downto 0 do
    begin
      if (FValues[i] is TLapeTree_Range) then
        with TLapeTree_Range(FValues[i]) do
        begin
          a := TLapeTree_Operator.Create(op_AND, FCompiler, @DocPos);
          a.Left := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, FCompiler, @DocPos);
          with TLapeTree_Operator(a.Left) do
          begin
            Left := TLapeTree_ResVar.Create(c, FCompiler, @FCondition.DocPos);
            Right := Lo;
          end;
          a.Right := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, FCompiler, @DocPos);
          with TLapeTree_Operator(a.Right) do
          begin
            Left := TLapeTree_ResVar.Create(c, FCompiler, @FCondition.DocPos);
            Right := Hi;
          end;
        end
      else if (FValues[i] is TLapeTree_ExprBase) then
      begin
        a := TLapeTree_Operator.Create(op_cmp_Equal, FCompiler, @FValues[i].DocPos);
        a.Left := TLapeTree_ResVar.Create(c, FCompiler, @FCondition.DocPos);
        a.Right := TLapeTree_ExprBase(FValues[i]);
      end
      else
        LapeException(lpeInvalidEvaluation, DocPos);

      if (b = nil) then
        b := a
      else
      begin
        t := b;
        b := TLapeTree_Operator.Create(op_OR, FCompiler, @DocPos);
        b.Left := a;
        b.Right := t;
      end;
      a := nil;
    end;

    t := FCondition;
    FCondition := b;
    try
      Result := inherited;
    finally
      FCondition := t;
      FreeAndnil(b);
    end;

  except
    if (a <> nil) then
      a.Free();
    if (b <> nil) then
      b.Free();
    raise;
  end;

  setNullResVar(c, 2);
end;

procedure TLapeTree_Case.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Case.setElse(Node: TLapeTree_Base);
begin
  if (FElse <> nil) and (FElse <> Node) then
    FElse.Free();
  FElse := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Case.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FElse = Node) then
    FElse := nil
  else
    FFields.DeleteItem(TLapeTree_MultiIf(Node));
end;

constructor TLapeTree_Case.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FElse := nil;
  FFields := TLapeCaseFieldList.Create(nil, dupIgnore);
end;

destructor TLapeTree_Case.Destroy;
var
  i: Integer;
begin
  for i := FFields.Count - 1 downto 0 do
    if (FFields[i] <> nil) and (FFields[i].Parent = Self) then
      FFields[i].Free();
  FreeAndNil(FFields);
  setCondition(nil);
  setElse(nil);
  inherited;
end;

function TLapeTree_Case.addField(p: TLapeTree_MultiIf): Integer;
begin
  Result := FFields.add(p);
  if (p <> nil) then
    p.Parent := Self;
end;

function TLapeTree_Case.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  c: TResVar;
begin
  Result := NullResVar;
  Assert(FCondition <> nil);

  if (FFields.Count > 0) then
  begin
    if (not getTempVar(FCondition, Offset, c)) then
      LapeException(lpeInvalidCondition, DocPos);

    for i := 0 to FFields.Count - 1 do
      FFields[i].Condition := TLapeTree_ResVar.Create(c, FCompiler, @FCondition.DocPos);
    FFields[FFields.Count - 1].ElseBody := FElse;
    for i := FFields.Count - 1 downto 1 do
      FFields[i - 1].ElseBody := FFields[i];
    Result := FFields[0].Compile(Offset);

    setNullResVar(c, 2);
  end
  else if (FElse <> nil) then
    Result := FElse.Compile(Offset);
end;

function TLapeTree_While.CompileBody(var Offset: Integer): TResVar;
var
  cnd, tmp, e: TResVar;
  o: Integer;
begin
  o := FCompiler.Emitter.CheckOffset(Offset);
  Result := inherited;

  e := NullResVar;
  cnd := FCondition.Compile(Offset);

  if (not (cnd.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmp := cnd;
    if (cnd.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition.DocPos)
    else
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition.DocPos);
    setNullResVar(tmp);
  end;

  FCompiler.Emitter._JmpRIf(o - Offset, cnd, Offset, @DocPos);
  setNullResVar(cnd);
end;

procedure TLapeTree_For.setCounter(Node: TLapeTree_ExprBase);
begin
  if (FCounter <> nil) and (FCounter <> Node) then
    FCounter.Free();
  FCounter := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_For.setLimit(Node: TLapeTree_ExprBase);
begin
  if (FLimit <> nil) and (FLimit <> Node) then
    FLimit.Free();
  FLimit := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_For.setStep(Node: TLapeTree_ExprBase);
begin
  if (FStep <> nil) and (FStep <> Node) then
    FStep.Free();
  FStep := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_For.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCounter = Node) then
    FCounter := nil
  else if (FLimit = Node) then
    FLimit := nil
  else if (FStep = Node) then
    FStep := nil
  else
    inherited;
end;

constructor TLapeTree_For.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCounter := nil;
  FLimit := nil;
  FStep := nil;
  WalkDown := False;
end;

destructor TLapeTree_For.Destroy;
begin
  setCounter(nil);
  setLimit(nil);
  setStep(nil);
  inherited;
end;

function TLapeTree_For.Compile(var Offset: Integer): TResVar;
var
  cnt, lim, stp: TResVar;
  o: TLapeTree_Operator;
  tb: TLapeTree_Base;
  a, b, c: TLapeType;
  v: TLapeVar;

  function changeVarType(var x: TResVar; t: TLapeType): TLapeType;
  begin
    Result := x.VarType;
    x.VarType := t;
  end;

begin
  Result := NullResVar;
  Assert(FCondition = nil);
  Assert(FCounter <> nil);
  Assert(FLimit <> nil);

  a := nil;
  b := nil;
  c := nil;
  v := nil;

  tb := nil;
  cnt := FCounter.Compile(Offset);
  try
    if (not getTempVar(FLimit, Offset, lim)) or (lim.VarType = nil) or (lim.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, FLimit.DocPos);

    if (cnt.VarType <> nil) and (not isVariable(cnt)) then
    begin
      v := FCompiler.getTempVar(cnt.VarType, 2);
      v.isConstant := False;
      cnt := v.VarType.Eval(op_Assign, Result, GetResVar(v), cnt, Offset, @FCounter.DocPos);
    end;
    if (cnt.VarType = nil) or (not isVariable(cnt)) or (cnt.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidIterator, FCounter.DocPos);

    a := changeVarType(cnt, FCompiler.getBaseType(cnt.VarType.BaseIntType));
    b := changeVarType(lim, FCompiler.getBaseType(lim.VarType.BaseIntType));

    if (FStep = nil) then
      stp := getResVar(FCompiler.addManagedVar(cnt.VarType.NewGlobalVarStr('1')))
    else if (not getTempVar(FStep, Offset, stp)) or (stp.VarType = nil) or (stp.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, FStep.DocPos);

    c := changeVarType(stp, FCompiler.getBaseType(stp.VarType.BaseIntType));

    if WalkDown then
      FCondition := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, FCompiler, @DocPos)
    else
      FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, FCompiler, @DocPos);
    TLapeTree_Operator(FCondition).Left := TLapeTree_ResVar.Create(cnt, FCompiler, @FCounter.DocPos);
    TLapeTree_Operator(FCondition).Right := TLapeTree_ResVar.Create(lim, FCompiler, @FLimit.DocPos);

    o := TLapeTree_Operator.Create(op_Assign, FCompiler, @FLimit.DocPos);
    o.Left := TLapeTree_ResVar.Create(cnt, FCompiler, @FCounter.DocPos);
    if WalkDown then
      o.Right := TLapeTree_Operator.Create(op_Minus, FCompiler, @FLimit.DocPos)
    else
      o.Right := TLapeTree_Operator.Create(op_Plus, FCompiler, @FLimit.DocPos);
    TLapeTree_Operator(o.Right).Left := TLapeTree_ResVar.Create(cnt, FCompiler, @FCounter.DocPos);
    TLapeTree_Operator(o.Right).Right := TLapeTree_ResVar.Create(stp, FCompiler, @FCounter.DocPos);

    changeVarType(cnt, a); a := nil;
    changeVarType(lim, b); b := nil;
    changeVarType(stp, c); c := nil;

    tb := FBody;
    if (tb <> nil) then
    begin
      FBody := TLapeTree_StatementList.Create(FCompiler, @FBody.DocPos);
      TLapeTree_StatementList(FBody).addStatement(tb);
      TLapeTree_StatementList(FBody).addStatement(o);
    end
    else
      FBody := o;

    Result := inherited;
  finally
    if (a <> nil) then
      changeVarType(cnt, a);
    if (b <> nil) then
      changeVarType(lim, b);
    if (c <> nil) then
      changeVarType(stp, c);

    if (tb <> nil) then
      TLapeTree_StatementList(FBody).Statements.Delete(0).setParent(nil);
    setBody(tb);
    setCondition(nil);
    if (v <> nil) then
    begin
      v.isConstant := True;
      setNullResVar(cnt, 2);
    end
    else
      setNullResVar(cnt, 1);
    setNullResVar(lim, 2);
    setNullResVar(stp, 2);
  end;
end;

procedure TLapeTree_Repeat.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Repeat.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Repeat.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FBody = Node) then
    FBody := nil;
end;

constructor TLapeTree_Repeat.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FBody := nil;
end;

destructor TLapeTree_Repeat.Destroy;
begin
  setCondition(nil);
  setBody(nil);
  inherited;
end;

function TLapeTree_Repeat.Compile(var Offset: Integer): TResVar;
var
  cnd, tmp, e: TResVar;
  o: Integer;
begin
  Result := NullResVar;
  o := FCompiler.Emitter.CheckOffset(Offset);
  if (FBody <> nil) then
    Result := FBody.Compile(Offset);

  e := NullResVar;
  cnd := FCondition.Compile(Offset);

  if (not (cnd.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmp := cnd;
    if (cnd.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition.DocPos)
    else
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition.DocPos);
    setNullResVar(tmp);
  end;

  FCompiler.Emitter._JmpRIfNot(o - Offset, cnd, Offset, @DocPos);
  setNullResVar(cnd);
end;

procedure TLapeTree_Try.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Try.setExcept(Node: TLapeTree_Base);
begin
  if (FExcept <> nil) and (FExcept <> Node) then
    FExcept.Free();
  FExcept := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Try.setFinally(Node: TLapeTree_Base);
begin
  if (FFinally <> nil) and (FFinally <> Node) then
    FFinally.Free();
  FFinally := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Try.DeleteChild(Node: TLapeTree_Base);
begin
  if (FBody = Node) then
    FBody := nil
  else if (FExcept = Node) then
    FExcept := nil
  else if (FFinally = Node) then
    FFinally := nil;
end;

constructor TLapeTree_Try.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBody := nil;
  FExcept := nil;
  FFinally := nil;
end;

destructor TLapeTree_Try.Destroy;
begin
  setBody(nil);
  setExcept(nil);
  setFinally(nil);
  inherited;
end;

function TLapeTree_Try.Compile(var Offset: Integer): TResVar;
var
  o_try, o_jmp: Integer;
begin
  Result := NullResVar;
  Assert((FExcept <> nil) or (FFinally <> nil));

  o_try := FCompiler.Emitter._IncTry(0, Offset, @DocPos);
  if (FBody <> nil) then
    Result := FBody.Compile(Offset);

  FCompiler.Emitter._DecTry(Offset, @DocPos);
  if (FExcept <> nil) then
  begin
    o_jmp := FCompiler.Emitter._JmpR(0, Offset, @FExcept.DocPos);
    FCompiler.Emitter._IncTry(Offset - o_try, o_try, @DocPos);
    FExcept.Compile(Offset);
    FCompiler.Emitter._CatchException(Offset, @FExcept.DocPos);
    FCompiler.Emitter._JmpR(Offset - o_jmp, o_jmp, @FExcept.DocPos);
  end;
  if (FFinally <> nil) then
    if (FExcept <> nil) then
      FFinally.Compile(Offset)
    else
    begin
      FCompiler.Emitter._IncTry(Offset - o_try, o_try, @DocPos);
      FFinally.Compile(Offset);
      FCompiler.Emitter._EndTry(Offset, @FFinally.DocPos);
    end;
end;

end.

