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

  TLapeFlowStatement = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    CodeOffset: Integer;
    DocPos: TDocPos;
    JumpSafe: Boolean;
  end;

  TLapeExpressionList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_ExprBase>;
  TLapeStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_Base>;
  TLapeFlowStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeFlowStatement>;

  ILapeTree_CanBreak = interface(IInterface)
    ['{F63EF9FB-2DD0-4C2C-B564-1333995EE9AF}']
    procedure addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
  end;

  ILapeTree_CanContinue = interface(IInterface)
    ['{411AF7EA-9D38-48DE-90E9-362E2D0D09E9}']
    procedure addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
  end;

  ILapeTree_CanExit = interface(IInterface)
    ['{D1963640-31F9-4BB3-B03E-0F1DB314D98E}']
    procedure addExitStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
  end;

  TLapeTree_Base = class(TLapeBaseClass)
  protected
    FParent: TLapeTree_Base;
    FCompiler: TLapeCompilerBase;

    procedure setParent(Parent: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); virtual;
  public
    DocPos: TDocPos;

    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
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
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Ident: TLapeTree_ExprBase read FIdent write setIdent;
    property Params: TLapeExpressionList read FParams;
  end;

  TLapeTree_InternalMethodClass = class of TLapeTree_InternalMethod;
  TLapeTree_InternalMethod = class(TLapeTree_Invoke)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    function isConstant: Boolean; override;
  end;

  TLapeTree_InternalMethod_FlowStatement = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
  end;

  TLapeTree_InternalMethod_Break = class(TLapeTree_InternalMethod_FlowStatement)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Continue = class(TLapeTree_InternalMethod_FlowStatement)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Exit = class(TLapeTree_InternalMethod_FlowStatement)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_SizeOf = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Ord = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Low = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_High = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Length = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Succ = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Pred = class(TLapeTree_InternalMethod_Succ)
  public
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Inc = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Dec = class(TLapeTree_InternalMethod)
  public
    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
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

    function isConstant: Boolean; override;
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
    constructor Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
  end;

  TLapeTree_Float = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: Extended; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
  end;

  TLapeTree_String = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: AnsiString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    {$IFNDEF Lape_NoWideString}
    constructor Create(AValue: WideString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
	{$ENDIF}
    constructor Create(AValue: UnicodeString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
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

  TLapeTree_Method = class(TLapeTree_Base, ILapeTree_CanExit)
  protected
    FMethod: TLapeGlobalVar;
    FStackInfo: TLapeStackInfo;
    FStatements: TLapeTree_StatementList;
    FExitStatements: TLapeFlowStatementList;

    procedure setStatements(Node: TLapeTree_StatementList); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    FreeStackInfo: Boolean;

    constructor Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;
    procedure addExitStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;

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
    FStartBodyOffset: Integer;
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

  TLapeTree_While = class(TLapeTree_If, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FContinueCount: Integer;
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;
    function CompileBody(var Offset: Integer): TResVar; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    procedure addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
    procedure addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
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
    function CompileBody(var Offset: Integer): TResVar; override;
  public
    WalkDown: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Counter: TLapeTree_ExprBase read FCounter write setCounter;
    property Limit: TLapeTree_ExprBase read FLimit write setLimit;
    property Step: TLapeTree_ExprBase read FStep write setStep;
  end;

  TLapeTree_Repeat = class(TLapeTree_Base, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FCondition: TLapeTree_ExprBase;
    FBody: TLapeTree_Base;
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    procedure addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
    procedure addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;

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
function getFlowStatement(Offset: Integer; Pos: PDocPos = nil; JumpSafe: Boolean = False): TLapeFlowStatement; {$IFDEF Lape_Inline}inline;{$ENDIF}

const
  NullFlowStatement: TLapeFlowStatement = (CodeOffset: 0; DocPos: (Line: 0; Col: 0; FileName: ''); JumpSafe: False);

implementation

uses
  lpexceptions, lpeval, lpinterpreter;

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
  Exit;
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

function getFlowStatement(Offset: Integer; Pos: PDocPos = nil; JumpSafe: Boolean = False): TLapeFlowStatement;
begin
  Result := NullFlowStatement;
  Result.CodeOffset := Offset;
  if (Pos <> nil) then
    Result.DocPos := Pos^;
  Result.JumpSafe := JumpSafe;
end;

procedure TLapeTree_Base.setParent(Parent: TLapeTree_Base);
begin
  Assert((Parent = nil) or (Parent.Compiler = FCompiler));
  if (FParent <> nil) and (FParent <> Parent) then
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
    DocPos := NullDocPos;
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
  end
  else
    Result := -1;
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
    else if (v.BaseType in LapeIntegerTypes - [ltUInt64, ltInt64]) then
      Result := FCompiler.getBaseType(ltInt32)
    //else if (v.BaseType in LapeRealTypes) then
    //  Result := FCompiler.getBaseType(ltExtended)
    //else if (v.BaseType in LapeBoolTypes) then
    //  Result := FCompiler.getBaseType(ltBoolean)
    //else if (v.BaseType in LapeStringTypes) then
    //  Result := FCompiler.getBaseType(ltString)
    else
      Result := v;
  end;

begin
  Result := ToType;
  if (Result = nil) then
    if (FValues.Count > 0) then
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
        else if FCompiler.getBaseType(ltVariant).CompatibleWith(t) and FCompiler.getBaseType(ltVariant).CompatibleWith(Result) then
          Result := FCompiler.getBaseType(ltVariant)
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
    end
    else
      Result := TLapeType_StaticArray.Create(NullRange, FCompiler.getBaseType(ltVariant), FCompiler, '', @DocPos);
end;

function TLapeTree_OpenArray.Evaluate: TLapeGlobalVar;
var
  i, c, tmp: Integer;
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
      Result := FCompiler.getTempStackVar(ToType)
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
            Left := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(DetermineIntType(i)).NewGlobalVarStr(IntToStr(i)))), FCompiler, @FValues[i].DocPos);
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

function TLapeTree_Invoke.isConstant: Boolean;
begin
  if (FIdent is TLapeTree_VarType) then
    Result := (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) and FParams[0].isConstant()
  else
    Result := False;
end;

function TLapeTree_Invoke.resType: TLapeType;
var
  t: TLapeType;

  function getVarType(v: TLapeGlobalVar): TLapeType;
  begin
    if (v = nil) then
      Result := nil
    else
      Result := v.VarType;
  end;

begin
  if (FIdent is TLapeTree_VarType) then
    Result := TLapeTree_VarType(FIdent).VarType
  else
  begin
    if (FIdent is TLapeTree_ResVar) or (FIdent is TLapeTree_GlobalVar) then
      t := FIdent.Compile().VarType
    else
      t := nil;

    if (t <> nil) and (t is TLapeType_OverloadedMethod) then
      t := getVarType(TLapeType_OverloadedMethod(t).getMethod(getParamTypes()));
    if (t = nil) or (not (t.BaseType in [ltScriptMethod, ltImportedMethod])) then
      Result := nil
    else
      Result := TLapeType_Method(t).Res;
  end;
end;

function TLapeTree_Invoke.Evaluate: TLapeGlobalVar;
var
  f: TLapeGlobalVar;

  function DoCast: TLapeGlobalVar;
  begin
    Result := nil;
    Assert(FIdent is TLapeTree_VarType);

    with TLapeTree_VarType(FIdent) do
    try
      if (FParams.Count <> 1) or (VarType = nil) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
        LapeException(lpeInvalidCast);
      if (FParams[0] is TLapeTree_OpenArray) then
        TLapeTree_OpenArray(FParams[0]).ToType := VarType;

      Result := FParams[0].Evaluate();
      if VarType.Equals(Result.VarType) or ((Result.VarType <> nil) and (VarType.Size = Result.VarType.Size)) then
        Result := TLapeGlobalVar(FCompiler.addManagedVar(VarType.NewGlobalVarP(Result.Ptr)))
      else if VarType.CompatibleWith(Result.VarType) then
        Result := TLapeGlobalVar(FCompiler.addManagedVar(VarType.EvalConst(op_Assign, VarType.NewGlobalVarP(), Result)))
      else
        LapeException(lpeInvalidCast);
    except on E: lpException do
      LapeException(E.Message, Self.DocPos);
    end;
  end;

  function DoImportedMethod(f: TLapeGlobalVar): TLapeGlobalVar;
  var
    i: Integer;
    t: TLapeGlobalVar;
    a: array of Pointer;
  begin
    Assert(f <> nil);
    Assert(f.VarType.BaseType = ltImportedMethod);
    Result := nil;

    with TLapeType_Method(f.VarType) do
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

        if (t = nil) or (Params[i].ParType in Lape_RefParams) then
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
      TLapeImportedFunc(f.Ptr^)(@a[0], Result.Ptr);
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
    else if (f.Ptr = nil) or (not (f.VarType.BaseType in [ltImportedMethod])) then
      LapeException(lpeCannotInvoke, FIdent.DocPos);

    Result := DoImportedMethod(f);
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
      if (FParams.Count <> 1) or (VarType = nil) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
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
        if (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and (FDest.VarType <> nil) and VarType.Equals(FDest.VarType) then
          b := FDest
        else
        begin
          setNullResVar(FDest);
          b := getResVar(Compiler.getTempVar(VarType));
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

  function DoScriptMethod(a: TResVar; c: TResVarArray): TResVar;
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
    Assert(a.VarType.BaseType = ltScriptMethod);
    Assert(Length(c) = TLapeType_Method(a.VarType).Params.Count);
    Result := NullResVar;
    e := NullResVar;

    with TLapeType_Method(a.VarType) do
    begin
      if ParamInitialization then
        FCompiler.Emitter._InitStack(ParamSize, Offset, @Self.DocPos);
      for i := 0 to Params.Count - 1 do
      try
        b := NullResVar;
        if (c[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          c[i] := getStackVar(FParams[i], Offset);

        if (Params[i].ParType in Lape_RefParams) then
          if (not isVariable(c[i])) then
            LapeException(lpeVariableExpected)
          else if c[i].VarPos.isPointer then
          begin
            b.VarPos.MemPos := mpStack;
            b.VarType := FCompiler.getBaseType(ltPointer);

            c[i].VarType := b.VarType;
            c[i].VarPos.isPointer := False;

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

      FCompiler.Emitter._IncCall(a, ParamSize, Offset, @Self.DocPos);
    end;
  end;

  function DoImportedMethod(a: TResVar; c: TResVarArray): TResVar;
  var
    i: Integer;
    b, e, tmp: TResVar;
  begin
    Assert(a.VarType.BaseType = ltImportedMethod);
    Assert(Length(c) = TLapeType_Method(a.VarType).Params.Count);
    Result := NullResVar;
    e := NullResVar;

    with TLapeType_Method(a.VarType) do
    begin
      for i := 0 to Params.Count - 1 do
      begin
        b := NullResVar;
        if (c[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          getTempVar(FParams[i], Offset, c[i], 0);

        if (c[i].VarPos.MemPos = mpStack) or (c[i].VarType = nil) then
          LapeException(lpeCannotInvoke, FParams[i].DocPos)
        else if (Params[i].ParType in Lape_RefParams) and (not isVariable(c[i])) then
          LapeException(lpeVariableExpected, FParams[i].DocPos);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(c[i].VarType)) then
          if (not (Params[i].ParType in Lape_RefParams)) and Params[i].VarType.CompatibleWith(c[i].VarType) then
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
        {if c[i].VarPos.isPointer then
        begin
          c[i].VarType := b.VarType;
          c[i].VarPos.isPointer := False;
          b.VarType.Eval(op_Assign, e, b, c[i], Offset, @Self.DocPos);
          c[i].VarPos.isPointer := True;
        end
        else}
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), b, c[i], NullResVar, Offset, @Self.DocPos);
      end;

      if (Res = nil) then
      begin
        setNullResVar(FDest);
        FCompiler.Emitter._InvokeImportedProc(a, Params.Count * SizeOf(Pointer), Offset, @Self.DocPos)
      end
      else
      begin
        Result.VarType := Res;
        if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          FDest := VarResVar;
        getDestVar(FDest, Result, op_Unknown, FCompiler);
        FCompiler.Emitter._InvokeImportedFunc(a, Result, Params.Count * SizeOf(Pointer), Offset, @Self.DocPos)
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
    else if (a.VarType = nil) or (not (a.VarType.BaseType in [ltScriptMethod, ltImportedMethod])) then
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
            if (Params[i].ParType in Lape_RefParams) and (not isVariable(c[i])) then
              if (FParams[i] <> nil) then
                LapeException(lpeVariableExpected, FParams[i].DocPos)
              else
                LapeException(lpeVariableExpected, Self.DocPos);
          end
          else if (i < FParams.Count) and (FParams[i] <> nil) then
            LapeException(lpeNoDefaultForParam, [i + 1], FParams[i].DocPos)
          else
            LapeException(lpeNoDefaultForParam, [i + 1], Self.DocPos)
        else
        begin
          if (FParams[i] is TLapeTree_OpenArray) then
            TLapeTree_OpenArray(FParams[i]).ToType := Params[i].VarType;
          c[i] := NullResVar;
        end;

      if (a.VarType.BaseType = ltScriptMethod) then
        Result := DoScriptMethod(a, c)
      else
        Result := DoImportedMethod(a, c);
    end;

    setNullResVar(a, 1);
    for i := 0 to FParams.Count - 1 do
      setNullResVar(c[i], 1);
  end;
end;

constructor TLapeTree_InternalMethod.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(nil, ACompiler, ADocPos);
end;

function TLapeTree_InternalMethod.isConstant: Boolean;
begin
  Result := (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) and FParams[0].isConstant();
end;

function TLapeTree_InternalMethod_FlowStatement.isConstant: Boolean;
begin
  Result := False;
end;

function TLapeTree_InternalMethod_FlowStatement.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_InternalMethod_Break.Compile(var Offset: Integer): TResVar;
var
  p: TLapeTree_Base;
  i: ILapeTree_CanBreak;
  a, b: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  p := FParent;

  if (not (FParams.Count in [0, 1])) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    a := 1
  else
    with FParams[0].Evaluate() do
    begin
      a := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0].DocPos)
      else if (a < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0].DocPos);
    end;

  b := 1;
  JumpSafe := False;
  while (p <> nil) do
  begin
    if (p.QueryInterface(ILapeTree_CanBreak, i) = 0) then
      if (b < a) then
        Inc(b)
      else
      begin
        i.addBreakStatement(JumpSafe, Offset, @DocPos);
        i := nil;
        Break;
      end
    else if (p is TLapeTree_Try) then
      JumpSafe := True;
    p := p.Parent;
  end;

  if (p = nil) then
    if (b < a) then
      LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
    else
      LapeException(lpeCannotBreak, DocPos);
end;

function TLapeTree_InternalMethod_Continue.Compile(var Offset: Integer): TResVar;
var
  p: TLapeTree_Base;
  i: ILapeTree_CanContinue;
  a, b: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  p := FParent;

  if (not (FParams.Count in [0, 1])) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    a := 1
  else
    with FParams[0].Evaluate() do
    begin
      a := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0].DocPos)
      else if (a < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0].DocPos);
    end;

  b := 1;
  JumpSafe := False;
  while (p <> nil) do
  begin
    if (p.QueryInterface(ILapeTree_CanContinue, i) = 0) then
      if (b < a) then
        Inc(b)
      else
      begin
        i.addContinueStatement(JumpSafe, Offset, @DocPos);
        i := nil;
        Break;
      end
    else if (p is TLapeTree_Try) then
      JumpSafe := True;
    p := p.Parent;
  end;

  if (p = nil) then
    if (b < a) then
      LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
    else
      LapeException(lpeCannotContinue, DocPos);
end;

function TLapeTree_InternalMethod_Exit.Compile(var Offset: Integer): TResVar;
var
  p: TLapeTree_Base;
  i: ILapeTree_CanExit;
  d: TLapeDeclaration;
begin
  Result := NullResVar;
  p := FParent;

  if (FParams.Count <> 0) then
    if (FParams.Count <> 1) then
      LapeException(lpeWrongNumberParams, [1], DocPos)
    else if (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
      LapeException(lpeNoDefaultForParam, [1], DocPos);

  if (FParams.Count = 1) then
    with TLapeTree_Operator.Create(op_Assign, FCompiler, @FParams[0].DocPos) do
    try
      d := FCompiler.getDeclaration('Result');
      if (d = nil) or (not (d is TLapeParameterVar)) then
        LapeException(lpeWrongNumberParams, [0], DocPos);
      Left := TLapeTree_ResVar.Create(getResVar(d as TLapeVar), FCompiler, @FParams[0].DocPos);
      Right := TLapeTree_ResVar.Create(FParams[0].Compile(Offset), FCompiler, @FParams[0].DocPos);
      Compile(Offset);
    finally
      Free();
    end;

  while (p <> nil) do
  begin
    if (p.QueryInterface(ILapeTree_CanExit, i) = 0) then
    begin
      i.addExitStatement(True, Offset, @DocPos);
      i := nil;
      Break;
    end;
    p := p.Parent;
  end;

  if (p = nil) then
    FCompiler.Emitter._JmpSafe(EndJump, Offset, @DocPos);
end;

function TLapeTree_InternalMethod_SizeOf.isConstant: Boolean;
begin
  Result := True;
end;

function TLapeTree_InternalMethod_SizeOf.resType: TLapeType;
begin
  Result := FCompiler.getBaseType(ltInt32);
end;

function TLapeTree_InternalMethod_SizeOf.Evaluate: TLapeGlobalVar;
var
  t: TLapeType;
begin
  if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    t := FParams[0].Compile().VarType
  else
    t := nil;

  if (t <> nil) and (t is TLapeType_Type) then
    t := TLapeType_Type(t).TType;
  if (t = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(t.Size))));
end;

function TLapeTree_InternalMethod_SizeOf.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeCannotEvalRunTime, DocPos);
end;

function TLapeTree_InternalMethod_Ord.resType: TLapeType;
var
  t: TLapeType;
begin
  Result := nil;
  if (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    t := FParams[0].resType();
    if (t <> nil) and (t is TLapeType_Type) then
      t := TLapeType_Type(t).TType;
    if (t <> nil) then
      Result := FCompiler.getBaseType(t.BaseIntType);
  end;
end;

function TLapeTree_InternalMethod_Ord.Evaluate: TLapeGlobalVar;
begin
  if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  setIdent(TLapeTree_VarType.Create(resType(), FCompiler, @DocPos));
  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.Compile(var Offset: Integer): TResVar;
begin
  if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  setIdent(TLapeTree_VarType.Create(resType(), FCompiler, @DocPos));
  Result := inherited;
end;

function TLapeTree_InternalMethod_Low.isConstant: Boolean;
begin
  Result := True;
end;

function TLapeTree_InternalMethod_Low.resType: TLapeType;
var
  t: TLapeType;
begin
  Result := nil;
  if (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    t := FParams[0].resType();
    if (t <> nil) and (t is TLapeType_Type) then
      t := TLapeType_Type(t).TType;
    if (t <> nil) and (t.BaseType in LapeOrdinalTypes) then
      Result := t
    else if (t <> nil) and (t.BaseType in LapeArrayTypes - LapeStringTypes + [ltShortString]) then
      Result := FCompiler.getBaseType(ltInt32);
  end;
end;

function TLapeTree_InternalMethod_Low.Evaluate: TLapeGlobalVar;
var
  t: TLapeType;
begin
  if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    t := FParams[0].Compile().VarType
  else
    t := nil;

  if (t <> nil) and (t is TLapeType_Type) then
    t := TLapeType_Type(t).TType;
  if (t = nil) or (not (t.BaseType in LapeOrdinalTypes + LapeArrayTypes  - LapeStringTypes + [ltShortString])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (t is TLapeType_SubRange) then
    with TLapeType_SubRange(t) do
      Result := TLapeGlobalVar(FCompiler.addManagedVar(t.NewGlobalVarStr(IntToStr(Range.Lo))))
  else if (t is TLapeType_StaticArray) then
    with TLapeType_StaticArray(t) do
      Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(Range.Lo))))
  else if (t is TLapeType_DynArray) then
    Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0')))
  else
    Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(t.BaseIntType).NewGlobalVarP(LapeTypeLow[t.BaseIntType])))
end;

function TLapeTree_InternalMethod_Low.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeCannotEvalRunTime, DocPos);
end;

function TLapeTree_InternalMethod_High.isConstant: Boolean;
var
  t: TLapeType;
begin
  Result := True;
  if (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    t := FParams[0].resType();
    if (t <> nil) and (not (t is TLapeType_Type)) and (t.BaseType = ltDynArray) then
      Result := False;
  end;
end;

function TLapeTree_InternalMethod_High.resType: TLapeType;
var
  t: TLapeType;
begin
  Result := nil;
  if (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    t := FParams[0].resType();
    if (t <> nil) and (t is TLapeType_Type) then
      t := TLapeType_Type(t).TType;
    if (t <> nil) and (t.BaseType in LapeOrdinalTypes) then
      Result := t
    else if (t <> nil) and (t.BaseType in LapeArrayTypes - LapeStringTypes + [ltShortString]) then
      Result := FCompiler.getBaseType(ltInt32);
  end;
end;

function TLapeTree_InternalMethod_High.Evaluate: TLapeGlobalVar;
var
  t: TLapeType;
begin
  if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    t := FParams[0].Compile().VarType
  else
    t := nil;

  if (t <> nil) and (t is TLapeType_Type) then
    t := TLapeType_Type(t).TType;
  if (t = nil) or (not (t.BaseType in LapeOrdinalTypes + LapeArrayTypes - LapeStringTypes + [ltShortString])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (t is TLapeType_SubRange) then
    with TLapeType_SubRange(t) do
      Result := TLapeGlobalVar(FCompiler.addManagedVar(t.NewGlobalVarStr(IntToStr(Range.Hi))))
  else if (t is TLapeType_StaticArray) then
    with TLapeType_StaticArray(t) do
      Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(Range.Hi))))
  else if (t is TLapeType_DynArray) then
    LapeException(lpeCannotEvalConst, DocPos)
  else
    Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(t.BaseIntType).NewGlobalVarP(LapeTypeHigh[t.BaseIntType])))
end;

function TLapeTree_InternalMethod_High.Compile(var Offset: Integer): TResVar;
begin
  if isConstant() or (resType() = nil) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  Result := NullResVar;
  Result.VarPos.MemPos := mpStack;
  Result.VarType := Compiler.getBaseType(ltPointer);
  FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, FParams[0].Compile(Offset), NullResVar, Offset, @Self.DocPos);

  Result := NullResVar;
  Result.VarType := FCompiler.getBaseType(ltInt32);
  if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    FDest := VarResVar;
  getDestVar(FDest, Result, op_Unknown, FCompiler);
  FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!high') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self.DocPos);
end;

function TLapeTree_InternalMethod_Length.isConstant: Boolean;
var
  t: TLapeType;
begin
  Result := False;
  if (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    t := FParams[0].resType();
    if (t <> nil) and (t is TLapeType_Type) then
      t := TLapeType_Type(t).TType;
    if (t <> nil) and (t.BaseType  = ltStaticArray) then
      Result := True;
  end;
end;

function TLapeTree_InternalMethod_Length.resType: TLapeType;
var
  t: TLapeType;
begin
  Result := FCompiler.getBaseType(ltInt32);
  if (FParams.Count = 1) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    t := FParams[0].resType();
    if (t <> nil) and (t is TLapeType_Type) then
      t := TLapeType_Type(t).TType;
    if (t <> nil) and (t.BaseType = ltShortString) then
      Result := FCompiler.getBaseType(ltUInt8);
  end;
end;

function TLapeTree_InternalMethod_Length.Evaluate: TLapeGlobalVar;
var
  t: TLapeType;
begin
  if (not isConstant()) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    t := FParams[0].Compile().VarType
  else
    t := nil;

  if (t <> nil) and (t is TLapeType_Type) then
    t := TLapeType_Type(t).TType;
  if (t = nil) or (t.BaseType <> ltStaticArray) then
    LapeException(lpeInvalidEvaluation, DocPos);

  with TLapeType_StaticArray(t) do
    Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(Range.Hi - Range.Lo + 1))));
end;

function TLapeTree_InternalMethod_Length.Compile(var Offset: Integer): TResVar;
var
  a: TResVar;
begin
  if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  a := FParams[0].Compile(Offset);
  if (a.VarType = nil) or (not (a.VarType.BaseType in LapeArrayTypes - [ltStaticArray])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (a.VarType.BaseType = ltShortString) then
  begin
    SetNullResVar(FDest);
    Result := a;
    Result.VarType := FCompiler.getBaseType(ltUInt8);
  end
  else
  begin
    Result := NullResVar;
    Result.VarPos.MemPos := mpStack;
    Result.VarType := Compiler.getBaseType(ltPointer);
    FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, a, NullResVar, Offset, @Self.DocPos);

    Result := NullResVar;
    Result.VarType := FCompiler.getBaseType(ltInt32);
    if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      FDest := VarResVar;
    getDestVar(FDest, Result, op_Unknown, FCompiler);
    if (a.VarType.BaseType in LapeStringTypes) then
      FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!strlen') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self.DocPos)
    else
      FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!length') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self.DocPos);
  end;
end;

function TLapeTree_InternalMethod_Succ.resType: TLapeType;
begin
  Result := nil;
  if (FParams.Count in [1, 2]) and (FParams[0] <> nil) and (FParams[0].ClassType <> TLapeTree_ExprBase) then
  begin
    Result := FParams[0].resType();
    if (Result <> nil) and (Result is TLapeType_Type) then
      Result := TLapeType_Type(Result).TType;
  end;
end;

function TLapeTree_InternalMethod_Succ.Evaluate: TLapeGlobalVar;
var
  a, b: TLapeGlobalVar;
  f, d: TLapeTree_ExprBase;
  t: TLapeType;
begin
  Result := nil;
  b := nil;
  d := nil;

  if (FParams.Count = 2) then
    if (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
      LapeException(lpeNoDefaultForParam, [1], DocPos)
    else if (FParams[1] = nil) or (FParams[1].ClassType = TLapeTree_ExprBase) then
      LapeException(lpeNoDefaultForParam, [2], DocPos)
    else
      d := FParams.Delete(1)
  else if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  try
    if (d <> nil) then
      b := d.Evaluate()
    else
      b := FCompiler.getBaseType(ltInt8).NewGlobalVarStr('1');

    a := FParams[0].Evaluate();
    if (a = nil) or (a.VarType = nil) or (a.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, DocPos);

    if (a.VarType.BaseType = ltPointer) then
      with TLapeTree_Operator.Create(op_Index, FCompiler, @DocPos) do
      try
        Left := TLapeTree_GlobalVar.Create(a, FCompiler, @DocPos);
        Right := TLapeTree_GlobalVar.Create(b, FCompiler, @DocPos);
        Result := Evaluate();
      finally
        Free();
      end
    else
    begin
      t := a.VarType;
      a := FCompiler.getBaseType(a.VarType.BaseIntType).NewGlobalVarP(a.Ptr);
      with TLapeTree_Operator.Create(op_Plus, FCompiler, @DocPos) do
      try
        Left := TLapeTree_GlobalVar.Create(a, FCompiler, @DocPos);
        Right := TLapeTree_GlobalVar.Create(b, FCompiler, @DocPos);
        Result := Evaluate();

        f := FParams.Delete(0);
        addParam(TLapeTree_GlobalVar.Create(Result, FCompiler, @DocPos));
        try
          setIdent(TLapeTree_VarType.Create(a.VarType, FCompiler, @DocPos));
          Result := inherited;

          a.Free();
          a := Result;
          Result := t.NewGlobalVarP(a.Ptr);
        finally
          FParams.Delete(0).Free();
          addParam(f);
        end;
      finally
        a.Free();
        Free();
      end;
    end;
  finally
    if (d <> nil) then
      addParam(d)
    else if (b <> nil) then
      b.Free();
  end;
end;

function TLapeTree_InternalMethod_Succ.Compile(var Offset: Integer): TResVar;
var
  a, b, c: TResVar;
  f, d: TLapeTree_ExprBase;
  t: TLapeType;
begin
  b := NullResVar;
  d := nil;
  if (FParams.Count = 2) then
    if (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
      LapeException(lpeNoDefaultForParam, [1], DocPos)
    else if (FParams[1] = nil) or (FParams[1].ClassType = TLapeTree_ExprBase) then
      LapeException(lpeNoDefaultForParam, [2], DocPos)
    else
      d := FParams.Delete(1)
  else if (FParams.Count <> 1) or (FParams[0] = nil) or (FParams[0].ClassType = TLapeTree_ExprBase) then
    LapeException(lpeWrongNumberParams, [1], DocPos);

  try
    if (d <> nil) then
      b := d.Compile(Offset)
    else
      b := getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt8).NewGlobalVarStr('1')));

    a := FParams[0].Compile(Offset);
    if (a.VarType = nil) or (a.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, DocPos);

    if (a.VarType.BaseType = ltPointer) then
      with TLapeTree_Operator.Create(op_Index, FCompiler, @DocPos) do
      try
        Dest := Self.Dest;
        Left := TLapeTree_ResVar.Create(a, FCompiler, @DocPos);
        Right := TLapeTree_ResVar.Create(b, FCompiler, @DocPos);
        Result := Compile(Offset);
        Self.Dest := Dest;
      finally
        Free();
      end
    else
    begin
      t := a.VarType;
      a.VarType := FCompiler.getBaseType(a.VarType.BaseIntType);
      with TLapeTree_Operator.Create(op_Plus, FCompiler, @DocPos) do
      try
        Dest := Self.Dest;
        Left := TLapeTree_ResVar.Create(a, FCompiler, @DocPos);
        Right := TLapeTree_ResVar.Create(b, FCompiler, @DocPos);
        Result := Compile(Offset);

        if (Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
          c := Dest
        else
          c := NullResVar;

        f := FParams.Delete(0);
        addParam(TLapeTree_ResVar.Create(Result, FCompiler, @DocPos));
        try
          setIdent(TLapeTree_VarType.Create(a.VarType, FCompiler, @DocPos));
          Result := inherited;
          Result.VarType := t;
          if (Self.Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
            Self.Dest := c;
        finally
          FParams.Delete(0).Free();
          addParam(f);
        end;
      finally
        Free();
      end;
    end;
  finally
    if (d <> nil) then
      addParam(d)
    else
      setNullResVar(b);
  end;
end;

function TLapeTree_InternalMethod_Pred.Evaluate: TLapeGlobalVar;
var
  b: TLapeTree_Operator;
begin
  b := nil;

  if (FParams.Count < 2) then
    addParam(TLapeTree_GlobalVar.Create(FCompiler.getBaseType(ltInt8).NewGlobalVarStr('-1'), FCompiler, @DocPos))
  else if (FParams[1] <> nil) and (FParams[1].ClassType <> TLapeTree_ExprBase) and (FParams.Count = 2) then
  begin
    b := TLapeTree_Operator.Create(op_UnaryMinus, FCompiler, @DocPos);
    b.Left := FParams.Delete(1);
    addParam(b);
  end;

  Result := inherited;

  if (FParams.Count = 2) then
  begin
    if (b <> nil) then
      addParam(b.Left)
    else
      TLapeTree_GlobalVar(FParams[1]).GlobalVar.Free();
    FParams.Delete(1).Free();
  end;
end;

function TLapeTree_InternalMethod_Pred.Compile(var Offset: Integer): TResVar;
var
  b: TLapeTree_Operator;
begin
  b := nil;

  if (FParams.Count < 2) then
    addParam(TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt8).NewGlobalVarStr('-1'))), FCompiler, @DocPos))
  else if (FParams[1] <> nil) and (FParams[1].ClassType <> TLapeTree_ExprBase) and (FParams.Count = 2) then
  begin
    b := TLapeTree_Operator.Create(op_UnaryMinus, FCompiler, @DocPos);
    b.Left := FParams.Delete(1);
    addParam(b);
  end;

  Result := inherited;

  if (FParams.Count = 2) then
  begin
    if (b <> nil) then
      addParam(b.Left);
    FParams.Delete(1).Free();
  end;
end;

function TLapeTree_InternalMethod_Inc.isConstant: Boolean;
begin
  Result := False;
end;

function TLapeTree_InternalMethod_Inc.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotEvalConstProc, DocPos);
end;

function TLapeTree_InternalMethod_Inc.Compile(var Offset: Integer): TResVar;
var
  a, b: TLapeTree_ExprBase;
  c: TLapeTree_Operator;
begin
  if (not (FParams.Count in [1, 2])) then
    lapeException(lpeWrongNumberParams, [1], DocPos);
  a := FParams.Delete(0);
  if (FParams.Count > 0) then
    b := FParams.Delete(0)
  else
    b := nil;
  c := nil;

  try
    Result := a.Compile(Offset);
    c := TLapeTree_Operator.Create(op_Assign, FCompiler, @DocPos);
    c.Left := TLapeTree_ResVar.Create(Result, FCompiler, @DocPos);
    c.Right := TLapeTree_InternalMethod_Succ.Create(FCompiler, @DocPos);
    TLapeTree_InternalMethod_Succ(c.Right).addParam(TLapeTree_ResVar.Create(Result, FCompiler, @DocPos));
    if (b <> nil) then
      TLapeTree_InternalMethod_Succ(c.Right).addParam(b);
    Result := c.Compile(Offset);
  finally
    addParam(a);
    if (b <> nil) then
      addParam(b);
    if (c <> nil) then
      c.Free();
  end;
end;

function TLapeTree_InternalMethod_Dec.isConstant: Boolean;
begin
  Result := False;
end;

function TLapeTree_InternalMethod_Dec.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotEvalConstProc, DocPos);
end;

function TLapeTree_InternalMethod_Dec.Compile(var Offset: Integer): TResVar;
var
  a, b: TLapeTree_ExprBase;
  c: TLapeTree_Operator;
begin
  if (not (FParams.Count in [1, 2])) then
    lapeException(lpeWrongNumberParams, [1], DocPos);
  a := FParams.Delete(0);
  if (FParams.Count > 0) then
    b := FParams.Delete(0)
  else
    b := nil;
  c := nil;

  try
    Result := a.Compile(Offset);
    c := TLapeTree_Operator.Create(op_Assign, FCompiler, @DocPos);
    c.Left := TLapeTree_ResVar.Create(Result, FCompiler, @DocPos);
    c.Right := TLapeTree_InternalMethod_Pred.Create(FCompiler, @DocPos);
    TLapeTree_InternalMethod_Pred(c.Right).addParam(TLapeTree_ResVar.Create(Result, FCompiler, @DocPos));
    if (b <> nil) then
      TLapeTree_InternalMethod_Pred(c.Right).addParam(b);
    Result := c.Compile(Offset);
  finally
    addParam(a);
    if (b <> nil) then
      addParam(b);
    if (c <> nil) then
      c.Free();
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
    ((FOperatorType in [op_Dot, op_Index]) and (FLeft <> nil) and (FLeft is TLapeTree_GlobalVar) and (FRight <> nil) and FRight.isConstant()) or
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
      Exit(FCompiler.getBaseType(ltEvalBool))
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
      v.VarType := FCompiler.getBaseType(ltEvalBool);
      Result := v.VarType;

      if (FDest.VarType <> nil) and (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and v.VarType.Equals(FDest.VarType) then
        v := FDest
      else
      begin
        FDest := NullResVar;
        v := getResVar(FCompiler.getTempVar(ltEvalBool));
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

function TLapeTree_ResVar.isConstant: Boolean;
begin
  Result := (FResVar.VarPos.MemPos = mpMem) and (FResVar.VarPos.GlobalVar <> nil) and FResVar.VarPos.GlobalVar.isConstant;
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
  Assert(AMethod.VarType is TLapeType_Method);
  Assert(AStackInfo <> nil);
  inherited Create(ACompiler, ADocPos);

  FreeStackInfo := True;
  FMethod := AMethod;
  FStackInfo := AStackInfo;
  FExitStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore);
end;

destructor TLapeTree_Method.Destroy;
begin
  setStatements(nil);
  if FreeStackInfo then
    FStackInfo.Free();
  FExitStatements.Free();
  inherited;
end;

function TLapeTree_Method.Compile(var Offset: Integer): TResVar;
var
  if_o, i, co: Integer;
begin
  Assert(FCompiler <> nil);
  Assert(FMethod <> nil);
  FExitStatements.Clear();
  if_o := FCompiler.Emitter._JmpR(0, Offset, @DocPos);
  PUInt32(FMethod.Ptr)^ := FCompiler.Emitter.getCodeOffset(Offset);
  FCompiler.Emitter.addCodePointer(FMethod.Ptr);
  FCompiler.IncStackInfo(FStackInfo, Offset, True, @DocPos);
  Result := FStatements.Compile(Offset);

  for i := 0 to FExitStatements.Count - 1 do
    with FExitStatements[i] do
    begin
      co := CodeOffset;
      FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos);
    end;

  FCompiler.DecStackInfo(Offset, True, True, False, @DocPos);
  FCompiler.Emitter._JmpR(Offset - if_o, if_o, @DocPos);
end;

procedure TLapeTree_Method.addExitStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  FExitStatements.add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe));
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
  if (FStartBodyOffset <= 0) then
    FStartBodyOffset := FCompiler.Emitter.CheckOffset(Offset);

  if (FBody <> nil) then
    Result := FBody.Compile(Offset)
  else
    Result := NullResVar;
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
  Assert(FCondition <> nil);

  Result := NullResVar;
  FStartBodyOffset := 0;
  e := NullResVar;

  cnd := FCondition.Compile(Offset);
  if (cnd.VarType = nil) or (not (cnd.VarType.BaseType in LapeIfTypes)) then
    LapeException(lpeInvalidCondition, FCondition.DocPos);

  if (not (cnd.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmp := cnd;
    if FCompiler.getBaseType(ltEvalBool).CompatibleWith(cnd.VarType) then
      cnd := FCompiler.getBaseType(ltEvalBool).Eval(op_Assign, e, FCompiler.getTempStackVar(ltEvalBool), cnd, Offset, @FCondition.DocPos)
    else if (cnd.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition.DocPos)
    else
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition.DocPos);
    setNullResVar(tmp);
  end;

  if_o := FCompiler.Emitter._JmpRIfNot(0, cnd, Offset, @DocPos);
  if (FBody <> nil) or (FElse = nil) then
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
  i, co: Integer;
begin
  Result := inherited;

  if (FContinueStatements <> nil) then
  begin
    FContinueCount := FContinueStatements.Count;
    for i := 0 to FContinueCount - 1 do
      with FContinueStatements[i] do
      begin
        co := CodeOffset;
        if JumpSafe then
          FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
        else
          FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
      end;
  end;

  e := NullResVar;
  cnd := FCondition.Compile(Offset);

  if (not (cnd.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmp := cnd;
    if FCompiler.getBaseType(ltEvalBool).CompatibleWith(cnd.VarType) then
      cnd := FCompiler.getBaseType(ltEvalBool).Eval(op_Assign, e, FCompiler.getTempStackVar(ltEvalBool), cnd, Offset, @FCondition.DocPos)
    else if (cnd.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition.DocPos)
    else
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition.DocPos);
    setNullResVar(tmp);
  end;

  FCompiler.Emitter._JmpRIf(FStartBodyOffset - Offset, cnd, Offset, @DocPos);
  setNullResVar(cnd);
end;

constructor TLapeTree_While.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBreakStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore);
end;

destructor TLapeTree_While.Destroy;
begin
  FBreakStatements.Free();
  FContinueStatements.Free();
  inherited;
end;

function TLapeTree_While.Compile(var Offset: Integer): TResVar;
var
  i, co: Integer;
begin
  FContinueCount := 0;
  FBreakStatements.Clear();
  FContinueStatements.Clear();
  Result := inherited;

  if (FContinueCount < FContinueStatements.Count) then
    LapeException(lpeStatementNotAllowed, FContinueStatements[FContinueCount].DocPos);

  for i := 0 to FBreakStatements.Count - 1 do
    with FBreakStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;
end;

procedure TLapeTree_While.addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FBreakStatements.add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FBreakStatements.add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
end;

procedure TLapeTree_While.addContinueStatement(JumpSafe: Boolean;var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FContinueStatements.add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FContinueStatements.add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
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

function TLapeTree_For.CompileBody(var Offset: Integer): TResVar;
var
  i, co: Integer;
  m: TLapeTree_InternalMethod;
  b: TLapeTree_Base;
  s: TLapeFlowStatementList;
begin
  Assert((FCondition <> nil) and (FCondition is TLapeTree_Operator));
  Assert((TLapeTree_Operator(FCondition).Right <> nil) and (TLapeTree_Operator(FCondition).Right is TLapeTree_ResVar));

  FStartBodyOffset := FCompiler.Emitter.CheckOffset(Offset);
  if (FBody <> nil) then
    Result := FBody.Compile(Offset)
  else
    Result := NullResVar;

  FContinueCount := FContinueStatements.Count;
  for i := 0 to FContinueCount - 1 do
    with FContinueStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  if WalkDown then
    m := TLapeTree_InternalMethod_Dec.Create(FCompiler, @DocPos)
  else
    m := TLapeTree_InternalMethod_Inc.Create(FCompiler, @DocPos);
  try
    m.addParam(TLapeTree_ResVar.Create(TLapeTree_ResVar(TLapeTree_Operator(FCondition).Left).ResVar, FCompiler, @FCounter.DocPos));
    if (FStep <> nil) then
    begin
      m.addParam(FStep);
      m.Compile(Offset);
      Step := m.Params.Delete(1);
    end
    else
      m.Compile(Offset);
  finally
    m.Free();
  end;

  b := FBody;
  s := FContinueStatements;
  try
    FBody := nil;
    FContinueStatements := nil;
    inherited;
  finally
    FBody := b;
    FContinueStatements := s;
  end;
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
  cnt, lim: TResVar;
  v: TLapeVar;
begin
  Result := NullResVar;
  Assert(FCondition = nil);
  Assert(FCounter <> nil);
  Assert(FLimit <> nil);

  v := nil;
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

    if WalkDown then
      FCondition := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, FCompiler, @DocPos)
    else
      FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, FCompiler, @DocPos);
    TLapeTree_Operator(FCondition).Left := TLapeTree_ResVar.Create(cnt, FCompiler, @FCounter.DocPos);
    TLapeTree_Operator(FCondition).Right := TLapeTree_ResVar.Create(lim, FCompiler, @FLimit.DocPos);

    Result := inherited;
  finally
    setCondition(nil);
    if (v <> nil) then
    begin
      v.isConstant := True;
      setNullResVar(cnt, 2);
    end
    else
      setNullResVar(cnt, 1);
    setNullResVar(lim, 2);
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

  FBreakStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore);
end;

destructor TLapeTree_Repeat.Destroy;
begin
  setCondition(nil);
  setBody(nil);
  FBreakStatements.Free();
  FContinueStatements.Free();
  inherited;
end;

function TLapeTree_Repeat.Compile(var Offset: Integer): TResVar;
var
  cnd, tmp, e: TResVar;
  i, o, co: Integer;
begin
  FBreakStatements.Clear();
  FContinueStatements.Clear();

  Result := NullResVar;
  o := FCompiler.Emitter.CheckOffset(Offset);
  if (FBody <> nil) then
    Result := FBody.Compile(Offset);

  for i := 0 to FContinueStatements.Count - 1 do
    with FContinueStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  e := NullResVar;
  cnd := FCondition.Compile(Offset);

  if (not (cnd.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmp := cnd;
    if FCompiler.getBaseType(ltEvalBool).CompatibleWith(cnd.VarType) then
      cnd := FCompiler.getBaseType(ltEvalBool).Eval(op_Assign, e, FCompiler.getTempStackVar(ltEvalBool), cnd, Offset, @FCondition.DocPos)
    else if (cnd.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition.DocPos)
    else
      cnd := cnd.VarType.Eval(op_cmp_NotEqual, e, cnd, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition.DocPos);
    setNullResVar(tmp);
  end;
  FCompiler.Emitter._JmpRIfNot(o - Offset, cnd, Offset, @DocPos);

  for i := 0 to FBreakStatements.Count - 1 do
    with FBreakStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  setNullResVar(cnd);
end;

procedure TLapeTree_Repeat.addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FBreakStatements.add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FBreakStatements.add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
end;

procedure TLapeTree_Repeat.addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FContinueStatements.add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FContinueStatements.add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
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
  o_try, o_except, o_jmp: Integer;
begin
  Result := NullResVar;
  Assert((FExcept <> nil) or (FFinally <> nil));

  o_try := FCompiler.Emitter._IncTry(0, 0, Offset, @DocPos);
  o_except := 0;
  if (FBody <> nil) then
    Result := FBody.Compile(Offset);

  FCompiler.Emitter._DecTry(Offset, @DocPos);
  if (FExcept <> nil) then
  begin
    o_jmp := FCompiler.Emitter._JmpR(0, Offset, @FExcept.DocPos);
    if (FFinally = nil) then
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoFinally, o_try, @DocPos)
    else
      o_except := Offset;
    FCompiler.Emitter._CatchException(Offset, @FExcept.DocPos);
    FExcept.Compile(Offset);
    FCompiler.Emitter._JmpR(Offset - o_jmp, o_jmp, @FExcept.DocPos);
  end;
  if (FFinally <> nil) then
  begin
    if (o_except <> 0) then
      FCompiler.Emitter._IncTry(Offset - o_try, Offset - o_except, o_try, @DocPos)
    else
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoExcept, o_try, @DocPos);
    FFinally.Compile(Offset);
    FCompiler.Emitter._EndTry(Offset, @FFinally.DocPos);
  end;
end;

end.

