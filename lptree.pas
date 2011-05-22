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

  TLapeTree_Base = class(TLapeBaseDeclClass)
  protected
    FParent: TLapeTree_Base;
    FCompiler: TLapeCompilerBase;

    function getDocPos: TDocPos; override;
    procedure setParent(Parent: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); virtual;
  public
    _DocPos: TDocPos;

    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    destructor Destroy; override;

    function Compile(var Offset: Integer): TResVar; overload; virtual;
    function Compile: TResVar; overload; virtual;

    property Parent: TLapeTree_Base read FParent write setParent;
    property Compiler: TLapeCompilerBase read FCompiler;
  end;

  TLapeTree_ExprBase = class(TLapeTree_Base)
  public
    function isConstant: Boolean; virtual;
    function resType: TLapeType; virtual;
    function Evaluate: TLapeGlobalVar; virtual;
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
    function addValue(Val: TLapeTree_Base): Integer; virtual;

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

  TLapeTree_InternalMethod_IsScriptMethod = class(TLapeTree_InternalMethod)
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
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

  TLapeTree_WithVar = class(TLapeTree_ExprBase)
  protected
    FWithDeclRec: TLapeWithDeclRec;
  public
    constructor Create(AWithDeclRec: TLapeWithDeclRec; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;

    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property WithDeclRec: TLapeWithDeclRec read FWithDeclRec;
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
  TLapeVarDeclList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeVarDecl>;

  TLapeTree_VarList = class(TLapeTree_Base)
  protected
    FVars: TLapeVarDeclList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addVar(AVar: TLapeVarDecl): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    property Vars: TLapeVarDeclList read FVars;
  end;

  TLapeTree_With = class(TLapeTree_Base)
  protected
    FWithList: TLapeExpressionList;
    FVarList: array of TLapeVar;
    FBody: TLapeTree_Base;
    procedure setBody(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclRec; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    property WithList: TLapeExpressionList read FWithList;
    property Body: TLapeTree_Base read FBody write setBody;
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
procedure setExpectedType(Node: TLapeTree_Base; ToType: TLapeType); {$IFDEF Lape_Inline}inline;{$ENDIF}
function isEmptyNode(Node: TLapeTree_Base): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
function getTempVar(Node: TLapeTree_Base; var Offset: Integer; out v: TResVar; Lock: Integer = 1): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
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
        setExpectedType(FRight, FLeft.resType());
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

procedure setExpectedType(Node: TLapeTree_Base; ToType: TLapeType);
begin
  if (Node <> nil) and (Node is TLapeTree_OpenArray) and (ToType <> nil) then
    TLapeTree_OpenArray(Node).ToType := ToType;
end;

function isEmptyNode(Node: TLapeTree_Base): Boolean;
begin
  Result := (Node = nil) or (Node.ClassType = TLapeTree_Base) or (Node.ClassType = TLapeTree_ExprBase);
end;

function getTempVar(Node: TLapeTree_Base; var Offset: Integer; out v: TResVar; Lock: Integer = 1): Boolean;
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

function TLapeTree_Base.getDocPos: TDocPos;
begin
  Result := _DocPos;
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
    _DocPos := ADocPos^
  else
    _DocPos := NullDocPos;
end;

destructor TLapeTree_Base.Destroy;
begin
  setParent(nil);
  inherited;
end;

function TLapeTree_Base.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeImpossible);
end;

function TLapeTree_Base.Compile: TResVar;
var
  o: Integer;
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

function TLapeTree_ExprBase.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeImpossible);
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

function TLapeTree_OpenArray.addValue(Val: TLapeTree_Base): Integer;
begin
  if (Val <> nil) then
  begin
    Result := FValues.add(Val);
    Val.Parent := Self;
  end
  else
    Result := -1;
end;

function TLapeTree_OpenArray.canCast: Boolean;
var
  i: Integer;
  CastTo: TLapeType;
begin
  if (ToType = nil) and (FDest.VarType <> nil) then
    ToType := FDest.VarType;
  if (ToType = nil) then
    ToType := resType();
  if (ToType = nil) then
    Exit(False);

  CastTo := nil;
  if (ToType is TLapeType_Set) then
    CastTo := TLapeType_Set(ToType).Range
  else if (ToType is TLapeType_DynArray) then
    CastTo := TLapeType_DynArray(ToType).PType;
  if (CastTo = nil) xor (ToType is TLapeType_Record) then
    Exit(False);

  for i := 0 to FValues.Count - 1 do
  begin
    if isEmptyNode(FValues[i]) then
      Continue
    else if (ToType is TLapeType_Record) then
      CastTo := TLapeType_Record(ToType).FieldMap.ItemsI[i].FieldType;

    if (CastTo = nil) then
      Exit(False)
    else
      setExpectedType(FValues[i], CastTo);

    if (FValues[i] is TLapeTree_ExprBase) and CastTo.CompatibleWith(TLapeTree_ExprBase(FValues[i]).resType()) then
      {nothing}
    else if (FValues[i] is TLapeTree_Range) and
      CastTo.CompatibleWith(TLapeTree_Range(FValues[i]).Lo.resType()) and
      CastTo.CompatibleWith(TLapeTree_Range(FValues[i]).Hi.resType())
    then
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
    if isEmptyNode(FValues[i]) then
      {nothing}
    else if (FValues[i] is TLapeTree_ExprBase) and TLapeTree_ExprBase(FValues[i]).isConstant() then
      {nothing}
    else if (FValues[i] is TLapeTree_Range) and
      TLapeTree_Range(FValues[i]).Lo.isConstant() and
      TLapeTree_Range(FValues[i]).Hi.isConstant()
    then
      {nothing}
    else
      Exit(False);
  Result := True;
end;

function TLapeTree_OpenArray.resType: TLapeType;
var
  i: Integer;
  Res: TLapeType;
  Range: TLapeRange;

  function determineArrType(Typ: TLapeType): TLapeType;
  begin
    if (Typ = nil) then
      Exit(nil)
    else if (Typ.BaseType in LapeIntegerTypes - [ltUInt64, ltInt64]) then
      Result := FCompiler.getBaseType(ltInt32)
    //else if (Typ.BaseType in LapeRealTypes) then
    //  Result := FCompiler.getBaseType(ltExtended)
    //else if (Typ.BaseType in LapeBoolTypes) then
    //  Result := FCompiler.getBaseType(ltBoolean)
    //else if (Typ.BaseType in LapeStringTypes) then
    //  Result := FCompiler.getBaseType(ltString)
    else
      Result := Typ;
  end;

begin
  Result := ToType;
  if (Result = nil) then
    if (FValues.Count > 0) then
    begin
      Range.Lo := 0;
      Range.Hi := FValues.Count - 1;

      for i := 0 to Range.Hi do
      begin
        if isEmptyNode(FValues[i]) then
          Continue
        else if (FValues[i] is TLapeTree_Range) and (TLapeTree_Range(FValues[i]).Hi <> nil) then
        begin
          Res := TLapeTree_Range(FValues[i]).Hi.resType();
          if (Res = nil) or (not (Res is TLapeType_SubRange)) or ((Result <> nil) and (not Res.CompatibleWith(Result))) then
            Exit(nil);
          Exit(FCompiler.addManagedType(TLapeType_Set.Create(TLapeType_SubRange(ResType), FCompiler, '', @_DocPos)))
        end
        else if (not (FValues[i] is TLapeTree_ExprBase)) then
          Exit(nil)
        else
          Res := determineArrType(TLapeTree_ExprBase(FValues[i]).ResType());

        if (Result = nil) then
          Result := Res
        else if (Res <> nil) and Res.Equals(Result) then
          {nothing}
        else if (Res <> nil) and (Res.BaseType > Result.BaseType) and Res.CompatibleWith(Result) then
          Result := Res
        else if (Res <> nil) and (Result.BaseType >= Res.BaseType) and Result.CompatibleWith(Res) then
          {nothing}
        else if FCompiler.getBaseType(ltVariant).CompatibleWith(Res) and FCompiler.getBaseType(ltVariant).CompatibleWith(Result) then
          Result := FCompiler.getBaseType(ltVariant)
        else
        begin
          Result := nil;
          Break;
        end;
      end;

      if (Result <> nil) then
        if (Result is TLapeType_Enum) then
          Result := FCompiler.addManagedType(TLapeType_Set.Create(TLapeType_Enum(Result), FCompiler, '', @_DocPos))
        else
          Result := FCompiler.addManagedType(TLapeType_StaticArray.Create(Range, Result, FCompiler, '', @_DocPos));
    end
    else
      Result := TLapeType_StaticArray.Create(NullRange, FCompiler.getBaseType(ltVariant), FCompiler, '', @_DocPos);
end;

function TLapeTree_OpenArray.Evaluate: TLapeGlobalVar;

  procedure doSet;
  var
    i, ii: Integer;
    FieldVar, tmpVar: TLapeGlobalVar;
  begin
    FieldVar := nil;
    tmpVar := nil;

    for i := 0 to FValues.Count - 1 do
      if isEmptyNode(FValues[i]) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (FValues[i] is TLapeTree_ExprBase) then
      begin
        tmpVar := Result;
        Result := ToType.EvalConst(op_Plus, tmpVar, TLapeTree_ExprBase(FValues[i]).Evaluate());
        tmpVar.Free();
      end
      else if (FValues[i] is TLapeTree_Range) then
      try
        FieldVar := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@ii);
        for ii := TLapeTree_Range(FValues[i]).Lo.Evaluate().AsInteger to TLapeTree_Range(FValues[i]).Hi.Evaluate().AsInteger do
        begin
          tmpVar := Result;
          Result := ToType.EvalConst(op_Plus, Result, FieldVar);
          tmpVar.Free();
        end;
      finally
        FieldVar.Free();
      end
      else
        LapeException(lpeInvalidCast, DocPos);
  end;

  procedure doStaticArray;
  var
    i, ii: Integer;
    FieldVar, tmpVar, Counter: TLapeGlobalVar;
    CounterInt: Integer;
  begin
    FieldVar := nil;
    tmpVar := nil;
    try
      CounterInt := TLapeType_StaticArray(ToType).Range.Lo;
      Counter := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@CounterInt);

      for i := 0 to FValues.Count - 1 do
      begin
        if isEmptyNode(FValues[i]) then
          LapeException(lpeInvalidCast, FValues[i].DocPos)
        else if (FValues[i] is TLapeTree_ExprBase) then
        try
          try
            FieldVar := ToType.EvalConst(op_Index, Result, Counter);
            FieldVar.VarType.EvalConst(op_Assign, FieldVar, TLapeTree_ExprBase(FValues[i]).Evaluate());
          except on E: lpException do
            LapeException(E.Message, FValues[i].DocPos);
          end;
        finally
          if (FieldVar <> nil) then
            FreeAndNil(FieldVar);
        end
        else if (FValues[i] is TLapeTree_Range) then
        try
          tmpVar := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@ii);
          for ii := TLapeTree_Range(FValues[i]).Lo.Evaluate().AsInteger to TLapeTree_Range(FValues[i]).Hi.Evaluate().AsInteger do
          try
            try
              FieldVar := ToType.EvalConst(op_Index, Result, Counter);
              FieldVar.VarType.EvalConst(op_Assign, FieldVar, tmpVar);
            except on E: lpException do
              LapeException(E.Message, FValues[i].DocPos);
            end;
          finally
            Inc(CounterInt);
            if (FieldVar <> nil) then
              FreeAndNil(FieldVar);
          end;
        finally
          Dec(CounterInt);
          tmpVar.Free();
        end
        else
          LapeException(lpeInvalidCast, FValues[i].DocPos);
        Inc(CounterInt);
      end;

      if (CounterInt <> TLapeType_StaticArray(ToType).Range.Hi - TLapeType_StaticArray(ToType).Range.Lo + 1) then
        LapeException(lpeInvalidRange, DocPos);
    finally
      Counter.Free();
    end
  end;

  procedure doRecord;
  var
    i: Integer;
    FieldVar, tmpVar: TLapeGlobalVar;
  begin
    FieldVar := nil;
    tmpVar := nil;

    if (FValues.Count > TLapeType_Record(ToType).FieldMap.Count) then
      LapeException(lpeInvalidRange, DocPos);
    for i := 0 to FValues.Count - 1 do
      if (FValues[i] is TLapeTree_ExprBase) then
      try
        tmpVar := FCompiler.getBaseType(ltString).NewGlobalVarStr(TLapeType_Record(ToType).FieldMap.Key[i]);
        try
          FieldVar := ToType.EvalConst(op_Dot, Result, tmpVar);
          FieldVar.VarType.EvalConst(op_Assign, FieldVar, TLapeTree_ExprBase(FValues[i]).Evaluate());
        except on E: lpException do
          LapeException(E.Message, FValues[i].DocPos);
        end;
      finally
        if (FieldVar <> nil) then
          FreeAndNil(FieldVar);
        if (tmpVar <> nil) then
          FreeAndNil(tmpVar);
      end
      else if isEmptyNode(FValues[i]) then
        LapeException(lpeInvalidCast, FValues[i].DocPos);
  end;

begin
  if (not canCast()) then
    LapeException(lpeInvalidEvaluation, DocPos);

  Result := ToType.NewGlobalVarP();
  try
    if (ToType is TLapeType_Set) then
      doSet()
    else if (ToType is TLapeType_StaticArray) then
      doStaticArray()
    else if (ToType is TLapeType_Record) then
      doRecord()
    else
      LapeException(lpeInvalidCast, DocPos);

    Result := FCompiler.addManagedVar(Result) as TLapeGlobalVar;
  except
    Result.Free();
    raise;
  end;
end;

function TLapeTree_OpenArray.Compile(var Offset: Integer): TResVar;

  procedure doSet;
  var
    i: Integer;
    tmpVar: TResVar;
    Counter: TLapeVar;
  begin
    tmpVar := NullResVar;
    Counter := nil;

    for i := FValues.Count - 1 downto 0 do
      if isEmptyNode(FValues[i]) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (FValues[i] is TLapeTree_ExprBase) then
        with TLapeTree_Operator.Create(op_Plus, FCompiler, @FValues[i]._DocPos) do
        try
          Dest := Result;
          Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i]._DocPos);
          Right := TLapeTree_ExprBase(FValues[i]);
          Result := Compile(Offset);
        finally
          Free();
        end
      else if (FValues[i] is TLapeTree_Range) then
      begin
        Counter := FCompiler.getTempVar(TLapeType_Set(ToType).Range, 2);
        Counter.isConstant := False;
        tmpVar := Counter.VarType.Eval(op_Assign, tmpVar, GetResVar(Counter), TLapeTree_Range(FValues[i]).Lo.Compile(Offset), Offset, @FValues[i]._DocPos);

        with TLapeTree_For.Create(FCompiler, @FValues[i]._DocPos) do
        try
          Counter := TLapeTree_ResVar.Create(tmpVar, FCompiler, @FValues[i]._DocPos);
          Limit := TLapeTree_Range(FValues[i]).Hi;
          Body := TLapeTree_Operator.Create(op_Plus, FCompiler, @FValues[i]._DocPos);
          with TLapeTree_Operator(Body) do
          begin
            Dest := Result;
            Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i]._DocPos);
            Right := TLapeTree_ResVar.Create(tmpVar, FCompiler, @FValues[i]._DocPos);
          end;
          Compile(Offset);
        finally
          Free();
          SetNullResVar(tmpVar, 2);
        end;
      end
      else
        LapeException(lpeInvalidCast, FValues[i].DocPos);
  end;

  procedure doStaticArray;
  var
    i: Integer;
  begin
    if (FValues.Count <> TLapeType_StaticArray(ToType).Range.Hi - TLapeType_StaticArray(ToType).Range.Lo + 1) then
      LapeException(lpeInvalidRange, DocPos);

    for i := FValues.Count - 1 downto 0 do
      if (not (FValues[i] is TLapeTree_ExprBase)) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else
      with TLapeTree_Operator.Create(op_Assign, FCompiler, @FValues[i]._DocPos) do
      try
        Left := TLapeTree_Operator.Create(op_Index, FCompiler, @FValues[i]._DocPos);
        with TLapeTree_Operator(Left) do
        begin
          Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i]._DocPos);
          Right := TLapeTree_Operator.Create(op_Minus, FCompiler, @FValues[i]._DocPos);
          with TLapeTree_Operator(Right) do
          begin
            Left := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(DetermineIntType(i)).NewGlobalVarStr(IntToStr(i)))), FCompiler, @FValues[i]._DocPos);
            Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(DetermineIntType(TLapeType_StaticArray(ToType).Range.Lo)).NewGlobalVarStr(IntToStr(TLapeType_StaticArray(ToType).Range.Lo)))), FCompiler, @FValues[i]._DocPos);
          end;
        end;
        Right := TLapeTree_ExprBase(FValues[i]);
        Compile(Offset);
      finally
        Free();
      end;
  end;

  procedure doRecord;
  var
    i: Integer;
  begin
    if (FValues.Count > TLapeType_Record(ToType).FieldMap.Count) then
      LapeException(lpeInvalidRange, DocPos);

    for i := FValues.Count - 1 downto 0 do
      if (FValues[i] is TLapeTree_ExprBase) then
        with TLapeTree_Operator.Create(op_Assign, FCompiler, @FValues[i]._DocPos) do
        try
          Left := TLapeTree_Operator.Create(op_Dot, FCompiler, @FValues[i]._DocPos);
          with TLapeTree_Operator(Left) do
          begin
            Left := TLapeTree_ResVar.Create(Result, FCompiler, @FValues[i]._DocPos);
            Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(TLapeType_Record(ToType).FieldMap.Key[i]))), FCompiler, @FValues[i]._DocPos);
          end;
          Right := TLapeTree_ExprBase(FValues[i]);
          Compile(Offset);
        finally
          Free();
        end
      else if (not isEmptyNode(FValues[i])) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
  end;

begin
  if (not canCast()) then
    LapeException(lpeInvalidEvaluation, DocPos);

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
    doSet()
  else if (ToType is TLapeType_StaticArray) then
    doStaticArray()
  else if (ToType is TLapeType_Record) then
    doRecord()
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
    if isEmptyNode(FParams[i]) then
      Result[i] := nil
    else
      Result[i] := FParams[i].resType();
end;

function TLapeTree_Invoke.getParamTypesStr: lpString;
var
  i: Integer;
  ParamType: TLapeType;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    if isEmptyNode(FParams[i]) then
      ParamType := nil
    else
      ParamType := FParams[i].resType();
    if (ParamType <> nil) then
      Result := Result + ParamType.AsString
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
    Result := (FParams.Count = 1) and (not isEmptyNode(FParams[0])) and FParams[0].isConstant()
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
    if (t = nil) or (not (t is TLapeType_Method)) then
      Result := nil
    else
      Result := TLapeType_Method(t).Res;
  end;
end;

function TLapeTree_Invoke.Evaluate: TLapeGlobalVar;
var
  IdentVar: TLapeGlobalVar;

  function DoCast: TLapeGlobalVar;
  begin
    Result := nil;
    Assert(FIdent is TLapeTree_VarType);

    with TLapeTree_VarType(FIdent) do
    try
      if (FParams.Count <> 1) or (VarType = nil) or isEmptyNode(FParams[0]) then
        LapeException(lpeInvalidCast);
      setExpectedType(FParams[0], VarType);

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

  function DoImportedMethod(IdentVar: TLapeGlobalVar): TLapeGlobalVar;
  var
    i: Integer;
    Par: TLapeGlobalVar;
    ParamVars: array of Pointer;
  begin
    Assert(IdentVar <> nil);
    Assert(IdentVar.VarType.BaseType = ltImportedMethod);
    Result := nil;

    with TLapeType_Method(IdentVar.VarType) do
    begin
      if (Res = nil) then
        LapeException(lpeCannotEvalConstProc, FIdent.DocPos);
      if (FParams.Count > Params.Count) then
        if (FParams.Count > 0) then
          LapeException(lpeTooMuchParameters, FParams[Params.Count].DocPos)
        else
          LapeException(lpeTooMuchParameters, Self.DocPos);

      SetLength(ParamVars, FParams.Count);
      for i := 0 to FParams.Count - 1 do
      begin
        if (i > FParams.Count) or isEmptyNode(FParams[i]) then
          if (Params[i].Default <> nil) then
            Par := Params[i].Default
          else if (FParams[i] <> nil) then
            LapeExceptionFmt(lpeNoDefaultForParam, [i + 1], FParams[i].DocPos)
          else
            LapeExceptionFmt(lpeNoDefaultForParam, [i + 1], Self.DocPos)
        else
        begin
          setExpectedType(FParams[i], Params[i].VarType);
          Par := FParams[i].Evaluate();
        end;

        if (Par = nil) or (Params[i].ParType in Lape_RefParams) then
          LapeException(lpeCannotInvoke, FParams[i].DocPos);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(Par.VarType)) then
          if (FCompiler <> nil) and Params[i].VarType.CompatibleWith(Par.VarType) then
          try
            Par := TLapeGlobalVar(FCompiler.addManagedVar(Params[i].VarType.EvalConst(op_Assign, Params[i].VarType.NewGlobalVarP(), Par)));
          except on E: lpException do
            LapeException(E.Message, FParams[i].DocPos);
          end
          else
            LapeExceptionFmt(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, Par.VarType.AsString], FParams[i].DocPos);

        ParamVars[i] := Par.Ptr;
      end;

      Result := Res.NewGlobalVarP();
      TLapeImportedFunc(IdentVar.Ptr^)(@ParamVars[0], Result.Ptr);
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
    IdentVar := FIdent.Evaluate();
    if (IdentVar = nil) or (IdentVar.VarType = nil) then
      LapeException(lpeCannotInvoke, DocPos);

    if (IdentVar.VarType is TLapeType_OverloadedMethod) then
    begin
      IdentVar := TLapeType_OverloadedMethod(IdentVar.VarType).getMethod(getParamTypes());
      if (IdentVar = nil) then
        LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], FIdent.DocPos);
    end;
    if (IdentVar.Ptr = nil) or (IdentVar.VarType.BaseType <> ltImportedMethod) then
      LapeException(lpeCannotInvoke, FIdent.DocPos);

    Result := DoImportedMethod(IdentVar);
  end;
end;

function TLapeTree_Invoke.Compile(var Offset: Integer): TResVar;
type
  TResVarArray = array of TResVar;
var
  i: Integer;
  IdentVar: TResVar;
  ParamVars: TResVarArray;

  function DoCast: TResVar;
  var
    tmpVar, DestVar, tmpRes: TResVar;
  begin
    Assert(FIdent is TLapeTree_VarType);
    Result := NullResVar;
    tmpVar := NullResVar;
    DestVar := NullResVar;

    with TLapeTree_VarType(FIdent) do
    try
      if (FParams.Count <> 1) or (VarType = nil) or isEmptyNode(FParams[0]) then
        LapeException(lpeInvalidCast);

      setExpectedType(FParams[0], VarType);
      Result := FParams[0].Compile(Offset);
      if VarType.Equals(Result.VarType) or ((Result.VarType <> nil) and (VarType.Size = Result.VarType.Size)) then
      begin
        setNullResVar(FDest);
        Result.VarType := VarType;
      end
      else if VarType.CompatibleWith(Result.VarType) then
      begin
        if (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and (FDest.VarType <> nil) and VarType.Equals(FDest.VarType) then
          DestVar := FDest
        else
        begin
          setNullResVar(FDest);
          DestVar := getResVar(Compiler.getTempVar(VarType));
        end;

        tmpRes := Result;
        Result := VarType.Eval(op_Assign, tmpVar, DestVar, Result, Offset, @Self._DocPos);
        setNullResVar(tmpRes, 1);
      end
      else
        LapeException(lpeInvalidCast);
    except on E: lpException do
      LapeException(E.Message, Self.DocPos);
    end;
  end;

  function DoScriptMethod(IdentVar: TResVar; ParamVars: TResVarArray): TResVar;
  var
    i: Integer;
    Par, tmpVar, tmpRes: TResVar;

    function getStackVar(Node: TLapeTree_Base; var Offset: Integer): TResVar;
    begin
      if (Node is TLapeTree_DestExprBase) then
        TLapeTree_DestExprBase(Node).Dest := StackResVar;
      Result := Node.Compile(Offset);
    end;

  begin
    Assert(IdentVar.VarType is TLapeType_Method);
    Assert(IdentVar.VarType.BaseType in [ltUnknown, ltScriptMethod]);
    Assert(Length(ParamVars) = TLapeType_Method(IdentVar.VarType).Params.Count);
    Result := NullResVar;
    tmpVar := NullResVar;

    with TLapeType_Method(IdentVar.VarType) do
    begin
      if ParamInitialization then
        FCompiler.Emitter._InitStack(ParamSize, Offset, @Self._DocPos);
      for i := 0 to Params.Count - 1 do
      try
        Par := NullResVar;
        if (ParamVars[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          ParamVars[i] := getStackVar(FParams[i], Offset);

        if (Params[i].ParType in Lape_RefParams) then
          if (not isVariable(ParamVars[i])) then
            LapeException(lpeVariableExpected)
          else if ParamVars[i].VarPos.isPointer then
          begin
            Par.VarPos.MemPos := mpStack;
            Par.VarType := FCompiler.getBaseType(ltPointer);

            ParamVars[i].VarType := Par.VarType;
            ParamVars[i].VarPos.isPointer := False;

            tmpRes := ParamVars[i];
            ParamVars[i] := Par.VarType.Eval(op_Assign, tmpVar, Par, ParamVars[i], Offset, @Self._DocPos);
            setNullResVar(tmpRes, 1);
          end
          else
          begin
            Par.VarPos.MemPos := mpStack;
            Par.VarType := FCompiler.getBaseType(ltPointer);
            ParamVars[i] := ParamVars[i].VarType.Eval(op_Addr, Par, ParamVars[i], tmpVar, Offset, @Self._DocPos);
          end
        else if (ParamVars[i].VarPos.MemPos <> mpStack) or ((Params[i].VarType <> nil) and (not Params[i].VarType.Equals(ParamVars[i].VarType))) then
          if Params[i].VarType.CompatibleWith(ParamVars[i].VarType) then
          begin
            Par.VarPos.MemPos := mpStack;
            Par.VarType := Params[i].VarType;

            tmpRes := ParamVars[i];
            ParamVars[i] := Params[i].VarType.Eval(op_Assign, tmpVar, Par, ParamVars[i], Offset, @Self._DocPos);
            setNullResVar(tmpRes, 1);
          end
          else
            LapeExceptionFmt(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, ParamVars[i].VarType.AsString]);

        if (ParamVars[i].VarPos.MemPos <> mpStack) or (ParamVars[i].VarType = nil) then
          LapeException(lpeCannotInvoke);
      except on tmpVar: lpException do
        LapeException(tmpVar.Message, FParams[i].DocPos);
      end;

      setNullResVar(FDest);
      if (Res <> nil) then
      begin
        Result.VarPos.MemPos := mpVar;
        Result.VarType := Res;
        Result.VarPos.StackVar := FCompiler.getTempVar(Res);

        Par := NullResVar;
        Par.VarPos.MemPos := mpStack;
        Par.VarType := FCompiler.getBaseType(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Par, Result, NullResVar, Offset, @Self._DocPos);
      end;

      FCompiler.Emitter._IncCall(IdentVar, ParamSize, Offset, @Self._DocPos);
    end;
  end;

  function DoImportedMethod(IdentVar: TResVar; ParamVars: TResVarArray): TResVar;
  var
    i: Integer;
    Par, tmpVar, tmpRes: TResVar;
  begin
    Assert(IdentVar.VarType is TLapeType_Method);
    Assert(IdentVar.VarType.BaseType in [ltUnknown, ltImportedMethod]);
    Assert(Length(ParamVars) = TLapeType_Method(IdentVar.VarType).Params.Count);
    Result := NullResVar;
    tmpVar := NullResVar;

    with TLapeType_Method(IdentVar.VarType) do
    begin
      for i := 0 to Params.Count - 1 do
      begin
        Par := NullResVar;
        if (ParamVars[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          getTempVar(FParams[i], Offset, ParamVars[i], 0);

        if (ParamVars[i].VarPos.MemPos = mpStack) or (ParamVars[i].VarType = nil) then
          LapeException(lpeCannotInvoke, FParams[i].DocPos)
        else if (Params[i].ParType in Lape_RefParams) and (not isVariable(ParamVars[i])) then
          LapeException(lpeVariableExpected, FParams[i].DocPos);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(ParamVars[i].VarType)) then
          if (not (Params[i].ParType in Lape_RefParams)) and Params[i].VarType.CompatibleWith(ParamVars[i].VarType) then
          try
            Par.VarPos.MemPos := mpVar;
            Par.VarType := Params[i].VarType;
            Par.VarPos.StackVar := Compiler.getTempVar(Par.VarType);

            tmpRes := ParamVars[i];
            ParamVars[i] := Params[i].VarType.Eval(op_Assign, tmpVar, Par, ParamVars[i], Offset, @Self._DocPos);
            setNullResVar(tmpRes, 1);
          except on tmpVar: lpException do
            LapeException(tmpVar.Message, FParams[i].DocPos);
          end
          else
            LapeExceptionFmt(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, ParamVars[i].VarType.AsString], FParams[i].DocPos);

        Par.VarPos.MemPos := mpStack;
        Par.VarType := Compiler.getBaseType(ltPointer);
        {if ParamVars[i].VarPos.isPointer then
        begin
          ParamVars[i].VarType := Par.VarType;
          ParamVars[i].VarPos.isPointer := False;
          Par.VarType.Eval(op_Assign, tmpVar, Par, ParamVars[i], Offset, @Self.DocPos);
          ParamVars[i].VarPos.isPointer := True;
        end
        else}
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Par, ParamVars[i], NullResVar, Offset, @Self._DocPos);
      end;

      if (Res = nil) then
      begin
        setNullResVar(FDest);
        FCompiler.Emitter._InvokeImportedProc(IdentVar, Params.Count * SizeOf(Pointer), Offset, @Self._DocPos)
      end
      else
      begin
        Result.VarType := Res;
        if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          FDest := VarResVar;
        getDestVar(FDest, Result, op_Unknown, FCompiler);
        FCompiler.Emitter._InvokeImportedFunc(IdentVar, Result, Params.Count * SizeOf(Pointer), Offset, @Self._DocPos)
      end;
    end;
  end;

  function DoCombiMethod(IdentVar: TResVar; ParamVars: TResVarArray): TResVar;
  var
    i, o_if, o_else: Integer;
    IsScriptMethod, tmpDest, tmpRes: TResVar;
    ParamsCopy: TResVarArray;
  begin
    with TLapeTree_InternalMethod_IsScriptMethod.Create(FCompiler, @_DocPos) do
    try
      addParam(TLapeTree_ResVar.Create(IdentVar, FCompiler, @_DocPos));
      IsScriptMethod := Compile(Offset);
    finally
      Free();
    end;

    ParamsCopy := Copy(ParamVars);
    o_if := FCompiler.Emitter._JmpRIfNot(0, IsScriptMethod, Offset, @_DocPos);
    tmpRes := DoScriptMethod(IdentVar, ParamsCopy);
    tmpDest := FDest;
    FDest := tmpRes;
    for i := 0 to FParams.Count - 1 do
      setNullResVar(ParamVars[i], 1);

    o_else := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
    FCompiler.Emitter._JmpRIfNot(Offset - o_if, IsScriptMethod, o_if, @_DocPos);
    Result := DoImportedMethod(IdentVar, ParamVars);
    FCompiler.Emitter._JmpR(Offset - o_else, o_else, @_DocPos);
    FDest := tmpDest;
  end;

begin
  Result := NullResVar;
  Assert(FIdent <> nil);

  if (FIdent is TLapeTree_VarType) then
    Result := DoCast()
  else
  begin
    IdentVar := FIdent.Compile(Offset);

    if (IdentVar.VarType <> nil) and (IdentVar.VarType is TLapeType_OverloadedMethod) then
    begin
      IdentVar := getResVar(TLapeType_OverloadedMethod(IdentVar.VarType).getMethod(getParamTypes(), FDest.VarType));
      if (IdentVar.VarType = nil) then
        LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], FIdent.DocPos);
    end;
    if (IdentVar.VarType = nil) or (not (IdentVar.VarType is TLapeType_Method)) then
      LapeException(lpeCannotInvoke, FIdent.DocPos);

    with TLapeType_Method(IdentVar.VarType) do
    begin
      if (FParams.Count > Params.Count) then
        if (FParams.Count > 0) then
          LapeException(lpeTooMuchParameters, FParams[Params.Count].DocPos)
        else
          LapeException(lpeTooMuchParameters, Self.DocPos);

      SetLength(ParamVars, Params.Count);
      for i := 0 to Params.Count - 1 do
        if (i >= FParams.Count) or isEmptyNode(FParams[i]) then
          if (Params[i].Default <> nil) then
          begin
            ParamVars[i] := getResVar(Params[i].Default);
            if (Params[i].ParType in Lape_RefParams) and (not isVariable(ParamVars[i])) then
              if (FParams[i] <> nil) then
                LapeException(lpeVariableExpected, FParams[i].DocPos)
              else
                LapeException(lpeVariableExpected, Self.DocPos);
          end
          else if (i < FParams.Count) and (FParams[i] <> nil) then
            LapeExceptionFmt(lpeNoDefaultForParam, [i + 1], FParams[i].DocPos)
          else
            LapeExceptionFmt(lpeNoDefaultForParam, [i + 1], Self.DocPos)
        else
        begin
          setExpectedType(FParams[i], Params[i].VarType);
          ParamVars[i] := NullResVar;
        end;

      if (IdentVar.VarType.BaseType = ltScriptMethod) then
        Result := DoScriptMethod(IdentVar, ParamVars)
      else if (IdentVar.VarType.BaseType = ltImportedMethod) then
        Result := DoImportedMethod(IdentVar, ParamVars)
      else
        Result := DoCombiMethod(IdentVar, ParamVars);
    end;

    setNullResVar(IdentVar, 1);
    for i := 0 to FParams.Count - 1 do
      setNullResVar(ParamVars[i], 1);
  end;
end;

constructor TLapeTree_InternalMethod.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(nil, ACompiler, ADocPos);
end;

function TLapeTree_InternalMethod.isConstant: Boolean;
begin
  Result := (FParams.Count = 1) and (not isEmptyNode(FParams[0])) and FParams[0].isConstant();
end;

function TLapeTree_InternalMethod_IsScriptMethod.isConstant: Boolean;
begin
  Result := False;
end;

function TLapeTree_InternalMethod_IsScriptMethod.resType: TLapeType;
begin
  Result := FCompiler.getBaseType(ltEvalBool);
end;

function TLapeTree_InternalMethod_IsScriptMethod.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_InternalMethod_IsScriptMethod.Compile(var Offset: Integer): TResVar;
var
  tmpVar, DestVar, Param: TResVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  tmpVar := NullResVar;
  Param := FParams[0].Compile(Offset);
  if (Param.VarType = nil) or (not (Param.VarType is TLapeType_Method)) then
    LapeException(lpeInvalidCondition, DocPos);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := Param.VarType;
  DestVar.VarType.Eval(op_Assign, tmpVar, DestVar, Param, Offset, @_DocPos);
  SetNullResVar(Param, 1);

  FCompiler.Emitter._IsInternal(Offset, @_DocPos);
  Result.VarPos.MemPos := mpStack;
  Result.VarType := resType();
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
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanBreak;
  BreakDepth, BreakCount: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  Node := FParent;

  if (not (FParams.Count in [0, 1])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) or isEmptyNode(FParams[0]) then
    BreakDepth := 1
  else
    with FParams[0].Evaluate() do
    begin
      BreakDepth := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0].DocPos)
      else if (BreakDepth < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0].DocPos);
    end;

  BreakCount := 1;
  JumpSafe := False;
  while (Node <> nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanBreak, FoundNode) = 0) then
      if (BreakCount < BreakDepth) then
        Inc(BreakCount)
      else
      begin
        FoundNode.addBreakStatement(JumpSafe, Offset, @_DocPos);
        FoundNode := nil;
        Break;
      end
    else if (Node is TLapeTree_Try) then
      JumpSafe := True;
    Node := Node.Parent;
  end;

  if (Node = nil) then
    if (BreakCount < BreakDepth) then
      LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
    else
      LapeException(lpeCannotBreak, DocPos);
end;

function TLapeTree_InternalMethod_Continue.Compile(var Offset: Integer): TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanContinue;
  ContinueDepth, ContinueCount: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  Node := FParent;

  if (not (FParams.Count in [0, 1])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) or isEmptyNode(FParams[0]) then
    ContinueDepth := 1
  else
    with FParams[0].Evaluate() do
    begin
      ContinueDepth := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0]._DocPos)
      else if (ContinueDepth < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0]._DocPos);
    end;

  ContinueCount := 1;
  JumpSafe := False;
  while (Node <> nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanContinue, FoundNode) = 0) then
      if (ContinueCount < ContinueDepth) then
        Inc(ContinueCount)
      else
      begin
        FoundNode.addContinueStatement(JumpSafe, Offset, @_DocPos);
        FoundNode := nil;
        Break;
      end
    else if (Node is TLapeTree_Try) then
      JumpSafe := True;
    Node := Node.Parent;
  end;

  if (Node = nil) then
    if (ContinueCount < ContinueDepth) then
      LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
    else
      LapeException(lpeCannotContinue, DocPos);
end;

function TLapeTree_InternalMethod_Exit.Compile(var Offset: Integer): TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanExit;
  ResultDecl: TLapeDeclaration;
begin
  Result := NullResVar;
  Node := FParent;

  if (FParams.Count <> 0) then
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isEmptyNode(FParams[0]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos);

  if (FParams.Count = 1) then
    with TLapeTree_Operator.Create(op_Assign, FCompiler, @FParams[0]._DocPos) do
    try
      ResultDecl := FCompiler.getDeclaration('Result');
      if (ResultDecl = nil) or (not (ResultDecl is TLapeParameterVar)) then
        LapeExceptionFmt(lpeWrongNumberParams, [0], DocPos);
      Left := TLapeTree_ResVar.Create(getResVar(ResultDecl as TLapeVar), FCompiler, @FParams[0]._DocPos);
      Right := TLapeTree_ResVar.Create(FParams[0].Compile(Offset), FCompiler, @FParams[0]._DocPos);
      Compile(Offset);
    finally
      Free();
    end;

  while (Node <> nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanExit, FoundNode) = 0) then
    begin
      FoundNode.addExitStatement(True, Offset, @_DocPos);
      FoundNode := nil;
      Break;
    end;
    Node := Node.Parent;
  end;

  if (Node = nil) then
    FCompiler.Emitter._JmpSafe(EndJump, Offset, @_DocPos);
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
  ParamType: TLapeType;
begin
  if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    ParamType := FParams[0].Compile().VarType
  else
    ParamType := nil;

  if (ParamType <> nil) and (ParamType is TLapeType_Type) then
    ParamType := TLapeType_Type(ParamType).TType;
  if (ParamType = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(ParamType.Size))));
end;

function TLapeTree_InternalMethod_SizeOf.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeCannotEvalRunTime, DocPos);
end;

function TLapeTree_InternalMethod_Ord.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  Result := nil;
  if (FParams.Count = 1) and (not isEmptyNode(FParams[0])) then
  begin
    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType <> nil) then
      Result := FCompiler.getBaseType(ParamType.BaseIntType);
  end;
end;

function TLapeTree_InternalMethod_Ord.Evaluate: TLapeGlobalVar;
begin
  if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  setIdent(TLapeTree_VarType.Create(resType(), FCompiler, @_DocPos));
  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.Compile(var Offset: Integer): TResVar;
begin
  if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  setIdent(TLapeTree_VarType.Create(resType(), FCompiler, @_DocPos));
  Result := inherited;
end;

function TLapeTree_InternalMethod_Low.isConstant: Boolean;
begin
  Result := True;
end;

function TLapeTree_InternalMethod_Low.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  Result := nil;
  if (FParams.Count = 1) and (not isEmptyNode(FParams[0])) then
  begin
    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
      Result := ParamType
    else if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes - LapeStringTypes + [ltShortString]) then
      Result := FCompiler.getBaseType(ltInt32);
  end;
end;

function TLapeTree_InternalMethod_Low.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    ParamType := FParams[0].Compile().VarType
  else
    ParamType := nil;

  if (ParamType <> nil) and (ParamType is TLapeType_Type) then
    ParamType := TLapeType_Type(ParamType).TType;
  if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes  - LapeStringTypes + [ltShortString])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  {if (ParamType is TLapeType_StaticArray) then
    with TLapeType_StaticArray(ParamType) do
      Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(Range.Lo))))
  else if (ParamType is TLapeType_DynArray) then
    Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0')))
  else}
  Result := ParamType.VarLo();
end;

function TLapeTree_InternalMethod_Low.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeCannotEvalRunTime, DocPos);
end;

function TLapeTree_InternalMethod_High.isConstant: Boolean;
var
  ParamType: TLapeType;
begin
  Result := True;
  if (FParams.Count = 1) and (not isEmptyNode(FParams[0])) then
  begin
    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (not (ParamType is TLapeType_Type)) and (ParamType.BaseType = ltDynArray) then
      Result := False;
  end;
end;

function TLapeTree_InternalMethod_High.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  Result := nil;
  if (FParams.Count = 1) and (not isEmptyNode(FParams[0])) then
  begin
    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
      Result := ParamType
    else if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes - LapeStringTypes + [ltShortString]) then
      Result := FCompiler.getBaseType(ltInt32);
  end;
end;

function TLapeTree_InternalMethod_High.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    ParamType := FParams[0].Compile().VarType
  else
    ParamType := nil;

  if (ParamType <> nil) and (ParamType is TLapeType_Type) then
    ParamType := TLapeType_Type(ParamType).TType;
  if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes - LapeStringTypes + [ltShortString])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  {if (ParamType is TLapeType_StaticArray) then
    with TLapeType_StaticArray(ParamType) do
      Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(Range.Hi))))
  else}
  if (ParamType is TLapeType_DynArray) then
    LapeException(lpeCannotEvalConst, DocPos)
  else
    Result := ParamType.VarHi();
  if (Result = nil) then
    LapeException(lpeCannotEvalConst, DocPos)
end;

function TLapeTree_InternalMethod_High.Compile(var Offset: Integer): TResVar;
begin
  if isConstant() or (resType() = nil) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Result := NullResVar;
  Result.VarPos.MemPos := mpStack;
  Result.VarType := Compiler.getBaseType(ltPointer);
  FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, FParams[0].Compile(Offset), NullResVar, Offset, @Self._DocPos);

  Result := NullResVar;
  Result.VarType := FCompiler.getBaseType(ltInt32);
  if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    FDest := VarResVar;
  getDestVar(FDest, Result, op_Unknown, FCompiler);
  FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!high') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self._DocPos);
end;

function TLapeTree_InternalMethod_Length.isConstant: Boolean;
var
  ParamType: TLapeType;
begin
  Result := False;
  if (FParams.Count = 1) and (not isEmptyNode(FParams[0])) then
  begin
    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType <> nil) and (ParamType.BaseType  = ltStaticArray) then
      Result := True;
  end;
end;

function TLapeTree_InternalMethod_Length.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  Result := FCompiler.getBaseType(ltInt32);
  if (FParams.Count = 1) and (not isEmptyNode(FParams[0])) then
  begin
    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType <> nil) and (ParamType.BaseType = ltShortString) then
      Result := FCompiler.getBaseType(ltUInt8);
  end;
end;

function TLapeTree_InternalMethod_Length.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (not isConstant()) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_ResVar) or (FParams[0] is TLapeTree_GlobalVar) then
    ParamType := FParams[0].Compile().VarType
  else
    ParamType := nil;

  if (ParamType <> nil) and (ParamType is TLapeType_Type) then
    ParamType := TLapeType_Type(ParamType).TType;
  if (ParamType = nil) or (ParamType.BaseType <> ltStaticArray) then
    LapeException(lpeInvalidEvaluation, DocPos);

  with TLapeType_StaticArray(ParamType) do
    Result := TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr(IntToStr(Range.Hi - Range.Lo + 1))));
end;

function TLapeTree_InternalMethod_Length.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
begin
  if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile(Offset);
  if (Param.VarType = nil) or (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (Param.VarType.BaseType = ltShortString) then
  begin
    SetNullResVar(FDest);
    Result := Param;
    Result.VarType := FCompiler.getBaseType(ltUInt8);
  end
  else
  begin
    Result := NullResVar;
    Result.VarPos.MemPos := mpStack;
    Result.VarType := Compiler.getBaseType(ltPointer);
    FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, Param, NullResVar, Offset, @Self._DocPos);

    Result := NullResVar;
    Result.VarType := FCompiler.getBaseType(ltInt32);
    if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      FDest := VarResVar;
    getDestVar(FDest, Result, op_Unknown, FCompiler);

    case Param.VarType.BaseType of
      ltAnsiString: FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!astrlen') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      ltWideString: FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!wstrlen') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      ltUnicodeString: FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!ustrlen') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      else FCompiler.Emitter._InvokeImportedFunc(getResVar(FCompiler.getDeclaration('!length') as TLapeVar), Result, SizeOf(Pointer), Offset, @Self._DocPos);
    end;
  end;
end;

function TLapeTree_InternalMethod_Succ.resType: TLapeType;
begin
  Result := nil;
  if (FParams.Count in [1, 2]) and (not isEmptyNode(FParams[0])) then
  begin
    Result := FParams[0].resType();
    if (Result <> nil) and (Result is TLapeType_Type) then
      Result := TLapeType_Type(Result).TType;
  end;
end;

function TLapeTree_InternalMethod_Succ.Evaluate: TLapeGlobalVar;
var
  VarParam, CountParam: TLapeGlobalVar;
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  ResultType: TLapeType;
begin
  Result := nil;
  CountParam := nil;
  OldCountParam := nil;

  if (FParams.Count = 2) then
    if isEmptyNode(FParams[0]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos)
    else if isEmptyNode(FParams[1]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [2], DocPos)
    else
      OldCountParam := FParams.Delete(1)
  else if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  try
    if (OldCountParam <> nil) then
      CountParam := OldCountParam.Evaluate()
    else
      CountParam := FCompiler.getBaseType(ltInt8).NewGlobalVarStr('1');

    VarParam := FParams[0].Evaluate();
    if (VarParam = nil) or (VarParam.VarType = nil) or (VarParam.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, DocPos);

    if (VarParam.VarType.BaseType = ltPointer) then
      with TLapeTree_Operator.Create(op_Index, FCompiler, @_DocPos) do
      try
        Left := TLapeTree_GlobalVar.Create(VarParam, FCompiler, @_DocPos);
        Right := TLapeTree_GlobalVar.Create(CountParam, FCompiler, @_DocPos);
        Result := Evaluate();
      finally
        Free();
      end
    else
    begin
      ResultType := VarParam.VarType;
      VarParam := FCompiler.getBaseType(VarParam.VarType.BaseIntType).NewGlobalVarP(VarParam.Ptr);
      with TLapeTree_Operator.Create(op_Plus, FCompiler, @_DocPos) do
      try
        Left := TLapeTree_GlobalVar.Create(VarParam, FCompiler, @_DocPos);
        Right := TLapeTree_GlobalVar.Create(CountParam, FCompiler, @_DocPos);
        Result := Evaluate();

        OldVarParam := FParams.Delete(0);
        addParam(TLapeTree_GlobalVar.Create(Result, FCompiler, @_DocPos));
        try
          setIdent(TLapeTree_VarType.Create(VarParam.VarType, FCompiler, @_DocPos));
          Result := inherited;

          VarParam.Free();
          VarParam := Result;
          Result := ResultType.NewGlobalVarP(VarParam.Ptr);
        finally
          FParams.Delete(0).Free();
          addParam(OldVarParam);
        end;
      finally
        VarParam.Free();
        Free();
      end;
    end;
  finally
    if (OldCountParam <> nil) then
      addParam(OldCountParam)
    else if (CountParam <> nil) then
      CountParam.Free();
  end;
end;

function TLapeTree_InternalMethod_Succ.Compile(var Offset: Integer): TResVar;
var
  VarParam, CountParam, tmpDest: TResVar;
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  ResultType: TLapeType;
begin
  CountParam := NullResVar;
  OldCountParam := nil;
  if (FParams.Count = 2) then
    if isEmptyNode(FParams[0]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos)
    else if isEmptyNode(FParams[1]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [2], DocPos)
    else
      OldCountParam := FParams.Delete(1)
  else if (FParams.Count <> 1) or isEmptyNode(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  try
    if (OldCountParam <> nil) then
      CountParam := OldCountParam.Compile(Offset)
    else
      CountParam := getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt8).NewGlobalVarStr('1')));

    VarParam := FParams[0].Compile(Offset);
    if (VarParam.VarType = nil) or (VarParam.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, DocPos);

    if (VarParam.VarType.BaseType = ltPointer) then
      with TLapeTree_Operator.Create(op_Index, FCompiler, @_DocPos) do
      try
        Dest := Self.Dest;
        Left := TLapeTree_ResVar.Create(VarParam, FCompiler, @_DocPos);
        Right := TLapeTree_ResVar.Create(CountParam, FCompiler, @_DocPos);
        Result := Compile(Offset);
        Self.Dest := Dest;
      finally
        Free();
      end
    else
    begin
      ResultType := VarParam.VarType;
      VarParam.VarType := FCompiler.getBaseType(VarParam.VarType.BaseIntType);
      with TLapeTree_Operator.Create(op_Plus, FCompiler, @_DocPos) do
      try
        Dest := Self.Dest;
        Left := TLapeTree_ResVar.Create(VarParam, FCompiler, @_DocPos);
        Right := TLapeTree_ResVar.Create(CountParam, FCompiler, @_DocPos);
        Result := Compile(Offset);

        if (Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
          tmpDest := Dest
        else
          tmpDest := NullResVar;

        OldVarParam := FParams.Delete(0);
        addParam(TLapeTree_ResVar.Create(Result, FCompiler, @_DocPos));
        try
          setIdent(TLapeTree_VarType.Create(VarParam.VarType, FCompiler, @_DocPos));
          Result := inherited;
          Result.VarType := ResultType;
          if (Self.Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
            Self.Dest := tmpDest;
        finally
          FParams.Delete(0).Free();
          addParam(OldVarParam);
        end;
      finally
        Free();
      end;
    end;
  finally
    if (OldCountParam <> nil) then
      addParam(OldCountParam)
    else
      setNullResVar(CountParam);
  end;
end;

function TLapeTree_InternalMethod_Pred.Evaluate: TLapeGlobalVar;
var
  Negation: TLapeTree_Operator;
begin
  Negation := nil;

  if (FParams.Count < 2) then
    addParam(TLapeTree_GlobalVar.Create(FCompiler.getBaseType(ltInt8).NewGlobalVarStr('-1'), FCompiler, @_DocPos))
  else if (not isEmptyNode(FParams[1])) and (FParams.Count = 2) then
  begin
    Negation := TLapeTree_Operator.Create(op_UnaryMinus, FCompiler, @_DocPos);
    Negation.Left := FParams.Delete(1);
    addParam(Negation);
  end;

  Result := inherited;

  if (FParams.Count = 2) then
  begin
    if (Negation <> nil) then
      addParam(Negation.Left)
    else
      TLapeTree_GlobalVar(FParams[1]).GlobalVar.Free();
    FParams.Delete(1).Free();
  end;
end;

function TLapeTree_InternalMethod_Pred.Compile(var Offset: Integer): TResVar;
var
  Negation: TLapeTree_Operator;
begin
  Negation := nil;

  if (FParams.Count < 2) then
    addParam(TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt8).NewGlobalVarStr('-1'))), FCompiler, @_DocPos))
  else if (not isEmptyNode(FParams[1])) and (FParams.Count = 2) then
  begin
    Negation := TLapeTree_Operator.Create(op_UnaryMinus, FCompiler, @_DocPos);
    Negation.Left := FParams.Delete(1);
    addParam(Negation);
  end;

  Result := inherited;

  if (FParams.Count = 2) then
  begin
    if (Negation <> nil) then
      addParam(Negation.Left);
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
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  Succ: TLapeTree_Operator;
begin
  if (not (FParams.Count in [1, 2])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  OldVarParam := FParams.Delete(0);
  if (FParams.Count > 0) then
    OldCountParam := FParams.Delete(0)
  else
    OldCountParam := nil;
  Succ := nil;

  try
    Result := OldVarParam.Compile(Offset);
    Succ := TLapeTree_Operator.Create(op_Assign, FCompiler, @_DocPos);
    Succ.Left := TLapeTree_ResVar.Create(Result, FCompiler, @_DocPos);
    Succ.Right := TLapeTree_InternalMethod_Succ.Create(FCompiler, @_DocPos);
    TLapeTree_InternalMethod_Succ(Succ.Right).addParam(TLapeTree_ResVar.Create(Result, FCompiler, @_DocPos));
    if (OldCountParam <> nil) then
      TLapeTree_InternalMethod_Succ(Succ.Right).addParam(OldCountParam);
    Result := Succ.Compile(Offset);
  finally
    addParam(OldVarParam);
    if (OldCountParam <> nil) then
      addParam(OldCountParam);
    if (Succ <> nil) then
      Succ.Free();
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
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  Pred: TLapeTree_Operator;
begin
  if (not (FParams.Count in [1, 2])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  OldVarParam := FParams.Delete(0);
  if (FParams.Count > 0) then
    OldCountParam := FParams.Delete(0)
  else
    OldCountParam := nil;
  Pred := nil;

  try
    Result := OldVarParam.Compile(Offset);
    Pred := TLapeTree_Operator.Create(op_Assign, FCompiler, @_DocPos);
    Pred.Left := TLapeTree_ResVar.Create(Result, FCompiler, @_DocPos);
    Pred.Right := TLapeTree_InternalMethod_Pred.Create(FCompiler, @_DocPos);
    TLapeTree_InternalMethod_Pred(Pred.Right).addParam(TLapeTree_ResVar.Create(Result, FCompiler, @_DocPos));
    if (OldCountParam <> nil) then
      TLapeTree_InternalMethod_Pred(Pred.Right).addParam(OldCountParam);
    Result := Pred.Compile(Offset);
  finally
    addParam(OldVarParam);
    if (OldCountParam <> nil) then
      addParam(OldCountParam);
    if (Pred <> nil) then
      Pred.Free();
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
  Result := ((FLeft = nil) or (not (TLapeTree_Base(FLeft) is TLapeTree_If))) and (
    ((FLeft <> nil) and (FLeft is TLapeTree_GlobalVar) and (
        ((FOperatorType = op_Dot)   and (TLapeTree_GlobalVar(FLeft).GlobalVar.VarType.BaseType in [ltRecord, ltUnion])) or
        ((FOperatorType = op_Index) and (TLapeTree_GlobalVar(FLeft).GlobalVar.VarType.BaseType in [ltShortString, ltStaticArray]))
      ) and (FRight <> nil) and FRight.isConstant()) or
    ((FLeft = nil) or FLeft.isConstant()) and ((FRight = nil) or FRight.isConstant())
  );
end;

function TLapeTree_Operator.resType: TLapeType;
var
  LeftType, RightType: TLapeType;
  tmpLeft: TLapeTree_ExprBase;
  ResVar: TResVar;
begin
  Result := nil;
  if (FLeft <> nil) then
    if (TLapeTree_Base(FLeft) is TLapeTree_If) then
      Exit(FCompiler.getBaseType(ltEvalBool))
    else
      LeftType := FLeft.resType()
  else
    LeftType := nil;

  setExpectedType(FRight, LeftType);
  if (FRight <> nil) then
    RightType := FRight.resType()
  else
    RightType := nil;
  if (LeftType = nil) and (RightType = nil) then
    Exit
  else if (LeftType = nil) then
  begin
    LeftType := RightType;
    RightType := nil;
  end;

  if (FRight <> nil) and (FRight is TLapeTree_GlobalVar) then
    Result := LeftType.EvalRes(FOperatorType, TLapeTree_GlobalVar(FRight).GlobalVar)
  else
    Result := LeftType.EvalRes(FOperatorType, RightType);

  if (LeftType <> nil) and (FRight <> nil) and
     ((Result = nil)  and ((FOperatorType = op_IN) and (FRight is TLapeTree_OpenArray)) or
     ((Result <> nil) and (Result.BaseType in LapeBoolTypes) and (FOperatorType in [op_AND, op_OR]) and
      (LeftType.BaseType in LapeBoolTypes) and (RightType <> nil) and (RightType.BaseType in LapeBoolTypes)))
  then
  begin
    tmpLeft := FLeft;
    if (FOperatorType = op_IN) then
      TLapeTree_Base(FLeft) := TLapeTree_MultiIf.Create(FLeft, FRight as TLapeTree_OpenArray)
    else
      TLapeTree_Base(FLeft) := TLapeTree_If.Create(FCompiler, @_DocPos);

    with TLapeTree_If(TLapeTree_Base(FLeft)) do
    begin
      if (FOperatorType <> op_IN) then
        Condition := tmpLeft;

      if (Result = nil) then
        Result := FCompiler.getBaseType(ltEvalBool);
      ResVar := FCompiler.getTempStackVar(Result);
      ResVar.VarPos.ForceVariable := True;

      if (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and
         (FDest.VarType <> nil) and ResVar.VarType.Equals(FDest.VarType)
      then
      begin
        setNullResVar(ResVar, 1);
        ResVar := FDest;
      end
      else
      begin
        FDest := NullResVar;
        if (ResVar.VarPos.MemPos = mpVar) then
          ResVar.VarPos.StackVar.isConstant := False;
      end;

      if (FOperatorType = op_AND) then
        Body := FRight
      else
      begin
        Body := TLapeTree_Operator.Create(op_Assign, FCompiler, @_DocPos);
        with TLapeTree_Operator(Body) do
        begin
          Left := TLapeTree_ResVar.Create(ResVar, FCompiler, @_DocPos);
          Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(ResVar.VarType.NewGlobalVarStr('1'))), FCompiler, @_DocPos);
        end;
      end;

      if (FOperatorType = op_OR) then
        ElseBody := FRight
      else
      begin
        ElseBody := TLapeTree_Operator.Create(op_Assign, FCompiler, @_DocPos);
        with TLapeTree_Operator(ElseBody) do
        begin
          Left := TLapeTree_ResVar.Create(ResVar, FCompiler, @_DocPos);
          Right := TLapeTree_GlobalVar.Create(TLapeGlobalVar(FCompiler.addManagedVar(ResVar.VarType.NewGlobalVarStr('0'))), FCompiler, @_DocPos);
        end;
      end;
    end;
    setRight(nil);
  end;
end;

function TLapeTree_Operator.Evaluate: TLapeGlobalVar;
var
  LeftVar, RightVar: TLapeGlobalVar;
  Short: Boolean;

  function canShort(t: TLapeType): Boolean;
  begin
    Result := (t <> nil) and (t.BaseType in LapeBoolTypes);
  end;

begin
  Result := nil;

  if (FLeft <> nil) then
    if (TLapeTree_Base(FLeft) is TLapeTree_If) then
      LapeException(lpeInvalidEvaluation, DocPos)
    else
      LeftVar := FLeft.Evaluate()
  else
    LeftVar := nil;

  setExpectedType(FRight, LeftVar.VarType);

  Short := (FOperatorType in [op_AND, op_OR]) and (LeftVar <> nil) and (FRight <> nil) and canShort(LeftVar.VarType) and canShort(FRight.resType());
  if Short and ((FOperatorType = op_AND) xor (LeftVar.AsInteger <> 0)) then
    Exit(LeftVar);

  if (FRight <> nil) then
    RightVar := FRight.Evaluate()
  else
    RightVar := nil;

  if Short and (RightVar <> nil) then
    Exit(RightVar);

  if (LeftVar = nil) and (RightVar = nil) then
    LapeException(lpeInvalidEvaluation, DocPos)
  else if (LeftVar = nil) then
  begin
    LeftVar := RightVar;
    RightVar := nil;
  end;
  if (LeftVar.VarType = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  try
    Result := TLapeGlobalVar(FCompiler.addManagedVar(LeftVar.VarType.EvalConst(FOperatorType, LeftVar, RightVar)));
  except on E: lpException do
    LapeException(E.Message, DocPos);
  end;
end;

function TLapeTree_Operator.Compile(var Offset: Integer): TResVar;
var
  LeftVar, RightVar: TResVar;
  DoneAssignment: Boolean;

  function doIf: TResVar;
  begin
    with TLapeTree_If(TLapeTree_Base(FLeft)) do
    begin
      if (FDest.VarType <> nil) and (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and TLapeTree_Operator(Body).Left.resType().CompatibleWith(FDest.VarType) then
      begin
        TLapeTree_Operator(Body).Left.Free();
        TLapeTree_Operator(Body).Left := TLapeTree_ResVar.Create(FDest, FCompiler, @_DocPos);
        TLapeTree_Operator(ElseBody).Left.Free();
        TLapeTree_Operator(ElseBody).Left := TLapeTree_ResVar.Create(FDest, FCompiler, @_DocPos);
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
    if (resType() <> nil) and (TLapeTree_Base(FLeft) is TLapeTree_If) then
      Exit(doIf())
    else
    begin
      LeftVar := FLeft.Compile(Offset);
      if (FOperatorType = op_Assign) and (not isVariable(LeftVar)) then
        LapeException(lpeCannotAssign, FLeft.DocPos);
    end
  else
    LeftVar := NullResVar;

  if (FRight <> nil) then
  begin
    setExpectedType(FRight, LeftVar.VarType);

    if (FOperatorType = op_Assign) and
      (FLeft <> nil) and isVariable(LeftVar) and
      (FRight is TLapeTree_DestExprBase) {and (not (TLapeTree_Operator(FRight).OperatorType in [op_Assign, op_Deref]))} and
      (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos)
    then
    begin
      TLapeTree_DestExprBase(FRight).Dest := LeftVar;
      DoneAssignment := True;
    end;

    RightVar := FRight.Compile(Offset);
    if DoneAssignment then
      if (TLapeTree_Operator(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
        DoneAssignment := False
      else
        Result := LeftVar;
  end
  else
    RightVar := NullResVar;

  try
    if (not DoneAssignment) then
    begin
      if (LeftVar.VarType = nil) and (RightVar.VarType = nil) then
        LapeException(lpeInvalidEvaluation, DocPos)
      else if (LeftVar.VarType = nil) then
      begin
        LeftVar := RightVar;
        RightVar := NullResVar;
      end;
      if (LeftVar.VarType = nil) then
        LapeException(lpeInvalidEvaluation, DocPos);

      try
        Result := LeftVar.VarType.Eval(FOperatorType, FDest, LeftVar, RightVar, Offset, @_DocPos);
      except on E: lpException do
        LapeException(E.Message, DocPos);
      end;
    end;
  finally
    if ((FLeft <> nil) or (FRight <> nil)) then
      setNullResVar(LeftVar, 1);
    if ((FLeft <> nil) and (FRight <> nil)) then
      setNullResVar(RightVar, 1);
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
  if (FGlobalVar = nil) then
    Result := False
  else
    Result := FGlobalVar.isConstant;
end;

function TLapeTree_GlobalVar.resType: TLapeType;
begin
  if (FGlobalVar = nil) then
    Result := nil
  else
    Result := FGlobalVar.VarType;
end;

function TLapeTree_GlobalVar.Evaluate: TLapeGlobalVar;
begin
  if (FGlobalVar = nil) then
    Result := nil
  else
    Result := FGlobalVar;
end;

function TLapeTree_GlobalVar.Compile(var Offset: Integer): TResVar;
begin
  Result := getResVar(FGlobalVar);
end;

constructor TLapeTree_WithVar.Create(AWithDeclRec: TLapeWithDeclRec; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  FWithDeclRec := AWithDeclRec;
end;

function TLapeTree_WithVar.isConstant: Boolean;
begin
  Result := (FWithDeclRec.WithVar <> nil) and
    (FWithDeclRec.WithVar^ <> nil) and
    (FWithDeclRec.WithVar^ is TLapeGlobalVar) and
    TLapeGlobalVar(FWithDeclRec.WithVar^).isConstant;
end;

function TLapeTree_WithVar.resType: TLapeType;
begin
  Result := FWithDeclRec.WithType;
end;

function TLapeTree_WithVar.Evaluate: TLapeGlobalVar;
begin
  if isConstant then
    Result := TLapeGlobalVar(FWithDeclRec.WithVar^)
  else
    Result := nil;
end;

function TLapeTree_WithVar.Compile(var Offset: Integer): TResVar;
begin
  if (FWithDeclRec.WithVar = nil) or (FWithDeclRec.WithVar^ = nil) then
    LapeException(lpeInvalidWithReference, DocPos);
  Result := getResVar(FWithDeclRec.WithVar^);
end;

constructor TLapeTree_VarType.Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
var
  TType: TLapeType_Type;
begin
  Assert(ACompiler <> nil);
  TType := TLapeType_Type(ACompiler.addManagedType(TLapeType_Type.Create(AVarType, ACompiler, '', ADocPos)));
  inherited Create(TType.NewGlobalVarP(), ACompiler, ADocPos);
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
  if_o := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
  PUInt32(FMethod.Ptr)^ := FCompiler.Emitter.getCodeOffset(Offset);
  FCompiler.Emitter.addCodePointer(FMethod.Ptr);
  FCompiler.IncStackInfo(FStackInfo, Offset, True, @_DocPos);
  Result := FStatements.Compile(Offset);

  for i := 0 to FExitStatements.Count - 1 do
    with FExitStatements[i] do
    begin
      co := CodeOffset;
      FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos);
    end;

  FCompiler.DecStackInfo(Offset, True, True, False, @_DocPos);
  FCompiler.Emitter._JmpR(Offset - if_o, if_o, @_DocPos);
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
  FVars := TLapeVarDeclList.Create(NullVar);
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
  wasConstant: Boolean;
begin
  Result := NullResVar;
  i := 0;
  while (i < FVars.Count) do
    if (FVars[i].VarDecl <> nil) and (FVars[i].Default <> nil) then
      with FVars[i], TLapeTree_Operator.Create(op_Assign, Compiler, @Default._DocPos) do
      try
        wasConstant := (VarDecl is TLapeVar) and TLapeVar(VarDecl).isConstant;
        if wasConstant then
          TLapeVar(VarDecl).isConstant := False;

        Left := TLapeTree_ResVar.Create(getResVar(VarDecl), Compiler, @Default._DocPos);
        Right := Default;
        Result := Compile(Offset);

        if wasConstant then
          TLapeVar(VarDecl).isConstant := True;
      finally
        Free();
      end
    else
      Inc(i);
end;

procedure TLapeTree_With.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

constructor TLapeTree_With.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBody := nil;
  FWithList := TLapeExpressionList.Create(nil);
end;

destructor TLapeTree_With.Destroy;
var
  i: Integer;
begin
  for i := FWithList.Count - 1 downto 0 do
    if (FWithList[i] <> nil) and (FWithList[i].Parent = Self) then
      FWithList[i].Free();
  FreeAndNil(FWithList);

  setBody(nil);
  inherited;
end;

function TLapeTree_With.addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclRec;
var
  i: Integer;
begin
  Result := NullWithDecl;
  i := FWithList.add(AWith);

  if (AWith <> nil) and (i > -1) then
  begin
    AWith.Parent := Self;
    SetLength(FVarList, FWithList.Count);

    if (AWith is TLapeTree_GlobalVar) and TLapeTree_GlobalVar(AWith).isConstant then
      FVarList[i] := TLapeTree_GlobalVar(AWith).GlobalVar
    else
      FVarList[i] := nil;

    Result.WithVar := @FVarList[i];
    Result.WithType := AWith.resType();
  end;

  if (Result.WithType = nil) or (not Result.WithType.CanHaveChild()) then
    LapeException(lpeInvalidWithReference, [AWith, Self])
end;

function TLapeTree_With.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  NewStack: Boolean;
  ResVarList: array of TResVar;
begin
  Result := NullResVar;
  Assert(FCompiler <> nil);

  SetLength(ResVarList, Length(FVarList));
  NewStack := (FCompiler.StackInfo = nil);
  if NewStack then
    FCompiler.IncStackInfo(True);

  for i := 0 to FWithList.Count - 1 do
    if (FVarList[i] = nil) then
    begin
      if (not getTempVar(FWithList[i], Offset, ResVarList[i], 2)) or (ResVarList[i].VarPos.MemPos in [mpNone, mpStack]) then
        LapeException(lpeInvalidCondition, FWithList[i].DocPos);

      FVarList[i] := ResVarList[i].VarPos.StackVar;
      FVarList[i].isConstant := FWithList[i].isConstant();
    end;

  if (FBody <> nil) then
    Result := FBody.Compile(Offset);

  for i := 0 to High(ResVarList) do
    setNullResVar(ResVarList[i], 2);

  if NewStack then
    FCompiler.DecStackInfo(False, True, True);
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
  ConditionVar, tmpCondition, tmpVar: TResVar;
  if_o, if_e: Integer;
begin
  Assert(FCondition <> nil);

  Result := NullResVar;
  FStartBodyOffset := 0;
  tmpVar := NullResVar;

  ConditionVar := FCondition.Compile(Offset);
  if (ConditionVar.VarType = nil) or (not (ConditionVar.VarType.BaseType in LapeIfTypes)) then
    LapeException(lpeInvalidCondition, FCondition.DocPos);

  if (not (ConditionVar.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmpCondition := ConditionVar;
    if FCompiler.getBaseType(ltEvalBool).CompatibleWith(ConditionVar.VarType) then
      ConditionVar := FCompiler.getBaseType(ltEvalBool).Eval(op_Assign, tmpVar, FCompiler.getTempStackVar(ltEvalBool), ConditionVar, Offset, @FCondition._DocPos)
    else if (ConditionVar.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition._DocPos)
    else
      ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition._DocPos);
    setNullResVar(tmpCondition);
  end;

  if_o := FCompiler.Emitter._JmpRIfNot(0, ConditionVar, Offset, @_DocPos);
  if (FBody <> nil) or (FElse = nil) then
  begin
    Result := CompileBody(Offset);

    if (FElse <> nil) then
      if_e := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
    FCompiler.Emitter._JmpRIfNot(Offset - if_o, ConditionVar, if_o, @_DocPos);
  end;

  if (FElse <> nil) then
  begin
    Result := FElse.Compile(Offset);

    if (FBody <> nil) then
      FCompiler.Emitter._JmpR(Offset - if_e, if_e, @_DocPos)
    else
      FCompiler.Emitter._JmpRIf(Offset - if_o, ConditionVar, if_o, @_DocPos);
  end;

  setNullResVar(ConditionVar, 1);
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
  Create(Ident, OpenArray.Compiler, @OpenArray._DocPos);
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
  ConditionVar: TResVar;
  CheckField, opOR: TLapeTree_Operator;
  tmpExpr: TLapeTree_ExprBase;
begin
  Result := NullResVar;
  Assert((FValues.Count > 0) or (FElse <> nil));
  Assert(FCondition <> nil);

  CheckField := nil;
  opOR := nil;
  if (not getTempVar(FCondition, Offset, ConditionVar)) then
    LapeException(lpeInvalidCondition, DocPos);

  try

    for i := FValues.Count - 1 downto 0 do
    begin
      if (FValues[i] is TLapeTree_Range) then
        with TLapeTree_Range(FValues[i]) do
        begin
          CheckField := TLapeTree_Operator.Create(op_AND, FCompiler, @_DocPos);
          CheckField.Left := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, FCompiler, @_DocPos);
          with TLapeTree_Operator(CheckField.Left) do
          begin
            Left := TLapeTree_ResVar.Create(ConditionVar, FCompiler, @FCondition._DocPos);
            Right := Lo;
          end;
          CheckField.Right := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, FCompiler, @_DocPos);
          with TLapeTree_Operator(CheckField.Right) do
          begin
            Left := TLapeTree_ResVar.Create(ConditionVar, FCompiler, @FCondition._DocPos);
            Right := Hi;
          end;
        end
      else if (FValues[i] is TLapeTree_ExprBase) then
      begin
        CheckField := TLapeTree_Operator.Create(op_cmp_Equal, FCompiler, @FValues[i]._DocPos);
        CheckField.Left := TLapeTree_ResVar.Create(ConditionVar, FCompiler, @FCondition._DocPos);
        CheckField.Right := TLapeTree_ExprBase(FValues[i]);
      end
      else
        LapeException(lpeInvalidEvaluation, DocPos);

      if (opOR = nil) then
        opOR := CheckField
      else
      begin
        tmpExpr := opOR;
        opOR := TLapeTree_Operator.Create(op_OR, FCompiler, @_DocPos);
        opOR.Left := CheckField;
        opOR.Right := tmpExpr;
      end;
      CheckField := nil;
    end;

    tmpExpr := FCondition;
    FCondition := opOR;
    try
      if (FCondition <> nil) then
        Result := inherited
      else
        Result := FElse.Compile(Offset);
    finally
      FCondition := tmpExpr;
      FreeAndnil(opOR);
    end;

  except
    if (CheckField <> nil) then
      CheckField.Free();
    if (opOR <> nil) then
      opOR.Free();
    raise;
  end;

  setNullResVar(ConditionVar, 2);
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
  ConditionVar: TResVar;
begin
  Result := NullResVar;
  Assert(FCondition <> nil);

  if (FFields.Count > 0) then
  begin
    if (not getTempVar(FCondition, Offset, ConditionVar)) then
      LapeException(lpeInvalidCondition, DocPos);

    for i := 0 to FFields.Count - 1 do
      FFields[i].Condition := TLapeTree_ResVar.Create(ConditionVar, FCompiler, @FCondition._DocPos);
    FFields[FFields.Count - 1].ElseBody := FElse;
    for i := FFields.Count - 1 downto 1 do
      FFields[i - 1].ElseBody := FFields[i];
    Result := FFields[0].Compile(Offset);

    setNullResVar(ConditionVar, 2);
  end
  else if (FElse <> nil) then
    Result := FElse.Compile(Offset);
end;

function TLapeTree_While.CompileBody(var Offset: Integer): TResVar;
var
  i, co: Integer;
  ConditionVar, tmpCondition, tmpVar: TResVar;
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

  tmpVar := NullResVar;
  ConditionVar := FCondition.Compile(Offset);

  if (not (ConditionVar.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmpCondition := ConditionVar;
    if FCompiler.getBaseType(ltEvalBool).CompatibleWith(ConditionVar.VarType) then
      ConditionVar := FCompiler.getBaseType(ltEvalBool).Eval(op_Assign, tmpVar, FCompiler.getTempStackVar(ltEvalBool), ConditionVar, Offset, @FCondition._DocPos)
    else if (ConditionVar.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition._DocPos)
    else
      ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition._DocPos);
    setNullResVar(tmpCondition);
  end;

  FCompiler.Emitter._JmpRIf(FStartBodyOffset - Offset, ConditionVar, Offset, @_DocPos);
  setNullResVar(ConditionVar);
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
  IncDec: TLapeTree_InternalMethod;
  tmpBody: TLapeTree_Base;
  tmpContinueStatements: TLapeFlowStatementList;
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
    IncDec := TLapeTree_InternalMethod_Dec.Create(FCompiler, @_DocPos)
  else
    IncDec := TLapeTree_InternalMethod_Inc.Create(FCompiler, @_DocPos);
  try
    IncDec.addParam(TLapeTree_ResVar.Create(TLapeTree_ResVar(TLapeTree_Operator(FCondition).Left).ResVar, FCompiler, @FCounter._DocPos));
    if (FStep <> nil) then
    begin
      IncDec.addParam(FStep);
      IncDec.Compile(Offset);
      Step := IncDec.Params.Delete(1);
    end
    else
      IncDec.Compile(Offset);
  finally
    IncDec.Free();
  end;

  tmpBody := FBody;
  tmpContinueStatements := FContinueStatements;
  try
    FBody := nil;
    FContinueStatements := nil;
    inherited;
  finally
    FBody := tmpBody;
    FContinueStatements := tmpContinueStatements;
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
  Count, Lim: TResVar;
  CounterVar: TLapeVar;
begin
  Result := NullResVar;
  Assert(FCondition = nil);
  Assert(FCounter <> nil);
  Assert(FLimit <> nil);

  CounterVar := nil;
  Count := FCounter.Compile(Offset);
  try
    if (not getTempVar(FLimit, Offset, Lim)) or (Lim.VarType = nil) or (Lim.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation, FLimit.DocPos);

    if (Count.VarType <> nil) and (not isVariable(Count)) then
    begin
      CounterVar := FCompiler.getTempVar(Count.VarType, 2);
      CounterVar.isConstant := False;
      Count := CounterVar.VarType.Eval(op_Assign, Result, GetResVar(CounterVar), Count, Offset, @FCounter._DocPos);
    end;
    if (Count.VarType = nil) or (not isVariable(Count)) or (Count.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidIterator, FCounter.DocPos);

    if WalkDown then
      FCondition := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, FCompiler, @_DocPos)
    else
      FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, FCompiler, @_DocPos);
    TLapeTree_Operator(FCondition).Left := TLapeTree_ResVar.Create(Count, FCompiler, @FCounter._DocPos);
    TLapeTree_Operator(FCondition).Right := TLapeTree_ResVar.Create(Lim, FCompiler, @FLimit._DocPos);

    Result := inherited;
  finally
    setCondition(nil);
    if (CounterVar <> nil) then
      setNullResVar(Count, 2)
    else
      setNullResVar(Count, 1);
    setNullResVar(Lim, 2);
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
  i, co, StartOffset: Integer;
  ConditionVar, tmpCondition, tmpVar: TResVar;
begin
  FBreakStatements.Clear();
  FContinueStatements.Clear();

  Result := NullResVar;
  StartOffset := FCompiler.Emitter.CheckOffset(Offset);
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

  tmpVar := NullResVar;
  ConditionVar := FCondition.Compile(Offset);

  if (not (ConditionVar.VarType.Size in [1, 2, 4, 8])) then
  begin
    tmpCondition := ConditionVar;
    if FCompiler.getBaseType(ltEvalBool).CompatibleWith(ConditionVar.VarType) then
      ConditionVar := FCompiler.getBaseType(ltEvalBool).Eval(op_Assign, tmpVar, FCompiler.getTempStackVar(ltEvalBool), ConditionVar, Offset, @FCondition._DocPos)
    else if (ConditionVar.VarType.BaseType in LapeStringTypes + LapeCharTypes) then
      ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltString).NewGlobalVarStr(''))), Offset, @FCondition._DocPos)
    else
      ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, getResVar(FCompiler.addManagedVar(FCompiler.getBaseType(ltInt32).NewGlobalVarStr('0'))), Offset, @FCondition._DocPos);
    setNullResVar(tmpCondition);
  end;
  FCompiler.Emitter._JmpRIfNot(StartOffset - Offset, ConditionVar, Offset, @_DocPos);

  for i := 0 to FBreakStatements.Count - 1 do
    with FBreakStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  setNullResVar(ConditionVar);
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

  o_try := FCompiler.Emitter._IncTry(0, 0, Offset, @_DocPos);
  o_except := 0;
  if (FBody <> nil) then
    Result := FBody.Compile(Offset);

  FCompiler.Emitter._DecTry(Offset, @_DocPos);
  if (FExcept <> nil) then
  begin
    o_jmp := FCompiler.Emitter._JmpR(0, Offset, @FExcept._DocPos);
    if (FFinally = nil) then
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoFinally, o_try, @_DocPos)
    else
      o_except := Offset;
    FCompiler.Emitter._CatchException(Offset, @FExcept._DocPos);
    FExcept.Compile(Offset);
    FCompiler.Emitter._JmpR(Offset - o_jmp, o_jmp, @FExcept._DocPos);
  end;
  if (FFinally <> nil) then
  begin
    if (o_except <> 0) then
      FCompiler.Emitter._IncTry(Offset - o_try, Offset - o_except, o_try, @_DocPos)
    else
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoExcept, o_try, @_DocPos);
    FFinally.Compile(Offset);
    FCompiler.Emitter._EndTry(Offset, @FFinally._DocPos);
  end;
end;

end.

