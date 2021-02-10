unit simba.script_compiler;

{$mode objfpc}{$H+}
{$modeswitch arrayoperators}

interface                         

uses
  classes, sysutils, typinfo,
  ffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers;

type
  TSimbaScript_Tokenzier = class(TLapeTokenizerFile)
  public
    constructor Create(AScript, AScriptName: String); reintroduce;
  end;

  TSimbaScript_Compiler = class(TLapeCompiler)
  public
  type
    TEnterMethodEvent = procedure(Sender: TObject; Index: Int32) of object;
    TLeaveMethodEvent = procedure(Sender: TObject; Index: Int32; Exception: Boolean) of object;

    TManagedImportClosure = class(TLapeDeclaration)
      Closure: TImportClosure;
    end;
  protected
    FSection: String;
    FDebugging: Boolean;
    FDebuggingMethods: TStringArray;

    FOnEnterMethod: TEnterMethodEvent;
    FOnLeaveMethod: TLeaveMethodEvent;

    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method; override;
  public
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    function addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar; overload;

    function addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar; virtual; overload;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; virtual; overload;

    procedure addClass(const Name: String; const Parent: String = 'TObject');
    procedure addClassVar(const Obj, Item, Typ: String; const ARead: Pointer; const AWrite: Pointer = nil; const Arr: Boolean = False; const ArrType: string = 'UInt32');

    function Compile: Boolean; override;

    property Section: String read FSection write FSection;

    property Debugging: Boolean read FDebugging write FDebugging;
    property DebuggingMethods: TStringArray read FDebuggingMethods;

    property OnEnterMethod: TEnterMethodEvent read FOnEnterMethod write FOnEnterMethod;
    property OnLeaveMethod: TLeaveMethodEvent read FOnLeaveMethod write FOnLeaveMethod;

    constructor Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True; AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True); override;
  end;

implementation

uses
  lpffi, lputils, lpmessages,

  // Base types
  simba.script_import_types,

  // Lazarus classes
  simba.script_import_tobject,
  simba.script_import_lclsystem,
  simba.script_import_lclgraphics,
  simba.script_import_lclcontrols,
  simba.script_import_lclforms,
  simba.script_import_lclstdctrls,
  simba.script_import_lclextctrls,
  simba.script_import_lclcomctrls,
  simba.script_import_lcldialogs,
  simba.script_import_lclmenus,
  simba.script_import_lclspin,
  simba.script_import_lclprocess,
  simba.script_import_lclregexpr,

  // Simba classes
  simba.script_import_tmdtm,
  simba.script_import_tmdtms,
  simba.script_import_tmufasabitmap,
  simba.script_import_tmbitmaps,
  simba.script_import_tmfiles,
  simba.script_import_tmfinder,
  simba.script_import_tmfont,
  simba.script_import_tmfonts,
  simba.script_import_tmocr,
  simba.script_import_ttarget,
  simba.script_import_tiomanager,
  simba.script_import_tclient,
  simba.script_import_tmmltimer,
  simba.script_import_json,
  simba.script_import_xml,
  simba.script_import_simbaimagebox,

  // Simba
  simba.script_import_system,
  simba.script_import_target,
  simba.script_import_input,
  simba.script_import_finder,
  simba.script_import_web,
  simba.script_import_arrays_algorithms,
  simba.script_import_matrix,
  simba.script_import_math,
  simba.script_import_time_date,
  simba.script_import_ocr,
  simba.script_import_string,
  simba.script_import_colormath,
  simba.script_import_bitmap,
  simba.script_import_dtm,
  simba.script_import_file,
  simba.script_import_other,
  simba.script_import_crypto,
  simba.script_import_deprecated,
  simba.script_import_oswindow,
  simba.script_import_dialog,
  simba.script_import_simba,
  simba.script_import_process,
  simba.script_import_matchtemplate;

constructor TSimbaScript_Tokenzier.Create(AScript, AScriptName: String);
begin
  inherited Create(AScript);

  if (AScriptName <> '') then
    FFileName := AScriptName;
end;

procedure Lape_EnterMethod(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Compiler: TSimbaScript_Compiler;
begin
  Compiler := TSimbaScript_Compiler(Params^[0]);
  if (Compiler.OnEnterMethod <> nil) then
    Compiler.OnEnterMethod(Compiler, PInt32(Params^[1])^);
end;

procedure Lape_LeaveMethod(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Compiler: TSimbaScript_Compiler;
begin
  Compiler := TSimbaScript_Compiler(Params^[0]);
  if (Compiler.OnLeaveMethod <> nil) then
    Compiler.OnLeaveMethod(Compiler, PInt32(Params^[1])^, PBoolean(Params^[2])^);
end;

type
  TLapeTree_WaitUntil_Operator = class(TLapeTree_Operator) // Dont take ownership of FLeft
  protected
    procedure setLeft(Node: TLapeTree_ExprBase); override;
  end;

  TLapeTree_InternalMethod_WaitUntil = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

procedure TLapeTree_WaitUntil_Operator.setLeft(Node: TLapeTree_ExprBase);
begin
  FLeft := Node;
end;

function TLapeTree_InternalMethod_WaitUntil.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltEvalBool);

  Result := inherited;
end;

function TLapeTree_InternalMethod_WaitUntil.Compile(var Offset: Integer): TResVar;
var
  Loop: TLapeTree_While;
  Assignment: TLapeTree_Operator;
  Condition: TLapeTree_Operator;
  Interval, Timeout, Limit: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 3) then
    LapeException('Three parameters expected (Condition, Interval, Timeout)', DocPos);
  if isEmpty(FParams[0]) or (FParams[0].resType() = nil) or (not (FParams[0].resType().BaseType in LapeBoolTypes)) then
    LapeException('Condition parameter is invalid, Boolean expression expected.', DocPos);
  if isEmpty(FParams[1]) or (FParams[1].resType() = nil) or (not (FParams[1].resType().BaseType in LapeIntegerTypes)) then
    LapeException('Interval parameter is invalid, Integer expected.', DocPos);
  if isEmpty(FParams[2]) or (FParams[2].resType() = nil) or (not (FParams[2].resType().BaseType in LapeIntegerTypes)) then
    LapeException('Timeout parameter is invalid, Integer expected.', DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(resType()));

  FCompiler.VarToDefault(Result, Offset, @Self._DocPos);

  Interval := FParams[1].Compile(Offset);
  Timeout := FParams[2].Compile(Offset);

  Condition := TLapeTree_WaitUntil_Operator.Create(op_cmp_Equal, Self);
  Condition.Left := FParams[0];
  Condition.Right := TLapeTree_GlobalVar.Create('True', ltEvalBool, Self);

  // Limit := GetTickCount();
  with TLapeTree_Invoke.Create('GetTickCount', Self) do
  try
    Limit := Compile(Offset);
    Limit.Writeable := True;
  finally
    Free();
  end;

  // Limit := Limit + Timeout;
  with TLapeTree_Operator.Create(op_AssignPlus, Self) do
  try
    Left := TLapeTree_ResVar.Create(Limit.IncLock(), Self);
    Right := TLapeTree_ResVar.Create(Timeout.IncLock(), Self);
    Limit := Compile(Offset);
  finally
    Free();
  end;

  Loop := TLapeTree_While.Create(Self);

  try
    // while GetTickCount() < Limit do
    Loop.Condition := TLapeTree_Operator.Create(op_cmp_LessThan, Self);
    with TLapeTree_Operator(Loop.Condition) do
    begin
      Left := TLapeTree_Invoke.Create('GetTickCount', Self);
      Right := TLapeTree_ResVar.Create(Limit.IncLock(), Self);
    end;

    Loop.Body := TLapeTree_If.Create(Self);

    TLapeTree_If(Loop.Body).Condition := Condition;
    TLapeTree_If(Loop.Body).Body := TLapeTree_StatementList.Create(Self);

    // If Condition then
    // Result := True
    // Break;
    with TLapeTree_If(Loop.Body) do
    begin
      Assignment := TLapeTree_Operator.Create(op_Assign, Self);
      Assignment.Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Assignment.Right := TLapeTree_GlobalVar.Create('True', ltEvalBool, Self);

      TLapeTree_StatementList(Body).addStatement(Assignment);
      TLapeTree_StatementList(Body).addStatement(TLapeTree_InternalMethod_Break.Create(Body));
    end;

    // Else Sleep(Interval)
    TLapeTree_If(Loop.Body).ElseBody := TLapeTree_Invoke.Create('Sleep', Self);
    with TLapeTree_If(Loop.Body) do
      TLapeTree_Invoke(ElseBody).addParam(TLapeTree_ResVar.Create(Interval.IncLock(), Self));

    Loop.Compile(Offset).Spill(1);
  finally
    Loop.Free();
  end;

  Limit.Spill(2);
  Interval.Spill(1);
  Timeout.Spill(1);
end;

function TSimbaScript_Compiler.addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar;
var
  Closure: TManagedImportClosure;
begin
  Closure := TManagedImportClosure.Create();
  Closure.Closure := LapeImportWrapper(Value, Self, Header, ABI);

  with TManagedImportClosure(addManagedDecl(Closure)) do
    Result := inherited addGlobalFunc(Header, Closure.Func);
end;

function TSimbaScript_Compiler.addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;
end;

function TSimbaScript_Compiler.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  Result := addGlobalType(Format('native(type %s, %s)', [Str, GetEnumName(TypeInfo(TFFIABI), Ord(ABI))]), AName);
end;

procedure TSimbaScript_Compiler.addClass(const Name: String; const Parent: String);
begin
  addGlobalType(Format('type %s', [Parent]), Name);
end;

procedure TSimbaScript_Compiler.addClassVar(const Obj, Item, Typ: String; const ARead: Pointer; const AWrite: Pointer; const Arr: Boolean; const ArrType: string);
var
  Param: String = '';
begin
  if Arr then
    Param := 'const Index: ' + ArrType;

  if (ARead <> nil) then
    addGlobalFunc(Format('function %s.get%s(%s): %s; constref;', [Obj, Item, Param, Typ]), ARead);

  if Arr then
    Param += '; ';

  if (AWrite <> nil) then
    addGlobalFunc(Format('procedure %s.set%s(%sconst Value: %s); constref;', [Obj, Item, Param, Typ]), AWrite);
end;

function TSimbaScript_Compiler.Compile: Boolean;
begin
  {$IF DEFINED(DARWIN) and DECLARED(LoadFFI)}
  if not FFILoaded then
    LoadFFI('/usr/local/opt/libffi/lib/');
  {$ENDIF}

  if not FFILoaded then
    raise Exception.Create('ERROR: libffi is missing or incompatible');

  FSection := 'External';
  InitializeFFI(Self);

  FSection := 'Types';
  InitializePascalScriptBasics(Self, [psiTypeAlias, psiSettings, psiMagicMethod, psiFunctionWrappers, psiExceptions]);

  FSection := '';
  addGlobalVar('array of String', @FDebuggingMethods, '_DebuggingMethods');
  addGlobalMethod('procedure _EnterMethod(constref Index: Int32);', @Lape_EnterMethod, Self);
  addGlobalMethod('procedure _LeaveMethod(constref Index: Int32; Exception: Boolean);', @Lape_LeaveMethod, Self);
  if FDebugging then
    addBaseDefine('DEBUGGING');

  StartImporting();

  try
    Lape_Import_Types(Self);;
    Lape_Import_TObject(Self);
    Lape_Import_LCLSystem(Self);
    Lape_Import_LCLGraphics(Self);
    Lape_Import_LCLControls(Self);
    Lape_Import_LCLForms(Self);
    Lape_Import_LCLStdCtrls(Self);
    Lape_Import_LCLExtCtrls(Self);
    Lape_Import_LCLComCtrls(Self);
    Lape_Import_LCLDialogs(Self);
    Lape_Import_LCLMenus(Self);
    Lape_Import_LCLSpinCtrls(Self);
    Lape_Import_LCLProcess(Self);
    Lape_Import_LCLRegExpr(Self);

    Lape_Import_TMDTM(Self);
    Lape_Import_TMDTMS(Self);
    Lape_Import_TMufasaBitmap(Self);
    Lape_Import_TMBitmaps(Self);
    Lape_Import_TMFiles(Self);
    Lape_Import_TMFinder(Self);
    Lape_Import_TMFont(Self);
    Lape_Import_TMFonts(Self);
    Lape_Import_TMOCR(Self);
    Lape_Import_TTarget(Self);
    Lape_Import_TIOManager(Self);
    Lape_Import_TClient(Self);
    Lape_Import_TMMLTimer(Self);
    Lape_Import_JSON(Self);
    Lape_Import_XML(Self);
    Lape_Import_SimbaImageBox(Self);

    Lape_Import_System(Self);
    Lape_Import_OSWindow(Self);
    Lape_Import_Target(Self);
    Lape_Import_Input(Self);
    Lape_Import_MatchTemplate(Self);
    Lape_Import_Finder(Self);
    Lape_Import_Web(Self);
    Lape_Import_Arrays_Algorithms(Self);
    Lape_Import_Matrix(Self);
    Lape_Import_Math(Self);
    Lape_Import_Time_Date(Self);
    Lape_Import_OCR(Self);
    Lape_Import_String(Self);
    Lape_Import_ColorMath(Self);
    Lape_Import_Bitmap(Self);
    Lape_Import_DTM(Self);
    Lape_Import_File(Self);
    Lape_Import_Other(Self);
    Lape_Import_Crypto(Self);
    Lape_Import_Deprecated(Self);
    Lape_Import_Dialogs(Self);
    Lape_Import_Simba(Self);
    Lape_Import_Process(Self);
  finally
    EndImporting();
  end;

  Result := inherited Compile();
end;

constructor TSimbaScript_Compiler.Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean; AEmitter: TLapeCodeEmitter; ManageEmitter: Boolean);
begin
  inherited Create(ATokenizer, ManageTokenizer, AEmitter, ManageEmitter);

  FInternalMethodMap['WaitUntil'] := TLapeTree_InternalMethod_WaitUntil;
end;

procedure TSimbaScript_Compiler.pushTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  inherited pushTokenizer(ATokenizer);
end;

procedure TSimbaScript_Compiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

function TSimbaScript_Compiler.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method;
var
  EnterMethod, LeaveMethod: TLapeTree_Invoke;
  Statement: TLapeTree_Try;
  Test: TLapeTree_Operator;
begin
  Result := inherited ParseMethod(FuncForwards, FuncHeader, FuncName, isExternal);

  if hasDefine('DEBUGGING') then
  begin
    if (Result <> nil) and (Result.Statements <> nil) then
    begin
      FuncName := UpperCase(FuncName);
      if (FuncName = '') or (FuncName[1] = '!') or (FuncName = '_ENTERMETHOD') or (FuncName = '_LEAVEMETHOD') then
        Exit;

      if (FuncHeader <> nil) and (FuncHeader is TLapeType_MethodOfType) then
        FuncName := UpperCase(TLapeType_MethodOfType(FuncHeader).ObjectType.Name) + '.' + FuncName;

      FDebuggingMethods += [FuncName];

      EnterMethod := TLapeTree_Invoke.Create('_EnterMethod', Self);
      EnterMethod.addParam(TLapeTree_Integer.Create(High(FDebuggingMethods), EnterMethod));

      Test := TLapeTree_Operator.Create(op_cmp_NotEqual, Self);
      Test.Left := TLapeTree_InternalMethod_GetExceptionMessage.Create(Test);
      Test.Right := TLapeTree_String.Create('', Test);

      LeaveMethod := TLapeTree_Invoke.Create('_LeaveMethod', Self);
      LeaveMethod.addParam(TLapeTree_Integer.Create(High(FDebuggingMethods), LeaveMethod));
      LeaveMethod.addParam(Test);

      Result.Statements.addStatement(EnterMethod, True);

      Statement := TLapeTree_Try.Create(Self);
      Statement.Body := Result.Statements;
      Statement.FinallyBody := LeaveMethod;

      Result.Statements := TLapeTree_StatementList.Create(Self);
      Result.Statements.AddStatement(Statement);
    end;
  end;
end;

end.

