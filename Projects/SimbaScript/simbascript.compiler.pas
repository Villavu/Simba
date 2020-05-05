unit simbascript.compiler;

{$mode objfpc}{$H+}

interface                         

uses
  classes, sysutils, typinfo, fgl,
  ffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers;

type
  TSimbaScript_Compiler = class;
  TSimbaScript_Import = procedure(Compiler: TSimbaScript_Compiler; Data: Pointer);
  TSimbaScript_DebuggingMethods = array of ShortString;

  TSimbaScript_Compiler = class(TLapeCompiler)
  protected
    FManagedClosures: array of TImportClosure;
    FSection: String;
    FDebugging: Boolean;
    FDebuggingMethods: TSimbaScript_DebuggingMethods;
  public
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method; override;

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

    procedure addBaseDefine(Define: lpString; Value: lpString); virtual; overload;

    procedure addClass(const Name: String; const Parent: String = 'TObject');
    procedure addClassVar(const Obj, Item, Typ: String; const ARead: Pointer; const AWrite: Pointer = nil; const Arr: Boolean = False; const ArrType: string = 'UInt32');

    procedure Import(Imports: array of TSimbaScript_Import; Data: Pointer); overload;
    procedure Import(Data: Pointer); overload;

    property Section: String read FSection write FSection;

    property Debugging: Boolean read FDebugging write FDebugging;
    property DebuggingMethods: TSimbaScript_DebuggingMethods read FDebuggingMethods;

    destructor Destroy; override;
  end;

implementation

uses
  lpffi, lputils,

  // Base types
  simbascript.import_types,

  // Lazarus classes
  simbascript.import_tobject,
  simbascript.import_lclsystem,
  simbascript.import_lclgraphics,
  simbascript.import_lclcontrols,
  simbascript.import_lclforms,
  simbascript.import_lclstdctrls,
  simbascript.import_lclextctrls,
  simbascript.import_lclcomctrls,
  simbascript.import_lcldialogs,
  simbascript.import_lclmenus,
  simbascript.import_lclspin,
  simbascript.import_lclprocess,
  simbascript.import_lclregexpr,

  // Simba classes
  simbascript.import_tmdtm,
  simbascript.import_tmdtms,
  simbascript.import_tmufasabitmap,
  simbascript.import_tmbitmaps,
  simbascript.import_tmfiles,
  simbascript.import_tmfinder,
  simbascript.import_tmfont,
  simbascript.import_tmfonts,
  simbascript.import_tmocr,
  simbascript.import_ttarget,
  simbascript.import_tiomanager,
  simbascript.import_tclient,
  simbascript.import_tmmltimer,
  simbascript.import_json,
  simbascript.import_xml,
  simbascript.import_simbaimagebox,

  // Simba
  simbascript.import_system,
  simbascript.import_target,
  simbascript.import_input,
  simbascript.import_finder,
  simbascript.import_web,
  simbascript.import_arrays_algorithms,
  simbascript.import_matrix,
  simbascript.import_math,
  simbascript.import_time_date,
  simbascript.import_ocr,
  simbascript.import_string,
  simbascript.import_colormath,
  simbascript.import_bitmap,
  simbascript.import_dtm,
  simbascript.import_file,
  simbascript.import_other,
  simbascript.import_crypto,
  simbascript.import_deprecated,
  simbascript.import_oswindow,
  simbascript.import_dialog,
  simbascript.import_simba,
  simbascript.import_process;

function TSimbaScript_Compiler.addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar;
begin
  SetLength(FManagedClosures, Length(FManagedClosures) + 1);
  FManagedClosures[High(FManagedClosures)] := LapeImportWrapper(Value, Self, Header, ABI);

  Result := inherited addGlobalFunc(Header, FManagedClosures[High(FManagedClosures)].Func);
end;

procedure TSimbaScript_Compiler.addBaseDefine(Define: lpString; Value: lpString);
begin
  FBaseDefines[Define] := Trim(Value);
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
  with addGlobalType(Str, '_' + AName) do
  begin
    Result := addGlobalType('native (_' + AName + ', ' + GetEnumName(TypeInfo(TFFIABI), Ord(ABI)) + ')', AName);

    Name := '!' + AName;
  end;
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

procedure TSimbaScript_Compiler.Import(Imports: array of TSimbaScript_Import; Data: Pointer);
var
  I: Int32;
begin
  StartImporting();

  try
    if not FFILoaded then
      raise Exception.Create('libFFI is missing or incompatible');

    addGlobalVar('array of ShortString', @FDebuggingMethods, '_DebuggingMethods');

    addDelayedCode('{$HINTS OFF} procedure _EnterMethod(constref Index: Int32); begin end;');
    addDelayedCode('{$HINTS OFF} procedure _LeaveMethod(constref Index: Int32); begin end;');

    InitializeFFI(Self);
    InitializePascalScriptBasics(Self, [psiTypeAlias, psiSettings, psiMagicMethod, psiFunctionWrappers, psiExceptions]);

    ExposeGlobals(Self, [egmInvoke]);

    for I := 0 to High(Imports) do
      Imports[I](Self, Data);

    if FDebugging then
      addBaseDefine('DEBUGGING');
  finally
    EndImporting();
  end;
end;

procedure TSimbaScript_Compiler.Import(Data: Pointer);
begin
  Import([
    @Lape_Import_Types,
    @Lape_Import_TObject,
    @Lape_Import_LCLSystem,
    @Lape_Import_LCLGraphics,
    @Lape_Import_LCLControls,
    @Lape_Import_LCLForms,
    @Lape_Import_LCLStdCtrls,
    @Lape_Import_LCLExtCtrls,
    @Lape_Import_LCLComCtrls,
    @Lape_Import_LCLDialogs,
    @Lape_Import_LCLMenus,
    @Lape_Import_LCLSpinCtrls,
    @Lape_Import_LCLProcess,
    @Lape_Import_LCLRegExpr,

    @Lape_Import_TMDTM,
    @Lape_Import_TMDTMS,
    @Lape_Import_TMufasaBitmap,
    @Lape_Import_TMBitmaps,
    @Lape_Import_TMFiles,
    @Lape_Import_TMFinder,
    @Lape_Import_TMFont,
    @Lape_Import_TMFonts,
    @Lape_Import_TMOCR,
    @Lape_Import_TTarget,
    @Lape_Import_TIOManager,
    @Lape_Import_TClient,
    @Lape_Import_TMMLTimer,
    @Lape_Import_JSON,
    @Lape_Import_XML,
    @Lape_Import_SimbaImageBox,

    @Lape_Import_System,
    @Lape_Import_OSWindow,
    @Lape_Import_Target,
    @Lape_Import_Input,
    @Lape_Import_Finder,
    @Lape_Import_Web,
    @Lape_Import_Arrays_Algorithms,
    @Lape_Import_Matrix,
    @Lape_Import_Math,
    @Lape_Import_Time_Date,
    @Lape_Import_OCR,
    @Lape_Import_String,
    @Lape_Import_ColorMath,
    @Lape_Import_Bitmap,
    @Lape_Import_DTM,
    @Lape_Import_File,
    @Lape_Import_Other,
    @Lape_Import_Crypto,
    @Lape_Import_Deprecated,
    @Lape_Import_Dialogs,
    @Lape_Import_Simba,
    @Lape_Import_Process
  ], Data);
end;

procedure TSimbaScript_Compiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

function TSimbaScript_Compiler.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method;
var
  EnterMethod, LeaveMethod: TLapeTree_Invoke;
  Statement: TLapeTree_Try;
begin
  Result := inherited ParseMethod(FuncForwards, FuncHeader, FuncName, isExternal);

  if FDebugging then
  begin
    if (Result <> nil) and (Result.Statements <> nil) and hasDefine('DEBUGGING') then
    begin
      FuncName := UpperCase(FuncName);
      if (FuncName[1] = '!') or (FuncName = '_ENTERMETHOD') or (FuncName = '_LEAVEMETHOD') then
        Exit;

      if (FuncHeader <> nil) and (FuncHeader is TLapeType_MethodOfType) then
        FuncName := UpperCase(TLapeType_MethodOfType(FuncHeader).ObjectType.Name) + '.' + FuncName;

      SetLength(FDebuggingMethods, Length(FDebuggingMethods) + 1);
      FDebuggingMethods[High(FDebuggingMethods)] := FuncName;

      EnterMethod := TLapeTree_Invoke.Create('_EnterMethod', Self);
      EnterMethod.addParam(TLapeTree_Integer.Create(High(FDebuggingMethods), EnterMethod));

      LeaveMethod := TLapeTree_Invoke.Create('_LeaveMethod', Self);
      LeaveMethod.addParam(TLapeTree_Integer.Create(High(FDebuggingMethods), LeaveMethod));

      Result.Statements.addStatement(EnterMethod, True);

      Statement := TLapeTree_Try.Create(Self);
      Statement.Body := Result.Statements;
      Statement.FinallyBody := LeaveMethod;

      Result.Statements := TLapeTree_StatementList.Create(Self);
      Result.Statements.AddStatement(Statement);
    end;
  end;
end;

destructor TSimbaScript_Compiler.Destroy;
var
  I: Int32;
begin
  for I := 0 to High(FManagedClosures) do
    FManagedClosures[I].Free();

  inherited Destroy();
end;

end.

