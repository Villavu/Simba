unit simba.script_compiler;

{$mode objfpc}{$H+}
{$i simba.inc}

interface                         

uses
  classes, sysutils, typinfo,
  ffi, lpffi, lputils, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers;

type
  TSimbaScript_Compiler = class(TLapeCompiler)
  public
  type
    TManagedImportClosure = class(TLapeDeclaration)
      Closure: TImportClosure;
    end;
  protected
    FSection: lpString;
  public
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    function addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar; virtual; overload;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; virtual; overload;

    procedure addClass(Name: lpString; Parent: lpString = 'TObject'); virtual;
    procedure addClassVar(Obj, Item, Typ: lpString; ARead: Pointer; AWrite: Pointer = nil; Arr: Boolean = False; ArrType: lpString = 'UInt32'); virtual;

    procedure Import; virtual;
    function Compile: Boolean; override;

    property Section: lpString read FSection write FSection;
  end;

implementation

uses
  simba.script_compiler_onterminate,
  simba.script_compiler_waituntil,

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

function TSimbaScript_Compiler.addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar;
var
  Closure: TManagedImportClosure;
begin
  Closure := TManagedImportClosure.Create();
  Closure.Closure := LapeImportWrapper(Value, Self, Header, ABI);

  with TManagedImportClosure(addManagedDecl(Closure)) do
    Result := inherited addGlobalFunc(Header, Closure.Func);
end;

function TSimbaScript_Compiler.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  Result := addGlobalType(Format('native(type %s, %s)', [Str, GetEnumName(TypeInfo(TFFIABI), Ord(ABI))]), AName);
end;

procedure TSimbaScript_Compiler.addClass(Name: lpString; Parent: lpString);
begin
  addGlobalType(Format('type %s', [Parent]), Name);
end;

procedure TSimbaScript_Compiler.addClassVar(Obj, Item, Typ: lpString; ARead: Pointer; AWrite: Pointer; Arr: Boolean; ArrType: lpString);
var
  Param: lpString = '';
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

procedure TSimbaScript_Compiler.Import;
begin
  StartImporting();

  FSection := 'External';
  InitializeFFI(Self);

  FSection := 'Types';
  InitializePascalScriptBasics(Self, [psiTypeAlias, psiSettings, psiFunctionWrappers, psiExceptions]);

  FSection := 'System';

  InitializeAddOnTerminate(Self);
  InitializeWaitUntil(Self);

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

  EndImporting();
end;

function TSimbaScript_Compiler.Compile: Boolean;
begin
  {$IF DEFINED(DARWIN) and DECLARED(LoadFFI)}
  if not FFILoaded then
    LoadFFI('/usr/local/opt/libffi/lib/');
  {$ENDIF}

  if not FFILoaded then
    raise Exception.Create('ERROR: libffi is missing or incompatible');

  Result := inherited Compile();
end;

procedure TSimbaScript_Compiler.pushTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  inherited pushTokenizer(ATokenizer);
end;

procedure TSimbaScript_Compiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

end.

