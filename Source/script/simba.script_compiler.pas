{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler;

{$i simba.inc}

interface

uses
  classes, sysutils, typinfo, contnrs,
  ffi, lpffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers, lpinterpreter,
  simba.mufasatypes;

type
  TSimbaScript_Compiler = class;
  TSimbaImport = procedure(Compiler: TSimbaScript_Compiler);
  TSimbaImportArray = array of TSimbaImport;

  TSimbaScript_Compiler = class(TLapeCompiler)
  protected class var
    Imports: TSimbaImportArray;
  public
    class procedure RegisterImport(Proc: TSimbaImport);
  public type
    TManagedImportClosure = class(TLapeDeclaration)
      Closure: TImportClosure;
    end;
  protected
    FImportingSection: String;
    FDump: TFPStringHashTable;

    function GetImportingSection: String;

    procedure InitBaseVariant; override;
    procedure InitBaseDefinitions; override;
  public
    property ImportingSection: String read GetImportingSection write FImportingSection;

    function getIntegerArray: TLapeType; override;
    function getFloatArray: TLapeType; override;

    function CurrentDir: String;

    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    procedure addDelayedCode(Code: TStringArray; AFileName: lpString = ''); virtual; overload;

    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; override;

    function addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method; virtual; overload;
    function addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar; virtual; overload;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; virtual; overload;
    function addGlobalType(Str: TStringArray; Name: String): TLapeType; virtual; overload;

    function addCallbackType(Str: String): TLapeType;

    procedure addClass(Name: lpString; Parent: lpString = 'TObject'); virtual;
    procedure addClassVar(Obj, Item, Typ: lpString; ARead: Pointer; AWrite: Pointer = nil; Arr: Boolean = False; ArrType: lpString = 'Integer'); virtual;

    procedure Import; virtual;
    function Compile: Boolean; override;

    procedure InvokeProc(Name: String);
    procedure InvokeProcFFI(Name: String);

    constructor Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean=True; AEmitter: TLapeCodeEmitter=nil; ManageEmitter: Boolean=True); reintroduce; override;
    destructor Destroy; override;
  end;

implementation

uses
  lpeval,

  simba.import_system,
  simba.import_matrix,
  simba.import_quad, simba.import_box, simba.import_boxarray, simba.import_point,

  // LCL
  simba.import_lcl_system, simba.import_lcl_graphics, simba.import_lcl_controls,
  simba.import_lcl_form, simba.import_lcl_stdctrls, simba.import_lcl_extctrls,
  simba.import_lcl_comctrls, simba.import_lcl_misc,

  // Simba classes
  simba.import_class_bitmap, simba.import_class_dtm, simba.import_matchtemplate,
  simba.import_class_finder, simba.import_class_target, simba.import_class_iomanager,
  simba.import_class_client, simba.import_class_xml, simba.import_class_json,
  simba.import_class_imagebox, simba.import_class_shapebox,

  // Simba
  simba.import_timing, simba.import_tpa, simba.import_atpa, simba.import_colormath,
  simba.import_hash, simba.import_compress, simba.import_windowhandle,
  simba.import_debugimage, simba.import_dialogs, simba.import_file,
  simba.import_internal, simba.import_finder,
  simba.import_math, simba.import_other, simba.import_input,
  simba.import_process, simba.import_script,  simba.import_slacktree,
  simba.import_string, simba.import_internet, simba.import_target,
  simba.import_variant, simba.import_simba, simba.import_random,

  simba.script_compiler_waituntil, simba.script_compiler_rtti;

function TSimbaScript_Compiler.addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method;
var
  OldState: Pointer;
begin
  OldState := getTempTokenizerState(LapeDelayedFlags + Header + LineEnding.Join(Body), '!' + Header);
  try
    Expect([tk_kw_Function, tk_kw_Procedure, tk_kw_Operator]);
    Result := ParseMethod(nil, False);
    CheckAfterCompile();
    addDelayedExpression(Result, True, True);
  finally
    resetTokenizerState(OldState);
  end;
end;

function TSimbaScript_Compiler.addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar;
var
  Closure: TManagedImportClosure;
begin
  Closure := TManagedImportClosure.Create();
  Closure.Closure := LapeImportWrapper(Value, Self, Header, ABI);

  with TManagedImportClosure(addManagedDecl(Closure)) do
    Result := addGlobalFunc(Header, Closure.Func);
end;

function TSimbaScript_Compiler.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  Result := addGlobalType(Format('native(type %s, %s)', [Str, GetEnumName(TypeInfo(TFFIABI), Ord(ABI))]), AName);
end;

function TSimbaScript_Compiler.addGlobalType(Str: TStringArray; Name: String): TLapeType;
begin
  Result := addGlobalType(LineEnding.Join(Str), Name);
end;

function TSimbaScript_Compiler.addCallbackType(Str: String): TLapeType;
begin
  Result := addGlobalType(Str.After('='), Str.Before('='), FFI_DEFAULT_ABI);
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
    addGlobalFunc(Format('function %s.Get%s(%s): %s;', [Obj, Item, Param, Typ]), ARead);

  if Arr then
    Param += '; ';

  if (AWrite <> nil) then
    addGlobalFunc(Format('procedure %s.Set%s(%sconst Value: %s);', [Obj, Item, Param, Typ]), AWrite);
end;

procedure TSimbaScript_Compiler.Import;
var
  Proc: TSimbaImport;
begin
  StartImporting();

  try
    Options := Options + [lcoLooseSemicolon, lcoAutoInvoke, lcoExplicitSelf, lcoAutoObjectify];

    ImportingSection := 'System';

    InitializeWaitUntil(Self);
    InitializeFFI(Self);
    InitializeRTTI(Self);

    addGlobalType('type Pointer', 'TClient');
    addGlobalVar('TClient', nil, 'Client'); // Will be assigned later

    ImportingSection := '';

    for Proc in Imports do
      Proc(Self);
  finally
    EndImporting();
  end;
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

procedure TSimbaScript_Compiler.InvokeProc(Name: String);
var
  Method: TLapeGlobalVar;
begin
  Method := Globals[Name];
  if (Method <> nil) then
    RunCode(FEmitter.Code, FEmitter.CodeLen, [], PCodePos(Method.Ptr)^);
end;

procedure TSimbaScript_Compiler.InvokeProcFFI(Name: String);
var
  Method: TLapeGlobalVar;
  Closure: TExportClosure;
begin
  Method := Globals[Name];

  if (Method <> nil) then
  try
    Closure := LapeExportWrapper(Method);

    TProcedure(Closure.Func)();
  finally
    Closure.Free();
  end;
end;

constructor TSimbaScript_Compiler.Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean; AEmitter: TLapeCodeEmitter; ManageEmitter: Boolean);
begin
  FDump := TFPStringHashTable.Create();

  inherited Create(ATokenizer, ManageTokenizer, AEmitter, ManageEmitter);
end;

destructor TSimbaScript_Compiler.Destroy;
begin
  if (FDump <> nil) then
    FreeAndNil(FDump);

  inherited Destroy();
end;

function TSimbaScript_Compiler.GetImportingSection: String;
begin
  if (FImportingSection = '') then
    FImportingSection := '!Simba';

  Result := FImportingSection;
end;

procedure TSimbaScript_Compiler.InitBaseVariant;
begin
  { nothing, we import our own variant }
end;

class procedure TSimbaScript_Compiler.RegisterImport(Proc: TSimbaImport);
begin
  Imports += [Proc];
end;

function TSimbaScript_Compiler.CurrentDir: String;
begin
  Result := '';
  if (Tokenizer <> nil) then
    Result := ExtractFileDir(Tokenizer.FileName);
end;

procedure TSimbaScript_Compiler.InitBaseDefinitions;
begin
  addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).createCopy(), 'Byte');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).createCopy(), 'Integer');
  addGlobalType(getPointerType(ltChar, False).createCopy(), 'PChar');

  inherited InitBaseDefinitions();
end;

function TSimbaScript_Compiler.getIntegerArray: TLapeType;
begin
  Result := getGlobalType('TIntegerArray');
  if (Result = nil) then
    Result := inherited;
end;

function TSimbaScript_Compiler.getFloatArray: TLapeType;
begin
  Result := getGlobalType('TExtendedArray');
  if (Result = nil) then
    Result := inherited;
end;

procedure TSimbaScript_Compiler.pushTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  inherited pushTokenizer(ATokenizer);
end;

procedure TSimbaScript_Compiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

procedure TSimbaScript_Compiler.addDelayedCode(Code: TStringArray; AFileName: lpString);
begin
  addDelayedCode(LapeDelayedFlags + LineEnding.Join(Code), AFileName);
end;

function TSimbaScript_Compiler.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;
  Result._DocPos.FileName := ImportingSection;
end;

end.

