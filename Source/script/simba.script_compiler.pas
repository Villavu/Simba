{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler;

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
    procedure addClassVar(Obj, Item, Typ: lpString; ARead: Pointer; AWrite: Pointer = nil; Arr: Boolean = False; ArrType: lpString = 'Integer'); virtual;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; reintroduce;

    procedure Import; virtual;
    function Compile: Boolean; override;
  end;

implementation

uses
  dialogs, extctrls, uitypes, graphtype, controls, comctrls, graphics,
  stdctrls, buttons, customtimer, checklst, lclclasses, spin, pipes,
  lclintf, math, regexpr, strutils, lazfileutils, fileutil, clipbrd,
  blowfish, md5, sha1, hmac, forms, process, lazloggerbase,

  simba.mufasatypes, simba.script, simba.files, simba.process, simba.bitmap,
  simba.windowhandlehelpers, simba.matchtemplate, simba.tpa,
  simba.target_exported, simba.math, simba.colormath, simba.stringutil,
  simba.internet, simba.datetime, simba.dtm, simba.iomanager,
  simba.aca, simba.dtmeditor, simba.script_communication,
  simba.imagebox, simba.client,simba.jsonparser, simba.xmlparser,
  simba.mmltimer, simba.finder, simba.target, simba.fontloader, simba.ocr,
  simba.ocrutil, simba.matrixhelpers, simba.nativeinterface, simba.geometry,
  simba.array_generics,

  simba.script_compiler_onterminate,
  simba.script_compiler_waituntil;

{$i simba.wrappers.inc}

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
    addGlobalFunc(Format('function %s.Get%s(%s): %s; constref;', [Obj, Item, Param, Typ]), ARead);

  if Arr then
    Param += '; ';

  if (AWrite <> nil) then
    addGlobalFunc(Format('procedure %s.Set%s(%sconst Value: %s); constref;', [Obj, Item, Param, Typ]), AWrite);
end;

function TSimbaScript_Compiler.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
begin
  Result := inherited HandleDirective(Sender, Directive, Argument);
end;

procedure TSimbaScript_Compiler.Import;
begin
  StartImporting();

  FSection := 'External';
  InitializeFFI(Self);

  FSection := 'System';

  InitializePascalScriptBasics(Self, [psiTypeAlias, psiSettings]);
  InitializeAddOnTerminate(Self);
  InitializeWaitUntil(Self);

  {$i simba.imports.inc}

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

