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
    FSectionStack: TStringArray;

    function Section: String;
    procedure pushSection(Name: String);
    procedure popSection;

    procedure InitBaseVariant; override;
  public
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    function addGlobalFunc(Header, Body: lpString): TLapeTree_Method; virtual; overload;
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
  dialogs, extctrls, graphtype, controls, comctrls, graphics, lpeval,
  stdctrls, buttons, customtimer, checklst, lclclasses, spin, pipes,
  lclintf, math, regexpr, strutils, lazfileutils, fileutil, clipbrd,
  blowfish, md5, sha1, hmac, forms, process, lazloggerbase,

  simba.mufasatypes, simba.script, simba.scriptthread, simba.outputform,
  simba.files, simba.process, simba.bitmap, simba.bitmap_helpers,
  simba.helpers_windowhandle, simba.matchtemplate, simba.tpa,
  simba.target_exported, simba.math, simba.colormath, simba.stringutil,
  simba.internet, simba.datetime, simba.dtmutil, simba.dtm, simba.iomanager,
  simba.aca, simba.dtmeditor, simba.script_communication,
  simba.imagebox, simba.client,simba.jsonparser, simba.xmlparser,
  simba.finder, simba.target, simba.fontloader, simba.ocr,
  simba.ocrutil, simba.matrixhelpers, simba.nativeinterface,
  simba.array_generics, simba.target_window, simba.mmltimer,

  simba.script_compiler_onterminate,
  simba.script_compiler_waituntil;

{$i simba.wrappers.inc}

function TSimbaScript_Compiler.addGlobalFunc(Header, Body: lpString): TLapeTree_Method;
var
  OldState: Pointer;
begin
  Result := nil;

  OldState := getTempTokenizerState(Header + Body, '!addGlobalFunc');
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
    addGlobalFunc(Format('function %s.Get%s(%s): %s;', [Obj, Item, Param, Typ]), ARead);

  if Arr then
    Param += '; ';

  if (AWrite <> nil) then
    addGlobalFunc(Format('procedure %s.Set%s(%sconst Value: %s);', [Obj, Item, Param, Typ]), AWrite);
end;

function TSimbaScript_Compiler.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;
begin
  Result := inherited HandleDirective(Sender, Directive, Argument);
end;

procedure TSimbaScript_Compiler.Import;
begin
  StartImporting();

  try
    Options := Options + LapePascalScriptCompilerOptions;

    addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).createCopy(), 'Byte');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).createCopy(), 'Integer');
    addGlobalType(getPointerType(ltChar, False).createCopy(), 'PChar');

    InitializeAddOnTerminate(Self);
    InitializeWaitUntil(Self);
    InitializeFFI(Self);

    {$i simba.imports.inc}
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

function TSimbaScript_Compiler.Section: String;
begin
  if Length(FSectionStack) > 0 then
    Result := FSectionStack[High(FSectionStack)]
  else
    Result := '';
end;

procedure TSimbaScript_Compiler.pushSection(Name: String);
begin
  FSectionStack := FSectionStack + [Name];
end;

procedure TSimbaScript_Compiler.popSection;
begin
  SetLength(FSectionStack, Length(FSectionStack) - 1);
end;

procedure TSimbaScript_Compiler.InitBaseVariant;

  procedure addVariantType(Value: TVarType; Name: String);
  var
    v: TLapeGlobalVar;
  begin
    v := addGlobalVar(getGlobalType('TVarType').NewGlobalVarP(nil, Name));
    v.isConstant := True;

    TVarType(v.Ptr^) := Value;
  end;

begin
  addGlobalType(getBaseType(DetermineIntType(SizeOf(TVarType), False)).createCopy(), 'TVarType');

  addVariantType(VarEmpty, 'VarEmpty');
  addVariantType(VarNull, 'VarNull');
  addVariantType(VarSmallInt, 'VarInt16');
  addVariantType(VarInteger, 'VarInt32');
  addVariantType(VarSingle, 'VarSingle');
  addVariantType(VarDouble, 'VarDouble');
  addVariantType(VarDate, 'VarDate');
  addVariantType(VarCurrency, 'VarCurrency');
  addVariantType(VarBoolean, 'VarBoolean');
  addVariantType(VarVariant, 'VarVariant');
  addVariantType(VarUnknown, 'VarUnknown');
  addVariantType(VarShortInt, 'VarInt8');
  addVariantType(VarByte, 'VarUInt8');
  addVariantType(VarWord, 'VarUInt16');
  addVariantType(VarLongWord, 'VarUInt32');
  addVariantType(VarInt64, 'VarInt64');
  addVariantType(VarString, 'VarString');
  addVariantType(VarUString, 'VarUnicodeString');
  addVariantType(VarUInt64, 'VarUInt64');

  addGlobalFunc('function VarType(const V: Variant): TVarType;', @_LapeVarType);

  addGlobalFunc('function VarIsOrdinal(const V: Variant): EvalBool;', @_LapeVarIsOrdinal);
  addGlobalFunc('function VarIsFloat(const V: Variant): EvalBool;', @_LapeVarIsFloat);
  addGlobalFunc('function VarIsNumeric(const V: Variant): EvalBool;', @_LapeVarIsNumeric);
  addGlobalFunc('function VarIsStr(const V: Variant): EvalBool;', @_LapeVarIsStr);
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

