{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler;

{$i simba.inc}

interface

uses
  Classes, SysUtils, TypInfo,
  ffi, lpffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers, lpinterpreter,
  simba.mufasatypes;

type
  TOverridingMethod = function(Name, Params: lpString; isFunction: Boolean): String is nested;

  TSimbaScript_Compiler = class;
  TSimbaImport = procedure(Compiler: TSimbaScript_Compiler);
  TSimbaImportArray = array of TSimbaImport;

  TManagedImportClosure = class(TLapeDeclaration)
    Closure: TImportClosure;
  end;

  TSimbaScript_Compiler = class(TLapeCompiler)
  protected
    FImportingSection: String;

    function GetImportingSection: String;

    procedure InitBaseFile; override;
    procedure InitBaseVariant; override;
    procedure InitBaseDefinitions; override;
  public
    function CurrentDir: String;

    procedure addOverrideMethod(Header: lpString; GetBody: TOverridingMethod);

    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    procedure addDelayedCode(Code: TStringArray; AFileName: lpString = ''); virtual; overload;

    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; override;

    function addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method; virtual; overload;
    function addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar; virtual; overload;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; virtual; overload;
    function addGlobalType(Str: TStringArray; Name: String): TLapeType; virtual; overload;

    procedure addClass(Name: lpString; Parent: lpString = 'TObject'); virtual;
    procedure addClassVar(Obj, Item, Typ: lpString; ARead: Pointer; AWrite: Pointer = nil; Arr: Boolean = False; ArrType: lpString = 'Integer'); virtual;

    procedure Import; virtual;
    function Compile: Boolean; override;

    procedure CallProc(ProcName: String; UseFFI: Boolean);
    property ImportingSection: String read GetImportingSection write FImportingSection;
  end;

implementation

uses
  lpeval,
  simba.script_imports, simba.paslex,
  simba.script_compiler_waituntil, simba.script_compiler_rtti;

procedure TSimbaScript_Compiler.addOverrideMethod(Header: lpString; GetBody: TOverridingMethod);
var
  isFunction: Boolean;
  FuncName, Params, Body: lpString;
begin
  with TPasLexer.Create() do
  try
    Origin := PChar(Header);

    Params := '';
    FuncName := '';
    isFunction := TokenID = tkFunction;
    if isNext(tkDot) then
    begin
      NextNoJunk();
      FuncName := Token;
    end;

    if isNext(tkRoundOpen) then
    begin
      while (TokenID <> tkNull) do
      begin
        case TokenID of
          tkIdentifier:
            begin
              if (Params <> '') then
                Params := Params + ', ';
              Params := Params + Token;
            end;
          tkColon:
            NextNoJunk();
          tkRoundClose:
            Break;
        end;
        NextNoJunk();
      end;
    end;

    Header := Header.Replace('overload', '').Trim([' ', ';']);
    Header := Header + '; override;';

    Body := GetBody(FuncName, Params, isFunction);

    addGlobalFunc(Header, [Body]);
  finally
    Free();
  end;
end;

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
    Options := Options + [lcoLooseSemicolon, lcoAutoInvoke, lcoExplicitSelf, lcoAutoObjectify, lcoRelativeFileNames] - [lcoInheritableRecords];

    ImportingSection := 'System';

    InitializeWaitUntil(Self);
    InitializeFFI(Self);
    InitializeRTTI(Self);

    ImportingSection := '';

    AddSimbaImports(Self);
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

procedure TSimbaScript_Compiler.CallProc(ProcName: String; UseFFI: Boolean);
var
  Method: TLapeGlobalVar;
begin
  Method := Globals[ProcName];
  if (Method = nil) or (Method.BaseType <> ltScriptMethod) or
     (TLapeType_Method(Method.VarType).Res <> nil) or (TLapeType_Method(Method.VarType).Params.Count <> 0) then
    SimbaException('CallProc: Invalid procedure "%s"', [ProcName]);

  if UseFFI then
  begin
    with LapeExportWrapper(Method) do
    try
      TProcedure(Func)();
    finally
      Free();
    end;
  end else
    RunCode(FEmitter, [], PCodePos(Method.Ptr)^);
end;

function TSimbaScript_Compiler.GetImportingSection: String;
begin
  if (FImportingSection = '') then
    FImportingSection := '!Simba';

  Result := FImportingSection;
end;

procedure TSimbaScript_Compiler.InitBaseFile;
begin
  { nothing, we import our own file }
end;

procedure TSimbaScript_Compiler.InitBaseVariant;
begin
  { nothing, we import our own variant }
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

