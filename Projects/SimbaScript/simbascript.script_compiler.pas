unit simbascript.script_compiler;

{$mode objfpc}{$H+}

interface                         

uses
  classes, sysutils, typinfo,
  ffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree;

type
  TScriptCompiler = class;
  TScriptImport = procedure(Compiler: TScriptCompiler);

  TScriptCompiler = class(TLapeCompiler)
  public
  class var
    Imports: array of TScriptImport;
  protected
    FDump: TStringList;
    FSection: String;

    procedure Write(Header: String; SemiColon: Boolean = True; Method: Boolean = False);
  public
    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; overload; override;

    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; override;

    function addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload;

    function addDelayedCode(Code: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; overload;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; override;

    procedure addBaseDefine(Define: lpString); override;
    procedure addBaseDefine(Define: lpString; Value: lpString); overload;

    procedure addClass(const Name: String; const Parent: String = 'TObject');
    procedure addClassVar(const Obj, Item, Typ: String; const ARead: Pointer; const AWrite: Pointer = nil; const Arr: Boolean = False; const ArrType: string = 'UInt32');

    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); override;

    property Section: String read FSection write FSection;

    function Dump: String;
    function Import(Data: Pointer = nil): Boolean;

    constructor Create(Script: String = 'begin end.'; FileName: String = ''); reintroduce;
    destructor Destroy; override;
  end;

procedure RegisterScriptImport(Callback: TScriptImport);

implementation

uses
  lpffi, lputils;

procedure RegisterScriptImport(Callback: TScriptImport);
begin
  with TScriptCompiler do
  begin
    SetLength(Imports, Length(Imports) + 1);
    Imports[High(Imports)] := Callback;
  end;
end;

procedure TScriptCompiler.Write(Header: String; SemiColon: Boolean; Method: Boolean);
begin
  Header := Trim(Header);

  if (FDump <> nil) and (FSection <> '') and (Header <> '') then
  begin
    if SemiColon and (Header[Length(Header)] <> ';') then
      Header := Header + ';';

    if Method then
    begin
      Header := Header + LineEnding + 'begin';
      Header := Header + LineEnding + '  // Internal method in Simba';
      Header := Header + LineEnding + 'end;';
    end;

    FDump.Values[FSection] := FDump.Values[FSection] + Header + LineEnding;
  end;
end;

procedure TScriptCompiler.addBaseDefine(Define: lpString);
begin
  inherited addBaseDefine(Define);

  if (FDump <> nil) then
    Write(Format('{$DEFINE %s}', [Define]), False, False);
end;

procedure TScriptCompiler.addBaseDefine(Define: lpString; Value: lpString);
begin
  FBaseDefines[Define] := Trim(Value);

  if (FDump <> nil) then
    Write(Format('{$DEFINE %s}', [Define]), False, False);
end;

function TScriptCompiler.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);

  if (FDump <> nil) then
    Write(Header, True, True);
end;

function TScriptCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  if (FDump <> nil) then
    Write(Format('type %s = %s', [AName, Str]));
end;

function TScriptCompiler.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: %s', [AName, Typ]));
end;

function TScriptCompiler.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: Int32 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: UInt32 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: Int64 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: UInt64 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: Extended = %s', [AName, FloatToStr(Value)]));
end;

function TScriptCompiler.addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: EvalBool = %s', [AName, BoolToStr(Value, True)]));
end;

function TScriptCompiler.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: String = "%s"', [AName, Value]));
end;

function TScriptCompiler.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  if (FDump <> nil) then
    Write(Format('var %s: UnicodeString = "%s"', [AName, Value]));
end;

function TScriptCompiler.addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: %s', [AName, Typ]));
end;

function TScriptCompiler.addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: Int32 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: UInt32 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

 if (FDump <> nil) then
   Write(Format('const %s: Int64 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: UInt64 = %d', [AName, Value]));
end;

function TScriptCompiler.addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: Extended = %s', [AName, FloatToStr(Value)]));
end;

function TScriptCompiler.addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: EvalBool = %s', [AName, BoolToStr(Value, True)]));
end;

function TScriptCompiler.addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: String = "%s"', [AName, Value]));
end;

function TScriptCompiler.addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDump <> nil) then
    Write(Format('const %s: UnicodeString = "%s"', [AName, Value]));
end;

function TScriptCompiler.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  with addGlobalType(Str, '_' + AName) do
  begin
    Result := addGlobalType('native (_' + AName + ', ' + GetEnumName(TypeInfo(TFFIABI), Ord(ABI)) + ')', AName);

    Name := '!' + AName;
  end;
end;

procedure TScriptCompiler.addClass(const Name: String; const Parent: String);
begin
  addGlobalType(Format('type %s', [Parent]), Name);
end;

procedure TScriptCompiler.addClassVar(const Obj, Item, Typ: String; const ARead: Pointer; const AWrite: Pointer; const Arr: Boolean; const ArrType: string);
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

procedure TScriptCompiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

function TScriptCompiler.addDelayedCode(Code: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean ): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(Code, AFileName, AfterCompilation, IsGlobal);

  if (FDump <> nil) and (not AFileName.StartsWith('!')) then
    Write(Code);
end;

function TScriptCompiler.Dump: String;
begin
  Result := '';

  if (FDump = nil) then
    FDump := TStringList.Create()
  else
    FDump.Clear();

  FDump.LineBreak := #0;
  FDump.Values['Types'] := {$i lape_type_imports.inc}
  FDump.Values['Math'] := {$i lape_math_imports.inc}
  FDump.Values['Time & Date'] := {$i lape_date_time_imports.inc}
  FDump.Values['String'] := {$i lape_string_imports.inc}
  FDump.Values['Variant'] := {$i lape_variant_imports.inc}
  FDump.Values['System'] := {$i lape_system_imports.inc}
  FDump.Values['File'] := {$i lape_file_imports.inc}

  if Self.Import() then
  begin
    FDump.Sort();

    Result := FDump.Text;
  end;
end;

function TScriptCompiler.Import(Data: Pointer): Boolean;
var
  i: Int32;
begin
  StartImporting();

  Section := 'System';

  if FFILoaded then
    InitializeFFI(Self);

  InitializePascalScriptBasics(Self, [psiTypeAlias, psiSettings, psiMagicMethod, psiFunctionWrappers, psiExceptions]);

  ExposeGlobals(Self, [egmInvoke]);

  try
    for i := 0 to High(Imports) do
      Imports[i](Self);

    Result := True;
  finally
    EndImporting();
  end;
end;

constructor TScriptCompiler.Create(Script: String; FileName: String);
begin
  inherited Create(TLapeTokenizerString.Create(Script, FileName));

  Globals['Move'].Name := 'MemMove';
end;

destructor TScriptCompiler.Destroy;
begin
  if (FDump <> nil) then
    FDump.Free();

  inherited Destroy();
end;

end.

