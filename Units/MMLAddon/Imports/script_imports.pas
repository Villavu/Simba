unit script_imports;

{
  TScriptImports: Map that stores the import section and a callback to import into a compiler instance, filled on initalization.
  TScriptImports_Dump: Compiler instance that writes all imports to a TStringList.
  TScriptImports_Helper: Class helper which adds more importing methods.
}

{$mode objfpc}{$H+}

interface                         

uses
  Classes, SysUtils, fgl, typinfo,
  ffi, lpcompiler, lptypes, lpvartypes, lpparser, lputils, lptree;

type
  TScriptImport = procedure(Compiler: TLapeCompiler; Data: Pointer);
  TScriptImports = class(specialize TFPGMap<String, TScriptImport>)
  public
    procedure Import(Name: String; Compiler: TLapeCompiler; AData: Pointer = nil);
  end;

  TScriptImports_Dump = class(TLapeCompiler)
  protected
    procedure Write(Header: String; SemiColon: Boolean = True; Method: Boolean = False);
  public
    Dump: TStringList;
    Section: String;

    procedure addBaseDefine(Define: lpString); override;

    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; overload; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; override;
    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; override;
    function addDelayedCode(Code: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TScriptImports_Helper = class helper for TLapeCompiler
  public
    procedure addClass(const Name: String; const Parent: String = 'TObject');
    procedure addClassVar(const Obj, Item, Typ: String; const Read: Pointer; const Write: Pointer = nil; const Arr: Boolean = False; const ArrType: string = 'UInt32');
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; overload;
  end;

var
  ScriptImports: TScriptImports;

implementation

procedure TScriptImports.Import(Name: String; Compiler: TLapeCompiler; AData: Pointer);
begin
  if (IndexOf(Name) < 0) then
    raise Exception.Create('Import "' + Name + '" not found');

  Data[IndexOf(Name)](Compiler, AData);
end;

procedure TScriptImports_Dump.Write(Header: String; SemiColon: Boolean; Method: Boolean);
begin
  Header := Trim(Header);

  if (Dump <> nil) and (Section <> '') and (Header <> '') then
  begin
    if SemiColon and (Header[Length(Header)] <> ';') then
      Header := Header + ';';
    if Method then
      Header := Header + 'begin end;';

    Dump.Values[Section] := Dump.Values[Section] + Header + LineEnding;
  end;
end;

procedure TScriptImports_Dump.addBaseDefine(Define: lpString);
begin
  inherited addBaseDefine(Define);

  Write(Format('{$DEFINE %s}', [Define]), False, False);
end;

function TScriptImports_Dump.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);

  Write(Header, True, True);
end;

function TScriptImports_Dump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  Write(Format('type %s = %s', [AName, Str]));
end;

function TScriptImports_Dump.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);

  Write(Format('var %s: %s', [AName, Typ]));
end;

function TScriptImports_Dump.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: Int32 = %d', [AName, Value]));
end;

function TScriptImports_Dump.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: UInt32 = %d', [AName, Value]));
end;

function TScriptImports_Dump.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: Int64 = %d', [AName, Value]));
end;

function TScriptImports_Dump.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: UInt64 = %d', [AName, Value]));
end;

function TScriptImports_Dump.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: Extended = %s', [AName, FloatToStr(Value)]));
end;

function TScriptImports_Dump.addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: EvalBool = %s', [AName, BoolToStr(Value, True)]));
end;

function TScriptImports_Dump.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: String = %s', [AName, Value]));
end;

function TScriptImports_Dump.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: UnicodeString = %s', [AName, Value]));
end;

function TScriptImports_Dump.addDelayedCode(Code: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean ): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(Code, AFileName, AfterCompilation, IsGlobal);

  if (not AFileName.StartsWith('!')) then
    Write(Code);
end;

constructor TScriptImports_Dump.Create;
var
  i: Int32;
begin
  inherited Create(TLapeTokenizerString.Create('begin end.'));

  Dump := TStringList.Create();
  Dump.Values['Math'] := {$i lape_import_math.inc}
  Dump.Values['Time & Date'] := {$i lape_import_timing.inc}
  Dump.Values['String'] := {$i lape_import_string.inc}
  Dump.Values['Variant'] := {$i lape_import_variant.inc}
  Dump.Values['System'] := {$i lape_import_system.inc}

  StartImporting();

  try
    for i := 0 to ScriptImports.Count - 1 do
    begin
      Section := ScriptImports.Keys[i];

      ScriptImports.Import(Section, Self);
    end;
  finally
    EndImporting();
  end;

  Dump.Sort();
end;

destructor TScriptImports_Dump.Destroy;
begin
  Dump.Free();

  inherited Destroy();
end;

function TScriptImports_Helper.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  with addGlobalType(Str, '_' + AName) do
  begin
    Result := addGlobalType('native (_' + AName + ', ' + GetEnumName(TypeInfo(TFFIABI), Ord(ABI)) + ')', AName);

    Name := '!' + AName;
  end;
end;

procedure TScriptImports_Helper.addClass(const Name: String; const Parent: String);
begin
  addGlobalType(Format('type %s', [Parent]), Name);
end;

procedure TScriptImports_Helper.addClassVar(const Obj, Item, Typ: String; const Read: Pointer; const Write: Pointer; const Arr: Boolean; const ArrType: string);
var
  Param: String = '';
begin
  if Arr then
    Param := 'const Index: ' + ArrType;

  if (Read <> nil) then
    addGlobalFunc(Format('function %s.get%s(%s): %s; constref;', [Obj, Item, Param, Typ]), Read);

  if Arr then
    Param += '; ';

  if (Write <> nil) then
    addGlobalFunc(Format('procedure %s.set%s(%sconst Value: %s); constref;', [Obj, Item, Param, Typ]), Write);
end;

initialization
  ScriptImports := TScriptImports.Create();

finalization
  ScriptImports.Free();

end.

