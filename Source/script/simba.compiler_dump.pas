{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Overrides import methods to gather declarations.
}
unit simba.compiler_dump;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

procedure DumpCompiler(FileName: String);

implementation

uses
  lpparser, lptypes, lpvartypes, lptree,
  simba.mufasatypes, simba.script_compiler, simba.list;

type
  TSimbaCompilerDump = class(TSimbaScript_Compiler)
  protected
    FItems: TSimbaStringPairList;

    procedure InitBaseVariant; override;
    procedure InitBaseMath; override;
    procedure InitBaseDefinitions; override;
    procedure InitBaseDateTime; override;
    procedure InitBaseFile; override;

    procedure Add(Section, Str: String);
    procedure AddMethod(Str: String);
    procedure AddType(Name, Str: String);
    procedure AddCode(Str: String; EnsureSemicolon: Boolean = True);

    procedure Move(Str, FromSection, ToSection: String);
  public
    constructor Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean=True; AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True); reintroduce; override;
    destructor Destroy; override;

    procedure addBaseDefine(Define: lpString; Value: lpString = ''); override;

    function addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;

    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method; override;

    function addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;

    function addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; override;

    procedure DumpToFile(FileName: String);
  end;

procedure DumpCompiler(FileName: String);
begin
  with TSimbaCompilerDump.Create(TLapeTokenizerString.Create('begin end')) do
  try
    DumpToFile(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaCompilerDump.InitBaseVariant;
begin
  { nothing, we import our own variant }
end;

procedure TSimbaCompilerDump.InitBaseMath;
begin
  ImportingSection := 'Math';
  inherited InitBaseMath();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.InitBaseDefinitions;
begin
  ImportingSection := 'System';
  inherited InitBaseDefinitions();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.InitBaseDateTime;
begin
  ImportingSection := 'DateTime';
  inherited InitBaseDateTime();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.InitBaseFile;
begin
  ImportingSection := 'File';
  inherited InitBaseFile();
  ImportingSection := '';
end;

procedure TSimbaCompilerDump.Add(Section, Str: String);
var
  Item: TSimbaStringPair;
begin
  Item.Name := Section;
  Item.Value := Str;

  FItems.Add(Item);
end;

procedure TSimbaCompilerDump.Move(Str, FromSection, ToSection: String);
var
  I: Integer;
  Item: TSimbaStringPair;
begin
  for I := 0 to FItems.Count - 1 do
    if (FItems[I].Name = FromSection) and FItems[I].Value.StartsWith(Str) then
    begin
      Item := FItems[I];
      Item.Name := ToSection;

      FItems[I] := Item;
    end;
end;

procedure TSimbaCompilerDump.AddMethod(Str: String);
begin
  Str := Str.Trim();
  if not Str.EndsWith(';') then
    Str := Str + ';';
  Str := Str + ' external;';

  Add(ImportingSection, Str);
end;

procedure TSimbaCompilerDump.AddType(Name, Str: String);
begin
  if Name.StartsWith('!') then
    Exit;

  Str := 'type ' + Name + ' = ' + Str;
  if not Str.EndsWith(';') then
    Str := Str + ';';

  Add(ImportingSection, Str);
end;

procedure TSimbaCompilerDump.AddCode(Str: String; EnsureSemicolon: Boolean);
begin
  if EnsureSemicolon and (not Str.EndsWith(';')) then
    Str := Str + ';';

  Add(ImportingSection, Str);
end;

constructor TSimbaCompilerDump.Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean; AEmitter: TLapeCodeEmitter; ManageEmitter: Boolean);
begin
  FItems := TSimbaStringPairList.Create();

  inherited Create(ATokenizer, ManageTokenizer, AEmitter, ManageEmitter);
end;

destructor TSimbaCompilerDump.Destroy;
begin
  if (FItems <> nil) then
    FreeAndNil(FItems);

  inherited Destroy();
end;

procedure TSimbaCompilerDump.addBaseDefine(Define: lpString; Value: lpString);
begin
  inherited addBaseDefine(Define, Value);

  AddCode('{$DEFINE ' + Define + '}', False);
end;

function TSimbaCompilerDump.addDelayedCode(ACode: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(ACode, AFileName, AfterCompilation, IsGlobal);

  if (not AFileName.StartsWith('!')) then
    AddCode(ACode);
end;

function TSimbaCompilerDump.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);

  AddMethod(Header);
end;

function TSimbaCompilerDump.addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method;
begin
  Result := inherited addGlobalFunc(Header, Body);

  AddMethod(Header);
end;

function TSimbaCompilerDump.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  Result := inherited addGlobalType(Typ, AName, ACopy);

  AddType(AName, Typ.Name);
end;

function TSimbaCompilerDump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  AddType(AName, Str);
end;

function TSimbaCompilerDump.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);
  Result._DocPos.FileName := ImportingSection;
end;

function TSimbaCompilerDump.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(AVar, AName);
  Result._DocPos.FileName := ImportingSection;
end;

procedure TSimbaCompilerDump.DumpToFile(FileName: String);
var
  BaseType: ELapeBaseType;
  Decl: TLapeDeclaration;
  I: Integer;
  Str: String;
begin
  Import();

  // Base types
  for BaseType in ELapeBaseType do
    if (FBaseTypes[BaseType] <> nil) then
      Add('System', 'type %s = %s;'.Format([LapeTypeToString(BaseType), LapeTypeToString(BaseType)]));

  // Variables & Constants
  for Decl in FGlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue) do
    with TLapeGlobalVar(Decl) do
    begin
      if (Name = '') or (VarType = nil) or (VarType.Name = '') or (BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
        Continue;
      if DocPos.FileName.StartsWith('!') then
        Continue;

      if isConstant then
      begin
        Str := 'const ' + UpperCase(Name) + ': ' + VarType.Name;

        if (VarType.BaseType in LapeCharTypes) then
          Str := Str + ' = "' + AsString + '";'
        else
          Str := Str + ' = ' + AsString + ';';
      end else
        Str := 'var ' + Name + ': ' + VarType.Name + ';';

      Add(DocPos.FileName, Str);
    end;

  // add internals
  Add('System', 'procedure Delete(A: array; Index: Int32; Count: Int32 = Length(A)); external;');
  Add('System', 'procedure Insert(Item: Anything; A: array; Index: Int32); external;');
  Add('System', 'procedure Copy(A: array; Index: Int32 = 0; Count: Int32 = Length(A)); overload; external;');
  Add('System', 'procedure SetLength(A: array; Length: Int32); overload; external;');
  Add('System', 'function Low(A: array): Int32; external;');
  Add('System', 'function High(A: array): Int32; external;');
  Add('System', 'function Length(A: array): Int32; overload; external;');
  Add('System', 'procedure WriteLn(Args: Anything); external;');
  Add('System', 'procedure Write(Args: Anything); external;');
  Add('System', 'procedure Swap(var A, B: Anything); external;');
  Add('System', 'function SizeOf(A: Anything): Int32; external;');
  Add('System', 'function ToString(A: Anything): String; external;');
  Add('System', 'function ToStr(A: Anything): String; external;');
  Add('System', 'function Inc(var X: Ordinal; Amount: SizeInt = 1): Ordinal; overload; external;');
  Add('System', 'function Dec(var X: Ordinal; Amount: SizeInt = 1): Ordinal; overload; external;');
  Add('System', 'function Ord(X: Ordinal): Int32; external;');
  Add('System', 'function WaitUntil(Condition: Expression; Interval, Timeout: Int32): Boolean; external;');
  Add('System', 'function Default(T: AnyType): AnyType; external;');
  Add('System', 'procedure Sort(var A: array); overload; external;');
  Add('System', 'procedure Sort(var A: array; Weights: array of Ordinal; LowToHigh: Boolean); overload; external;');
  Add('System', 'procedure Sort(var A: array; CompareFunc: function(constref L, R: Anything): Int32); overload; external;');
  Add('System', 'function Sorted(const A: array): array; overload; external;');
  Add('System', 'function Sorted(const A: array; CompareFunc: function(constref L, R: Anything): Int32): array; overload; external;');
  Add('System', 'function Sorted(const A: array; Weights: array of Ordinal; LowToHigh: Boolean): array; overload; external;');
  Add('System', 'function Unique(const A: array): array; external;');
  Add('System', 'procedure Reverse(var A: array); external;');
  Add('System', 'function Reversed(const A: array): array; external;');
  Add('System', 'function IndexOf(const Item: T; const A: array): Integer; external;');
  Add('System', 'function IndicesOf(const Item: T; const A: array): TIntegerArray; external;');
  Add('System', 'function Contains(const Item: T; const A: array): Boolean; external;');

  Add('System', 'function RTTIFields(constref AnyRecord): TRTTIFields; external;');
  Add('System', 'function RTTIClassFields(constref AnyRecord): TRTTIFields; external;');

  Add('System', 'function GetCallerAddress: Pointer; external;');
  Add('System', 'function GetCallerName: String; external;');
  Add('System', 'function GetCallerLocation: Pointer; external;');
  Add('System', 'function GetCallerLocationStr: String; external;');

  Add('System', 'function GetExceptionLocation: Pointer; external;');
  Add('System', 'function GetExceptionLocationStr: String; external;');
  Add('System', 'function GetExceptionMessage: String; external;');

  Add('System', 'function GetScriptMethodName(Address: Pointer): String; external;');
  Add('System', 'function DumpCallStack(Start: Integer = 0): String; external;');

  // Move lape stuff to better sections
  Move('function Random(min, max: Int64): Int64;', 'Math', 'Random');
  Move('function Random(min, max: Extended): Extended', 'Math', 'Random');
  Move('function Random(l: Int64): Int64', 'Math', 'Random');
  Move('function Random: Extended', 'Math', 'Random');
  Move('procedure Randomize', 'Math', 'Random');
  Move('var RandSeed', 'Math', 'Random');

  Move('function GetTickCount: UInt64', 'DateTime', 'Timing');
  Move('procedure Sleep(MilliSeconds: UInt32);', 'DateTime', 'Timing');

  with TStringList.Create() do
  try
    LineBreak := #0;
    for I := 0 to FItems.Count - 1 do
      Values[FItems[I].Name] := Values[FItems[I].Name] + FItems[I].Value + LineEnding;

    SaveToFile(FileName);
  finally
    Free();
  end;
end;

end.

