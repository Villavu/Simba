{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_dump;

{$i simba.inc}

interface

uses
  classes, sysutils;

function DumpCompiler: TStringList;
function DumpPlugin(Plugin: String): TStringList;

implementation

uses
  lazloggerbase,
  lpparser, lptypes, lpvartypes, lptree, Contnrs,
  simba.script_compiler, simba.script_plugin;

type
  TCompilerDump = class(TSimbaScript_Compiler)
  protected
    FHashList: TFPHashObjectList;

    procedure InitBaseDefinitions; override;
    procedure InitBaseMath; override;
    procedure InitBaseString; override;
    procedure InitBaseDateTime; override;
    procedure InitBaseVariant; override;
    procedure InitBaseFile; override;

    procedure WriteMethod(Header, Name: String);
    procedure WriteType(Str, Name: String);
    procedure Write(Header: String);
  public
    procedure addBaseDefine(Define: lpString; Value: lpString = ''); override;

    function addDelayedCode(Code: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;
    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; override;
    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; override;

    function Dump: TStringList;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

procedure TCompilerDump.Write(Header: String);
var
  List: TObject;
begin
  if (FHashList = nil) or (Section = '') then
    Exit;

  List := FHashList.Find(Section);
  if (List = nil) then
    List := FHashList[FHashList.Add(FSectionStack[High(FSectionStack)], TStringList.Create())];

  TStringList(List).Add(Header);
end;

procedure TCompilerDump.WriteMethod(Header, Name: String);
begin
  if (Section = 'System') and Name.StartsWith('_') then
    Exit;

  Header := Trim(Header);
  if not Header.EndsWith(';') then
    Header := Header + ';';
  Header := Header + ' external;';

  Write(Header);
end;

procedure TCompilerDump.WriteType(Str, Name: String);
var
  IsSystemSection: Boolean;
begin
  IsSystemSection := (Section = 'System');
  if IsSystemSection then
    pushSection('Types');

  if not Name.StartsWith('!') then
  begin
    Str := 'type ' + Name + ' = ' + Str;
    if not Str.EndsWith(';') then
      Str := Str + ';';

    Write(Str);
  end;

  if IsSystemSection then
    popSection();
end;

procedure TCompilerDump.InitBaseDefinitions;
var
  BaseType: ELapeBaseType;
begin
  for BaseType in ELapeBaseType do
    if (FBaseTypes[BaseType] <> nil) then
      WriteType(LapeTypeToString(BaseType), LapeTypeToString(BaseType));

  inherited InitBaseDefinitions();
end;

procedure TCompilerDump.InitBaseMath;
begin
  pushSection('Math');

  inherited InitBaseMath();

  popSection();
end;

procedure TCompilerDump.InitBaseString;
begin
  pushSection('String');

  inherited InitBaseString();

  popSection();
end;

procedure TCompilerDump.InitBaseDateTime;
begin
  pushSection('Date & Time');

  inherited InitBaseDateTime();

  popSection();
end;

procedure TCompilerDump.InitBaseVariant;
begin
  pushSection('Variant');

  inherited InitBaseVariant();

  popSection();
end;

procedure TCompilerDump.InitBaseFile;
begin
  pushSection('File');

  inherited InitBaseFile();

  popSection();
end;

function TCompilerDump.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);

  WriteMethod(Header, Result.VarType.Name);
end;

function TCompilerDump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  WriteType(Str, AName);
end;

function TCompilerDump.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  Result := inherited addGlobalType(Typ, AName, ACopy);

  WriteType(Result.Name, Result.Name);
end;

function TCompilerDump.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(AVar, AName);
  Result._DocPos.FileName := Section;
end;

function TCompilerDump.addDelayedCode(Code: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(Code, AFileName, AfterCompilation, IsGlobal);
  if (not AFileName.StartsWith('!')) then
    Write(Code);
end;

procedure TCompilerDump.addBaseDefine(Define: lpString; Value: lpString);
begin
  inherited addBaseDefine(Define, Value);

  Write('{$DEFINE ' + Define + '}');
end;

constructor TCompilerDump.Create;
begin
  FHashList := TFPHashObjectList.Create();

  pushSection('System');

  inherited Create(TLapeTokenizerString.Create('begin end.', ''));

  // Internal methods
  Write('procedure Delete(A: array; Index: Int32; Count: Int32 = Length(A)); external;');
  Write('procedure Insert(Item: Anything; A: array; Index: Int32); external;');
  Write('procedure Copy(A: array; Index: Int32 = 0; Count: Int32 = Length(A)); overload; external;');
  Write('procedure Copy(S: String; Index: Int32 = 1; Count: Int32 = Length(S)); overload; external;');
  Write('procedure SetLength(A: array; Length: Int32); overload; external;');
  Write('procedure SetLength(S: String; Length: Int32); overload; external;');
  Write('function Low(A: array): Int32; external;');
  Write('function Low(A: String): Int32; external;');
  Write('function High(A: array): Int32; external;');
  Write('function High(A: String): Int32; external;');
  Write('function Length(A: array): Int32; overload; external;');
  Write('function Length(S: String): Int32; overload; external;');
  Write('procedure WriteLn(Args: Anything); external;');
  Write('procedure Write(Args: Anything); external;');
  Write('procedure Swap(var A, B: Anything); external;');
  Write('function SizeOf(A: Anything): Int32; external;');
  Write('function ToString(A: Anything): String; external;');
  Write('function ToStr(A: Anything): String; external;');
  Write('function GetExceptionMessage: ShortString; external;');
  Write('function Inc(var X: Ordinal): Ordinal; overload; external;');
  Write('function Dec(var X: Ordinal): Ordinal; overload; external;');
  Write('function Inc(var X: Ordinal; Amount: SizeInt): Ordinal; overload; external;');
  Write('function Dec(var X: Ordinal; Amount: SizeInt): Ordinal; overload; external;');
  Write('function Ord(X: Ordinal): Int32; external;');
  Write('function WaitUntil(Condition: Expression; Interval, Timeout: Int32): Boolean; external;');
  Write('function Default(T: AnyType): AnyType; external;');
  Write('procedure Sort(var A: array); overload; external;');
  Write('procedure Sort(var A: array; CompareFunc: function(constref L, R: Anything): Int32); overload; external;');
  Write('procedure Sort(var A: array; Weights: TIntegerArray; LowToHigh: Boolean); overload; external;');
  Write('procedure Sort(var A: array; Weights: TExtendedArray; LowToHigh: Boolean); overload; external;');
  Write('function Sorted(const A: array): array; overload; external;');
  Write('function Sorted(const A: array; CompareFunc: function(constref L, R: Anything): Int32): array; overload; external;');
  Write('function Sorted(const A: array; Weights: TIntegerArray; LowToHigh: Boolean): array; overload; external;');
  Write('function Sorted(const A: array; Weights: TExtendedArray; LowToHigh: Boolean): array; overload; external;');
  Write('function Unique(const A: array): array; external;');
  Write('procedure Reverse(var A: array); external;');
  Write('function Reversed(const A: array): array; external;');
  Write('function IndexOf(const Item: T; const A: array): Integer; external;');
  Write('function IndicesOf(const Item: T; const A: array): TIntegerArray; external;');
  Write('function Contains(const Item: T; const A: array): Boolean; external;');
end;

destructor TCompilerDump.Destroy;
begin
  if (FHashList <> nil) then
    FreeAndNil(FHashList);

  inherited Destroy();
end;

function TCompilerDump.Dump: TStringList;
var
  Decl: TLapeDeclaration;
  I: Integer;
begin
  Import();

  for Decl in FGlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue) do
  begin
    with TLapeGlobalVar(Decl) do
    begin
      if (DocPos.FileName = '') or (DocPos.FileName[1] = '!') then
        Continue;
      if (Name = '') or (Name = 'nil') or (VarType.Name = '') or (BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
        Continue;

      pushSection(DocPos.FileName);

      if isConstant then
        Write('const ' + Name + ' = ' + VarType.Name + ';')
      else
        Write('var ' + Name + ': ' + VarType.Name + ';');

      popSection();
    end;
  end;

  Result := TStringList.Create();
  Result.LineBreak := #0;
  for I := 0 to FHashList.Count - 1 do
    Result.Values[FHashList.NameOfIndex(I)] := TStringList(FHashList[I]).Text;
end;

function DumpCompiler: TStringList;
begin
  Result := nil;

  try
    with TCompilerDump.Create() do
      Result := Dump();
  except
    on E: Exception do
      DebugLn('DumpCompiler: ', E.Message);
  end;
end;

function DumpPlugin(Plugin: String): TStringList;
begin
  Result := nil;

  try
    with TSimbaScriptPlugin.Create(Plugin) do
      Result := Dump();
  except
    on E: Exception do
      DebugLn('DumpPlugin: ', E.Message);
  end;
end;

end.

