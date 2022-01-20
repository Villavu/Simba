unit simba.script_dump;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils;

function DumpCompiler: TStringList;
function DumpPlugin(Plugin: String): TStringList;

implementation

uses
  lpparser, lptypes, lpvartypes, lptree,
  simba.script_compiler, simba.script_plugin;

type
  TCompilerDump = class(TSimbaScript_Compiler)
  protected
    FList: TStringList;

    procedure InitBaseDefinitions; override;
    procedure InitBaseMath; override;
    procedure InitBaseString; override;
    procedure InitBaseDateTime; override;
    procedure InitBaseVariant; override;
    procedure InitBaseFile; override;

    procedure WriteMethod(Header: String);
    procedure Write(Header: String);
  public
    procedure addBaseDefine(Define: lpString; AValue: lpString = ''); override;

    function addDelayedCode(Code: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;
    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; override;
    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; override;
    function addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar; override;

    procedure Dump(Strings: TStringList);

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

procedure TCompilerDump.Write(Header: String);
begin
  if (FList = nil) or (FSection = '') then
    Exit;

  FList.Values[FSection] := FList.Values[FSection] + Header + LineEnding;
end;

procedure TCompilerDump.InitBaseDefinitions;
var
  BaseType: ELapeBaseType;
begin
  FSection := 'System';

  for BaseType in ELapeBaseType do
    if (FBaseTypes[BaseType] <> nil) then
      Write(Format('type %s = %s;', [LapeTypeToString(BaseType), LapeTypeToString(BaseType)]));

  inherited InitBaseDefinitions();
end;

procedure TCompilerDump.InitBaseMath;
begin
  FSection := 'Math';

  inherited InitBaseMath();
end;

procedure TCompilerDump.InitBaseString;
begin
  FSection := 'String';

  inherited InitBaseString();
end;

procedure TCompilerDump.InitBaseDateTime;
begin
  FSection := 'Date & Time';

  inherited InitBaseDateTime();
end;

procedure TCompilerDump.InitBaseVariant;
begin
  FSection := 'Variant';

  inherited InitBaseVariant();
end;

procedure TCompilerDump.InitBaseFile;
begin
  FSection := 'File';

  inherited InitBaseFile();
end;

procedure TCompilerDump.WriteMethod(Header: String);
begin
  if (FList = nil) or (FSection = '') then
    Exit;

  Header := Trim(Header);
  if not Header.EndsWith(';') then
    Header := Header + ';';
  Header := Header + ' external;';

  FList.Values[FSection] := FList.Values[FSection] + Header + LineEnding;
end;

function TCompilerDump.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited addGlobalFunc(Header, Value);
  if (Result <> nil) and Result.VarType.Name.StartsWith('_') and (FSection = 'System') then
    Exit;

  WriteMethod(Header);
end;

function TCompilerDump.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited addGlobalType(Str, AName);

  if not Str.EndsWith(';') then
    Str := Str + ';';

  Write(Format('type %s = %s', [AName, Str]));
end;

function TCompilerDump.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  Result := inherited addGlobalType(Typ, AName, ACopy);

  if (not AName.StartsWith('!')) then
    Write(Format('type %s = %s;', [AName, Typ.Name]));
end;

function TCompilerDump.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(AVar, AName);
  if (Result <> nil) then
    Result._DocPos.FileName := FSection;
end;

function TCompilerDump.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);
  if (Result <> nil) then
    Result._DocPos.FileName := FSection;
end;

function TCompilerDump.addDelayedCode(Code: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(Code, AFileName, AfterCompilation, IsGlobal);
  if (not AFileName.StartsWith('!')) then
    Write(Code);
end;

procedure TCompilerDump.addBaseDefine(Define: lpString; AValue: lpString);
begin
  inherited addBaseDefine(Define, AValue);

  Write(Format('{$DEFINE %s}', [Define]));
end;

constructor TCompilerDump.Create;
begin
  FList := TStringList.Create();

  inherited Create(TLapeTokenizerString.Create('begin end.', ''));

  FSection := 'System';

  // Internal methods
  Write('{$DEFINE Lape}');
  Write('procedure Delete(A: array of Anything; Index: Int32; Count: Int32 = Length(A)); external;');
  Write('procedure Insert(Item: Anything; A: array of Anything; Index: Int32); external;');
  Write('procedure Copy(A: array of Anything; Index: Int32 = 0; Count: Int32 = Length(A)); overload; external;');
  Write('procedure Copy(S: String; Index: Int32 = 1; Count: Int32 = Length(S)); overload; external;');
  Write('procedure SetLength(A: Array of Anything; Length: Int32); overload; external;');
  Write('procedure SetLength(S: String; Length: Int32); overload; external;');
  Write('function Low(A: array of Anything): Int32; external;');
  Write('function Low(A: String): Int32; external;');
  Write('function High(A: array of Anything): Int32; external;');
  Write('function High(A: String): Int32; external;');
  Write('function Length(A: array of Anything): Int32; overload; external;');
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
  Write('procedure Sort(var A: array of Anything); overload; external;');
  Write('procedure Sort(var A: array of Anything; CompareFunc: function(constref L, R: Anything): Int32); overload; external;');
  Write('function Sorted(constref A: array of Anything): array of Anything; overload; external;');
  Write('function Sorted(constref A: array of Anything; CompareFunc: function(constref L, R: Anything): Int32): array of Anything; overload; external;');
end;

destructor TCompilerDump.Destroy;
begin
  FList.Free();

  inherited Destroy();
end;

procedure TCompilerDump.Dump(Strings: TStringList);
var
  Decl: TLapeDeclaration;
begin
  Import();

  for Decl in FGlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue) do
  begin
    with TLapeGlobalVar(Decl) do
    begin
      FSection := DocPos.FileName;
      if (FSection = '') or (FSection[1] = '!') then
        Continue;

      if (Name = '') or (Name = 'nil') or (VarType.Name = '') or
         (BaseType in [ltUnknown, ltScriptMethod, ltImportedMethod]) then
        Continue;

      if isConstant then
        Write(Format('const %s = %s;', [Name, VarType.Name]))
      else
        Write(Format('var %s: %s;', [Name, VarType.Name]));
    end;
  end;

  Strings.Clear();
  Strings.AddStrings(FList);
end;

function DumpCompiler: TStringList;
begin
  Result := TStringList.Create();

  with TCompilerDump.Create() do
  try
    Dump(Result);
  finally
    Free();
  end;
end;

function DumpPlugin(Plugin: String): TStringList;
begin
  Result := TStringList.Create();

  try
    with TSimbaScriptPlugin.Create(Plugin) do
      Result.AddStrings(Dump);
  except
  end;
end;

end.

