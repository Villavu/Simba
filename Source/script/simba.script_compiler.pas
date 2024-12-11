{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Subclasses Lape compiler for Simba script needs.
}
unit simba.script_compiler;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lpcompiler, lptypes, lpvartypes, lptree, lpffiwrappers, ffi,
  simba.base, simba.containers, simba.vartype_string;

type
  TScriptCompiler = class(TLapeCompiler)
  protected type
    TManagedImportClosure = class(TLapeDeclaration)
      Closure: TImportClosure;
    end;
  protected
    FDumpSection: String;
    FDump: TSimbaStringPairList;

    procedure DumpCode(const Section, Code: String);
    procedure DumpMethod(Str: String);
    procedure DumpType(Name, Str: String);
    procedure DumpVar(Name, Typ: String);

    procedure InitBaseFile; override;
    procedure InitBaseVariant; override;
    procedure InitBaseDefinitions; override;
    procedure InitBaseMath; override;
    procedure InitBaseString; override;
    procedure InitBaseDateTime; override;

    function GetDumpSection: String;
  public
    constructor Create(Doc: String; FileName: String = ''); reintroduce;
    constructor CreateDump;
    destructor Destroy; override;

    // Overrides for dumping
    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Typ: ELapeBaseType; Value: Pointer; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Single; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Double; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar; override;
    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; override;

    // Compiler addons
    procedure pushCode(Code: String);
    procedure addDelayedCode(Code: TStringArray; AFileName: lpString = ''); overload;
    function addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method;overload;
    function addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar; overload;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; overload;
    function addGlobalType(Str: TStringArray; Name: String): TLapeType;  overload;
    function addClassConstructor(Obj, Params: lpString; Func: Pointer; IsOverload: Boolean = False): TLapeGlobalVar;
    procedure addClass(Name: lpString; Parent: lpString = 'TObject');
    procedure addProperty(Obj, Name, Typ: String; ReadFunc: Pointer; WriteFunc: Pointer = nil);
    procedure addPropertyIndexed(Obj, Name, Params, Typ: String; ReadFunc: Pointer; WriteFunc: Pointer = nil);
    procedure addMagic(Name: String; Params: array of lpString; ParamTypes: array of ELapeParameterType; Res: String; Func: Pointer);
    function Compile: Boolean; override;
    procedure CallProc(ProcName: String);

    property DumpSection: String read GetDumpSection write FDumpSection;
    property Dump: TSimbaStringPairList read FDump;
  end;

implementation

uses
  lpeval, lpparser, lpinterpreter;

procedure TScriptCompiler.DumpCode(const Section, Code: String);
var
  Item: TSimbaStringPair;
begin
  Item.Name := Section;
  Item.Value := Code;

  FDump.Add(Item);
end;

procedure TScriptCompiler.DumpMethod(Str: String);
begin
  Str := Str.Trim();
  if not Str.EndsWith(';') then
    Str := Str + ';';
  Str := Str + ' external;';

  DumpCode(DumpSection, Str);
end;

procedure TScriptCompiler.DumpType(Name, Str: String);
begin
  if Name.StartsWith('!') then
    Exit;
  Str := 'type ' + Name + ' = ' + Str;
  if not Str.EndsWith(';') then
    Str := Str + ';';

  DumpCode(DumpSection, Str);
end;

procedure TScriptCompiler.DumpVar(Name, Typ: String);
var
  Str: String;
begin
  if Name.StartsWith('!') then
    Exit;
  Str := 'var ' + Name + ': ' + Typ;
  if not Str.EndsWith(';') then
    Str := Str + ';';

  DumpCode(DumpSection, Str);
end;

function TScriptCompiler.GetDumpSection: String;
begin
  if (FDumpSection = '') then
    FDumpSection := '!Simba';

  Result := FDumpSection;
end;

procedure TScriptCompiler.InitBaseFile;
begin
  { nothing, we import our own file later }
end;

procedure TScriptCompiler.InitBaseVariant;
begin
  { nothing, we import our own variant later }
end;

procedure TScriptCompiler.InitBaseDefinitions;
begin
  DumpSection := 'Base';

  inherited InitBaseDefinitions();

  DumpSection := '';
end;

// lpeval_import_math.inc but moved Random functions under Random section
procedure TScriptCompiler.InitBaseMath;
begin
  DumpSection := 'Math';

  addGlobalVar(Pi, 'Pi').isConstant := True;

  addGlobalFunc('function Min(x,y: Int64): Int64; overload;', @_LapeMin);
  addGlobalFunc('function Min(x,y: Double): Double; overload;', @_LapeMinF);
  addGlobalFunc('function Max(x,y: Int64): Int64; overload;', @_LapeMax);
  addGlobalFunc('function Max(x,y: Double): Double; overload;', @_LapeMaxF);
  addGlobalFunc('function EnsureRange(Value, Min, Max: Int64): Int64; overload;', @_LapeEnsureRange);
  addGlobalFunc('function EnsureRange(Value, Min, Max: Double): Double; overload;', @_LapeEnsureRangeF);
  addGlobalFunc('function InRange(Value, Min, Max: Int64): Boolean; overload;', @_LapeInRange);
  addGlobalFunc('function InRange(Value, Min, Max: Double): Boolean; overload;', @_LapeInRangeF);

  addGlobalFunc('function Abs(x: Double): Double; overload;', @_LapeAbs);
  addGlobalFunc('function Abs(x: Int64): Int64; overload;', @_LapeAbsI);
  addGlobalFunc('function Sign(AValue: Int64): Int8; overload;', @_LapeSign);
  addGlobalFunc('function Sign(AValue: Double): Int8; overload;', @_LapeSignF);
  addGlobalFunc('function Power(Base, Exponent: Double): Double;', @_LapePower);
  addGlobalFunc('function Sqr(x: Double): Double; overload;', @_LapeSqr);
  addGlobalFunc('function Sqr(x: Int64): Int64; overload;', @_LapeSqrI);
  addGlobalFunc('function Sqrt(x: Double): Double;', @_LapeSqrt);
  addGlobalFunc('function ArcTan(x: Double): Double;', @_LapeArcTan);
  addGlobalFunc('function Ln(x: Double): Double;', @_LapeLn);
  addGlobalFunc('function Sin(x: Double): Double;', @_LapeSin);
  addGlobalFunc('function Cos(x: Double): Double;', @_LapeCos);
  addGlobalFunc('function Exp(x: Double): Double;', @_LapeExp);
  addGlobalFunc('function Hypot(x,y: Double): Double', @_LapeHypot);
  addGlobalFunc('function ArcTan2(x,y: Double): Double', @_LapeArcTan2);
  addGlobalFunc('function Tan(x: Double): Double', @_LapeTan);
  addGlobalFunc('function ArcSin(x: Double): Double', @_LapeArcSin);
  addGlobalFunc('function ArcCos(x: Double): Double', @_LapeArcCos);
  addGlobalFunc('function Cotan(x: Double): Double', @_LapeCotan);
  addGlobalFunc('function Secant(x: Double): Double', @_LapeSecant);
  addGlobalFunc('function Cosecant(x: Double): Double', @_LapeCosecant);
  addGlobalFunc('function Round(x: Double): Int64; overload;', @_LapeRound);
  addGlobalFunc('function Round(x: Double; Precision: Int8): Double; overload;', @_LapeRoundTo);
  addGlobalFunc('function Frac(x: Double): Double;', @_LapeFrac);
  addGlobalFunc('function Int(x: Double): Double;', @_LapeInt);
  addGlobalFunc('function Trunc(x: Double): Int64;', @_LapeTrunc);
  addGlobalFunc('function Ceil(x: Double): Int64; overload;', @_LapeCeil);
  addGlobalFunc('function Ceil(x: Double; Precision: Int8): Double; overload;', @_LapeCeilTo);
  addGlobalFunc('function Floor(x: Double): Int64;', @_LapeFloor);
  addGlobalFunc('function CosH(x: Double): Double;', @_LapeCosH);
  addGlobalFunc('function SinH(x: Double): Double;', @_LapeSinH);
  addGlobalFunc('function TanH(x: Double): Double;', @_LapeTanH);
  addGlobalFunc('function ArcCosH(x: Double): Double;', @_LapeArcCosH);
  addGlobalFunc('function ArcSinH(x: Double): Double;', @_LapeArcSinH);
  addGlobalFunc('function ArcTanH(x: Double): Double;', @_LapeArcTanH);
  addGlobalFunc('procedure SinCos(theta: Double; out sinus, cosinus: Double);', @_LapeSinCos);
  addGlobalFunc('procedure DivMod(Dividend: UInt32; Divisor: UInt16; var Result, Remainder: UInt16);', @_LapeDivMod);

  DumpSection := 'Random';

  addGlobalVar(ltUInt32, @RandSeed, 'RandSeed');

  addGlobalFunc('function Random(min, max: Int64): Int64; overload;', @_LapeRandomRange);
  addGlobalFunc('function Random(min, max: Double): Double; overload;', @_LapeRandomRangeF);
  addGlobalFunc('function Random(l: Int64): Int64; overload;', @_LapeRandom);
  addGlobalFunc('function Random: Double; overload;', @_LapeRandomF);
  addGlobalFunc('procedure Randomize;', @_LapeRandomize);

  DumpSection := '';
end;

// lpeval_import_string.inc but removed a few things
procedure TScriptCompiler.InitBaseString;
begin
  DumpSection := 'Base';

  addGlobalType('set of (rfReplaceAll, rfIgnoreCase)', 'TReplaceFlags');

  addGlobalFunc('function UTF8Encode(s: WideString): AnsiString; overload;', @_LapeUTF8EncodeW);
  addGlobalFunc('function UTF8Encode(s: UnicodeString): AnsiString; overload;', @_LapeUTF8EncodeU);
  addGlobalFunc('function UTF8Decode(s: AnsiString): WideString; overload;', @_LapeUTF8DecodeW);
  addGlobalFunc('function UTF8Decode(s: AnsiString): UnicodeString; overload;', @_LapeUTF8DecodeU);

  // locale independent
  addGlobalFunc('function UpperCase(s: string): string;', @_LapeUpperCase);
  addGlobalFunc('function LowerCase(s: string): string;', @_LapeLowerCase);
  addGlobalFunc('function UpCase(c: AnsiChar): AnsiChar; overload;', @_LapeUpCaseA);
  addGlobalFunc('function UpCase(c: WideChar): WideChar; overload;', @_LapeUpCaseW);

  addGlobalFunc('function CompareStr(s1, s2: string): Int32;', @_LapeCompareStr);
  addGlobalFunc('function CompareText(s1, s2: string): Int32;', @_LapeCompareText);
  addGlobalFunc('function SameText(s1, s2: string): Boolean;', @_LapeSameText);

  // Uses current user locale
  addGlobalFunc('function AnsiUpperCase(s: string): string;', @_LapeAnsiUpperCase);
  addGlobalFunc('function AnsiLowerCase(s: string): string;', @_LapeAnsiLowerCase);
  addGlobalFunc('function AnsiCompareStr(s1, s2: string): Int32;', @_LapeAnsiCompareStr);
  addGlobalFunc('function AnsiCompareText(s1, s2: string): Int32;', @_LapeAnsiCompareText);
  addGlobalFunc('function AnsiSameText(s1,s2: string): Boolean;', @_LapeAnsiSameText);
  addGlobalFunc('function AnsiSameStr(s1,s2: string): Boolean;', @_LapeAnsiSameStr);

  // Uses current user locale
  addGlobalFunc('function WideUpperCase(s: WideString): WideString;', @_LapeWideUpperCase);
  addGlobalFunc('function WideLowerCase(s: WideString): WideString;', @_LapeWideLowerCase);
  addGlobalFunc('function WideCompareStr(s1, s2: WideString): Int32;', @_LapeWideCompareStr);
  addGlobalFunc('function WideCompareText(s1, s2: WideString): Int32;', @_LapeWideCompareText);
  addGlobalFunc('function WideSameText(s1,s2: WideString): Boolean;', @_LapeWideSameText);
  addGlobalFunc('function WideSameStr(s1,s2: WideString): Boolean;', @_LapeWideSameStr);
  addGlobalFunc('function WideFormat(Fmt: WideString; Args: array of Variant): WideString;', @_LapeWideFormat);

  addGlobalFunc('function Pos(Substr, Source: AnsiString): SizeInt; overload;', @_LapePosA);
  addGlobalFunc('function Pos(Substr, Source: WideString): SizeInt; overload;', @_LapePosW);
  addGlobalFunc('function Pos(Substr, Source: UnicodeString): SizeInt; overload;', @_LapePosU);

  addGlobalFunc('function StringReplace(S, OldPattern, NewPattern: string; Flags: TReplaceFlags = [rfReplaceAll]): string;', @_LapeStringReplace);
  addGlobalFunc('function UnicodeStringReplace(S, OldPattern, NewPattern: UnicodeString; Flags: TReplaceFlags = [rfReplaceAll]): UnicodeString;', @_LapeUnicodeStringReplace);
  addGlobalFunc('function WideStringReplace(S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags = [rfReplaceAll]): WideString;', @_LapeWideStringReplace);

  addGlobalFunc('function Trim(s: string): string;', @_LapeTrim);
  addGlobalFunc('function TrimLeft(s: string): string;', @_LapeTrimLeft);
  addGlobalFunc('function TrimRight(s: string): string;', @_LapeTrimRight);
  addGlobalFunc('function PadL(s: string; Len: SizeInt; c: Char = '' ''): string;', @_LapePadL);
  addGlobalFunc('function PadR(s: string; Len: SizeInt; c: Char = '' ''): string;', @_LapePadR);

  addGlobalFunc('function IntToHex(Value: Int64; Digits: Int32 = 1): string; overload;', @_LapeIntToHex);
  addGlobalFunc('function IntToHex(Value: UInt64; Digits: Int32 = 1): string; overload;', @_LapeUIntToHex);
  addGlobalFunc('function IntToStr(i: Int64): string; overload;', @_LapeToString_Int64);
  addGlobalFunc('function IntToStr(i: UInt64): string; overload;', @_LapeToString_UInt64);

  addGlobalFunc('function StrToInt(s: string): Int32; overload;', @_LapeStrToInt);
  addGlobalFunc('function StrToInt(s: string; Def: Int32): Int32; overload;', @_LapeStrToIntDef);
  addGlobalFunc('function StrToInt64(s: string): Int64; overload;', @_LapeStrToInt64);
  addGlobalFunc('function StrToInt64(s: string; Def: Int64): Int64; overload;', @_LapeStrToInt64Def);
  addGlobalFunc('function StrToUInt64(s: string): UInt64; overload;', @_LapeStrToUInt64);
  addGlobalFunc('function StrToUInt64(s: string; Def: UInt64): UInt64; overload;', @_LapeStrToUInt64Def);
  addGlobalFunc('function StrToFloat(s: string): Double; overload;', @_LapeStrToFloat);
  addGlobalFunc('function StrToFloat(s: string; Def: Double): Double; overload;', @_LapeStrToFloatDef);
  addGlobalFunc('function StrToCurr(s: string): Currency; overload;', @_LapeStrToCurr);
  addGlobalFunc('function StrToCurr(s: string; Def: Currency): Currency; overload;', @_LapeStrToCurrDef);
  addGlobalFunc('function StrToBool(s: string): Boolean; overload;', @_LapeStrToBool);
  addGlobalFunc('function StrToBool(s: string; Default: Boolean): Boolean; overload;', @_LapeStrToBoolDef);

  addGlobalFunc('function BoolToStr(B: Boolean; TrueS: string = ''True''; FalseS: string = ''False''): string;', @_LapeBoolToStr);
  addGlobalFunc('function FloatToStr(f: Double): string;', @_LapeToString_Double);
  addGlobalFunc('function CurrToStr(Value: Currency): string;', @_LapeToString_Currency);

  addGlobalFunc('function Format(Fmt: string; Args: array of Variant): string;', @_LapeFormat);
  addGlobalFunc('function FormatCurr(Format: string; Value: Currency): string;', @_LapeFormatCurr);
  addGlobalFunc('function FormatFloat(Format: string; Value: Double): string;', @_LapeFormatFloat);

  addGlobalFunc('function StringOfChar(c: Char; l: SizeInt): string;', @_LapeStringOfChar);

  DumpSection := '';
end;

// Import our own methods later (import_datetime.pas)
procedure TScriptCompiler.InitBaseDateTime;
begin
  DumpSection := 'Date & Time';

  addGlobalType(getBaseType(ltDouble).createCopy(True), 'TDateTime', False);

  TLapeType_OverloadedMethod(Globals['ToString'].VarType).addMethod(
    TLapeType_Method(addManagedType(
      TLapeType_Method.Create(
        Self,
        [getGlobalType('TDateTime')],
        [lptConstRef],
        [TLapeGlobalVar(nil)],
        getBaseType(ltString)
      )
    )).NewGlobalVar(@_LapeDateTimeToStr)
  );

  addGlobalFunc('function GetTickCount: UInt64;', @_LapeGetTickCount);
  addGlobalFunc('procedure Sleep(MilliSeconds: UInt32);', @_LapeSleep);

  DumpSection := '';
end;

constructor TScriptCompiler.Create(Doc: String; FileName: String);
begin
  inherited Create(TLapeTokenizerString.Create(Doc, FileName));
end;

constructor TScriptCompiler.CreateDump;
var
  BaseType: ELapeBaseType;
begin
  FDump := TSimbaStringPairList.Create();

  // init the dump with things not imported by normal means
  for BaseType in ELapeBaseType do
    if (FBaseTypes[BaseType] <> nil) then
      DumpCode('Base', 'type %s = %s;'.Format([LapeTypeToString(BaseType), LapeTypeToString(BaseType)]));

  DumpCode('Base', 'procedure Delete(A: array; Index: Int32; Count: Int32 = Length(A)); external;');
  DumpCode('Base', 'procedure Insert(Item: Anything; A: array; Index: Int32); external;');
  DumpCode('Base', 'procedure Copy(A: array; Index: Int32 = 0; Count: Int32 = Length(A)); external;');
  DumpCode('Base', 'procedure SetLength(A: array; Length: Int32); external;');
  DumpCode('Base', 'function Low(A: array): Int32; external;');
  DumpCode('Base', 'function High(A: array): Int32; external;');
  DumpCode('Base', 'function Length(A: array): Int32; external;');
  DumpCode('Base', 'procedure WriteLn(Args: Anything); external;');
  DumpCode('Base', 'procedure Write(Args: Anything); external;');
  DumpCode('Base', 'procedure Swap(var A, B: Anything); external;');
  DumpCode('Base', 'function SizeOf(A: Anything): Int32; external;');
  DumpCode('Base', 'function ToString(A: Anything): String; external;');
  DumpCode('Base', 'function ToStr(A: Anything): String; external;');
  DumpCode('Base', 'function Inc(var X: Ordinal; Amount: SizeInt = 1): Ordinal; external;');
  DumpCode('Base', 'function Dec(var X: Ordinal; Amount: SizeInt = 1): Ordinal; external;');
  DumpCode('Base', 'function Ord(X: Ordinal): Int32; external;');
  DumpCode('Base', 'function SleepUntil(Condition: BoolExpr; Interval, Timeout: Int32): Boolean; external;');
  DumpCode('Base', 'function Default(T: AnyType): AnyType; external;');
  DumpCode('Base', 'procedure Sort(var A: array); overload; external;');
  DumpCode('Base', 'procedure Sort(var A: array; Weights: array of Ordinal; LowToHigh: Boolean); overload; external;');
  DumpCode('Base', 'procedure Sort(var A: array; CompareFunc: function(L, R: Anything): Int32); overload; external;');
  DumpCode('Base', 'function Sorted(const A: array): array; overload; external;');
  DumpCode('Base', 'function Sorted(const A: array; CompareFunc: function(L, R: Anything): Int32): array; overload; external;');
  DumpCode('Base', 'function Sorted(const A: array; Weights: array of Ordinal; LowToHigh: Boolean): array; overload; external;');
  DumpCode('Base', 'function Unique(const A: array): array; external;');
  DumpCode('Base', 'procedure Reverse(var A: array); external;');
  DumpCode('Base', 'function Reversed(const A: array): array; external;');
  DumpCode('Base', 'function IndexOf(const Item: T; const A: array): Integer; external;');
  DumpCode('Base', 'function IndicesOf(const Item: T; const A: array): TIntegerArray; external;');
  DumpCode('Base', 'function Contains(const Item: T; const A: array): Boolean; external;');
  DumpCode('Base', 'function RTTIFields(constref RecordTypeOrVar): TRTTIFields; external;');
  DumpCode('Base', 'function GetExceptionLocationStr: String; external;');
  DumpCode('Base', 'function GetExceptionMessage: String; external;');
  DumpCode('Base', 'function GetScriptMethodName(Address: Pointer): String; external;');
  DumpCode('Base', 'function DumpCallStack(Start: Integer = 0): String; external;');

  DumpCode('Base', 'function TMap(KeyType: T; ValueType: V): TMap; external;');
  DumpCode('Base', 'function TStringMap(ValueType: V): TStringMap; external;');
  DumpCode('Base', 'function THeap(ValueType: V): THeap; external;');
  DumpCode('Base', 'function TArrayBuffer(ValueType: V): TArrayBuffer; external;');

  inherited Create(TLapeTokenizerString.Create(''));
end;

destructor TScriptCompiler.Destroy;
begin
  if (FDump <> nil) then
    FreeAndNil(FDump);

  inherited Destroy;
end;

function TScriptCompiler.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, Typ);
end;

function TScriptCompiler.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, Typ);
end;

function TScriptCompiler.addGlobalVar(Typ: ELapeBaseType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, LapeTypeToString(Typ));
end;

function TScriptCompiler.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'Int32');
end;

function TScriptCompiler.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'UInt32');
end;

function TScriptCompiler.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'Int64');
end;

function TScriptCompiler.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'UInt64');
end;

function TScriptCompiler.addGlobalVar(Value: Single; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'Single');
end;

function TScriptCompiler.addGlobalVar(Value: Double; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'Double');
end;

function TScriptCompiler.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'String');
end;

function TScriptCompiler.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'UnicodeString');
end;

function TScriptCompiler.addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'Variant');
end;

function TScriptCompiler.addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpVar(AName, 'Pointer');
end;

function TScriptCompiler.addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpMethod(Header);
end;

function TScriptCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  Result := inherited;

  if (FDump <> nil) then
    DumpType(AName, Str);
end;

function TScriptCompiler.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  if (FDump <> nil) and (Typ <> nil) and (Typ.Name <> '') then
    DumpType(AName, Typ.Name);

  Result := inherited;
end;

procedure TScriptCompiler.pushCode(Code: String);
begin
  pushTokenizer(TLapeTokenizerString.Create(Code));
end;

procedure TScriptCompiler.addDelayedCode(Code: TStringArray; AFileName: lpString);
begin
  addDelayedCode(LapeDelayedFlags + LineEnding.Join(Code), AFileName);
end;

procedure TScriptCompiler.addProperty(Obj, Name, Typ: String; ReadFunc: Pointer; WriteFunc: Pointer);
begin
  if (ReadFunc <> nil) then
    addGlobalFunc('property ' + Obj + '.' + Name + ': ' + Typ + ';', ReadFunc);
  if (WriteFunc <> nil) then
    addGlobalFunc('property ' + Obj + '.' + Name + '(Value: ' + Typ + ');', WriteFunc);
end;

procedure TScriptCompiler.addPropertyIndexed(Obj, Name, Params, Typ: String; ReadFunc: Pointer; WriteFunc: Pointer);
begin
  if (ReadFunc <> nil) then
    addGlobalFunc('property ' + Obj + '.' + Name + '(' + Params + '):' + Typ + ';', ReadFunc);
  if (WriteFunc <> nil) then
    addGlobalFunc('property ' + Obj + '.' + Name + '(' + Params + '; Value: ' + Typ + ');', WriteFunc);
end;

procedure TScriptCompiler.addMagic(Name: String; Params: array of lpString; ParamTypes: array of ELapeParameterType; Res: String; Func: Pointer);

  function getType(Name: lpString): TLapeType;
  begin
    if (Name <> '') then
    begin
      Result := getGlobalType(Name);
      if (Result = nil) then
      begin
        Result := getBaseType(Name);
        if (Result = nil) then
          SimbaException('Type "%s" not found', [Name]);
      end;
    end else
      Result := nil;
  end;

var
  ParamVarTypes: array of TLapeType;
  ParamDefaults: array of TLapeGlobalVar;
  i: Integer;
  Header: TLapeType_Method;
begin
  if (Globals[Name] = nil) or (not (Globals[Name].VarType is TLapeType_OverloadedMethod)) then
    SimbaException('addNativeMagic "%s" is incorrect', [Name]);

  SetLength(ParamVarTypes, Length(Params));
  SetLength(ParamDefaults, Length(Params));
  for i := 0 to High(ParamVarTypes) do
    ParamVarTypes[i] := getType(Params[i]);

  Header := addManagedType(TLapeType_Method.Create(Self, ParamVarTypes, ParamTypes, ParamDefaults, getType(Res))) as TLapeType_Method;
  with TLapeType_OverloadedMethod(Globals[Name].VarType) do
    addMethod(Header.NewGlobalVar(Func));
end;

function TScriptCompiler.addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method;
var
  Decl: lpString;
  OldState: Pointer;
begin
  Decl := Header + LineEnding + LineEnding.Join(Body);
  OldState := getTempTokenizerState(LapeDelayedFlags + Decl, '!' + Header);
  try
    Expect([tk_kw_Function, tk_kw_Procedure, tk_kw_Operator, tk_kw_Property]);
    Result := ParseMethod(nil, False);
    CheckAfterCompile();
    addDelayedExpression(Result, True, True);
  finally
    resetTokenizerState(OldState);
  end;

  if (FDump <> nil) then
    DumpCode(DumpSection, Decl);
end;

function TScriptCompiler.addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar;
var
  Closure: TManagedImportClosure;
begin
  Closure := TManagedImportClosure.Create();
  Closure.Closure := LapeImportWrapper(Value, Self, Header, ABI);

  with TManagedImportClosure(addManagedDecl(Closure)) do
    Result := addGlobalFunc(Header, Closure.Func);
end;

function TScriptCompiler.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  Result := addGlobalType('native(type ' + Str + ', ffi_' + ABIToStr(ABI) + ')', AName);
end;

function TScriptCompiler.addGlobalType(Str: TStringArray; Name: String): TLapeType;
begin
  Result := addGlobalType(LineEnding.Join(Str), Name);
end;

function TScriptCompiler.addClassConstructor(Obj, Params: lpString; Func: Pointer; IsOverload: Boolean): TLapeGlobalVar;
var
  Directives: String;
begin
  Directives := 'static;';
  if IsOverload then
    Directives := Directives + ' overload;';

  Result := addGlobalFunc('function ' + Obj + '.Create' + Params + ': ' + Obj + ';' + Directives, Func);
end;

procedure TScriptCompiler.addClass(Name: lpString; Parent: lpString);
begin
  addGlobalType('strict ' + Parent, Name);
end;

function TScriptCompiler.Compile: Boolean;
begin
  {$IF DEFINED(DARWIN) and DECLARED(LoadFFI)}
  if not FFILoaded then
    LoadFFI('/usr/local/opt/libffi/lib/');
  {$ENDIF}

  if not FFILoaded then
    SimbaException('ERROR: libffi is missing or incompatible');

  Result := inherited Compile();
end;

procedure TScriptCompiler.CallProc(ProcName: String);
var
  Method: TLapeGlobalVar;
begin
  Method := Globals[ProcName];
  if (Method = nil) or (Method.BaseType <> ltScriptMethod) or
     (TLapeType_Method(Method.VarType).Res <> nil) or (TLapeType_Method(Method.VarType).Params.Count <> 0) then
    SimbaException('CallProc: Invalid procedure "%s"', [ProcName]);

  with TLapeCodeRunner.Create(Emitter) do
  try
    Run(PCodePos(Method.Ptr)^);
  finally
    Free();
  end;
end;

end.

