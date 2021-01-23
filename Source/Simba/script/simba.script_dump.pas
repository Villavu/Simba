unit simba.script_dump;

{$mode objfpc}{$H+}

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

    procedure WriteMethod(Header: String);
    procedure Write(Header: String);
  public
    function addDelayedCode(Code: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;

    function addGlobalFunc(Header: lpString; Value: Pointer): TLapeGlobalVar; overload; override;

    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; override;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; overload; override;

    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; override;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; override;

    function addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar; override;
    function addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar; override;

    procedure addBaseDefine(Define: lpString); override;

    procedure Dump(Strings: TStringList); overload;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

procedure TCompilerDump.Write(Header: String);
begin
  if (FList = nil) or (FSection = '') then
    Exit;

  FList.Values[FSection] := FList.Values[FSection] + Header + LineEnding;
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

function TCompilerDump.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);

  Write(Format('var %s: %s;', [AName, Typ]));
end;

function TCompilerDump.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: Int32 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: UInt32 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: Int64 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: UInt64 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: Extended = %s;', [AName, FloatToStr(Value)]));
end;

function TCompilerDump.addGlobalVar(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: EvalBool = %s;', [AName, BoolToStr(Value, True)]));
end;

function TCompilerDump.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: String = "%s";', [AName, Value]));
end;

function TCompilerDump.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('var %s: UnicodeString = "%s";', [AName, Value]));
end;

function TCompilerDump.addGlobalConst(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Typ, Value, AName);

  Write(Format('const %s: %s;', [AName, Typ]));
end;

function TCompilerDump.addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: Int32 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: UInt32 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalConst(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: Int64 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalConst(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: UInt64 = %d;', [AName, Value]));
end;

function TCompilerDump.addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: Extended = %s;', [AName, FloatToStr(Value)]));
end;

function TCompilerDump.addGlobalConst(Value: EvalBool; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: EvalBool = %s;', [AName, BoolToStr(Value, True)]));
end;

function TCompilerDump.addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: String = "%s";', [AName, Value]));
end;

function TCompilerDump.addGlobalConst(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := inherited addGlobalVar(Value, AName);

  Write(Format('const %s: UnicodeString = "%s";', [AName, Value]));
end;

function TCompilerDump.addDelayedCode(Code: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  Result := inherited addDelayedCode(Code, AFileName, AfterCompilation, IsGlobal);

  if (not AFileName.StartsWith('!')) then
    Write(Code);
end;

procedure TCompilerDump.addBaseDefine(Define: lpString);
begin
  inherited addBaseDefine(Define);

  Write(Format('{$DEFINE %s}', [Define]));
end;

constructor TCompilerDump.Create;
begin
  inherited Create(TLapeTokenizerString.Create('begin end.', ''));

  FList := TStringList.Create();
  FSection := 'System';

  Write('{$DEFINE Lape}');
  Write('{$DEFINE Sesquipedalian}');
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
  Write('function GetMem(i: SizeInt): Pointer; external;');
  Write('function AllocMem(i: SizeInt): Pointer; external;');
  Write('procedure FreeMem(p: Pointer); external;');
  Write('procedure ReallocMem(var p: Pointer; s: SizeInt); external;');
  Write('procedure FillMem(var p; s: SizeInt; b: UInt8 = 0); external;');
  Write('function CompareMem(constref p1, p2; Length: SizeInt): EvalBool; external;');
  Write('function Assigned(constref p): EvalBool; external;');
  Write('procedure RaiseException(Ex: string); overload; external;');
  Write('procedure UniqueString(var Str: AnsiString); overload; external;');
  Write('procedure UniqueString(var Str: WideString); overload; external;');
  Write('procedure UniqueString(var Str: UnicodeString); overload; external;');
  Write('function Random(min, max: Int64): Int64; overload; external;');
  Write('function Random(min, max: Extended): Extended; overload; external;');
  Write('function Random(l: Int64): Int64; overload; external;');
  Write('function Random: Extended; overload; external;');
  Write('procedure Randomize; external;');
  Write('var RandSeed: Int32;');
  Write('procedure Sleep(Milliseconds: UInt32); external;');
  Write('function GetExceptionMessage: ShortString; external;');
  Write('function Inc(var X: Ordinal): Ordinal; overload; external;');
  Write('function Dec(var X: Ordinal): Ordinal; overload; external;');
  Write('function Inc(var X: Ordinal; Amount: SizeInt): Ordinal; overload; external;');
  Write('function Dec(var X: Ordinal; Amount: SizeInt): Ordinal; overload; external;');
  Write('function Ord(X: Ordinal): Int32; external;');
  Write('function WaitUntil(Condition: Expression; Interval, Timeout: Int32): Boolean; external;');

  Section := 'Types';

  Write('type UInt8 = UInt8;');
  Write('type Int8 = Int8;');
  Write('type UInt16 = UInt16;');
  Write('type Int16 = Int16;');
  Write('type UInt32 = UInt32;');
  Write('type Int32 = Int32;');
  Write('type UInt64 = UInt64;');
  Write('type Int64 = Int64;');
  Write('type Currency = Currency;');
  Write('type Single = Single;');
  Write('type Double = Double;');
  Write('type Extended = Extended;');
  Write('type Boolean = Boolean;');
  Write('type ByteBool = ByteBool;');
  Write('type WordBool = WordBool;');
  Write('type LongBool = LongBool;');
  Write('type AnsiChar = AnsiChar;');
  Write('type WideChar = WideChar;');
  Write('type Pointer = Pointer;');
  Write('type AnsiString = AnsiString;');
  Write('type ShortString = ShortString;');
  Write('type UnicodeString = UnicodeString;');
  Write('type String = AnsiString;');
  Write('type TMethod = packed record Method, Self: Pointer; end;');
  Write('var True: Boolean;');
  Write('var False: Boolean;');

  Section := 'String';

  Write('type TTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);');
  Write('type TReplaceFlag = (rfReplaceAll, rfIgnoreCase);');
  Write('type TReplaceFlags = set of TReplaceFlag;');
  Write('function UpperCase(s: string): string; external;');
  Write('function LowerCase(s: string): string; external;');
  Write('function UpCase(c: AnsiChar): AnsiChar; overload; external;');
  Write('function UpCase(c: WideChar): WideChar; overload; external;');
  Write('function CompareStr(s1, s2: string): Int32; external;');
  Write('function CompareText(s1, s2: string): Int32; external;');
  Write('function SameText(s1, s2: string): EvalBool; external;');
  Write('function AnsiUpperCase(s: string): string; external;');
  Write('function AnsiLowerCase(s: string): string; external;');
  Write('function AnsiCompareStr(s1, s2: string): Int32; external;');
  Write('function AnsiCompareText(s1, s2: string): Int32; external;');
  Write('function AnsiSameText(s1,s2: string): EvalBool; external;');
  Write('function AnsiSameStr(s1,s2: string): EvalBool; external;');
  Write('function Trim(s: string): string; external;');
  Write('function TrimLeft(s: string): string; external;');
  Write('function TrimRight(s: string): string; external;');
  Write('function PadL(s: string; Len: SizeInt; c: Char = ' +  #32 + '): string; external;');
  Write('function PadR(s: string; Len: SizeInt; c: Char = ' +  #32 + '): string; external;');
  Write('function QuotedStr(s: string): string; external;');
  Write('function AnsiQuotedStr(s: string; Quote: Char): string; external;');
  Write('function AnsiDequotedStr(s: string; AQuote: Char): string; external;');
  Write('function WrapText(Line, BreakStr: string; BreakChars: set of AnsiChar; MaxCol: Int32): string; external;');
  Write('function AdjustLineBreaks(s: string; Style: TTextLineBreakStyle): string; external;');
  Write('function IntToHex(Value: Int64; Digits: Int32 = 1): string; overload; external;');
  Write('function IntToHex(Value: UInt64; Digits: Int32 = 1): string; overload; external;');
  Write('function IntToStr(i: Int64): string; overload; external;');
  Write('function IntToStr(i: UInt64): string; overload; external;');
  Write('function StrToInt(s: string): Int32; external;');
  Write('function StrToIntDef(s: string; Def: Int32): Int32; external;');
  Write('function StrToInt64(s: string): Int64; external;');
  Write('function StrToInt64Def(s: string; Def: Int64): Int64; external;');
  Write('function StrToUInt64(s: string): UInt64; external;');
  Write('function StrToUInt64Def(s: string; Def: UInt64): UInt64; external;');
  Write('function FloatToStr(f: Extended): string; external;');
  Write('function StrToFloat(s: string): Extended; external;');
  Write('function StrToFloatDef(s: string; Def: Extended): Extended; external;');
  Write('function CurrToStr(Value: Currency): string; external;');
  Write('function StrToCurr(s: string): Currency; external;');
  Write('function StrToCurrDef(s: string; Def: Currency): Currency; external;');
  Write('function StrToBool(s: string): EvalBool; external;');
  Write('function BoolToStr(B: EvalBool; TrueStr: string = ' + #39 + 'True' + #39 + '; FalseStr: string = ' + #39 + 'False' + #39 +'): string; external;');
  Write('function StrToBoolDef(s: string; Default: EvalBool): EvalBool; external;');
  Write('function Format(Fmt: string; Args: array of Variant): string; external;');
  Write('function FormatFloat(Format: string; Value: Extended): string; external;');
  Write('function FormatCurr(Format: string; Value: Currency): string; external;');
  Write('function LastDelimiter(Delimiters, s: string): SizeInt; external;');
  Write('function StringReplace(S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string; external;');
  Write('Function IsDelimiter(Delimiters, s: string; Index: SizeInt): EvalBool; external;');
  Write('function Pos(Substr: string; Source: string): SizeInt; external;');
  Write('function StringOfChar(c: Char; l: SizeInt): string; external;');

  Section := 'Math';

  Write('const Pi: Extended = 3.14159265358979;');
  Write('function Min(x,y: Int64): Int64; overload; external;');
  Write('function Min(x,y: Extended): Extended; overload; external;');
  Write('function Max(x,y: Int64): Int64; overload; external;');
  Write('function Max(x,y: Extended): Extended; overload; external;');
  Write('function Abs(x: Extended): Extended; overload; external;');
  Write('function Abs(x: Int64): Int64; overload; external;');
  Write('function Sign(AValue: Int64): Int8; overload; external;');
  Write('function Sign(AValue: Extended): Int8; overload; external;');
  Write('function Power(Base, Exponent: Extended): Extended; external;');
  Write('function Sqr(x: Extended): Extended; overload; external;');
  Write('function Sqr(x: Int64): Int64; overload; external;');
  Write('function Sqrt(x: Extended): Extended; external;');
  Write('function ArcTan(x: Extended): Extended; external;');
  Write('function Ln(x: Extended): Extended; external;');
  Write('function Sin(x: Extended): Extended; external;');
  Write('function Cos(x: Extended): Extended; external;');
  Write('function Exp(x: Extended): Extended; external;');
  Write('function Hypot(x,y: Extended): Extended; external;');
  Write('function ArcTan2(x,y: Extended): Extended; external;');
  Write('function Tan(x: Extended): Extended; external;');
  Write('function ArcSin(x: Extended): Extended; external;');
  Write('function ArcCos(x: Extended): Extended; external;');
  Write('function Cotan(x: Extended): Extended; external;');
  Write('function Secant(x: Extended): Extended; external;');
  Write('function Cosecant(x: Extended): Extended; external;');
  Write('function Round(x: Extended): Int64; overload; external;');
  Write('function Round(x: Extended; Precision: Int8): Extended; overload; external;');
  Write('function Frac(x: Extended): Extended; external;');
  Write('function Int(x: Extended): Extended; external;');
  Write('function Trunc(x: Extended): Int64; external;');
  Write('function Ceil(x: Extended): Int64; external;');
  Write('function Floor(x: Extended): Int64; external;');
  Write('function CosH(x: Extended): Extended; external;');
  Write('function SinH(x: Extended): Extended; external;');
  Write('function TanH(x: Extended): Extended; external;');
  Write('function ArcCosH(x: Extended): Extended; external;');
  Write('function ArcSinH(x: Extended): Extended; external;');
  Write('function ArcTanH(x: Extended): Extended; external;');

  Section := 'File';

  Write('const DirectorySeparator = ' + #39 + DirectorySeparator + #39 + ';');
  Write('function ExtractFilePath(const FileName: String): String; external;');
  Write('function ExtractFileDrive(const FileName: String): String; external;');
  Write('function ExtractFileName(const FileName: String): String; external;');
  Write('function ExtractFileExt(const FileName: String): String; external;');
  Write('function ExtractFileDir(const FileName: String): String; external;');
  Write('function ExpandFileName(const FileName: String): String; external;');
  Write('function ExtractRelativePath(const BaseName, DestName: String): String; external;');
  Write('function IncludeTrailingPathDelimiter(const Path: String) : String; external;');
  Write('function ExcludeTrailingPathDelimiter(const Path: String): String; external;');
  Write('function IncludeTrailingBackslash(const Path: String) : String; external;');
  Write('function ExcludeTrailingBackslash(const Path: String): String; external;');
  Write('function IncludeLeadingPathDelimiter(const Path : String) : String; external;');
  Write('function ExcludeLeadingPathDelimiter(const Path: String): String; external;');

  Section := 'Date & Time';

  Write('var HoursPerDay: Int32 = 24;');
  Write('var MinsPerHour: Int32 = 60;');
  Write('var SecsPerMin: Int32 = 60;');
  Write('var MSecsPerSec: Int32 = 1000;');
  Write('var MinsPerDay: Int32 = 1440;');
  Write('var SecsPerDay: Int32 = 86400;');
  Write('var MSecsPerDay: Int32 = 86400000;');
  Write('var DateDelta: Int32 = 693594;');
  Write('function EncodeDate(Year, Month, Day: UInt16): TDateTime; external;');
  Write('function EncodeTime(Hour, Min, Sec, MSec: UInt16): TDateTime; external;');
  Write('procedure DecodeDate(DateTime: TDateTime; var Year, Month, Day: UInt16); external;');
  Write('function DecodeDateFully(DateTime: TDateTime; var Year, Month, Day, DOW: UInt16): Boolean; external;');
  Write('procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: UInt16); external;');
  Write('function DateTimeToStr(const DateTime: TDateTime): string; external;');
  Write('function DateToStr(const DateTime: TDateTime): string; external;');
  Write('function TimeToStr(const DateTime: TDateTime): string; external;');
  Write('function Date: TDateTime; external;');
  Write('function Time: TDateTime; external;');
  Write('function Now: TDateTime; external;');
  Write('function GetTickCount: UInt64; external;');
  Write('procedure ReplaceTime(var DateTime: TDateTime; NewTime: TDateTime); external;');
  Write('procedure ReplaceDate(var DateTime: TDateTime; NewDate: TDateTime); external;');
  Write('function FormatDateTime(Format: string; DateTime: TDateTime): string; external;');
  Write('function StrToDate(s: string): TDateTime; external;');
  Write('function StrToDateDef(s: string; Default: TDateTime): TDateTime; external;');
  Write('function StrToTime(s: string): TDateTime; external;');
  Write('function StrToTimeDef(s: string; Default: TDateTime): TDateTime; external;');
  Write('function StrToDateTime(s: string): TDateTime; external;');
  Write('function StrToDateTimeDef(s: string; Default: TDateTime): TDateTime; external;');

  Section := 'Variant';

  Write('var Null: Variant;');
  Write('var Unassigned: Variant;');
  Write('type TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);');
  Write('var VarEmpty: Int32 = 0;');
  Write('var VarNull: Int32 = 1;');
  Write('var VarSmallInt: Int32 = 2;');
  Write('var VarInteger: Int32 = 3;');
  Write('var VarSingle: Int32 = 4;');
  Write('var VarDouble: Int32 = 5;');
  Write('var VarDate: Int32 = 7;');
  Write('var VarCurrency: Int32 = 6;');
  Write('var VarOleStr: Int32 = 8;');
  Write('var VarDispatch: Int32 = 9;');
  Write('var VarError: Int32 = 10;');
  Write('var VarBoolean: Int32 = 11;');
  Write('var VarVariant: Int32 = 12;');
  Write('var VarUnknown: Int32 = 13;');
  Write('var VarShortInt: Int32 = 16;');
  Write('var VarByte: Int32 = 17;');
  Write('var VarWord: Int32 = 18;');
  Write('var VarLongWord: Int32 = 19;');
  Write('var VarInt64: Int32 = 20;');
  Write('var VarStrArg: Int32 = 72;');
  Write('var VarString: Int32 = 256;');
  Write('var VarAny: Int32 = 257;');
  Write('var VarUString: Int32 = 258;');
  Write('var VarUInt64: Int32 = 21;');
  Write('var VarTypeMask: Int32 = 4095;');
  Write('var VarArray: Int32 = 8192;');
  Write('var VarByRef: Int32 = 16384;');
  Write('function VarType(const V: Variant): TVarType; external;');
  Write('function VarAsType(const V: Variant; aVarType: TVarType): Variant; external;');
  Write('function VarIsByRef(const V: Variant): EvalBool; external;');
  Write('function VarIsEmpty(const V: Variant): EvalBool; external;');
  Write('function VarIsNull(const V: Variant): EvalBool; external;');
  Write('function VarIsClear(const V: Variant): EvalBool; external;');
  Write('function VarIsError(const V: Variant; out AResult: HRESULT): EvalBool; external;');
  Write('function VarAsError(AResult: HRESULT): Variant; external;');
  Write('function VarIsCustom(const V: Variant): EvalBool; external;');
  Write('function VarIsOrdinal(const V: Variant): EvalBool; external;');
  Write('function VarIsFloat(const V: Variant): EvalBool; external;');
  Write('function VarIsNumeric(const V: Variant): EvalBool; external;');
  Write('function VarIsStr(const V: Variant): EvalBool; external;');
  Write('function VarIsArray(const A: Variant; AResolveByRef: EvalBool = True): EvalBool; external;');
  Write('function VarToStr(const V: Variant): string; external;');
  Write('function VarToStrDef(const V: Variant; ADefault: string): string; external;');
  Write('function VarToWideStr(const V: Variant): WideString; external;');
  Write('function VarToWideStrDef(const V: Variant; ADefault: WideString): WideString; external;');
  Write('function VarToUnicodeStr(const V: Variant): UnicodeString; external;');
  Write('function VarToUnicodeStrDef(const V: Variant; ADefault: UnicodeString): UnicodeString; external;');
  Write('function VarToDateTime(const V: Variant): TDateTime; external;');
  Write('function VarFromDateTime(DateTime: TDateTime): Variant; external;');
  Write('function VarInRange(const AValue, AMin, AMax: Variant): EvalBool; external;');
  Write('function VarEnsureRange(const AValue, AMin, AMax: Variant): Variant; external;');
  Write('function VarSameValue(const A, B: Variant): EvalBool; external;');
  Write('function VarCompareValue(const A, B: Variant): TVariantRelationship; external;');
  Write('function VarTypeIsValidArrayType(aVarType: TVarType): EvalBool; external;');
  Write('function VarTypeIsValidElementType(aVarType: TVarType): EvalBool; external;');
  Write('function VarArrayCreate(Bounds: array of SizeInt; aVarType: TVarType): Variant; external;');
  Write('function VarArrayOf(Values: array of Variant): Variant; external;');
  Write('procedure VarArrayRedim(var A: Variant; HighBound: SizeInt); external;');
  Write('function VarArrayAsPSafeArray(const A: Variant): Pointer; external;');
  Write('procedure VarCopyNoInd(var Dest: Variant; const Source: Variant); external;');
  Write('function VarArrayDimCount(const A: Variant): SizeInt; external;');
  Write('function VarArrayLowBound(const A: Variant; Dim: SizeInt): SizeInt; external;');
  Write('function VarArrayHighBound(const A: Variant; Dim: SizeInt): SizeInt; external;');
  Write('function VarArrayLock(const A: Variant): Pointer; external;');
  Write('procedure VarArrayUnlock(const A: Variant); external;');
  Write('function VarArrayRef(const A: Variant): Variant; external;');
  Write('function VarArrayGet(const A: Variant; Indices: array of Int32): Variant; external;');
  Write('procedure VarArraySet(var A: Variant; const Value: Variant; Indices: array of Int32); external;');
end;

destructor TCompilerDump.Destroy;
begin
  FList.Free();

  inherited Destroy();
end;

procedure TCompilerDump.Dump(Strings: TStringList);
begin
  Compile();

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

