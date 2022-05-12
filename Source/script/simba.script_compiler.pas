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
  ffi, lpffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers;

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

    procedure InitBaseDefinitions; override;
    procedure InitBaseDateTime; override;
    procedure InitBaseString; override;
    procedure InitBaseVariant; override;
  public
    function getIntegerArray: TLapeType; override;
    function getFloatArray: TLapeType; override;

    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    procedure addDelayedCode(Code: array of lpString; AFileName: lpString = ''); virtual; overload;

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
  blowfish, md5, sha1, hmac, forms, process, lazloggerbase, variants,
  SynLZ,

  simba.mufasatypes, simba.script, simba.scriptthread, simba.outputform,
  simba.files, simba.process, simba.bitmap, simba.bitmap_helpers,
  simba.helpers_windowhandle, simba.matchtemplate, simba.tpa,
  simba.target_exported, simba.math, simba.colormath, simba.stringutil,
  simba.internet, simba.datetime, simba.dtmutil, simba.dtm, simba.iomanager,
  simba.aca, simba.dtmeditor, simba.script_communication,
  simba.imagebox, simba.client,simba.jsonparser, simba.xmlparser,
  simba.finder, simba.target, simba.fontloader, simba.ocr,
  simba.ocrutil, simba.helpers_matrix, simba.nativeinterface,
  simba.generics_array, simba.target_window,

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
    Options := Options + [lcoLooseSemicolon, lcoAutoInvoke, lcoExplictSelf, lcoAutoObjectify];

    InitializeAddOnTerminate(Self);
    InitializeWaitUntil(Self);
    InitializeFFI(Self);

    addGlobalType('type Pointer', 'TClient');
    addGlobalVar('TClient', nil, 'Client'); // Will be assigned later

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

procedure TSimbaScript_Compiler.InitBaseDefinitions;
begin
  addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).createCopy(), 'Byte');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).createCopy(), 'Integer');
  addGlobalType(getPointerType(ltChar, False).createCopy(), 'PChar');

  inherited InitBaseDefinitions();
end;

procedure TSimbaScript_Compiler.InitBaseDateTime;
begin
  addGlobalVar(HoursPerDay, 'HoursPerDay').isConstant := True;
  addGlobalVar(MinsPerHour, 'MinsPerHour').isConstant := True;
  addGlobalVar(SecsPerMin, 'SecsPerMin').isConstant := True;
  addGlobalVar(MSecsPerSec, 'MSecsPerSec').isConstant := True;
  addGlobalVar(MinsPerDay, 'MinsPerDay').isConstant := True;
  addGlobalVar(SecsPerDay, 'SecsPerDay').isConstant := True;
  addGlobalVar(MSecsPerDay, 'MSecsPerDay').isConstant := True;
  addGlobalVar(DateDelta, 'DateDelta').isConstant := True;

  addGlobalType(getBaseType(ltDouble).createCopy(True), 'TDateTime', False);

  addGlobalFunc('function EncodeDate(Year, Month, Day: UInt16): TDateTime;', @_LapeEncodeDate);
  addGlobalFunc('function EncodeTime(Hour, Min, Sec, MSec: UInt16): TDateTime;', @_LapeEncodeTime);
  addGlobalFunc('procedure DecodeDate(DateTime: TDateTime; var Year, Month, Day: UInt16);', @_LapeDecodeDate);
  addGlobalFunc('function DecodeDateFully(DateTime: TDateTime; var Year, Month, Day, DOW: UInt16): Boolean;', @_LapeDecodeDateFully);
  addGlobalFunc('procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: UInt16);', @_LapeDecodeTime);

  addGlobalFunc('function DateTimeToStr(const DateTime: TDateTime): string;', @_LapeDateTimeToStr);
  addGlobalFunc('function DateToStr(const DateTime: TDateTime): string;', @_LapeDateToStr);
  addGlobalFunc('function TimeToStr(const DateTime: TDateTime): string;', @_LapeTimeToStr);

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

  addGlobalFunc('function Date: TDateTime;', @_LapeDate);
  addGlobalFunc('function Time: TDateTime;', @_LapeTime);
  addGlobalFunc('function Now: TDateTime;', @_LapeNow);
  addGlobalFunc('function NowUTC: TDateTime;', @_LapeNowUTC);

  addGlobalFunc('procedure ReplaceTime(var DateTime: TDateTime; NewTime: TDateTime);', @_LapeReplaceTime);
  addGlobalFunc('procedure ReplaceDate(var DateTime: TDateTime; NewDate: TDateTime);', @_LapeReplaceDate);

  addGlobalFunc('function FormatDateTime(Format: string; DateTime: TDateTime): string;', @_LapeFormatDateTime);
  addGlobalFunc('function StrToDate(s: string): TDateTime;', @_LapeStrToDate);
  addGlobalFunc('function StrToDateDef(s: string; Default: TDateTime): TDateTime;', @_LapeStrToDateDef);
  addGlobalFunc('function StrToTime(s: string): TDateTime;', @_LapeStrToTime);
  addGlobalFunc('function StrToTimeDef(s: string; Default: TDateTime): TDateTime;', @_LapeStrToTimeDef);
  addGlobalFunc('function StrToDateTime(s: string): TDateTime;', @_LapeStrToDateTime);
  addGlobalFunc('function StrToDateTimeDef(s: string; Default: TDateTime): TDateTime;', @_LapeStrToDateTimeDef);

  addGlobalFunc('function DateTimeToUnix(const Value: TDateTime; InputIsUTC: Boolean = True): Int64;', @_LapeDateTimeToUnix);
  addGlobalFunc('function UnixToDateTime(const Value: Int64; ReturnUTC: Boolean = True): TDateTime;', @_LapeUnixToDateTime);
  addGlobalFunc('function UnixTime: Int64;', @_LapeUnixTime);

  addGlobalFunc('function YearsBetween(const ANow, AThen: TDateTime): Int32;', @_LapeYearsBetween);
  addGlobalFunc('function MonthsBetween(const ANow, AThen: TDateTime): Int32;', @_LapeMonthsBetween);
  addGlobalFunc('function WeeksBetween(const ANow, AThen: TDateTime): Int32;', @_LapeWeeksBetween);
  addGlobalFunc('function DaysBetween(const ANow, AThen: TDateTime): Int32;', @_LapeDaysBetween);
  addGlobalFunc('function HoursBetween(const ANow, AThen: TDateTime): Int64;', @_LapeHoursBetween);
  addGlobalFunc('function MinutesBetween(const ANow, AThen: TDateTime): Int64;', @_LapeMinutesBetween);
  addGlobalFunc('function SecondsBetween(const ANow, AThen: TDateTime): Int64;', @_LapeSecondsBetween);
  addGlobalFunc('function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;', @_LapeMilliSecondsBetween);

  addGlobalFunc('function IncYear(const Value: TDateTime; const NumberOfYears: Int32 = 1): TDateTime;', @_LapeIncYear);
  addGlobalFunc('function IncWeek(const Value: TDateTime; const NumberOfWeeks: Int32 = 1): TDateTime;', @_LapeIncWeek);
  addGlobalFunc('function IncDay(const Value: TDateTime; const NumberOfDays: Int32 = 1): TDateTime;', @_LapeIncDay);
  addGlobalFunc('function IncHour(const Value: TDateTime; const NumberOfHours: Int64 = 1): TDateTime;', @_LapeIncHour);
  addGlobalFunc('function IncMinute(const Value: TDateTime; const NumberOfMinutes: Int64 = 1): TDateTime;', @_LapeIncMinute);
  addGlobalFunc('function IncSecond(const Value: TDateTime; const NumberOfSeconds: Int64 = 1): TDateTime;', @_LapeIncSecond);
  addGlobalFunc('function IncMilliSecond(const Value: TDateTime; const NumberOfMilliSeconds: Int64 = 1): TDateTime;', @_LapeIncMilliSecond);
end;

procedure TSimbaScript_Compiler.InitBaseString;
begin
  addGlobalType('set of (rfReplaceAll, rfIgnoreCase)', 'TReplaceFlags');

  addGlobalFunc('function Pos(Needle, Haystack: String): SizeInt;', @_LapePos);
  addGlobalFunc('function UpperCase(S: String): String;', @_LapeUpperCase);
  addGlobalFunc('function LowerCase(S: String): String;', @_LapeLowerCase);
  addGlobalFunc('function UpCase(c: AnsiChar): AnsiChar; overload;', @_LapeUpCaseA);
  addGlobalFunc('function UpCase(c: WideChar): WideChar; overload;', @_LapeUpCaseW);
  addGlobalFunc('function CompareStr(s1, s2: String): Integer;', @_LapeCompareStr);
  addGlobalFunc('function CompareText(s1, s2: String): Integer;', @_LapeCompareText);
  addGlobalFunc('function SameText(s1, s2: String): EvalBool;', @_LapeSameText);
  addGlobalFunc('function Trim(S: String): String;', @_LapeTrim);
  addGlobalFunc('function TrimLeft(S: String): String;', @_LapeTrimLeft);
  addGlobalFunc('function TrimRight(S: String): String;', @_LapeTrimRight);
  addGlobalFunc('function PadL(S: String; Len: SizeInt; c: Char = " "): String;', @_LapePadL);
  addGlobalFunc('function PadR(S: String; Len: SizeInt; c: Char = " "): String;', @_LapePadR);
  addGlobalFunc('function QuotedStr(S: String): String;', @_LapeQuotedStr);
  addGlobalFunc('function IntToHex(Value: Int64; Digits: Integer = 1): String; overload;', @_LapeIntToHex);
  addGlobalFunc('function IntToHex(Value: UInt64; Digits: Integer = 1): String; overload;', @_LapeUIntToHex);
  addGlobalFunc('function IntToStr(i: Int64): String; overload;', @_LapeToString_Int64);
  addGlobalFunc('function IntToStr(i: UInt64): String; overload;', @_LapeToString_UInt64);
  addGlobalFunc('function StrToInt(S: String): Integer;', @_LapeStrToInt);
  addGlobalFunc('function StrToIntDef(S: String; Def: Integer): Integer;', @_LapeStrToIntDef);
  addGlobalFunc('function StrToInt64(S: String): Int64;', @_LapeStrToInt64);
  addGlobalFunc('function StrToInt64Def(S: String; Def: Int64): Int64;', @_LapeStrToInt64Def);
  addGlobalFunc('function StrToUInt64(S: String): UInt64;', @_LapeStrToUInt64);
  addGlobalFunc('function StrToUInt64Def(S: String; Def: UInt64): UInt64;', @_LapeStrToUInt64Def);
  addGlobalFunc('function FloatToStr(f: Extended): String;', @_LapeToString_Extended);
  addGlobalFunc('function StrToFloat(S: String): Extended;', @_LapeStrToFloat);
  addGlobalFunc('function StrToFloatDef(S: String; Def: Extended): Extended;', @_LapeStrToFloatDef);
  addGlobalFunc('function CurrToStr(Value: Currency): String;', @_LapeToString_Currency);
  addGlobalFunc('function StrToCurr(S: String): Currency;', @_LapeStrToCurr);
  addGlobalFunc('function StrToCurrDef(S: String; Def: Currency): Currency;', @_LapeStrToCurrDef);
  addGlobalFunc('function StrToBool(S: String): EvalBool;', @_LapeStrToBool);
  addGlobalFunc('function BoolToStr(B: EvalBool; TrueS: String = "True"; FalseS: String = "False"): String;', @_LapeBoolToStr);
  addGlobalFunc('function StrToBoolDef(S: String; Default: EvalBool): EvalBool;', @_LapeStrToBoolDef);
  addGlobalFunc('function Format(Fmt: String; Args: array of Variant): String;', @_LapeFormat);
  addGlobalFunc('function FormatFloat(Format: String; Value: Extended): String;', @_LapeFormatFloat);
  addGlobalFunc('function FormatCurr(Format: String; Value: Currency): String;', @_LapeFormatCurr);
  addGlobalFunc('function LastDelimiter(Delimiters, S: String): SizeInt;', @_LapeLastDelimiter);
  addGlobalFunc('function StringReplace(S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;', @_LapeStringReplace);
  addGlobalFunc('Function IsDelimiter(Delimiters, S: String; Index: SizeInt): EvalBool;', @_LapeIsDelimiter);
  addGlobalFunc('function StringOfChar(c: Char; l: SizeInt): String;', @_LapeStringOfChar);

  addDelayedCode(LapeDelayedFlags +
    'function Chr(IntValue: UInt8):  AnsiChar; overload; begin Result := AnsiChar(IntValue); end;' + LineEnding +
    'function Chr(IntValue: UInt16): WideChar; overload; begin Result := WideChar(IntValue); end;',
    '!addDelayedString');
end;

procedure TSimbaScript_Compiler.InitBaseVariant;
begin
  addGlobalType('enum(VarUnknown, VarUnassigned, VarNull, VarInt8, VarInt16, VarInt32, VarInt64, VarUInt8, VarUInt16, VarUInt32, VarUInt64, VarSingle, VarDouble, VarDate, VarCurrency, VarBoolean, VarVariant, VarString, VarUnicodeString)', 'EVarType');

  addGlobalVar(Variants.Null, 'Null').isConstant := True;
  addGlobalVar(Variants.Unassigned, 'Unassigned').isConstant := True;

  addGlobalFunc('function VarType(const V: Variant): EVarType;', @_LapeVarType);
  addGlobalFunc('function VarIsOrdinal(const V: Variant): EvalBool;', @_LapeVarIsOrdinal);
  addGlobalFunc('function VarIsFloat(const V: Variant): EvalBool;', @_LapeVarIsFloat);
  addGlobalFunc('function VarIsNumeric(const V: Variant): EvalBool;', @_LapeVarIsNumeric);
  addGlobalFunc('function VarIsStr(const V: Variant): EvalBool;', @_LapeVarIsStr);
end;

function TSimbaScript_Compiler.getIntegerArray: TLapeType;
begin
  Result := getGlobalType('TIntegerArray');
  if (Result = nil) then
    Result := inherited;
end;

function TSimbaScript_Compiler.getFloatArray: TLapeType;
begin
  Result := getGlobalType('TExtendedArray');
  if (Result = nil) then
    Result := inherited;
end;

procedure TSimbaScript_Compiler.pushTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  inherited pushTokenizer(ATokenizer);
end;

procedure TSimbaScript_Compiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

procedure TSimbaScript_Compiler.addDelayedCode(Code: array of lpString; AFileName: lpString);
begin
  addDelayedCode(''.Join(LineEnding, Code), AFileName);
end;

end.

