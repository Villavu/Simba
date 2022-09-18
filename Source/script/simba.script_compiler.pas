{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_compiler;

{$i simba.inc}

interface

uses
  classes, sysutils, typinfo, variants,
  ffi, lpffi, lpcompiler, lptypes, lpvartypes, lpparser, lptree, lpffiwrappers, lpinterpreter, lpeval,
  simba.mufasatypes;

type
  TSimbaScript_Compiler = class;
  TSimbaImport = procedure(Compiler: TSimbaScript_Compiler);
  TSimbaImportArray = array of TSimbaImport;

  TSimbaScript_Compiler = class(TLapeCompiler)
  public class var
    Imports: TSimbaImportArray;

    class procedure RegisterImport(Proc: TSimbaImport);
  public type
    TManagedImportClosure = class(TLapeDeclaration)
      Closure: TImportClosure;
    end;
  protected
    FSectionStack: TStringArray;

    function Section: String;

    procedure InitBaseDefinitions; override;
    procedure InitBaseDateTime; override;
    procedure InitBaseVariant; override;
  public
    function getIntegerArray: TLapeType; override;
    function getFloatArray: TLapeType; override;

    function CurrentDir: String;

    procedure pushSection(Name: String);
    procedure popSection;

    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); reintroduce;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); reintroduce;

    procedure addDelayedCode(Code: TStringArray; AFileName: lpString = ''); virtual; overload;

    function addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method; virtual; overload;
    function addGlobalFunc(Header: lpString; Value: Pointer; ABI: TFFIABI): TLapeGlobalVar; virtual; overload;
    function addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType; virtual; overload;

    function addCallbackType(Str: String): TLapeType;

    procedure addClass(Name: lpString; Parent: lpString = 'TObject'); virtual;
    procedure addClassVar(Obj, Item, Typ: lpString; ARead: Pointer; AWrite: Pointer = nil; Arr: Boolean = False; ArrType: lpString = 'Integer'); virtual;

    procedure Import; virtual;
    function Compile: Boolean; override;

    procedure InvokeProc(Name: String);
    procedure InvokeProcFFI(Name: String);

    constructor Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean=True; AEmitter: TLapeCodeEmitter=nil; ManageEmitter: Boolean=True); reintroduce; override;
    destructor Destroy; override;
  end;

implementation

uses
  simba.import_system,
  simba.import_matrix,
  simba.import_box, simba.import_boxarray, simba.import_point,

  // LCL
  simba.import_lcl_system, simba.import_lcl_graphics, simba.import_lcl_controls,
  simba.import_lcl_form, simba.import_lcl_stdctrls, simba.import_lcl_extctrls,
  simba.import_lcl_comctrls, simba.import_lcl_misc,

  // Simba classes
  simba.import_class_bitmap, simba.import_class_dtm, simba.import_class_dtms,
  simba.import_class_finder, simba.import_class_font, simba.import_class_fonts,
  simba.import_class_ocr, simba.import_class_target, simba.import_class_iomanager,
  simba.import_class_client, simba.import_class_xml, simba.import_class_json,
  simba.import_class_imagebox, simba.import_class_shapebox,

  // Simba
  simba.import_timing, simba.import_algorithms, simba.import_colormath,
  simba.import_crypto, simba.import_windowhandle, simba.import_debugimage,
  simba.import_dialogs, simba.import_dtm, simba.import_file, simba.import_internal,
  simba.import_keyboard, simba.import_matchtemplate, simba.import_finder,
  simba.import_math, simba.import_mouse, simba.import_ocr, simba.import_other,
  simba.import_process, simba.import_script, simba.import_slacktree,
  simba.import_string, simba.import_internet, simba.import_target, simba.import_variant,
  simba.import_simba,

  simba.script_compiler_waituntil;

function TSimbaScript_Compiler.addGlobalFunc(Header: lpString; Body: TStringArray): TLapeTree_Method;
var
  OldState: Pointer;
begin
  Result := nil;

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
    Result := inherited addGlobalFunc(Header, Closure.Func);
end;

function TSimbaScript_Compiler.addGlobalType(Str: lpString; AName: lpString; ABI: TFFIABI): TLapeType;
begin
  Result := addGlobalType(Format('native(type %s, %s)', [Str, GetEnumName(TypeInfo(TFFIABI), Ord(ABI))]), AName);
end;

function TSimbaScript_Compiler.addCallbackType(Str: String): TLapeType;
begin
  Result := addGlobalType(Str.After('='), Str.Before('='), FFI_DEFAULT_ABI);
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
    Options := Options + [lcoLooseSemicolon, lcoAutoInvoke, lcoExplicitSelf, lcoAutoObjectify];

    InitializeWaitUntil(Self);
    InitializeFFI(Self);

    addGlobalType('type Pointer', 'TClient');
    addGlobalVar('TClient', nil, 'Client'); // Will be assigned later

    for Proc in Imports do
      Proc(Self);
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

procedure TSimbaScript_Compiler.InvokeProc(Name: String);
var
  Method: TLapeGlobalVar;
begin
  Method := Globals[Name];
  if (Method <> nil) then
    RunCode(FEmitter.Code, FEmitter.CodeLen, [], PCodePos(Method.Ptr)^);
end;

procedure TSimbaScript_Compiler.InvokeProcFFI(Name: String);
var
  Method: TLapeGlobalVar;
  Closure: TExportClosure;
begin
  Method := Globals[Name];

  if (Method <> nil) then
  try
    Closure := LapeExportWrapper(Method);

    TProcedure(Closure.Func)();
  finally
    Closure.Free();
  end;
end;

constructor TSimbaScript_Compiler.Create(ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean; AEmitter: TLapeCodeEmitter; ManageEmitter: Boolean);
begin
  inherited Create(ATokenizer, ManageTokenizer, AEmitter, ManageEmitter);

  pushSection('!Simba');
end;

destructor TSimbaScript_Compiler.Destroy;
begin
  popSection();

  inherited Destroy();
end;

class procedure TSimbaScript_Compiler.RegisterImport(Proc: TSimbaImport);
begin
  Imports += [Proc];
end;

function TSimbaScript_Compiler.CurrentDir: String;
begin
  Result := '';
  if (Tokenizer <> nil) then
    Result := ExtractFileDir(Tokenizer.FileName);
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

procedure TSimbaScript_Compiler.InitBaseVariant;
begin
  { nothing }
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

procedure TSimbaScript_Compiler.addDelayedCode(Code: TStringArray; AFileName: lpString);
begin
  addDelayedCode(LapeDelayedFlags + LineEnding.Join(Code), AFileName);
end;

end.

