{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	General basetypes and objects.
}
unit lptypes;

{$I lape.inc}

interface

uses
  Classes, SysUtils, IniFiles;

type
  UInt8 = Byte;
  Int8 = ShortInt;
  UInt16 = Word;
  Int16 = SmallInt;
  UInt32 = LongWord;
  Int32 = LongInt;
  //UInt64 = QWord;    Already defined
  //Int64 = Int64;     Already defined

  PUInt8 = ^UInt8;
  PInt8 = ^Int8;
  PUInt16 = ^UInt16;
  PInt16 = ^Int16;
  PUInt32 = ^UInt32;
  PInt32 = ^Int32;
  PUInt64 = ^UInt64;
  //PInt64 = ^Int64;   Already defined

  {$IFDEF FPC}
  NativeInt = PtrInt;
  NativeUInt = PtrUInt;
  {$ELSE}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  PLongBool = ^LongBool;
  {$ENDIF}

  {$IFDEF Lape_Unicode}
  lpString = UnicodeString;
  lpChar = WideChar;
  lpCharInt = UInt16;
  {$ELSE}
  lpString = ansistring;
  lpChar = AnsiChar;
  lpCharInt = UInt8;
  {$ENDIF}
  PlpString = ^lpString;
  PlpChar = ^lpChar;
  PlpCharInt = ^lpCharInt;

  TStringArray = array of lpString;

  PCodeArray = ^TCodeArray;
  TCodeArray = array of Byte;

  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

  PDocPos = ^TDocPos;
  TDocPos = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Line, Col: {$IFDEF Lape_SmallCode}UInt16{$ELSE}UInt32{$ENDIF};
    FileName: lpString;
  end;

  TLapeRange = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Lo, Hi: Int64;
  end;

  TCodePos = NativeUInt;
  TCodeOffset = NativeInt;

  {$IFDEF Lape_SmallCode}
  //Means Lape can only locate up to 65kb of local variables (per stackframe)
  TStackInc = Int16;
  TVarStackOffset = UInt16;
  TStackOffset = UInt16;
  TPointerOffset = Int16;
  TParamSize = UInt16;
  EvalBool = Boolean;
  {$ELSE}
  TStackInc = NativeInt;
  TVarStackOffset = NativeUInt;
  TStackOffset = NativeUInt;
  TPointerOffset = NativeInt;
  TParamSize = NativeUInt;
  EvalBool = LongBool;
  {$ENDIF}

  PStackOffset = ^TStackOffset;
  PParamSize = ^TParamSize;
  PCodePos = ^TCodePos;
  PCodeOffset = ^TCodeOffset;
  PEvalBool = ^EvalBool;

  EMemoryPos = (mpNone, mpStack, mpMem, mpVar);
  TLapeEvalProc = procedure(const Dest, Left, Right: Pointer);
  TLapeImportedProc = procedure(const Params: PParamArray);
  TLapeImportedFunc = procedure(const Params: PParamArray; const Result: Pointer);

  ELapeBaseType = (
    ltUnknown,
    ltUInt8, ltInt8, ltUInt16, ltInt16, ltUInt32, ltInt32, ltUInt64, ltInt64, //Integer
    ltSingle, ltDouble, ltCurrency, ltExtended,                               //Real
    ltBoolean, ltByteBool, ltWordBool, ltLongBool,                            //Boolean
    ltAnsiChar, ltWideChar,                                                   //Char
    ltShortString, ltAnsiString, ltWideString, ltUnicodeString,               //String
    ltVariant,                                                                //Variant
    ltSmallEnum, ltLargeEnum, ltSmallSet, ltLargeSet,                         //Set
    ltPointer,                                                                //Pointer
    ltRecord, ltUnion,                                                        //Struct
    ltDynArray, ltStaticArray,                                                //Array
    ltScriptMethod, ltImportedMethod                                          //Methods
  );
  LapeIntegerTypeRange = ltUInt8..ltInt64;

  EOperatorAssociative = (assocNone, assocLeft, assocRight);
  EOperator = (
    op_Unknown,

    //Same order as lpparser.EParserToken
    op_cmp_Equal,
    op_cmp_GreaterThan,
    op_cmp_GreaterThanOrEqual,
    op_cmp_LessThan,
    op_cmp_LessThanOrEqual,
    op_cmp_NotEqual,

    op_Addr,
    op_AND,
    op_Assign,
    op_Deref,
    op_DIV,
    op_Divide,
    op_Dot,
    op_IN,
    op_Index,
    op_Minus,
    op_MOD,
    op_Multiply,
    op_NOT,
    op_OR,
    op_Plus,
    op_Power,
    op_SHL,
    op_SHR,
    op_XOR,

    //Extra
    op_UnaryMinus,
    op_UnaryPlus
  );

  ELapeSmallEnum = (__LapeSmallEnum1,__LapeSmallEnum2,__LapeSmallEnum3,__LapeSmallEnum4,__LapeSmallEnum5,__LapeSmallEnum6,__LapeSmallEnum7,__LapeSmallEnum8,__LapeSmallEnum9,__LapeSmallEnum10,__LapeSmallEnum11,__LapeSmallEnum12,__LapeSmallEnum13,__LapeSmallEnum14,__LapeSmallEnum15,__LapeSmallEnum16,__LapeSmallEnum17,__LapeSmallEnum18,__LapeSmallEnum19,__LapeSmallEnum20,__LapeSmallEnum21,__LapeSmallEnum22,__LapeSmallEnum23,__LapeSmallEnum24,__LapeSmallEnum25,__LapeSmallEnum26,__LapeSmallEnum27,__LapeSmallEnum28,__LapeSmallEnum29,__LapeSmallEnum30,__LapeSmallEnum31,__LapeSmallEnum32);
  ELapeLargeEnum = (__LapeLargeEnum1,__LapeLargeEnum2,__LapeLargeEnum3,__LapeLargeEnum4,__LapeLargeEnum5,__LapeLargeEnum6,__LapeLargeEnum7,__LapeLargeEnum8,__LapeLargeEnum9,__LapeLargeEnum10,__LapeLargeEnum11,__LapeLargeEnum12,__LapeLargeEnum13,__LapeLargeEnum14,__LapeLargeEnum15,__LapeLargeEnum16,__LapeLargeEnum17,__LapeLargeEnum18,__LapeLargeEnum19,__LapeLargeEnum20,__LapeLargeEnum21,__LapeLargeEnum22,__LapeLargeEnum23,__LapeLargeEnum24,__LapeLargeEnum25,__LapeLargeEnum26,__LapeLargeEnum27,__LapeLargeEnum28,__LapeLargeEnum29,__LapeLargeEnum30,__LapeLargeEnum31,__LapeLargeEnum32,__LapeLargeEnum33,__LapeLargeEnum34,__LapeLargeEnum35,__LapeLargeEnum36,__LapeLargeEnum37,__LapeLargeEnum38,__LapeLargeEnum39,__LapeLargeEnum40,__LapeLargeEnum41,__LapeLargeEnum42,__LapeLargeEnum43,__LapeLargeEnum44,__LapeLargeEnum45,__LapeLargeEnum46,__LapeLargeEnum47,__LapeLargeEnum48,__LapeLargeEnum49,__LapeLargeEnum50,
                    __LapeLargeEnum51,__LapeLargeEnum52,__LapeLargeEnum53,__LapeLargeEnum54,__LapeLargeEnum55,__LapeLargeEnum56,__LapeLargeEnum57,__LapeLargeEnum58,__LapeLargeEnum59,__LapeLargeEnum60,__LapeLargeEnum61,__LapeLargeEnum62,__LapeLargeEnum63,__LapeLargeEnum64,__LapeLargeEnum65,__LapeLargeEnum66,__LapeLargeEnum67,__LapeLargeEnum68,__LapeLargeEnum69,__LapeLargeEnum70,__LapeLargeEnum71,__LapeLargeEnum72,__LapeLargeEnum73,__LapeLargeEnum74,__LapeLargeEnum75,__LapeLargeEnum76,__LapeLargeEnum77,__LapeLargeEnum78,__LapeLargeEnum79,__LapeLargeEnum80,__LapeLargeEnum81,__LapeLargeEnum82,__LapeLargeEnum83,__LapeLargeEnum84,__LapeLargeEnum85,__LapeLargeEnum86,__LapeLargeEnum87,__LapeLargeEnum88,__LapeLargeEnum89,__LapeLargeEnum90,__LapeLargeEnum91,__LapeLargeEnum92,__LapeLargeEnum93,__LapeLargeEnum94,__LapeLargeEnum95,__LapeLargeEnum96,__LapeLargeEnum97,__LapeLargeEnum98,__LapeLargeEnum99,__LapeLargeEnum100,
                    __LapeLargeEnum101,__LapeLargeEnum102,__LapeLargeEnum103,__LapeLargeEnum104,__LapeLargeEnum105,__LapeLargeEnum106,__LapeLargeEnum107,__LapeLargeEnum108,__LapeLargeEnum109,__LapeLargeEnum110,__LapeLargeEnum111,__LapeLargeEnum112,__LapeLargeEnum113,__LapeLargeEnum114,__LapeLargeEnum115,__LapeLargeEnum116,__LapeLargeEnum117,__LapeLargeEnum118,__LapeLargeEnum119,__LapeLargeEnum120,__LapeLargeEnum121,__LapeLargeEnum122,__LapeLargeEnum123,__LapeLargeEnum124,__LapeLargeEnum125,__LapeLargeEnum126,__LapeLargeEnum127,__LapeLargeEnum128,__LapeLargeEnum129,__LapeLargeEnum130,__LapeLargeEnum131,__LapeLargeEnum132,__LapeLargeEnum133,__LapeLargeEnum134,__LapeLargeEnum135,__LapeLargeEnum136,__LapeLargeEnum137,__LapeLargeEnum138,__LapeLargeEnum139,__LapeLargeEnum140,__LapeLargeEnum141,__LapeLargeEnum142,__LapeLargeEnum143,__LapeLargeEnum144,__LapeLargeEnum145,__LapeLargeEnum146,__LapeLargeEnum147,__LapeLargeEnum148,__LapeLargeEnum149,__LapeLargeEnum150,
                    __LapeLargeEnum151,__LapeLargeEnum152,__LapeLargeEnum153,__LapeLargeEnum154,__LapeLargeEnum155,__LapeLargeEnum156,__LapeLargeEnum157,__LapeLargeEnum158,__LapeLargeEnum159,__LapeLargeEnum160,__LapeLargeEnum161,__LapeLargeEnum162,__LapeLargeEnum163,__LapeLargeEnum164,__LapeLargeEnum165,__LapeLargeEnum166,__LapeLargeEnum167,__LapeLargeEnum168,__LapeLargeEnum169,__LapeLargeEnum170,__LapeLargeEnum171,__LapeLargeEnum172,__LapeLargeEnum173,__LapeLargeEnum174,__LapeLargeEnum175,__LapeLargeEnum176,__LapeLargeEnum177,__LapeLargeEnum178,__LapeLargeEnum179,__LapeLargeEnum180,__LapeLargeEnum181,__LapeLargeEnum182,__LapeLargeEnum183,__LapeLargeEnum184,__LapeLargeEnum185,__LapeLargeEnum186,__LapeLargeEnum187,__LapeLargeEnum188,__LapeLargeEnum189,__LapeLargeEnum190,__LapeLargeEnum191,__LapeLargeEnum192,__LapeLargeEnum193,__LapeLargeEnum194,__LapeLargeEnum195,__LapeLargeEnum196,__LapeLargeEnum197,__LapeLargeEnum198,__LapeLargeEnum199,__LapeLargeEnum200,
                    __LapeLargeEnum201,__LapeLargeEnum202,__LapeLargeEnum203,__LapeLargeEnum204,__LapeLargeEnum205,__LapeLargeEnum206,__LapeLargeEnum207,__LapeLargeEnum208,__LapeLargeEnum209,__LapeLargeEnum210,__LapeLargeEnum211,__LapeLargeEnum212,__LapeLargeEnum213,__LapeLargeEnum214,__LapeLargeEnum215,__LapeLargeEnum216,__LapeLargeEnum217,__LapeLargeEnum218,__LapeLargeEnum219,__LapeLargeEnum220,__LapeLargeEnum221,__LapeLargeEnum222,__LapeLargeEnum223,__LapeLargeEnum224,__LapeLargeEnum225,__LapeLargeEnum226,__LapeLargeEnum227,__LapeLargeEnum228,__LapeLargeEnum229,__LapeLargeEnum230,__LapeLargeEnum231,__LapeLargeEnum232,__LapeLargeEnum233,__LapeLargeEnum234,__LapeLargeEnum235,__LapeLargeEnum236,__LapeLargeEnum237,__LapeLargeEnum238,__LapeLargeEnum239,__LapeLargeEnum240,__LapeLargeEnum241,__LapeLargeEnum242,__LapeLargeEnum243,__LapeLargeEnum244,__LapeLargeEnum245,__LapeLargeEnum246,__LapeLargeEnum247,__LapeLargeEnum248,__LapeLargeEnum249,__LapeLargeEnum250,
                    __LapeLargeEnum251,__LapeLargeEnum252,__LapeLargeEnum253,__LapeLargeEnum254,__LapeLargeEnum255);
  TLapeSmallSet = set of ELapeSmallEnum;
  TLapeLargeSet = set of ELapeLargeEnum;

  PLapeSmallEnum = ^ELapeSmallEnum;
  PLapeLargeEnum = ^ELapeLargeEnum;
  PLapeSmallSet = ^TLapeSmallSet;
  PLapeLargeSet = ^TLapeLargeSet;

  TLapeBaseClass = class(TObject, IUnknown)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create; virtual;
    {$IFDEF Lape_TrackObjects}
    destructor Destroy; override;
    {$ENDIF}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  end;

  TLapeBaseDeclClass = class(TLapeBaseClass)
  protected
    function getDocPos: TDocPos; virtual; abstract;
  public
    property DocPos: TDocPos read getDocPos;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeStack<_T> = class(TLapeBaseClass)
  public type
    TTArray = array of _T;
  var protected
    FArr: TTArray;
    FLen: Integer;
    FCur: Integer;

    procedure Grow(AGrowSize: Integer); virtual;
    procedure CheckIndex(Index: Integer; GrowIfNeeded: Boolean = False); virtual;
    function getItem(Index: Integer): _T; virtual;
    function getCurItem: _T; virtual;
    procedure setCurItem(Item: _T); virtual;
    function getCount: Integer; virtual;
  public
    GrowSize: Word;

    constructor Create(StartBufLen: Cardinal = 32); reintroduce; virtual;
    procedure Reset; virtual;
    function Pop: _T; virtual;
    function Push(Item: _T): Integer; virtual;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem; default;
    property Top: _T read getCurItem write setCurItem;
    property Size: Integer read FLen;
    property Count: Integer read getCount;
    property Cur: Integer read FCur;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeList<_T> = class(TLapeBaseClass)
  public type
    TTArray = array of _T;
  var protected
    FDuplicates: TDuplicates;
    FItems: TTArray;
    FLen: Integer;

    function getItem(Index: Integer): _T; virtual;
    procedure setItem(Index: Integer; Item: _T); virtual;
  public
    InvalidVal: _T;

    constructor Create(InvalidValue: _T; Duplicates: TDuplicates); reintroduce; virtual;
    procedure Clear; virtual;

    function add(Item: _T): Integer; virtual;
    function Delete(Index: Integer): _T; overload; virtual;
    function DeleteItem(Item: _T): _T; overload; virtual;
    function IndexOf(Item: _T): Integer; overload; virtual;
    function ExistsItem(Item: _T): Boolean; overload;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem write setItem; default;
    property Count: Integer read FLen;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeStringMap<_T> = class(TLapeBaseClass)
  protected
    FStringList: THashedStringList;
    FItems: array of _T;
    FLen: Integer;

    function getItem(Key: lpString): _T; virtual;
    procedure setItem(Key: lpString; Item: _T); virtual;
    function getItemI(Index: Integer): _T; virtual;
    procedure setItemI(Index: Integer; Item: _T); virtual;
    function getIndex(Index: Integer): lpString; virtual;
  public
    InvalidVal: _T;

    constructor Create(InvalidValue: _T; CaseSensitive: Boolean = {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF}; Duplicates: TDuplicates = dupError); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure add(Key: lpString; Item: _T); virtual;
    function Delete(Key: lpString): _T; overload; virtual;
    function Delete(Index: Integer): _T; overload; virtual;
    function DeleteItem(Item: _T): _T; overload; virtual;
    function IndexOf(Item: _T): lpString; overload; virtual;
    function IndexOf(Key: lpString): Integer; overload; virtual;
    function ExistsItem(Item: _T): Boolean; overload;
    function ExistsKey(Key: lpString): Boolean; overload;

    property Items[Index: lpString]: _T read getItem write setItem; default;
    property ItemsI[Index: Integer]: _T read getItemI write setItemI;
    property Key[Index: Integer]: lpString read getIndex;
    property Count: Integer read FLen;
  end;

  TLapeDeclaration = class;
  TLapeDeclarationClass = class of TLapeDeclaration;
  TLapeDeclArray = array of TLapeDeclaration;
  TLapeDeclCollection = class({$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeDeclaration>); //Needs class() for Delphi support!!

  TLapeDeclarationList = class(TLapeBaseClass)
  protected
    FList: TLapeDeclCollection;
  public
    FreeDecls: Boolean;

    constructor Create(AList: TLapeDeclCollection; ManageDeclarations: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    function addDeclaration(d: TLapeDeclaration): Integer; virtual;
    function getByName(AName: lpString): TLapeDeclArray; virtual;
    function getByClass(AClass: TLapeDeclarationClass): TLapeDeclArray; virtual;
    procedure Delete(d: TLapeDeclaration; DoFree: Boolean = False); overload; virtual;
    procedure Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False); overload; virtual;

    property Items: TLapeDeclCollection read FList;
  end;

  TLapeDeclaration = class(TLapeBaseDeclClass)
  protected
    FList: TLapeDeclarationList;
    function getDocPos: TDocPos; override;
    procedure setList(AList: TLapeDeclarationList); virtual;
  public
    _DocPos: TDocPos;
    Name: lpString;
    Used: Boolean;
    constructor Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    destructor Destroy; override;

    property DeclarationList: TLapeDeclarationList read FList write setList;
  end;

const
  op_Invoke = op_Index;

  {$IFDEF Lape_Unicode}
  ltString = ltUnicodeString;
  ltChar = ltWideChar;
  ltCharInt = ltUInt16;
  {$ELSE}
  ltString = ltAnsiString;
  ltChar = ltAnsiChar;
  ltCharInt = ltUInt8;
  {$ENDIF}

  {$IFDEF Lape_SmallCode}
  ltEvalBool = ltBoolean;
  {$ELSE}
  ltEvalBool = ltLongBool;
  {$ENDIF}

  LapeTypeSize: array[ELapeBaseType] of Integer = (
    -1,
    SizeOf(UInt8), SizeOf(Int8), SizeOf(UInt16), SizeOf(Int16), SizeOf(UInt32),
    SizeOf(Int32), SizeOf(UInt64), SizeOf(Int64),
    SizeOf(Single), SizeOf(Double), SizeOf(Currency), SizeOf(Extended),
    SizeOf(Boolean), SizeOf(ByteBool), SizeOf(WordBool), SizeOf(LongBool),
    SizeOf(AnsiChar), SizeOf(WideChar),
    SizeOf(ShortString), SizeOf(AnsiString), SizeOf(WideString), SizeOf(UnicodeString),
    SizeOf(Variant),
    SizeOf(ELapeSmallEnum), SizeOf(ELapeLargeEnum), SizeOf(TLapeSmallSet), SizeOf(TLapeLargeSet),
    SizeOf(Pointer),
    -1, -1,
    SizeOf(Pointer), -1,
    SizeOf(TCodePos), SizeOf(Pointer)
  );

  {$IF SizeOf(TCodePos) > SizeOf(Pointer)}
    {$MESSAGE Fatal 'TCodePos should be <= Pointer for universal methods'}
  {$IFEND}

  LapeIntegerTypes = [Low(LapeIntegerTypeRange)..High(LapeIntegerTypeRange)];
  LapeRealTypes = [ltSingle..ltExtended];
  LapeBoolTypes = [ltBoolean..ltLongBool];
  LapeStringTypes = [ltShortString..ltUnicodeString];
  LapeCharTypes = [ltAnsiChar..ltWideChar];
  LapeEnumTypes = [ltSmallEnum..ltLargeEnum, ltBoolean];
  LapeSetTypes = [ltSmallSet..ltLargeSet];
  LapeArrayTypes = [ltDynArray..ltStaticArray] + LapeStringTypes;
  LapeStructTypes = [ltRecord..ltUnion];
  LapeOrdinalTypes = LapeIntegerTypes + LapeBoolTypes + LapeCharTypes + LapeEnumTypes;
  LapePointerTypes = [ltPointer, ltDynArray, ltScriptMethod, ltImportedMethod] + LapeStringTypes - [ltShortString];
  LapeStackTypes = LapeOrdinalTypes + LapeRealTypes + LapeSetTypes + [ltShortString];
  LapeIfTypes = LapeOrdinalTypes + LapeStringTypes + LapePointerTypes + LapeRealTypes + [ltVariant];
  LapeNoInitTypes = LapeOrdinalTypes + LapeRealTypes + [ltPointer, ltScriptMethod, ltImportedMethod, ltShortString];

  NullDocPos: TDocPos = (Line: 0; Col: 0; FileName: '');
  NullRange: TLapeRange = (Lo: 0; Hi: 0);

  UnaryOperators = [op_Addr, op_Deref, op_NOT, op_UnaryMinus, op_UnaryPlus];
  BinaryOperators = [op_AND, op_NOT, op_OR, op_XOR];
  CompareOperators = [op_cmp_Equal, op_cmp_GreaterThan, op_cmp_GreaterThanOrEqual, op_cmp_LessThan, op_cmp_LessThanOrEqual, op_cmp_NotEqual];
  EnumOperators = [op_Plus, op_Minus, op_Assign] + CompareOperators;

  op_str: array[EOperator] of lpString = ('',
    '=', '>', '>=', '<', '<=', '<>', '@', 'and', ':=', '^', 'div', '/', '.' , 'in',
    '[', '-', 'mod', '*', 'not', 'or', '+', '**', 'shl', 'shr', 'xor', '-', '+');
  op_name: array[EOperator] of lpString = ('',
    'EQ', 'GT', 'GTEQ', 'LT', 'LTEQ', 'NEQ', {'ADDR'}'', 'AND', 'ASGN', {'DREF'}'', 'IDIV', 'DIV', {'dot'}'', 'IN',
    {'index'}'', 'SUB', 'MOD', 'MUL', 'NOT', 'OR', 'ADD', {'power'}'', 'SHL', 'SHR', 'XOR', 'UMIN', {'UPOS'}'');

var
  lowUInt8: UInt8 = Low(UInt8);    highUInt8: UInt8 = High(UInt8);
  lowInt8: Int8 = Low(Int8);       highInt8: Int8 = High(Int8);
  lowUInt16: UInt16 = Low(UInt16); highUInt16: UInt16 = High(UInt16);
  lowInt16: Int16 = Low(Int16);    highInt16: Int16 = High(Int16);
  lowUInt32: UInt32 = Low(UInt32); highUInt32: UInt32 = High(UInt32);
  lowInt32: Int32 = Low(Int32);    highInt32: Int32 = High(Int32);
  lowUInt64: UInt64 = Low(UInt64); highUInt64: UInt64 = High(UInt64);
  lowInt64: Int64 = Low(Int64);    highInt64: Int64 = High(Int64);

  LapeTypeLow: array[LapeIntegerTypeRange] of Pointer = (
    @lowUInt8, @lowInt8, @lowUInt16, @lowInt16, @lowUInt32, @lowInt32, @lowUInt64, @lowInt64
  );

  LapeTypeHigh: array[LapeIntegerTypeRange] of Pointer = (
    @highUInt8, @highInt8, @highUInt16, @highInt16, @highUInt32, @highInt32, @highUInt64, @highInt64
  );

function LapeCase(const Str: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeTypeToString(Token: ELapeBaseType): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeOperatorToString(Token: EOperator): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}

procedure Swap(var A, B: Pointer); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
procedure Swap(var A, B: Boolean); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
function _Compare8(Arr: PUInt8; Item: UInt8; Hi: Integer): Integer;
function _Compare16(Arr: PUInt16; Item: UInt16; Hi: Integer): Integer;
function _Compare32(Arr: PUInt32; Item: UInt32; Hi: Integer): Integer;
function _Compare64(Arr: PUInt64; Item: UInt64; Hi: Integer): Integer;

{$IFDEF Lape_TrackObjects}
var
  lpgCounter: Integer;
  lpgList: TList;
{$ENDIF}

implementation

uses
  typinfo,
  lpexceptions;

function LapeCase(const Str: lpString): lpString;
begin
  {$IFDEF Lape_CaseSensitive}
  Result := Str;
  {$ELSE}
  Result := LowerCase(Str);
  {$ENDIF}
end;

function LapeTypeToString(Token: ELapeBaseType): lpString;
begin
  Result := getEnumName(TypeInfo(ELapeBaseType), Ord(Token));
  Delete(Result, 1, 2);
end;

function LapeOperatorToString(Token: EOperator): lpString;
begin
  Result := getEnumName(TypeInfo(EOperator), Ord(Token));
  Delete(Result, 1, 3);
end;

procedure Swap(var A, B: Pointer);
var
  C: Pointer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Swap(var A, B: Boolean);
var
  C: Boolean;
begin
  C := A;
  A := B;
  B := C;
end;

function _Compare8(Arr: PUInt8; Item: UInt8; Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare16(Arr: PUInt16; Item: UInt16; Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare32(Arr: PUInt32; Item: UInt32; Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare64(Arr: PUInt64; Item: UInt64; Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function TLapeBaseClass._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TLapeBaseClass._Release: Integer; stdcall;
begin
  Result := -1;
end;

constructor TLapeBaseClass.Create;
begin
  inherited;
  {$IFDEF Lape_TrackObjects}
  Inc(lpgCounter);
  lpgList.add(Pointer(Self));
  if (Self is TLapeDeclaration) and (TLapeDeclaration(Self).Name <> '') then
    WriteLn('New(', ClassName, '::', TLapeDeclaration(Self).Name, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']')
  else
    WriteLn('New(', ClassName, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']');
  {$ENDIF}
end;

{$IFDEF Lape_TrackObjects}
destructor TLapeBaseClass.Destroy;
begin
  Dec(lpgCounter);
  lpgList.Delete(lpgList.IndexOf(Pointer(Self)));
  if (Self is TLapeDeclaration) and (TLapeDeclaration(Self).Name <> '') then
    WriteLn('Free(', ClassName, '::', TLapeDeclaration(Self).Name, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']')
  else
    WriteLn('Free(', ClassName, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']');
  inherited;
end;
{$ENDIF}

function TLapeBaseClass.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if getInterface(IId, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Grow(AGrowSize: Integer);
begin
  FLen := FLen + AGrowSize;
  if (FLen < 0) then
    FLen := 0;
  SetLength(FArr, FLen);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.CheckIndex(Index: Integer; GrowIfNeeded: Boolean = False);
var
  NeedGrow: Boolean;
begin
  NeedGrow := (Index >= FLen);
  if (NeedGrow and (not GrowIfNeeded)) or (Index < 0) then
    LapeException(lpeOutOfStackRange)
  else if NeedGrow then
    Grow(GrowSize);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Index: Integer): _T;
begin
  CheckIndex(Index);
  Result := FArr[Index];
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getCurItem: _T;
begin
  if (FCur >= 0) then
    Result := FArr[FCur]
  else
    CheckIndex(FCur);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.setCurItem(Item: _T);
begin
  if (FCur >= 0) then
    FArr[FCur] := Item
  else
    CheckIndex(FCur);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getCount: Integer;
begin
  if (FCur < 0) then
    Result := 0
  else
    Result := FCur + 1;
end;

constructor TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Create(StartBufLen: Cardinal = 32);
begin
  inherited Create();

  GrowSize := StartBufLen div 2;
  if (GrowSize < 1) then
    GrowSize := 1;

  Reset();
  Grow(StartBufLen);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Reset;
begin
  SetLength(FArr, 0);
  FLen := 0;
  FCur := -1;
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Pop: _T;
begin
  Result := getCurItem;
  Dec(FCur);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Push(Item: _T): Integer;
begin
  Inc(FCur);
  CheckIndex(FCur, True);
  FArr[FCur] := Item;
  Result := FCur;
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.ImportFromArray(Arr: TTArray);
begin
  FArr := Arr;
  FLen := Length(Arr);
  FCur := FLen - 1;
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArray: TTArray;
var
  i: Integer;
begin
  SetLength(Result, FCur + 1);
  for i := 0 to FCur do
    Result[i] := FArr[i];
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Index: Integer): _T;
begin
  if (Index > -1) and (Index < FLen) then
    Result := FItems[Index]
  else
    Result := InvalidVal;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.setItem(Index: Integer; Item: _T);
begin
  if (Index > -1) and (Index < FLen) then
    FItems[Index] := Item
  else
    LapeExceptionFmt(lpeInvalidIndex, [IntToStr(Index)]);
end;

constructor TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Create(InvalidValue: _T; Duplicates: TDuplicates);
begin
  inherited Create();

  InvalidVal := InvalidValue;
  FLen := 0;
  FDuplicates := Duplicates;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  SetLength(FItems, 0);
  FLen := 0;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.add(Item: _T): Integer;
begin
  if (FDuplicates in [dupIgnore, dupError]) and ExistsItem(Item) then
    if (FDuplicates = dupError) then
      LapeExceptionFmt(lpeDuplicateDeclaration, ['_T'])
    else
      Exit(-1);

  Result := FLen;
  Inc(FLen);
  SetLength(FItems, FLen);
  FItems[Result] := Item;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Index: Integer): _T;
var
  i: Integer;
  tmp: _T;
begin
  Result := InvalidVal;
  if (Index > -1) and (Index < FLen) then
  begin
    Result := FItems[Index];
    Dec(FLen);
    for i := Index to FLen - 1 do
    begin
      tmp := FItems[i];
      FItems[i] := FItems[i + 1];
      FItems[i + 1] := tmp;
    end;
    SetLength(FItems, FLen);
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [IntToStr(Index)]);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.DeleteItem(Item: _T): _T;
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := InvalidVal;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.IndexOf(Item: _T): Integer;
var
  i, ii: Integer;
  ItemA, ItemB: PByteArray;
  Match: Boolean;
begin
  case SizeOf(_T) of
    SizeOf(UInt8) : Result := _Compare8 (@FItems[0], PUInt8 (@Item)^, High(FItems));
    SizeOf(UInt16): Result := _Compare16(@FItems[0], PUInt16(@Item)^, High(FItems));
    SizeOf(UInt32): Result := _Compare32(@FItems[0], PUInt32(@Item)^, High(FItems));
    SizeOf(UInt64): Result := _Compare64(@FItems[0], PUInt64(@Item)^, High(FItems));
    else
    begin
      ItemB := PByteArray(@Item);
      for i := High(FItems) downto 0 do
      begin
        ItemA := PByteArray(@FItems[i]);
        Match := True;
        for ii := 0 to SizeOf(_T) - 1 do
          if (ItemA^[ii] <> ItemB^[ii]) then
          begin
            Match := False;
            Break;
          end;
        if Match then
          Exit(i);
      end;
      Result := -1;
    end;
  end;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.ExistsItem(Item: _T): Boolean;
begin
  Result := (IndexOf(Item) > -1);
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.ImportFromArray(Arr: TTArray);
begin
  FItems := Arr;
  FLen := Length(Arr);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArray: TTArray;
var
  i: Integer;
begin
  SetLength(Result, FLen);
  for i := 0 to FLen - 1 do
    Result[i] := FItems[i];
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Key: lpString): _T;
var
  Index: Integer;
begin
  if (not FStringList.CaseSensitive) then
    Key := UpperCase(Key);

  Index := FStringList.IndexOf(Key);
  if (Index > -1) and (Index < FLen) then
    Result := FItems[Index]
  else
    Result := InvalidVal;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setItem(Key: lpString; Item: _T);
var
  Index: Integer;
begin
  if (Key <> '') then
  begin
    if (not FStringList.CaseSensitive) then
      Key := UpperCase(Key);

    Index := FStringList.IndexOf(Key);
    if (Index > -1) and (Index < FLen) then
      FItems[Index] := Item
    else
      add(Key, Item);
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [Key]);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getItemI(Index: Integer): _T;
begin
  if (Index > -1) and (Index < FLen) then
    Result := FItems[Index]
  else
    Result := InvalidVal;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setItemI(Index: Integer; Item: _T);
begin
  if (Index > -1) and (Index < FLen) then
    FItems[Index] := Item
  else
    LapeExceptionFmt(lpeInvalidIndex, [IntToStr(Index)]);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getIndex(Index: Integer): lpString;
begin
  if (Index > -1) and (Index < FStringList.Count) then
    Result := FStringList[Index]
  else
    Result := '';
end;

constructor TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Create(InvalidValue: _T; CaseSensitive: Boolean = {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF}; Duplicates: TDuplicates = dupError);
begin
  inherited Create();

  InvalidVal := InvalidValue;
  FLen := 0;

  FStringList := THashedStringList.Create;
  FStringList.CaseSensitive := CaseSensitive;
  FStringList.Duplicates := Duplicates;
end;

destructor TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Destroy;
begin
  if (FStringList <> nil) then
    FStringList.Free;

  inherited;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FStringList.Clear;
  SetLength(FItems, 0);
  FLen := 0;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.add(Key: lpString; Item: _T);
var
  Index: Integer;
begin
  if (Key <> '') then
  begin
    if (not FStringList.CaseSensitive) then
      Key := UpperCase(Key);

    Index := FStringList.add(Key);
    if (Index > -1) then
    begin
      SetLength(FItems, FLen + 1);
      FItems[FLen] := Item;
      Inc(FLen);
    end;
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [Key]);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Key: lpString): _T;
var
  Index: Integer;
begin
  if (not FStringList.CaseSensitive) then
    Key := UpperCase(Key);

  Index := FStringList.IndexOf(Key);
  if (Index > -1) then
    Result := Delete(Index)
  else
  begin
    Result := InvalidVal;
    LapeExceptionFmt(lpeInvalidIndex, [Key]);
  end;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Index: Integer): _T;
var
  i: Integer;
  tmp: _T;
begin
  if (Index > -1) and (Index < FLen) then
  begin
    Result := FItems[Index];
    FStringList.Delete(Index);
    Dec(FLen);
    for i := Index to FLen - 1 do
    begin
      tmp := FItems[i];
      FItems[i] := FItems[i + 1];
      FItems[i + 1] := tmp;
    end;
    SetLength(FItems, FLen);
  end
  else
  begin
    Result := InvalidVal;
    LapeExceptionFmt(lpeInvalidIndex, [IntToStr(Index)]);
  end;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.DeleteItem(Item: _T): _T;
var
  Index: lpString;
begin
  Index := IndexOf(Item);
  if (Index <> '') then
    Result := Delete(Index)
  else
    Result := InvalidVal;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndexOf(Item: _T): lpString;
var
  i, ii, Index: Integer;
  ItemA, ItemB: PByteArray;
  Match: Boolean;
begin
  case SizeOf(_T) of
    SizeOf(UInt8) : Index := _Compare8 (@FItems[0], PUInt8 (@Item)^, High(FItems));
    SizeOf(UInt16): Index := _Compare16(@FItems[0], PUInt16(@Item)^, High(FItems));
    SizeOf(UInt32): Index := _Compare32(@FItems[0], PUInt32(@Item)^, High(FItems));
    SizeOf(UInt64): Index := _Compare64(@FItems[0], PUInt64(@Item)^, High(FItems));
    else
    begin
      ItemB := PByteArray(@Item);
      for i := High(FItems) downto 0 do
      begin
        ItemA := PByteArray(@FItems[i]);
        Match := True;
        for ii := 0 to SizeOf(_T) - 1 do
          if (ItemA^[ii] <> ItemB^[ii]) then
          begin
            Match := False;
            Break;
          end;
        if Match then
          Exit(FStringList[i]);
      end;
      Index := -1;;
    end;
  end;
  if (Index > -1) then
    Result := FStringList[i]
  else
    Result := '';
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndexOf(Key: lpString): Integer;
begin
  if (not FStringList.CaseSensitive) then
    Key := UpperCase(Key);
  Result := FStringList.IndexOf(Key);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.ExistsItem(Item: _T): Boolean;
begin
  Result := (IndexOf(Item) <> '');
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.ExistsKey(Key: lpString): Boolean;
begin
  if (not FStringList.CaseSensitive) then
    Key := UpperCase(Key);
  Result := (FStringList.IndexOf(Key) > -1);
end;

constructor TLapeDeclarationList.Create(AList: TLapeDeclCollection; ManageDeclarations: Boolean = True);
begin
  inherited Create();
  if (AList = nil) then
    AList := TLapeDeclCollection.Create(nil, dupAccept);
  FList := AList;
  FreeDecls := ManageDeclarations;
end;

destructor TLapeDeclarationList.Destroy;
begin
  Clear();
  FList.Free();
  inherited;
end;

procedure TLapeDeclarationList.Clear;
begin
  if (FList <> nil) then
  begin
    while (FList.Count > 0) do
      if (FList[0] = nil) or (not FreeDecls) then
        FList.Delete(0)
      else
        FList[0].Free();
    FList.Clear();
  end;
end;

function TLapeDeclarationList.addDeclaration(d: TLapeDeclaration): Integer;
begin
  if (FList <> nil) and (((d <> nil) and
     ((d.DeclarationList = nil) or (d.DeclarationList <> Self))) or
     (not FList.ExistsItem(d)))
  then
  begin
    Result := FList.add(d);
    if (d <> nil) then
      if (d.DeclarationList <> nil) then
        d.DeclarationList := Self
      else
        d.FList := Self;
  end
  else
    Result := -1;
end;

function TLapeDeclarationList.getByName(AName: lpString): TLapeDeclArray;
var
  i: Integer;
begin
  Result := nil;
  AName := LapeCase(AName);
  if (FList <> nil) then
    for i := 0 to FList.Count - 1 do
      if (FList[i] <> nil) and (LapeCase(FList[i].Name) = AName) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FList[i];
      end;
end;

function TLapeDeclarationList.getByClass(AClass: TLapeDeclarationClass): TLapeDeclArray;
var
  i: Integer;
begin
  Result := nil;
  if (FList <> nil) then
    for i := 0 to FList.Count - 1 do
      if (FList[i] <> nil) and (FList[i] is AClass) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FList[i];
      end;
end;

procedure TLapeDeclarationList.Delete(d: TLapeDeclaration; DoFree: Boolean = False);
begin
  if (FList <> nil) and (FList.DeleteItem(d) <> nil) then
    if DoFree then
      d.Free();
end;

procedure TLapeDeclarationList.Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False);
var
  ClassItems: TLapeDeclArray;
  i: Integer;
begin
  ClassItems := getByClass(AClass);
  for i := High(ClassItems) downto 0 do
    Delete(ClassItems[i], DoFree);
end;

function TLapeDeclaration.getDocPos: TDocPos;
begin
  Result := _DocPos;
end;

procedure TLapeDeclaration.setList(AList: TLapeDeclarationList);
begin
  if (AList <> FList) then
  begin
    if (FList <> nil) then
      FList.Delete(Self);
    if (AList <> nil) then
      FList.addDeclaration(Self);
    FList := AList;
  end;
end;

constructor TLapeDeclaration.Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create();
  Name := AName;
  Used := False;
  if (ADocPos <> nil) then
    _DocPos := ADocPos^
  else
    _DocPos := NullDocPos;
  setList(AList);
end;

destructor TLapeDeclaration.Destroy;
begin
  setList(nil);
  inherited;
end;

{$IFDEF Lape_TrackObjects}
initialization
  lpgList := TList.Create();
finalization
  lpgList.Free();
{$ENDIF}

end.

