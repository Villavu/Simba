.. _scriptref-lape:

Lape
====

Magic Functions
---------------

ASSERT
~~~~~~

.. code-block:: pascal

    procedure ASSERT

BREAK
~~~~~

.. code-block:: pascal

    procedure BREAK

CONTINUE
~~~~~~~~

.. code-block:: pascal

    procedure CONTINUE

COPY
~~~~

.. code-block:: pascal

    procedure COPY

DEC
~~~

.. code-block:: pascal

    procedure DEC

DEFAULT
~~~~~~~

.. code-block:: pascal

    procedure DEFAULT

DELETE
~~~~~~

.. code-block:: pascal

    procedure DELETE

DISPOSE
~~~~~~~

.. code-block:: pascal

    procedure DISPOSE

EXIT
~~~~

.. code-block:: pascal

    procedure EXIT

GETARRAYLENGTH
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure GETARRAYLENGTH

HALT
~~~~

.. code-block:: pascal

    procedure HALT

HIGH
~~~~

.. code-block:: pascal

    procedure HIGH

INC
~~~

.. code-block:: pascal

    procedure INC

INSERT
~~~~~~

.. code-block:: pascal

    procedure INSERT

ISSCRIPTMETHOD
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ISSCRIPTMETHOD

LAPIFY
~~~~~~

.. code-block:: pascal

    procedure LAPIFY

LENGTH
~~~~~~

.. code-block:: pascal

    procedure LENGTH

LOW
~~~

.. code-block:: pascal

    procedure LOW

NATIFY
~~~~~~

.. code-block:: pascal

    procedure NATIFY

NATIVE
~~~~~~

.. code-block:: pascal

    procedure NATIVE

NEW
~~~

.. code-block:: pascal

    procedure NEW

ORD
~~~

.. code-block:: pascal

    procedure ORD

PRED
~~~~

.. code-block:: pascal

    procedure PRED

SETARRAYLENGTH
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SETARRAYLENGTH

SETLENGTH
~~~~~~~~~

.. code-block:: pascal

    procedure SETLENGTH

SIZEOF
~~~~~~

.. code-block:: pascal

    procedure SIZEOF

SUCC
~~~~

.. code-block:: pascal

    procedure SUCC

SWAP
~~~~

.. code-block:: pascal

    procedure SWAP

WRITELN
~~~~~~~

.. code-block:: pascal

    procedure WRITELN

WRITE
~~~~~

.. code-block:: pascal

    procedure WRITE

Standard Library Functions
--------------------------

AdjustLineBreaks
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function AdjustLineBreaks(s: string; Style: TTextLineBreakStyle): string

AllocMem
~~~~~~~~

.. code-block:: pascal

    function AllocMem(i: SizeInt): Pointer

AnsiCompareStr
~~~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiCompareStr(s1, s2: string): Int32

AnsiCompareText
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiCompareText(s1, s2: string): Int32

AnsiDequotedStr
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiDequotedStr(s: string; AQuote: Char): string

AnsiLowerCase
~~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiLowerCase(s: string): string

AnsiQuotedStr
~~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiQuotedStr(s: string; Quote: Char): string

AnsiSameStr
~~~~~~~~~~~

.. code-block:: pascal

    function AnsiSameStr(s1, s2: string): EvalBool

AnsiSameText
~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiSameText(s1, s2: string): EvalBool

AnsiUpperCase
~~~~~~~~~~~~~

.. code-block:: pascal

    function AnsiUpperCase(s: string): string

ArcCos
~~~~~~

.. code-block:: pascal

    function ArcCos(x: Extended): Extended

ArcCosH
~~~~~~~

.. code-block:: pascal

    function ArcCosH(x: Extended): Extended

ArcSin
~~~~~~

.. code-block:: pascal

    function ArcSin(x: Extended): Extended

ArcSinH
~~~~~~~

.. code-block:: pascal

    function ArcSinH(x: Extended): Extended

ArcTan2
~~~~~~~

.. code-block:: pascal

    function ArcTan2(x, y: Extended): Extended

ArcTan
~~~~~~

.. code-block:: pascal

    function ArcTan(x: Extended): Extended

ArcTanH
~~~~~~~

.. code-block:: pascal

    function ArcTanH(x: Extended): Extended

Assigned
~~~~~~~~

.. code-block:: pascal

    function Assigned(constref p): EvalBool

Ceil
~~~~

.. code-block:: pascal

    function Ceil(x: Extended): Int64

Chr
~~~

.. code-block:: pascal

    function Chr(IntValue: UInt16): WideChar
    function Chr(IntValue: UInt8): AnsiChar

CompareMem
~~~~~~~~~~

.. code-block:: pascal

    function CompareMem(constref p1, p2; Length: SizeInt): EvalBool

CompareStr
~~~~~~~~~~

.. code-block:: pascal

    function CompareStr(s1, s2: string): Int32

CompareText
~~~~~~~~~~~

.. code-block:: pascal

    function CompareText(s1, s2: string): Int32

Cosecant
~~~~~~~~

.. code-block:: pascal

    function Cosecant(x: Extended): Extended

Cos
~~~

.. code-block:: pascal

    function Cos(x: Extended): Extended

CosH
~~~~

.. code-block:: pascal

    function CosH(x: Extended): Extended

Cotan
~~~~~

.. code-block:: pascal

    function Cotan(x: Extended): Extended

CurrToStr
~~~~~~~~~

.. code-block:: pascal

    function CurrToStr(Value: Currency): string

Date
~~~~

.. code-block:: pascal

    function Date: TDateTime

DateTimeToStr
~~~~~~~~~~~~~

.. code-block:: pascal

    function DateTimeToStr(const DateTime: TDateTime): string

DateToStr
~~~~~~~~~

.. code-block:: pascal

    function DateToStr(const DateTime: TDateTime): string

DebugLn
~~~~~~~

.. code-block:: pascal

    procedure DebugLn(s: string)

DecodeDateFully
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function DecodeDateFully(DateTime: TDateTime; var Year, Month, Day, DOW: UInt16): Boolean

DecodeDate
~~~~~~~~~~

.. code-block:: pascal

    procedure DecodeDate(DateTime: TDateTime; var Year, Month, Day: UInt16)

DecodeTime
~~~~~~~~~~

.. code-block:: pascal

    procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: UInt16)

EncodeDate
~~~~~~~~~~

.. code-block:: pascal

    function EncodeDate(Year, Month, Day: UInt16): TDateTime

EncodeTime
~~~~~~~~~~

.. code-block:: pascal

    function EncodeTime(Hour, Min, Sec, MSec: UInt16): TDateTime

ExceptionTostring
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function ExceptionTostring(Ex: TIFException; Param: string): string

Exp
~~~

.. code-block:: pascal

    function Exp(x: Extended): Extended

FillMem
~~~~~~~

.. code-block:: pascal

    procedure FillMem(var p; s: SizeInt; b: UInt8= 0)

Floor
~~~~~

.. code-block:: pascal

    function Floor(x: Extended): Int64

FormatCurr
~~~~~~~~~~

.. code-block:: pascal

    function FormatCurr(Format: string; Value: Currency): string

FormatDateTime
~~~~~~~~~~~~~~

.. code-block:: pascal

    function FormatDateTime(Format: string; DateTime: TDateTime): string

FormatFloat
~~~~~~~~~~~

.. code-block:: pascal

    function FormatFloat(Format: string; Value: Extended): string

Frac
~~~~

.. code-block:: pascal

    function Frac(x: Extended): Extended

FreeLibrary
~~~~~~~~~~~

.. code-block:: pascal

    function FreeLibrary(Lib: TLibHandle): EvalBool

FreeMem
~~~~~~~

.. code-block:: pascal

    procedure FreeMem(p: Pointer)

GetCurrThreadID
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetCurrThreadID: PtrUInt

GetGlobal
~~~~~~~~~

.. code-block:: pascal

    function GetGlobal(Name: string): Variant

GetGlobalName
~~~~~~~~~~~~~

.. code-block:: pascal

    function GetGlobalName(Ptr: ConstPointer): string

GetGlobalPtr
~~~~~~~~~~~~

.. code-block:: pascal

    function GetGlobalPtr(Name: string): ConstPointer

GetMem
~~~~~~

.. code-block:: pascal

    function GetMem(i: SizeInt): Pointer

GetProcAddress
~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetProcAddress(Lib: TlibHandle; const ProcName: string): ConstPointer

GetSystemTime
~~~~~~~~~~~~~

.. code-block:: pascal

    function GetSystemTime: LongWord

GetTickCount
~~~~~~~~~~~~

.. code-block:: pascal

    function GetTickCount: UInt64

Hypot
~~~~~

.. code-block:: pascal

    function Hypot(x,y: Extended): Extended

Int64ToStr
~~~~~~~~~~

.. code-block:: pascal

    function Int64ToStr(i: Int64): string

Int
~~~

.. code-block:: pascal

    function Int(x: Extended): Extended

IsDelimiter
~~~~~~~~~~~

.. code-block:: pascal

    Function IsDelimiter(Delimiters, s: string; Index: SizeInt): EvalBool

LastDelimiter
~~~~~~~~~~~~~

.. code-block:: pascal

    function LastDelimiter(Delimiters, s: string): SizeInt

Ln
~~

.. code-block:: pascal

    function Ln(x: Extended): Extended

LoadLibrary
~~~~~~~~~~~

.. code-block:: pascal

    function LoadLibrary(const Name: string): TLibHandle

LowerCase
~~~~~~~~~

.. code-block:: pascal

    function LowerCase(s: string): string

Move
~~~~

.. code-block:: pascal

    procedure Move(constref Src; var Dst; s: SizeInt)

Now
~~~

.. code-block:: pascal

    function Now: TDateTime

Pos
~~~

.. code-block:: pascal

    function Pos(Substr: string; Source: string): SizeInt

Power
~~~~~

.. code-block:: pascal

    function Power(Base, Exponent: Extended): Extended

QuotedStr
~~~~~~~~~

.. code-block:: pascal

    function QuotedStr(s: string): string

Randomize
~~~~~~~~~

.. code-block:: pascal

    procedure Randomize

ReallocMem
~~~~~~~~~~

.. code-block:: pascal

    procedure ReallocMem(var p: Pointer; s: SizeInt)

ReplaceDate
~~~~~~~~~~~

.. code-block:: pascal

    procedure ReplaceDate(var DateTime: TDateTime; NewDate: TDateTime)

ReplaceTime
~~~~~~~~~~~

.. code-block:: pascal

    procedure ReplaceTime(var DateTime: TDateTime; NewTime: TDateTime)

Replicate
~~~~~~~~~

.. code-block:: pascal

    function Replicate(c: Char; l: SizeInt): string

SameText
~~~~~~~~

.. code-block:: pascal

    function SameText(s1, s2: string): EvalBool

Secant
~~~~~~

.. code-block:: pascal

    function Secant(x: Extended): Extended

Sin
~~~

.. code-block:: pascal

    function Sin(x: Extended): Extended

SinH
~~~~

.. code-block:: pascal

    function SinH(x: Extended): Extended

Sqrt
~~~~

.. code-block:: pascal

    function Sqrt(x: Extended): Extended

StrGet2
~~~~~~~

.. code-block:: pascal

    function StrGet2(s: string; Index: SizeInt): Char

StrGet
~~~~~~

.. code-block:: pascal

    function StrGet(var s: string; Index: SizeInt): Char

StringOfChar
~~~~~~~~~~~~

.. code-block:: pascal

    function StringOfChar(c: Char; l: SizeInt): string

StringReplace
~~~~~~~~~~~~~

.. code-block:: pascal

    function StringReplace(S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string

StrSet
~~~~~~

.. code-block:: pascal

    procedure StrSet(c: Char; Index: SizeInt; var s: string)

StrToCurrDef
~~~~~~~~~~~~

.. code-block:: pascal

    function StrToCurrDef(s: string; Def: Currency): Currency

StrToCurr
~~~~~~~~~

.. code-block:: pascal

    function StrToCurr(s: string): Currency

StrToDateDef
~~~~~~~~~~~~

.. code-block:: pascal

    function StrToDateDef(s: string; Default: TDateTime): TDateTime

StrToDate
~~~~~~~~~

.. code-block:: pascal

    function StrToDate(s: string): TDateTime

StrToDateTimeDef
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function StrToDateTimeDef(s: string; Default: TDateTime): TDateTime

StrToDateTime
~~~~~~~~~~~~~

.. code-block:: pascal

    function StrToDateTime(s: string): TDateTime

StrToInt64Def
~~~~~~~~~~~~~

.. code-block:: pascal

    function StrToInt64Def(s: string; Def: Int64): Int64

StrToInt64
~~~~~~~~~~

.. code-block:: pascal

    function StrToInt64(s: string): Int64

StrToTimeDef
~~~~~~~~~~~~

.. code-block:: pascal

    function StrToTimeDef(s: string; Default: TDateTime): TDateTime

StrToTime
~~~~~~~~~

.. code-block:: pascal

    function StrToTime(s: string): TDateTime

StrToUInt64Def
~~~~~~~~~~~~~~

.. code-block:: pascal

    function StrToUInt64Def(s: string; Def: UInt64): UInt64

StrToUInt64
~~~~~~~~~~~

.. code-block:: pascal

    function StrToUInt64(s: string): UInt64

Sync
~~~~

.. code-block:: pascal

    procedure Sync(Proc: TSyncMethod)

Tan
~~~

.. code-block:: pascal

    function Tan(x: Extended): Extended

TanH
~~~~

.. code-block:: pascal

    function TanH(x: Extended): Extended

Time
~~~~

.. code-block:: pascal

    function Time: TDateTime

TimeToStr
~~~~~~~~~

.. code-block:: pascal

    function TimeToStr(const DateTime: TDateTime): string

ToString
~~~~~~~~

.. code-block:: pascal

    function ToString(constref p: Pointer): string

Trim
~~~~

.. code-block:: pascal

    function Trim(s: string): string

TrimLeft
~~~~~~~~

.. code-block:: pascal

    function TrimLeft(s: string): string

TrimRight
~~~~~~~~~

.. code-block:: pascal

    function TrimRight(s: string): string

Trunc
~~~~~

.. code-block:: pascal

    function Trunc(x: Extended): Int64

UInt64ToStr
~~~~~~~~~~~

.. code-block:: pascal

    function UInt64ToStr(i: UInt64): string

UpperCase
~~~~~~~~~

.. code-block:: pascal

    function UpperCase(s: string): string

VarArrayAsPSafeArray
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayAsPSafeArray(const A: Variant): Pointer

VarArrayCreate
~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayCreate(Bounds: array of SizeInt; aVarType: TVarType): Variant

VarArrayDimCount
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayDimCount(const A: Variant): SizeInt

VarArrayGet
~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayGet(const A: Variant; Indices: array of Int32): Variant

VarArrayGet
~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayGet(var s: Variant; Index: Int32): Variant

VarArrayHighBound
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayHighBound(const A: Variant; Dim: SizeInt): SizeInt

VarArrayLock
~~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayLock(const A: Variant): Pointer

VarArrayLowBound
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayLowBound(const A: Variant; Dim: SizeInt): SizeInt

VarArrayOf
~~~~~~~~~~

.. code-block:: pascal

    function VarArrayOf(Values: array of Variant): Variant

VarArrayRedim
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure VarArrayRedim(var A: Variant; HighBound: SizeInt)

VarArrayRef
~~~~~~~~~~~

.. code-block:: pascal

    function VarArrayRef(const A: Variant): Variant

VarArraySet
~~~~~~~~~~~

.. code-block:: pascal

    procedure VarArraySet(c: Variant; Index: Int32; var s: Variant)

VarArraySet
~~~~~~~~~~~

.. code-block:: pascal

    procedure VarArraySet(var A: Variant; const Value: Variant; Indices: array of Int32)

VarArrayUnlock
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure VarArrayUnlock(const A: Variant)

VarAsError
~~~~~~~~~~

.. code-block:: pascal

    function VarAsError(AResult: HRESULT): Variant

VarAsType
~~~~~~~~~

.. code-block:: pascal

    function VarAsType(const V: Variant; aVarType: TVarType): Variant

VarCompareValue
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarCompareValue(const A, B: Variant): TVariantRelationship

VarCopyNoInd
~~~~~~~~~~~~

.. code-block:: pascal

    procedure VarCopyNoInd(var Dest: Variant; const Source: Variant)

VarEnsureRange
~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarEnsureRange(const AValue, AMin, AMax: Variant): Variant

VarFromDateTime
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarFromDateTime(DateTime: TDateTime): Variant

VariantInvoke
~~~~~~~~~~~~~

.. code-block:: pascal

    function VariantInvoke(Name: string; Params: array of Variant=[]): Variant

VarInRange
~~~~~~~~~~

.. code-block:: pascal

    function VarInRange(const AValue, AMin, AMax: Variant): EvalBool

VarIsArray
~~~~~~~~~~

.. code-block:: pascal

    function VarIsArray(const A: Variant; AResolveByRef: EvalBool= True): EvalBool

VarIsByRef
~~~~~~~~~~

.. code-block:: pascal

    function VarIsByRef(const V: Variant): EvalBool

VarIsClear
~~~~~~~~~~

.. code-block:: pascal

    function VarIsClear(const V: Variant): EvalBool

VarIsCustom
~~~~~~~~~~~

.. code-block:: pascal

    function VarIsCustom(const V: Variant): EvalBool

VarIsEmpty
~~~~~~~~~~

.. code-block:: pascal

    function VarIsEmpty(const V: Variant): EvalBool

VarIsError
~~~~~~~~~~

.. code-block:: pascal

    function VarIsError(const V: Variant; out AResult: HRESULT): EvalBool

VarIsFloat
~~~~~~~~~~

.. code-block:: pascal

    function VarIsFloat(const V: Variant): EvalBool

VarIsNull
~~~~~~~~~

.. code-block:: pascal

    function VarIsNull(const V: Variant): EvalBool

VarIsNumeric
~~~~~~~~~~~~

.. code-block:: pascal

    function VarIsNumeric(const V: Variant): EvalBool

VarIsOrdinal
~~~~~~~~~~~~

.. code-block:: pascal

    function VarIsOrdinal(const V: Variant): EvalBool

VarIsStr
~~~~~~~~

.. code-block:: pascal

    function VarIsStr(const V: Variant): EvalBool

VarSameValue
~~~~~~~~~~~~

.. code-block:: pascal

    function VarSameValue(const A, B: Variant): EvalBool

VarToDateTime
~~~~~~~~~~~~~

.. code-block:: pascal

    function VarToDateTime(const V: Variant): TDateTime

VarToStrDef
~~~~~~~~~~~

.. code-block:: pascal

    function VarToStrDef(const V: Variant; ADefault: string): string

VarToStr
~~~~~~~~

.. code-block:: pascal

    function VarToStr(const V: Variant): string

VarToUnicodeStrDef
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarToUnicodeStrDef(const V: Variant; ADefault: UnicodeString): UnicodeString

VarToUnicodeStr
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarToUnicodeStr(const V: Variant): UnicodeString

VarToWideStrDef
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarToWideStrDef(const V: Variant; ADefault: WideString): WideString

VarToWideStr
~~~~~~~~~~~~

.. code-block:: pascal

    function VarToWideStr(const V: Variant): WideString

VarType
~~~~~~~

.. code-block:: pascal

    function VarType(const V: Variant): TVarType

VarTypeIsValidArrayType
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarTypeIsValidArrayType(aVarType: TVarType): EvalBool

VarTypeIsValidElementType
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function VarTypeIsValidElementType(aVarType: TVarType): EvalBool

WrapText
~~~~~~~~

.. code-block:: pascal

    function WrapText(Line, BreakStr: string; BreakChars: set of AnsiChar; MaxCol: Int32): string

WStrGet
~~~~~~~

.. code-block:: pascal

    function WStrGet(var s: WideString; Index: SizeInt): WideChar
