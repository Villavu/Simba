
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

ASSERT
~~~~~~

.. code-block:: pascal

    procedure ASSERT

Assigned
~~~~~~~~

.. code-block:: pascal

    function Assigned(constref p): EvalBool

BREAK
~~~~~

.. code-block:: pascal

    procedure BREAK

Ceil
~~~~

.. code-block:: pascal

    function Ceil(x: Extended): Int64

Chr
~~~

.. code-block:: pascal

    function Chr(IntValue: UInt16): WideChar

Chr
~~~

.. code-block:: pascal

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

CONTINUE
~~~~~~~~

.. code-block:: pascal

    procedure CONTINUE

COPY
~~~~

.. code-block:: pascal

    procedure COPY

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
