
.. _scriptref_string:

String Functions
================

Capitalize
----------

.. code-block:: pascal

    function Capitalize(str : string) : string;


CompressString
--------------

.. code-block:: pascal

    function CompressString(const Str : string) : string;


DecompressString
----------------

.. code-block:: pascal

    function DecompressString(const Compressed : string) : string;


Base64Encode
------------

.. code-block:: pascal

    function Base64Encode(const str : string) : string;


Base64Decode
------------

.. code-block:: pascal

    function Base64Decode(const str : string) : string;


Format
------

.. code-block:: pascal

    function Format(const fmt : string;const args : array of const) : string;


ToStr
-----

.. code-block:: pascal

    function ToStr(x) : string;


Between
-------

.. code-block:: pascal

    function Between(s1, s2, str: string): string;


IntToStr
--------

.. code-block:: pascal

    function IntToStr(value: Integer): String;


FloatToStr
----------

.. code-block:: pascal

    function FloatToStr(value: Extended): String;


BoolToStr
---------

.. code-block:: pascal

    function BoolToStr(value: Boolean): String;


StrToInt
--------

.. code-block:: pascal

    function StrToInt(value: String): Integer;


StrToIntDef
-----------

.. code-block:: pascal

    function StrToIntDef(value: String; default: Integer): Integer;


StrToFloat
----------

.. code-block:: pascal

    function StrToFloat(value: String): Extended;


StrToFloatDef
-------------

.. code-block:: pascal

    function StrToFloatDef(value: String; default: Extended): Extended;


StrToBool
---------

.. code-block:: pascal

    function StrToBool(value: String): Boolean;


StrToBoolDef
------------

.. code-block:: pascal

    function StrToBoolDef(value: String; default: Boolean): Boolean;


ExtractFromStr
--------------

.. code-block:: pascal

    function ExtractFromStr( Str : string; Extract : StrExtr) : string;


Replace
-------

.. code-block:: pascal

    function Replace(Text, FindStr, ReplaceStr: string; Flags: TReplaceFlags): string;


ReplaceWrap
-----------

.. code-block:: pascal

    function ReplaceWrap(Text, FindStr, ReplaceStr: string; Flags: TReplaceFlags): string;


Implode
-------

.. code-block:: pascal

    function Implode(Glue: string; Pieces: TStringArray): string;


Explode
-------

.. code-block:: pascal

    function Explode(del, str: string): TStringArray;


ExplodeWrap
-----------

.. code-block:: pascal

    procedure ExplodeWrap(del, str: string; var res : TStringArray);


Padl
----

.. code-block:: pascal

    function Padl(s: String; i: longInt): String;


Padz
----

.. code-block:: pascal

    function Padz(s: String; i: longInt): String;


Padr
----

.. code-block:: pascal

    function Padr(s: String; i: longInt): String;


ExecRegExpr
-----------

.. code-block:: pascal

    function ExecRegExpr( const RegExpr, InputStr : String) : boolean;


SplitRegExpr
------------

.. code-block:: pascal

    procedure SplitRegExpr( const RegExpr, InputStr : String; Pieces : TStrings);


ReplaceRegExpr
--------------

.. code-block:: pascal

    function ReplaceRegExpr( const RegExpr, InputStr, ReplaceStr : String; UseSubstitution : boolean) : String;


PosEx
-----

.. code-block:: pascal

    function PosEx(needle, haystack: String; offset: integer): integer;');  



