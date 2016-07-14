
BinCoe
~~~~~~

.. code-block:: pascal

    function BinCoe(a, b: LongInt): Extended

Cot
~~~

.. code-block:: pascal

    function Cot(e: Extended): Extended

CotH
~~~~

.. code-block:: pascal

    function CotH(e: Extended): Extended

Csc
~~~

.. code-block:: pascal

    function Csc(e: Extended): Extended

CscH
~~~~

.. code-block:: pascal

    function CscH(e: Extended): Extended

DecEx
~~~~~

.. code-block:: pascal

    procedure DecEx(var x: Integer; Decrease: Integer)

DecRet
~~~~~~

.. code-block:: pascal

    function DecRet(e: Extended): Extended

Degrees
~~~~~~~

.. code-block:: pascal

    function Degrees(e: Extended): Extended

DiscreteGauss
~~~~~~~~~~~~~

.. code-block:: pascal

    function DiscreteGauss(Xstart, Xend: Integer; Sigma: Extended): TExtendedArray

Distance
~~~~~~~~

.. code-block:: pascal

    function Distance(xs, ys, xe, ye: Integer): Integer

Factorial
~~~~~~~~~

.. code-block:: pascal

    function Factorial(number: LongWord): Int64

FixD
~~~~

.. code-block:: pascal

    function FixD(Degrees: Extended): Extended

FixRad
~~~~~~

.. code-block:: pascal

    function FixRad(rad: Extended): Extended

GaussMatrix
~~~~~~~~~~~

.. code-block:: pascal

    function GaussMatrix(N: Integer; Sigma: Extended): T2DExtendedArray

InAbstractBox
~~~~~~~~~~~~~

.. code-block:: pascal

    function InAbstractBox(xs, ys, xe, ye, x3, y3, x4, y4: Integer; x, y: Integer): Boolean

IncEx
~~~~~

.. code-block:: pascal

    procedure IncEx(var x: Integer; increase: Integer)

InRange
~~~~~~~

.. code-block:: pascal

    function InRange(const value, min, max: Integer): Boolean

IntInBox
~~~~~~~~

.. code-block:: pascal

    function IntInBox(x, y: Integer; Box: TBox): Boolean

IntToBox
~~~~~~~~

.. code-block:: pascal

    function IntToBox(xs, ys, xe, ye: Integer): TBox

IntToHex
~~~~~~~~

.. code-block:: pascal

    function IntToHex(number: Integer): string

IntToHex
~~~~~~~~

.. code-block:: pascal

    function IntToHex(number: Integer): string; overload

log10
~~~~~

.. code-block:: pascal

    function log10(f: Extended): Extended

logn
~~~~

.. code-block:: pascal

    function logn(base, x: Extended): Extended

MaxA
~~~~

.. code-block:: pascal

    function MaxA(a: TIntegerArray): Integer

MaxE
~~~~

.. code-block:: pascal

    function MaxE(a, b: Extended): Extended

MiddleBox
~~~~~~~~~

.. code-block:: pascal

    function MiddleBox(b: TBox): TPoint

MinA
~~~~

.. code-block:: pascal

    function MinA(a: TIntegerArray): Integer

MinE
~~~~

.. code-block:: pascal

    function MinE(a, b: Extended): Extended

Point
~~~~~

.. code-block:: pascal

    function Point(x, y: Integer): TPoint

PointInBox
~~~~~~~~~~

.. code-block:: pascal

    function PointInBox(PT: TPoint; Box: TBox): Boolean

PointToBox
~~~~~~~~~~

.. code-block:: pascal

    function PointToBox(PT1, PT2: TPoint): TBox

pow
~~~

.. code-block:: pascal

    function pow(base, exponent: Extended): Extended

radians
~~~~~~~

.. code-block:: pascal

    function radians(e: Extended): Extended

RandomE
~~~~~~~

.. code-block:: pascal

    function RandomE: Extended

RandomRange
~~~~~~~~~~~

.. code-block:: pascal

    function RandomRange(const aFrom, aTo: Integer): Integer

RiemannGauss
~~~~~~~~~~~~

.. code-block:: pascal

    function RiemannGauss(Xstart, StepSize, Sigma: Extended; AmountSteps: Integer): Extended

rol
~~~

.. code-block:: pascal

    function rol(num: LongWord; shift: Byte): LongWord

ror
~~~

.. code-block:: pascal

    function ror(num: LongWord; shift: Byte): LongWord

sar
~~~

.. code-block:: pascal

    function sar(AValue: LongInt; shift: Byte): LongInt

Sec
~~~

.. code-block:: pascal

    function Sec(e: Extended): Extended

SecH
~~~~

.. code-block:: pascal

    function SecH(e: Extended): Extended

Sum64IntArr
~~~~~~~~~~~

.. code-block:: pascal

    function Sum64IntArr(const Arr: TIntegerArray): Int64
