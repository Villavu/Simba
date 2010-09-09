
.. _scriptref_bitmaps:

Bitmaps
=======

CreateBitmapString
------------------

.. code-block:: pascal

    function CreateBitmapString(bmp : integer) : string;


GetMufasaBitmap
---------------

.. code-block:: pascal

    function GetMufasaBitmap(bmp : integer) : TMufasaBitmap;


CreateBitmap
------------

.. code-block:: pascal

    function CreateBitmap(w,h :integer) : integer;


FreeBitmap
----------

.. code-block:: pascal

    procedure FreeBitmap(Bmp : integer);


SaveBitmap
----------

.. code-block:: pascal

    procedure SaveBitmap(Bmp : integer; path : string);


BitmapFromString
----------------

.. code-block:: pascal

    function BitmapFromString(Width,Height : integer; Data : string): integer;


LoadBitmap
----------

.. code-block:: pascal

    function LoadBitmap(Path : string) : integer;


SetBitmapSize
-------------

.. code-block:: pascal

    procedure SetBitmapSize(Bmp,NewW,NewH : integer);


GetBitmapSize
-------------

.. code-block:: pascal

    procedure GetBitmapSize(Bmp : integer; var BmpW,BmpH : integer);


StretchBitmapResize
-------------------

.. code-block:: pascal

    procedure StretchBitmapResize(Bmp,NewW,NewH : integer);


CreateMirroredBitmap
--------------------

.. code-block:: pascal

    function CreateMirroredBitmap(Bmp : integer) : integer;


CreateMirroredBitmapEx
----------------------

.. code-block:: pascal

    function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;


FastSetPixel
------------

.. code-block:: pascal

    procedure FastSetPixel(bmp,x,y : integer; Color : TColor);


FastSetPixels
-------------

.. code-block:: pascal

    procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);


FastGetPixel
------------

.. code-block:: pascal

    function FastGetPixel(bmp, x,y : integer) : TColor;


FastGetPixels
-------------

.. code-block:: pascal

    function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;


GetBitmapAreaColors
-------------------

.. code-block:: pascal

    function GetBitmapAreaColors(bmp,xs, ys, xe, ye: Integer): T2DIntegerArray;


FastDrawClear
-------------

.. code-block:: pascal

    procedure FastDrawClear(bmp : integer; Color : TColor);


FastDrawTransparent
-------------------

.. code-block:: pascal

    procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);


SetTransparentColor
-------------------

.. code-block:: pascal

    procedure SetTransparentColor(bmp : integer; Color : TColor);


GetTransparentColor
-------------------

.. code-block:: pascal

    function GetTransparentColor(bmp: integer) : TColor;


FastReplaceColor
----------------

.. code-block:: pascal

    procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);


CopyClientToBitmap
------------------

.. code-block:: pascal

    procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer);


BitmapFromClient
----------------

.. code-block:: pascal

    function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer;


SetBitmapName
-------------

.. code-block:: pascal

    procedure SetBitmapName(Bmp : integer; name : string);


FindBitmap
----------

.. code-block:: pascal

    function FindBitmap(bitmap: integer; var x, y: Integer): Boolean;


FindBitmapIn
------------

.. code-block:: pascal

    function FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;


FindBitmapToleranceIn
---------------------

.. code-block:: pascal

    function FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;


FindBitmapSpiral
----------------

.. code-block:: pascal

    function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;


FindBitmapsSpiralTolerance
--------------------------

.. code-block:: pascal

    function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;


FindBitmapSpiralTolerance
-------------------------

.. code-block:: pascal

    function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;


RotateBitmap
------------

.. code-block:: pascal

    function RotateBitmap(bitmap: Integer; angle: Extended): Integer;


DesaturateBitmap
----------------

.. code-block:: pascal

    function DesaturateBitmap(Bitmap : integer) : integer;


InvertBitmap
------------

.. code-block:: pascal

    procedure InvertBitmap(Bitmap : integer);


CopyBitmap
----------

.. code-block:: pascal

    function CopyBitmap(Bitmap:  integer) : integer)


GreyScaleBitmap
---------------

.. code-block:: pascal

    function GreyScaleBitmap(bitmap : integer) : integer


BrightnessBitmap
----------------

.. code-block:: pascal

    function BrightnessBitmap(Bitmap,br : integer) : integer;


ContrastBitmap
--------------

.. code-block:: pascal

    function ContrastBitmap(bitmap : integer; co : extended) : integer;


PosterizeBitmap
---------------

.. code-block:: pascal

    function PosterizeBitmap(Bitmap : integer; po : integer) : integer;


CreateMaskFromBitmap
--------------------

.. code-block:: pascal

    function CreateMaskFromBitmap(Bitmap : integer) : TMask;


FindMaskTolerance
-----------------

.. code-block:: pascal

    function FindMaskTolerance(const mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;


FindBitmapMaskTolerance
-----------------------

.. code-block:: pascal

    function FindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;


FindDeformedBitmapToleranceIn
-----------------------------

.. code-block:: pascal

    function FindDeformedBitmapToleranceIn(bitmap: integer; var x,y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;


DrawTPABitmap
-------------

.. code-block:: pascal

    procedure DrawTPABitmap(bitmap: integer; TPA: TPointArray; Color: integer);


DrawATPABitmap
--------------

.. code-block:: pascal

    procedure DrawATPABitmap(bitmap: integer; ATPA: T2DPointArray);


DrawATPABitmapEx
----------------

.. code-block:: pascal

    procedure DrawATPABitmapEx(bitmap: integer; ATPA: T2DPointArray; Colors: TIntegerArray);


DrawBitmap
----------

.. code-block:: pascal

    procedure DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);


RectangleBitmap
---------------

.. code-block:: pascal

    procedure RectangleBitmap(bitmap : integer; const box : TBox; Color : TColor);


FloodFillBitmap
---------------

.. code-block:: pascal

    procedure FloodFillBitmap(bitmap : integer; const StartPoint : TPoint; const SearchCol,ReplaceCol : TColor);


CalculatePixelShift
-------------------

.. code-block:: pascal

    function CalculatePixelShift(Bmp1,Bmp2 : Integer; CompareBox : TBox) : integer;


CalculatePixelTolerance
-----------------------

.. code-block:: pascal

    function CalculatePixelTolerance(Bmp1,Bmp2 : Integer; CompareBox : TBox; CTS : integer) : extended;')


