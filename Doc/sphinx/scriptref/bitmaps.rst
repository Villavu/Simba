
.. _scriptref-bitmaps:

Bitmaps
=======

A bitmap in Simba is simply a two dimensional *field of colours*. These colours
can all be the same, or they can be of different colours. Simba features
functions to create, manipulate and search for bitmaps.

.. INSERT BITMAP EXAMPLE HERE (Picture, etc)

The bitmaps are - just as files - represented as integer in Simba (they point to
a list of bitmaps, and the value of the integer is the position in the list).
So typically, when referring to bitmaps in Simba, you simply represent them as
an integer:

.. code-block:: pascal

    var bmp, x, y: integer;
    bmp := CreateBitmap(10, 10); // Create a bitmap of size (10, 10)
    if FindBitmapIn(bmp, x, y, 0, 0, 300, 300) then
      writeln('Found it!');

Note that the previous example doesn't make a lot of sense as the bitmap has
only been created and not filled with any colours, they are as of yet,
undefined. You can also create bitmaps from screenshots and load them when your
script starts using the :ref:`scriptref-bitmapfromstring` function, or
simple store them as files and load them using the :ref:`scriptref-loadbitmap`
function.

Word of caution on bitmap creation
----------------------------------

Bitmaps in Simba are internally all instances of *TMufasBitmap*. Scripts should
generally access bitmaps using their *handle*: an integer. All functions
referenced here either require a bitmap *handle* or return one.

If you want to gain more access over a specific bitmap, see the
*GetMufasaBitmap* function. It is highly unrecommended to create bitmaps like
this, because Simba will not free them automatically for you. (There's no
problem doing it like this if you only want to perform operations on it and then
free it again)

.. code-block:: pascal

    var bmp: TMufasaBitmap;
    bmp := TMufasBitmap.Create;

Because there is no way to get a *handle* to this bitmap; as it will not be
managed by Simba internally. (All Bitmaps created by *CreateBitmap* are managed
by Simba, if you don't know what this means: you generally want Simba to manage
the bitmaps)

If you still want access to the TMufasaBitmap, use *GetMufasaBitmap*, described
below.

GetMufasaBitmap
---------------

.. code-block:: pascal

    function GetMufasaBitmap(bmp : integer) : TMufasaBitmap;

Returns the *TMufasaBitmap* for the given bitmap. They both reference the same
bitmap. TMufasaBitmap is a more advanced interface to bitmaps in Simba.
There is no way to get a *internal* (integer)
reference to a bitmap if you create it with TMufasaBitmap.Create; so the
recommended way is to use *CreateBitmap* to get the integer reference/handle and
then call this function to get the class reference.


.. code-block:: pascal

    var bmp: TMufasaBitmap;
        bmph: integer;
    bmph := CreateBitmap(100, 100);
    bmp := GetMufasaBitmap(bmph);

    bmp.SetSize(150,150); // also changes bmph, as they are the same bitmap.


CreateBitmapString
------------------

.. code-block:: pascal

    function CreateBitmapString(bmp : integer) : string;

Creates a string for the given bitmap.

CreateBitmap
------------

.. code-block:: pascal

    function CreateBitmap(w,h :integer) : integer;

Create a bitmap with width *h* and height *h*. Returns the bitmap reference.


FreeBitmap
----------

.. code-block:: pascal

    procedure FreeBitmap(Bmp : integer);

Free the bitmap. You should do this when you no longer need the bitmap.
Be careful when working with bitmaps: not freeing it when you no longer need it
leads to memory leaks, which will eventually make your script crash. (Unless you
stop it in time, in which case Simba will free the bitmaps for you)

SaveBitmap
----------

.. code-block:: pascal

    procedure SaveBitmap(Bmp : integer; path : string);

Save the given bitmap to the specified path.


.. _scriptref-bitmapfromstring:

BitmapFromString
----------------

.. code-block:: pascal

    function BitmapFromString(Width,Height : integer; Data : string): integer;

Load a bitmap from the given string. This command is usually generated with the
Bitmap to String feature in Simba.


.. _scriptref-loadbitmap:

LoadBitmap
----------

.. code-block:: pascal

    function LoadBitmap(Path : string) : integer;

Load a bitmap from a path to a file. Known formats are .bmp and .png. (Possibly
others, don't know for sure)

SetBitmapSize
-------------

.. code-block:: pascal

    procedure SetBitmapSize(Bmp,NewW,NewH : integer);

Change the size of the bitmap. Previous data will be preserved (if possible).

GetBitmapSize
-------------

.. code-block:: pascal

    procedure GetBitmapSize(Bmp : integer; var BmpW,BmpH : integer);

Returns the size of the bitmap in *BmpW*, *BmpH*.

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

Set the pixel on the bitmap at position x, y to *color*.

FastSetPixels
-------------

.. code-block:: pascal

    procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);

Set the pixels on the bitmap at position TPA[index] to Colors[index].


FastGetPixel
------------

.. code-block:: pascal

    function FastGetPixel(bmp, x,y : integer) : TColor;

Return the colour of pixel on the bitmap, position specified by x, y.

FastGetPixels
-------------

.. code-block:: pascal

    function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;

Return an array of the colours on the bitmap; positions specified by *TPA*.


GetBitmapAreaColors
-------------------

.. code-block:: pascal

    function GetBitmapAreaColors(bmp,xs, ys, xe, ye: Integer): T2DIntegerArray;

Returns all the colours in the area defined by (*xs*, *xy*, *xe*, *ye*) on the
bitmap in a two dimensions integer array.

FastDrawClear
-------------

.. code-block:: pascal

    procedure FastDrawClear(bmp : integer; Color : TColor);

Draw *Color* on every pixel on the bitmap.

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

Copy client area *xs, ys, xe, ye* to specified bitmap.

BitmapFromClient
----------------

.. code-block:: pascal

    function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer;

Create a bitmap from the client. Area specified by *xs, ye, xe, ye*.

SetBitmapName
-------------

.. code-block:: pascal

    procedure SetBitmapName(Bmp : integer; name : string);

Assign a name to the bitmap. Mainly for debugging purposes. (It will write the
name of the bitmap if it hasn't been freed.)

FindBitmap
----------

.. code-block:: pascal

    function FindBitmap(bitmap: integer; var x, y: Integer): Boolean;

Searches for the Bitmap *bmp* on the entire client. Returns true if found.
If found, *x, y* specifies the position where the bitmap was found.

FindBitmapIn
------------

.. code-block:: pascal

    function FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;


Searches for the Bitmap *bmp* on the client in the area defined by *xs,ys,xe,ye*.
Returns true if found. If found, *x, y* specifies the position where the bitmap
was found.

FindBitmapToleranceIn
---------------------

.. code-block:: pascal

    function FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;

Searches for the Bitmap *bmp* on the client in the area defined by *xs,ys,xe,ye*.
Tolerance defines the tolerance per pixel when matching bitmaps. See
:ref:`scriptref-CTS` for more information on tolerance.
Returns true if found. If found, *x, y* specifies the position where the bitmap
was found.

FindBitmapSpiral
----------------

.. code-block:: pascal

    function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;

Searches for the Bitmap *bmp* on the client in the area defined by *xs,ys,xe,ye*.
Returns true if found. If found, *x, y* specifies the position where the bitmap
was found. Search starts from a point defined by *x, y*.


FindBitmapsSpiralTolerance
--------------------------

.. code-block:: pascal

    function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;


Searches for the Bitmap *bmp* on the client in the area defined by *xs,ys,xe,ye*.
Tolerance defines the tolerance per pixel when matching bitmaps. See
:ref:`scriptref-CTS` for more information on tolerance.
Search starts from a point defined by *x, y*.
Returns true if found. If found, each point in *TPA* specifies a match.

FindBitmapSpiralTolerance
-------------------------

.. code-block:: pascal

    function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;

Searches for the Bitmap *bmp* on the client in the area defined by *xs,ys,xe,ye*.
Tolerance defines the tolerance per pixel when matching bitmaps. See
:ref:`scriptref-CTS` for more information on tolerance.
Search starts from a point defined by *x, y*.
Returns true if found. If found, *x, y* specifies the position where the bitmap
was found.

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

Creates a copy of the *Bitmap*. Returns the bitmap copy.

GreyScaleBitmap
---------------

.. code-block:: pascal

    function GreyScaleBitmap(bitmap : integer) : integer

Creates a copy of the bitmap, greyscaled.


BrightnessBitmap
----------------

.. code-block:: pascal

    function BrightnessBitmap(Bitmap,br : integer) : integer;

Changes the brightness of a bitmap, intensity defined by *br*.
Returns a new bitmap with the brightness applied.

If you instead want to apply brightness to the current bitmap, see
:ref:`filter_apply_bitmap`

ContrastBitmap
--------------

.. code-block:: pascal

    function ContrastBitmap(bitmap : integer; co : extended) : integer;

Changes the constrast of a bitmap, returns a new bitmap with the contrast
applied.


PosterizeBitmap
---------------

.. code-block:: pascal

    function PosterizeBitmap(Bitmap : integer; po : integer) : integer;

Posterizes a bitmap, intensity defined by *po*; returns a new bitmap with the
posterisation applied.


.. _filter_apply_bitmap:

Applying a filter on the current bitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    var b: integer;
    begin
        // Dummy bitmap. You'll want something that's not just a blank bitmap.
        B:=CreateBitmap(100,100);

        // Apply the filter (Posterize in this case) without making a copy.
        GetMufasaBitmap(b).Posterize(GetMufasaBitmap(b), 10);

        // Always free your bitmaps when you no longer use them. :) 
        FreeBitmap(b);
    end.

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

*Draws* a TPointArray on a bitmap. Each point in the TPointArray is *painted*
on the bitmap by setting the pixel on the bitmap (position defined by tpa point)
to *color*.


DrawATPABitmap
--------------

.. code-block:: pascal

    procedure DrawATPABitmap(bitmap: integer; ATPA: T2DPointArray);

*Draws* a Array of TPointArray on a bitmap.
Each point in the TPointArray is *painted* on the bitmap by setting
the pixel on the bitmap (position defined by tpa point)
to a color. Colors differ per TPointArray (group).

DrawATPABitmapEx
----------------

.. code-block:: pascal

    procedure DrawATPABitmapEx(bitmap: integer; ATPA: T2DPointArray; Colors: TIntegerArray);

*Draws* a Array of TPointArray on a bitmap.
Each point in the TPointArray is *painted* on the bitmap by setting
the pixel on the bitmap (position defined by tpa point)
to a color. Colors are defined by *Colors*.


DrawBitmap
----------

.. code-block:: pascal

    procedure DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);

Draw the bitmap to a TCanvas.

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


