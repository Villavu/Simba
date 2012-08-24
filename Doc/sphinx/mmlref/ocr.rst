
.. _mmlref-ocr:

TMOCR Class
===========

The TMOCR class uses the powerful ``ocrutil`` unit to create some default but
useful functions that can be used to create and identify text. It also contains
some functions used in special cases to filter noise. Specifically, these are
all the ``Filter*`` functions.


InitTOCR
~~~~~~~~

.. code-block:: pascal

    function TMOCR.InitTOCR(const path: string): boolean;

InitTOCR loads all fonts in path
We don't do this in the constructor because we may not yet have the path.


FilterUpTextByColour
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure TMOCR.FilterUpTextByColour(bmp: TMufasaBitmap);


FilterUpTextByCharacteristics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure TMOCR.FilterUpTextByCharacteristics(bmp: TMufasaBitmap; w,h: integer);

FilterShadowBitmap
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure TMOCR.FilterShadowBitmap(bmp: TMufasaBitmap);

Remove anything but the shadows on the bitmap (Shadow = clPurple)


FilterCharsBitmap
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure TMOCR.FilterCharsBitmap(bmp: TMufasaBitmap);

Remove all but uptext colours clWhite,clGreen, etc.

This assumes that the bitmap only consists of colour 0, and the other
constants founds above the functions

getTextPointsIn
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.getTextPointsIn(sx, sy, w, h: Integer; shadow: boolean;
                               var _chars, _shadows: T2DPointArray): Boolean;

This uses the two filters, and performs a split on the bitmap.
A split per character, that is. So we can more easily identify it.

TODO:
  *
    Remove more noise after we have split, it should be possible to identify
    noise; weird positions or boxes compared to the rest, etc.
  *
    Split each colours seperately, and combine only later, after removing noise.

GetUpTextAtEx
~~~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.GetUpTextAtEx(atX, atY: integer; shadow: boolean): string;


GetUpTextAtEx will identify each character, and also keep track of the previous
chars' final *x* bounds. If the difference between the .x2 of the previous
character and the .x1 of the current character is bigger than 5, then there
was a space between them. (Add ' ' to result)

GetUpTextAt
~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.GetUpTextAt(atX, atY: integer; shadow: boolean): string;

Retreives the (special) uptext.


GetTextATPA
~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.GetTextATPA(const ATPA : T2DPointArray;const maxvspacing : integer; font: string): string;

Returns the text defined by the ATPA. Each TPA represents one character,
approximately.

GetTextAt
~~~~~~~~~

.. code-block:: pascal

    function TMOCR.GetTextAt(xs, ys, xe,ye, minvspacing, maxvspacing, hspacing,
                               color, tol: integer; font: string): string;

General text-finding function.

GetTextAt (2)
~~~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing,
                               color, tol, len: integer; font: string): string;

General text-finding function. Different parameters than other GetTextAt.

TextToFontTPA
~~~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.TextToFontTPA(Text, font: String; out w, h: integer): TPointArray;

Returns a TPA of a specific *Text* of the specified *Font*.


TextToFontBitmap
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function TMOCR.TextToFontBitmap(Text, font: String): TMufasaBitmap;

Returns a Bitmap of the specified *Text* of the specified *Font*.


.. _uptext-filter:

Uptext
======

To read the UpText, the TMOCR class applies several filters on the client data
before performing the actual OCR. We will take a look at the two filters first.

Filter 1: The Colour Filter
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We first filter the raw client image with a very rough and tolerant colour
comparison / check.
We first convert the colour to RGB, and if it falls into the following
defined ranges, it may be part of the uptext. We also get the possible
shadows.


We will iterate over each pixel in the bitmap, and if it matches any of the
*rules* for the colour; we will set it to a constant colour which
represents this colour (and corresponding rule). Usually the *base*
colour. If it doesn't match any of the rules, it will be painted black.
We won't just check for colours, but also for differences between specific
R, G, B values. For example, if the colour is white; R, G and B should all
lie very close to each other. (That's what makes a colour white.)

The tolerance for getting the pixels is quite large. The reasons for the
high tolerance is because the uptext colour vary quite a lot. They're also
transparent and vary thus per background.
We will store/match shadow as well; we need it later on in filter 2.

To my knowledge this algorithm doesn't remove any *valid* points. It does
not remove *all* invalid points either; but that is simply not possible
based purely on the colour. (If someone has a good idea, let me know)

In code:

.. code-block:: pascal

    for y := 0 to bmp.Height - 1 do
      for x := 0 to bmp.Width - 1 do
      begin
        colortorgb(bmp.fastgetpixel(x,y),r,g,b);
    
        if (r < ocr_Limit_Low) and (g < ocr_Limit_Low) and
            (b < ocr_Limit_Low) then
        begin
          bmp.FastSetPixel(x,y, ocr_Purple);
          continue;
        end;
    
        // Black if no match
        bmp.fastsetpixel(x,y,0);
      end;

Filter 2: The Characteristics Filter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This second filter is easy to understand but also very powerful:

    -   It removes *all* false shadow pixels.
    -   It removes uptext pixels that can't be uptext according to specific
        rules. These rules are specifically designed so that it will never
        throw away proper points.

It also performs another filter right at the start, but we'll disregard that
filter for now.

Removing shadow points is trivial if one understands the following insight.

If there some pixel is shadow on *x, y*, then it's neighbour *x+1, y+1*
may not be a shadow pixel. A shadow is always only one pixel *thick*.

With this in mind, we can easily define an algorithm which removes all false
shadow pixels. In code:

.. code-block:: pascal

    {
        The tricky part of the algorithm is that it starts at the bottom,
        removing shadow point x,y if x-1,y-1 is also shadow. This is
        more efficient than the obvious way. (It is also easier to implement)
    }

    for y := bmp.Height - 1 downto 1 do
      for x := bmp.Width - 1 downto 1 do
      begin
        // Is it shadow?
        if bmp.fastgetpixel(x,y) <> clPurple then
          continue;
        // Is the point at x-1,y-1 shadow? If it is
        // then x, y cannot be shadow.
        if bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x-1,y-1) then
        begin
          bmp.fastsetpixel(x,y,clSilver);
          continue;
        end;
        if bmp.fastgetpixel(x-1,y-1) = 0 then
          bmp.fastsetpixel(x,y,clSilver);
      end;

We are now left with only proper shadow pixels.
Now it is time to filter out false Uptext pixels.

Realize:

    -   If *x, y* is uptext, then *x+1, y+1* must be either uptext or shadow.

In code:

.. code-block:: pascal

    for y := bmp.Height - 2 downto 0 do
      for x := bmp.Width - 2 downto 0 do
      begin
        if bmp.fastgetpixel(x,y) = clPurple then
          continue;
        if bmp.fastgetpixel(x,y) = clBlack then
          continue;

        // Is the other pixel also uptext?
        // NOTE THAT IT ALSO HAS TO BE THE SAME COLOUR
        // UPTEXT IN THIS CASE.
        // I'm still not sure if this is a good idea or not.
        // Perhaps it should match *any* uptext colour.
        if (bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x+1,y+1) ) then
          continue;

        // If it isn't shadow (and not the same colour uptext, see above)
        // then it is not uptext.
        if bmp.fastgetpixel(x+1,y+1) <> clPurple then
        begin
          bmp.fastsetpixel(x,y,clOlive);
          continue;
        end;

       // If we make it to here, it means the pixel is part of the uptext.
      end;

Identifying characters
~~~~~~~~~~~~~~~~~~~~~~

.. note::
    This part of the documentation is a bit vague and incomplete.

To actually identify the text we split it up into single character and then
pass each character to the OCR engine.

In the function *getTextPointsIn* we will use both the filters mentioned above.
After these have been applied, we will make a bitmap that only contains the 
shadows as well as a bitmap that only contains the uptext chars (not the 
shadows)

Now it is a good idea to count the occurances of all colours
(on the character bitmap); we will also use this later on.
To split the characters we use the well known *splittpaex* function.

We will then sort the points for in each character TPA, as this makes
makes looping over them and comparing distances easier. We will also
calculate the bounding box of each characters TPA.

.. note::
    Some more hackery is then used to seperate the characters and find
    spaces; but isn't yet documented here.

Normal OCR
----------

.. note::
    To do :-)
    A large part is already explained above.
    Most of the other OCR functions are simply used for plain identifying
    and have no filtering tasks.
