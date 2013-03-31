
.. _scriptref_ocr:

OCR 
===
Simba has OCR functions (Optical Text Recognition); these are used to *read* text from an image.

It also has a wide variation of text *finding* functions. Both will be covered in
this article.


Fonts
-----

Fonts are an essential part of the text finding and identifying.
Now follows a brief explanation of the Font related functions in Simba.

All fonts have an unique identifier in the shape of a ``string``.
If you create or load a new font, they will need to be given a name.

LoadSystemFont
~~~~~~~~~~~~~~

.. _scriptref_fonts:

.. code-block:: pascal

    function LoadSystemFont(const SysFont: TFont; const FontName: string):
    boolean;

This font loads a previously created Font with the name specified by
*FontName*.

Example:

.. code-block:: pascal

    program new;
    var
      Font : TFont;
    begin
      Font := TFont.Create;
      Font.Name := 'Courier New';
      Font.Size := 10;
      Font.Style := [];
      LoadSystemFont(Font,'test');
      DisplayDebugImgWindow(0,0);
      DisplayDebugImgWindow(150,50);
      DrawBitmapDebugImg(BitmapFromText('BMP[0] has not been freed','test'));
      Font.free;
    end.

LoadFont
~~~~~~~~

.. code-block:: pascal

    function LoadFont(const FontName: string; shadow: boolean): boolean;

Load the font specific by the *FontName*. The font has to recide in the Fonts
directory.

As of Simba version 993 and up, LoadFont also accepts paths outside
the Fonts directory. In this case *FontName* is thus an entire path; and the
internal font name is simply the name of the last folder in the path.

FreeFont
~~~~~~~~

.. code-block:: pascal

    function FreeFont(const FontName: string): boolean;

This function is used to free a font identified by *FontName*.


BitmapFromText
~~~~~~~~~~~~~~

.. code-block:: pascal

    function BitmapFromText(const text, font: String): integer;

This function creates a bitmap from a string *text* with the given *font*.
For an explanation on how to use and work with Bitmaps, please refer to 
:ref:`scriptref-bitmaps`.

TPAFromText
~~~~~~~~~~~

.. code-block:: pascal

    function TPAFromText(const text, font: String;var w,h : integer): TPointArray;

This function creates a TPA from a string *text* with the given *font*.
For an explanation on how to use and work with TPAs, please refer to 
:ref:`scriptref-tpointarray`.

TPAFromTextWrap
~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure TPAFromTextWrap(const text, font: String;var w,h : integer;var TPA : TPointArray);

A wrapper function for the previously mentioned function. Required to
work arounds bugs in the interpreter.

MaskFromText
~~~~~~~~~~~~

.. code-block:: pascal

    function MaskFromText(const text, font: String): TMask;

This function creates a Mask from a string *text* with the given *font*.
For an explanation on how to use and work with TPAs, please refer to 
:ref:`scriptref-masks`.

Reading Text
------------

rs_GetUpText
~~~~~~~~~~~~

.. code-block:: pascal

    function rs_GetUpText: string;

This function is a function specific to RuneScape(tm); it reads the text 
in the upper left corner into a string.

How these functions actually work can be found here: :ref:`uptext-filter`.

rs_GetUpTextAt
~~~~~~~~~~~~~~

.. code-block:: pascal

    function rs_GetUpTextAt(x, y : integer): string;

This function is a function specific to RuneScape(tm); it reads the text 
at the specified position in (*x*, *y*) into a string.

rs_GetUpTextAtEx
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function rs_GetUpTextAt(x, y : integer; shadow: boolean; fontname: string): string;

This function is a function specific to RuneScape(tm); it reads the text 
at the specified position in (*x*, *y*) into a string, optionally using the
shadows of the font. Fontname specifies the name of the font to use; with the
other functions this defaults to "UpChars".

GetTextAt
~~~~~~~~~

.. code-block:: pascal

    function GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing,color, tol, len: integer;const font: string): string;

A general function for reading text.
Reads text at (*atX*, *atY*) with a minimal vertical spacing of *minvspacing*
and a maximal vertical spacing of *maxvspacing*, the text colour should match
the colour *color* with the given tolerance *Tolerance*; the length of the text
is specified with *len*. Finally, the font to use for the identifying is
specified with the fontname *font*.

GetTextAtEx
~~~~~~~~~~~

.. code-block:: pascal

    function GetTextAtEx(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing,color, tol: integer;const font: string): string;

A general function for reading text.
Reads text in the rectangle defined by (*xs*, *ys*), (*xe*, *ye*)
with a minimal vertical spacing of *minvspacing*
and a maximal vertical spacing of *maxvspacing*, the text colour should match
the colour *color* with the given tolerance *Tolerance*; the length of the text
is specified with *len*. Finally, the font to use for the identifying is
specified with the fontname *font*.

GetTextATPA
~~~~~~~~~~~

.. code-block:: pascal

    function GetTextATPA(const ATPA : T2DPointArray; const maxvspacing : integer; const font : string): string;

Similar to GetTextAt but reads the text from a ATPA rather than the client.

GetTextAtExWrap
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetTextAtExWrap(const xs,ys,xe,ye, minvspacing, maxvspacing, hspacing,color, tol: integer;const font: string): string;

A wrapper function for the previously mentioned function. Required to
work arounds bugs in the interpreter.


Modifying the Uptext filter
---------------------------

rs_ResetUpTextFilter
~~~~~~~~~~~~~~~~~~~~

Reset the colours for the colour-filter to default.
See `rs_SetUpTextFilter`_ for an example.

rs_SetUpTextFilter
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure rs_SetUpTextFilter(filter: TOCRFilterDataArray);

Defines the colours that the colour-filter will look for.


Example:

.. code-block:: pascal

    program UpTextFilter;

    { Some constants for the OCR - taken directly from Simba }
    const
        { Very rough limits for R, G, B }
        ocr_Limit_High = 190;
        ocr_Limit_Med = 130;
        ocr_Limit_Low = 65;


        { `base' Colours of the Uptext }

        { White }
        ocr_White = 16777215;

        { Level < Your Level }
        ocr_Green = 65280;

        { Level > Your Level }
        ocr_Red = 255;

        { Interact or Level = Your Level }
        ocr_Yellow = 65535;

        { Object }
        ocr_Blue = 16776960;

        { Item }
        ocr_ItemC = 16744447;

        { Item }
        ocr_ItemC2 = ocr_Red or ocr_Green;

        { Shadow }
        ocr_Purple = 8388736;

    const
        OF_LN = 256;
        OF_HN = -1;


    { Helper function to easily load a struct }
    function load0(r_low,r_high,g_low,g_high,b_low,b_high,set_col: integer;
        is_text_color: boolean): tocrfilterdata;
    begin
      result.r_low := r_low;
      result.r_high := r_high;
      result.g_low := g_low;
      result.g_high := g_high;
      result.b_low := b_low;
      result.b_high := b_high;
      result.set_col := set_col;
      result._type := 0;
      result.is_text_color:= is_text_color;
    end;

    {
    Load our own ``filter data''. This particular set doesn't contain the item
    colours - those are replaced with extra (effectively nill as they already
    exist) green colours.
    }
    rocedure foo;
    var filterdata: TOCRFilterDataArray;
    begin
      setlength(filterdata, 9);

      filterdata[0] := load0(65, OF_HN, OF_LN, 190, OF_LN, 190, ocr_Blue, True); // blue
      filterdata[1] := load0(65, OF_HN, OF_LN, 190, 65, OF_HN, ocr_Green, True); // green

      // ``False'' item
      filterdata[2] := load0(65, OF_HN, OF_LN, 190, 65, OF_HN, ocr_Green, True); // green

      { This is the real one }
      //filterdata[2] := load0(OF_LN, 190, 220, 100, 127, 40, ocr_ItemC, True); // itemC

      filterdata[3] := load0(OF_LN, 190, OF_LN, 190, 65, OF_HN, ocr_Yellow, True); // yellow
      filterdata[4] := load0(OF_LN, 190, 65, OF_HN, 65, OF_HN, ocr_Red, True); // red
      filterdata[5] := load0(OF_LN, 190, OF_LN, 65, 65, OF_HN, ocr_Red, True); // red 2
      filterdata[6] := load0(190 + 10, 130, OF_LN, 65 - 10, 20, OF_HN, ocr_Green, True); // green 2

      // ``False'' item 2
      filterdata[7] := load0(65, OF_HN, OF_LN, 190, 65, OF_HN, ocr_Green, True);

      { This is the real one }
      //filterdata[7] := load0(190, 140, 210, 150, 200, 160, ocr_ItemC2, True); // item2, temp item_c
      filterdata[8] := load0(65, OF_HN, 65, OF_HN, 65, OF_HN, ocr_Purple, False); // shadow

      rs_SetUpTextFilter(filterdata);
    end;


    var
      bmp: integer;
    begin
      bmp := LoadBitmap('uptext.png');
      SetTargetBitmap(bmp);

      writeln( rs_GetUpTextAt(0, 0) );

      foo;

      writeln( rs_GetUpTextAt(0, 0) );

      rs_ResetUpTextFilter;

      writeln( rs_GetUpTextAt(0, 0) );

      SetDesktopAsClient;
      FreeBitmap(bmp);
    end.
