
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
:ref:`scriptref_bitmaps`.

TPAFromText
~~~~~~~~~~~

.. code-block:: pascal

    function TPAFromText(const text, font: String;var w,h : integer): TPointArray;

This function creates a TPA from a string *text* with the given *font*.
For an explanation on how to use and work with TPAs, please refer to 
:ref:`scriptref_tpointarray`.

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
:ref:`scriptref_masks`.

Reading Text
------------

rs_GetUpText
~~~~~~~~~~~~

.. code-block:: pascal

    function rs_GetUpText: string;

This function is a function specific to RuneScape(tm); it reads the text 
in the upper left corner into a string.

rs_GetUpTextAt
~~~~~~~~~~~~~~

.. code-block:: pascal

    function rs_GetUpTextAt(x, y : integer): string;

This function is a function specific to RuneScape(tm); it reads the text 
at the specified position in (*x*, *y*) into a string.

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
