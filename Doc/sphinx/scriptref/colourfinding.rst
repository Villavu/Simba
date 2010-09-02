..  _scriptref_colour:


Colour Finding
==============

Finding colours on the screen is quite simple. Simba offers methods like
``FindColor`` to locate colours on the screen.

These methods are usually composed out of several (but not always all) 
components:

    *   The colour to search for.
    *   An area to search in, defined by *x1*, *y1*, *x2*, *y2*.
    *   Tolerance applied to the colour matching. With a maximum tolerance all
        colours are matched.
    *   Spiral. A spiral defines a point where the search will start from.
        This is particulary useful if you want the first result near specific
        coordinates.
    *   AreaSize. The size the box of colours should be. Usually this is not
        adjustable.
    *   A single point in *x*, *y* can be returned, or a set or points called
        a *TPointArray*.

.. note::
    Other techniques exist, which involve relative point distance from one point
    to another; these are found in the :ref:`_scriptref_DTM` section.

.. note::

    Although the documentation uses the ``English`` spelling of 
    ``colour``; the code for compatibility sake uses ``color``, without the u.


Colour Finding Methods
----------------------

A list of all colour finding methods in Simba.

SimilarColors
~~~~~~~~~~~~~

.. code-block:: pascal

    function SimilarColors(C1, C2, Tolerance: Integer): Boolean;

SimilarColors returns true if the two passed colours are *similar* given the
passed tolerance. 

GetColor
~~~~~~~~

.. code-block:: pascal

    function GetColor(x, y: Integer): Integer;

GetColor returns the color on the coordinate (*x*, *y*).

*Example: Printing the colour on coordinate (25, 25)*

.. code-block:: pascal

    Writeln('Colour is ' + IntToStr(GetColor(25, 25)))

GetColors
~~~~~~~~~

.. code-block:: pascal

    function GetColors(const Coords : TPointArray) : TIntegerArray;

GetColor returns the color on the coordinate (x, y) defined by *Coords*.


CountColor
~~~~~~~~~~

.. code-block:: pascal

    function CountColor(Color, xs, ys, xe, ye: Integer): Integer;

Returns how many times *Color* occurs in the area defined by (*xs*, *ys*), 
(*xe*, *ye*)

CountColorTolerance
~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;

Returns how many times *Color* occurs (within *Tolerance*)
in the area defined by (*xs*, *ys*), (*xe*, *ye*)

FindColor
~~~~~~~~~

.. code-block:: pascal

    function FindColor(var x, y: Integer; col, x1, y1, x2, y2: Integer): 
    Boolean;


FindColor returns true if the exact colour given (col) is found in the box
defined by *x1*, *y1*, *x2*, *y2*.
The point is returned in *x* and *y*.
It searches from the top left to the bottom right and will stop
after matching a point.

FindColorTolerance
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorTolerance(var x, y: Integer; col, x1, y1, x2, y2, tol: 
    Integer): Boolean; 

FindColorTolerance returns true if a colour within the given tolerance range 
*tol* of the given colour *col* is found in the box defined by *x1*, *y1*,
*x2*, *y2*.
Only the first point is returned in *x* and *y*.
Whether or not a colour is within the tolerance range is determined by the 
:ref:`scriptref_CTS` mode. It searches from the top left to the bottom right
and will stop after matching a point.

FindColors
~~~~~~~~~~

.. code-block:: pascal

    function FindColors(var pts: TPointArray; col, x1, y1, x2, y2): Boolean;

FindColors returns a list of all points that match the colour *col* in an area
defined by *x1*, *y1*, *x2*, *y2*. It returns true if one or more points have
been found.

FindColorsTolerance
~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorsTolerance(var pts: TPointArray; col, x1, y1, x2, y2, 
    tol: Integer): Boolean; 

FindColorsTolerance returns true if at least one point was found.
A point is found if it is within the given tolerance range *tol* 
of the given colour *col* and inside the box defined by *x1*, *y1*, *x2*, *y2*.
Whether or not a color is within the tolerance range is determined by the 
:ref:`scriptref_CTS` mode.
It searches from the top left to the bottom right and will find all
matching points in the area.

FindColorSpiral
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorSpiral(var x, y: Integer; color, xs,ys,xe,ye:Integer):
    Boolean;

Same as FindColor, but starts searching from *x*, *y*.

FindColorSpiralTolerance
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorToleranceSpiral(var x, y: Integer; color,
    xs,ys,xe,ye,tolerance:Integer): Boolean

Same as FindColorTolerance, but starts searching from *x*, *y*.

FindColorsSpiralTolerance
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorsSpiralTolerance(x, y: Integer;
    var pts: TPointArray; col, x1, y1, x2, y2, tol: Integer): Boolean; 

Same as FindColorsTolerance, but starts searching from *x*, *y*.

Find areas of colours
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye,
    MinArea: Integer): Boolean;

FindColoredArea finds an area that consists out of *Color* and has a minimal
size of *MinArea*. If you want minimal area of 5x5 pixels (25), then set MinArea
to 25.

.. code-block:: pascal

    function FindColoredAreaTolerance(var x, y : Integer; color, xs, ys, xe,
    ye, MinArea, Tolerance : Integer): Boolean;

FindColoredArea finds an area that consists out of Colours that match *Color* with
the given *Tolerance* and has a minimal size of *MinArea*.
If you want minimal area of 5x5 pixels (25), then set MinArea to 25.

.. _scriptref_CTS:

Colour tolerance
----------------

Simba contains several algorithms for determining if two colours are equal
given a tolerance. There are three algorithms, from fastest to slowest:

    *   CTS 0: Quick and dirty comparison. Matches if the differences between the 
        three RGB values are <= Tolerance

    *   CTS 1: RGB comparison that uses the Pythagorean distance in the RGB cube
        to define tolerance. Matches if the distance <= Tolerance.

    *   CTS 2: HSL comparison. It has two modifiers that modify the
        result tolerance, Hue and Saturation. The lower the modifier, the higher
        tolerance required for a match. They can be set seperately and therefore
        used to distinguish very specific colours. Some differ a lot in saturation, but
        very little in hue. Luminance is assigned a somewhat static function, and
        has no modifier.


Get and Set Colour Tolerance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetColorToleranceSpeed(cts: integer);

Set the current colour tolerance speed. Valid values are: 0, 1 and 2.
Somewhat improperly named compared to the other CTS functions.

.. code-block:: pascal

    SetColorToleranceSpeed(2);

And the proper way to get the current tolerance is to use the following
function, which returns the current colour tolerance speed:

.. code-block:: pascal

    function GetToleranceSpeed: Integer;

*Example: Printing the Color Tolerance*

.. code-block:: pascal

    Writeln(Format('Tolerance Speed = %d', [GetToleranceSpeed]))

Get And Set Colour Modifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetToleranceSpeed2Modifiers(nHue, nSat: Extended);

Set colour speed 2 modifiers.

.. code-block:: pascal

    // 42.0 is a very high value, but this doesn't matter as this code is
    // only meant to illustrate how to use this function
    SetToleranceSpeed2Modifiers(42.0, 0.4)

The following function
    
.. code-block:: pascal

    procedure GetToleranceSpeed2Modifiers(var hMod, sMod: Extended);

returns colour speed 2 modifiers.

*Example: Getting the modifiers*

.. code-block:: pascal

    procedure WriteModifiers;
    var
        H, S: Extended;
    begin
        GetToleranceSpeed2Modifiers(H, S);
        Writeln(format('H = %f; S = %f', [H, S])); 
    end;

