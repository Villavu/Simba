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


.. _scriptref_CTS:

Colour tolerance
----------------

Simba contains several algorithms for determining if two colours are equal
given a tolerance. There are three algorithms:

..  XXX FIXME COMPLETE ME

.. note::
    This is a stub and needs to be expanded.

    *   CTS 0:    
    *   CTS 1:
    *   CTS 2:
