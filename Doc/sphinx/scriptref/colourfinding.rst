Colour Finding
==============

Finding colours on the screen is quite simple. Simba offers methods like
``FindColor`` to locate colours on the screen.

.. note::

    Although the documentation uses the ``English`` spelling of 
    ``colour``; the code for compatibility sake uses ``color``, without the u.

Colour Finding Methods
----------------------

A list of (not yet all) colour finding methods in Simba.

FindColor
~~~~~~~~~

.. code-block:: pascal

    function FindColor(var x, y: Integer; col, x1, y1, x2, y2: Integer): 
    Boolean;


FindColor returns true if the exact colour given (col) is found in the box defined by x1, y1, x2, y2.
The point is returned in x and y. It searches from the top left to the bottom right and will stop
after matching a point.

FindColorTolerance
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorTolerance(var x, y: Integer; col, x1, y1, x2, y2, tol: 
    Integer): Boolean; 

FindColorTolerance returns true if a colour within the given tolerance range 
(tol) of the given colour (col) is found in the box defined by x1, y1, x2, y2.
Only the first point is returned in x and y.
Whether or not a colour is within the tolerance range is determined by the :ref:`CTS` mode.
It searches from the top left to the bottom right and will stop after matching a point.


FindColorsTolerance
~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function FindColorsTolerance(var pts: TPointArray; col, x1, y1, x2, y2, 
    tol: Integer): Boolean; 

FindColorsTolerance returns true if at least one point was found. A point is found if it is within the
given tolerance range (tol) of the given color (col) and inside the box defined by x1, y1, x2, y2.
Whether or not a color is within the tolerance range is determined by the CTS mode.
It searches from the top left to the bottom right and will find all matching points in the area.
