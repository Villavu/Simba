Mouse
=====

Simba contains several functions to manipulate the mouse and keyboard.
Features range from clicking and moving the mouse to faking keypresses.

Types
-----

A few variables are exported for working with the Simba mouse functions.

TClickType, which defines the click type.

.. code-block:: pascal

    const
        mouse_Right = 0
        mouse_Left = 1
        mouse_Middle = 2

TMousePress, which defines if the mouse button is to be down or up.

.. code-block:: pascal

    TMousePress = (mouse_Down, mouse_Up);  

Mouse Functions
---------------

Simba's coordinate system is similar to most computer coordinate systems.
Coordinate *(0, 0)* is the top-left part of your selected window (the desktop by
default). To get to point *(5, 0)* we move five pixels to the right; to get to
*(5, 5)* we move five pixels to the right, and five pixels down.

Recall that the first value is *x*, the second value is *y*: *(x, y)*

..
    TODO: Picture?

MoveMouse
~~~~~~~~~

.. _scriptref-movemouse:

.. code-block:: pascal

    procedure MoveMouse(x, y: integer);

MoveMouse moves the mouse pointer to the specified x and y coordinates.

The following example will move the mouse to position *(10, 10)*; relative
to the selected client. (To get to point (10, 10) visually, recall that (0, 0)
is the *top left* part and to get to (10, 10) we move 10 pixels to the right,
and ten pixels down.)

.. code-block:: pascal

    Program MouseMove;

    begin
      MoveMouse(10, 10);
    end.


.. _scriptref-getmousepos:

GetMousePos
~~~~~~~~~~~

.. code-block:: pascal

    procedure GetMousePos(var x, y: integer);

GetMousePos returns the current position of the mouse in x and y.

The following example moves the mouse 1 pixel to the right, relative to its
current position:

.. code-block:: pascal

    Program MouseMoveRelative;

    var x, y: integer;

    begin
      GetMousePos(x, y);
      MoveMouse(x + 1, y);
    end.

.. _scriptref-holdmouse:

HoldMouse
~~~~~~~~~

.. code-block:: pascal

    procedure HoldMouse(x, y: Integer; clickType: TClickType);

HoldMouse holds the given mouse button specified by clickType down at the
specified *(x, y)* coordinate. If the mouse if not at the given
(*x, y)* yet, the mouse position will be set to *(x, y)*.

The following example holds the left mouse button and moves it one pixel
to the right relative to its current position.

.. code-block:: pascal

    program HoldMouse;

    var x, y: integer;

    begin
      GetMousePos(x, y);
      HoldMouse(x, y, mouse_Left);
      MoveMouse(x + 1, y);
    end.

.. _scriptref-releasemouse:

ReleaseMouse
~~~~~~~~~~~~

.. code-block:: pascal

    procedure ReleaseMouse(x, y: Integer; clickType: TClickType);

HoldMouse holds the given mouse button (clickType) down at the specified
x, y coordinate. If the mouse if not at the given x, y yet, the
mouse position will be set to x, y.

The following example holds the left mouse button and moves it one pixel
to the right and releases it to simulate a drag and drop motion.

.. code-block:: pascal

    program HoldMouseRelative;

    var x, y: integer;

    begin
      GetMousePos(x, y);
      HoldMouse(x, y, mouse_Left);
      MoveMouse(x + 1, y);
      GetMousePos(x, y);
      ReleaseMouse(x, y, mouse_Left);
    end.

.. _scriptref-clickmouse:

ClickMouse
~~~~~~~~~~

.. code-block:: pascal

    procedure ClickMouse(x, y: Integer; clickType: Integer):

ClickMouse performs a click with the given mouse button (clickType) at the
specified *(x, y)* coordinate. This ``click`` equals an immediate click, with no
wait between holding down and releasing the mouse button. To create a more
human-like effect, use the HoldMouse and ReleaseMouse functions.

The following example clicks the right mouse button at a specified point.

.. code-block:: pascal

    program ClickMouse;

    var x, y: integer;

    begin
      ClickMouse(x, y, mouse_Right);
    end.


