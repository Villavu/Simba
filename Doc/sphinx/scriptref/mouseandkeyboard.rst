Mouse and Keyboard
==================

Types
-----

A few variables are exported for working with Mufasa Mouse Functions.

TClickType, which defines the click type.

Mouse Functions
---------------

.. code-block:: pascal

    const
        mouse_Right = 0
        mouse_Left = 1
        mouse_Middle = 2

TMousePress, which defines if the mouse button is to be down or up.

.. code-block:: pascal

    TMousePress = (mouse_Down, mouse_Up);  

MoveMouse
~~~~~~~~~

.. code-block:: pascal
   
    procedure MoveMouse(x, y: integer);

MoveMouse moves the mouse pointer to the specified x and y coordinates.

GetMousePos
~~~~~~~~~~~

.. code-block:: pascal

    procedure GetMousePos(var x, y: integer);

GetMousePos returns the current position of the mouse in x and
y.


HoldMouse
~~~~~~~~~

.. code-block:: pascal

    procedure HoldMouse(x, y: Integer; clickType: TClickType);

HoldMouse holds the given mouse button (clickType) down at the specified
x, y coordinate. If the mouse if not at the given x, y yet, the mouse position
will be set to x, y.

ReleaseMouse
~~~~~~~~~~~~

.. code-block:: pascal

    procedure ReleaseMouse(x, y: Integer; clickType: TClickType);

HoldMouse holds the given mouse button (clickType) down at the specified
x, y coordinate. If the mouse if not at the given x, y yet, the
mouse position will be set to x, y.
