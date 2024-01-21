################
Mouse & Keyboard
################

Simba provides a :code:`Input` variable for methods relating to input.

----

Moving the mouse
""""""""""""""""

There are two ways to move the mouse, these are:

- :code:`Input.MouseTeleport` - Instantly changes the cursors position without taking a path.
- :code:`Input.MouseMove` - Moves the cursor using a randomized path like a human would.

For :code:`Input.MouseMove` there are variables to control the movement, these are:

- :code:`Input.MouseWind` - Strength pulling the position in random directions.
- :code:`Input.MouseGravity` - Strength pulling the position towards the destination.
- :code:`Input.MouseSpeed` - Speed of the mouse movement.

You can read a much more in depth article about the algorithm used (WindMouse) `here <https://ben.land/post/2021/04/25/windmouse-human-mouse-movement>`_

-----

Clicking the mouse
""""""""""""""""""

The mouse can be held, released or pressed using:

- :code:`Input.MouseDown`
- :code:`Input.MouseUp`
- :code:`Input.MousePress`

These methods use :code:`EMouseButton` enum.

The following will press the left mouse button for 50 milliseconds.

.. code-block::

  Input.MouseDown(EMouseButton.LEFT);
  Sleep(50);
  Input.MouseUp(EMouseButton.LEFT);

:code:`Input.MousePress` will do above but will use :code:`Input.MouseClickMin` and :code:`Input.MouseClickMax` to determine the sleep time.

- :code:`Input.MouseClickMin` Minimum milliseconds to hold a mouse button.
- :code:`Input.MouseClickMax` Maximum milliseconds to hold a mouse button.

.. code-block::

  Input.MouseClick(EMouseButton.LEFT);

-----

Keyboard typing
"""""""""""""""

:code:`Input.KeySend` will type text in a human like way, for example typing 'A' would hold the shift key down and at a customizable speed.

There are variables to control the speed. These are:

- :code:`Input.KeyPressMin` Minimum milliseconds to hold a key.
- :code:`Input.KeyPressMax` Maximum milliseconds to hold a key.

Keys can be held, released or pressed using:

- :code:`Input.KeyDown`
- :code:`Input.KeyUp`
- :code:`Input.KeyPress`

These methods use :code:`EKeyCode` enum.

The following will press shift for 50 milliseconds.

.. code-block::

  Input.KeyDown(EKeyCode.SHIFT)
  Sleep(50);
  Input.KeyUp(EKeyCode.SHIFT);

:code:`Input.KeyPress` will do above but will use :code:`Input.KeyPressMin` and :code:`Input.KeyPressMax` to determine the sleep time.

- :code:`Input.KeyPressMin` Minimum milliseconds to hold a button.
- :code:`Input.KeyPressMax` Maximum milliseconds to hold a button.

.. code-block::

  Input.KeyPress(EKeyCode.SHIFT);

-----

Detecting the state of a button
"""""""""""""""""""""""""""""""

To detect if a button is pressed use :code:`Input.KeyPressed` or :code:`Input.MousePressed`

.. code-block::

  WriteLn Input.KeyPressed(EKeyCode.A);
  WriteLn Input.MousePressed(EMouseButton.LEFT)