################
Mouse & Keyboard
################


Moving the mouse
""""""""""""""""

There are two ways to move the mouse, these are:

- :code:`TTarget.MouseTeleport` - Instantly changes the cursors position without taking a path.
- :code:`TTarget.MouseMove` - Moves the cursor using a randomized path like a human would.

For :code:`TTarget.MouseMove` there are variables to control the movement, these are:

- :code:`TTarget.MouseWind` - Strength pulling the position in random directions.
- :code:`TTarget.MouseGravity` - Strength pulling the position towards the destination.
- :code:`TTarget.MouseSpeed` - Speed of the mouse movement.

You can read a more in depth article about the algorithm used (WindMouse) `here <https://ben.land/post/2021/04/25/windmouse-human-mouse-movement>`_

-----

Clicking the mouse
""""""""""""""""""

The mouse can be held, released or pressed using:

- :code:`TTarget.MouseDown`
- :code:`TTarget.MouseUp`
- :code:`TTarget.MousePress`

These methods use :code:`EMouseButton` enum.

The following will press the left mouse button for 50 milliseconds.

.. code-block::

  Target.MouseDown(EMouseButton.LEFT);
  Sleep(50);
  Target.MouseUp(EMouseButton.LEFT);

:code:`TTarget.MouseClick` will do above but will use :code:`TTarget.MouseOptions.MinClickTime` and :code:`TTarget.MouseOptions.MaxClickTime` to determine the sleep time.

- :code:`TTarget.MouseOptions.MinClickTime` Minimum milliseconds to hold a mouse button.
- :code:`TTarget.MouseOptions.MaxClickTime` Maximum milliseconds to hold a mouse button.

.. code-block::

  Target.MouseClick(EMouseButton.LEFT);

-----

Keyboard typing
"""""""""""""""

:code:`TTarget.KeySend` will type text in a human like way, for example typing 'A' would hold the shift key down and at a customizable speed.

There are variables to control the speed. These are:

- :code:`TTarget.KeyOptions.MinPressTime` Minimum milliseconds to hold a key.
- :code:`TTarget.KeyOptions.MaxPressTime` Maximum milliseconds to hold a key.

Keys can be held, released or pressed using:

- :code:`TTarget.KeyDown`
- :code:`TTarget.KeyUp`
- :code:`TTarget.KeyPress`

These methods use :code:`EKeyCode` enum.

The following will press shift for 50 milliseconds.

.. code-block::

  Target.KeyDown(EKeyCode.SHIFT)
  Sleep(50);
  Target.KeyUp(EKeyCode.SHIFT);

:code:`TTarget.KeyPress` will do above but will use :code:`Input.MinPressTime` and :code:`Input.MaxPressTime` to determine the sleep time.

- :code:`TTarget.KeyOptions.MinPressTime` Minimum milliseconds to hold a button.
- :code:`TTarget.KeyOptions.MaxPressTime` Maximum milliseconds to hold a button.

.. code-block::

  Target.KeyPress(EKeyCode.SHIFT);

-----

Detecting the state of a button
"""""""""""""""""""""""""""""""

To detect if a button is pressed use :code:`Input.KeyPressed` or :code:`Input.MousePressed`

.. code-block::

  WriteLn Target.KeyPressed(EKeyCode.A);
  WriteLn Target.MousePressed(EMouseButton.LEFT)