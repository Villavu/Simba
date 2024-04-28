#######
Targets
#######

The :code:`TTarget` type many methods to detect and interact on a given target.
Targets can either be a window, image or provided by a plugin.

A global :code:`Target` variable is available which is set to Simba's target window when the script was started, or the desktop if no window has been selected.

This will move the teleport the mouse cursor to 100,100 relative to Simba's target window.

.. code-block::

  Target.MouseTeleport([100,100]);

-----

You can declare as many TTarget variables as you like, all with different targets.

.. code-block::

  var
    MyOtherTarget: TTarget;
  begin
    MyOtherTarget.SetWindow(12345); // some window handle
    WriteLn MyOtherTarget.CountColor($0000FF, 10); // Count a red color, with 10 tolerance.
  end;