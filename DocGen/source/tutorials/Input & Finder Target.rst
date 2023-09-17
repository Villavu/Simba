#####################
Input & Finder Target
#####################

Both :code:`Input` and :code:`Finder` records have a :code:`Target` field.
If this is not set (is default value) the global :code:`Target` variable is used. 

-----

These examples will use the global :code:`Target` variable. This by default is set to Simba's target window when the script was started, or the desktop if no window has been selected. 

.. code-block::

  Input.MouseTeleport([100,100]);

Of course you can also change the global :code:`Target` variable and the Input/Finder variables will keep updated.

.. code-block::

  Target.SetWindow(some_window_handle);
  Input.MouseTeleport([100,100]);

-----

In these examples since :code:`Input.Target` has a value that target is used.

.. code-block::

  Input.Target.SetWindow(some_window_handle);
  Input.MouseTeleport([100,100]); 

You can also declare as many Input/Finder variables as you like, all with different targets.

.. code-block::

  var
    MyOtherInput: TInput;
  begin
    MyOtherInput.Target.SetWindow(123456);
    MyOtherInput.MouseTeleport([100,100]);
  end; 

-----

You can clear the :code:`Input.Target` variable so the global :code:`Target` variable is once again used.

.. code-block::

  begin
    Input.Target := [];
    Input.MouseTeleport([100,100]);
  end; 
