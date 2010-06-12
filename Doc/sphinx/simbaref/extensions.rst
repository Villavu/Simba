Writing Simba Extensions
========================

Simba extensions are scripts written in PascalScript that can be embedded into
Simba. Purposes vary from updaters to full blown dtm and bitmap editors.

.. FIXME link to dtm and bitmap

How they work
-------------

Extensions are event based. This means you don't have some ``loop`` in your
program that never ends and does all the logic for you. When a system is event
based, you implement some functions and those are called on a certain event.

Extension core hooks
--------------------

Simba offers several core hooks: init, free, attach and detach. These are used
to initialize, show, hide and free your extension. GetName and GetVersion are
called to retreive the name and version of an extension.

init
~~~~

Called when the Extension is initialized. 

.. code-block:: pascal

    procedure init;
    begin;
        Writeln('Init your extension here.');
    end;    

If you want to add a button to the menu, do it now.
From the SRL updater extension:

.. code-block:: pascal
    :linenos:

    procedure Init;
    begin;
      MainMenuItem := TMenuItem.Create(Simba_MainMenu);  
      MainMenuItem.Caption := 'SRL';
      Simba_MainMenu.Items.Add(MainMenuItem);
    
      MenuCheck := TMenuItem.Create(MainMenuItem);
      MenuCheck.Caption := 'Check for new SRL';
      MenuCheck.OnClick := @OnSRLCheckClick;
      MainMenuItem.Add(MenuCheck);
    
      MenuUpdate := TMenuItem.Create(MainMenuItem);
      MenuUpdate.Caption := 'Update SRL';
      MenuUpdate.OnClick := @OnSRLUpdateClick;
      MainMenuItem.Add(MenuUpdate);
      
      AutoUpdate := TMenuItem.Create(MainMenuItem);
      AutoUpdate.Caption := 'Automatically update';
      AutoUpdate.OnClick := @SetAutoUpdate;
      AutoUpdate.Checked := LowerCase(Settings.GetKeyValueDef('AutoUpdate',
                                    'True')) = 'true';
      MainMenuItem.Add(AutoUpdate);
     
      Timer := TTimer.Create(Simba);
      Timer.Interval := 5000;
      Timer.OnTimer := @OnUpdateTimer;
      Timer.Enabled :=AutoUpdate.Checked;
    
      started := True;
    end;     

free
~~~~

Called upon freeing the extension. Usually this means that Simba is closed.

.. code-block:: pascal
    
    procedure free;
    begin
        if started then
            writeln('Init was called');
    end;

From the SRL updater extension:

.. code-block:: pascal

    procedure Free;
    begin
      if (started) then
        Timer.Enabled := False;
        { Freeing the components is not needed, as they will be freed
           upon the freeing of Simba. }
    end;    

attach
~~~~~~

Called when your extension has been enabled.

From the SRL updater extension:

.. code-block:: pascal
      
    procedure Attach;
    begin;
      Writeln('From now on, you shall be alerted as to when your SRL is'+
                +'out of date!');
      MainMenuItem.Visible := True;
      Timer.Enabled := AutoUpdate.Checked;
    end;        



detach
~~~~~~

Called when your extension has been disabled. (This is not the same as freeing)

.. code-block:: pascal

    Procedure Detach;
    begin
      Timer.Enabled := False;
      MainMenuItem.Visible := False;
    end;    

GetName
~~~~~~~

Called when Simba requests the name of your extension.

.. code-block:: pascal

    function GetName : string;
    begin;
      result := 'SRL Updater';
    end;   

GetVersion
~~~~~~~~~~

Called when Simba requests the version of the extension.

.. code-block:: pascal

    function GetVersion : string;
    begin;
      result := '1.0';
    end;    

More extension hooks
~~~~~~~~~~~~~~~~~~~~

The following hooks are called upon if the event that the name describes has 
happened.

.. code-block:: pascal

    EventHooks: Array [0..8] of TEventHook =
    (      (HookName : 'onColourPick'    ; ArgumentCount : 3), 
       (HookName : 'onOpenFile'      ; ArgumentCount : 2), 
       (HookName : 'onWriteFile'     ; ArgumentCount : 2),
           (HookName : 'onOpenConnection'; ArgumentCount : 2), 
           (HookName : 'onScriptStart'   ; ArgumentCount : 2), 
           (HookName : 'onScriptCompile' ; ArgumentCount : 1),
       (HookName : 'onScriptExecute' ; ArgumentCount : 1),
       (HookName : 'onScriptPause'   ; ArgumentCount : 1),
       (HookName : 'onScriptStop'    ; ArgumentCount : 1));       

For the exact arguments to all these functions, refer to
:ref:`extension-example-code`.

onOpenConnection
~~~~~~~~~~~~~~~~

onWriteFile
~~~~~~~~~~~

onOpenFile
~~~~~~~~~~

onColourPick
~~~~~~~~~~~~

onScriptStart
~~~~~~~~~~~~~

Special cases
-------------

Multiple extensions hooking onto the same event
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So what happens when multiple extensions hook onto the same event/hook?

The behaviour is currently defined, but prone to change in the near future.
Currently simply all extensions are called. The order is simple the order they
were loaded. 

The behaviour will probably change to something like the following:

    In loading order, extensions are called with the proper hook. As soon as one
    extension returns something that is not equal to zero, it will bail and stop
    calling other events. Apart from not calling other events, the hook will not
    allow the action it is bound to.

Pitfalls
--------

Extensions can be very dangerous in the sense that they run on the main thread
of Simba, it is very simple to crash Simba or make it infinitely loop. There is
no way to prevent this, so make sure to check what you're doing before you try
your own (or someone else's extension)
    


.. _extension-example-code:

Example code
------------

Example code for most hooks:

.. literalinclude:: extraextensionhooks.sex
    :language: pascal
    :linenos:

.. note::

    If you need more examples, you can always look at the Extensions in the
    Simba git repository. 
