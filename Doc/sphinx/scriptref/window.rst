.. _scriptref-window:

Target Window
=============

Target Window Functions
-----------------------

Freeze
~~~~~~

.. code-block:: pascal

    function Freeze: boolean;


If you call Freeze, the data that is *currently* in the client
is stored into memory. Simba will then target this memory for all further
finding operations; until *Unfreeze* is called. This can dramatically increase
speed if you don't care if the image doesn't change. It can be even more
important if you don't *want* the image to change; if you want to analyze a
specific frame.

Use like:

.. code-block:: pascal

    Freeze;

    if findcolors(...) then
      ...

    Unfreeze;

Make sure you never forget to call Unfreeze!

Unfreeze
~~~~~~~~

.. code-block:: pascal

    function Unfreeze: boolean;

Unfreeze the client data and restore the original client. See *Freeze* for more
details.

GetClientDimensions
~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure GetClientDimensions(var w, h:integer);

Return the size of the client in *w* and *h*.
If it fails, it returns -1 for both h and w.

GetClientPosition
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure GetClientPosition(var left, top:integer);

Return the position of the client in *left* and *top*.
May return negative values.


SetTargetBitmap
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function SetTargetBitmap(Bitmap : integer): integer;

Set a bitmap as target / client. (The bitmap must be loaded by Simba)
This can be combined with the SetPersistentMemoryBitmap feature to achieve
the same effect as `SetTargetArray`_.


SetTargetArray
~~~~~~~~~~~~~~

.. code-block:: pascal

    function SetTargetArray(P: Integer; w, h: integer): integer;

Set a target array as client data. This is generally not something you'd
want to call yourself. It is mainly included for external components to allow
Simba to efficiently target its memory. See the SMART source on how to do this.


SetEIOSTarget
~~~~~~~~~~~~~

.. code-block:: pascal

    function SetEIOSTarget(name: string; initargs: Variant): integer;


.. _image-target:

SetImageTarget
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetImageTarget(idx: integer);

Set the Image target defined by index *idx* as active target.
An Image target controls what data Simba performs color (and bitmap, dtm, etc)
searches on.

Both `SetTargetBitmap`_, and `SetTargetArray`_ return a target index.
Alternatively you can get the index of the current target with `GetImageTarget`_.


.. _mouse_target:

SetKeyMouseTarget
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetKeyMouseTarget(idx: integer);

Set the KeyMouse target defined by index *idx* as active target.
A KeyMouse target controls how Simba moves the mouse cursor and emulates the
keyboard.

GetImageTarget
~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetImageTarget: integer;

Returns the current Image target.


GetKeyMouseTarget
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetKeyMouseTarget: integer;

Returns the current KeyMouse target.

ExportImageTarget 
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function ExportImageTarget : TTarget_Exported;


ExportKeyMouseTarget 
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function ExportKeyMouseTarget : TTarget_Exported;


FreeTarget
~~~~~~~~~~

.. code-block:: pascal

    procedure FreeTarget(idx: integer);

Free a previously loaded target.

This procedure does not free the data associated with the target as in the
case of `SetTargetBitmap`_ or `SetTargetArray`_.

SetDesktopAsClient
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetDesktopAsClient;

Set the default desktop as client.

ActivateClient
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ActivateClient;

Set the current target as active for key input.


IsTargetValid
~~~~~~~~~~~~~

.. code-block:: pascal

    function IsTargetValid: boolean;

Returns true if the current target is valid.

GetNativeWindow
~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetNativeWindow: Integer

Finding a specific window
-------------------------

GetProcesses
~~~~~~~~~~~~

.. code-block:: pascal

    function GetProcesses: TSysProcArr;

Returns processes with the title of their window, the handle of the window, the
process id and their width and height.

With TSysProc being defined as:

.. code-block:: pascal

    TSysProc = record
        Title: string;
        Handle: integer;
        Pid: integer;
        Width, Height: integer;
    end;

Example usage:

.. code-block:: pascal

    function FindAndSetTarget(TitlePrefix: String; SetAsTarget: Boolean): Boolean;
    var
      T: TSysProcArr;
      I: Integer;
    begin
      T:= GetProcesses();
      for I := 0 to high(T) do
        if ExecRegExpr('^' + TitlePrefix, T[i].Title) then
        begin
          Result := True;
          if SetAsTarget then
          begin
            SetTarget(T[i]);
            ActivateClient;
          end;
        end;
    end;

Client Area
-----------

Client Areas were introduced to cope with clients which have a normal
coordinate system, but a variable base for this coordinate system.

More specifically: client areas allow you to transparently add a certain X and Y
to all the mouse and image (finding, dtm, etc) functions.

Support for Mouse and Image targets have been separated. This is required for
targets that only support say, an Image target.

In this case you do not want to accidentally touch or reset the Mouse target
area.

Setting an area multiple times is the same as resetting it and then setting
the area. Multiple calls to \*SetClientArea will not result in nested areas.

MouseSetClientArea
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function MouseSetClientArea(x1, y1, x2, y2: integer): boolean;

Define a new `Client Area`_ for all Mouse operations on this Mouse target.

MouseResetClientArea
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure MouseResetClientArea;

Reset the `Client Area`_ for the Mouse Target.

ImageSetClientArea
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function ImageSetClientArea(x1, y1, x2, y2: integer): boolean;

Define a new `Client Area`_ for all Image operations on this Image Target.

ImageResetClientArea
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ImageResetClientArea;

Reset the `Client Area`_ for the Image Target.

