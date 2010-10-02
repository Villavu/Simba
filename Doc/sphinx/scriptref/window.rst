
.. _scriptref_window:

Target Window Functions
=======================

Freeze
------

.. code-block:: pascal

    function Freeze: boolean;


If you call Freeze, the data that is *currently* currently in the client
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

    Unfreeze

Make sure you never forget to call Unfreeze!

Unfreeze
--------

.. code-block:: pascal

    function Unfreeze: boolean;

Unfreeze the client data and restore the original client. See *Freeze* for more
details.

GetClientDimensions
-------------------

.. code-block:: pascal

    procedure GetClientDimensions(var w, h:integer);

Return the size of the client in *w* and *h*.


SetTargetBitmap
---------------

.. code-block:: pascal

    function SetTargetBitmap(Bitmap : integer): integer;

Set a bitmap as target / client. (It must be loaded by Simba)


SetTargetArray
--------------

.. code-block:: pascal

    function SetTargetArray(P: Integer; w, h: integer): integer;

Set a target array as client data. This is generally not something you'd
want to call yourself. It is mainly included for external components to allow
Simba to efficiently target its memory. See the SMART source on how to do this.


SetEIOSTarget
-------------

.. code-block:: pascal

    function SetEIOSTarget(name: string; initargs: Variant): integer;


SetImageTarget
--------------

.. code-block:: pascal

    procedure SetImageTarget(idx: integer);


SetKeyMouseTarget
-----------------

.. code-block:: pascal

    procedure SetKeyMouseTarget(idx: integer);


GetImageTarget
--------------

.. code-block:: pascal

    function GetImageTarget: integer;


GetKeyMouseTarget
-----------------

.. code-block:: pascal

    function GetKeyMouseTarget: integer;


ExportImageTarget 
------------------

.. code-block:: pascal

    function ExportImageTarget : TTarget_Exported;


ExportKeyMouseTarget 
---------------------

.. code-block:: pascal

    function ExportKeyMouseTarget : TTarget_Exported;


FreeTarget
----------

.. code-block:: pascal

    procedure FreeTarget(idx: integer);


SetDesktopAsClient
------------------

.. code-block:: pascal

    procedure SetDesktopAsClient;

Set the default desktop as client.

ActivateClient
--------------

.. code-block:: pascal

    procedure ActivateClient;

Set the current target as active for key input.


IsTargetValid
-------------

.. code-block:: pascal

    function IsTargetValid: boolean;

Returns true if the current target is valid.

