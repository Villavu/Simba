
.. _scriptref_window:

Target Window Functions
=======================

Freeze
------

.. code-block:: pascal

    function Freeze: boolean;


Unfreeze
--------

.. code-block:: pascal

    function Unfreeze: boolean;


GetClientDimensions
-------------------

.. code-block:: pascal

    procedure GetClientDimensions(var w, h:integer);


SetTargetBitmap
---------------

.. code-block:: pascal

    function SetTargetBitmap(Bitmap : integer): integer;


SetTargetArray
--------------

.. code-block:: pascal

    function SetTargetArray(P: Integer; w, h: integer): integer;


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


ActivateClient
--------------

.. code-block:: pascal

    procedure ActivateClient;


IsTargetValid
-------------

.. code-block:: pascal

    function IsTargetValid: boolean;


