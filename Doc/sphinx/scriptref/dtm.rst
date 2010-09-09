
.. _scriptref_dtm:

Deformable Template Models (DTM)
==============================

DTMFromString
-------------

.. code-block:: pascal

    function DTMFromString(const DTMString: String): Integer;


SetDTMName
----------

.. code-block:: pascal

    procedure SetDTMName(DTM : integer;const name : string);


FreeDTM
-------

.. code-block:: pascal

    procedure FreeDTM(DTM: Integer);


FindDTM
-------

.. code-block:: pascal

    function FindDTM(DTM: Integer; var x, y: Integer;
    xs, ys, xe, ye: Integer): Boolean;


FindDTMs
--------

.. code-block:: pascal

    function FindDTMs(DTM: Integer; var p: TPointArray;
    xs, ys, xe, ye: Integer): Boolean;


FindDTMRotatedSE
----------------

.. code-block:: pascal

    function FindDTMRotatedSE(DTM: Integer; var x, y: Integer;
    xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended;
    var aFound: Extended): Boolean;


FindDTMRotatedAlternating
-------------------------

.. code-block:: pascal

    function FindDTMRotatedAlternating(DTM: Integer; var x, y: Integer;
    xs, ys, xe, ye: Integer;
    sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean;


FindDTMsRotatedSE
-----------------

.. code-block:: pascal

    function FindDTMsRotatedSE(DTM: Integer; var Points: TPointArray; 
    xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended;
    var aFound: T2DExtendedArray) : Boolean;


FindDTMsRotatedAlternating
--------------------------

.. code-block:: pascal

    function FindDTMsRotatedAlternating(DTM: Integer; 
    var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: 
    Extended; var aFound: T2DExtendedArray) : Boolean;


AddMDTM
-------

.. code-block:: pascal

    function AddMDTM(const d: TMDTM): Integer;


AddDTM
------

.. code-block:: pascal

    function AddDTM(const d: TMDTM): Integer;


AddSDTM
-------

.. code-block:: pascal

    function AddSDTM(const d: TSDTM): Integer;


GetDTM
------

.. code-block:: pascal

    function GetDTM(index: Integer) : TMDTM


SDTMToMDTM
----------

.. code-block:: pascal

    function SDTMToMDTM(Const DTM: TSDTM): TMDTM;


PrintDTM
--------

.. code-block:: pascal

    procedure PrintDTM(const DTM : TMDTM);


MDTMToSDTM
----------

.. code-block:: pascal

    function MDTMToSDTM(Const DTM: TMDTM): TSDTM;


CreateDTMPoint
--------------

.. code-block:: pascal

    function CreateDTMPoint(x,y,c,t,asz : integer; bp : boolean) : TMDTMPoint;


