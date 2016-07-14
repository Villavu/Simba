
.. _scriptref-other:

Miscellaneous
=============

Miscellaneous Functions
-----------------------

AddOnTerminate
~~~~~~~~~~~~~~

.. code-block:: pascal

    function AddOnTerminate(const proc: string; skipIfUserTerminated: Boolean = False): Boolean

ClearDebugImg
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ClearDebugImg

ClearDebug
~~~~~~~~~~

.. code-block:: pascal

    procedure ClearDebug

ConvertTime64
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ConvertTime64(Time: int64; var y, m, w, d, h, min, s: Integer)

ConvertTime
~~~~~~~~~~~

.. code-block:: pascal

    procedure ConvertTime(Time: Integer; var h, m, s: Integer)

DeleteOnTerminate
~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function DeleteOnTerminate(const proc: string): Boolean

Disguise
~~~~~~~~

.. code-block:: pascal

    procedure Disguise(Caption: string)

DisplayDebugImgWindow
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure DisplayDebugImgWindow(w, h: Integer)

DrawBitmapDebugImg
~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure DrawBitmapDebugImg(Bitmap: Integer)

GetClipBoard
~~~~~~~~~~~~

.. code-block:: pascal

    function GetClipBoard: string

GetDebugBitmap
~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetDebugBitmap: Integer

GetEnvironmentVariable
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetEnvironmentVariable(const VarName: string): string

GetProcessID
~~~~~~~~~~~~

.. code-block:: pascal

    function GetProcessID: LongInt

GetScriptProp
~~~~~~~~~~~~~

.. code-block:: pascal

    function GetScriptProp(prop: TSP_Property; var Value: TVariantArray): Boolean

GetTClient
~~~~~~~~~~

.. code-block:: pascal

    function GetTClient: TClient

GetTimeRunning
~~~~~~~~~~~~~~

.. code-block:: pascal

    function GetTimeRunning: LongWord

HakunaMatata
~~~~~~~~~~~~

.. code-block:: pascal

    procedure HakunaMatata

InputQuery
~~~~~~~~~~

.. code-block:: pascal

    function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean

MessageBox
~~~~~~~~~~

.. code-block:: pascal

    function MessageBox(Text, Caption: string; Flags: LongInt): Integer

MessageDlg
~~~~~~~~~~

.. code-block:: pascal

    function MessageDlg(const Caption, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer

SaveScreenshot
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SaveScreenshot(FileName: string)

SetClipBoard
~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetClipBoard(const Data: string)

SetScriptProp
~~~~~~~~~~~~~

.. code-block:: pascal

    function SetScriptProp(prop: TSP_Property; Value: TVariantArray): Boolean

SetSupressExceptions
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetSupressExceptions(Supress: Boolean)

SetTarget
~~~~~~~~~

.. code-block:: pascal

    procedure SetTarget(Proc: TSysProc)

ShowBalloonHint
~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ShowBalloonHint(const Title, Hint: string; const Timeout: Integer; const Flag: TBalloonFlags)

ShowMessage
~~~~~~~~~~~

.. code-block:: pascal

    procedure ShowMessage(msg: string)

Simba
~~~~~

.. code-block:: pascal

    procedure Simba

Sleep
~~~~~

.. code-block:: pascal

    procedure Sleep(t: DWord); override

Status
~~~~~~

.. code-block:: pascal

    procedure Status(Status: string)

TerminateScript
~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure TerminateScript

Wait
~~~~

.. code-block:: pascal

    procedure Wait(t: DWord)
