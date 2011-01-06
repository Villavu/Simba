Working with Files
==================

Files in Simba are all *integers*, internal handles for Simba which on their
turn point to operating system files. Functions like CreateFile and OpenFile
return a file handle. You should not forget to close these when you no longer
need them.

CreateFile
----------

.. code-block:: pascal

    function CreateFile(const Path: string): Integer;

Create a file with *Path*. Raturns -1 on failure, otherwise returns the handle
to the file.

OpenFile
--------

.. code-block:: pascal

    function OpenFile(const Path: string; Shared: Boolean): Integer;

Opens file for reading. Opens shared if *Shared* is true.
Returns -1 on failure, otherwise returns the handle to the file.


RewriteFile
-----------

.. code-block:: pascal

    function RewriteFile(const Path: string; Shared: Boolean): Integer;


AppendFile
----------

.. code-block:: pascal

    function AppendFile(const Path: string): Integer;


CloseFile
---------

.. code-block:: pascal

    procedure CloseFile(FileNum: Integer);


EndOfFile
---------

.. code-block:: pascal

    function EndOfFile(FileNum: Integer): Boolean;


FileSize
--------

.. code-block:: pascal

    function FileSize(FileNum: Integer): LongInt;


ReadFileString
--------------

.. code-block:: pascal

    function ReadFileString(FileNum: Integer; var s: string; x: Integer):
    Boolean;


WriteFileString
---------------

.. code-block:: pascal

    function WriteFileString(FileNum: Integer; s: string): Boolean;


SetFileCharPointer
------------------

.. code-block:: pascal

    function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;


FilePointerPos
--------------

.. code-block:: pascal

    function FilePointerPos(FileNum: Integer): Integer;


DirectoryExists
---------------

.. code-block:: pascal

    function DirectoryExists(const DirectoryName : string ) : Boolean;


CreateDirectory
---------------

.. code-block:: pascal

    function CreateDirectory(const DirectoryName : string) : boolean;


FileExists 
-----------

.. code-block:: pascal

    function FileExists (const FileName : string ) : Boolean;


ForceDirectories
----------------

.. code-block:: pascal

    function ForceDirectories(const dir : string) : boolean;


GetFiles
--------

.. code-block:: pascal

    function GetFiles(const Path, Ext : string) : TStringArray;


GetDirectories
--------------

.. code-block:: pascal

    function GetDirectories(const path : string) : TStringArray;


WriteINI
--------

.. code-block:: pascal

    procedure WriteINI(const Section, KeyName, NewString, FileName: string);


ReadINI
-------

.. code-block:: pascal

    function ReadINI(const Section, KeyName, FileName: string): string;


DeleteINI
---------

.. code-block:: pascal

    procedure DeleteINI(const Section, KeyName, FileName: string);


ExtractFileExt
--------------

.. code-block:: pascal

    function ExtractFileExt(const FileName: string): string;');   


