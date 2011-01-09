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

Opens file for rewriting. (File is cleared on open)
Opens shared if *Shared* is true.
Returns -1 on failure, otherwise returns the handle to the file.

AppendFile
----------

.. code-block:: pascal

    function AppendFile(const Path: string): Integer;

Opens file for writing (appending).
Returns -1 on failure, otherwise returns the handle to the file.

CloseFile
---------

.. code-block:: pascal

    procedure CloseFile(FileNum: Integer);

Close the file defined by *FileNum*. Never forget to close your files!


EndOfFile
---------

.. code-block:: pascal

    function EndOfFile(FileNum: Integer): Boolean;

Returns true if the end of the file has been reached.

FileSize
--------

.. code-block:: pascal

    function FileSize(FileNum: Integer): LongInt;

Returns the file size in characters.


ReadFileString
--------------

.. code-block:: pascal

    function ReadFileString(FileNum: Integer; var s: string; x: Integer):
    Boolean;

Read *x* characters into string *s* from file *FileNum*.
Returns true if the number of characters read equals *x*.

WriteFileString
---------------

.. code-block:: pascal

    function WriteFileString(FileNum: Integer; s: string): Boolean;

Writes *s* to file *FileNum*. Returns false on failure.


SetFileCharPointer
------------------

.. code-block:: pascal

    function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;

*Seek* through the file. Set the cursor to *cChars* from *Origin*.

Origin can be any of these:

.. code-block:: pascal

    { File seek origins }
    FsFromBeginning = 0;
    FsFromCurrent   = 1;
    FsFromEnd       = 2;

FilePointerPos
--------------

.. code-block:: pascal

    function FilePointerPos(FileNum: Integer): Integer;

Returns the position of the *cursur* in the file.
(What character # you are at)

DirectoryExists
---------------

.. code-block:: pascal

    function DirectoryExists(const DirectoryName : string ) : Boolean;

Returns true if the directory exists.

CreateDirectory
---------------

.. code-block:: pascal

    function CreateDirectory(const DirectoryName : string) : boolean;

Creates a directory. Returns true on success.

FileExists 
-----------

.. code-block:: pascal

    function FileExists (const FileName : string ) : Boolean;

Returns true if the file exists.


ForceDirectories
----------------

.. code-block:: pascal

    function ForceDirectories(const dir : string) : boolean;

Creates multiple *nested* directories. Returns true on success.

GetFiles
--------

.. code-block:: pascal

    function GetFiles(const Path, Ext : string) : TStringArray;

Returns the files in the directory defined by *Path* with extension *Ext*.

GetDirectories
--------------

.. code-block:: pascal

    function GetDirectories(const path : string) : TStringArray;

Returns the directories in *path*.

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

Returns the file extension from file *Filename*.
