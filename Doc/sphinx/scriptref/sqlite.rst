.. _scriptref-sqlite:

SQLite Functions
==================

Simba has support for reading and manipulating SQLite databases.
Open connections are all represented by integers in scripts. The integers point to an index in an internal array of pointers which is managed by Simba.
sqlite_open and function sqlite_open_v2 return an integer that you use for most other functions. This page documents only the functions, and not `SQLite <http://www.sqlite.org>`_ or the `SQL <http://en.wikipedia.org/wiki/SQL>`_ language.
After opening a connection, you should use sqlite_close on it when you are no longer using it. If, however, for some reason you forget, Simba will free all unfreed connections automatically.

sqlite_open
-----------

.. code-block:: pascal

    function sqlite_open(filename : string) : integer;

Will try to open a connection to the database file specified and returns an index to the internal array.
If the database file does not exist, it will attempt to create it.
This will return -1 if no connection could be established.

*Example:*

.. code-block:: pascal

    DB := sqlite_open('test.db'); // DB would be an integer that you pass as the index parameter to the other methods.

sqlite_open_v2
--------------

.. code-block:: pascal

    function sqlite_open_v2(filename : string; flags : integer) : integer;

Does the same as sqlite_open however you can provide flags to use when opening.

*Open flags:*

.. code-block:: pascal

	const
	  SQLITE_OPEN_READONLY = 1;
	  SQLITE_OPEN_READWRITE = 2;
	  SQLITE_OPEN_CREATE = 4;
	  SQLITE_OPEN_URI = 40;
	  SQLITE_OPEN_NOMUTEX = 8000;
	  SQLITE_OPEN_FULLMUTEX = 10000;
	  SQLITE_OPEN_SHAREDCACHE = 20000;
	  SQLITE_OPEN_PRIVATECACHE = 40000;

*Example:*

.. code-block:: pascal

	DB := sqlite_open_v2('test.db', SQLITE_OPEN_READONLY); // Open as read only, will not create file.
	DB := sqlite_open_v2('test.db', SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE); // Open as read write, will create file.

sqlite_version
--------------

.. code-block:: pascal

    function sqlite_version() : string;

Returns the version of the loaded SQLite library expressed as a string (x.y.z).

.. code-block:: pascal

	Writeln(sqlite_version()); // Outputs 3.7.10 for me

sqlite_version_num
------------------

.. code-block:: pascal

    function sqlite_version_num() : integer;

Returns the version of the loaded SQLite library expressed as an integer (x * 1000000 + y * 1000 + x).

*Example:*

.. code-block:: pascal

    Writeln(sqlite_version_num()); // Outputs 3007010 for me

sqlite_query
------------

.. code-block:: pascal

    function sqlite_query(index : integer; sql : string) : boolean;

Attempts to execute a query on the database handle specified by index. Returns true if SQLITE_OK is returned by SQLite.
If it returns false, it is useful to see what sqlite_errMsg outputs.

*Example:*

.. code-block:: pascal

	sqlite_query(DB, 'CREATE TABLE test (id INTEGER PRIMARY KEY AUTOINCREMENT, name VARCHAR(50) UNIQUE NOT NULL);');
	sqlite_query(DB, 'INSERT INTO test (name) VALUES (''Sex'');');

sqlite_queryValue
-----------------

.. code-block:: pascal

    function sqlite_queryValue(index : integer; sql : string; out results : T2DStringArray) : boolean;

Attempts to execute a query on the database handle specified by index. Return true if SQLITE_OK is returned by SQLite.
This will also save the resulting rows in the Results variable provided. The first array will be an array containing column names.
If it returns false, it is useful to see what sqlite_errMsg outputs.

*Example:*

.. code-block:: pascal

	sqlite_queryValue(DB, 'SELECT * FROM test;', Results);
	Writeln(Results); // Should output [['id', 'name'], ['1', 'Sex']]

sqlite_queryResult
------------------

.. code-block:: pascal

    function sqlite_queryResult(index : integer; sql : string; var error : boolean) : T2DStringArray;

Attempts to execute a query on the database handle specified by index. The resulting rows are returned.
If an error occurred during the query, the error boolean will be set to true. Otherwise, it will be false.

*Example:*

.. code-block:: pascal

	Results := sqlite_queryResult(DB, 'SELECT * FROM test;', error);
	if error then
	  [...] // do your error handling here...
	Writeln(Results); // Should output [['id', 'name'], ['1', 'Sex']]

sqlite_escape
-------------

.. code-block:: pascal

    function sqlite_escape(s : string): string;

Sanitizes a string for input into the database by replacing apostrophes with anothe apostrophe. It will return the escaped string.

*Example:*

.. code-block:: pascal

	Writeln(sqlite_escape('foo '' or 1=1')); // Outputs foo '' or 1=1. Note that it looks as I inputted it as SQLite uses the same escaping conventions for apostrophes as Pascal.

sqlite_close
------------

.. code-block:: pascal

    procedure sqlite_close(index : integer);

Closes the database handle specified by index (removing file locks, etc.). Don't forget to use this when you're done working on a database!

*Example:*

.. code-block:: pascal

	DB := sqlite_open('test.db');
	// [...]
	sqlite_close(DB);

sqlite_errMsg
-------------

.. code-block:: pascal

    function sqlite_errMsg(index : integer) : string;

Returns the error message returned by the last SQLite library call. You must provide an index to a database handle.
If no error has occurred, this will return 'not an error'.

*Example:*

.. code-block:: pascal

	sqlite_query(DB, 'asdfghjkl');
	Writeln(sqlite_errmsg(DB)); // near "asdfghjkl": syntax error

sqlite_errCode
--------------

.. code-block:: pascal

    function sqlite_errCode(index : integer) : integer;

Returns the result code returned by the last SQLite library call. You must provide an index to a database handle.
If no error has occurred, this will return SQLITE_OK.

*Result codes:*

.. code-block:: pascal

	const
	  SQLITE_OK = 0; // Successful result
	  SQLITE_ERROR = 1; // SQL error or missing database
	  SQLITE_INTERNAL = 2; // Internal logic error in SQLite
	  SQLITE_PERM = 3; // Access permission denied
	  SQLITE_ABORT = 4; // Callback routine requested an abort
	  SQLITE_BUSY = 5; // The database file is locked
	  SQLITE_LOCKED = 6; // A table in the database is locked
	  SQLITE_NOMEM = 7; // A malloc() failed
	  SQLITE_READONLY = 8; // Attempt to write a readonly database
	  SQLITE_INTERRUPT = 9; // Operation terminated by sqlite3_interrupt()
	  SQLITE_IOERR = 10; // Some kind of disk I/O error occurred
	  SQLITE_CORRUPT = 11; // The database disk image is malformed
	  SQLITE_NOTFOUND := 12; // Unknown opcode in sqlite3_file_control()
	  SQLITE_FULL := 13; // Insertion failed because database is full
	  SQLITE_CANTOPEN := 14; // Unable to open the database file
	  SQLITE_PROTOCOL = 15; // Database lock protocol error
	  SQLITE_EMPTY = 16; // Database is empty
	  SQLITE_SCHEMA = 17; // The database schema changed
	  SQLITE_TOOBIG = 18; // String or BLOB exceeds size limit
	  SQLITE_CONSTRAINT = 19; // Abort due to constraint violation
	  SQLITE_MISMATCH = 20; // Data type mismatch
	  SQLITE_MISUSE = 21; // Library used incorrectly
	  SQLITE_NOLFS = 22; // Uses OS features not supported on host
	  SQLITE_AUTH = 23; // Authorization denied
	  SQLITE_FORMAT = 24; // Auxiliary database format error
	  SQLITE_RANGE = 25; // 2nd parameter to sqlite3_bind out of range
	  SQLITE_NOTADB = 26; // File opened that is not a database file
	  SQLITE_ROW = 27; // sqlite3_step() has another row ready
	  SQLITE_DONE = 28; // sqlite3_step() has finished executing

*Example:*

.. code-block:: pascal

	sqlite_query(DB, 'asdfghjkl');
	Writeln(sqlite_errmsg(DB)); // 1 (SQLITE_ERROR)
