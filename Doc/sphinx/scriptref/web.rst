
.. _scriptref_web:

Internet Functions
==================

OpenWebPage
-----------

.. code-block:: pascal

    procedure OpenWebPage(const url : string);


GetPage
-------

.. code-block:: pascal

    function GetPage(const url : string): string;


InitializeHTTPClient
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function InitializeHTTPClient(HandleCookies: Boolean): Integer;


InitializeHTTPClientWrap
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function InitializeHTTPClientWrap(HandleCookies: Boolean): Integer;


FreeHTTPClient
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure FreeHTTPClient(Client: Integer);


GetHTTPPage
~~~~~~~~~~-

.. code-block:: pascal

    function GetHTTPPage(Client: Integer;const URL: string): string;


SetHTTPUserAgent
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetHTTPUserAgent(Client: Integer;const Agent: string);


PostHTTPPage
~~~~~~~~~~~~

.. code-block:: pascal

    function PostHTTPPage(Client: Integer;const Url,PostData: string): string;


PostHTTPPageEx
~~~~~~~~~~~~~~

.. code-block:: pascal

    function PostHTTPPageEx(Client: Integer;const Url: string): string;


ClearPostData
~~~~~~~~~~~~-

.. code-block:: pascal

    procedure ClearPostData(Client: Integer);


AddPostVariable
~~~~~~~~~~~~~~-

.. code-block:: pascal

    procedure AddPostVariable(Client: Integer;const VarName, VarValue: string);


GetRawHeaders
~~~~~~~~~~~~-

.. code-block:: pascal

    function GetRawHeaders(Client: Integer): string;


SetProxy
~~~~~~~~

.. code-block:: pascal

    procedure SetProxy(Client : Integer; pHost, pPort : String);');


CreateSocket
~~~~~~~~~~~~

.. code-block:: pascal

    function CreateSocket: integer;


FreeSocket
~~~~~~~~~~

.. code-block:: pascal

    procedure FreeSocket(Index: integer);


ConnectSocket
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ConnectSocket(Client: integer; IP, Port: string);


BindSocket
~~~~~~~~~~

.. code-block:: pascal

    procedure BindSocket(Client: integer; IP, Port: string);


ListenSocket
~~~~~~~~~~~~

.. code-block:: pascal

    procedure ListenSocket(Client: integer);


AcceptSocket
~~~~~~~~~~~~

.. code-block:: pascal

    function AcceptSocket(Client: integer): integer;


CloseSocket
~~~~~~~~~~~

.. code-block:: pascal

    procedure CloseSocket(Client: integer);

RecvSocket
~~~~~~~~~~

.. code-block:: pascal

    function RecvSocket(Client: integer): string;


RecvSocketStr
~~~~~~~~~~~~~

.. code-block:: pascal

    function RecvSocketStr(Client: integer): string;


RecvSocketEx
~~~~~~~~~~~~

.. code-block:: pascal

    unction RecvSocketEx(Client, Length: integer): string;


SendSocket
~~~~~~~~~~

.. code-block:: pascal

    procedure SendSocket(Client: integer; Data: string);


SetTimeout
~~~~~~~~~~

.. code-block:: pascal

    procedure SetTimeout(Client, Time: integer);


SocketInfo
~~~~~~~~~~

.. code-block:: pascal

    procedure SocketInfo(Client: integer; out IP, Port: string);


