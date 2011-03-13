
.. _scriptref_web:

Internet Functions
==================

The internet functions in Simba range from HTTP functions and low-level
socket functions. With the HTTP functions you can scrape the web, send form data
to web pages.

With the socket functions you can implement virtually any network procotol:
there have been several IRC bots.

HTTP Functions
--------------

OpenWebPage
~~~~~~~~~~~

.. code-block:: pascal

    procedure OpenWebPage(const url : string);

OpenWebPage opens the given web page (url) with your default browser.

Example:

.. code-block:: pascal

    OpenWebPage('http://villavu.com');

GetPage
~~~~~~~

.. code-block:: pascal

    function GetPage(const url : string): string;

GetPage returns a string of HTML from the given web page.


InitializeHTTPClient
~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function InitializeHTTPClient(HandleCookies: Boolean): Integer;

InitializeHTTPClient creates a new client and assigns it an ID. You use this for
all the other web functions that require a client.

InitializeHTTPClientWrap
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: pascal

    function InitializeHTTPClientWrap(HandleCookies: Boolean): Integer;

This should probably not be documented.


FreeHTTPClient
~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure FreeHTTPClient(Client: Integer);

Free the HTTP client returned by *InitializeHTTPClient*.

GetHTTPPage
~~~~~~~~~~~

.. code-block:: pascal

    function GetHTTPPage(Client: Integer;const URL: string): string;

GetHTTPPage is just like GetPage, except you can choose which client to get the
HTTP code from.


SetHTTPUserAgent
~~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure SetHTTPUserAgent(Client: Integer;const Agent: string);

SetHTTPUserAgent allows you to change the agent string of a client.

PostHTTPPage
~~~~~~~~~~~~

.. code-block:: pascal

    function PostHTTPPage(Client: Integer;const Url,PostData: string): string;

PostHTTPPage requests to post data (PostData) on the web page (Url) of the
client (Client).

PostHTTPPageEx
~~~~~~~~~~~~~~

.. code-block:: pascal

    function PostHTTPPageEx(Client: Integer;const Url: string): string;

PostHTTPPageEx is just like PostHTTPPage but uses predefined post data added by
ddPostVariable and cleared by ClearPostData.

ClearPostData
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ClearPostData(Client: Integer);

ClearPostData clears the post data added to the web page (Client). Used with
PostHTTPPageEx.


AddPostVariable
~~~~~~~~~~~~~~~

.. code-block:: pascal

    procedure AddPostVariable(Client: Integer;const VarName, VarValue: string);

AddPostVariable adds a post variable to the web page (Client). Used with
PostHTTPPageEx.

GetRawHeaders
~~~~~~~~~~~~~

.. code-block:: pascal

    function GetRawHeaders(Client: Integer): string;

GetRawHeaders returns a string of headers from the specified client.


Socket Functions
----------------

CreateSocket
~~~~~~~~~~~~

.. code-block:: pascal

    function CreateSocket: integer;

CreateSocket creates a new socket and assigns it an ID.

FreeSocket
~~~~~~~~~~

.. code-block:: pascal

    procedure FreeSocket(Index: integer);

FreeSocket frees the socket with the ID (Index) assigned to it upon creation.

ConnectSocket
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ConnectSocket(Client: integer; IP, Port: string);

ConnectSocket connects the socket to an IP and port on the specified client
(Client).


BindSocket
~~~~~~~~~~

.. code-block:: pascal

    procedure BindSocket(Client: integer; IP, Port: string);

BindSocket binds a connected socket to an IP and port on the specified client
(Client).

ListenSocket
~~~~~~~~~~~~

.. code-block:: pascal

    procedure ListenSocket(Client: integer);

ListenSocket allows for a client socket to accept connections.


AcceptSocket
~~~~~~~~~~~~

.. code-block:: pascal

    function AcceptSocket(Client: integer): integer;

AcceptSocket accepts pending connection requests to a client socket.


CloseSocket
~~~~~~~~~~~

.. code-block:: pascal

    procedure CloseSocket(Client: integer);

CloseSocket closes connections to a client socket.

RecvSocket
~~~~~~~~~~

.. code-block:: pascal

    function RecvSocket(Client: integer): string;

RecvSocket method reads all data waiting for read.

RecvSocketStr
~~~~~~~~~~~~~

.. code-block:: pascal

    function RecvSocketStr(Client: integer): string;

Method waits until data string is received. This string is terminated by CR-LF
characters. The resulting string is returned without this termination (CR-LF)

RecvSocketEx
~~~~~~~~~~~~

.. code-block:: pascal

    function RecvSocketEx(Client, Length: integer): string;

RecvSocketEx returns received data of a specified length from a bound socket as
a string.


SendSocket
~~~~~~~~~~

.. code-block:: pascal

    procedure SendSocket(Client: integer; Data: string);

SendSocket sends a string of data to a bound client socket.

SetTimeout
~~~~~~~~~~

.. code-block:: pascal

    procedure SetTimeout(Client, Time: integer);

SetTimeout sets a maximum amount of time for a bound client socket to wait for
data from another socket.

SocketInfo
~~~~~~~~~~

.. code-block:: pascal

    procedure SocketInfo(Client: integer; out IP, Port: string);

SocketInfo sets where a bound socket will be sending data to (out IP, out Port).

Generic functions
-----------------

SetProxy
~~~~~~~~

.. code-block:: pascal

    procedure SetProxy(Client : Integer; pHost, pPort : String);');

SetProxy configures a proxy with the given client (Client) proxy host (pHost)
and port (pPort).
