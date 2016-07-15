.. _scriptref-socket:

Sockets
=======

With the socket functions you can implement virtually any network procotol:
there have been several IRC bots.

Socket Functions
----------------

Simba's Socket Functions. Examples required; if you have one, please let u know.

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
data from another socket. Time is in *milliseconds*.

SocketInfo
~~~~~~~~~~

.. code-block:: pascal

    procedure SocketInfo(Client: integer; out IP, Port: string);

SocketInfo sets where a bound socket will be sending data to (out IP, out Port).

