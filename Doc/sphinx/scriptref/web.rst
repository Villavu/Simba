.. _scriptref-web:

Internet
========

With HTTP functions you can scrape the web, and send form data to web pages.

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

SetProxy
~~~~~~~~

.. code-block:: pascal

    procedure SetProxy(Client : Integer; pHost, pPort : String);');

SetProxy configures a proxy with the given client (Client) proxy host (pHost)
and port (pPort).
