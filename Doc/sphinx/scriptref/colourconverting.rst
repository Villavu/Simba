.. _scriptref_colourconv:

Colour Conversions
==================

Colour spaces
-------------

Explain some colour spaces here.


Colour Conversion Methods
-------------------------

.. code-block:: pascal

    procedure ColorToRGB(Color: integer; var r, g, b: Integer);

RGBtoColor
~~~~~~~~~~

.. code-block:: pascal

    function RGBtoColor(r, g, b: Integer): TColor;

ColorToHSL
~~~~~~~~~~

.. code-block:: pascal

    procedure ColorToHSL(Color: Integer; var h, s, l: Extended);

HSLToColor
~~~~~~~~~~

.. code-block:: pascal

    function HSLToColor(H, S, L: Extended): TColor;

ColorToXYZ
~~~~~~~~~~

.. code-block:: pascal

    procedure ColorToXYZ(Color: Integer; var x, y, z: Extended);

XYZToColor
~~~~~~~~~~

.. code-block:: pascal

    function XYZToColor(X, Y, Z: Extended): TColor;

RGBToHSL
~~~~~~~~

.. code-block:: pascal

    procedure RGBToHSL(R, G, B: Integer; var h, s, l: Extended);

HSLtoRGB
~~~~~~~~

.. code-block:: pascal

    procedure HSLtoRGB(H, S, L: extended; var R, G ,B: Integer);

RGBToXYZ
~~~~~~~~

.. code-block:: pascal

    procedure RGBToXYZ(R, G, B: Integer;var x, y ,z: Extended);

XYZToRGB
~~~~~~~~

.. code-block:: pascal

    procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Integer);


