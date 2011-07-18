.. _scriptref-colourconv:

Colour Conversions
==================

..
    TODO:

Colour spaces
-------------

Explain some colour spaces here.


Colour Conversion Methods
-------------------------

.. _scriptref-colortorgb:

ColorToRGB
~~~~~~~~~~

.. code-block:: pascal

    procedure ColorToRGB(Color: integer; var r, g, b: Integer);

.. _scriptref-rgbtocolor:

RGBtoColor
~~~~~~~~~~

.. code-block:: pascal

    function RGBtoColor(r, g, b: Integer): TColor;

.. _scriptref-colortohsl:

ColorToHSL
~~~~~~~~~~

.. code-block:: pascal

    procedure ColorToHSL(Color: Integer; var h, s, l: Extended);

.. _scriptref-hsltocolor:

HSLToColor
~~~~~~~~~~

.. code-block:: pascal

    function HSLToColor(H, S, L: Extended): TColor;

.. _scriptref-colortoxyz:

ColorToXYZ
~~~~~~~~~~

.. code-block:: pascal

    procedure ColorToXYZ(Color: Integer; var x, y, z: Extended);

.. _scriptref-xyztocolor:

XYZToColor
~~~~~~~~~~

.. code-block:: pascal

    function XYZToColor(X, Y, Z: Extended): TColor;

.. _scriptref-rgbtohsl:

RGBToHSL
~~~~~~~~

.. code-block:: pascal

    procedure RGBToHSL(R, G, B: Integer; var h, s, l: Extended);

.. _scriptref-hsltorgb:

HSLtoRGB
~~~~~~~~

.. code-block:: pascal

    procedure HSLtoRGB(H, S, L: extended; var R, G ,B: Integer);

.. _scriptref-rgbtoxyz:

RGBToXYZ
~~~~~~~~

.. code-block:: pascal

    procedure RGBToXYZ(R, G, B: Integer;var x, y ,z: Extended);

.. _scriptref-xyztorgb:

XYZToRGB
~~~~~~~~

.. code-block:: pascal

    procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Integer);

..
    TODO: Add CIE L*a*b.
