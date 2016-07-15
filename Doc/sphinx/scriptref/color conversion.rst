.. _scriptref-colorconv:

Color Conversions
=================

..
    TODO:

Color spaces
------------

Explain some color spaces here.


Color Conversion Methods
------------------------

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

CIELabToColor
~~~~~~~~~~~~~

.. code-block:: pascal

    function CIELabToColor(L, a, b: Extended): TColor

CIELabToHSL
~~~~~~~~~~~

.. code-block:: pascal

    procedure CIELabToHSL(L, a, b: Extended; out HH, SS, LL: Extended)

CIELabToRGB
~~~~~~~~~~~

.. code-block:: pascal

    procedure CIELabToRGB(L, a, b: Extended; out rr, gg, bb: Integer)

CIELabtoXYZ
~~~~~~~~~~~

.. code-block:: pascal

    procedure CIELabtoXYZ(L, a, b: Extended; out X, Y, Z: Extended)

ColorToCIELab
~~~~~~~~~~~~~

.. code-block:: pascal

    procedure ColorToCIELab(Color: Integer; out L, a, b: Extended)

ColorToGray
~~~~~~~~~~~

.. code-block:: pascal

    function ColorToGray(const Color: Integer): TColor

HSLToCIELab
~~~~~~~~~~~

.. code-block:: pascal

    procedure HSLToCIELab(HH, SS, LL: Extended; out L, a, b: Extended)

HSLToXYZ
~~~~~~~~

.. code-block:: pascal

    procedure HSLToXYZ(H, S, L: Extended; out X, Y, Z: Extended)

RGBToCIELab
~~~~~~~~~~~

.. code-block:: pascal

    procedure RGBToCIELab(rr, gg, bb: Integer; out L, a, b: Extended)

XYZtoCIELab
~~~~~~~~~~~

.. code-block:: pascal

    procedure XYZtoCIELab(X, Y, Z: Extended; out L, a, b: Extended)

XYZToHSL
~~~~~~~~

.. code-block:: pascal

    procedure XYZToHSL(X, Y, Z: Extended; out H, S, L: Extended)
