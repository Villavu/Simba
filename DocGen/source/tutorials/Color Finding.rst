#############
Color Finding
#############

Colorfinding with Simba 2.0

The colorfinding concept has been changed after Simba 1.4, this new system adds several new mechanisms to aid you when you automate tasks using Simba.

----

Colorspaces and distance metrics:
=================================

The following color spaces are supported in Simba:

RGB distance:
"""""""""""""

  .. math::
    :nowrap:

    \begin{flalign}
    & d=\sqrt{\Delta R^2 + \Delta G^2 + \Delta B^2} &
    \end{flalign}

HSL distance:
"""""""""""""

  .. math::
    :nowrap:

    \begin{flalign}
    &h =
      \begin{cases}
        360-\Delta H  & \quad \text{if } \Delta H \geq 180\\\
        \Delta H  & \quad \text{ otherwise}
      \end{cases} &\\\\
    &d =\sqrt{(h * max(S_{1}, S_{2}) / 100)^2 + \Delta S^2 + \Delta L^2}&
    \end{flalign}

HSV distance:
"""""""""""""

  .. math::
    :nowrap:

    \begin{flalign}
    & h =
      \begin{cases}
        360-\Delta H  & \quad \text{if } \Delta H \geq 180\\\
        \Delta H  & \quad \text{ otherwise}
      \end{cases} & \\\\
    & d =\sqrt{(h * max(S_{1}, S_{2}) / 100)^2 + \Delta S^2 + \Delta V^2} &
    \end{flalign}

XYZ distance:
"""""""""""""

  .. math::
    :nowrap:

    \begin{flalign}
    & d = \sqrt{\Delta X^2 + \Delta Y^2 + \Delta Z^2} &
    \end{flalign}

LAB distance:
"""""""""""""

  .. math::
    :nowrap:

    \begin{flalign}
    & d=\sqrt{\Delta L^2 + \Delta A^2 + \Delta B^2} &
    \end{flalign}

LCH distance:
"""""""""""""

  .. math::
    :nowrap:

    \begin{flalign}
    & h =
      \begin{cases}
        360-\Delta H  & \quad \text{if } \Delta H \geq 180\\\
        \Delta H  & \quad \text{ otherwise}
      \end{cases} & \\\\
    & d =\sqrt{\Delta L^2 + \Delta C^2 + (h * max(C_{1}, C_{2}) / 100)^2} &
    \end{flalign}

DeltaE distance:
""""""""""""""""

See: https://en.wikipedia.org/wiki/Color_difference#CIE94

.. note::

  :math:`\Delta H` is set to zero if one of the two colors has no chromaticity or saturation. (Only applies to color spaces with hue)

----

However this alone does not describe how the distance is presented to the scripter in any Finder-method. The distance is converted to a percentage in methods related to finding a set of colors on the screen. So you will work with a range that goes from `0` to `100`. Where `100` would be a completely different color than what you want.
This is done to make it simpler to get going with Simba. So no matter colorspace, or distance measurement your tolerance-range is `0..100`.


Multipliers
===========

Multipliers are added to allow you to add weights to some channels. Say you are looking for colors that are mostly green, using RGB colorspace, but you want to allow very dark greens, as well as bright greens, you just dont want red and blue. Such a multiplier could look something like this:

.. code-block::

  MyRGBMultiplier := [2, 0.1, 2];

With this we have said that we want to add more weight to R and B channel. So those would make up for most the difference.

.. math::

  \sqrt{(\Delta R*R_{multiplier})^2 + (\Delta G*G_{multiplier})^2 + (\Delta B*B_{multiplier})^2}

Manually working out the tolerance range where you find the green you want is tricky, so we have added tools for this, like ACA that estimates multipliers automatically, given a set of input colors.

In short, a larger channel multiplier means that channel will make up for a larger portion of the resulting difference. The way they are implemented would allow you to define a multiplier like this:

.. code-block::

  MyRGBMultiplier := [60, 10, 30];

Now you can think of them as percentages:

The value of red is 60% - This means the final difference value is 60% based of the red distance \
The value of green is 10% - This means 10% of the result difference is based on green \
The value of blue is 30% - This means 30% of the result difference is based on blue

Basically implying that if green is quite different, we don't really care. However if the red channel is quite different, we really care, and we don't want that.

.. note ::

  Multipliers do not affect tolerance-range, it's in finder methods still a percentage, `0..100`.

----

Methods
=======

.. code-block::

  finder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;

Takes all the input as individual parameters:
- `Color`, the color to search for as an Integer
- `Tolerance` the tolerance needed to find the color-shades you want
- `EColorSpace` defines the colorspace's distance metric used in the search (RGB, HSL, HSV, XYZ, LAB, LCH, DeltaE)
- `Multipliers` defines the channel multipliers, if each value is equal then they have no effect.
- `Bounds`, the search area on the target `left, top, right, bottom`

----

.. code-block::

  finder.FindColor(Color: TColor; Tolerance: Single: Bounds: TBox = [-1,-1,-1,-1]): TPointArray;

This method takes no adjusters for colorspace and multipliers, and uses whatever is default:
- `Color`, the color to search for as an Integer
- `Tolerance` the tolerance needed to find the color-shades you want
- `Bounds`, the search area on the target `left, top, right, bottom`

----

.. code-block::

  finder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;

This method takes all the color related search data as a single parameter, in a format that can be stored as a variable. This way you can more easily set up several different objects to search for depending on some input.

:code:`TColorTolerance` is defined as:

.. code-block::

  TColorTolerance = record
    Color: TColor;
    Tolerance: Single;
    Colorspace: EColorSpace;
    Multipliers: array [0..2] of Single;
  end;

Example:

.. code-block::

  var
    SimbaIcon: TColorTolerance;
    TPA: TPointArray;
  begin
    SimbaIcon := ColorTolerance($1044BE, 10.592, EColorSpace.HSL, [1.073, 1.138, 0.791]);

    TPA := finder.FindColor(SimbaIcon, [0,0,400,400]);
    if Length(TPA) > 0 then
    begin
      WriteLn('Found: ', Length(TPA), ' points');
    end;
  end.
