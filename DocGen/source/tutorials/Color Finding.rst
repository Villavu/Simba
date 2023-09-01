Color Finding
==============

Colorfinding with Simba 1.5

The colorfinding concept has been changed after Simba 1.4, this new system adds several new mechanisms to aid you when you automate tasks using Simba.

Colorspaces and distance metrics

**RGB distance**:

.. math::
  :nowrap:

  \begin{flalign}
  & d=\sqrt{\Delta R^2 + \Delta G^2 + \Delta B^2} &
  \end{flalign}

**HSL distance**: 

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

**HSV distance**:

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

**XYZ distance**:

.. math::
  :nowrap:

  \begin{flalign}
  & d = \sqrt{\Delta X^2 + \Delta Y^2 + \Delta Z^2} &
  \end{flalign}

**LAB distance**:

.. math::
  :nowrap:

  \begin{flalign}
  & d=\sqrt{\Delta L^2 + \Delta A^2 + \Delta B^2} &
  \end{flalign}

**LCH distance**:

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

**DeltaE distance**:

See: https://en.wikipedia.org/wiki/Color_difference#CIE94   