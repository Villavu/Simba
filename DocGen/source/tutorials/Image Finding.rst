#############
Image Finding
#############

Transparent pixels
==================

With :code:`FindImage` and :code:`FindTemplate` pixels that are fully transparent (being the pixels alpha value is 0) will be "skipped" when searched for.

For example with this image:

.. image:: ../../images/shape.png

With this as the target to search in:

.. image:: ../../images/shape_background.png

Would result with the image being found as transparent pixels would be ignored when searching.