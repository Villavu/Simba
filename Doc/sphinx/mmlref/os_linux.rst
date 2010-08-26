Linux Specific Implementation Details
=====================================

The Linux specific implementation of the IOManager is written around the Linux
X11 Windowing System. The X11 windowing system can run simultaneously to other
X11 Window Systems, and each is defined by a ``Display``.

A ``Window`` on a ``Display`` is represented as an unsigned integer.

To retreive data that is on a window, we use the XGetImage function. This
function returns an XImage which contains the raw data. Simba then returns the
pointer to this data. The image has to be freed afterwards; and this is done in
the ``FreeReturnData`` procedure. Alternatively we could copy the image data to
another place and then free the XImage, but this would result in additional
overhead.

Most mouse related functionality is implemented with XQueryPointer, XWarpPointer
and XTestFakeButtonEvent. (Requires XTest Extension)

All the keyboard functions use XTest as well. It currently lacks the
functionality to test if a key on the keyboard is down.
