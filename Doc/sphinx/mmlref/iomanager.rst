IOManager Class
===============

.. note::
    This page is still WIP.
    It only covers the function of the IOManager class vaguely. In reality, the
    IOManager unit contains quite some classes, each with a (slightly) different
    function. There are not yet documented. (Perhaps BenLand100 can do this?)

The IOManager class manages the core functionality for retreiving Window data,
such as the actual pixel data and the position and dimension of a window.

The main purpose is to form a cross platform class to retrieve window 
information, press and poll mouse buttons and to press and poll keyboard keys.

The IOManager is the only class that should use platform (or operating system)
specific calls; this is all abstracted by the IOManager class.

To achieve this, several abstract classes are defined by the IOManager class.
Every operating system (or window system) needs it's own implementation of the
``TWindow_Abstract`` class. We wrote one for both Linux and Windows. 

.. toctree::
    :maxdepth: 2

    os_linux.rst
    os_windows.rst

Offscreen image capturing
-------------------------

On Linux, offscreen image capturing is possible when compositing is turned on.
With compiz, this is turned on by default. Other windows managers like Metacity
and KWin can turn this on. When enabled, Simba can capture images from windows
that are below others. Minimized does not work. It is also possible to turn on
Compositing for specific X11 Windows with an api call, but this is currently not
implemented.

The status on Windows is unknown.

Silent Input
------------

So what is Silent Input?
We define Silent Input as methods to manipulate the user's mouse and keyboard,
without visually using them. So what does this mean?

This basically means that you will still be able to use your mouse while
the MML is performing mouse operations on your targetted window/client.

However, silent input is very hard to implement, and often hardly supported
by host operating systems. Often silent mouse or keyboard input is simply 
ignored. So in general it is advised to stick to non silent input.
