libMML
======

libMML is short for the library to the Mufasa Macro Library.


Why libMML?
-----------

One of the first questions that rise are probably *why* libMML and *what* is
libMML exactly?

libMML is a loadable C-like library that provides most of the MML functionality.
For the ones that do not know, MML is the core component for Simba that does all
the *computational* and *algorithmic* work. See :ref:`mml-ref` for more
information.

To summarize, the MML covers:

    -   Targetting specific windows and getting the window information such as
        their bitmaps and dimensions.
    -   Controlling the mouse and keyboard.
    -   Finding colours, bitmaps and dtms.
    -   Text recognition (OCR)
    -   Accessing files and sockets in an unified manner

Hopefully the *what* part of the question has mainly been covered by now. If
you're literate in computer science the *why* question has also been answered as
soon as it was mentioned that is was a loadable library - which is also implied
by its name. Exporting the MML into a loadable library allows virtually any
program to load it and just use all the MML functionality.

Design (issues)
---------------

libMML itself should not be too complex. It should simply translate the OOP MML
to a non-OOP C-type library and convert datatypes when required (see below as to
why). libMML is basically just a codebase that calls MML functions and passes
the result along in a slightly different format. In simple cases such as
MoveMouse the integers are simply passed; since there's do not differ, but in
the case of arrays of any type we have to copy the arrays to a C format - at
least until MML internally will no longer use Free Pascal (managed) arrays.

As previously mentioned, libMML is a *C*-type library; this is mentioned
explicitly because MML is written in Free Pascal (Object Pascal) which has quite
a few different datatypes. Strings are typically not compatible, and arrays are
managed in Pascal whereas they are not in C which makes it hard to just *pass*
the array along. One of the problems we have to cope with when writing libMML is
converting datatypes to C-compatible datatypes. C-compatible datatypes are
supported by most programming languages and thus the best way to go when making
a universal MML library.

libMML use cases
----------------

Theoretically libMML can be loaded by any programming language; but typically
each programming languages has it's own kind of programming practices and thus
write - again - their own wrapper around libMML. This is what is being done with
*pyMML*, the python libMML wrapper. It is still as much in development as libMML
is, but the functionality exposed by libMML is succesfully used.

