libMML
======

libMML is short for the library to the Mufasa Macro Library.


Why libMML?
-----------

One of the first questions that rise are probably *why* libMML and *what* is
libMML exactly?

libMML is a loadable C-like library that provides most of the MML functionality.
For the ones that do not know, MML is the core component for Simba that does all
the *computational* and *algorithmic* work. See :ref:`mmlref` for more
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

As of writing the pyMML usage looks like this, the passing around of a client
may be removed in a later stage, or at least have it's behaviour changed.

.. code-block:: python

    DLL = MMLCore('../libmml.so')

    client = DLL.dll.create_client()
    print 'Python Client: %d' % client
    if client in (0, 1):
        raise Exception('Could create a client');

    c = Color(DLL, client)


    ret = c.find((0, 0, 100, 100), 0)
    print ret

    ret = c.find_all((0, 0, 100, 100), 0, tol=100)
    print ret

    m = Mouse(DLL, client)

    print m[(Mouse.Pos, Mouse.Left, Mouse.Right)]
    m[(Mouse.Pos, Mouse.Right)] = ((300,300), True)

    print m.getButtonStates()
    sleep(0.5)
    m.setPos((200,200))

    sleep(2)
    print 'Done'

    m[(Mouse.Left, Mouse.Right, Mouse.Middle)] = [False for x in range(3)]
    for v in zip((Mouse.Left, Mouse.Right), m[(Mouse.Left, Mouse.Right)]):
        print v
    print m.getPos()

    del DLL
