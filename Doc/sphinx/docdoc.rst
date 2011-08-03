.. _docdoc:

Documentation Documentation
===========================

This page is the documentation for the documentation.

It is very important to know this by heart when you are writing documentation
for Simba.

Documentation system
--------------------
The documentation system we use is sphinx. The link to sphinx can
be found at the bottom of the page.

.. note::
    It is important to note that there is also a database SQL fulltext engine
    called Sphinx, but this is not the project we use. We use the Sphinx
    "documentation system" ( http://sphinx.pocoo.org/ )

Building the documentation
--------------------------

In the future, the online documentation will be refreshed every hour. However,
if you want to build the documentation yourself, you should install
``python-sphinx``.

Move to the ``Simba/doc/sphinx`` directory and run ``make all``.
This will place an HTML version of the documentation in ``_build/html``.

.. note::
    The build instructions are for Linux only. If you want to build the doc on
    Windows, you are on your own. The sphinx resource site is probably a good
    place to start.

Writing documentation
---------------------

Sphinx uses the reStructuredText markup language. It is not a hard language, but
looking through the quickstart is a good idea:
http://docutils.sourceforge.net/docs/user/rst/quickstart.html

As stated above, the markup language is not the hard part about writing
documentation; the hard part is simply coming up with good content suited for
the documentation. Be sure to check :ref:`todo`

Directory structure
~~~~~~~~~~~~~~~~~~~

So you have written a new piece of documentation? Great!

Now we just need to know where to place it. If you have simply extended a file,
then there should be no worries as to where to place your new text. However if
you are writing a new chapter, then placing the file in the correct directory is
something we'd like you to consider.

If you write a chapter for the ``Simba Reference`` or ``Scripting Reference``
or ``Features``
part of the documentation, place it in the ``simbaref``, ``scriptref`` or
``features`` folder repectively.
Any other files can be put directly in the root of the sphinx folder.
(The same place as ``index.rst``)

Capitalisation
~~~~~~~~~~~~~~

The titles of all major sections have all words capitalized. (The ones with
===)
The minor sections and subsections (---) and (~~~) have only the first word and
Simba specific words (like Simba itself) capitalized.

Try to stick to the Python documentation standards.
( http://docs.python.org/using/index.html )

References
----------

Sphinx has references, most of the .rst files contain labels and references.

A label looks something like this:

.. code-block:: python

    .. _<X>-<Y>:

Where X is either the name of file or folder; and Y is the name of the
file/function if X is a folder. For referring to specific functions for example,
use:

.. code-block:: python

    .. _scriptref-movemouse:

To define a label for the MoveMouse function. Labels are always placed right
above section/chapter declarations.

