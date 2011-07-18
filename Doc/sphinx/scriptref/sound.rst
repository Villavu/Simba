
.. _scriptref_sound:

Multimedia Functions
====================


Sound Functions
---------------

PlaySound
~~~~~~~~~

.. code-block:: pascal

    procedure PlaySound(Sound : string);

PlaySound plays the sound file with the path *Sound*.

Supported formats: *.wav*. (Possibly others, if someone has time to figures
this out please let us know)

Example:

.. code-block:: pascal

    PlaySound('C:\roar.wav');


StopSound
~~~~~~~~~


.. code-block:: pascal

    procedure StopSound;

StopSound stops all currently playing sounds.
