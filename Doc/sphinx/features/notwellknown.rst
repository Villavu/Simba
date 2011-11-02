Not Well Known Features
=======================

Simba has several features that are relatively unknown.
A few will be listed here.

Custom Block Folding
--------------------

TODO

Timestamped Writeln
-------------------

Simba can timestamp all your debug.

.. code-block:: pascal

    SetScriptProp(SP_WriteTimeStamp, [True]);
    WriteLn('Before the wait');
    Wait(1000);
    Writeln('After the wait');

