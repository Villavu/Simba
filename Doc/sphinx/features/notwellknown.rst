Not Well Known Features
=======================

Timestamped Writeln
-------------------

Simba can timestamp all your *writeln* calls.

.. code-block:: pascal

    SetScriptProp(SP_WriteTimeStamp, [True]);
    WriteLn('Before the wait');
    Wait(1000);
    Writeln('After the wait');

