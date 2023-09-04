###########
Sleep Until
###########

`SleepUntil` is a magic method in Simba.
It's magic because can pass in any boolean expression which is normally not possible.

.. code-block::

  function SleepUntil(Condition: BoolExpr; Interval, Timeout: Int32): Boolean;

The expression will be called every `Interval` until the expression returns True or `Timeout` is reached.

Some examples:

.. code-block::
  
  if SleepUntil(Random(100) = 1, 50, 5000) then
    WriteLn('Random(100) returned 1 within 5 seconds (checked every 50ms)!');

.. code-block::
  
  SleepUntil((Random(5)+Random(5) = 5) and (GetTimeRunning() > 500), 50, 5000);