##############
Random Numbers
##############

Simba provides various methods to generate random numbers with different distributions.

The following methods are:

Random:
"""""""

Random generates approximately uniform distribution. 

.. code-block::

  function Random: Double; overload;
  function Random(l: Int64): Int64; overload;   
  function Random(min, max: Double): Double; overload;
  function Random(min, max: Int64): Int64; overload;

-----

RandomLeft:
"""""""""""

RandomLeft generates weighted numbers towards :code:`Lo`

.. code-block::

  function RandomLeft(Lo, Hi: Double): Double; overload;
  function RandomLeft(Lo, Hi: Int64): Int64; overload;

-----

RandomRight:
""""""""""""

RandomRight generates weighted numbers towards :code:`Hi`

.. code-block::

  function RandomRight(Lo, Hi: Double): Double; overload;
  function RandomRight(Lo, Hi: Int64): Int64; overload;

-----

RandomMean:
"""""""""""

RandomMean generates weighted numbers towards the mean of :code:`Lo..Hi`

.. code-block::

  function RandomMean(Lo, Hi: Double): Double; overload;
  function RandomMean(Lo, Hi: Int64): Int64; overload;

-----

RandomMode:
"""""""""""

RandomMode generates weighted numbers towards :code:`Mode` within :code:`Lo..Hi`

.. code-block::

  function RandomMode(Mode, Lo, Hi: Double): Double; overload;
  function RandomMode(Mode, Lo, Hi: Int64): Int64; overload;

-----

| You can also write a small script to show the spread.
| This shows the spread of 1000000 calls of :code:`RandomLeft(0, 10)`:

.. code-block::

  const
    SampleCount = 1000000;
    Range = 10;

  var
    Hits: TIntegerArray;
    I: Integer;
  begin
    SetLength(Hits, Range);
    for I := 1 to SampleCount do
      Hits[RandomLeft(0, Range)] += 1;

    for I := 0 to High(Hits) do
      WriteLn(I, ' was rolled ', Hits[I], ' times (', Round((Hits[I] / SampleCount) * 100, 4), ' percent)');
  end;          

Output

.. code-block:: none

  0 was rolled 382874 times (38.2874 percent)
  1 was rolled 300305 times (30.0305 percent)
  2 was rolled 183102 times (18.3102 percent)
  3 was rolled 88345 times (8.8345 percent)
  4 was rolled 32958 times (3.2958 percent)
  5 was rolled 9715 times (0.9715 percent)
  6 was rolled 2195 times (0.2195 percent)
  7 was rolled 442 times (0.0442 percent)
  8 was rolled 60 times (0.006 percent)
  9 was rolled 4 times (0.0004 percent) 