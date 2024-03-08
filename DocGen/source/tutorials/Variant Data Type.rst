#################
Variant data type
#################

The variant datatype can store most base types.

.. code-block::

  var 
    v: Variant;
  begin
    WriteLn('Should be unassigned: ', not v.IsAssigned());
    WriteLn();

    v := 'I am a string';
    Writeln('Now should *not* be unassigned: ', v.IsAssigned());
    WriteLn('And should be string:');
    WriteLn(v.VarType, ' -> ', v);
    WriteLn();

    v := Int64(123);
    WriteLn('Now should be Int64:');
    WriteLn(v.VarType, ' -> ', v);
    WriteLn();

    v := 0.123456;
    WriteLn('Now should be Double:');
    WriteLn(v.VarType, ' -> ', v);
  end;


.. Note::

  If curious to how the Variant datatype works, internally it's a record:

  .. code-block::
    
    // pseudo code
    type
      InternalVariantDataType = record
        VarType: EVariantType;
        Value: array[0..SizeOf(LargestDataTypeVariantCanStore)] of Byte;
      end;