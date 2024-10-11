##########
Properties
##########

Properties are new in Simba 2.0.

A property is a method which uses the same syntax as a field to invoke.

There are getter and setter properties which implement read/write. Both are not required - for example you can have a read-only property.

.. code-block::

  type
    TMyRecord = record
      Str: String;
    end;

  property TMyRecord.SomeValue: Integer;
  begin
    Result := StrToInt(Self.Str);
  end;

  property TMyRecord.SomeValue(NewValue: Integer);
  begin
    Self.Str := IntToStr(NewValue);
  end;

  var
    myRecord: TMyRecord;

  begin
    myRecord.SomeValue := 123;
    WriteLn myRecord.SomeValue;
  end;