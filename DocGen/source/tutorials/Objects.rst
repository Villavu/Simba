###########
Object type
###########

The object type is a new datatype in Simba 2.0 which are ref counted objects that have construct/destruct methods that are automatically destroyed when going out of scope.

TImage type is now a object, so you dont have to worry about freeing them.

All objects are constructed using the :code:`new` keyword:

.. code-block::

  obj := new TMyObject();

----

Defining a object
"""""""""""""""""

Objects are defined just how records are.

.. code-block::

  type
    TMyObject = object
      magic: Integer;
    end;

----

Declaring construct and destructors
"""""""""""""""""""""""""""""""""""

.. code-block::

  function TMyObject.Construct(magic: Integer): TMyObject; static;
  begin
    Result.magic := magic;
  end;

  procedure TMyObject.Destroy;
  begin
    WriteLn('Object ', Self.magic, ' is being destroyed');
  end;

----

Example
"""""""

.. code-block::

  type
    TMyObject = object
      magic: Integer;
    end;

  function TMyObject.Construct(magic: Integer): TMyObject; static;
  begin
    Result.magic := magic;
  end;

  procedure TMyObject.Destroy;
  begin
    WriteLn('Object ', Self.magic, ' is being destroyed');
  end;

  procedure Test(obj: TMyObject);
  var
    i: Integer;
  begin
    obj := new TMyObject(1);
    for i := 2 to 4 do
      obj := new TMyObject(i);
  end;

  begin
    WriteLn('Should free objects 1..4');
    Test(new TMyObject(0));
    WriteLn('Done');
    WriteLn('Should free object 0');
  end;