#############
Plugin Target
#############

A plugin can also provide a target for a script.

.. code-block::

  Target.SetPlugin('myplugin.dll', 'someargs')

Will load :code:`myplugin.dll` expecting these exports:

.. code-block::

  SimbaPluginTarget_Request: function(Args: PChar): Pointer; cdecl;
  SimbaPluginTarget_RequestWithDebugImage: function(Args: PChar; out DebugImage: TSimbaExternalImage): Pointer; cdecl;
  SimbaPluginTarget_Release: procedure(Target: Pointer); cdecl;

  SimbaPluginTarget_GetDimensions: procedure(Target: Pointer; out W, H: Int32); cdecl;
  SimbaPluginTarget_GetImageData: function(Target: Pointer; X, Y, Width, Height: Int32; var Data: PColorBGRA; var DataWidth: Int32): Boolean; cdecl;

  SimbaPluginTarget_MousePressed: function(Target: Pointer; Button: Int32): Boolean; cdecl;
  SimbaPluginTarget_MousePosition: procedure(Target: Pointer; out X, Y: Integer); cdecl;
  SimbaPluginTarget_MouseTeleport: procedure(Target: Pointer; X, Y: Int32); cdecl;
  SimbaPluginTarget_MouseUp: procedure(Target: Pointer; Button: Int32); cdecl;
  SimbaPluginTarget_MouseDown: procedure(Target: Pointer; Button: Int32); cdecl;
  SimbaPluginTarget_MouseScroll: procedure(Target: Pointer; Scrolls: Int32); cdecl;

  SimbaPluginTarget_KeyDown: procedure(Target: Pointer; Key: Int32); cdecl;
  SimbaPluginTarget_KeyUp: procedure(Target: Pointer; Key: Int32); cdecl;
  SimbaPluginTarget_KeySend: procedure(Target: Pointer; Text: PChar; TextLen: Int32; SleepTimes: PInt32); cdecl;
  SimbaPluginTarget_KeyPressed: function(Target: Pointer; Key: Int32): Boolean; cdecl;

-----

KeySend
-------

The plugins KeySend is responsible for holding down modifiers (such as shift).

:code:`SleepTimes` is a Int32 array which is graciously overallocated :code:`(TextLen*4)` of sleep times which should be performed after every keydown/keyrelease which is used to control the speed of typing.

Something like:

.. code-block::

  procedure SimbaPluginTarget_KeySend(Text: PChar; TextLen: Integer; SleepTimes: PInt32);
    
    procedure DoSleep;
    begin
      PreciseSleep(SleepTimes^);
      Inc(SleepTimes);
    end;

  var 
    I: Integer;
  begin
    for I := 0 to TextLen - 1 do
    begin
      PressTheKey();
      DoSleep();
      ReleaseTheKey();
      DoSleep();
    end;
  end;

.. note::
  
  A high resolution :code:`Sleep` is preferred if possible.
