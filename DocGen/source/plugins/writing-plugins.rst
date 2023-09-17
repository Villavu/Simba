###############
Writing Plugins
###############

Simba scripts can be extended with plugins.
Plugins are libraries (dll/so/dylib) which can be written in virtually every low-level language.  

.. note::
  
  For 32bit the **cdecl** calling convention is used for methods plugin related. Otherwise the default systems calling convention is used.

-----

Importing functions
-------------------

To import functions the following methods must be exported to provide information to Simba.

.. code-block::

    function GetFunctionInfo(Index: Integer; var Address: Pointer; var Header: PChar): Integer; cdecl;
    function GetFunctionCount: Integer; cdecl;

.. note:: 

  | Appending :code:`native;` to the header cause parameters and result passed in the Lape wrapper format.
  | This is the preferred way, otherwise libffi is used.

  The Lape wrapper format is :code:`procedure(const Params: PParamArray; const Result: Pointer)`

-----

Importing types
---------------

To import types the following methods must be exported to provide information to Simba.

.. code-block::

    function GetTypeInfo(Index: Integer; var Name: PChar; var Str: PChar): Integer; cdecl;
    function GetTypeCount: Integer; cdecl;                 

.. note::
  
  These methods are imported before GetFunctionInfo & GetFunctionCount so these types will be available when adding functions.

-----

Importing code
--------------

Code can be imported in the script with the following methods exported.

.. code-block::

  procedure GetCode(var Code: PChar); cdecl;
  function GetCodeLength: Integer; cdecl;  

-----

RegisterSimbaPlugin
-------------------

A plugin can export `RegisterSimbaPlugin` which provides access to various Simba's infomation & methods.

.. code-block::

  procedure RegisterSimbaPlugin(Infomation: PSimbaInfomation; Methods: PSimbaMethods); cdecl;

These pointers are provided to these structures:

.. code-block::

  TSimbaInfomation = packed record
    SimbaVersion: Integer;
    SimbaMajor: Integer;

    FileName: PChar;

    Compiler: Pointer;
  end;

  TSimbaMethods = packed record
    RunOnMainThread: procedure(Method: TMainThreadMethod; Data: Pointer = nil); cdecl;

    GetMem: function(Size: NativeUInt): Pointer; cdecl;
    FreeMem: function(P: Pointer): NativeUInt; cdecl;
    AllocMem: function(Size: NativeUInt): Pointer; cdecl;
    ReAllocMem: function(var P: Pointer; Size: NativeUInt): Pointer; cdecl;
    MemSize: function(P: Pointer): NativeUInt; cdecl;

    RaiseException: procedure(Message: PChar); cdecl;

    GetTypeInfo: function(Compiler: Pointer; Typ: PChar): Pointer; cdecl;
    GetTypeInfoSize: function(TypeInfo: Pointer): NativeInt; cdecl;
    GetTypeInfoFieldOffset: function(TypeInfo: Pointer; FieldName: PChar): NativeInt; cdecl;

    AllocateRawArray: function(ElementSize, Len: NativeInt): Pointer; cdecl;
    ReAllocateRawArray: procedure(var Arr: Pointer; ElementSize, NewLen: NativeInt); cdecl;

    AllocateArray: function(TypeInfo: Pointer; Len: NativeInt): Pointer; cdecl;
    AllocateString: function(Data: PChar): Pointer; cdecl;
    AllocateUnicodeString: function(Data: PUnicodeChar): Pointer; cdecl;

    SetArrayLength: procedure(TypeInfo: Pointer; var AVar: Pointer; NewLen: NativeInt); cdecl;
    GetArrayLength: function(AVar: Pointer): NativeInt; cdecl;

    ExternalImage_Create: function(FreeOnTerminate: Boolean): Pointer; cdecl;
    ExternalImage_SetMemory: procedure(Img: Pointer; Data: PColorBGRA; AWidth, AHeight: Integer); cdecl;
    ExternalImage_TryLock: function(Img: Pointer): Boolean; cdecl;
    ExternalImage_Lock: procedure(Img: Pointer); cdecl;
    ExternalImage_UnLock: procedure(Img: Pointer); cdecl;

    ExternalImage_AddCallbackOnUnlock: procedure(Img: Pointer; Callback: TSimbaExternalImageCallback); cdecl;
    ExternalImage_RemoveCallbackOnUnlock: procedure(Img: Pointer; Callback: TSimbaExternalImageCallback); cdecl;  
  end; 

See the `Example C++ plugin <plugin-cpp.html>`_ to see how TypeInfo is used to allocate custom records & arrays.

.. note::
  
  `RunOnMainThread` is available for creating forms, installing windows hooks and other things that must be done on the main thread.
  Simba scripts are not ran on the processes main thread for this reason.
