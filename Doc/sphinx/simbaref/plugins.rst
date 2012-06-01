.. _writing-simba-plugins:

Writing Simba Plugins
=====================

Plugin overview
---------------

Plugins for Simba can be written in virtually every language. (Make sure to read
`Caveats`_ though)

Simba Plugin ABI
----------------

First of all, it is important to know in what way you should represent your
functions to Simba. Using the newest ABI is recommended. Currently, all three
ABIs are supported; but older ones may be deprecated in subsequent releases.

Plugins with ABI <= 2 are not exported to Lape.

Simba ABI Version 0
*******************

This is the first Simba plugin ABI. Any plugin that does not export
`GetPluginABIVersion`_ is treated as this ABI.

In this ABI `GetTypeInfo`_ passes the arguments as a native pascal **String**
rather than a **PChar**. This is error prone; and versions > 0 use **PChar**.

The calling convention for all functions is expected to be **stdcall**.

Simba ABI Version 1
*******************

In this ABI `GetTypeInfo`_ passes the arguments as a  **PChar**.

The calling convention for all functions (except `GetPluginABIVersion`_)
is expected to be **stdcall**.

Simba ABI Version 2
*******************

The calling convention for all functions is expected to be **cdecl** on
32 bit platforms; on platforms where **cdecl** does not exist, the native
calling convention is expected.

Functions
---------

GetPluginABIVersion
*******************

This function should return the ABI version of the plugin. The calling
convention for this function is **always** **cdecl** on 32 bit and the native
calling convention if **cdecl** does not exist.

SetPluginMemManager
*******************

.. code-block:: pascal

  procedure SetPluginMemManager(MemMgr : TMemoryManager);

This function should be implemented when one is writing a plugin
in Free Pascal.
Using the Simba memory manager it is possible to easily pass arrays and other
managed structures from the script to the plugin. It is however wise to also
store the old memory manager of your plugin and restore it when *OnDetach* is
called.

For other languages, this functions doesn't really help - however you should
realise that it is not possible to pass pascal strings and arrays directly to
the script in a safe manner. Doing so will require some hacks (it is possible),
but might also lead to memory leaks if implemented wrong or used wrong in
scripts.

.. warning::
    Simba does not guarantee that this function will be called only once; make
    sure you don't overwrite your old memory manager when this function is
    called twice.

OnAttach
********

.. code-block:: pascal

  procedure OnAttach(info: Pointer);

This method is called when the plugin is loaded.
Currently *info* will always be zero, but it might be used to pass information
in the future.

OnDetach
********

.. code-block:: pascal

  procedure OnDetach();

This method is called just before the plugin is beeing freed.
*If you changed your memory manager to Simba's; you should now revert to your old
one.*

GetFunctionCount
****************

.. code-block:: pascal

  function GetFunctionCount: integer;

The first function, *GetFunctionCount* returns how many functions are to be
imported.

GetFunctionInfo
***************

.. code-block:: pascal

  function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): integer;

Simba will then call *GetFunctionInfo* and *GetFunctionCallingConv* **N**
amount of times (where **N** is the result of *GetFunctionCount*) with
*x* increased by one every time. Obviously, each function must be mapped
to a specific value of *x*.

For *GetFunctionInfo*, the value of *ProcAddr* should be set to the address of
the procedure to be called; and *ProcDef* should contain the definition (in
Pascal types) of the function.


GetFunctionCallingConv
**********************

.. warning::
    This function is deprecated as of ABI >= 2

.. code-block:: pascal

  function GetFunctionCallingConv(x: integer): integer; stdcall;

*GetFunctionCallingConv* returns the calling convention for the specific
function. Currently, the only two support conventions are *stdcall* (0) and
*register* (1).

GetTypeCount
************

GetTypeInfo
***********

Exporting functions to scripts
------------------------------

To let Simba know what functions you want to export to a script, your plugin
needs to implement the following functions: `GetFunctionCount`_ and
`GetFunctionInfo`_. Refer to their sections

Exporting types to scripts
--------------------------

.. warning::
    TODO

TTarget_Exported
----------------

.. warning::
    TODO

Caveats
-------

If you're writing a plugin in a language other than Free Pascal, you'll not be
able to share arrays and strings with Simba in an easy manner. (It is possible
to "craft" pascal-type strings and arrays)

Pascal Arrays
*************

Say we have an array of *foo* called *bar*. *bar[0]* holds the first element of
the array. *bar* - Sizeof(Pointer) contains the length of the array, and *bar* -
Sizeof(Pointer) * 2 contains the reference count of the array. If you want to
share an array with Simba, make sure the reference is count is high enough so
that Simba/Free Pascal won't try to free it for you.

Pascal Strings
**************

.. warning::
    I believe pascal strings are very similar to pascal arrays, but I am not
    completely sure.

Sharing Arrays and Strings with a FPC Plugin
********************************************

To share arrays and strings in a nice way with a FPC plugin, you need to create
a function called SetPluginMemManager as shown above and make sure it is
exported properly. Simba will try to call this function when loading the plugin
and will pass the plugin its own memory manager. Use FPC's *SetMemoryManager* to
change your own memory manager to Simba's memory manager. When *OnDetach* is
called, make sure you reset your memory manager. See `SetPluginMemManager`_.

Sample FPC Plugin
-----------------

.. code-block:: pascal

    { Example based upon the SPS Plugin }
    library project1;

    {$mode objfpc}{$H+}

    {$macro on}
    {$define callconv:=
        {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
        {$IFDEF LINUX}{$IFDEF CPU32}cdecl;{$ELSE}{$ENDIF}{$ENDIF}
    }

    uses
      classes, sysutils, math
      { you can add units after this };

    var
      OldMemoryManager: TMemoryManager;
      memisset: Boolean = False;


      type T3DIntegerArray = array of array of array of integer;

    function HelloPlugin(s: String): String; callconv
    begin
      result := s;
    end;

    function GetPluginABIVersion: Integer; callconv export;
    begin
      Result := 2;
    end;

    procedure SetPluginMemManager(MemMgr : TMemoryManager); callconv export;
    begin
      if memisset then
        exit;
      GetMemoryManager(OldMemoryManager);
      SetMemoryManager(MemMgr);
      memisset := true;
    end;

    procedure OnDetach; callconv export;
    begin
      SetMemoryManager(OldMemoryManager);
    end;

    function GetTypeCount(): Integer; callconv export;
    begin
      Result := 1;
    end;

    function GetTypeInfo(x: Integer; var sType, sTypeDef: PChar): integer; callconv export;
    begin
      case x of
        0: begin
            StrPCopy(sType, 'T3DIntegerArray');
            StrPCopy(sTypeDef, 'array of array of array of integer;');
           end;

        else
          x := -1;
      end;

      Result := x;
    end;

    function GetFunctionCount(): Integer; callconv export;
    begin
      Result := 1;
    end;

    function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): Integer; callconv export;
    begin
      case x of
        0:
          begin
            ProcAddr := @HelloPlugin;
            StrPCopy(ProcDef, 'function HelloPlugin(s: String): String;');
          end;

        else
          x := -1;
      end;

      Result := x;
    end;

    exports GetPluginABIVersion;
    exports SetPluginMemManager;
    exports GetTypeCount;
    exports GetTypeInfo;
    exports GetFunctionCount;
    exports GetFunctionInfo;
    exports OnDetach;

    begin
    end.
