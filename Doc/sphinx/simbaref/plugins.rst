.. _writing-simba-plugins:

Writing Simba Plugins
=====================

.. warning::
    This is work in progress.

Plugin overview
---------------

Plugins for Simba can be written in virtually every language. (Make sure to read
`Caveats`_ though)

Calling Conventions
-------------------

Currently, all methods in `Simba Plugin Functions`_ must follow the *stdcall*
calling convention on x86 architectures. On x64 the native calling convention of
the operating system is used, this convention differs per platform. As long as
you don't define the calling convention you should be fine.

.. warning::

    In the future stdcall will be replaced by cdecl. This will most likely
    happen when Simba 1.0 is released.

Functions that are to be imported by a script can currently use several calling
convention, but we highly recommend using *stdcall* on x86 (until replaced with
*cdecl*) and the native convention on x64.

Exporting functions to scripts
------------------------------

To let Simba know what functions you want to export to a script, your plugin
needs to implement the following functions:

.. code-block:: pascal

  GetFunctionCount: function: integer; stdcall;
  GetFunctionInfo: function(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): integer; stdcall;
  GetFunctionCallingConv: function(x: integer): integer; stdcall;

The first function, *GetFunctionCount* returns how many functions are to be
imported.

Simba will then call *GetFunctionInfo* and *GetFunctionCallingConv* **N**
amount of times (where **N** is the result of *GetFunctionCount*) with
*x* increased by one every time. Obviously, each function must be mapped
to a specific value of *x*.

For *GetFunctionInfo*, the value of *ProcAddr* should be set to the address of
the procedure to be called; and *ProcDef* should contain the definition (in
Pascal types) of the function.

*GetFunctionCallingConv* returns the calling convention for the specific
function. Currently, the only two support conventions are *stdcall* (0) and
*register* (1).

Exporting types to scripts
--------------------------

.. warning::
    TODO

Special Functions
-----------------

.. code-block:: pascal

  OnAttach: procedure(info: Pointer); stdcall;

This method is called when the plugin is loaded.
Currently *info* will always be zero, but it might be used to pass information
in the future.

.. code-block:: pascal

  OnDetach: procedure(); stdcall;

This method is called just before the plugin is beeing freed.

.. code-block:: pascal

  SetPluginMemManager: procedure(MemMgr : TMemoryManager); stdcall;

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

TTargetExported
---------------

.. warning::
    TODO

Overview of all functions
-------------------------

.. warning::
    Remove this section


Simba Plugin Functions
----------------------

.. code-block:: pascal

  GetFunctionCount: function: integer; stdcall;
  GetFunctionInfo: function(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar: integer; stdcall;
  GetFunctionCallingConv: function(x: integer): integer; stdcall;
  GetTypeCount: function: integer; stdcall;
  GetTypeInfo: function(x: Integer; var sType, sTypeDef: string): integer; stdcall;
  SetPluginMemManager: procedure(MemMgr : TMemoryManager); stdcall;
  OnAttach: procedure(info: Pointer); stdcall;
  OnDetach: procedure(); stdcall;

Caveats
-------

If you're writing a plugin in a language other than Free Pascal, you'll not be
able to share arrays and strings with Simba in an easy manner. (It is possible
to "craft" pascal-type strings and arrays)

Pascal Strings
**************


Pascal Arrays
*************

Say we have an array of *foo* called *bar*. *bar[0]* holds the first element of
the array. *bar* - Sizeof(Pointer) contains the length of the array, and *bar* -
Sizeof(Pointer) * 2 contains the reference count of the array. If you want to
share an array with Simba, make sure the reference is count is high enough so
that Simba/Free Pascal won't try to free it for you.

Sharing Arrays and String with a FPC Plugin
*******************************************

To share arrays and strings in a nice way with a FPC plugin, you need to create
a function called SetPluginMemManager as shown above and make sure it is
exported properly. Simba will try to call this function when loading the plugin
and will pass the plugin its own memory manager. Use FPC's *SetMemoryManager* to
change your own memory manager to Simba's memory manager.
