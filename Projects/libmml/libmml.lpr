library libmml;

{
libMML - the C-like interface to MML.

Will (eventually) support the following functionality:
    [ ] -   Bitmap creation and find functions.
    [ ] -   DTM creation and find functions
    [/] -   Color find, get (and convert?) functions.
    [/] -   Mouse/Keyboard functions.
    [ ] -   OCR Engine + functions. (font loader?)

It should not include:
    -   Simba file functions (the stdlib of other languages should suffice)
    -   Simba web functions (the stdlib of other languages should suffice)

Debatable:
    -   TPointArray functions, the languages themselves should probably handle
        this.
    [ ] -   TMufasaBitmap class?
}

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client,sysutils,MufasaTypes,dtmutil, dtm;

//{$R *.res}

(*
Constants
=========

Constants defined by libMML:

.. code-block:: c

    #define RESULT_OK       0
    #define RESULT_FALSE    1
    #define RESULT_ERROR    -1

    #define MOUSE_UP        0
    #define MOUSE_DOWN      1
*)

Const
  RESULT_OK = 0;
  RESULT_FALSE = 1;
  RESULT_ERROR = -1;

  MOUSE_UP = 0;
  MOUSE_DOWN = 1;

var
  last_error: String; // TODO: make it default to ''
  debug: boolean; // TODO: make it default to false


(*

libMLL Specific Functions
=========================


*)

(*
init
----

This function must be called immediately after loading the library.
*)
function init: integer;  cdecl;
begin
  last_error := '';
  debug := true; // TODO: Change this to false
  result := RESULT_OK;
end;

(*
get_last_error
--------------

Returns a char pointer.

.. DANGER::

    VERY IMPORTANT: If you use get_last_error, you must immediately store the
    resulting string somewhere else. As soon as you do other calls, the last
    error may be reset or assigned a different memory position, making your old
    pointer invalid.

    TODO: Perhaps copy it to another pchar and return that? Just let the program
    using the library free it?

*)
function get_last_error: pchar; cdecl;
begin
  exit(@last_error[1]);
end;

procedure set_last_error(s: string);
begin
  last_error := s;
  if debug then
      writeln('ERROR: ' + s);
end;

{ Validate the TClient. If it is NULL, set last error and return false }
function validate_client(C: TClient): boolean; inline;
begin
  result := Assigned(C);
  if not result then
  begin
    last_error := 'TClient is NULL';
    if debug then
      writeln(last_error);
  end;
end;

{
  Create a TClient. A TClient contains all the functions.
  You can use multiple, but you'll have to manage them yourself.
}
function create_client: PtrUInt; cdecl;
var
  C: TClient;
begin
  try
    C := TClient.Create('');
    Result := PtrUInt(C);
  except on e : Exception do
  begin
    // FIXME UINT negative
    result := PtrUInt(RESULT_ERROR);
    set_last_error(e.message);
  end;
  end;
  writeln(format('C: %d, IOManager: %d', [PtrUInt(C), PtrUInt(C.IOManager)]));
end;

{ Destroy a TClient }
function destroy_client(C: TClient): integer; cdecl;
begin
  try
    C.Free;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    set_last_error(e.message);
  end;
  end;
end;

{ Set (verbose) debug on/off }
procedure set_debug(v: Boolean); cdecl;
begin
  debug := v;
end;

{ Get debug }
function get_debug: boolean; cdecl;
begin
  exit(debug);
end;

{ Turn an array into a pointer. The pointer memory is not managed by FPC, so we can pass
  it along happily. It'll have to be freed by the external control though }
function array_to_ptr(ptr: Pointer; size: PtrUInt; objsize: PtrUInt): Pointer; cdecl;
begin
  result := GetMem(objsize * size);
  Move(ptr^, result^, objsize * size);
end;

{ Free memory previously allocated by libMML }
function free_ptr(ptr: pointer): boolean; cdecl;
begin
  result := Assigned(ptr);
  if not result then
  begin
    set_last_error('TClient is NULL');
    if debug then
      writeln(last_error);
  end else
    FreeMem(ptr);
end;

{ Allocate memory with libMML }
function alloc_mem(size, objsize: PtrUInt): Pointer; cdecl;
begin
  result := GetMem(size * objsize);
end;

{ Reallocate memory with libMML }
function realloc_mem(ptr: Pointer; size, objsize: PtrUInt): Pointer; cdecl;
begin
  result := ReAllocMem(ptr, size*objsize);
end;

{$I input.pas}
{$I colour.pas}
{$I dtm.pas}


(*
Client Functions
================



*)

function set_array_target(C: TClient; Arr: PRGB32; Size: TPoint): integer; 
    cdecl;
begin
  if not assigned(Arr) then
  begin
    set_last_error('Arr is not assigned');
    exit(RESULT_FALSE);
  end;

  try
    C.IOManager.SetTarget(Arr, Size);
    result := RESULT_OK;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_FALSE;
    end;
  end;
end;

exports

  init,
  create_client,
  destroy_client,
  get_last_error,
  get_debug,
  set_debug,
  alloc_mem,
  realloc_mem,
  free_ptr,

  get_mouse_pos, set_mouse_pos,
  get_mouse_button_state, set_mouse_button_state,

  get_color,

  find_color, find_color_tolerance,

  find_colors, find_colors_tolerance,

  create_dtm, delete_dtm, add_dtm, remove_dtm,

  find_dtm,

  set_array_target;


begin
end.
