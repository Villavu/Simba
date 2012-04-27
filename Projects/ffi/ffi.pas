unit ffi;

{$mode objfpc}{$H+}
{$linklib libffi}

interface

uses
    Classes, SysUtils, ctypes;

(*
TODO:
  -  Add ARM compat. Replace cpu32 ifdef's with better ifdefs.
  -  Mac compat.
  -  Test architectures.
  -  Clean up some parameters (I think we can use more arrays and less pointers)
*)

type
  TFFIStatus = (
      FFI_OK := 0,
      FFI_BAD_TYPEDEF,
      FFI_BAD_ABI
  );

  TFFIABI = (
      FFI_FIRST_ABI := 0,
  {$IFDEF LINUX}
    FFI_SYSV,
    FFI_UNIX64,   { Unix variants all use the same ABI for x86-64  }
    FFI_LAST_ABI,

    {$IFDEF CPU32}
    FFI_DEFAULT_ABI := FFI_SYSV
    {$ELSE}
    FFI_DEFAULT_ABI := FFI_UNIX64
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    {$IFDEF CPU32}
      FFI_SYSV,
      FFI_STDCALL,
      FFI_THISCALL,
      FFI_FASTCALL,
      FFI_MS_CDECL,
      FFI_LAST_ABI,
      FFI_DEFAULT_ABI := FFI_MS_CDECL
  (*
    TODO:
    #ifdef _MSC_VER
      FFI_DEFAULT_ABI = FFI_MS_CDECL
    #else
      FFI_DEFAULT_ABI = FFI_SYSV
    #endif
  *)
    {$ELSE}
      FFI_WIN64,
      FFI_LAST_ABI,
      FFI_DEFAULT_ABI := FFI_WIN64
    {$ENDIF}
  {$ENDIF}
  );
  PFFIType = ^TFFIType;
  PPFFIType = ^PFFIType;

  TFFIType = record
    size: csize_t;
    alignment: cushort;
    _type: cushort;

    elements: PPFFIType;
  end;

  TFFICif = record
    abi: TFFIABI;
    nargs: cunsigned;
    arg_types: PPFFIType;
    rtype: PFFIType;
    bytes: cunsigned;
    flags: cunsigned;
    {$IFDEF FFI_EXTRA_FIELDS}
    // TODO
    {$ENDIF}
  end;
  PFFICif = ^TFFICif;


  TPointerArray = Array of Pointer;
  TClosureBindingFunction = procedure(var cif: TFFICif; var ret: cuint;
    args: TPointerArray; userdata: Pointer); cdecl;

  (*
  #if defined (X86_64) || (defined (__x86_64__) && defined (X86_DARWIN))
  #define FFI_TRAMPOLINE_SIZE 24
  #define FFI_NATIVE_RAW_API 0
  #else
  #ifdef X86_WIN32
  #define FFI_TRAMPOLINE_SIZE 52
  #else
  #ifdef X86_WIN64
  #define FFI_TRAMPOLINE_SIZE 29
  #define FFI_NATIVE_RAW_API 0
  #define FFI_NO_RAW_API 1
  #else
  #define FFI_TRAMPOLINE_SIZE 10
  #endif
  #endif
  *)

const
  FFI_TRAMPOLINE_SIZE =
  {$IFDEF WINDOWS}
  {$IFDEF CPU32}
  52
  {$ELSE}
  29
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
  {$IFDEF CPU32}
  10
  {$ELSE}
  24
  {$ENDIF}
  {$ENDIF}
  ;

  (*
  #if FFI_CLOSURES

  #ifdef _MSC_VER
  __declspec(align(8))
  #endif
  typedef struct {
  #if @FFI_EXEC_TRAMPOLINE_TABLE@
    void *trampoline_table;
    void *trampoline_table_entry;
  #else
    char tramp[FFI_TRAMPOLINE_SIZE];
  #endif
    ffi_cif   *cif;
    void     (*fun)(ffi_cif*,void*,void**,void*);
    void      *user_data;
  #ifdef __GNUC__
  } ffi_closure __attribute__((aligned (8)));
  #else
  } ffi_closure;
  # ifdef __sgi
  #  pragma pack 0
  # endif
  #endif
  *)

type
  TFFIClosure = record
    tramp: array [0..FFI_TRAMPOLINE_SIZE] of cchar; // Let's hope FFI_EXEC_TRAMPOLINE_TABLE is not defined/true
    cif: PFFICif;
    fun: TClosureBindingFunction;
    user_data: Pointer;
  end;
  PFFIClosure = ^TFFIClosure;

function ffi_prep_cif(out cif: TFFICif; abi: TFFIABI; nargs: cuint; rtype: PFFIType;
    atypes: PPFFIType): TFFIStatus;  cdecl; external;
procedure ffi_call(var cif: TFFICif; fn: Pointer; rvalue: Pointer; avalue: Pointer);
  cdecl; external;

function ffi_closure_alloc(size: csize_t; code: Pointer): Pointer;  cdecl; external;
procedure ffi_closure_free(closure: Pointer); cdecl; external;

{ I don't think we need this one }
{
function ffi_prep_closure(closure: PFFIClosure; var CIF: TFFICif;
    fun: TClosureBindingFunction; user_data: Pointer): TFFIStatus;  cdecl; external;
}

function ffi_prep_closure_loc(closure: PFFIClosure; var CIF: TFFICif;
    fun: TClosureBindingFunction; user_data: Pointer; codeloc: Pointer): TFFIStatus;  cdecl; external;

var
  ffi_type_void: TFFIType; cvar; external;
  ffi_type_uint8: TFFIType; cvar; external;
  ffi_type_sint8: TFFIType; cvar; external;
  ffi_type_uint16: TFFIType; cvar; external;
  ffi_type_sint16: TFFIType; cvar; external;
  ffi_type_uint32: TFFIType; cvar; external;
  ffi_type_sint32: TFFIType; cvar; external;
  ffi_type_uint64: TFFIType; cvar; external;
  ffi_type_sint64: TFFIType; cvar; external;
  ffi_type_float: TFFIType; cvar; external;
  ffi_type_double: TFFIType; cvar; external;
  ffi_type_pointer: TFFIType; cvar; external;

implementation


end.
