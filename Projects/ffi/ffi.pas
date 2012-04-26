unit ffi;

{$mode objfpc}{$H+}
{$linklib libffi}

interface

uses
    Classes, SysUtils, ctypes;

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

function ffi_prep_cif(out cif: TFFICif; abi: TFFIABI; nargs: cuint; rtype: PFFIType;
    atypes: PPFFIType): TFFIStatus;  cdecl; external;
procedure ffi_call(var cif: TFFICif; fn: Pointer; rvalue: Pointer; avalue: Pointer);
  cdecl; external;

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
