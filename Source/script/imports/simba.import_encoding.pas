unit simba.import_encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi,
  simba.encoding, simba.hash;

(*
Encoding
========
Encoding & Hashing
*)

(*
HashAlgo
--------
> type HashAlgo = enum(SHA1, SHA256, SHA384, SHA512, MD5);

```{note}
This enum is scoped, so must be used like `HashAlgo.SHA512`
```
*)

(*
BaseEncoding
------------
> type BaseEncoding = enum(b64URL, b64, b32, b32Hex, b16);

```{note}
This enum is scoped, so must be used like `BaseEncoding.b64`
```
*)

(*
HashData
--------
> function HashData(Algo: HashAlgo; Buf: PByte; Len: Int32): String;
*)
procedure _LapeHashData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashBuffer(HashAlgo(Params^[0]^), PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

(*
HashString
----------
> function HashString(Algo: HashAlgo; S: String): String;
*)
procedure _LapeHashString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashString(HashAlgo(Params^[0]^), PString(Params^[1])^);
end;

(*
HashFile
--------
> function HashFile(Algo: HashAlgo; FileName: String): String;
*)
procedure _LapeHashFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashFile(HashAlgo(Params^[0]^), PString(Params^[1])^);
end;

(*
Hash32
------
> function Hash32(Data: Pointer; Len: Int32; Seed: UInt32 = 0): UInt32;
*)
procedure _LapeHash32(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := Hash32(PPointer(Params^[0])^, PInteger(Params^[1])^, PUInt32(Params^[2])^);
end;

(*
Hash32
------
> function Hash32(S: String; Seed: UInt32 = 0): UInt32;
*)
procedure _LapeHash32String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := Hash32(PString(Params^[0])^, PUInt32(Params^[1])^);
end;

(*
Hash64
------
> function Hash64(Data: PByte; Len: Int32; Seed: UInt64 = 0): UInt64;
*)
procedure _LapeHash64(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := Hash64(PPointer(Params^[0])^, PInteger(Params^[1])^, PUInt64(Params^[2])^);
end;

(*
Hash64
------
> function Hash64(S: String; Seed: UInt64 = 0): UInt64;
*)
procedure _LapeHash64String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := Hash64(PString(Params^[0])^, PUInt64(Params^[1])^);
end;

(*
BaseEncode
----------
> function BaseEncode(Encoding: BaseEncoding; const Data: String): String;
*)
procedure _LapeBaseEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := BaseEncode(BaseEncoding(Params^[0]^), PString(Params^[1])^);
end;

(*
BaseDecode
----------
> function BaseDecode(Encoding: BaseEncoding; const Data: String): String;
*)
procedure _LapeBaseDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := BaseDecode(BaseEncoding(Params^[0]^), PString(Params^[1])^);
end;

(*
HOTPCalculateToken
------------------
> function HOTPCalculateToken(const Secret: String; const Counter: Integer): Integer;
*)
procedure _LapeHOTPCalculateToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := HOTPCalculateToken(PString(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TOTPCalculateToken
------------------
> function TOTPCalculateToken(const Secret: String): Integer;
*)
procedure _LapeTOTPCalculateToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := TOTPCalculateToken(PString(Params^[0])^);
end;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Encoding';

    addGlobalType('enum(SHA1, SHA256, SHA384, SHA512, MD5)', 'HashAlgo');
    addGlobalType('enum(b64URL, b64, b32, b32Hex, b16)', 'BaseEncoding');

    addGlobalFunc('function HOTPCalculateToken(const Secret: String; const Counter: Integer): Integer', @_LapeHOTPCalculateToken);
    addGlobalFunc('function TOTPCalculateToken(const Secret: String): Integer', @_LapeTOTPCalculateToken);

    addGlobalFunc('function BaseEncode(Encoding: BaseEncoding; const S: String): String', @_LapeBaseEncode);
    addGlobalFunc('function BaseDecode(Encoding: BaseEncoding; const S: String): String', @_LapeBaseDecode);

    addGlobalFunc('function HashData(Algo: HashAlgo; Data: Pointer; Len: Int32): String', @_LapeHashData);
    addGlobalFunc('function HashString(Algo: HashAlgo; S: String): String', @_LapeHashString);
    addGlobalFunc('function HashFile(Algo: HashAlgo; FileName: String): String', @_LapeHashFile);

    addGlobalFunc('function Hash32(Data: Pointer; Len: Int32; Seed: UInt32 = 0): UInt32; overload', @_LapeHash32);
    addGlobalFunc('function Hash32(S: String; Seed: UInt32 = 0): UInt32; overload', @_LapeHash32String);
    addGlobalFunc('function Hash64(Data: Pointer; Len: Int32; Seed: UInt64 = 0): UInt64; overload', @_LapeHash64);
    addGlobalFunc('function Hash64(S: String; Seed: UInt64 = 0): UInt64; overload', @_LapeHash64String);

    ImportingSection := '';
  end;
end;

end.

