unit simba.import_encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils, SynLZ,
  simba.base, simba.script_compiler;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi,
  simba.encoding, simba.hash, simba.compress, simba.image, simba.image_fastcompress;

type
  PImageCompressThread = ^TImageCompressThread;

(*
Encoding
========
Encoding & Hashing
*)

(*
EHashAlgo
--------
```
type EHashAlgo = enum(SHA1, SHA256, SHA384, SHA512, MD5);
```

```{note}
This enum is scoped, so must be used like `EHashAlgo.SHA512`
```
*)

(*
EBaseEncoding
------------
```
type EBaseEncoding = enum(b64URL, b64, b32, b32Hex, b16);
```

```{note}
This enum is scoped, so must be used like `EBaseEncoding.b64`
```
*)

(*
HashData
--------
```
function HashData(Algo: EHashAlgo; Buf: PByte; Len: Int32): String;
```
*)
procedure _LapeHashData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashBuffer(EHashAlgo(Params^[0]^), PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

(*
HashString
----------
```
function HashString(Algo: EHashAlgo; S: String): String;
```
*)
procedure _LapeHashString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashString(EHashAlgo(Params^[0]^), PString(Params^[1])^);
end;

(*
HashFile
--------
```
function HashFile(Algo: EHashAlgo; FileName: String): String;
```
*)
procedure _LapeHashFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashFile(EHashAlgo(Params^[0]^), PString(Params^[1])^);
end;

(*
Hash32
------
```
function Hash32(Data: Pointer; Len: Int32; Seed: UInt32 = 0): UInt32;
```
*)
procedure _LapeHash32(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := Hash32(PPointer(Params^[0])^, PInteger(Params^[1])^, PUInt32(Params^[2])^);
end;

(*
Hash32
------
```
function Hash32(S: String; Seed: UInt32 = 0): UInt32;
```
*)
procedure _LapeHash32String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := Hash32(PString(Params^[0])^, PUInt32(Params^[1])^);
end;

(*
Hash64
------
```
function Hash64(Data: PByte; Len: Int32; Seed: UInt64 = 0): UInt64;
```
*)
procedure _LapeHash64(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := Hash64(PPointer(Params^[0])^, PInteger(Params^[1])^, PUInt64(Params^[2])^);
end;

(*
Hash64
------
```
function Hash64(S: String; Seed: UInt64 = 0): UInt64;
```
*)
procedure _LapeHash64String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := Hash64(PString(Params^[0])^, PUInt64(Params^[1])^);
end;

(*
BaseEncode
----------
```
function BaseEncode(Encoding: EBaseEncoding; const Data: String): String;
```
*)
procedure _LapeBaseEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := BaseEncode(EBaseEncoding(Params^[0]^), PString(Params^[1])^);
end;

(*
BaseDecode
----------
```
function BaseDecode(Encoding: EBaseEncoding; const Data: String): String;
```
*)
procedure _LapeBaseDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := BaseDecode(EBaseEncoding(Params^[0]^), PString(Params^[1])^);
end;

(*
HOTPCalculateToken
------------------
```
function HOTPCalculateToken(const Secret: String; const Counter: Integer): Integer;
```
*)
procedure _LapeHOTPCalculateToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := HOTPCalculateToken(PString(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TOTPCalculateToken
------------------
```
function TOTPCalculateToken(const Secret: String): Integer;
```
*)
procedure _LapeTOTPCalculateToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := TOTPCalculateToken(PString(Params^[0])^);
end;

procedure _LapeImageCompressThread_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Result)^ := TImageCompressThread.Create();
end;

procedure _LapeImageCompressThread_Push(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Params^[0])^.Push(PSimbaImageArray(Params^[1])^);
end;

procedure _LapeImageCompressThread_OnCompressed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Params^[0])^.OnCompressed := TImageCompressedEvent(Params^[1]^);
end;

procedure _LapeImageCompressThread_OnCompressed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageCompressedEvent(Result^) := PImageCompressThread(Params^[0])^.OnCompressed;
end;

procedure _LapeImageCompressThread_TimeUsed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PImageCompressThread(Params^[0])^.TimeUsed;
end;

procedure _LapeImageCompressThread_IsCompressing(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PImageCompressThread(Params^[0])^.IsCompressing;
end;

procedure _LapeImageCompressThread_WaitCompressing(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Params^[0])^.WaitCompressing();
end;

(*
CompressBytes
-------------
```
function CompressBytes(Bytes: TByteArray): TByteArray;
```

```{note}
Zlib compression is used.
```
*)
procedure _LapeCompressBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TByteArray(Result^) := CompressBytes(TByteArray(Params^[0]^));
end;

(*
DecompressBytes
---------------
```
function DecompressBytes(Bytes: TByteArray): TByteArray;
```

```{note}
Zlib compression is used.
```
*)
procedure _LapeDeCompressBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TByteArray(Result^) := DeCompressBytes(TByteArray(Params^[0]^));
end;

(*
CompressString
--------------
```
function CompressString(Data: String; Encoding: EBaseEncoding = EBaseEncoding.b64): String;
```

```{note}
Zlib compression is used.
```
*)
procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := CompressString(PString(Params^[0])^, EBaseEncoding(Params^[1]^));
end;

(*
DecompressBytes
---------------
```
function DeCompressString(Data: String; Encoding: EBaseEncoding = EBaseEncoding.b64): String;
```

```{note}
Zlib compression is used.
```
*)
procedure _LapeDeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := DeCompressString(PString(Params^[0])^, EBaseEncoding(Params^[1]^));
end;

(*
SynLZCompress
-------------
```
function SynLZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
```
*)
procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
SynLZDecompress
---------------
```
function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
```
*)
procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
SynLZCompressDestLen
--------------------
```
function SynLZCompressDestLen(Len: Integer): Integer;
```
*)
procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

(*
SynLZDecompressDestLen
----------------------
```
function SynLZDecompressDestLen(Src: Pointer): Integer;
```
*)
procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPointer(Params^[0])^);
end;

(*
FastCompressImage
-----------------
```
procedure FastCompressImages(Images: TSimbaImageArray; var Data: Pointer; out DataSize: SizeUInt);
```
*)
procedure _LapeFastCompressImages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaImage_FastCompress(TSimbaImageArray(Params^[0]^), PPointer(Params^[1])^, PSizeUInt(Params^[2])^);
end;

(*
FastDeCompressImages
--------------------
```
function FastDeCompressImages(Data: Pointer; DataLen: SizeUInt): TSimbaImageArray;
```
*)
procedure _LapeFastDeCompressImages(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImageArray(Result^) := SimbaImage_FastDeCompress(PPointer(Params^[0])^, PSizeUInt(Params^[1])^);
end;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Encoding';

    addGlobalType('enum(SHA1, SHA256, SHA384, SHA512, MD5)', 'EHashAlgo');
    addGlobalType('enum(b64URL, b64, b32, b32Hex, b16)', 'EBaseEncoding');

    addGlobalFunc('function HOTPCalculateToken(const Secret: String; const Counter: Integer): Integer', @_LapeHOTPCalculateToken);
    addGlobalFunc('function TOTPCalculateToken(const Secret: String): Integer', @_LapeTOTPCalculateToken);

    addGlobalFunc('function BaseEncode(Encoding: EBaseEncoding; const S: String): String', @_LapeBaseEncode);
    addGlobalFunc('function BaseDecode(Encoding: EBaseEncoding; const S: String): String', @_LapeBaseDecode);

    addGlobalFunc('function HashData(Algo: EHashAlgo; Data: Pointer; Len: Int32): String', @_LapeHashData);
    addGlobalFunc('function HashString(Algo: EHashAlgo; S: String): String', @_LapeHashString);
    addGlobalFunc('function HashFile(Algo: EHashAlgo; FileName: String): String', @_LapeHashFile);

    addGlobalFunc('function Hash32(Data: Pointer; Len: Int32; Seed: UInt32 = 0): UInt32; overload', @_LapeHash32);
    addGlobalFunc('function Hash32(S: String; Seed: UInt32 = 0): UInt32; overload', @_LapeHash32String);
    addGlobalFunc('function Hash64(Data: Pointer; Len: Int32; Seed: UInt64 = 0): UInt64; overload', @_LapeHash64);
    addGlobalFunc('function Hash64(S: String; Seed: UInt64 = 0): UInt64; overload', @_LapeHash64String);

    addGlobalFunc('function CompressBytes(Bytes: TByteArray): TByteArray', @_LapeCompressBytes);
    addGlobalFunc('function DecompressBytes(Bytes: TByteArray): TByteArray', @_LapeDecompressBytes);

    addGlobalFunc('function CompressString(S: String; Encoding: EBaseEncoding = EBaseEncoding.b64): String', @_LapeCompressString);
    addGlobalFunc('function DecompressString(S: String; Encoding: EBaseEncoding = EBaseEncoding.b64): String', @_LapeDeCompressString);

    addGlobalFunc('function SynLZCompressDestLen(Len: Integer): Integer', @_LapeSynLZCompressDestLen);
    addGlobalFunc('function SynLZDecompressDestLen(Src: Pointer): Integer', @_LapeSynLZDecompressDestLen);
    addGlobalFunc('function SynLZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZCompress);
    addGlobalFunc('function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZDecompress);

    addGlobalFunc('procedure FastCompressImages(Images: TImageArray; var Data: Pointer; out DataSize: SizeUInt);', @_LapeFastCompressImages);
    addGlobalFunc('function FastDeCompressImages(Data: Pointer; out DataLen: SizeUInt): TImageArray', @_LapeFastDeCompressImages);

    ImportingSection := '';

    addClass('TImageCompressThread');
    addClassConstructor('TImageCompressThread', '', @_LapeImageCompressThread_Create);
    addGlobalType('procedure(Sender: TImageCompressThread; Images: TImageArray; Data: Pointer; DataSize: SizeUInt) of object', 'TImageCompressedEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('procedure TImageCompressThread.Push(Images: TImageArray)', @_LapeImageCompressThread_Push);
    addGlobalFunc('function TImageCompressThread.TimeUsed: Double', @_LapeImageCompressThread_TimeUsed);
    addGlobalFunc('function TImageCompressThread.IsCompressing: Boolean', @_LapeImageCompressThread_IsCompressing);
    addGlobalFunc('procedure TImageCompressThread.WaitCompressing;', @_LapeImageCompressThread_WaitCompressing);
    addProperty('TImageCompressThread', 'OnCompressed', 'TImageCompressedEvent', @_LapeImageCompressThread_OnCompressed_Read, @_LapeImageCompressThread_OnCompressed_Write);
  end;
end;

end.
