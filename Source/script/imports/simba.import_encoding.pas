unit simba.import_encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.script,
  simba.script_objectutil;

procedure ImportEncoding(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.encoding,
  simba.hash,
  simba.compress,
  simba.resourcefile;

(*
Encoding
========
Encoding & Hashing
*)

(*
EHashAlgo
---------
```
type EHashAlgo = enum(CRC32, MD4, MD5, SHA1, SHA256, SHA512);
```

```{note}
This enum is scoped, so must be used like `EHashAlgo.SHA512`
```
*)

(*
EBaseEncoding
-------------
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
Hash
----
```
function Hash(Data: Pointer; Len: Int32; Seed: UInt32 = 0): UInt32;
```
Computes a UInt32 hash of data using xxhash32 algorithm.
https://xxhash.com/
*)
procedure _LapeHash(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := Hash(PPointer(Params^[0])^, PInteger(Params^[1])^, PUInt32(Params^[2])^);
end;

(*
Hash
----
```
function Hash(S: String; Seed: UInt32 = 0): UInt32;
```
Computes a UInt32 hash of string using xxhash32 algorithm.
https://xxhash.com/
*)
procedure _LapeHashStr(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := Hash(PString(Params^[0])^, PUInt32(Params^[1])^);
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
ECompressAlgo
-------------
```
type ECompressAlgo = enum(ZLIB, SYNLZ, GZ, BZIP2, LZ4, LZMA);
```
```{note}
This enum is scoped, so must be used like `ECompressAlgo.ZLIB`
```
*)

(*
CompressData
------------
```
procedure CompressData(Algo: ECompressAlgo; InData: Pointer; InSize: Int64; out OutData: Pointer; out OutSize: Int64; Truncate: Boolean = True);
```
*)
procedure _LapeCompressData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  CompressData(ESimbaCompressAlgo(Params^[0]^), PPByte(Params^[1])^, PInt64(Params^[2])^, PPByte(Params^[3])^, PInt64(Params^[4])^, PBoolean(Params^[5])^);
end;

(*
DecompressData
--------------
```
procedure DecompressData(Algo: ECompressAlgo; InData: Pointer; InSize: Int64; out OutData: Pointer; out OutSize: Int64; Truncate: Boolean = True);
```
*)
procedure _LapeDecompressData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  DecompressData(ESimbaCompressAlgo(Params^[0]^), PPByte(Params^[1])^, PInt64(Params^[2])^, PPByte(Params^[3])^, PInt64(Params^[4])^, PBoolean(Params^[5])^);
end;

(*
CompressBytes
-------------
```
function CompressBytes(Algo: ECompressAlgo; Bytes: TByteArray): TByteArray;
```
*)
procedure _LapeCompressBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := CompressBytes(ESimbaCompressAlgo(Params^[0]^), PByteArray(Params^[1])^);
end;

(*
DecompressBytes
---------------
```
function DecompressBytes(Algo: ECompressAlgo; Bytes: TByteArray): TByteArray;
```
*)
procedure _LapeDecompressBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := DecompressBytes(ESimbaCompressAlgo(Params^[0]^), PByteArray(Params^[1])^);
end;

(*
CompressString
--------------
```
function CompressString(Algo: ECompressAlgo; Encoding: EBaseEncoding; Str: String): String;
```
*)
procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := CompressString(ESimbaCompressAlgo(Params^[0]^), EBaseEncoding(Params^[1]^), PString(Params^[2])^);
end;

(*
DecompressString
----------------
```
function DecompressString(Algo: ESimbaCompressAlgo; Encoding: EBaseEncoding; Str: String): String;
```
*)
procedure _LapeDecompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := DecompressString(ESimbaCompressAlgo(Params^[0]^), EBaseEncoding(Params^[1]^), PString(Params^[2])^);
end;

(*
HOTPCalculateToken
------------------
```
function HOTPCalculateToken(const Secret: String; const Counter: Integer): Integer;
function TOTPCalculateToken(const Secret: String): Integer;
```

HOTP and TOTP One-time password algorithms. Compatible with Google Authenticator.
*)
procedure _LapeHOTPCalculateToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := HOTPCalculateToken(PString(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeTOTPCalculateToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := TOTPCalculateToken(PString(Params^[0])^);
end;

procedure _LapeResourceWriter_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Result)^^ := TSimbaResourceWriter.Create();
end;

procedure _LapeResourceWriter_Destroy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  LapeObjectDestroy(PLapeObject(Params^[0]));
end;

procedure _LapeResourceWriter_Add(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Params^[0])^^.Add(PString(Params^[1])^, PPByte(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeResourceWriter_AddString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Params^[0])^^.AddString(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeResourceWriter_AddImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Params^[0])^^.AddImage(PString(Params^[1])^, PLapeObjectImage(Params^[2])^^);
end;

procedure _LapeResourceWriter_AddImages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Params^[0])^^.AddImages(PString(Params^[1])^, PString(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeResourceWriter_AddFiles(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Params^[0])^^.AddFiles(PString(Params^[1])^, PString(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeResourceWriter_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceWriter(Params^[0])^^.Save(PString(Params^[1])^);
end;

procedure _LapeResourceReader_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceReader(Result)^^ := TSimbaResourceReader.Create(PString(Params^[0])^);
end;

procedure _LapeResourceReader_Destroy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  LapeObjectDestroy(PLapeObject(Params^[0]));
end;

procedure _LapeResourceReader_Names_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Names;
end;

procedure _LapeResourceReader_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Count;
end;

procedure _LapeResourceReader_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Name[PInteger(Params^[1])^];
end;

procedure _LapeResourceReader_Hash_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Hash[PInteger(Params^[1])^];
end;

procedure _LapeResourceReader_UncompressedSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PLapeObjectResourceReader(Params^[0])^^.UncompressedSize[PInteger(Params^[1])^];
end;

procedure _LapeResourceReader_CompressAlgo_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt8(Result)^ := PLapeObjectResourceReader(Params^[0])^^.CompressAlgo[PInteger(Params^[1])^];
end;

procedure _LapeResourceReader_CompressedSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PLapeObjectResourceReader(Params^[0])^^.CompressedSize[PInteger(Params^[1])^];
end;

procedure _LapeResourceReader_Find(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Find(PString(Params^[1])^);
end;

procedure _LapeResourceReader_Load1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Load(PInteger(Params^[1])^);
end;

procedure _LapeResourceReader_Load2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Load(PString(Params^[1])^);
end;

procedure _LapeResourceReader_LoadString1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectResourceReader(Params^[0])^^.LoadString(PInteger(Params^[1])^);
end;

procedure _LapeResourceReader_LoadString2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectResourceReader(Params^[0])^^.LoadString(PString(Params^[1])^);
end;

procedure _LapeResourceReader_LoadImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectResourceReader(Params^[0])^^.LoadImage(PInteger(Params^[1])^);
end;

procedure _LapeResourceReader_LoadImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectResourceReader(Params^[0])^^.LoadImage(PString(Params^[1])^);
end;

procedure _LapeResourceReader_Save1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Save(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeResourceReader_Save2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Save(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeResourceReader_Unload(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceReader(Params^[0])^^.Unload();
end;

procedure ImportEncoding(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Encoding';

    addGlobalType('enum(CRC32, MD4, MD5, SHA1, SHA256, SHA512)', 'EHashAlgo');
    addGlobalType('enum(b64URL, b64, b32, b32Hex, b16)', 'EBaseEncoding');

    addGlobalFunc('function HOTPCalculateToken(Secret: String; Counter: Integer): Integer', @_LapeHOTPCalculateToken);
    addGlobalFunc('function TOTPCalculateToken(Secret: String): Integer', @_LapeTOTPCalculateToken);

    addGlobalFunc('function BaseEncode(Encoding: EBaseEncoding; const S: String): String', @_LapeBaseEncode);
    addGlobalFunc('function BaseDecode(Encoding: EBaseEncoding; const S: String): String', @_LapeBaseDecode);

    addGlobalFunc('function HashData(Algo: EHashAlgo; Data: Pointer; Len: Int32): String', @_LapeHashData);
    addGlobalFunc('function HashString(Algo: EHashAlgo; S: String): String', @_LapeHashString);
    addGlobalFunc('function HashFile(Algo: EHashAlgo; FileName: String): String', @_LapeHashFile);

    addGlobalFunc('function Hash(Data: Pointer; Len: Int32; Seed: UInt32 = 0): UInt32; overload', @_LapeHash);
    addGlobalFunc('function Hash(S: String; Seed: UInt32 = 0): UInt32; overload', @_LapeHashStr);

    addGlobalType('enum(ZLIB, SYNLZ, GZ, BZIP2, LZ4, LZMA)', 'ECompressAlgo');

    addGlobalFunc('procedure CompressData(Algo: ECompressAlgo; InData: Pointer; InSize: Int64; var OutData: Pointer; out OutSize: Int64; Truncate: Boolean = True);', @_LapeCompressData);
    addGlobalFunc('function CompressBytes(Algo: ECompressAlgo; Bytes: TByteArray): TByteArray', @_LapeCompressBytes);
    addGlobalFunc('function CompressString(Algo: ECompressAlgo; Encoding: EBaseEncoding; Str: String): String', @_LapeCompressString);

    addGlobalFunc('procedure DecompressData(Algo: ECompressAlgo; InData: Pointer; InSize: Int64; var OutData: Pointer; out OutSize: Int64; Truncate: Boolean = True);', @_LapeDecompressData);
    addGlobalFunc('function DecompressBytes(Algo: ECompressAlgo; Bytes: TByteArray): TByteArray', @_LapeDecompressBytes);
    addGlobalFunc('function DecompressString(Algo: ECompressAlgo; Encoding: EBaseEncoding; Str: String): String', @_LapeDecompressString);

    DumpSection := '';

    LapeObjectImport(Script.Compiler, 'TResourceWriter');
    LapeObjectImport(Script.Compiler, 'TResourceReader');

    addGlobalFunc('function TResourceWriter.Construct: TResourceWriter; static;', @_LapeResourceWriter_Create);
    addGlobalFunc('procedure TResourceWriter.Destroy;', @_LapeResourceWriter_Destroy);
    addGlobalFunc('procedure TResourceWriter.Add(Name: String; Data: Pointer; DataSize: Integer);', @_LapeResourceWriter_Add);
    addGlobalFunc('procedure TResourceWriter.AddString(Name: String; Str: String);', @_LapeResourceWriter_AddString);
    addGlobalFunc('procedure TResourceWriter.AddImage(Name: String; Image: TImage);', @_LapeResourceWriter_AddImage);
    addGlobalFunc('procedure TResourceWriter.AddImages(Directory: String; Mask: String; Recursive: Boolean = False);', @_LapeResourceWriter_AddImages);
    addGlobalFunc('procedure TResourceWriter.AddFiles(Directory: String; Mask: String; Recursive: Boolean = False);', @_LapeResourceWriter_AddFiles);
    addGlobalFunc('procedure TResourceWriter.Save(FileName: String);', @_LapeResourceWriter_Save);

    addGlobalFunc('function TResourceReader.Construct(FileName: String): TResourceReader; static;', @_LapeResourceReader_Create);
    addGlobalFunc('procedure TResourceReader.Destroy;', @_LapeResourceReader_Destroy);
    addGlobalFunc('property TResourceReader.Names: TStringArray;', @_LapeResourceReader_Names_Read);
    addGlobalFunc('property TResourceReader.Count: Integer;', @_LapeResourceReader_Count_Read);
    addGlobalFunc('property TResourceReader.Name(Index: Integer): String;', @_LapeResourceReader_Name_Read);
    addGlobalFunc('property TResourceReader.Hash(Index: Integer): UInt32;', @_LapeResourceReader_Hash_Read);
    addGlobalFunc('property TResourceReader.CompressAlgo(Index: Integer): UInt8;', @_LapeResourceReader_CompressAlgo_Read);
    addGlobalFunc('property TResourceReader.CompressedSize(Index: Integer): UInt32;', @_LapeResourceReader_CompressedSize_Read);
    addGlobalFunc('property TResourceReader.UncompressedSize(Index: Integer): UInt32;', @_LapeResourceReader_UnCompressedSize_Read);
    addGlobalFunc('function TResourceReader.Find(Name: String): Integer;', @_LapeResourceReader_Find);
    addGlobalFunc('function TResourceReader.Load(Index: Integer): TByteArray; overload', @_LapeResourceReader_Load1);
    addGlobalFunc('function TResourceReader.Load(Name: String): TByteArray; overload', @_LapeResourceReader_Load2);
    addGlobalFunc('function TResourceReader.LoadString(Index: Integer): String; overload', @_LapeResourceReader_LoadString1);
    addGlobalFunc('function TResourceReader.LoadString(Name: String): String; overload', @_LapeResourceReader_LoadString2);
    addGlobalFunc('function TResourceReader.LoadImage(Index: Integer): TImage; overload', @_LapeResourceReader_LoadImage1);
    addGlobalFunc('function TResourceReader.LoadImage(Name: String): TImage; overload', @_LapeResourceReader_LoadImage2);
    addGlobalFunc('function TResourceReader.Save(Index: Integer; FileName: String): Boolean; overload;', @_LapeResourceReader_Save1);
    addGlobalFunc('function TResourceReader.Save(Name: String; FileName: String): Boolean; overload;', @_LapeResourceReader_Save2);
    addGlobalFunc('procedure TResourceReader.Unload;', @_LapeResourceReader_Unload);
  end;
end;

end.
