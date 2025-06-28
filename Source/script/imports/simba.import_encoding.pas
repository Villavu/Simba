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
  SynLZ,
  lptypes,
  simba.encoding,
  simba.hash,
  simba.compress,
  simba.resource;

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
FastCompress
------------
```
function FastCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
```
*)
procedure _LapeFastCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
FastDecompress
--------------
```
function FastDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
```
*)
procedure _LapeFastDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
FastCompressDestLen
-------------------
```
function FastCompressDestLen(Len: Integer): Integer;
```
*)
procedure _LapeFastCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

(*
FastDecompressDestLen
---------------------
```
function FastDecompressDestLen(Src: Pointer): Integer;
```
*)
procedure _LapeFastDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPointer(Params^[0])^);
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

procedure _LapeResourceReader_UnloadData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectResourceReader(Params^[0])^^.UnloadData();
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

procedure _LapeResourceReader_CompressedSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PLapeObjectResourceReader(Params^[0])^^.CompressedSize[PInteger(Params^[1])^];
end;

procedure _LapeResourceReader_Find(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Find(PString(Params^[1])^);
end;

procedure _LapeResourceReader_Load(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectResourceReader(Params^[0])^^.Load(PInteger(Params^[1])^, PPointer(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeResourceReader_LoadPartial(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectResourceReader(Params^[0])^^.LoadPartial(PInteger(Params^[1])^, PInteger(Params^[2])^, PPByte(Params^[3])^);
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

procedure ImportEncoding(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Encoding';

    addGlobalType('enum(CRC32, CRC64, MD4, MD5, SHA1, SHA256, SHA512)', 'EHashAlgo');
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

    addGlobalFunc('function FastCompressDestLen(Len: Integer): Integer', @_LapeFastCompressDestLen);
    addGlobalFunc('function FastDecompressDestLen(Src: Pointer): Integer', @_LapeFastDecompressDestLen);
    addGlobalFunc('function FastCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeFastCompress);
    addGlobalFunc('function FastDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeFastDecompress);

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
    addGlobalFunc('procedure TResourceReader.UnloadData;', @_LapeResourceReader_UnloadData);
    addGlobalFunc('property TResourceReader.Names: TStringArray;', @_LapeResourceReader_Names_Read);
    addGlobalFunc('property TResourceReader.Count: Integer;', @_LapeResourceReader_Count_Read);
    addGlobalFunc('property TResourceReader.Name(Index: Integer): String;', @_LapeResourceReader_Name_Read);
    addGlobalFunc('property TResourceReader.Hash(Index: Integer): UInt32;', @_LapeResourceReader_Hash_Read);
    addGlobalFunc('property TResourceReader.CompressedSize(Index: Integer): UInt32;', @_LapeResourceReader_CompressedSize_Read);
    addGlobalFunc('property TResourceReader.UncompressedSize(Index: Integer): UInt32;', @_LapeResourceReader_UnCompressedSize_Read);
    addGlobalFunc('function TResourceReader.Find(Name: String): Integer;', @_LapeResourceReader_Find);
    addGlobalFunc('function TResourceReader.Load(Index: Integer; out Data: Pointer; out DataSize: Integer): Boolean;', @_LapeResourceReader_Load);
    addGlobalFunc('function TResourceReader.LoadPartial(Index: Integer; Size: Integer; out Data: Pointer): Boolean;', @_LapeResourceReader_LoadPartial);
    addGlobalFunc('function TResourceReader.LoadString(Index: Integer): String; overload', @_LapeResourceReader_LoadString1);
    addGlobalFunc('function TResourceReader.LoadString(Name: String): String; overload', @_LapeResourceReader_LoadString2);
    addGlobalFunc('function TResourceReader.LoadImage(Index: Integer): TImage; overload', @_LapeResourceReader_LoadImage1);
    addGlobalFunc('function TResourceReader.LoadImage(Name: String): TImage; overload', @_LapeResourceReader_LoadImage2);
    addGlobalFunc('function TResourceReader.Save(Index: Integer; FileName: String): Boolean; overload;', @_LapeResourceReader_Save1);
    addGlobalFunc('function TResourceReader.Save(Name: String; FileName: String): Boolean; overload;', @_LapeResourceReader_Save2);

    DumpSection := '';
  end;
end;

end.
