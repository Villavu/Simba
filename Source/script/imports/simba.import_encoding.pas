unit simba.import_encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi, SynLZ,
  simba.encoding, simba.compress, simba.hash;

(*
Encoding
========
A bit of Hashing, Encoding, Compressing.
*)

(*
HashBuffer
----------
> function HashBuffer(HashType: EHashType; Buf: PByte; Len: SizeUInt): String;
*)
procedure _LapeHashBuffer(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashBuffer(PHashType(Params^[0])^, PPointer(Params^[1])^, PSizeUInt(Params^[2])^);
end;

(*
HashString
----------
> function HashString(const Data: String): String;
*)
procedure _LapeHashString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashString(PHashType(Params^[0])^, PString(Params^[1])^);
end;

(*
HashFile
--------
> function HashFile(HashType: EHashType; FileName: String): String;
*)
procedure _LapeHashFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HashFile(PHashType(Params^[0])^, PString(Params^[1])^);
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

(*
ZCompressString
---------------
> function ZCompressString(S: String): String;
*)
procedure _LapeZCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := ZCompressString(PString(Params^[0])^);
end;

(*
ZDecompressString
-----------------
> function ZDecompressString(S: String): String;
*)
procedure _LapeZDecompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := ZDecompressString(PString(Params^[0])^);
end;

(*
SynLZCompress
-------------
> function SynLZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
SynLZDecompress
---------------
> function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
SynLZCompressDestLen
--------------------
> function SynLZCompressDestLen(Len: Integer): Integer;
*)
procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

(*
SynLZDecompressDestLen
----------------------
> function SynLZDecompressDestLen(Src: Pointer): Integer;
*)
procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPointer(Params^[0])^);
end;

procedure _LapeLZCompressionThread_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Result)^ := TLZCompressionThread.Create();
end;

procedure _LapeLZCompressionThread_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.Write(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeLZCompressionThread_WaitCompressing(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.WaitCompressing();
end;

procedure _LapeLZCompressionThread_IsCompressing(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLZCompressionThread(Params^[0])^.IsCompressing;
end;

procedure _LapeLZCompressionThreadOnCompressed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressedEvent(Result)^ := PLZCompressionThread(Params^[0])^.OnCompressed;
end;

procedure _LapeLZCompressionThreadOnCompressed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.OnCompressed := PLZCompressedEvent(Params^[1])^;
end;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Encoding';

    addGlobalType('enum(SHA1, SHA256, SHA384, SHA512, MD5, CRC32, CRC64)', 'EHashType');
    addGlobalType('enum(b64URL, b64, b32, b32Hex, b16)', 'BaseEncoding');

    addGlobalFunc('function HOTPCalculateToken(const Secret: String; const Counter: Integer): Integer', @_LapeHOTPCalculateToken);
    addGlobalFunc('function TOTPCalculateToken(const Secret: String): Integer', @_LapeTOTPCalculateToken);

    addGlobalFunc('function BaseEncode(Encoding: BaseEncoding; const S: String): String', @_LapeBaseEncode);
    addGlobalFunc('function BaseDecode(Encoding: BaseEncoding; const S: String): String', @_LapeBaseDecode);

    addGlobalFunc('function HashBuffer(HashType: EHashType; Buf: Pointer; Len: SizeUInt): String', @_LapeHashBuffer);
    addGlobalFunc('function HashString(HashType: EHashType; S: String): String', @_LapeHashString);
    addGlobalFunc('function HashFile(HashType: EHashType; FileName: String): String', @_LapeHashFile);

    addGlobalFunc('function ZCompressString(S: String): String', @_LapeZCompressString);
    addGlobalFunc('function ZDecompressString(S: String): String', @_LapeZDecompressString);

    addGlobalFunc('function SynLZCompressDestLen(Len: Integer): Integer', @_LapeSynLZCompressDestLen);
    addGlobalFunc('function SynLZDecompressDestLen(Src: Pointer): Integer', @_LapeSynLZDecompressDestLen);
    addGlobalFunc('function SynLZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZCompress);
    addGlobalFunc('function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZDecompress);

    ImportingSection := '';

    addClass('TSynLZThread');
    addClassConstructor('TSynLZThread', '', @_LapeLZCompressionThread_Create);
    addGlobalType('procedure(Sender: TSynLZThread; Data: Pointer; DataSize: Integer; TimeUsed: Double) of object', 'TSynLZCompressedEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('procedure TSynLZThread.Write(constref Data; DataSize: Integer)', @_LapeLZCompressionThread_Write);
    addGlobalFunc('procedure TSynLZThread.WaitCompressing', @_LapeLZCompressionThread_WaitCompressing);
    addGlobalFunc('procedure TSynLZThread.SetOnCompressed(Value: TSynLZCompressedEvent)', @_LapeLZCompressionThreadOnCompressed_Write);
    addGlobalFunc('function TSynLZThread.GetOnCompressed: TSynLZCompressedEvent', @_LapeLZCompressionThreadOnCompressed_Read);
    addGlobalFunc('function TSynLZThread.IsCompressing: Boolean', @_LapeLZCompressionThread_IsCompressing);
  end;
end;

end.

