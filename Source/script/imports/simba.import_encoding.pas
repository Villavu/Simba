unit simba.import_encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, blowfish, hmac, ffi, SynLZ,
  simba.encoding, simba.compress, simba.hash;

(*
Encoding
========
A bit of Hashing, Encoding, Compressing.
*)

(*
BlowFishEncrypt
---------------
> function BlowFishEncrypt(const Data, Password: String): String;
*)
procedure _LapeBlowFishEncrypt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
var
  Data: String;
  Output: TStringStream;
  Encrypt: TBlowFishEncryptStream;
begin
  Data := PString(Params^[0])^;
  Output := TStringStream.Create('');

  try
    Encrypt := TBlowFishEncryptStream.Create(PString(Params^[1])^, Output);

    try
      Encrypt.Write(Data[1], Length(Data));
    finally
      Encrypt.Free();
    end;
  finally
    PString(Result)^ := Output.DataString;

    Output.Free();
  end;
end;

(*
BlowFishDecrypt
---------------
> function BlowFishDecrypt(const Data, Password: String): String;
*)
procedure _LapeBlowFishDecrypt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
var
  Input: TStringStream;
  Decrypt: TBlowFishDeCryptStream;
begin
  Input := TStringStream.Create(PString(Params^[0])^);

  try
    Decrypt := TBlowFishDeCryptStream.Create(PString(Params^[1])^, Input);

    try
      SetLength(PString(Result)^, Input.Size);

      Decrypt.Read(PString(Result)^[1], Input.Size);
    finally
      Decrypt.Free();
    end;
  finally
    Input.Free();
  end;
end;

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
HMACMD5
-------
> function HMACMD5(const Key, Message: String): String;
*)
procedure _LapeHMACMD5(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACMD5(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
HMACSHA1
--------
> function HMACSHA1(const Key, Message: String): String;
*)
procedure _LapeHMACSHA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACSHA1(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
Base64Encode
------------
> function Base64Encode(const S: String): String;
*)
procedure _LapeBase64Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base64Encode(PString(Params^[0])^);
end;

(*
Base64Decode
------------
> function Base64Decode(const S: String): String;
*)
procedure _LapeBase64Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base64Decode(PString(Params^[0])^);
end;

(*
Base32Encode
------------
> function Base32Encode(const S: String): String;
*)
procedure _LapeBase32Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base32Encode(PString(Params^[0])^);
end;

(*
Base32Decode
------------
> function Base32Decode(const S: String): String;
*)
procedure _LapeBase32Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base32Decode(PString(Params^[0])^);
end;

(*
HexEncode
---------
> function HexEncode(const S: String): String;
*)
procedure _LapeHexEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HexEncode(PString(Params^[0])^);
end;

(*
HexDecode
---------
> function HexDecode(const S: String): String;
*)
procedure _LapeHexDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HexDecode(PString(Params^[0])^);
end;

(*
GetOTPToken
-----------
> function GetOTPToken(const Secret: String): Integer;
*)
procedure _LapeGetOTPToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := GetOTPToken(PString(Params^[0])^);
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
  PInteger(Result)^ := SynLZcompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

(*
SynLZDecompress
---------------
> function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
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
  PInteger(Result)^ := SynLZdecompressdestlen(PPAnsiChar(Params^[0])^);
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

    addGlobalType('enum(SHA1, SHA256, SHA384, SHA512, MD5, CRC32)', 'EHashType');

    addGlobalFunc('function GetOTPToken(const Secret: String): Integer', @_LapeGetOTPToken);

    addGlobalFunc('function Base64Encode(const S: String): String', @_LapeBase64Encode);
    addGlobalFunc('function Base64Decode(const S: String): String', @_LapeBase64Decode);
    addGlobalFunc('function Base32Encode(const S: String): String', @_LapeBase32Encode);
    addGlobalFunc('function Base32Decode(const S: String): String', @_LapeBase32Decode);

    addGlobalFunc('function HexEncode(const S: String): String', @_LapeHexEncode);
    addGlobalFunc('function HexDecode(const S: String): String', @_LapeHexDecode);

    addGlobalFunc('function BlowFishEncrypt(const Data, Password: String): String', @_LapeBlowFishEncrypt);
    addGlobalFunc('function BlowFishDecrypt(const Data, Password: String): String', @_LapeBlowFishDecrypt);

    addGlobalFunc('function HashBuffer(HashType: EHashType; Buf: Pointer; Len: SizeUInt): String', @_LapeHashBuffer);
    addGlobalFunc('function HashString(HashType: EHashType; S: String): String', @_LapeHashString);
    addGlobalFunc('function HashFile(HashType: EHashType; FileName: String): String', @_LapeHashFile);

    addGlobalFunc('function HMACMD5(const Key, Message: String): String', @_LapeHMACMD5);
    addGlobalFunc('function HMACSHA1(const Key, Message: String): String', @_LapeHMACSHA1);

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

