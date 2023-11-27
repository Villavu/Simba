unit simba.import_encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, blowfish, md5, sha1, hmac, ffi,
  simba.encoding, simba.compress, SynLZ;

(*
Encoding
========
A bit of Hashing, Encoding, Compressing.
*)

(*
BlowFishEncrypt
~~~~~~~~~~~~~~~
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
~~~~~~~~~~~~~~~
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
MD5String
~~~~~~~~~
> function MD5String(const Data: String): String;
*)
procedure _LapeMD5String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := MD5Print(MD5String(PString(Params^[0])^));
end;

(*
SHA1String
~~~~~~~~~~
> function SHA1String(const Data: String): String;
*)
procedure _LapeSHA1String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA1Print(SHA1String(PString(Params^[0])^));
end;

(*
SHA256String
~~~~~~~~~~~~
> function SHA256String(const Data: String): String;
*)
procedure _LapeSHA256String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA256String(PString(Params^[0])^);
end;

(*
SHA512String
~~~~~~~~~~~~
> function SHA512String(const Data: String): String;
*)
procedure _LapeSHA512String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA512String(PString(Params^[0])^);
end;

(*
HMACMD5
~~~~~~~
> function HMACMD5(const Key, Message: String): String;
*)
procedure _LapeHMACMD5(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACMD5(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
HMACSHA1
~~~~~~~~
> function HMACSHA1(const Key, Message: String): String;
*)
procedure _LapeHMACSHA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACSHA1(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
Base64Encode
~~~~~~~~~~~~
> function Base64Encode(const S: String): String;
*)
procedure _LapeBase64Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := Base64Encode(PString(Params^[0])^);
end;

(*
Base64Decode
~~~~~~~~~~~~
> function Base64Decode(const S: String): String;
*)
procedure _LapeBase64Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := Base64Decode(PString(Params^[0])^);
end;

(*
Base32Encode
~~~~~~~~~~~~
> function Base32Encode(const S: String): String;
*)
procedure _LapeBase32Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base32Encode(PString(Params^[0])^);
end;

(*
Base32Decode
~~~~~~~~~~~~
> function Base32Decode(const S: String): String;
*)
procedure _LapeBase32Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base32Decode(PString(Params^[0])^);
end;

(*
HexEncode
~~~~~~~~~
> function HexEncode(const S: String): String;
*)
procedure _LapeHexEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HexEncode(PString(Params^[0])^);
end;

(*
HexDecode
~~~~~~~~~
> function HexDecode(const S: String): String;
*)
procedure _LapeHexDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HexDecode(PString(Params^[0])^);
end;

(*
GetOTPToken
~~~~~~~~~~~
> function GetOTPToken(const Secret: String): Integer;
*)
procedure _LapeGetOTPToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := GetOTPToken(PString(Params^[0])^);
end;

(*
CompressString
~~~~~~~~~~~~~~
> function CompressString(S: String): String;
*)
procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := CompressString(PString(Params^[0])^);
end;

(*
DecompressString
~~~~~~~~~~~~~~~~
> function DecompressString(S: String): String;
*)
procedure _LapeDecompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := DecompressString(PString(Params^[0])^);
end;

(*
LZCompress
~~~~~~~~~~
> function LZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

(*
LZDecompress
~~~~~~~~~~~~
> function LZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

(*
LZCompressDestLen
~~~~~~~~~~~~~~~~~
> function LZCompressDestLen(Len: Integer): Integer;
*)
procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

(*
LZDecompressDestLen
~~~~~~~~~~~~~~~~~~~
> function LZDecompressDestLen(Src: Pointer): Integer;
*)
procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPAnsiChar(Params^[0])^);
end;

(*
TLZCompressionThread.Create
~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TLZCompressionThread.Create: TLZCompressionThread; static;
*)
procedure _LapeLZCompressionThread_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Result)^ := TLZCompressionThread.Create();
end;

(*
TLZCompressionThread.Free
~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TLZCompressionThread.Free;
*)
procedure _LapeLZCompressionThread_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.Free();
end;

(*
TLZCompressionThread.Write
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TLZCompressionThread.Write(constref Data; DataSize: Integer);
*)
procedure _LapeLZCompressionThread_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.Write(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TLZCompressionThread.WaitCompressing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TLZCompressionThread.WaitCompressing;
*)
procedure _LapeLZCompressionThread_WaitCompressing(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.WaitCompressing();
end;

(*
TLZCompressionThread.IsCompressing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TLZCompressionThread.IsCompressing: Boolean;
*)
procedure _LapeLZCompressionThread_IsCompressing(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLZCompressionThread(Params^[0])^.IsCompressing;
end;

(*
TLZCompressionThread.GetOnCompressed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TLZCompressionThread.GetOnCompressed: TLZCompressedEvent;
*)
procedure _LapeLZCompressionThreadOnCompressed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressedEvent(Result)^ := PLZCompressionThread(Params^[0])^.OnCompressed;
end;

(*
TLZCompressionThread.SetOnCompressed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TLZCompressionThread.SetOnCompressed(Value: TLZCompressedEvent);
*)
procedure _LapeLZCompressionThreadOnCompressed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.OnCompressed := PLZCompressedEvent(Params^[1])^;
end;

procedure ImportEncoding(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Encoding';

    addGlobalFunc('function GetOTPToken(const Secret: String): Integer', @_LapeGetOTPToken);

    addGlobalFunc('function Base64Encode(const S: String): String', @_LapeBase64Encode);
    addGlobalFunc('function Base64Decode(const S: String): String', @_LapeBase64Decode);
    addGlobalFunc('function Base32Encode(const S: String): String', @_LapeBase32Encode);
    addGlobalFunc('function Base32Decode(const S: String): String', @_LapeBase32Decode);

    addGlobalFunc('function HexEncode(const S: String): String', @_LapeHexEncode);
    addGlobalFunc('function HexDecode(const S: String): String', @_LapeHexDecode);

    addGlobalFunc('function BlowFishEncrypt(const Data, Password: String): String', @_LapeBlowFishEncrypt);
    addGlobalFunc('function BlowFishDecrypt(const Data, Password: String): String', @_LapeBlowFishDecrypt);
    addGlobalFunc('function MD5String(const Data: String): String', @_LapeMD5String);
    addGlobalFunc('function SHA1String(const Data: String): String', @_LapeSHA1String);
    addGlobalFunc('function SHA256String(const Data: String): String', @_LapeSHA256String);
    addGlobalFunc('function SHA512String(const Data: String): String', @_LapeSHA512String);
    addGlobalFunc('function HMACMD5(const Key, Message: String): String', @_LapeHMACMD5);
    addGlobalFunc('function HMACSHA1(const Key, Message: String): String', @_LapeHMACSHA1);

    addGlobalFunc('function CompressString(S: String): String', @_LapeCompressString);
    addGlobalFunc('function DecompressString(S: String): String', @_LapeDecompressString);

    addGlobalFunc('function LZCompressDestLen(Len: Integer): Integer', @_LapeSynLZCompressDestLen);
    addGlobalFunc('function LZDecompressDestLen(Src: Pointer): Integer', @_LapeSynLZDecompressDestLen);
    addGlobalFunc('function LZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZCompress);
    addGlobalFunc('function LZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZDecompress);

    addGlobalType('type Pointer', 'TLZCompressionThread');
    addGlobalType('procedure(Sender: TLZCompressionThread; Data: Pointer; DataSize: Integer; TimeUsed: Double) of object', 'TLZCompressedEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('function TLZCompressionThread.Create: TLZCompressionThread; static', @_LapeLZCompressionThread_Create);
    addGlobalFunc('procedure TLZCompressionThread.Free;', @_LapeLZCompressionThread_Free);
    addGlobalFunc('procedure TLZCompressionThread.Write(constref Data; DataSize: Integer)', @_LapeLZCompressionThread_Write);
    addGlobalFunc('procedure TLZCompressionThread.WaitCompressing', @_LapeLZCompressionThread_WaitCompressing);
    addGlobalFunc('procedure TLZCompressionThread.SetOnCompressed(Value: TLZCompressedEvent)', @_LapeLZCompressionThreadOnCompressed_Write);
    addGlobalFunc('function TLZCompressionThread.GetOnCompressed: TLZCompressedEvent', @_LapeLZCompressionThreadOnCompressed_Read);
    addGlobalFunc('function TLZCompressionThread.IsCompressing: Boolean', @_LapeLZCompressionThread_IsCompressing);

    ImportingSection := '';
  end;
end;

end.

