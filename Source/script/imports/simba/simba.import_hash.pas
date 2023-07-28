unit simba.import_hash;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportHash(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, blowfish, md5, sha1, hmac,
  simba.encoding;

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

procedure _LapeMD5String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := MD5Print(MD5String(PString(Params^[0])^));
end;

procedure _LapeSHA1String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA1Print(SHA1String(PString(Params^[0])^));
end;

procedure _LapeSHA256String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA256String(PString(Params^[0])^);
end;

procedure _LapeSHA512String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA512String(PString(Params^[0])^);
end;

procedure _LapeHMACMD5(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACMD5(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeHMACSHA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACSHA1(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeBase64Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := Base64Encode(PString(Params^[0])^);
end;

procedure _LapeBase64Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := Base64Decode(PString(Params^[0])^);
end;

procedure _LapeBase32Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base32Encode(PString(Params^[0])^);
end;

procedure _LapeBase32Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := Base32Decode(PString(Params^[0])^);
end;

procedure _LapeHexEncode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HexEncode(PString(Params^[0])^);
end;

procedure _LapeHexDecode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HexDecode(PString(Params^[0])^);
end;

procedure _LapeGetOTPToken(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := GetOTPToken(PString(Params^[0])^);
end;

procedure ImportHash(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Hash';

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

    ImportingSection := '';
  end;
end;

end.

