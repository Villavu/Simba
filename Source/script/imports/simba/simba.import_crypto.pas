unit simba.import_crypto;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  SynLZ, blowfish, md5, sha1, HMAC,
  simba.script_compiler, simba.stringutil;

procedure _LapeBlowFish_Encrypt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

procedure _LapeBlowFish_Decrypt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Input: TStringStream;
  Decrypt: TBlowFishDeCryptStream;
begin
  Input := TStringStream.Create(PString(Params^[0])^);

  try
    Decrypt := TBlowFishDeCryptStream.Create(PString(Params^[1])^, Input);

    try
      SetLength(PString(Result)^, Input.Size);

      Decrypt.Read(PString(Result)^[2], Input.Size);
    finally
      Decrypt.Free();
    end;
  finally
    Input.Free();
  end;
end;

procedure _LapeMD5String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := MD5Print(MD5String(PString(Params^[0])^));
end;

procedure _LapeMD5File(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if FileExists(PString(Params^[0])^) then
    PString(Result)^ := MD5Print(MD5File(PString(Params^[0])^));
end;

procedure _LapeSHA1String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SHA1Print(SHA1String(PString(Params^[0])^));
end;

procedure _LapeSHA1File(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if FileExists(PString(Params^[0])^) then
    PString(Result)^ := SHA1Print(SHA1File(PString(Params^[0])^));
end;

procedure _LapeHMACMD5(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := HMACMD5(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeHMACSHA1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := HMACSHA1(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := CompressString(Pstring(Params^[0])^);
end;

procedure _LapeDecompressString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := DecompressString(Pstring(Params^[0])^);
end;

procedure _LapeBase64Encode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Base64Encode(Pstring(Params^[0])^);
end;

procedure _LapeBase64Decode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pstring(Result)^ := Base64Decode(Pstring(Params^[0])^);
end;

procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SynLZcompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SynLZdecompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPAnsiChar(Params^[0])^);
end;

procedure ImportCrypto(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Crypto');

    addGlobalFunc('function BlowFish_Encrypt(const Data, Password: String): String', @_LapeBlowFish_Encrypt);
    addGlobalFunc('function BlowFish_Decrypt(const Data, Password: String): String', @_LapeBlowFish_Decrypt);
    addGlobalFunc('function MD5String(const Data: String): String', @_LapeMD5String);
    addGlobalFunc('function MD5File(const FileName: String): String', @_LapeMD5File);
    addGlobalFunc('function SHA1String(const Data: String): String', @_LapeSHA1String);
    addGlobalFunc('function SHA1File(const FileName: String): String', @_LapeSHA1File);
    addGlobalFunc('function HMACMD5(const Key, Message: String): String', @_LapeHMACMD5);
    addGlobalFunc('function HMACSHA1(const Key, Message: String): String', @_LapeHMACSHA1);

    addGlobalFunc('function CompressString(S: String): String', @_LapeCompressString);
    addGlobalFunc('function DecompressString(S: String): String', @_LapeDecompressString);
    addGlobalFunc('function Base64Encode(S: String): String', @_LapeBase64Encode);
    addGlobalFunc('function Base64Decode(S: String): String', @_LapeBase64Decode);

    addGlobalFunc('function SynLZCompressDestLen(in_len: Integer): Integer', @_LapeSynLZCompressDestLen);
    addGlobalFunc('function SynLZDecompressDestLen(in_p: Pointer): Integer', @_LapeSynLZDecompressDestLen);
    addGlobalFunc('function SynLZCompress(src: Pointer; size: integer; dst: Pointer): Integer', @_LapeSynLZCompress);
    addGlobalFunc('function SynLZDecompress(src: Pointer; size: integer; dst: Pointer): Integer', @_LapeSynLZDecompress);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportCrypto);

end.

