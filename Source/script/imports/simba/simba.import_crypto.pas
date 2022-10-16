unit simba.import_crypto;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, ffi,
  synlz, blowfish, md5, sha1, hmac,
  simba.script_compiler, simba.stringutil, simba.compressionthread;

procedure _LapeBlowFish_Encrypt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
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

procedure _LapeBlowFish_Decrypt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
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

procedure _LapeMD5File(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if FileExists(PString(Params^[0])^) then
    PString(Result)^ := MD5Print(MD5File(PString(Params^[0])^));
end;

procedure _LapeSHA1String(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SHA1Print(SHA1String(PString(Params^[0])^));
end;

procedure _LapeSHA1File(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if FileExists(PString(Params^[0])^) then
    PString(Result)^ := SHA1Print(SHA1File(PString(Params^[0])^));
end;

procedure _LapeHMACMD5(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACMD5(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeHMACSHA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := HMACSHA1(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := CompressString(PString(Params^[0])^);
end;

procedure _LapeDecompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := DecompressString(PString(Params^[0])^);
end;

procedure _LapeBase64Encode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := Base64Encode(PString(Params^[0])^);
end;

procedure _LapeBase64Decode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := Base64Decode(PString(Params^[0])^);
end;

procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPAnsiChar(Params^[0])^);
end;

procedure _LapeLZCompressionThread_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Result)^ := TLZCompressionThread.Create();
end;

procedure _LapeLZCompressionThread_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.Free();
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

procedure ImportCrypto(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Crypto';

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

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportCrypto);

end.

