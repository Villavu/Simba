unit simba.script_import_crypto;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Crypto(Compiler: TSimbaScript_Compiler);

implementation

uses
  blowfish, md5, sha1, hmac;

procedure Lape_BlowFish_Encrypt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

procedure Lape_BlowFish_Decrypt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

procedure Lape_MD5String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := MD5Print(MD5String(PString(Params^[0])^));
end;

procedure Lape_MD5File(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if FileExists(PString(Params^[0])^) then
    PString(Result)^ := MD5Print(MD5File(PString(Params^[0])^));
end;

procedure Lape_SHA1String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SHA1Print(SHA1String(PString(Params^[0])^));
end;

procedure Lape_SHA1File(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if FileExists(PString(Params^[0])^) then
    PString(Result)^ := SHA1Print(SHA1File(PString(Params^[0])^));
end;

procedure Lape_HMACMD5(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := HMACMD5(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_HMACSHA1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := HMACSHA1(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure Lape_Import_Crypto(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'Crypto';

    addGlobalFunc('function BlowFish_Encrypt(constref Data, Password: String): String;', @Lape_BlowFish_Encrypt);
    addGlobalFunc('function BlowFish_Decrypt(constref Data, Password: String): String;', @Lape_BlowFish_Decrypt);
    addGlobalFunc('function MD5String(constref Data: String): String;', @Lape_MD5String);
    addGlobalFunc('function MD5File(constref FilePath: String): String;', @Lape_MD5File);
    addGlobalFunc('function SHA1String(constref Data: String): String;', @Lape_SHA1String);
    addGlobalFunc('function SHA1File(constref FilePath: String): String;', @Lape_SHA1File);
    addGlobalFunc('function HMACMD5(const Key, Message: string): String;', @Lape_HMACMD5);
    addGlobalFunc('function HMACSHA1(const Key, Message: string): String;', @Lape_HMACSHA1);
  end;
end;

end.

