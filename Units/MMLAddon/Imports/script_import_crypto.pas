unit script_import_crypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, lpcompiler, lptypes, BlowFish, md5, sha1;

procedure Lape_BlowFish_Encrypt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Data: String;
  Output: TStringStream;
  Encrypt: TBlowFishEncryptStream;
begin
  Data := PString(Params^[1])^;
  Output := TStringStream.Create('');

  try
    Encrypt := TBlowFishEncryptStream.Create(PString(Params^[2])^, Output);

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
  Input := TStringStream.Create(PString(Params^[1])^);

  try
    Decrypt := TBlowFishDeCryptStream.Create(PString(Params^[2])^, Input);

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

procedure Lape_MD5String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := MD5Print(MD5String(PString(Params^[1])^));
end;

procedure Lape_MD5File(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if FileExists(PString(Params^[1])^) then
    PString(Result)^ := MD5Print(MD5File(PString(Params^[1])^));
end;

procedure Lape_SHA1String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SHA1Print(SHA1String(PString(Params^[1])^));
end;

procedure Lape_SHA1File(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if FileExists(PString(Params^[1])^) then
    PString(Result)^ := SHA1Print(SHA1File(PString(Params^[1])^));
end;

procedure Lape_Import_Crypto(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function BlowFish_Encrypt(constref Data, Password: String): String;', @Lape_BlowFish_Encrypt, Data);
    addGlobalMethod('function BlowFish_Decrypt(constref Data, Password: String): String;', @Lape_BlowFish_Decrypt, Data);
    addGlobalMethod('function MD5String(constref Data: String): String;', @Lape_MD5String, Data);
    addGlobalMethod('function MD5File(constref FilePath: String): String;', @Lape_MD5File, Data);
    addGlobalMethod('function SHA1String(constref Data: String): String;', @Lape_SHA1String, Data);
    addGlobalMethod('function SHA1File(constref FilePath: String): String;', @Lape_SHA1File, Data);
  end;
end;

initialization
  ScriptImports.Add('Crypto', @Lape_Import_Crypto);

end.

