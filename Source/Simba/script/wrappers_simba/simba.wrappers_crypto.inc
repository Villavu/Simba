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

