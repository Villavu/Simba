
procedure _Encrypt(CipherType: TDCP_cipherclass; HashType: TDCP_hashclass; const Key: string; var Data: string); inline;
begin
  with CipherType.Create(nil) do
  try
    InitStr(Key, HashType);
    Data := EncryptString(Data);
    Burn;
  finally
    Free;
  end;
end;

procedure _Decrypt(CipherType: TDCP_cipherclass; HashType: TDCP_hashclass; const Key: string; var Data: string); inline;
begin
  with CipherType.Create(nil) do
  try
    InitStr(Key, HashType);
    Data := DecryptString(Data);
    Burn;
  finally
    Free;
  end;
end;

function _Hash(HashType: TDCP_hashclass; Data: string): string; inline;
var
  Digest: Pointer;
  I, Size: LongInt;
begin
  with HashType.Create(nil) do
  try
    Init();
    UpdateStr(Data);

    Size := (GetHashSize() div 8);
    Digest := GetMem(Size);
    FillChar(Digest^, Size, $00);
    Final(Digest^);
  finally
    Free;
  end;

  Dec(Size);
  Result := '';
  for I := 0 to Size do
    Result := Result + IntToHex(PByte(Digest)[I], 2);

  FreeMem(Digest, Size + 1);
end;

function _HashType2Class(const HashType: THashType): TDCP_hashclass; inline;
begin
  case (HashType) of
    htHaval: Result := TDCP_Haval;
    htMD4: Result := TDCP_MD4;
    htMD5: Result := TDCP_MD5;
    htRIPEMD128: Result := TDCP_RIPEMD128;
    htRIPEMD160: Result := TDCP_RIPEMD160;
    htSHA1: Result := TDCP_SHA1;
    htSHA256: Result := TDCP_SHA256;
    htSHA384: Result := TDCP_SHA384;
    htSHA512: Result := TDCP_SHA512;
    htTiger: Result := TDCP_Tiger;
  else
    Result := TDCP_SHA1;
  end;
end;

function ps_hash(const HashType: THashType; const Data: string): string;
begin
  Result := _Hash(_HashType2Class(HashType), Data);
end;

function ps_haval(const Data: string): string; begin Result := _Hash(TDCP_Haval, Data); end;
function ps_md4(const Data: string): string; begin Result := _Hash(TDCP_MD4, Data); end;
function ps_md5(const Data: string): string; begin Result := _Hash(TDCP_MD5, Data); end;
function ps_ripemd128(const Data: string): string; begin Result := _Hash(TDCP_RipeMD128, Data); end;
function ps_ripemd160(const Data: string): string; begin Result := _Hash(TDCP_RipeMD160, Data); end;
function ps_sha1(const Data: string): string; begin Result := _Hash(TDCP_SHA1, Data); end;
function ps_sha256(const Data: string): string; begin Result := _Hash(TDCP_SHA256, Data); end;
function ps_sha384(const Data: string): string; begin Result := _Hash(TDCP_SHA384, Data); end;
function ps_sha512(const Data: string): string; begin Result := _Hash(TDCP_SHA512, Data); end;
function ps_tiger(const Data: string): string; begin Result := _Hash(TDCP_Tiger, Data); end;

procedure ps_rc2_encrypt(const Key: string; const HashType: THashType; var Data: string); begin _Encrypt(TDCP_RC2, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc2_decrypt(const Key: string; const HashType: THashType; var Data: string); begin _Decrypt(TDCP_RC2, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc4_encrypt(const Key: string; const HashType: THashType; var Data: string); begin _Encrypt(TDCP_RC4, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc4_decrypt(const Key: string; const HashType: THashType; var Data: string); begin _Decrypt(TDCP_RC4, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc5_encrypt(const Key: string; const HashType: THashType; var Data: string); begin _Encrypt(TDCP_RC5, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc5_decrypt(const Key: string; const HashType: THashType; var Data: string); begin _Decrypt(TDCP_RC5, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc6_encrypt(const Key: string; const HashType: THashType; var Data: string); begin _Encrypt(TDCP_RC6, _HashType2Class(HashType), Key, Data); end;
procedure ps_rc6_decrypt(const Key: string; const HashType: THashType; var Data: string); begin _Decrypt(TDCP_RC6, _HashType2Class(HashType), Key, Data); end;
