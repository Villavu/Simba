
function _Hash(HashType: TDCP_hashclass; Data: string): string; inline;
var
  Digest: Pointer;
  I, Size: LongInt;
begin
  with HashType.Create(nil) do
  try
    Size := (GetHashSize() div 8);
    Digest := GetMem(Size);
    FillChar(Digest^, Size, $00);

    Init();
    UpdateStr(Data);
    Final(Digest^);
  finally
    Free;
  end;

  Dec(Size);
  Result := '';
  for I := 0 to Size do
    Result := Result + IntToHex(PByte(Digest)[I], 2);
end;

function ps_haval(Data: string): string; begin Result := _Hash(TDCP_Haval, Data); end;
function ps_md4(Data: string): string; begin Result := _Hash(TDCP_MD4, Data); end;
function ps_md5(Data: string): string; begin Result := _Hash(TDCP_MD5, Data); end;
function ps_ripemd128(Data: string): string; begin Result := _Hash(TDCP_RipeMD128, Data); end;
function ps_ripemd160(Data: string): string; begin Result := _Hash(TDCP_RipeMD160, Data); end;
function ps_sha1(Data: string): string; begin Result := _Hash(TDCP_SHA1, Data); end;
function ps_sha256(Data: string): string; begin Result := _Hash(TDCP_SHA256, Data); end;
function ps_sha384(Data: string): string; begin Result := _Hash(TDCP_SHA384, Data); end;
function ps_sha512(Data: string): string; begin Result := _Hash(TDCP_SHA512, Data); end;
function ps_tiger(Data: string): string; begin Result := _Hash(TDCP_Tiger, Data); end;
