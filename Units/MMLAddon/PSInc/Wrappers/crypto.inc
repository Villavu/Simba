
function ps_haval(Data: string): string;
var
  HASH: TDCP_haval;
  Digest: array[0..31] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_haval.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 31 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_md4(Data: string): string;
var
  HASH: TDCP_md4;
  Digest: array[0..15] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_md4.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 15 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end; 

function ps_md5(Data: string): string;
var
  HASH: TDCP_md5;
  Digest: array[0..15] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_md5.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 15 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_ripemd128(Data: string): string;
var
  HASH: TDCP_ripemd128;
  Digest: array[0..15] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_ripemd128.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 15 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_ripemd160(Data: string): string;
var
  HASH: TDCP_ripemd160;
  Digest: array[0..19] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_ripemd160.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 19 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_sha1(Data: string): string;
var
  HASH: TDCP_sha1;
  Digest: array[0..19] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha1.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 19 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_sha256(Data: string): string;
var
  HASH: TDCP_sha256;
  Digest: array[0..31] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 31 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_sha384(Data: string): string;
var
  HASH: TDCP_sha384;
  Digest: array[0..47] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha384.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 47 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_sha512(Data: string): string;
var
  HASH: TDCP_sha512;
  Digest: array[0..63] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_sha512.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 63 do
      Result := Result + IntToHex(Digest[I], 2);
  finally
    Hash.Free;
  end;
end;

function ps_tiger(Data: string): string;
var
  HASH: TDCP_tiger;
  Digest: array[0..23] of byte;
  I: integer;
begin
  dcpFillChar(Digest, SizeOf(Digest), 0);
  Hash := TDCP_tiger.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(Data);
    Hash.Final(Digest);
    Result := '';
    for I := 0 to 23 do
      Result := Result + string(IntToHex(Digest[I], 2));
  finally
    Hash.Free;
  end;
end;

