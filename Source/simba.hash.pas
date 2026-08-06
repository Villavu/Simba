{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.hash;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  {$SCOPEDENUMS ON}
  EDigest = (
    MD4, MD5,
    SHA1, SHA256, SHA512,
    WHIRLPOOL
  );
  {$SCOPEDENUMS OFF}

  // Cryptographic digests -> lowercase hex string.
  function DigestData(Algo: EDigest; Buf: PByte; Len: Int32): String;
  function DigestString(Algo: EDigest; const S: String): String;
  function DigestFile(Algo: EDigest; const FileName: String): String;

  // CRC32 checksum -> UInt32.
  function CrcData(Buf: PByte; Len: Int32): UInt32;
  function CrcString(const S: String): UInt32;
  function CrcFile(const FileName: String): UInt32;

  // Fast non-cryptographic 32-bit hash (xxHash32) -> UInt32.
  function HashData(Data: PByte; Len: Int32; Seed: UInt32 = 0): UInt32;
  function HashString(const S: String; Seed: UInt32 = 0): UInt32;
  function HashFile(const FileName: String; Seed: UInt32 = 0): UInt32;

implementation

uses
  mormor2_xxhash32,
  md5, fpsha256, fpsha512, sha1,
  simba.crc, simba.whirlpool;

function DigestData(Algo: EDigest; Buf: PByte; Len: Int32): String;
var
  SHA256: TSHA256;
  SHA512: TSHA512;
begin
  Result := '';

  if (Len > 0) then
    case Algo of
      EDigest.MD4:   Result := MD4Print(MD4Buffer(Buf^, Len));
      EDigest.MD5:   Result := MD5Print(MD5Buffer(Buf^, Len));

      EDigest.SHA1:  Result := SHA1Print(SHA1Buffer(Buf^, Len));
      EDigest.SHA256:
        begin
          SHA256.Init;
          SHA256.Update(Buf, Len);
          SHA256.Final;
          SHA256.OutputHexa(Result);
          Result := LowerCase(Result);
        end;
      EDigest.SHA512:
        begin
          SHA512.Init;
          SHA512.Update(Buf, Len);
          SHA512.Final;
          SHA512.OutputHexa(Result);
          Result := LowerCase(Result);
        end;

      EDigest.WHIRLPOOL: Result := WhirlpoolHex(Buf, Len);
    end;
end;

function DigestString(Algo: EDigest; const S: String): String;
begin
  if (Length(S) > 0) then
    Result := DigestData(Algo, @S[1], Length(S))
  else
    Result := '';
end;

function DigestFile(Algo: EDigest; const FileName: String): String;
var
  Bytes: TBytes;
begin
  try
    Bytes := GetFileContents(FileName);
  except
  end;

  if (Length(Bytes) > 0) then
    Result := DigestData(Algo, @Bytes[0], Length(Bytes))
  else
    Result := '';
end;

function CrcData(Buf: PByte; Len: Int32): UInt32;
begin
  if (Len > 0) then
    Result := CRC32(Buf, Len)
  else
    Result := 0;
end;

function CrcString(const S: String): UInt32;
begin
  if (Length(S) > 0) then
    Result := CrcData(@S[1], Length(S))
  else
    Result := 0;
end;

function CrcFile(const FileName: String): UInt32;
var
  Bytes: TBytes;
begin
  Result := 0;

  try
    Bytes := GetFileContents(FileName);
  except
  end;

  if (Length(Bytes) > 0) then
    Result := CrcData(@Bytes[0], Length(Bytes));
end;

function HashData(Data: PByte; Len: Int32; Seed: UInt32): UInt32;
begin
  Result := xxHash32(Data, Len, Seed);
end;

function HashString(const S: String; Seed: UInt32): UInt32;
begin
  if (Length(S) > 0) then
    Result := xxHash32(@S[1], Length(S), Seed)
  else
    Result := Seed;
end;

function HashFile(const FileName: String; Seed: UInt32): UInt32;
var
  Bytes: TBytes;
begin
  Result := Seed;

  try
    Bytes := GetFileContents(FileName);
  except
  end;

  if (Length(Bytes) > 0) then
    Result := xxHash32(@Bytes[0], Length(Bytes), Seed);
end;

end.
