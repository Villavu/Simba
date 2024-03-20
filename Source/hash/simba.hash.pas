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
  HashAlgo = (SHA1, SHA256, SHA384, SHA512, MD5);
  {$SCOPEDENUMS OFF}

  function HashBuffer(Algo: HashAlgo; Buf: PByte; Len: Integer): String;
  function HashString(Algo: HashAlgo; const S: String): String;
  function HashFile(Algo: HashAlgo; const FileName: String): String;

  function Hash32(Data: PByte; Len: Int32; Seed: UInt32 = 0): UInt32; overload;
  function Hash32(S: String; Seed: UInt32 = 0): UInt32; overload;

  function Hash64(Data: PByte; Len: Int32; Seed: UInt64 = 0): UInt64; overload;
  function Hash64(S: String; Seed: UInt64 = 0): UInt64; overload;

  function CRC32(Data: PByte; Len: Int32): UInt32;
  function CRC64(Data: PByte; Len: Int32): UInt64;

implementation

uses
  crc,
  simba.hash_sha1, simba.hash_sha256, simba.hash_sha384, simba.hash_sha512,
  simba.hash_md5,
  simba.hash_murmur;

function HashBuffer(Algo: HashAlgo; Buf: PByte; Len: Integer): String;
begin
  Result := '';

  if (Len > 0) then
    case Algo of
      HashAlgo.SHA1:   Result := Hash_SHA1(Buf, Len);
      HashAlgo.SHA256: Result := Hash_SHA256(Buf, Len);
      HashAlgo.SHA384: Result := Hash_SHA384(Buf, Len);
      HashAlgo.SHA512: Result := Hash_SHA512(Buf, Len);
      HashAlgo.MD5:    Result := Hash_MD5(Buf, Len);
    end;
end;

function HashString(Algo: HashAlgo; const S: String): String;
begin
  if (Length(S) > 0) then
    Result := HashBuffer(Algo, @S[1], Length(S))
  else
    Result := '';
end;

function HashFile(Algo: HashAlgo; const FileName: String): String;
var
  Bytes: TBytes;
begin
  try
    Bytes := GetFileContents(FileName);
  except
  end;

  if (Length(Bytes) > 0) then
    Result := HashBuffer(Algo, @Bytes[0], Length(Bytes))
  else
    Result := '';
end;

function Hash32(Data: PByte; Len: Int32; Seed: UInt32): UInt32;
begin
  Result := TMurmur2aLE.HashBuf(Data, Len, Seed);
end;

function Hash32(S: String; Seed: UInt32): UInt32;
begin
  if (Length(S) > 0) then
    Result := Hash32(@S[1], Length(S), Seed)
  else
    Result := Seed;
end;

function Hash64(Data: PByte; Len: Int32; Seed: UInt64): UInt64;
begin
  Result := TMurmur64aLE.HashBuf(Data, Len, Seed);
end;

function Hash64(S: String; Seed: UInt64): UInt64;
begin
  if (Length(S) > 0) then
    Result := Hash64(@S[1], Length(S), Seed)
  else
    Result := Seed;
end;

function CRC32(Data: PByte; Len: Int32): UInt32;
begin
  Result := crc.crc32(crc.crc32(0, nil, 0), Data, Len);
end;

function CRC64(Data: PByte; Len: Int32): UInt64;
begin
  Result := crc.crc64(crc.crc64(0, nil, 0), Data, Len);
end;

end.

