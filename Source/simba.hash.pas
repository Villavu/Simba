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
  EHashAlgo = (
    CRC32, CRC64,
    MD4, MD5,
    SHA1, SHA256, SHA512
  );
  {$SCOPEDENUMS OFF}

  function HashBuffer(Algo: EHashAlgo; Buf: PByte; Len: Integer): String;
  function HashString(Algo: EHashAlgo; const S: String): String;
  function HashFile(Algo: EHashAlgo; const FileName: String): String;

  function Hash32(Data: PByte; Len: Int32; Seed: UInt32 = 0): UInt32; overload;
  function Hash32(S: String; Seed: UInt32 = 0): UInt32; overload;

  function Hash64(Data: PByte; Len: Int32; Seed: UInt64 = 0): UInt64; overload;
  function Hash64(S: String; Seed: UInt64 = 0): UInt64; overload;

  function CRC32(Data: PByte; Len: Int32): UInt32;
  function CRC64(Data: PByte; Len: Int32): UInt64;

implementation

uses
  crc, md5, fpsha256, fpsha512, sha1,
  simba.hash_murmur;

function HashBuffer(Algo: EHashAlgo; Buf: PByte; Len: Integer): String;
var
  SHA256: TSHA256;
  SHA512: TSHA512;
begin
  Result := '';

  if (Len > 0) then
    case Algo of
      EHashAlgo.CRC32: Result := IntToHex(CRC32(Buf, Len), 8);
      EHashAlgo.CRC64: Result := IntToHex(CRC64(Buf, Len), 16);

      EHashAlgo.MD4:   Result := MD4Print(MD4Buffer(Buf^, Len));
      EHashAlgo.MD5:   Result := MD5Print(MD5Buffer(Buf^, Len));

      EHashAlgo.SHA1:  Result := SHA1Print(SHA1Buffer(Buf^, Len));
      EHashAlgo.SHA256:
        begin
          SHA256.Init;
          SHA256.Update(Buf, Len);
          SHA256.Final;
          SHA256.OutputHexa(Result);
          Result := LowerCase(Result);
        end;
      EHashAlgo.SHA512:
        begin
          SHA512.Init;
          SHA512.Update(Buf, Len);
          SHA512.Final;
          SHA512.OutputHexa(Result);
          Result := LowerCase(Result);
        end;
    end;
end;

function HashString(Algo: EHashAlgo; const S: String): String;
begin
  if (Length(S) > 0) then
    Result := HashBuffer(Algo, @S[1], Length(S))
  else
    Result := '';
end;

function HashFile(Algo: EHashAlgo; const FileName: String): String;
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

