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
  {$scopedenums on}
  PHashType = ^EHashType;
  EHashType = (
    SHA1, SHA256, SHA384, SHA512,
    MD5,
    CRC32
  );
  {$scopedenums off}

  TSimbaHasher = class
  protected
    const HexDigits: array[0..15] of Char = '0123456789abcdef'; // lowercase
    class function Hex(Value: Int64; Digits: Integer): string;
    class function Hex(Value: UInt64; Digits: Integer): string;
  public
    constructor Create; virtual; abstract;
    procedure Update(Msg: PByte; Length: Integer); virtual; abstract;
    function Final: String; virtual; abstract;
  end;
  TSimbaHasherClass = class of TSimbaHasher;

  procedure RegisterHasher(HashType: EHashType; HashClass: TSimbaHasherClass);

  function HashBuffer(HashType: EHashType; Buf: PByte; Len: PtrUInt): String;
  function HashString(HashType: EHashType; S: String): String;
  function HashFile(HashType: EHashType; FileName: String): String;

implementation

uses
  simba.hash_sha1, simba.hash_sha256, simba.hash_sha384, simba.hash_sha512,
  simba.hash_md5,
  simba.hash_crc32;

var
  Hashers: array[EHashType] of TSimbaHasherClass;

procedure RegisterHasher(HashType: EHashType; HashClass: TSimbaHasherClass);
begin
  Hashers[HashType] := HashClass;
end;

function HashBuffer(HashType: EHashType; Buf: PByte; Len: PtrUInt): String;
begin
  if (Hashers[HashType] = nil) then
    SimbaException('Hash type not registered');

  if (Len > 0) then
    with Hashers[HashType].Create() do
    try
      Update(Buf, Len);

      Result := Final();
    finally
      Free();
    end
  else
    Result := '';
end;

function HashString(HashType: EHashType; S: String): String;
begin
  if (Length(S) > 0) then
    Result := HashBuffer(HashType, @S[1], Length(S))
  else
    Result := '';
end;

function HashFile(HashType: EHashType; FileName: String): String;
var
  Bytes: TBytes;
begin
  try
    Bytes := GetFileContents(FileName);
  except
  end;

  if (Length(Bytes) > 0) then
    Result := HashBuffer(HashType, @Bytes[0], Length(Bytes))
  else
    Result := '';
end;

class function TSimbaHasher.Hex(Value: Int64; Digits: Integer): string;
var
  i: Integer;
begin
  if (Digits = 0) then
    Digits := 1;

  SetLength(Result, Digits);
  for i := 0 to Digits - 1 do
  begin
    Result[Digits - i] := HexDigits[Value and 15];
    Value := Value shr 4;
  end;

  while (Value <> 0) do
  begin
    Result := HexDigits[Value and 15] + Result;
    Value := Value shr 4;
  end;
end;

class function TSimbaHasher.Hex(Value: UInt64; Digits: Integer): string;
begin
  Result := Hex(Int64(Value), Digits);
end;

end.

