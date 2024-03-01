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
  HashAlgo = (
    SHA1, SHA256, SHA384, SHA512,
    MD5,
    CRC32, CRC64
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

  procedure RegisterHasher(Algo: HashAlgo; HashClass: TSimbaHasherClass);

  function HashBuffer(Algo: HashAlgo; Buf: PByte; Len: PtrUInt): String;
  function HashString(Algo: HashAlgo; const S: String): String;
  function HashFile(Algo: HashAlgo; const FileName: String): String;

implementation

uses
  simba.hash_sha1, simba.hash_sha256, simba.hash_sha384, simba.hash_sha512,
  simba.hash_md5,
  simba.hash_crc32, simba.hash_crc64;

var
  Hashers: array[HashAlgo] of TSimbaHasherClass;

procedure RegisterHasher(Algo: HashAlgo; HashClass: TSimbaHasherClass);
begin
  Hashers[Algo] := HashClass;
end;

function HashBuffer(Algo: HashAlgo; Buf: PByte; Len: PtrUInt): String;
begin
  if (Hashers[Algo] = nil) then
    SimbaException('Hash type not registered');

  if (Len > 0) then
    with Hashers[Algo].Create() do
    try
      Update(Buf, Len);

      Result := Final();
    finally
      Free();
    end
  else
    Result := '';
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

