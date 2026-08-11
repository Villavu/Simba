{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.encoding;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

{$PUSH}
{$SCOPEDENUMS ON}
type
  EBaseEncoding = (b64URL, b64, b32, b32Hex, b16);
{$POP}

function BaseEncode(Encoding: EBaseEncoding; const Data: String): String; overload;
function BaseEncode(Encoding: EBaseEncoding; Stream: TStream): String; overload;
function BaseDecode(Encoding: EBaseEncoding; const Data: String): String; overload;
procedure BaseDecode(Encoding: EBaseEncoding; const Data: String; Stream: TStream); overload;

function BaseEncodeBytes(Encoding: EBaseEncoding; Bytes: TByteArray): String;
function BaseDecodeBytes(Encoding: EBaseEncoding; const Data: String): TByteArray;

function HOTPCalculateToken(const aSecret: String; const Counter: Integer): Integer;
function TOTPCalculateToken(const aSecret: String): Integer;

implementation

uses
  SHA1, HMAC, DateUtils,
  fpbasenenc;

function BaseEncode(Encoding: EBaseEncoding; const Data: String): String;
begin
  if (Length(Data) > 0) then
  begin
    case Encoding of
      EBaseEncoding.b64:    Result := Base64.Encode(Data);
      EBaseEncoding.b64URL: Result := Base64URL.Encode(Data);
      EBaseEncoding.b32:    Result := Base32.Encode(Data);
      EBaseEncoding.b32Hex: Result := Base32Hex.Encode(Data);
      EBaseEncoding.b16:    Result := Base16.Encode(Data);
    end;
  end else
    Result := '';
end;

function BaseEncodeBytes(Encoding: EBaseEncoding; Bytes: TByteArray): String;
begin
  if (Length(Bytes) > 0) then
  begin
    case Encoding of
      EBaseEncoding.b64:    Result := Base64.Encode(Bytes);
      EBaseEncoding.b64URL: Result := Base64URL.Encode(Bytes);
      EBaseEncoding.b32:    Result := Base32.Encode(Bytes);
      EBaseEncoding.b32Hex: Result := Base32Hex.Encode(Bytes);
      EBaseEncoding.b16:    Result := Base16.Encode(Bytes);
    end;
  end else
    Result := '';
end;

function BaseDecode(Encoding: EBaseEncoding; const Data: String): String;
var
  Bytes: TByteArray;
begin
  if (Length(Data) > 0) then
  begin
    case Encoding of
      EBaseEncoding.b64:    Bytes := Base64.Decode(Data);
      EBaseEncoding.b64URL: Bytes := Base64URL.Decode(Data);
      EBaseEncoding.b32:    Bytes := Base32.Decode(Data);
      EBaseEncoding.b32Hex: Bytes := Base32Hex.Decode(Data);
      EBaseEncoding.b16:    Bytes := Base16.Decode(Data);
    end;

    Result := GetRawStringFromBytes(Bytes);
  end else
    Result := '';
end;

function BaseDecodeBytes(Encoding: EBaseEncoding; const Data: String): TByteArray;
begin
  if (Length(Data) > 0) then
  begin
    case Encoding of
      EBaseEncoding.b64:    Result := Base64.Decode(Data);
      EBaseEncoding.b64URL: Result := Base64URL.Decode(Data);
      EBaseEncoding.b32:    Result := Base32.Decode(Data);
      EBaseEncoding.b32Hex: Result := Base32Hex.Decode(Data);
      EBaseEncoding.b16:    Result := Base16.Decode(Data);
    end;
  end else
    Result := [];
end;

// Encodes from the stream's current position to the end.
function BaseEncode(Encoding: EBaseEncoding; Stream: TStream): String;
var
  Bytes: TByteArray;
begin
  SetLength(Bytes, Stream.Size - Stream.Position);
  if (Length(Bytes) > 0) then
    Stream.ReadBuffer(Bytes[0], Length(Bytes));

  Result := BaseEncodeBytes(Encoding, Bytes);
end;

// Decodes and writes the bytes at the stream's current position.
procedure BaseDecode(Encoding: EBaseEncoding; const Data: String; Stream: TStream);
var
  Bytes: TByteArray;
begin
  Bytes := BaseDecodeBytes(Encoding, Data);
  if (Length(Bytes) > 0) then
    Stream.WriteBuffer(Bytes[0], Length(Bytes));
end;

// https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/fcl-hash/src/onetimepass.pp
function HOTPCalculateToken(const aSecret: String; const Counter: Integer): Integer;
const
  TOTP_Mod = 1000000;
  TOTP_KeyRegeneration = 30; // Time step for TOTP generation. Google Authenticator uses 30 seconds.

  function Int64ToRawString(const Value: Int64): String;
  var
    B: array[0..7] of Byte;
    I: Int32;
  begin
    PInt64(@B)^ := Value;
    Result := '';
    for I := 7 downto 0 do
      Result := Result + AnsiChar(B[I]);
  end;

var
  Digest: TSHA1Digest;
  Key: UInt32;
  Offset: Longint;
  Part1, Part2, Part3, Part4: UInt32;
  SecretBinBuf: TByteArray;
  STime, SSecretBin: RawbyteString;
  Time: Longint;
begin
  Time := Counter;
  if (Time = -1) then
    Time := DateTimeToUnix(Now, False) div TOTP_KeyRegeneration;

  SSecretBin := '';
  SecretBinBuf := Base32.Decode(aSecret);
  STime := Int64ToRawString(Time);
  SetLength(SSecretBin, Length(SecretBinBuf));
  Move(SecretBinBuf[0], SSecretBin[1], Length(SecretBinBuf));

  Digest := HMACSHA1Digest(SSecretBin, STime);
  Offset := Digest[19] and $0F;
  Part1 := (Digest[Offset + 0] and $7F);
  Part2 := (Digest[Offset + 1] and $FF);
  Part3 := (Digest[Offset + 2] and $FF);
  Part4 := (Digest[Offset + 3] and $FF);
  Key := (Part1 shl 24) or (Part2 shl 16) or (Part3 shl 8) or Part4;

  Result := Key mod TOTP_Mod; // mod 1000000 in case of otpLength of 6 digits
end;

function TOTPCalculateToken(const aSecret: String): Integer;
begin
  Result := HOTPCalculateToken(aSecret, -1);
end;

end.

