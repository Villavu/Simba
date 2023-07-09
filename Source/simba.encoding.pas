{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.encoding;

{$i simba.Inc}

interface

uses
  Classes, SysUtils;

function HexEncode(const Data: String): String;
function HexDecode(const Data: String): String;

function Base32Encode(const Data: String): String;
function Base32Decode(const Data: String): String;

function Base64Encode(const Data: String): String;
function Base64Decode(const Data: String): String;

function GetOTPToken(Secret: String): Integer;

function SHA512String(const Data: String): String;
function SHA512File(const FileName: String): String;

function SHA256String(const Data: String): String;
function SHA256File(const FileName: String): String;

implementation

uses
  HMAC, DateUtils, sha1,
  fpsha256_simba, fpsha512_simba, fphashutils_simba;

{$R-}
{$Q-}

const
  Base32Table: array[0..31] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
  Base64Table: array[0..63] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  HexTable:    array[0..15] of Char = '0123456789abcdef';

function HexEncode(const Data:String): String;
var
  i: Integer;
begin
  if (Data = '') then
    Exit('');

  SetLength(Result, Length(Data) * 2);
  for i := 1 to Length(Data) do
  begin
    Result[i*2-1] := HexTable[Byte(Data[i]) shr $4];
    Result[i*2  ] := HexTable[Byte(Data[i]) and $F];
  end;
end;

function HexDecode(const Data: String): String;
var
  i: Integer;
begin
  if (Data = '') then
    Exit('');

  SetLength(Result, Length(Data) div 2);
  for I := 1 to Length(Result) do
  begin
    Byte(Result[i]) := 0;

    if (Byte(Data[i*2-1]) and $F0) = $30 then
      Inc(Result[i], (Byte(Data[i*2-1]) and $0F) shl 4)
    else
      Inc(Result[i], ((Byte(Data[i*2-1]) and $1F) + 9) shl 4);

    if (Byte(Data[i*2]) and $F0) = $30 then
      Inc(Result[i], Byte(Data[i*2]) and $0F)
    else
      Inc(Result[i], (Byte(Data[i*2]) and $1F) + 9);
  end;
end;

function Base32Encode(const Data: String): String;
var
  i,j,l: Integer;
begin
  if (Data = '') then
    Exit('');

  l := Length(Data);
  i := (l div 5);
  if (l mod 5) <> 0 then
    Inc(i);
  SetLength(Result, i*8);

  i := 1;
  j := 0;
  while (i+4 <= l) do
  begin
    Result[j+1] := Base32Table[  Byte(Data[i])   shr 3];
    Result[j+2] := Base32Table[((Byte(Data[i])   and $07) shl 2) or (Byte(Data[i+1]) shr 6)];
    Result[j+3] := Base32Table[ (Byte(Data[i+1]) and $3E) shr 1];
    Result[j+4] := Base32Table[((Byte(Data[i+1]) and $01) shl 4) or (Byte(Data[i+2]) shr 4)];
    Result[j+5] := Base32Table[((Byte(Data[i+2]) and $0F) shl 1) or (Byte(Data[i+3]) shr 7)];
    Result[j+6] := Base32Table[ (Byte(Data[i+3]) and $7C) shr 2];
    Result[j+7] := Base32Table[((Byte(Data[i+3]) and $03) shl 3) or (Byte(Data[i+4]) shr 5)];
    Result[j+8] := Base32Table[  Byte(Data[i+4]) and $1F];

    Inc(j,8);
    Inc(i,5);
  end;

  case l-i of
    0:
     begin
       Result[j+1] := Base32Table[  Byte(Data[i]) shr 3];
       Result[j+2] := Base32Table[((Byte(Data[i]) and $07) shl 2)];
       Result[j+3] := '=';
       Result[j+4] := '=';
       Result[j+5] := '=';
       Result[j+6] := '=';
       Result[j+7] := '=';
       Result[j+8] := '=';
     end;
    1:
     begin
       Result[j+1] := Base32Table[  Byte(Data[i])   shr 3];
       Result[j+2] := Base32Table[((Byte(Data[i])   and $07) shl 2) or (Byte(Data[i+1]) shr 6)];
       Result[j+3] := Base32Table[ (Byte(Data[i+1]) and $3E) shr 1];
       Result[j+4] := Base32Table[((Byte(Data[i+1]) and $01) shl 4)];
       Result[j+5] := '=';
       Result[j+6] := '=';
       Result[j+7] := '=';
       Result[j+8] := '=';
     end;
    2:
     begin
       Result[j+1] := Base32Table[  Byte(Data[i])   shr 3];
       Result[j+2] := Base32Table[((Byte(Data[i])   and $07) shl 2) or (Byte(Data[i+1]) shr 6)];
       Result[j+3] := Base32Table[ (Byte(Data[i+1]) and $3E) shr 1];
       Result[j+4] := Base32Table[((Byte(Data[i+1]) and $01) shl 4) or (Byte(Data[i+2]) shr 4)];
       Result[j+5] := Base32Table[((Byte(Data[i+2]) and $0F) shl 1)];
       Result[j+6] := '=';
       Result[j+7] := '=';
       Result[j+8] := '=';
     end;
    3:
     begin
       Result[j+1] := Base32Table[  Byte(Data[i])   shr 3];
       Result[j+2] := Base32Table[((Byte(Data[i])   and $07) shl 2) or (Byte(Data[i+1]) shr 6)];
       Result[j+3] := Base32Table[ (Byte(Data[i+1]) and $3E) shr 1];
       Result[j+4] := Base32Table[((Byte(Data[i+1]) and $01) shl 4) or (Byte(Data[i+2]) shr 4)];
       Result[j+5] := Base32Table[((Byte(Data[i+2]) and $0F) shl 1) or (Byte(Data[i+3]) shr 7)];
       Result[j+6] := Base32Table[ (Byte(Data[i+3]) and $7C) shr 2];
       Result[j+7] := Base32Table[((Byte(Data[i+3]) and $03) shl 3)];
       Result[j+8] := '=';
     end;
  end;
end;

function Base32Decode(const Data:String): String;
var
  i,j,k,l,n:Integer;
  a:array[0..7] of Byte;
begin
  l := Length(Data);
  if l < 8 then
    Exit('');

  k := (Length(Data) div 8) * 5;
  SetLength(Result, k);

  if Data[l  ] = '=' then Dec(k);
  if Data[l-2] = '=' then Dec(k);
  if Data[l-3] = '=' then Dec(k);
  if Data[l-5] = '=' then Dec(k);

  i := 0;
  j := 0;
  while (i < l) do
  begin
    for n := 0 to 7 do
    begin
      Inc(i);

      a[n] := 0;
      while (a[n] < 32) and (Data[i] <> Base32Table[a[n]]) do
        Inc(a[n]);
    end;

    if (i = l) then
      for n := 1 to 7 do
        if a[n] = 32 then
          a[n] := 0;

    Result[j+1] := Char((a[0] shl 3) or (a[1] shr 2));
    Result[j+2] := Char((a[1] shl 6) or (a[2] shl 1) or (a[3] shr 4));
    Result[j+3] := Char((a[3] shl 4) or (a[4] shr 1));
    Result[j+4] := Char((a[4] shl 7) or (a[5] shl 2) or (a[6] shr 3));
    Result[j+5] := Char((a[6] shl 5) or (a[7]      ));

    Inc(j, 5);
  end;

  SetLength(Result, k);
end;

function Base64Encode(const Data:String): String;
var
  i,j,l: Integer;
begin
  if (Data = '') then
    Exit('');

  l := Length(Data);
  i := (l div 3);
  if (l mod 3) <> 0 then
    Inc(i);
  SetLength(Result, i*4);

  i := 1;
  j := 0;
  while (i+2 <= l) do
  begin
    Result[j+1] := Base64Table[  Byte(Data[i])   shr 2];
    Result[j+2] := Base64Table[((Byte(Data[i])   and $03) shl 4) or (Byte(Data[i+1]) shr 4)];
    Result[j+3] := Base64Table[((Byte(Data[i+1]) and $0F) shl 2) or (Byte(Data[i+2]) shr 6)];
    Result[j+4] := Base64Table[  Byte(Data[i+2]) and $3F];

    Inc(j, 4);
    Inc(i, 3);
  end;

  if (i = l) then
  begin
    Result[j+1] := Base64Table[  Byte(Data[i]) shr 2];
    Result[j+2] := Base64Table[((Byte(Data[i]) and $03) shl 4)];
    Result[j+3] := '=';
    Result[j+4] := '=';
  end else
  if (i+1=l) then
  begin
    Result[j+1] := Base64Table[  Byte(Data[i])   shr 2];
    Result[j+2] := Base64Table[((Byte(Data[i])   and $03) shl 4) or (Byte(Data[i+1]) shr 4)];
    Result[j+3] := Base64Table[((Byte(Data[i+1]) and $0F) shl 2)];
    Result[j+4] := '=';
  end;
end;

function Base64Decode(const Data: String): String;
var
  i,j,k,l: Integer;
  a,b,c,d: Byte;
begin
  l := Length(Data);
  if (l < 4) then
    Exit('');

  k := (Length(Data) div 4) * 3;
  SetLength(Result, k);
  if Data[l  ] = '=' then Dec(k);
  if Data[l-1] = '=' then Dec(k);

  i := 0;
  j := 0;
  while (i < l) do
  begin
    a := 0; while (a < 64) and (Data[i+1] <> Base64Table[a]) do Inc(a);
    b := 0; while (b < 64) and (Data[i+2] <> Base64Table[b]) do Inc(b);
    c := 0; while (c < 64) and (Data[i+3] <> Base64Table[c]) do Inc(c);
    d := 0; while (d < 64) and (Data[i+4] <> Base64Table[d]) do Inc(d);

    if (i = l) then
    begin
      if (c = 64) then c := 0;
      if (d = 64) then d := 0;
    end;

    Result[j+1] := Char((a shl 2) or (b shr 4));
    Result[j+2] := Char((b shl 4) or (c shr 2));
    Result[j+3] := Char((c shl 6) or (d      ));

    Inc(i, 4);
    Inc(j, 3);
  end;

  SetLength(Result, k);
end;

function GetOTPToken(Secret: String): Integer;

  function Int64ToRawString(const Value: Int64): String;
  var
    B: array[0..7] of Byte absolute Value;
    I: Int32;
  begin
    Result := '';
    for I := 7 downto 0 do
      Result := Result + Char(B[I]);
  end;

var
  Digest: TSHA1Digest;
  Offset: Integer;
  Part1, Part2, Part3, Part4: UInt32;
begin
  Digest := HMACSHA1Digest(Base32Decode(Secret), Int64ToRawString(DateTimeToUnix(Now, False) div 30));
  Offset := Digest[19] and $0F;

  Part1 := (Digest[Offset + 0] and $7F);
  Part2 := (Digest[Offset + 1] and $FF);
  Part3 := (Digest[Offset + 2] and $FF);
  Part4 := (Digest[Offset + 3] and $FF);

  Result := ((Part1 shl 24) or (Part2 shl 16) or (Part3 shl 8) or Part4) mod 1000000;
end;

function SHA512String(const Data: String): String;
begin
  TSHA512.DigestHexa(BytesFromVar(@Data[1], Length(Data) * SizeOf(Char)), Result);
end;

function SHA512File(const FileName: String): String;
var
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    TSHA512.StreamHexa(Stream, Result);
  except
  end;
  if Assigned(Stream) then
    Stream.Free();
end;

function SHA256String(const Data: String): String;
begin
  TSHA256.DigestHexa(BytesFromVar(@Data[1], Length(Data) * SizeOf(Char)), Result);
end;

function SHA256File(const FileName: String): String;
var
  Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);

    TSHA256.StreamHexa(Stream, Result);
  except
  end;
  if Assigned(Stream) then
    Stream.Free();
end;

end.

