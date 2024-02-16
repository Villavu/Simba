// https://github.com/PascalVault/Lazarus_Hashing
//
// Author: domasz
// Licence: MIT

unit simba.hash_md5;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.hash;

type
  THasherMD5 = class(TSimbaHasher)
  private
    FHash: array[0..3] of Cardinal;
    FTotalSize: Int64;
    procedure Process(X: array of Cardinal);
    procedure Last(Msg: PByte; Length: Integer);
  public
    constructor Create; override;
    procedure Update(Msg: PByte; Length: Integer); override;
    function Final: String; override;
  end;

implementation

{$R-}
{$Q-}

const
  K: array[0..63] of Cardinal = (
    $D76AA478, $E8C7B756, $242070DB, $C1BDCEEE,
    $F57C0FAF, $4787C62A, $A8304613, $FD469501,
    $698098D8, $8B44F7AF, $FFFF5BB1, $895CD7BE,
    $6B901122, $FD987193, $A679438E, $49B40821,
    $F61E2562, $C040B340, $265E5A51, $E9B6C7AA,
    $D62F105D, $02441453, $D8A1E681, $E7D3FBC8,
    $21E1CDE6, $C33707D6, $F4D50D87, $455A14ED,
    $A9E3E905, $FCEFA3F8, $676F02D9, $8D2A4C8A,
    $FFFA3942, $8771F681, $6D9D6122, $FDE5380C,
    $A4BEEA44, $4BDECFA9, $F6BB4B60, $BEBFBC70,
    $289B7EC6, $EAA127FA, $D4EF3085, $04881D05,
    $D9D4D039, $E6DB99E5, $1FA27CF8, $C4AC5665,
    $F4292244, $432AFF97, $AB9423A7, $FC93A039,
    $655B59C3, $8F0CCC92, $FFEFF47D, $85845DD1,
    $6FA87E4F, $FE2CE6E0, $A3014314, $4E0811A1,
    $F7537E82, $BD3AF235, $2AD7D2BB, $EB86D391
  );

  S: array[0..63] of Byte = (
    7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
  );

procedure THasherMD5.Update(Msg: PByte; Length: Integer);
var i: Integer;
    X: array[0..15] of Cardinal;
begin
  Inc(FTotalSize, Length);
  i := 0;

  while i < Length do begin
    if Length - i > 63 then begin
      Move(Msg^, X[0], 64);
      Inc(Msg, 64);

      Process(X);
    end
    else Last(Msg, Length);

    Inc(i, 64);
  end;
end;

procedure THasherMD5.Last(Msg: PByte; Length: Integer);
var buf: array[0..63] of Byte;
    Left: Integer;
    Bits: QWord;
    X: array[0..15] of Cardinal;
begin
  Left := Length mod 64;

  FillChar(Buf, 64, 0);
  Move(Msg^, buf[0], Left);

  Buf[Left] := $80;

  Bits := FTotalSize shl 3;

  buf[56] := bits;
  buf[57] := bits shr 8;
  buf[58] := bits shr 16;
  buf[59] := bits shr 24;
  buf[60] := bits shr 32;
  buf[61] := bits shr 40;
  buf[62] := bits shr 48;
  Buf[63] := bits shr 56;

  Move(buf[0], X[0], 64);
  Process(X);
end;

procedure THasherMD5.Process(X: array of Cardinal);
var j: Integer;
    A,B,C,D,F,G: Cardinal;
begin
  A := FHash[0];
  B := FHash[1];
  C := FHash[2];
  D := FHash[3];

  for j:=0 to 63 do begin

    if j <= 15 then begin
        F := (B and C) or ((not B) and D);
        g := j;
    end
    else if j <= 31 then begin
        F := (D and B) or ((not D) and C);
        g := (5*j + 1) mod 16;
    end
    else if j <= 47 then begin
        F := B xor C xor D;
        g := (3*j + 5) mod 16;
    end
    else if j <= 63 then begin
        F := C xor (B or (not D));
        g := (7*j) mod 16;
    end;

    F := F + A + K[j] + X[g];
    A := D;
    D := C;
    C := B;
    B := B + RolDWord(F, s[j]);
  end;

  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;
end;

function THasherMD5.Final: String;
var
  Msg: array[0..63] of Byte;
begin
  if FTotalSize mod 64 = 0 then
  begin
    FillChar(Msg, 64, 0);
    Last(@Msg[0], 64);
  end;

  FHash[0] := SwapEndian(FHash[0]);
  FHash[1] := SwapEndian(FHash[1]);
  FHash[2] := SwapEndian(FHash[2]);
  FHash[3] := SwapEndian(FHash[3]);

  Result := Hex(FHash[0], 8) + Hex(FHash[1], 8) + Hex(FHash[2], 8) + Hex(FHash[3], 8);
end;

constructor THasherMD5.Create;
begin
  FHash[0] := $67452301;
  FHash[1] := $EFCDAB89;
  FHash[2] := $98BADCFE;
  FHash[3] := $10325476;
end;

initialization
  RegisterHasher(EHashType.MD5, THasherMD5);

end.

