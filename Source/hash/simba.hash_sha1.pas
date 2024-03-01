// https://github.com/PascalVault/Lazarus_Hashing
//
// Author: domasz
// Licence: MIT

unit simba.hash_sha1;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.hash;

type
  THasherSHA1 = class(TSimbaHasher)
  private
    FTotalSize: Int64;
    FHash: array[0..4] of Cardinal;

    procedure Process(buf: array of Byte);
    procedure Last(Msg: PByte; Length: Integer);
  public
    constructor Create; override;
    procedure Update(Msg: PByte; Length: Integer); override;
    function Final: String; override;
  end;

implementation

{$R-}
{$Q-}

procedure THasherSHA1.Update(Msg: PByte; Length: Integer);
var i: Integer;
    buf: array[0..63] of Byte;
begin
  Inc(FTotalSize, Length);
  i := 0;

  while i < Length do begin
     if Length - i > 63 then begin
       Move(Msg^, buf[0], 64);
       Inc(Msg, 64);

       Process(buf);
     end
     else Last(Msg, Length);

     Inc(i, 64);
  end;
end;

procedure THasherSHA1.Last(Msg: PByte; Length: Integer);
var buf: array[0..63] of Byte;
    Left: Integer;
    Bits: QWord;
begin
  Left := Length mod 64;

  FillChar(Buf, 64, 0);
  Move(Msg^, buf[0], Left);

  Buf[Left] := $80;

  Bits := FTotalSize shl 3;

  buf[56] := bits shr 56;
  buf[57] := bits shr 48;
  buf[58] := bits shr 40;
  buf[59] := bits shr 32;
  buf[60] := bits shr 24;
  buf[61] := bits shr 16;
  buf[62] := bits shr 8;
  buf[63] := bits;

  Process(buf);
end;

procedure THasherSHA1.Process(buf: array of Byte);
var j: Integer;
    A,B,C,D,E,F: Cardinal;
    w: array[0..79] of Integer;
    r: Integer;
    T: Cardinal;
begin
  for j:=0 to 15 do begin
    w[j] := (buf[j*4] shl 24) or (buf[j*4 +1] shl 16) or (buf[j*4 +2] shl 8) or (buf[j*4 +3]);
  end;

  for r := 16 to 79 do begin
    T    := w[r - 3] xor w[r - 8] xor w[r - 14] xor w[r - 16];
    w[r] := RolDWord(T, 1);
  end;

  A := FHash[0];
  B := FHash[1];
  C := FHash[2];
  D := FHash[3];
  E := FHash[4];

  for r := 0 to 19 do begin
    F := ((B and C) or ((not B) and D)) + $5A827999;
    T := RolDWord(A, 5) + F + E + w[r];
    E := D;
    D := C;
    C := RolDWord(B, 30);
    B := A;
    A := T;
  end;

  for r := 20 to 39 do begin
    F := (B xor C xor D) + $6ED9EBA1;
    T := RolDWord(A, 5) + F + E + w[r];
    E := D;
    D := C;
    C := RolDWord(B, 30);
    B := A;
    A := T;
  end;

  for r := 40 to 59 do begin
    F := (B and C or B and D or C and D) + $8F1BBCDC;
    T := RolDWord(A, 5) + F + E + w[r];
    E := D;
    D := C;
    C := RolDWord(B, 30);
    B := A;
    A := T;
  end;

  for r := 60 to 79 do begin
    F := (B xor C xor D) + $CA62C1D6;
    T := RolDWord(A, 5) + F + E + w[r];
    E := D;
    D := C;
    C := RolDWord(B, 30);
    B := A;
    A := T;
  end;

  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;
  FHash[4] := FHash[4] + E;
end;

function THasherSHA1.Final: String;
var Msg: array[0..63] of Byte;
begin
  if FTotalSize mod 64 = 0 then begin
    FillChar(Msg, 64, 0);
    Last(@Msg[0], 64);
  end;

  Result := Hex(FHash[0], 8) + Hex(FHash[1], 8) + Hex(FHash[2], 8) +
            Hex(FHash[3], 8) + Hex(FHash[4], 8);
end;

constructor THasherSHA1.Create;
begin
  FHash[0] := $67452301;
  FHash[1] := $EFCDAB89;
  FHash[2] := $98BADCFE;
  FHash[3] := $10325476;
  FHash[4] := $C3D2E1F0;
end;

initialization
  RegisterHasher(HashAlgo.SHA1, THasherSHA1);

end.

