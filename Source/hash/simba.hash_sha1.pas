// https://github.com/PascalVault/Lazarus_Hashing
//
// Author: domasz
// Licence: MIT

unit simba.hash_sha1;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

function Hash_SHA1(ABuffer: PByte; ALength: Integer): String;

implementation

{$R-}
{$Q-}

function Hash_SHA1(ABuffer: PByte; ALength: Integer): String;
var
  Hash: array[0..4] of Cardinal = ($67452301, $EFCDAB89, $98BADCFE, $10325476, $C3D2E1F0);

  procedure Process(buf: array of Byte);
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

    A := Hash[0];
    B := Hash[1];
    C := Hash[2];
    D := Hash[3];
    E := Hash[4];

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

    Hash[0] := Hash[0] + A;
    Hash[1] := Hash[1] + B;
    Hash[2] := Hash[2] + C;
    Hash[3] := Hash[3] + D;
    Hash[4] := Hash[4] + E;
  end;

  procedure Last(Msg: PByte; Length: Integer);
  var buf: array[0..63] of Byte;
      Left: Integer;
      Bits: QWord;
  begin
    Left := Length mod 64;

    FillChar(Buf, 64, 0);
    Move(Msg^, buf[0], Left);

    Buf[Left] := $80;

    Bits := ALength shl 3;

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

  function Final: String;
  var Msg: array[0..63] of Byte;
  begin
    if ALength mod 64 = 0 then begin
      FillChar(Msg, 64, 0);
      Last(@Msg[0], 64);
    end;

    Result := LowerCase(IntToHex(Hash[0], 8) + IntToHex(Hash[1], 8) + IntToHex(Hash[2], 8) +
                        IntToHex(Hash[3], 8) + IntToHex(Hash[4], 8));
  end;

var i: Integer;
    buf: array[0..63] of Byte;
begin
  i := 0;

  while i < ALength do begin
     if ALength - i > 63 then begin
       Move(ABuffer^, buf[0], 64);
       Inc(ABuffer, 64);

       Process(buf);
     end
     else
      Last(ABuffer, ALength);

     Inc(i, 64);
  end;

  Result := Final();
end;

end.

