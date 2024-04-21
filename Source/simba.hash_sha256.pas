// https://github.com/PascalVault/Lazarus_Hashing
//
// Author: domasz
// Licence: MIT

unit simba.hash_sha256;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

function Hash_SHA256(ABuffer: PByte; ALength: Integer): String;

implementation

{$R-}
{$Q-}

const
  K: array[0..63] of Cardinal = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );

function Hash_SHA256(ABuffer: PByte; ALength: Integer): String;
var
  Hash: array[0..7] of Cardinal = ($6a09e667, $bb67ae85, $3c6ef372, $a54ff53a,$510e527f, $9b05688c, $1f83d9ab, $5be0cd19);

  function Final: String;
  begin
    Result := LowerCase(IntToHex(Hash[0], 8) + IntToHex(Hash[1], 8) + IntToHex(Hash[2], 8) +
                        IntToHex(Hash[3], 8) + IntToHex(Hash[4], 8) + IntToHex(Hash[5], 8) +
                        IntToHex(Hash[6], 8) + IntToHex(Hash[7], 8));
  end;

var i: Integer;
    j: Integer;
    A,B,C,D,E,F,G,H: Cardinal;
    w: array[0..79] of Cardinal;
    buf: array[0..63] of Byte;
    Left: Integer;
    Bits: QWord;
    s0, s1: Cardinal;
    ch, t1, t2, maj: Cardinal;
begin
  j := 0;

  while j <= ALength do begin

    if ALength - j > 63 then begin
      Move(ABuffer^, buf[0], 64);
      Inc(ABuffer, 64);
    end
    else begin
      Left := ALength mod 64;

      FillChar(buf, 64, 0);
      Move(ABuffer^, buf[0], Left);

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
    end;

    Move(buf[0], w[0], 64);
    for i:=0 to 15 do w[i] := SwapEndian(w[i]);

    for i:=16 to 63 do begin
      s0 := RorDWord(w[i-15], 7) xor RorDWord(w[i-15], 18) xor (w[i-15] shr 3);
      s1 := RorDWord(w[i-2], 17) xor RorDWord(w[i-2], 19)  xor (w[i-2] shr  10);
      w[i] := w[i-16] +  s0 +  w[i-7] +  s1;
    end;

    a := Hash[0];
    b := Hash[1];
    c := Hash[2];
    d := Hash[3];
    e := Hash[4];
    f := Hash[5];
    g := Hash[6];
    h := Hash[7];

    //main loop
    for i:=0 to 63 do begin
      s1 := RorDWord(e, 6) xor RorDWord(e, 11) xor RorDWord(e, 25);

      ch := (e and f) xor ((not e) and g);
      t1 := h + s1 + ch + k[i] + w[i];

      s0 := RorDWord(a, 2) xor RorDWord(a, 13) xor RorDWord(a, 22);
      maj := (a and b) xor (a and c) xor (b and c);
      t2 := s0 + maj;

      h := g;
      g := f;
      f := e;
      e := d + t1;
      d := c;
      c := b;
      b := a;
      a := t1 + t2;
    end;

    //finalize
    Hash[0] := Hash[0] + a;
    Hash[1] := Hash[1] + b;
    Hash[2] := Hash[2] + c;
    Hash[3] := Hash[3] + d;
    Hash[4] := Hash[4] + e;
    Hash[5] := Hash[5] + f;
    Hash[6] := Hash[6] + g;
    Hash[7] := Hash[7] + h;

    Inc(j, 64);
  end;

  Result := Final();
end;

end.

