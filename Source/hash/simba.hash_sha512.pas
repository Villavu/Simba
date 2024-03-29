// https://github.com/PascalVault/Lazarus_Hashing
//
// Author: domasz
// Licence: MIT

unit simba.hash_sha512;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

function Hash_SHA512(ABuffer: PByte; ALength: Integer): String;

implementation

{$R-}
{$Q-}

const
  K: array[0..79] of QWord = (
    QWord($428A2F98D728AE22), QWord($7137449123EF65CD), QWord($B5C0FBCFEC4D3B2F), QWord($E9B5DBA58189DBBC),
    QWord($3956C25BF348B538), QWord($59F111F1B605D019), QWord($923F82A4AF194F9B), QWord($AB1C5ED5DA6D8118),
    QWord($D807AA98A3030242), QWord($12835B0145706FBE), QWord($243185BE4EE4B28C), QWord($550C7DC3D5FFB4E2),
    QWord($72BE5D74F27B896F), QWord($80DEB1FE3B1696B1), QWord($9BDC06A725C71235), QWord($C19BF174CF692694),
    QWord($E49B69C19EF14AD2), QWord($EFBE4786384F25E3), QWord($0FC19DC68B8CD5B5), QWord($240CA1CC77AC9C65),
    QWord($2DE92C6F592B0275), QWord($4A7484AA6EA6E483), QWord($5CB0A9DCBD41FBD4), QWord($76F988DA831153B5),
    QWord($983E5152EE66DFAB), QWord($A831C66D2DB43210), QWord($B00327C898FB213F), QWord($BF597FC7BEEF0EE4),
    QWord($C6E00BF33DA88FC2), QWord($D5A79147930AA725), QWord($06CA6351E003826F), QWord($142929670A0E6E70),
    QWord($27B70A8546D22FFC), QWord($2E1B21385C26C926), QWord($4D2C6DFC5AC42AED), QWord($53380D139D95B3DF),
    QWord($650A73548BAF63DE), QWord($766A0ABB3C77B2A8), QWord($81C2C92E47EDAEE6), QWord($92722C851482353B),
    QWord($A2BFE8A14CF10364), QWord($A81A664BBC423001), QWord($C24B8B70D0F89791), QWord($C76C51A30654BE30),
    QWord($D192E819D6EF5218), QWord($D69906245565A910), QWord($F40E35855771202A), QWord($106AA07032BBD1B8),
    QWord($19A4C116B8D2D0C8), QWord($1E376C085141AB53), QWord($2748774CDF8EEB99), QWord($34B0BCB5E19B48A8),
    QWord($391C0CB3C5C95A63), QWord($4ED8AA4AE3418ACB), QWord($5B9CCA4F7763E373), QWord($682E6FF3D6B2B8A3),
    QWord($748F82EE5DEFB2FC), QWord($78A5636F43172F60), QWord($84C87814A1F0AB72), QWord($8CC702081A6439EC),
    QWord($90BEFFFA23631E28), QWord($A4506CEBDE82BDE9), QWord($BEF9A3F7B2C67915), QWord($C67178F2E372532B),
    QWord($CA273ECEEA26619C), QWord($D186B8C721C0C207), QWord($EADA7DD6CDE0EB1E), QWord($F57D4F7FEE6ED178),
    QWord($06F067AA72176FBA), QWord($0A637DC5A2C898A6), QWord($113F9804BEF90DAE), QWord($1B710B35131C471B),
    QWord($28DB77F523047D84), QWord($32CAAB7B40C72493), QWord($3C9EBE0A15C9BEBC), QWord($431D67C49C100D4C),
    QWord($4CC5D4BECB3E42B6), QWord($597F299CFC657E2A), QWord($5FCB6FAB3AD6FAEC), QWord($6C44198C4A475817)
  );

function Hash_SHA512(ABuffer: PByte; ALength: Integer): String;
var
  Hash: array[0..7] of QWord = (QWord($6A09E667F3BCC908), QWord($BB67AE8584CAA73B), QWord($3C6EF372FE94F82B), QWord($A54FF53A5F1D36F1),
                                QWord($510E527FADE682D1), QWord($9B05688C2B3E6C1F), QWord($1F83D9ABFB41BD6B), QWord($5BE0CD19137E2179));

  function Final: String;
  begin
    Result := LowerCase(IntToHex(Hash[0], 16) + IntToHex(Hash[1], 16) + IntToHex(Hash[2], 16) +
                        IntToHex(Hash[3], 16) + IntToHex(Hash[4], 16) + IntToHex(Hash[5], 16) +
                        IntToHex(Hash[6], 16) + IntToHex(Hash[7], 16));
  end;

var i: Integer;
    A,B,C,D,E,F,G,H: QWord;
    w: array[0..79] of QWord;
    buf: array[0..127] of Byte;
    Left: Integer;
    Bits: QWord;
    s0, s1: QWord;
    r: Integer;
    j: Integer;
    T, T2: QWord;
begin
  i := 0;

  while i <= ALength do begin

    if ALength - i > 127 then begin
      Move(ABuffer^, buf[0], 128);
      Inc(ABuffer, 128);
    end
    else begin
      Left := ALength mod 128;

      FillChar(buf, 128, 0);
      Move(ABuffer^, buf[0], Left);

      Buf[Left] := $80;

      Bits := ALength shl 3;

      buf[64+56] := bits shr 56;
      buf[64+57] := bits shr 48;
      buf[64+58] := bits shr 40;
      buf[64+59] := bits shr 32;
      buf[64+60] := bits shr 24;
      buf[64+61] := bits shr 16;
      buf[64+62] := bits shr 8;
      buf[64+63] := bits;
    end;

    a := Hash[0];
    b := Hash[1];
    c := Hash[2];
    d := Hash[3];
    e := Hash[4];
    f := Hash[5];
    g := Hash[6];
    h := Hash[7];

    Move(buf[0], w[0], 128);
    for j:=0 to 15 do w[j] := SwapEndian(w[j]);

    for r:=16 to 79 do begin
      T  := w[r - 2];
      T2 := w[r - 15];

      s0 := RorQWord(T, 19) xor RorQWord(T, 61) xor (T shr 6);
      s1 := RorQWord(T2, 1) xor RorQWord(T2, 8) xor (T2 shr 7);

      w[r] := s0 + w[r - 7] + s1 + w[r - 16];
    end;

    //main loop
    for r:=0 to 79 do begin
      T  := H + (RorQWord(E, 14) xor RorQWord(E, 18) xor RorQWord(E, 41)) +
            ((E and F) xor ((not E) and G)) + k[r] + w[r];

      T2 := (RorQWord(A, 28) xor RorQWord(A, 34) xor RorQWord(A, 39) )+
            ((A and B) xor (A and C) xor (B and C));

      H  := G;
      G  := F;
      F  := E;
      E  := D + T;
      D  := C;
      C  := B;
      B  := A;
      A  := T + T2;
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

    Inc(i, 128);
  end;

  Result := Final();
end;

end.

