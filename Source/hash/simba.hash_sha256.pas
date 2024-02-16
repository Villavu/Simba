// https://github.com/PascalVault/Lazarus_Hashing
//
// Author: domasz
// Licence: MIT

unit simba.hash_sha256;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.hash;

type
  THasherSHA256 = class(TSimbaHasher)
  private
    FTotalSize: Int64;
    FHash: array[0..7] of Cardinal;
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
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );

procedure THasherSHA256.Update(Msg: PByte; Length: Integer);
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
  Inc(FTotalSize, Length);

  j := 0;

  while j <= Length do begin

    if Length - j > 63 then begin
      Move(Msg^, buf[0], 64);
      Inc(Msg, 64);
    end
    else begin
      Left := Length mod 64;

      FillChar(buf, 64, 0);
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
    end;

    Move(buf[0], w[0], 64);
    for i:=0 to 15 do w[i] := SwapEndian(w[i]);

    for i:=16 to 63 do begin
      s0 := RorDWord(w[i-15], 7) xor RorDWord(w[i-15], 18) xor (w[i-15] shr 3);
      s1 := RorDWord(w[i-2], 17) xor RorDWord(w[i-2], 19)  xor (w[i-2] shr  10);
      w[i] := w[i-16] +  s0 +  w[i-7] +  s1;
    end;

    a := FHash[0];
    b := FHash[1];
    c := FHash[2];
    d := FHash[3];
    e := FHash[4];
    f := FHash[5];
    g := FHash[6];
    h := FHash[7];

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
    FHash[0] := FHash[0] + a;
    FHash[1] := FHash[1] + b;
    FHash[2] := FHash[2] + c;
    FHash[3] := FHash[3] + d;
    FHash[4] := FHash[4] + e;
    FHash[5] := FHash[5] + f;
    FHash[6] := FHash[6] + g;
    FHash[7] := FHash[7] + h;

    Inc(j, 64);
  end;
end;

function THasherSHA256.Final: String;
begin
  Result := Hex(FHash[0], 8) + Hex(FHash[1], 8) + Hex(FHash[2], 8) +
            Hex(FHash[3], 8) + Hex(FHash[4], 8) + Hex(FHash[5], 8) +
            Hex(FHash[6], 8) + Hex(FHash[7], 8);
end;

constructor THasherSHA256.Create;
begin
  FHash[0] := $6a09e667;
  FHash[1] := $bb67ae85;
  FHash[2] := $3c6ef372;
  FHash[3] := $a54ff53a;
  FHash[4] := $510e527f;
  FHash[5] := $9b05688c;
  FHash[6] := $1f83d9ab;
  FHash[7] := $5be0cd19
end;

initialization
  RegisterHasher(EHashType.SHA256, THasherSHA256);

end.

