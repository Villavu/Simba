// https://github.com/synopse/mORMot2

/// Framework Core Shared Types and RTL-like Functions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormor2_xxhash32;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function xxHash32(P: PByte; const Len: UInt32; const Seed: UInt32 = 0): UInt32;

implementation

{$R-}
{$Q-}

function xxHash32(P: PByte; const Len: UInt32; const Seed: UInt32): UInt32;
const
  PRIME32_1 = UInt32(2654435761);
  PRIME32_2 = UInt32(2246822519);
  PRIME32_3 = UInt32(3266489917);
  PRIME32_4 = UInt32(668265263);
  PRIME32_5 = UInt32(374761393);

  function Rol13(const Value: UInt32): UInt32; inline;
  begin
    Result := RolDWord(Value, 13);
  end;

var
  c1, c2, c3, c4: UInt32;
  PLimit, PEnd: PByte;
begin
  PEnd := P + Len;
  if Len >= 16 then
  begin
    PLimit := PEnd - 16;
    c3 := Seed;
    c2 := c3 + PRIME32_2;
    c1 := c2 + PRIME32_1;
    c4 := c3 - PRIME32_1;
    repeat
      c1 := PRIME32_1 * Rol13(c1 + PRIME32_2 * PUInt32(P)^);
      c2 := PRIME32_1 * Rol13(c2 + PRIME32_2 * PUInt32(P + 4)^);
      c3 := PRIME32_1 * Rol13(c3 + PRIME32_2 * PUInt32(P + 8)^);
      c4 := PRIME32_1 * Rol13(c4 + PRIME32_2 * PUInt32(P + 12)^);
      Inc(P, 16);
    until not (P <= PLimit);
    Result := RolDWord(c1, 1) + RolDWord(c2, 7) + RolDWord(c3, 12) + RolDWord(c4, 18);
  end else
    Result := Seed + PRIME32_5;

  Inc(Result, Len);
  while P + 4 <= PEnd do
  begin
    Inc(Result, PUInt32(P)^ * PRIME32_3);
    Result := RolDWord(Result, 17) * PRIME32_4;
    Inc(P, 4);
  end;
  while P < PEnd do
  begin
    Inc(Result, PByte(P)^ * PRIME32_5);
    Result := RolDWord(Result, 11) * PRIME32_1;
    Inc(P);
  end;
  Result := Result xor (Result shr 15); // inlined xxHash32Mixup()
  Result := Result * PRIME32_2;
  Result := Result xor (Result shr 13);
  Result := Result * PRIME32_3;
  Result := Result xor (Result shr 16);
end;

end.

