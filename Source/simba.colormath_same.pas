unit simba.colormath_same;

{$i simba.inc}

{$IFOPT D-}
  {$OPTIMIZATION LEVEL4}
{$ENDIF}

interface

uses
  Classes, SysUtils, math,
  simba.mufasatypes, simba.colormath;

type
  TSameColor = record
    GoalR, GoalG, GoalB: Integer;

    class function Create(Color: Integer): TSameColor; static;
    function IsSame(const Color: TRGB32): Boolean; inline;
  end;

  TSameColorCTS0 = record
    GoalR, GoalG, GoalB: Integer;
    GoalTolerance: Integer;

    class function Create(Color, Tolerance: Integer): TSameColorCTS0; static;
    function IsSame(const Color: TRGB32): Boolean; inline;
  end;

  TSameColorCTS1 = record
    GoalR, GoalG, GoalB: Integer;
    GoalTolerance: Integer;

    class function Create(Color, Tolerance: Integer): TSameColorCTS1; static;
    function IsSame(const Color: TRGB32): Boolean; inline;
  end;

  TSameColorCTS2 = record
    GoalH, GoalS, GoalL: Single;
    GoalHueMod, GoalSatMod: Single;
    GoalTolerance: Single;

    class function Create(Color, Tolerance: Integer; HueMod, SatMod: Single): TSameColorCTS2; static;
    function IsSame(const Color: TRGB32): Boolean; inline;
  end;

  TSameColorCTS3 = record
    GoalL, GoalA, GoalB: Single;
    GoalTolerance: Single;

    class function Create(Color, Tolerance: Integer; Modifier: Single): TSameColorCTS3; static;
    function IsSame(const Color: TRGB32): Boolean; inline;
  end;

// Needs to be global for inline to work
var
  Percentage: array[0..255] of Single;

implementation

class function TSameColor.Create(Color: Integer): TSameColor;
begin
  Result.GoalR := Color and $FF;
  Result.GoalG := Color shr 8 and $FF;
  Result.GoalB := Color shr 16 and $FF;
end;

function TSameColor.IsSame(const Color: TRGB32): Boolean;
begin
  Result := (Color.R = Self.GoalR) and (Color.G = Self.GoalG) and (Color.B = Self.GoalB);
end;

class function TSameColorCTS0.Create(Color, Tolerance: Integer): TSameColorCTS0;
begin
  Result.GoalR := Color and $FF;
  Result.GoalG := Color shr 8 and $FF;
  Result.GoalB := Color shr 16 and $FF;
  Result.GoalTolerance := Tolerance;
end;

function TSameColorCTS0.IsSame(const Color: TRGB32): Boolean;
begin
  Result := (Abs(Self.GoalR - Color.R) <= Self.GoalTolerance) and
            (Abs(Self.GoalG - Color.G) <= Self.GoalTolerance) and
            (Abs(Self.GoalB - Color.B) <= Self.GoalTolerance);
end;

class function TSameColorCTS1.Create(Color, Tolerance: Integer): TSameColorCTS1;
begin
  Result.GoalR := Color and $FF;
  Result.GoalG := Color shr 8 and $FF;
  Result.GoalB := Color shr 16 and $FF;
  Result.GoalTolerance := Sqr(Tolerance);
end;

function TSameColorCTS1.IsSame(const Color: TRGB32): Boolean;
begin
  Result := Sqr(Self.GoalR - Color.R) + Sqr(Self.GoalG - Color.G) + Sqr(Self.GoalB - Color.B) <= Self.GoalTolerance;
end;

class function TSameColorCTS2.Create(Color, Tolerance: Integer; HueMod, SatMod: Single): TSameColorCTS2;
begin
  RGBToHSL(
    Color and $FF,
    Color shr 8 and $FF,
    Color shr 16 and $FF,
    Result.GoalH,
    Result.GoalS,
    Result.GoalL
  );

  Result.GoalHueMod    := (Tolerance * HueMod);
  Result.GoalSatMod    := (Tolerance * SatMod) / 100;
  Result.GoalTolerance := (Tolerance / 100);

  Result.GoalL := Result.GoalL / 100;
  Result.GoalS := Result.GoalS / 100;
end;

function TSameColorCTS2.IsSame(const Color: TRGB32): Boolean;
var
  R, G, B: Single;
  H, S, L: Single;
  CMin, CMax, D: Single;
begin
  R := Percentage[Color.R];
  G := Percentage[Color.G];
  B := Percentage[Color.B];

  CMin := R;
  CMax := R;
  if G < CMin then CMin := G;
  if B < CMin then CMin := B;
  if G > CMax then CMax := G;
  if B > CMax then CMax := B;

  L := 0.5 * (CMax + CMin);

  if Abs(L-Self.GoalL) > Self.GoalTolerance then
    Exit(False);
  if (CMax = CMin) or (Self.GoalS = 0) then
    Exit(Self.GoalS <= Self.GoalSatMod);

  D := CMax - CMin;
  if (L < 0.5) then
    S := D / (CMax + CMin)
  else
    S := D / (2 - CMax - CMin);

  if Abs(S - Self.GoalS) > Self.GoalSatMod then
    Exit(False);

  if (R = CMax) then
    H := (G - B) / D
  else
  if (G = CMax) then
    H := 2 + (B - R) / D
  else
    H := 4 + (R - G) / D;

  H := H / 6;
  if (H < 0) then
    H := H + 1;
  H := H * 100;

  if (H > Self.GoalH) then
    Result := Min(H - Self.GoalH, Abs(H - (Self.GoalH + 100))) <= Self.GoalHueMod
  else
    Result := Min(Self.GoalH - H, Abs(Self.GoalH - (H + 100))) <= Self.GoalHueMod;
end;

class function TSameColorCTS3.Create(Color, Tolerance: Integer; Modifier: Single): TSameColorCTS3;
var
  X, Y, Z: Extended;
  L, A, B: Extended;
begin
  RGBToXYZ(
    Color and $FF,
    Color shr 8 and $FF,
    Color shr 16 and $FF,
    X, Y,  Z
  );
  XYZToCIELab(X, Y, Z, L, A, B);

  Result.GoalL := L;
  Result.GoalA := A;
  Result.GoalB := B;
  Result.GoalTolerance := Sqr(Tolerance * Modifier);
end;

function TSameColorCTS3.IsSame(const Color: TRGB32): Boolean;
var
  R, G, B: Single;
  X, Y, Z: Single;
  LL, AA, BB: Single;
begin
  R := Percentage[Color.R];
  G := Percentage[Color.G];
  B := Percentage[Color.B];

  if (R > 0.04045) then
    R := Power((R + 0.055) / 1.055, 2.4) * 100
  else
    R := R * 7.73993808;

  if (G > 0.04045) then
    G := Power((G + 0.055) / 1.055, 2.4) * 100
  else
    G := G * 7.73993808;

  if (B > 0.04045) then
    B := Power((B + 0.055) / 1.055, 2.4) * 100
  else
    B := B * 7.73993808;

  Y := (R * 0.2126 + G * 0.7152 + B * 0.0722) / 100.000;
  if (Y > 0.008856) then
    Y := Power(Y, 1.0/3.0)
  else
    Y := (7.787 * Y) + (16.0 / 116.0);

  X := (R * 0.4124 + G * 0.3576 + B * 0.1805) / 95.047;
  if (X > 0.008856 ) then
    X := Power(X, 1.0/3.0)
  else
    X := (7.787 * X) + (16.0 / 116.0);

  Z := (R * 0.0193 + G * 0.1192 + B * 0.9505) / 108.883;
  if (Z > 0.008856) then
    Z := Power(Z, 1.0/3.0)
  else
    Z := (7.787 * Z) + (16.0 / 116.0);

  LL := (116.0 * Y) - 16.0;
  AA := 500.0 * (X - Y);
  BB := 200.0 * (Y - Z);

  Result := (Sqr(LL - GoalL) + Sqr(AA - GoalA) + Sqr(BB - GoalB)) <= GoalTolerance;
end;

procedure LoadPercentages;
var
  I: Integer;
begin
  for I := 0 to 255 do
    Percentage[I] := I / 255;
end;

initialization
  LoadPercentages();

end.

