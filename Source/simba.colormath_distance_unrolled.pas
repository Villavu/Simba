{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  FPC doesn't support "force inline" so these are "unrolled" color distance
  where color conversion and distance are all in one function.

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.colormath_distance_unrolled;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.colormath,
  simba.colormath_conversion;

function DistanceRGB_UnRolled(const C1: PColorRGB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceHSL_UnRolled(const C1: PColorHSL; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceHSV_UnRolled(const C1: PColorHSV; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceXYZ_UnRolled(const C1: PColorXYZ; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceLAB_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceLCH_UnRolled(const C1: PColorLCH; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceDeltaE_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;

implementation

function DistanceRGB_UnRolled(const C1: PColorRGB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((ByteToSingle[C1^.R] - ByteToSingle[C2.R]) * mul[0])
               + Sqr((ByteToSingle[C1^.G] - ByteToSingle[C2.G]) * mul[1])
               + Sqr((ByteToSingle[C1^.B] - ByteToSingle[C2.B]) * mul[2]));
end;

function DistanceHSL_UnRolled(const C1: PColorHSL; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  R,G,B,deltaC,deltaH,cMax,cMin, H,S,L: Single;
begin
  // function RGBToHSL
  R := ByteToSingle[C2.R] * Single(1.0/255.0);
  G := ByteToSingle[C2.G] * Single(1.0/255.0);
  B := ByteToSingle[C2.B] * Single(1.0/255.0);

  cMin := Min(R, Min(G, B));
  cMax := Max(R, Max(G, B));
  deltaC := cMax - cMin;

  L := (cMax + cMin) * Single(0.5);
  if (deltaC = 0) then
  begin
    H := 0;
    S := 0;
  end else
  begin
    if (L < Single(0.5)) then S := deltaC / (cMax + cMin)
    else                      S := deltaC / (2 - cMax - cMin);

    if     (R = cMax) then H := (    (G - B) / deltaC) * 60
    else if(G = cMax) then H := (2 + (B - R) / deltaC) * 60
    else{if(B = cMax) then}H := (4 + (R - G) / deltaC) * 60;

    if (H < 0) then H += 360;
  end;
  S *= 100;
  L *= 100;

  // function DistanceHSL
  if (C1^.S < Single(1.0e-10)) or (S < Single(1.0e-10)) then
    deltaH := 0
  else
  begin
    deltaH := Abs(C1^.H - H);
    deltaH := Min(deltaH, 360 - deltaH);
    deltaH *= Max(C1^.S, S) / 100;
  end;

  Result := Sqrt(Sqr(deltaH * mul[0]) + Sqr((C1^.S - S) * mul[1]) + Sqr((C1^.L - L) * mul[2]));
end;

function DistanceHSV_UnRolled(const C1: PColorHSV; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  R, G, B: Single;
  Chroma,t, k, deltaH: Single;
  H, S, V: Single;
begin
  // function ColorToHSV
  R := ByteToSingle[C2.R] * Single(1.0/255.0);
  G := ByteToSingle[C2.G] * Single(1.0/255.0);
  B := ByteToSingle[C2.B] * Single(1.0/255.0);

  if (g < b) then
  begin
    t := b; b := g; g := t;
    k := -1.0;
  end else
    k := 0.0;

  if (r < g) then
  begin
    t := r; r := g; g := t;
    K := NEG_ONE_DIV_THREE - K;
  end;

  Chroma := R - Min(G, B);
  S := Chroma / (R + Single(1.0e-10)) * 100;
  if (S < Single(1.0e-10)) then
    H := 0
  else
    H := Abs(K + (G - B) / (Single(6.0) * Chroma + Single(1.0e-20))) * 360;
  V := R * 100;

  // function DistanceHSV
  if (C1^.S < Single(1.0e-10)) or (S < Single(1.0e-10)) then // no saturation = gray (hue has no value here)
    deltaH := 0
  else
  begin
    deltaH := Abs(C1^.H - H);
    if (deltaH >= 180) then
      deltaH := 360 - deltaH;
    deltaH *= Max(C1^.S, S) / 100;
  end;

  Result := Sqrt(Sqr(deltaH * mul[0]) + Sqr((C1^.S - S) * mul[1]) + Sqr((C1^.V - V) * mul[2]));
end;

function DistanceXYZ_UnRolled(const C1: PColorXYZ; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  linearR, linearG, linearB: Single;
  X, Y, Z: Single;
begin
  // function RGBToXYZ
  linearR := RGB_TO_LINEAR[C2.R];
  linearG := RGB_TO_LINEAR[C2.G];
  linearB := RGB_TO_LINEAR[C2.B];

  linearR := linearR * 100;
  linearG := linearG * 100;
  linearB := linearB * 100;

  X := (linearR * Single(0.4124) + linearG * Single(0.3576) + linearB * Single(0.1805)) * D65_INV.X;
  Y := (linearR * Single(0.2126) + linearG * Single(0.7152) + linearB * Single(0.0722)) * D65_INV.Y;
  Z := (linearR * Single(0.0193) + linearG * Single(0.1192) + linearB * Single(0.9505)) * D65_INV.Z;
  
  // function DistanceXY
  Result := Sqrt(Sqr((C1^.X - X) * mul[0]) + Sqr((C1^.Y - Y) * mul[1]) + Sqr((C1^.Z - Z) * mul[2]));
end;

function DistanceLAB_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  linearR, linearG, linearB: Single;
  X, Y, Z: Single;
  L, A, B: Single;
begin
  // function RGBToLAB
  linearR := RGB_TO_LINEAR[C2.R];
  linearG := RGB_TO_LINEAR[C2.G];
  linearB := RGB_TO_LINEAR[C2.B];

  X := (linearR * Single(0.4124) + linearG * Single(0.3576) + linearB * Single(0.1805)) * D65_INV.X;
  Y := (linearR * Single(0.2126) + linearG * Single(0.7152) + linearB * Single(0.0722)) * D65_INV.Y;
  Z := (linearR * Single(0.0193) + linearG * Single(0.1192) + linearB * Single(0.9505)) * D65_INV.Z;

  if X > Single(0.008856) then X := fcbrt(X)
  else                         X := (Single(7.787) * X) + Single(0.137931);
  if Y > Single(0.008856) then Y := fcbrt(Y)
  else                         Y := (Single(7.787) * Y) + Single(0.137931);
  if Z > Single(0.008856) then Z := fcbrt(Z)
  else                         Z := (Single(7.787) * Z) + Single(0.137931);

  L := (Single(116.0) * Y) - Single(16.0);
  A := 500 * (X - Y);
  B := 200 * (Y - Z);

  // function DistanceLAB
  Result := Sqrt(Sqr((C1^.L - L) * mul[0]) + Sqr((C1^.A - A) * mul[1]) + Sqr((C1^.B - B) * mul[2]));
end;

function DistanceLCH_UnRolled(const C1: PColorLCH; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  linearR, linearG, linearB: Single;
  X, Y, Z: Single;
  L, A, B: Single;
  chroma, hue, deltaHue: Single;
begin
  // function RGBToLAB
  linearR := RGB_TO_LINEAR[C2.R];
  linearG := RGB_TO_LINEAR[C2.G];
  linearB := RGB_TO_LINEAR[C2.B];

  X := (linearR * Single(0.4124) + linearG * Single(0.3576) + linearB * Single(0.1805)) * D65_INV.X;
  Y := (linearR * Single(0.2126) + linearG * Single(0.7152) + linearB * Single(0.0722)) * D65_INV.Y;
  Z := (linearR * Single(0.0193) + linearG * Single(0.1192) + linearB * Single(0.9505)) * D65_INV.Z;

  if X > Single(0.008856) then X := fcbrt(X)
  else                         X := (Single(7.787) * X) + Single(0.137931);
  if Y > Single(0.008856) then Y := fcbrt(Y)
  else                         Y := (Single(7.787) * Y) + Single(0.137931);
  if Z > Single(0.008856) then Z := fcbrt(Z)
  else                         Z := (Single(7.787) * Z) + Single(0.137931);

  L := (Single(116.0) * Y) - Single(16.0);
  A := 500 * (X - Y);
  B := 200 * (Y - Z);

  chroma := Sqrt(Sqr(A) + Sqr(B));
  // function DistanceLCH
  if (C1^.C < Single(0.4)) or (chroma < Single(0.4)) then // achromatic (gray): hue has no meaning
    deltaHue := 0
  else
  begin
    hue := fast_atan2(B, A);
    if (hue > 0) then
      hue := (hue / Single(PI)) * 180
    else
      hue := 360 - (Abs(hue) / Single(PI)) * 180;

    deltaHue := Abs(C1^.H - hue);
    deltaHue := Min(deltaHue, 360 - deltaHue);
    deltaHue *= Max(C1^.C, chroma) / 142;
  end;

  Result := Sqrt(Sqr((C1^.L - L) * mul[0]) + Sqr((C1^.C - chroma) * mul[1]) + Sqr(deltaHue * mul[2]));
end;

function DistanceDeltaE_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  linearR, linearG, linearB: Single;
  X, Y, Z: Single;
  L, A, B: Single;
  chroma1, chroma2: Single;
  deltaL, deltaChroma, deltaHue: Single;
  deltaESq, deltaHueSq: Single;
  chromaWeight, hueWeight: Single;
begin
  // function RGBToLAB
  linearR := RGB_TO_LINEAR[C2.R];
  linearG := RGB_TO_LINEAR[C2.G];
  linearB := RGB_TO_LINEAR[C2.B];

  X := (linearR * Single(0.4124) + linearG * Single(0.3576) + linearB * Single(0.1805)) * D65_INV.X;
  Y := (linearR * Single(0.2126) + linearG * Single(0.7152) + linearB * Single(0.0722)) * D65_INV.Y;
  Z := (linearR * Single(0.0193) + linearG * Single(0.1192) + linearB * Single(0.9505)) * D65_INV.Z;

  if X > Single(0.008856) then X := fcbrt(X)
  else                         X := (Single(7.787) * X) + Single(0.137931);
  if Y > Single(0.008856) then Y := fcbrt(Y)
  else                         Y := (Single(7.787) * Y) + Single(0.137931);
  if Z > Single(0.008856) then Z := fcbrt(Z)
  else                         Z := (Single(7.787) * Z) + Single(0.137931);

  L := (Single(116.0) * Y) - Single(16.0);
  A := 500 * (X - Y);
  B := 200 * (Y - Z);

  // function DistanceDeltaE
  chroma1 := Sqrt(Sqr(C1^.A) + Sqr(C1^.B));
  chroma2 := Sqrt(Sqr(A) + Sqr(B));
  deltaL := L - C1^.L;
  deltaChroma := chroma2 - chroma1;

  // deltaHue = sqrt(deltaE^2 - deltaL^2 - deltaChroma^2), clamped to avoid the sqrt of a negative
  deltaESq := Sqr(C1^.L - L) + Sqr(C1^.A - A) + Sqr(C1^.B - B);
  deltaHueSq := deltaESq - Sqr(deltaL) - Sqr(deltaChroma);
  if deltaHueSq > 0 then 
    deltaHue := Sqrt(deltaHueSq) else deltaHue := 0;
  chromaWeight := 1 + (Single(0.045) * (chroma1 + chroma2) / 2);
  hueWeight    := 1 + (Single(0.015) * (chroma1 + chroma2) / 2);
  deltaChroma /= chromaWeight;
  deltaHue    /= hueWeight;

  Result := Sqrt(Sqr(deltaL * mul[0]) + Sqr(deltaChroma * mul[1]) + Sqr(deltaHue * mul[2]));
end;

end.


