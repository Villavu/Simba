{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  FPC doesn't support "force inline" so these are "unrolled" color distance
  where color conversion and distance are all in one function.

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.colormath_distance_unrolled;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.mufasatypes, simba.colormath_distance;

type
  TColorDistanceFunc = function(const Color1: Pointer; const Color2: TColorBGRA; const mul: TChannelMultipliers): Single;

function DistanceRGB_UnRolled(const C1: PColorRGB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceHSL_UnRolled(const C1: PColorHSL; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceHSV_UnRolled(const C1: PColorHSV; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceXYZ_UnRolled(const C1: PColorXYZ; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceLAB_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceLCH_UnRolled(const C1: PColorLCH; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
function DistanceDeltaE_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;

implementation

uses
  simba.colormath_conversion;

function DistanceRGB_UnRolled(const C1: PColorRGB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((C1^.R-C2.R) * mul[0]) + Sqr((C1^.G-C2.G) * mul[1]) + Sqr((C1^.B-C2.B) * mul[2]));
end;

function DistanceHSL_UnRolled(const C1: PColorHSL; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  R,G,B,deltaC,deltaH,cMax,cMin: Single;
  Color1, Color2: TColorHSL;
begin
  Color1 := C1^;

  // function RGBToHSL
  R := C2.R / 255;
  G := C2.G / 255;
  B := C2.B / 255;

  cMin := Min(R, Min(G, B));
  cMax := Max(R, Max(G, B));

  deltaC := cMax - cMin;

  Color2.L := (cMax + cMin) * 0.5;
  if (deltaC = 0) then
  begin
    Color2.H := 0;
    Color2.S := 0;
  end else
  begin
    if (Color2.L < 0.5) then Color2.S := deltaC / (cMax + cMin)
    else                     Color2.S := deltaC / (2 - cMax - cMin);

    if     (R = cMax) then Color2.H := (    (G - B) / deltaC) * 60
    else if(G = cMax) then Color2.H := (2 + (B - R) / deltaC) * 60
    else{if(B = cMax) then}Color2.H := (4 + (R - G) / deltaC) * 60;

    if (Color2.H < 0) then Color2.H += 360;
  end;
  Color2.S *= 100;
  Color2.L *= 100;

  // function DistanceHSL
  if (Color1.S < 1.0e-10) or (Color2.S < 1.0e-10) then // no saturation = gray (hue has no value here)
    deltaH := 0
  else
  begin
    deltaH := Abs(Color1.H - Color2.H);
    if (deltaH >= 180) then
      deltaH := 360 - deltaH;
    deltaH *= Max(Color1.S, Color2.S) / 100;
  end;

  Result := Sqrt(Sqr(deltaH * mul[0]) + Sqr((Color1.S - Color2.S) * mul[1]) + Sqr((Color1.L - Color2.L) * mul[2]));
end;

function DistanceHSV_UnRolled(const C1: PColorHSV; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  R, G, B: Single;
  Chroma,t, k, deltaH: Single;
  Color1, Color2: TColorHSV;
begin
  Color1 := C1^;

  R := C2.R / 255;
  G := C2.G / 255;
  B := C2.B / 255;

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
  Color2.S := Chroma / (R + 1.0e-10)  * 100;
  if (Color2.S < 1.0e-10) then
    Color2.H := 0
  else
    Color2.H := Abs(K + (G - B) / (6.0 * Chroma + 1.0e-20)) * 360;
  Color2.V := R * 100;

  // function DistanceHSV
  if (Color1.S < 1.0e-10) or (Color2.S < 1.0e-10) then // no saturation = gray (hue has no value here)
    deltaH := 0
  else
  begin
    deltaH := Abs(Color1.H - Color2.H);
    if (deltaH >= 180) then
      deltaH := 360 - deltaH;
    deltaH *= Max(Color1.S, Color2.S) / 100;
  end;

  Result := Sqrt(Sqr(deltaH * mul[0]) + Sqr((Color1.S - Color2.S) * mul[1]) + Sqr((Color1.V - Color2.V) * mul[2]));
end;

function DistanceXYZ_UnRolled(const C1: PColorXYZ; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  vR,vG,vB: Single;
  Color1, Color2: TColorXYZ;
begin
  Color1 := C1^;

  // function RGBToXYZ
  with C2 do
  begin
    if R > 10 then vR := XYZ_POW_2_4[R]
    else           vR := (R / 255.0) / 12.92;
    if G > 10 then vG := XYZ_POW_2_4[G]
    else           vG := (G / 255.0) / 12.92;
    if B > 10 then vB := XYZ_POW_2_4[B]
    else           vB := (B / 255.0) / 12.92;
  end;

  vR := vR * 100;
  vG := vG * 100;
  vB := vB * 100;

  Color2.X := (vR * 0.4124 + vG * 0.3576 + vB * 0.1805);
  Color2.Y := (vR * 0.2126 + vG * 0.7152 + vB * 0.0722);
  Color2.Z := (vR * 0.0193 + vG * 0.1192 + vB * 0.9505);

  // function DistanceXYZ
  Result := Sqrt(Sqr((Color1.X - Color2.X) * mul[0]) + Sqr((Color1.Y - Color2.Y) * mul[1]) + Sqr((Color1.Z - Color2.Z) * mul[2]));
end;

function DistanceLAB_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  vR,vG,vB, X,Y,Z: Single;
  Color1, Color2: TColorLAB;
begin
  Color1 := C1^;

  // function RGBToLAB
  with C2 do
  begin
    if R > 10 then vR := XYZ_POW_2_4[R]
    else           vR := (R / 255.0) / 12.92;
    if G > 10 then vG := XYZ_POW_2_4[G]
    else           vG := (G / 255.0) / 12.92;
    if B > 10 then vB := XYZ_POW_2_4[B]
    else           vB := (B / 255.0) / 12.92;
  end;

  X := (vR * 0.4124 + vG * 0.3576 + vB * 0.1805);
  Y := (vR * 0.2126 + vG * 0.7152 + vB * 0.0722);
  Z := (vR * 0.0193 + vG * 0.1192 + vB * 0.9505);

  if X > 0.008856 then X := Power(X, ONE_DIV_THREE)
  else                 X := (7.787 * X) + 0.137931;
  if Y > 0.008856 then Y := Power(Y, ONE_DIV_THREE)
  else                 Y := (7.787 * Y) + 0.137931;
  if Z > 0.008856 then Z := Power(Z, ONE_DIV_THREE)
  else                 Z := (7.787 * Z) + 0.137931;

  Color2.L := (116.0 * Y) - 16.0;
  Color2.A := 500 * (X - Y);
  Color2.B := 200 * (Y - Z);

  // function DistanceLAB
  Result := Sqrt(Sqr((Color1.L - Color2.L) * mul[0]) + Sqr((Color1.A - Color2.A) * mul[1]) + Sqr((Color1.B - Color2.B) * mul[2]));
end;

function DistanceLCH_UnRolled(const C1: PColorLCH; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  vR,vG,vB, X,Y,Z, L,A,B, deltaH: Single;
  Color1, Color2: TColorLCH;
begin
  Color1 := C1^;

  // function RGBToLAB
  with C2 do
  begin
    if R > 10 then vR := XYZ_POW_2_4[R]
    else           vR := (R / 255.0) / 12.92;
    if G > 10 then vG := XYZ_POW_2_4[G]
    else           vG := (G / 255.0) / 12.92;
    if B > 10 then vB := XYZ_POW_2_4[B]
    else           vB := (B / 255.0) / 12.92;
  end;

  X := (vR * 0.4124 + vG * 0.3576 + vB * 0.1805);
  Y := (vR * 0.2126 + vG * 0.7152 + vB * 0.0722);
  Z := (vR * 0.0193 + vG * 0.1192 + vB * 0.9505);

  if X > 0.008856 then X := Power(X, ONE_DIV_THREE)
  else                 X := (7.787 * X) + 0.137931;
  if Y > 0.008856 then Y := Power(Y, ONE_DIV_THREE)
  else                 Y := (7.787 * Y) + 0.137931;
  if Z > 0.008856 then Z := Power(Z, ONE_DIV_THREE)
  else                 Z := (7.787 * Z) + 0.137931;

  L := (116.0 * Y) - 16.0;
  A := 500 * (X - Y);
  B := 200 * (Y - Z);

  // function RGBToLCH
  Color2.L := L;
  Color2.C := Sqrt(Sqr(A) + Sqr(B));
  Color2.H := ArcTan2(B, A);

  if (Color2.H > 0) then
    Color2.H := (Color2.H / PI) * 180
  else
    Color2.H := 360 - (Abs(Color2.H) / PI) * 180;

  // function DistanceLCH
  deltaH := Abs(Color1.H - Color2.H);
  if deltaH >= 180 then
    deltaH := 360 - deltaH;
  deltaH *= Max(Color1.C, Color2.C) / 100;

  if (Color1.C < 0.4) or (Color2.C < 0.4) then // no chromaticity = gray (hue has no value here)
    deltaH := 0
  else
  begin
    deltaH := Abs(Color1.H - Color2.H);
    if (deltaH >= 180) then
      deltaH := 360 - deltaH;
    deltaH *= Max(Color1.C, Color2.C) / 142;
  end;

  Result := Sqrt(Sqr((Color1.L - Color2.L) * mul[0]) + Sqr((Color1.C - Color2.C) * mul[1]) + Sqr(deltaH * mul[2]));
end;

function DistanceDeltaE_UnRolled(const C1: PColorLAB; const C2: TColorBGRA; const mul: TChannelMultipliers): Single;
var
  vR,vG,vB, X,Y,Z: Single;
  xC1,xC2,xDL,xDC,xDE,xDH,xSC,xSH: Single;
  Color1, Color2: TColorLAB;
begin
  Color1 := C1^;

  // function RGBToLAB
  with C2 do
  begin
    if R > 10 then vR := XYZ_POW_2_4[R]
    else           vR := (R / 255.0) / 12.92;
    if G > 10 then vG := XYZ_POW_2_4[G]
    else           vG := (G / 255.0) / 12.92;
    if B > 10 then vB := XYZ_POW_2_4[B]
    else           vB := (B / 255.0) / 12.92;
  end;

  X := (vR * 0.4124 + vG * 0.3576 + vB * 0.1805);
  Y := (vR * 0.2126 + vG * 0.7152 + vB * 0.0722);
  Z := (vR * 0.0193 + vG * 0.1192 + vB * 0.9505);

  if X > 0.008856 then X := Power(X, ONE_DIV_THREE)
  else                 X := (7.787 * X) + 0.137931;
  if Y > 0.008856 then Y := Power(Y, ONE_DIV_THREE)
  else                 Y := (7.787 * Y) + 0.137931;
  if Z > 0.008856 then Z := Power(Z, ONE_DIV_THREE)
  else                 Z := (7.787 * Z) + 0.137931;

  Color2.L := (116.0 * Y) - 16.0;
  Color2.A := 500 * (X - Y);
  Color2.B := 200 * (Y - Z);

  // function DistanceDeltaE
  xC1 := Sqrt(Sqr(Color1.A) + Sqr(Color1.B));
  xC2 := Sqrt(Sqr(Color2.A) + Sqr(Color2.B));
  xDL := Color2.L - Color1.L;
  xDC := xC2 - xC1;
  xDE := Sqrt(Sqr(Color1.L - Color2.L) + Sqr(Color1.A - Color2.A) + Sqr(Color1.B - Color2.B));

  if Sqrt(xDE) > Sqrt(Abs(xDL)) + Sqrt(Abs(xDC))  then
    xDH := Sqrt(Sqr(xDE) - Sqr(xDL) - Sqr(xDC))
  else
    xDH := 0;

  xSC := 1 + (0.045 * (xC1+xC2)/2);
  xSH := 1 + (0.015 * (xC1+xC2)/2);

  xDC /= xSC;
  xDH /= xSH;

  Result := Sqrt(Sqr(xDL * mul[0]) + Sqr(xDC * mul[1]) + Sqr(xDH * mul[2]));
end;

end.


