{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Color space converting, includes lots of code from: https://github.com/slackydev/colorlib

  note: TColor stuff should not include alpha
}
unit simba.colormath_conversion;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, Math,
  simba.base, simba.math, simba.colormath;

const
  // RGB byte (0..255) to linear light (0..1)
  RGB_TO_LINEAR: array[0..255] of Single = (
    0.000000, 0.000304, 0.000607, 0.000911, 0.001214, 0.001518, 0.001821, 0.002125, 0.002428, 0.002732, 0.003035, 0.003345, 0.003677, 0.004025, 0.004391, 0.004777, 0.005182, 0.005605, 0.006049, 0.006512, 0.006995, 0.007499, 0.008023, 0.008568, 0.009134, 0.009721, 0.010330, 0.010960, 0.011612, 0.012286, 0.012983, 0.013702, 0.014444, 0.015209, 0.015996, 0.016807, 0.017642, 0.018500, 0.019382, 0.020289, 0.021219, 0.022174, 0.023153, 0.024158, 0.025187, 0.026241, 0.027321, 0.028426, 0.029557, 0.030713, 0.031896, 0.033105,
    0.034340, 0.035601, 0.036889, 0.038204, 0.039546, 0.040915, 0.042311, 0.043735, 0.045186, 0.046665, 0.048172, 0.049707, 0.051269, 0.052861, 0.054480, 0.056128, 0.057805, 0.059511, 0.061246, 0.063010, 0.064803, 0.066626, 0.068478, 0.070360, 0.072272, 0.074214, 0.076185, 0.078187, 0.080220, 0.082283, 0.084376, 0.086500, 0.088656, 0.090842, 0.093059, 0.095307, 0.097587, 0.099899, 0.102242, 0.104616, 0.107023, 0.109462, 0.111932, 0.114435, 0.116971, 0.119538, 0.122139, 0.124772, 0.127438, 0.130136, 0.132868, 0.135633,
    0.138432, 0.141263, 0.144128, 0.147027, 0.149960, 0.152926, 0.155926, 0.158961, 0.162029, 0.165132, 0.168269, 0.171441, 0.174647, 0.177888, 0.181164, 0.184475, 0.187821, 0.191202, 0.194618, 0.198069, 0.201556, 0.205079, 0.208637, 0.212231, 0.215861, 0.219526, 0.223228, 0.226966, 0.230740, 0.234551, 0.238398, 0.242281, 0.246201, 0.250158, 0.254152, 0.258183, 0.262251, 0.266356, 0.270498, 0.274677, 0.278894, 0.283149, 0.287441, 0.291771, 0.296138, 0.300544, 0.304987, 0.309469, 0.313989, 0.318547, 0.323143, 0.327778,
    0.332452, 0.337164, 0.341914, 0.346704, 0.351533, 0.356400, 0.361307, 0.366253, 0.371238, 0.376262, 0.381326, 0.386429, 0.391572, 0.396755, 0.401978, 0.407240, 0.412543, 0.417885, 0.423268, 0.428690, 0.434154, 0.439657, 0.445201, 0.450786, 0.456411, 0.462077, 0.467784, 0.473531, 0.479320, 0.485150, 0.491021, 0.496933, 0.502886, 0.508881, 0.514918, 0.520996, 0.527115, 0.533276, 0.539479, 0.545724, 0.552011, 0.558340, 0.564712, 0.571125, 0.577580, 0.584078, 0.590619, 0.597202, 0.603827, 0.610496, 0.617207, 0.623960,
    0.630757, 0.637597, 0.644480, 0.651406, 0.658375, 0.665387, 0.672443, 0.679542, 0.686685, 0.693872, 0.701102, 0.708376, 0.715694, 0.723055, 0.730461, 0.737910, 0.745404, 0.752942, 0.760525, 0.768151, 0.775822, 0.783538, 0.791298, 0.799103, 0.806952, 0.814847, 0.822786, 0.830770, 0.838799, 0.846873, 0.854993, 0.863157, 0.871367, 0.879622, 0.887923, 0.896269, 0.904661, 0.913099, 0.921582, 0.930111, 0.938686, 0.947307, 0.955973, 0.964686, 0.973445, 0.982251, 0.991102, 1.000000
  );

  ONE_DIV_THREE:     Single =  1.0 / 3.0;
  TWO_DIV_THREE:     Single =  2.0 / 3.0;
  NEG_ONE_DIV_THREE: Single = -1.0 / 3.0;

  // D65 white point, pre-inverted so XYZ normalisation is a multiply not a divide
  D65_INV: record X, Y, Z: Single; end = (
    X: 1.0 / 0.95047;
    Y: 1.0 / 1.00000;
    Z: 1.0 / 1.08883
  );

type
  TSimbaColorConversion = class
  public
    class function ColorToBGRA(const Color: TColor; const Alpha: Byte = 0): TColorBGRA; static; inline;
    class function ColorToRGB(const Color: TColor): TColorRGB; static; inline;

    class function BGRAToColor(const BGRA: TColorBGRA): TColor; static; inline;
    class function BGRAToRGB(const RGB: TColorBGRA): TColorRGB; static; inline;

    class function RGBToColor(const RGB: TColorRGB): TColor; static; inline;
    class function RGBToXYZ(const RGB: TColorRGB): TColorXYZ; static;
    class function RGBToLAB(const RGB: TColorRGB): TColorLAB; static;
    class function RGBToLCH(const RGB: TColorRGB): TColorLCH; static;
    class function RGBToHSV(const RGB: TColorRGB): TColorHSV; static;
    class function RGBToHSL(const RGB: TColorRGB): TColorHSL; static;

    class function LABToLCH(const LAB: TColorLAB): TColorLCH; static;
    class function LABToRGB(const LAB: TColorLAB): TColorRGB; static;
    class function LABToXYZ(const LAB: TColorLAB): TColorXYZ; static;

    class function HSVToRGB(const HSV: TColorHSV): TColorRGB; static;
    class function HSLToRGB(const HSL: TColorHSL): TColorRGB; static;
    class function LCHToRGB(const LCH: TColorLCH): TColorRGB; static;
    class function LCHToLAB(const LCH: TColorLCH): TColorLAB; static;

    class function XYZToRGB(const XYZ: TColorXYZ): TColorRGB; static;
  end;

function fcbrt(x: Single): Single; {$IFNDEF CPUX86_64}inline;{$ENDIF}
function fast_atan2(y, x: Single): Single; inline;

implementation

{$IFDEF CPUX86_64}
function fcbrt(x: Single): Single; assembler; nostackframe;  // x and Result in xmm0 (win64 + SysV)
asm
  movd   eax, xmm0             // seed: y = bits(4/3 * 127*2^23) - bits(x)/3
  mov    ecx, eax
  mov    eax, $AAAAAAAB        // bits div 3  via  (bits * 0xAAAAAAAB) >> 33
  mul    ecx
  shr    edx, 1
  mov    eax, 1419910245
  sub    eax, edx
  movd   xmm1, eax             // xmm1 = y0
  movss  xmm4, dword ptr [rip+ONE_DIV_THREE]   // xmm4 = 1/3
  mov    eax, $40800000        // xmm5 = 4.0f, materialised inline (no data const)
  movd   xmm5, eax
  movaps xmm2, xmm1            // Newton step 1:  y := y * (4 - x*y^3) / 3
  mulss  xmm2, xmm1
  mulss  xmm2, xmm1
  mulss  xmm2, xmm0
  movaps xmm3, xmm5            // 4.0
  subss  xmm3, xmm2
  mulss  xmm1, xmm3
  mulss  xmm1, xmm4
  movaps xmm2, xmm1            // Newton step 2
  mulss  xmm2, xmm1
  mulss  xmm2, xmm1
  mulss  xmm2, xmm0
  movaps xmm3, xmm5            // 4.0
  subss  xmm3, xmm2
  mulss  xmm1, xmm3
  mulss  xmm1, xmm4
  mulss  xmm1, xmm1            // cbrt = x * y^2
  mulss  xmm0, xmm1
end;
{$ELSE}
(*
 * 2-4x speedup over Power(x, 1/3) in LAB colorspace finding
 * Not a generalizable solution, but good for small numbers
 * For number beyond this, consider the less accurate (for small numbers)
 * https://pastebin.com/uRAwkm7s - Sun Microsystems (C) 1993
 *)
function fcbrt(x: Single): Single; inline;
begin
  Result := Sqrt(x);
  Result := (2*Result + x/Sqr(Result)) * Single(1.0/3.0);
  Result := (2*Result + x/Sqr(Result)) * Single(1.0/3.0);
  Result := (2*Result + x/Sqr(Result)) * Single(1.0/3.0);
end;
{$ENDIF}

// Fast single-precision atan2, max error ~1.2e-5 rad (< 0.001 deg)
function fast_atan2(y, x: Single): Single; inline;
var
  ax, ay, z, z2, a: Single;
begin
  ax := Abs(x);  ay := Abs(y);
  if ax >= ay then z := ay / (ax + Single(1.0e-20))
  else             z := ax / (ay + Single(1.0e-20));
  z2 := z * z;
  a := z * (Single(0.9998660) + z2 * (Single(-0.3302995) + z2 * (Single(0.1801410) + z2 * (Single(-0.0851330) + z2 * Single(0.0208351)))));
  if ay > ax then a := Single(PI / 2) - a;
  if x < 0   then a := Single(PI) - a;
  if y < 0   then a := -a;
  Result := a;
end;

class function TSimbaColorConversion.ColorToBGRA(const Color: TColor; const Alpha: Byte): TColorBGRA;
begin
  Result.R := (Color and R_MASK) shr R_BIT;
  Result.G := (Color and G_MASK) shr G_BIT;
  Result.B := (Color and B_MASK) shr B_BIT;
  Result.A := Alpha;
end;

class function TSimbaColorConversion.BGRAToColor(const BGRA: TColorBGRA): TColor;
begin
  Result := TColor(BGRA.R or BGRA.G shl G_BIT or BGRA.B shl B_BIT);
end;

class function TSimbaColorConversion.ColorToRGB(const Color: TColor): TColorRGB;
begin
  Result.R := Color shr R_BIT and $FF;
  Result.G := Color shr G_BIT and $FF;
  Result.B := Color shr B_BIT and $FF;
end;

class function TSimbaColorConversion.RGBToColor(const RGB: TColorRGB): TColor;
begin
  Result := TColor(RGB.R or RGB.G shl G_BIT or RGB.B shl B_BIT);
end;

class function TSimbaColorConversion.BGRAToRGB(const RGB: TColorBGRA): TColorRGB;
begin
  Result.R := RGB.R;
  Result.G := RGB.G;
  Result.B := RGB.B;
end;

class function TSimbaColorConversion.RGBToXYZ(const RGB: TColorRGB): TColorXYZ;
var
  vR,vG,vB: Single;
begin
  vR := RGB_TO_LINEAR[RGB.R];
  vG := RGB_TO_LINEAR[RGB.G];
  vB := RGB_TO_LINEAR[RGB.B];

  vR := vR * 100;
  vG := vG * 100;
  vB := vB * 100;

  // Illuminant = D65
  Result.X := (vR * Single(0.4124) + vG * Single(0.3576) + vB * Single(0.1805)) * D65_INV.X;
  Result.Y := (vR * Single(0.2126) + vG * Single(0.7152) + vB * Single(0.0722)) * D65_INV.Y;
  Result.Z := (vR * Single(0.0193) + vG * Single(0.1192) + vB * Single(0.9505)) * D65_INV.Z;
end;

class function TSimbaColorConversion.RGBToLAB(const RGB: TColorRGB): TColorLAB;
var
  vR,vG,vB, X,Y,Z: Single;
begin
  vR := RGB_TO_LINEAR[RGB.R];
  vG := RGB_TO_LINEAR[RGB.G];
  vB := RGB_TO_LINEAR[RGB.B];

  // Illuminant = D65 & Normalize D65
  X := (vR * Single(0.4124) + vG * Single(0.3576) + vB * Single(0.1805)) * D65_INV.X;
  Y := (vR * Single(0.2126) + vG * Single(0.7152) + vB * Single(0.0722)) * D65_INV.Y;
  Z := (vR * Single(0.0193) + vG * Single(0.1192) + vB * Single(0.9505)) * D65_INV.Z;

  // XYZ To LAB
  if X > Single(0.008856) then X := fcbrt(X)
  else                         X := (Single(7.787) * X) + Single(0.137931);
  if Y > Single(0.008856) then Y := fcbrt(Y)
  else                         Y := (Single(7.787) * Y) + Single(0.137931);
  if Z > Single(0.008856) then Z := fcbrt(Z)
  else                         Z := (Single(7.787) * Z) + Single(0.137931);

  Result.L := (Single(116.0) * Y) - Single(16.0);
  Result.A := 500 * (X - Y);
  Result.B := 200 * (Y - Z);
end;

class function TSimbaColorConversion.RGBToLCH(const RGB: TColorRGB): TColorLCH;
var
  LAB: TColorLAB;
begin
  LAB := RGBToLab(RGB);
  Result.L := LAB.L;
  Result.C := Sqrt(Sqr(LAB.A) + Sqr(LAB.B));
  Result.H := fast_atan2(LAB.B, LAB.A);

  if (Result.H > 0) then
    Result.H := (Result.H / Single(PI)) * 180
  else
    Result.H := 360 - (Abs(Result.H) / Single(PI)) * 180;
end;

class function TSimbaColorConversion.RGBToHSV(const RGB: TColorRGB): TColorHSV;
var
  Chroma,R,G,B,K: Single;
begin
  R := RGB.R * Single(1.0/255.0);
  G := RGB.G * Single(1.0/255.0);
  B := RGB.B * Single(1.0/255.0);
  K := 0.0;

  if (G < b) then
  begin
    Swap(G, B);
    K := -1.0;
  end;

  if (R < G) then
  begin
    Swap(R, G);
    K := NEG_ONE_DIV_THREE - K;
  end;

  Chroma := R - Min(G, B);
  Result.S := Chroma / (R + Single(1.0e-10)) * 100;
  if (Result.S < Single(1.0e-10)) then
    Result.H := 0
  else
    Result.H := Abs(K + (G - B) / (Single(6.0) * Chroma + Single(1.0e-20))) * 360;
  Result.V := R * 100;
end;

(*
  Converts HSV to RGB
  Input:
    H values is in degrees [0..360]
    S and V values are percentages [0..100]

  Output:
    R,G,B is in range of [0..255]
*)
class function TSimbaColorConversion.HSVToRGB(const HSV: TColorHSV): TColorRGB;
var
  h,s,v,i,f,p,q,t,R,G,B: Single;
begin
  H := HSV.H / 360;
  S := HSV.S / 100;
  V := HSV.V / 100;
  R := 0; G := 0; B := 0;
  if (S = 0.0) then
  begin
    Result.R := Trunc(V * 255);
    Result.G := Trunc(V * 255);
    Result.B := Trunc(V * 255);
  end else
  begin
    i := Trunc(H * 6);
    f := (H * 6) - i;
    p := V * (1 - S);
    q := V * (1 - S * f);
    t := V * (1 - S * (1 - f));
    i := Modulo(i, 6);
    case Trunc(i) of
      0:begin
          R := v;
          G := t;
          B := p;
        end;
      1:begin
          R := q;
          G := v;
          B := p;
        end;
      2:begin
          R := p;
          G := v;
          B := t;
        end;
      3:begin
          R := p;
          G := q;
          B := v;
        end;
      4:begin
          R := t;
          G := p;
          B := v;
        end;
      5:begin
          R := v;
          G := p;
          B := q;
        end;
    end;

    Result.R := Trunc(R * 255);
    Result.G := Trunc(G * 255);
    Result.B := Trunc(B * 255);
  end;
end;

(*
  Converts Color (RGB) to HSL

  Output:
    H value is in degrees [0..360]
    S and L values are percentages [0..100]
*)
class function TSimbaColorConversion.RGBToHSL(const RGB: TColorRGB): TColorHSL;
var
  R,G,B,deltaC,cMax,cMin: Single;
begin
  R := RGB.R * Single(1.0/255.0);
  G := RGB.G * Single(1.0/255.0);
  B := RGB.B * Single(1.0/255.0);
  cMin := Min(R,Min(G,B));
  cMax := Max(R,Max(G,B));
  deltaC := cMax - cMin;

  Result.L := (cMax + cMin) * Single(0.5);
  if deltaC = 0 then
  begin
    Result.H := 0;
    Result.S := 0;
  end else
  begin
    if Result.L < Single(0.5) then Result.S := deltaC / (cMax + cMin)
    else                           Result.S := deltaC / (2 - cMax - cMin);

    if     (R = cMax) then Result.H := (    (G - B) / deltaC) * 60
    else if(G = cMax) then Result.H := (2 + (B - R) / deltaC) * 60
    else{if(B = cMax) then}Result.H := (4 + (R - G) / deltaC) * 60;

    if(Result.H < 0) then Result.H += 360;
  end;
  Result.S *= 100;
  Result.L *= 100;
end;

(*
  Converts HSL to RGB
  Input:
    H values is in degrees [0..360]
    S and L values are percentages [0..100]

  Output:
    R,G,B is in range of [0..255]
*)
function Hue2RGB(v1, v2, vH: Single): Byte; inline;
begin
  if (vH < 0) then vH += 1;
  if (vH > 1) then vH -= 1;
  if (6 * vH < 1) then Exit(Round(255 * (v1 + (v2 - v1) * 6 * vH)));
  if (2 * vH < 1) then Exit(Round(255 * v2));
  if (3 * vH < 2) then Exit(Round(255 * (v1 + (v2 - v1) * (TWO_DIV_THREE - vH) * 6)));
  Result := Round(255 * v1);
end;

class function TSimbaColorConversion.HSLToRGB(const HSL: TColorHSL): TColorRGB;
var
  tmp,tmp2: Single;
  H,S,L: Single;
begin
  if (HSL.S = 0) then
  begin
    Result.R := Round(HSL.L * 2.55);
    Result.G := Round(HSL.L * 2.55);
    Result.B := Round(HSL.L * 2.55);
  end else
  begin
    H := HSL.H / 360;
    S := HSL.S / 100;
    L := HSL.L / 100;
    if (L < 0.5) then tmp2 := (L) * (1 + S)
    else              tmp2 := (L + S) - (S * L);

    tmp := 2 * L - tmp2;
    Result.R := Hue2RGB(tmp, tmp2, H + ONE_DIV_THREE);
    Result.G := Hue2RGB(tmp, tmp2, H);
    Result.B := Hue2RGB(tmp, tmp2, H - ONE_DIV_THREE);
  end;
end;

(*
  Converts XYZ to RGB
  Input:
    X,Y,Z in range [0..100]
  Output:
    R,G,B is in range of [0..255]
*)
class function TSimbaColorConversion.XYZToRGB(const XYZ: TColorXYZ): TColorRGB;
var
  vR,vG,vB,vX,vY,vZ: Single;
begin
  vX := XYZ.X / 100;
  vY := XYZ.Y / 100;
  vZ := XYZ.Z / 100;

  vR := vX *  3.2406 + vY * -1.5372 + vZ * -0.4986;
  vG := vX * -0.9689 + vY *  1.8758 + vZ *  0.0415;
  vB := vX *  0.0557 + vY * -0.2040 + vZ *  1.0570;

  if (vR > 0.0031308) then vR := 1.055 * Power(vR, 1/2.4) - 0.055
  else                     vR := 12.92 * vR;
  if (vG > 0.0031308) then vG := 1.055 * Power(vG, 1/2.4) - 0.055
  else                     vG := 12.92 * vG;
  if (vB > 0.0031308) then vB := 1.055 * Power(vB, 1/2.4) - 0.055
  else                     vB := 12.92 * vB;

  Result.R := Round(Min(255, Max(0, vR * 255)));
  Result.G := Round(Min(255, Max(0, vG * 255)));
  Result.B := Round(Min(255, Max(0, vB * 255)));
end;

class function TSimbaColorConversion.LABToXYZ(const LAB: TColorLAB): TColorXYZ;
var
  vX,vY,vZ,vX3,vY3,vZ3: Single;
begin
  vY := (LAB.L + 16) / 116;
  vX := LAB.A / 500 + vY;
  vZ := vY - LAB.B / 200;

  vX3 := vX*vX*vX;
  vY3 := vY*vY*vY;
  vZ3 := vZ*vZ*vZ;
  if (vX3 > 0.008856) then vX := vX3
  else                     vX := (vX - 16 / 116) / 7.787;
  if (vY3 > 0.008856) then vY := vY3
  else                     vY := (vY - 16 / 116) / 7.787;
  if (vZ3 > 0.008856) then vZ := vZ3
  else                     vZ := (vZ - 16 / 116) / 7.787;

  Result.X := vX * 95.047;
  Result.Y := vY * 100.0;
  Result.Z := vZ * 108.883;
end;

class function TSimbaColorConversion.LABToRGB(const LAB: TColorLAB): TColorRGB;
begin
  Result := XYZToRGB(LABToXYZ(LAB));
end;

class function TSimbaColorConversion.LABToLCH(const LAB: TColorLAB): TColorLCH;
begin
  Result.L := LAB.L;
  Result.C := Sqrt(Sqr(LAB.A) + Sqr(LAB.B));
  Result.H := fast_atan2(LAB.B, LAB.A);

  if (Result.H > 0) then
    Result.H := (Result.H / Single(PI)) * 180
  else
    Result.H := 360 - (Abs(Result.H) / Single(PI)) * 180;
end;

class function TSimbaColorConversion.LCHToLAB(const LCH: TColorLCH): TColorLAB;
begin
  Result.L := LCH.L;
  Result.A := Cos(DegToRad(LCH.H)) * LCH.C;
  Result.B := Sin(DegToRad(LCH.H)) * LCH.C;
end;

class function TSimbaColorConversion.LCHToRGB(const LCH: TColorLCH): TColorRGB;
begin
  Result := LABToRGB(LCHToLAB(LCH));
end;

end.

