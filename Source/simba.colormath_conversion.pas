{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
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
  ONE_DIV_THREE:     Single =  1.0 / 3.0;
  TWO_DIV_THREE:     Single =  2.0 / 3.0;
  NEG_ONE_DIV_THREE: Single = -1.0 / 3.0;

  // D65 white point, pre-inverted so XYZ normalisation is a multiply not a divide
  D65_INV: record X, Y, Z: Single; end = (
    X: 1.0 / 0.95047;
    Y: 1.0 / 1.00000;
    Z: 1.0 / 1.08883
  );

var
  // RGB byte (0..255) to its share of the Rec. 601 grey.
  // grey := Round(RED_TO_GREY[R] + GREEN_TO_GREY[G] + BLUE_TO_GREY[B])
  RED_TO_GREY: array[0..255] of Single;
  GREEN_TO_GREY: array[0..255] of Single;
  BLUE_TO_GREY: array[0..255] of Single;

  // RGB byte (0..255) to linear light (0..1), the sRGB transfer curve
  RGB_TO_LINEAR: array[0..255] of Single;

type
  TSimbaColorConversion = class
  public
    class function RGBToXYZ(const R,G,B: Byte): TColorXYZ; static;
    class function RGBToLAB(const R,G,B: Byte): TColorLAB; static;
    class function RGBToLCH(const R,G,B: Byte): TColorLCH; static;
    class function RGBToHSV(const R,G,B: Byte): TColorHSV; static;
    class function RGBToHSL(const R,G,B: Byte): TColorHSL; static;

    class function XYZToRGB(const X,Y,Z: Single): TColorRGB; static;
    class function LABToRGB(const L,A,B: Single): TColorRGB; static;
    class function LCHToRGB(const L,C,H: Single): TColorRGB; static;
    class function HSVToRGB(const H,S,V: Single): TColorRGB; static;
    class function HSLToRGB(const H,S,L: Single): TColorRGB; static;
  end;

function fcbrt(x: Single): Single; {$IFNDEF COLORMATH_ASM}inline;{$ENDIF}
function fast_atan2(y, x: Single): Single; inline;

implementation

function fcbrt(x: Single): Single; {$IFNDEF COLORMATH_ASM}inline;{$ENDIF}
{$IF DEFINED(COLORMATH_ASM)}
  {$I asm/fcbrt_x86_64.inc}
{$ELSE}
(*
 * 2-4x speedup over Power(x, 1/3) in LAB colorspace finding
 * Not a generalizable solution, but good for small numbers
 * For number beyond this, consider the less accurate (for small numbers)
 * https://pastebin.com/uRAwkm7s - Sun Microsystems (C) 1993
 *)
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

class function TSimbaColorConversion.RGBToXYZ(const R, G, B: Byte): TColorXYZ;
var
  vR,vG,vB: Single;
begin
  vR := RGB_TO_LINEAR[R];
  vG := RGB_TO_LINEAR[G];
  vB := RGB_TO_LINEAR[B];

  vR := vR * 100;
  vG := vG * 100;
  vB := vB * 100;

  // Illuminant = D65
  Result.X := (vR * Single(0.4124) + vG * Single(0.3576) + vB * Single(0.1805)) * D65_INV.X;
  Result.Y := (vR * Single(0.2126) + vG * Single(0.7152) + vB * Single(0.0722)) * D65_INV.Y;
  Result.Z := (vR * Single(0.0193) + vG * Single(0.1192) + vB * Single(0.9505)) * D65_INV.Z;
end;

class function TSimbaColorConversion.RGBToLAB(const R, G, B: Byte): TColorLAB;
var
  vR,vG,vB, X,Y,Z: Single;
begin
  vR := RGB_TO_LINEAR[R];
  vG := RGB_TO_LINEAR[G];
  vB := RGB_TO_LINEAR[B];

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

class function TSimbaColorConversion.RGBToLCH(const R, G, B: Byte): TColorLCH;
var
  LAB: TColorLAB;
begin
  LAB := RGBToLAB(R, G, B);
  Result.L := LAB.L;
  Result.C := Sqrt(Sqr(LAB.A) + Sqr(LAB.B));
  Result.H := fast_atan2(LAB.B, LAB.A);

  if (Result.H > 0) then
    Result.H := (Result.H / Single(PI)) * 180
  else
    Result.H := 360 - (Abs(Result.H) / Single(PI)) * 180;
end;

class function TSimbaColorConversion.RGBToHSV(const R, G, B: Byte): TColorHSV;
var
  Chroma,vR,vG,vB,K: Single;
begin
  vR := R * Single(1.0/255.0);
  vG := G * Single(1.0/255.0);
  vB := B * Single(1.0/255.0);
  K := 0.0;

  if (vG < vB) then
  begin
    Swap(vG, vB);
    K := -1.0;
  end;

  if (vR < vG) then
  begin
    Swap(vR, vG);
    K := NEG_ONE_DIV_THREE - K;
  end;

  Chroma := vR - Min(vG, vB);
  Result.S := Chroma / (vR + Single(1.0e-10)) * 100;
  if (Result.S < Single(1.0e-10)) then
    Result.H := 0
  else
    Result.H := Abs(K + (vG - vB) / (Single(6.0) * Chroma + Single(1.0e-20))) * 360;
  Result.V := vR * 100;
end;

(*
  Converts HSV to RGB
  Input:
    H values is in degrees [0..360]
    S and V values are percentages [0..100]

  Output:
    R,G,B is in range of [0..255]
*)
class function TSimbaColorConversion.HSVToRGB(const H, S, V: Single): TColorRGB;
var
  vH,vS,vV,i,f,p,q,t,vR,vG,vB: Single;
begin
  vH := H / 360;
  vS := S / 100;
  vV := V / 100;
  vR := 0;
  vG := 0;
  vB := 0;
  if (vS = 0.0) then
  begin
    Result.R := Trunc(vV * 255);
    Result.G := Trunc(vV * 255);
    Result.B := Trunc(vV * 255);
  end else
  begin
    i := Trunc(vH * 6);
    f := (vH * 6) - i;
    p := vV * (1 - vS);
    q := vV * (1 - vS * f);
    t := vV * (1 - vS * (1 - f));
    i := Modulo(i, 6);
    case Trunc(i) of
      0:begin
          vR := vV;
          vG := t;
          vB := p;
        end;
      1:begin
          vR := q;
          vG := vV;
          vB := p;
        end;
      2:begin
          vR := p;
          vG := vV;
          vB := t;
        end;
      3:begin
          vR := p;
          vG := q;
          vB := vV;
        end;
      4:begin
          vR := t;
          vG := p;
          vB := vV;
        end;
      5:begin
          vR := vV;
          vG := p;
          vB := q;
        end;
    end;

    Result.R := Trunc(vR * 255);
    Result.G := Trunc(vG * 255);
    Result.B := Trunc(vB * 255);
  end;
end;

(*
  Converts Color (RGB) to HSL

  Output:
    H value is in degrees [0..360]
    S and L values are percentages [0..100]
*)
class function TSimbaColorConversion.RGBToHSL(const R, G, B: Byte): TColorHSL;
var
  vR,vG,vB,deltaC,cMax,cMin: Single;
begin
  vR := R * Single(1.0/255.0);
  vG := G * Single(1.0/255.0);
  vB := B * Single(1.0/255.0);
  cMin := Min(vR,Min(vG,vB));
  cMax := Max(vR,Max(vG,vB));
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

    if     (vR = cMax) then Result.H := (    (vG - vB) / deltaC) * 60
    else if(vG = cMax) then Result.H := (2 + (vB - vR) / deltaC) * 60
    else{if(vB = cMax) then}Result.H := (4 + (vR - vG) / deltaC) * 60;

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

class function TSimbaColorConversion.HSLToRGB(const H, S, L: Single): TColorRGB;
var
  tmp,tmp2: Single;
  vH,vS,vL: Single;
begin
  if (S = 0) then
  begin
    Result.R := Round(L * 2.55);
    Result.G := Round(L * 2.55);
    Result.B := Round(L * 2.55);
  end else
  begin
    vH := H / 360;
    vS := S / 100;
    vL := L / 100;
    if (vL < 0.5) then tmp2 := (vL) * (1 + vS)
    else              tmp2 := (vL + vS) - (vS * vL);

    tmp := 2 * vL - tmp2;
    Result.R := Hue2RGB(tmp, tmp2, vH + ONE_DIV_THREE);
    Result.G := Hue2RGB(tmp, tmp2, vH);
    Result.B := Hue2RGB(tmp, tmp2, vH - ONE_DIV_THREE);
  end;
end;

(*
  Converts XYZ to RGB
  Input:
    X,Y,Z in range [0..100]
  Output:
    R,G,B is in range of [0..255]
*)
class function TSimbaColorConversion.XYZToRGB(const X, Y, Z: Single): TColorRGB;
var
  vR,vG,vB,vX,vY,vZ: Single;
begin
  vX := X / 100;
  vY := Y / 100;
  vZ := Z / 100;

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

class function TSimbaColorConversion.LABToRGB(const L, A, B: Single): TColorRGB;
var
  vX,vY,vZ,vX3,vY3,vZ3: Single;
begin
  vY := (L + 16) / 116;
  vX := A / 500 + vY;
  vZ := vY - B / 200;

  vX3 := vX*vX*vX;
  vY3 := vY*vY*vY;
  vZ3 := vZ*vZ*vZ;
  if (vX3 > 0.008856) then vX := vX3
  else                     vX := (vX - 16 / 116) / 7.787;
  if (vY3 > 0.008856) then vY := vY3
  else                     vY := (vY - 16 / 116) / 7.787;
  if (vZ3 > 0.008856) then vZ := vZ3
  else                     vZ := (vZ - 16 / 116) / 7.787;

  Result := XYZToRGB(vX * 95.047, vY * 100.0, vZ * 108.883);
end;

class function TSimbaColorConversion.LCHToRGB(const L, C, H: Single): TColorRGB;
begin
  Result := LABToRGB(L, Cos(DegToRad(H)) * C, Sin(DegToRad(H)) * C);
end;

procedure BuildColorTables;
var
  I: Integer;
  Channel: Double;
begin
  for I := 0 to 255 do
  begin
    RED_TO_GREY[I]   := I * 0.299;
    GREEN_TO_GREY[I] := I * 0.587;
    BLUE_TO_GREY[I]  := I * 0.114;

    Channel := I / 255;
    if (Channel <= 0.04045) then
      RGB_TO_LINEAR[I] := Channel / 12.92
    else
      RGB_TO_LINEAR[I] := Power((Channel + 0.055) / 1.055, 2.4);
  end;
end;

initialization
  BuildColorTables();

end.
