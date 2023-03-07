{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Calculate the distance between two colors different colorspaces.

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.colormath_distance;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes, simba.colormath_conversion;

function DistanceRGB(const Color1, Color2: ColorRGB; const mul: TChannelMultipliers): Single; inline;
function DistanceHSV(const Color1, Color2: ColorHSV; const mul: TChannelMultipliers): Single; inline;
function DistanceHSL(const Color1, Color2: ColorHSL; const mul: TChannelMultipliers): Single; inline;
function DistanceXYZ(const Color1, Color2: ColorXYZ; const mul: TChannelMultipliers): Single; inline;
function DistanceLAB(const Color1, Color2: ColorLAB; const mul: TChannelMultipliers): Single; inline;
function DistanceLCH(const Color1, Color2: ColorLCH; const mul: TChannelMultipliers): Single; inline;
function DistanceDeltaE(const Color1, Color2: ColorLAB; const  mul: TChannelMultipliers): Single; inline;

function DistanceRGB_Max(mul: TChannelMultipliers): Single; inline;
function DistanceHSV_Max(mul: TChannelMultipliers): Single; inline;
function DistanceHSL_Max(mul: TChannelMultipliers): Single; inline;
function DistanceXYZ_Max(mul: TChannelMultipliers): Single; inline;
function DistanceLAB_Max(mul: TChannelMultipliers): Single; inline;
function DistanceLCH_Max(mul: TChannelMultipliers): Single; inline;
function DistanceDeltaE_Max(mul: TChannelMultipliers): Single; inline;

// For finder usage
function _DistanceRGB(const Color1: PColorRGB; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;
function _DistanceHSV(const Color1: PColorHSV; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;
function _DistanceHSL(const Color1: PColorHSL; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;
function _DistanceXYZ(const Color1: PColorXYZ; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;
function _DistanceLAB(const Color1: PColorLAB; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;
function _DistanceLCH(const Color1: PColorLCH; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;
function _DistanceDeltaE(const Color1: PColorLAB;const Color2: ColorBGRA; const mul: TChannelMultipliers): Single; inline;

implementation

// ----| RGB |-----------------------------------------------------------------
function DistanceRGB(const Color1, Color2: ColorRGB; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((Color1.R-Color2.R) * mul[0]) + Sqr((Color1.G-Color2.G) * mul[1]) + Sqr((Color1.B-Color2.B) * mul[2]));
end;

function DistanceRGB_Max(mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(255 * mul[0]) + Sqr(255 * mul[1]) + Sqr(255 * mul[2]));
end;

// ----| HSV |-----------------------------------------------------------------
// Hue is weighted based on max saturation of the two colors:
// The "simple" solution causes a problem where two dark slightly saturated gray colors can have
// completely different hue's, causing the distance measure to be larger than what it should be.
function DistanceHSV(const Color1, Color2: ColorHSV; const mul: TChannelMultipliers): Single;
var
  deltaH: Single;
begin
  if (Color1.S < 1.0e-10) or (Color2.S < 1.0e-10) then // no saturation = gray (hue has no value here)
    deltaH := 0
  else begin
    deltaH := Abs(Color1.H - Color2.H);
    if deltaH >= 180 then deltaH := 360 - deltaH;
    deltaH *= Max(Color1.S, Color2.S) / 100;
  end;
  Result := Sqrt(Sqr(deltaH * mul[0]) + Sqr((Color1.S-Color2.S) * mul[1]) + Sqr((Color1.V-Color2.V) * mul[2]));
end;

function DistanceHSV_Max(mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(180 * mul[0]) + Sqr(100 * mul[1]) + Sqr(100 * mul[2]));
end;

// ----| HSL |-----------------------------------------------------------------
// Hue is weighted based on max saturation of the two colors:
// The "simple" solution causes a problem where two dark slightly saturated gray colors can have
// completely different hue's, causing the distance measure to be larger than what it should be.
function DistanceHSL(const Color1, Color2: ColorHSL; const mul: TChannelMultipliers): Single;
var
  deltaH: Single;
begin
  if (Color1.S < 1.0e-10) or (Color2.S < 1.0e-10) then // no saturation = gray (hue has no value here)
    deltaH := 0
  else begin
    deltaH := Abs(Color1.H - Color2.H);
    if deltaH >= 180 then deltaH := 360 - deltaH;
    deltaH *= Max(Color1.S, Color2.S) / 100;
  end;
  Result := Sqrt(Sqr(deltaH * mul[0]) + Sqr((Color1.S - Color2.S) * mul[1]) + Sqr((Color1.L - Color2.L) * mul[2]));
end;

function DistanceHSL_Max(mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(180 * mul[0]) + Sqr(100 * mul[1]) + Sqr(100 * mul[2]));
end;


// ----| XYZ |-----------------------------------------------------------------
function DistanceXYZ(const Color1, Color2: ColorXYZ; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((Color1.X-Color2.X) * mul[0]) + Sqr((Color1.Y-Color2.Y) * mul[1]) + Sqr((Color1.Z-Color2.Z) * mul[2]));
end;

function DistanceXYZ_Max(mul:TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(100 * mul[0]) + Sqr(100 * mul[1]) + Sqr(100 * mul[2]));
end;


// ----| LAB |-----------------------------------------------------------------
function DistanceLAB(const Color1, Color2: ColorLAB; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((Color1.L-Color2.L) * mul[0]) + Sqr((Color1.A-Color2.A) * mul[1]) + Sqr((Color1.B-Color2.B) * mul[2]));
end;

function DistanceLAB_Max(mul:TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(100 * mul[0]) + Sqr(200 * mul[1]) + Sqr(200 * mul[2]));
end;


// ----| LCH |-----------------------------------------------------------------
// Hue is weighted based on Chroma:
// The "simple" solution causes a problem where two dark slightly saturated gray colors can have
// completely different hue's, causing the distance measure to be larger than what it should be.
function DistanceLCH(const Color1, Color2: ColorLCH; const mul: TChannelMultipliers): Single;
var
  deltaH: Single;
begin
  deltaH := Abs(Color1.H - Color2.H);
  if deltaH >= 180 then deltaH := 360 - deltaH;
  deltaH *= Max(Color1.C, Color2.C) / 100;

  if (Color1.C < 0.4) or (Color2.C < 0.4) then // no chromaticity = gray (hue has no value here)
    deltaH := 0
  else begin
    deltaH := Abs(Color1.H - Color2.H);
    if deltaH >= 180 then deltaH := 360 - deltaH;
    deltaH *= Max(Color1.C, Color2.C) / 142;
  end;

  Result := Sqrt(Sqr((Color1.L-Color2.L) * mul[0]) + Sqr((Color1.C - Color2.C) * mul[1]) + Sqr(deltaH * mul[2]));
end;

function DistanceLCH_Max(mul:TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(100 * mul[0]) + Sqr(142 * mul[1]) + Sqr(180 * mul[2]));
end;


// ----| DeltaE |--------------------------------------------------------------
function DistanceDeltaE(const Color1, Color2: ColorLAB; const mul: TChannelMultipliers): Single;
var
  xc1,xc2,xdl,xdc,xde,xdh,xsc,xsh: Single;
begin
  xc1 := Sqrt(Sqr(Color1.a) + Sqr(Color1.b));
  xc2 := Sqrt(Sqr(Color2.a) + Sqr(Color2.b));
  xdl := Color2.L - Color1.L;
  xdc := xc2 - xc1;
  xde := Sqrt(Sqr(Color1.L - Color2.L) + Sqr(Color1.a - Color2.A) + Sqr(Color1.b - Color2.B));

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

function DistanceDeltaE_Max(mul:TChannelMultipliers): Single;
var
  Color1,Color2: ColorLAB;
  xc1,xc2,xdl,xdc,xde,xdh,xsc,xsh: Single;
begin
  Color1.L := 0;
  Color1.A := -92;
  Color1.B := -113;

  Color2.L := 100;
  Color2.A := 92;
  Color2.B := 92;

  xc1 := Sqrt(Sqr(Color1.a) + Sqr(Color1.b));
  xc2 := Sqrt(Sqr(Color2.a) + Sqr(Color2.b));
  xdl := Color2.L - Color1.L;
  xdc := xc2 - xc1;
  xde := Sqrt(Sqr(Color1.L - Color2.L) + Sqr(Color1.a - Color2.A) + Sqr(Color1.b - Color2.B));

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

function _DistanceRGB(const Color1: PColorRGB; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceRGB(Color1^, BGRAToRGB(Color2), Mul);
end;

function _DistanceHSV(const Color1: PColorHSV; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceHSV(Color1^, RGBToHSV(Color2.R, Color2.G, Color2.B), Mul);
end;

function _DistanceHSL(const Color1: PColorHSL; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceHSL(Color1^, RGBToHSL(Color2.R, Color2.G, Color2.B), Mul);
end;

function _DistanceXYZ(const Color1: PColorXYZ; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceXYZ(Color1^, RGBToXYZ(Color2.R, Color2.G, Color2.B), Mul);
end;

function _DistanceLAB(const Color1: PColorLAB; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceLAB(Color1^, RGBToLAB(Color2.R, Color2.G, Color2.B), Mul);
end;

function _DistanceLCH(const Color1: PColorLCH; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceLCH(Color1^, RGBToLCH(Color2.R, Color2.G, Color2.B), Mul);
end;

function _DistanceDeltaE(const Color1: PColorLAB; const Color2: ColorBGRA; const mul: TChannelMultipliers): Single;
begin
  Result := DistanceDeltaE(Color1^, RGBToLAB(Color2.R, Color2.G, Color2.B), Mul);
end;

end.

