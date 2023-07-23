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
  simba.mufasatypes, simba.colormath;

type
  PChannelMultipliers = ^TChannelMultipliers;
  TChannelMultipliers = array [0..2] of Single;

const
  DefaultMultipliers: TChannelMultipliers = (1, 1, 1);
  DefaultColorSpace = EColorSpace.RGB;

function DistanceRGB(const Color1, Color2: TColorRGB; const mul: TChannelMultipliers): Single; inline;
function DistanceHSV(const Color1, Color2: TColorHSV; const mul: TChannelMultipliers): Single; inline;
function DistanceHSL(const Color1, Color2: TColorHSL; const mul: TChannelMultipliers): Single; inline;
function DistanceXYZ(const Color1, Color2: TColorXYZ; const mul: TChannelMultipliers): Single; inline;
function DistanceLAB(const Color1, Color2: TColorLAB; const mul: TChannelMultipliers): Single; inline;
function DistanceLCH(const Color1, Color2: TColorLCH; const mul: TChannelMultipliers): Single; inline;
function DistanceDeltaE(const Color1, Color2: TColorLAB; const  mul: TChannelMultipliers): Single; inline;

function DistanceRGB_Max(const mul: TChannelMultipliers): Single; inline;
function DistanceHSV_Max(const mul: TChannelMultipliers): Single; inline;
function DistanceHSL_Max(const mul: TChannelMultipliers): Single; inline;
function DistanceXYZ_Max(const mul: TChannelMultipliers): Single; inline;
function DistanceLAB_Max(const mul: TChannelMultipliers): Single; inline;
function DistanceLCH_Max(const mul: TChannelMultipliers): Single; inline;
function DistanceDeltaE_Max(const mul: TChannelMultipliers): Single; inline;

function ColorDistance(const Color1, Color2: TColor; const ColorSpace: EColorSpace; const Multipliers: TChannelMultipliers): Single;

function SimilarColors(const Color1, Color2: TColor; const Tolerance: Single): Boolean; overload;
function SimilarColors(const Color1, Color2: TColor; const Tolerance: Single; ColorSpace: EColorSpace; const Multipliers: TChannelMultipliers): Boolean; overload;

implementation

// ----| RGB |-----------------------------------------------------------------
function DistanceRGB(const Color1, Color2: TColorRGB; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((Color1.R-Color2.R) * mul[0]) + Sqr((Color1.G-Color2.G) * mul[1]) + Sqr((Color1.B-Color2.B) * mul[2]));
end;

function DistanceRGB_Max(const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(255 * mul[0]) + Sqr(255 * mul[1]) + Sqr(255 * mul[2]));
end;

// ----| HSV |-----------------------------------------------------------------
// Hue is weighted based on max saturation of the two colors:
// The "simple" solution causes a problem where two dark slightly saturated gray colors can have
// completely different hue's, causing the distance measure to be larger than what it should be.
function DistanceHSV(const Color1, Color2: TColorHSV; const mul: TChannelMultipliers): Single;
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

function DistanceHSV_Max(const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(180 * mul[0]) + Sqr(100 * mul[1]) + Sqr(100 * mul[2]));
end;

// ----| HSL |-----------------------------------------------------------------
// Hue is weighted based on max saturation of the two colors:
// The "simple" solution causes a problem where two dark slightly saturated gray colors can have
// completely different hue's, causing the distance measure to be larger than what it should be.
function DistanceHSL(const Color1, Color2: TColorHSL; const mul: TChannelMultipliers): Single;
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

function DistanceHSL_Max(const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(180 * mul[0]) + Sqr(100 * mul[1]) + Sqr(100 * mul[2]));
end;


// ----| XYZ |-----------------------------------------------------------------
function DistanceXYZ(const Color1, Color2: TColorXYZ; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((Color1.X-Color2.X) * mul[0]) + Sqr((Color1.Y-Color2.Y) * mul[1]) + Sqr((Color1.Z-Color2.Z) * mul[2]));
end;

function DistanceXYZ_Max(const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(100 * mul[0]) + Sqr(100 * mul[1]) + Sqr(100 * mul[2]));
end;


// ----| LAB |-----------------------------------------------------------------
function DistanceLAB(const Color1, Color2: TColorLAB; const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr((Color1.L-Color2.L) * mul[0]) + Sqr((Color1.A-Color2.A) * mul[1]) + Sqr((Color1.B-Color2.B) * mul[2]));
end;

function DistanceLAB_Max(const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(100 * mul[0]) + Sqr(200 * mul[1]) + Sqr(200 * mul[2]));
end;


// ----| LCH |-----------------------------------------------------------------
// Hue is weighted based on Chroma:
// The "simple" solution causes a problem where two dark slightly saturated gray colors can have
// completely different hue's, causing the distance measure to be larger than what it should be.
function DistanceLCH(const Color1, Color2: TColorLCH; const mul: TChannelMultipliers): Single;
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

function DistanceLCH_Max(const mul: TChannelMultipliers): Single;
begin
  Result := Sqrt(Sqr(100 * mul[0]) + Sqr(142 * mul[1]) + Sqr(180 * mul[2]));
end;


// ----| DeltaE |--------------------------------------------------------------
function DistanceDeltaE(const Color1, Color2: TColorLAB; const mul: TChannelMultipliers): Single;
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

function DistanceDeltaE_Max(const mul: TChannelMultipliers): Single;
var
  Color1,Color2: TColorLAB;
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

function ColorDistance(const Color1, Color2: TColor; const ColorSpace: EColorSpace; const Multipliers: TChannelMultipliers): Single;
begin
  case ColorSpace of
    EColorSpace.RGB:    Result := DistanceRGB(Color1.ToRGB(), Color2.ToRGB(), Multipliers) / DistanceRGB_Max(Multipliers) * 100;
    EColorSpace.HSV:    Result := DistanceHSV(Color1.ToHSV(), Color2.ToHSV(), Multipliers) / DistanceHSV_Max(Multipliers) * 100;
    EColorSpace.HSL:    Result := DistanceHSL(Color1.ToHSL(), Color2.ToHSL(), Multipliers) / DistanceHSL_Max(Multipliers) * 100;
    EColorSpace.XYZ:    Result := DistanceXYZ(Color1.ToXYZ(), Color2.ToXYZ(), Multipliers) / DistanceXYZ_Max(Multipliers) * 100;
    EColorSpace.LCH:    Result := DistanceLCH(Color1.ToLCH(), Color2.ToLCH(), Multipliers) / DistanceLCH_Max(Multipliers) * 100;
    EColorSpace.LAB:    Result := DistanceLAB(Color1.ToLAB(), Color2.ToLAB(), Multipliers) / DistanceLAB_Max(Multipliers) * 100;
    EColorSpace.DeltaE: Result := DistanceDeltaE(Color1.ToLAB(), Color2.ToLAB(), Multipliers) / DistanceDeltaE_Max(Multipliers) * 100;
  end;
end;

function SimilarColors(const Color1, Color2: TColor; const Tolerance: Single): Boolean;
begin
  Result := ColorDistance(Color1, Color2, EColorSpace.RGB, DefaultMultipliers) <= Tolerance;
end;

function SimilarColors(const Color1, Color2: TColor; const Tolerance: Single; ColorSpace: EColorSpace; const Multipliers: TChannelMultipliers): Boolean;
begin
  Result := ColorDistance(Color1, Color2, ColorSpace, Multipliers) <= Tolerance;
end;

end.

