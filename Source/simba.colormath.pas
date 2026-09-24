{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.colormath;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  EColorSpace = (RGB, HSV, HSL, XYZ, LAB, LCH, DELTAE);
  PColorSpace = ^EColorSpace;
  {$POP}

  TColorRGB = record
    R,G,B: Byte;
  end;

  TColorXYZ = record
    X,Y,Z: Single;
  end;

  TColorLAB = record
    L,A,B: Single;
  end;

  TColorLCH = record
    L,C,H: Single;
  end;

  TColorHSV = record
    H,S,V: Single;
  end;

  TColorHSL = record
    H,S,L: Single;
  end;

  TColorBGR = record
    B,G,R: Byte;
  end;
  PColorBGR = ^TColorBGR;

  TColorARGB = record
  case Byte of
    0: (A, R, G, B: Byte);
    1: (AsInteger: UInt32);
  end;

  TColorRGBA = record
  case Byte of
    0: (R,G,B,A: Byte);
    1: (AsInteger: UInt32);
  end;

  PColorARGB = ^TColorARGB;
  PColorRGB = ^TColorRGB;
  PColorXYZ = ^TColorXYZ;
  PColorLAB = ^TColorLAB;
  PColorLCH = ^TColorLCH;
  PColorHSV = ^TColorHSV;
  PColorHSL = ^TColorHSL;

  PChannelMultipliers = ^TChannelMultipliers;
  TChannelMultipliers = array[0..2] of Single;

  TColorDistanceFunc = function(const Color1: Pointer; const Color2: PColorBGRA; const mul: TChannelMultipliers): Single;

  PColorTolerance = ^TColorTolerance;
  TColorTolerance = record
    Color: TColor;
    Tolerance: Single;
    ColorSpace: EColorSpace;
    Multipliers: TChannelMultipliers;

    class function Create(AColor: TColor; ATolerance: Single): TColorTolerance; static; overload;
    class function Create(AColor: TColor; ATolerance: Single; AColorSpace: EColorSpace; AMultipliers: TChannelMultipliers): TColorTolerance; static; overload;
  end;

const
  DefaultMultipliers: TChannelMultipliers = (1, 1, 1);
  DefaultColorSpace = EColorSpace.RGB;

type
  EColorSpaceHelper = type helper for EColorSpace
    function AsString: String;
  end;

  // note: TColor stuff should not include alpha
  TColor = Graphics.TColor;
  PColor = ^TColor;

  TColorHelper = type helper for TColor
    function R: Byte; inline;
    function G: Byte; inline;
    function B: Byte; inline;

    function ToBGRA(const Alpha: Byte = ALPHA_TRANSPARENT): TColorBGRA; inline;
    function ToRGB: TColorRGB; inline;
    function ToXYZ: TColorXYZ;
    function ToLAB: TColorLAB;
    function ToLCH: TColorLCH;
    function ToHSV: TColorHSV;
    function ToHSL: TColorHSL;
  end;

  TColorRGB_Helper = record helper for TColorRGB
    function ToXYZ: TColorXYZ;
    function ToLAB: TColorLAB;
    function ToLCH: TColorLCH;
    function ToHSV: TColorHSV;
    function ToHSL: TColorHSL;
    function ToColor: TColor; inline;
  end;

  TColorBGRA_Helper = record helper for TColorBGRA
    function Equals(const Other: TColorBGRA): Boolean; inline;
    function EqualsIgnoreAlpha(const Other: TColorBGRA): Boolean; inline;

    function ToColor: TColor; inline;
  end;

  TColorHSL_Helper = record helper for TColorHSL
    function ToRGB: TColorRGB;
    function ToColor: TColor;
  end;

  TColorHSV_Helper = record helper for TColorHSV
    function ToRGB: TColorRGB;
    function ToColor: TColor;
  end;

  TColorXYZ_Helper = record helper for TColorXYZ
    function ToRGB: TColorRGB;
    function ToColor: TColor;
  end;

  TColorLAB_Helper = record helper for TColorLAB
    function ToRGB: TColorRGB;
    function ToColor: TColor;
  end;

  TColorLCH_Helper = record helper for TColorLCH
    function ToRGB: TColorRGB;
    function ToColor: TColor;
  end;

function ColorIntensity(const Color: TColor): Byte;
function ColorToGray(const Color: TColor): Byte;
function ColorToRGB(const Color: TColor): TColorRGB;
function ColorToBGRA(const Color: TColor): TColorBGRA;
function ColorToHSL(const Color: TColor): TColorHSL;
function ColorToHSV(const Color: TColor): TColorHSV;
function ColorToXYZ(const Color: TColor): TColorXYZ;
function ColorToLAB(const Color: TColor): TColorLAB;
function ColorToLCH(const Color: TColor): TColorLCH;

function ColorToStr(Color: TColor): String;
function StrToColor(Str: String): TColor;

implementation

uses
  TypInfo,
  simba.colormath_conversion;

class function TColorTolerance.Create(AColor: TColor; ATolerance: Single): TColorTolerance;
begin
  Result := TColorTolerance.Create(AColor, ATolerance, DefaultColorSpace, DefaultMultipliers);
end;

class function TColorTolerance.Create(AColor: TColor; ATolerance: Single; AColorSpace: EColorSpace; AMultipliers: TChannelMultipliers): TColorTolerance;
begin
  Result.Color := AColor;
  Result.Tolerance := ATolerance;
  Result.ColorSpace := AColorSpace;
  Result.Multipliers := AMultipliers;
end;

function EColorSpaceHelper.AsString: String;
begin
  Result := GetEnumName(TypeInfo(Self), Ord(Self));
end;

function ColorToRGB(const Color: TColor): TColorRGB;
begin
  Result := Color.ToRGB();
end;

function ColorToBGRA(const Color: TColor): TColorBGRA;
begin
  Result := Color.ToBGRA();
end;

(*
  Average of R,G,B - Can be used to measure intensity.
*)
function ColorIntensity(const Color: TColor): Byte;
begin
  Result := ((Color and $FF) + (Color shr G_BIT and $FF) + (Color shr B_BIT and $FF)) div 3;
end;

(*
  Convert Color(RGB) to Grayscale / Luma
  Rec. 601: Y' = 0.299 R' + 0.587 G' + 0.114 B'
*)
function ColorToGray(const Color: TColor): Byte;
begin
  Result := Round(RED_TO_GREY[Color shr R_BIT and $FF] +
                  GREEN_TO_GREY[Color shr G_BIT and $FF] +
                  BLUE_TO_GREY[Color shr B_BIT and $FF]);
end;

function ColorToHSL(const Color: TColor): TColorHSL;
begin
  Result := Color.ToHSL();
end;

function ColorToHSV(const Color: TColor): TColorHSV;
begin
  Result := Color.ToHSV();
end;

function ColorToXYZ(const Color: TColor): TColorXYZ;
begin
  Result := Color.ToXYZ();
end;

function ColorToLAB(const Color: TColor): TColorLAB;
begin
  Result := Color.ToLAB();
end;

function ColorToLCH(const Color: TColor): TColorLCH;
begin
  Result := Color.ToLCH();
end;

function TColorHSV_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.HSVToRGB(H, S, V);
end;

function TColorHSV_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.HSVToRGB(H, S, V).ToColor();
end;

function TColorLAB_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.LABToRGB(L, A, B);
end;

function TColorLAB_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.LABToRGB(L, A, B).ToColor();
end;

function TColorLCH_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.LCHToRGB(L, C, H);
end;

function TColorLCH_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.LCHToRGB(L, C, H).ToColor();
end;

function TColorXYZ_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.XYZToRGB(X, Y, Z);
end;

function TColorXYZ_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.XYZToRGB(X, Y, Z).ToColor();
end;

function TColorHSL_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.HSLToRGB(H, S, L);
end;

function TColorHSL_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.HSLToRGB(H, S, L).ToColor();
end;

function TColorBGRA_Helper.ToColor: TColor;
begin
  Result := TColor(R or G shl G_BIT or B shl B_BIT);
end;

function TColorBGRA_Helper.Equals(const Other: TColorBGRA): Boolean;
begin
  Result := (AsInteger = Other.AsInteger);
end;

function TColorBGRA_Helper.EqualsIgnoreAlpha(const Other: TColorBGRA): Boolean;
begin
  Result := (AsInteger and $FFFFFF) = (Other.AsInteger and $FFFFFF);
end;

function TColorRGB_Helper.ToXYZ: TColorXYZ;
begin
  Result := TSimbaColorConversion.RGBToXYZ(R, G, B);
end;

function TColorRGB_Helper.ToLAB: TColorLAB;
begin
  Result := TSimbaColorConversion.RGBToLAB(R, G, B);
end;

function TColorRGB_Helper.ToLCH: TColorLCH;
begin
  Result := TSimbaColorConversion.RGBToLCH(R, G, B);
end;

function TColorRGB_Helper.ToHSV: TColorHSV;
begin
  Result := TSimbaColorConversion.RGBToHSV(R, G, B);
end;

function TColorRGB_Helper.ToHSL: TColorHSL;
begin
  Result := TSimbaColorConversion.RGBToHSL(R, G, B);
end;

function TColorRGB_Helper.ToColor: TColor;
begin
  Result := TColor(R or G shl G_BIT or B shl B_BIT);
end;

function TColorHelper.R: Byte;
begin
  Result := Self shr R_BIT and $FF;
end;

function TColorHelper.G: Byte;
begin
  Result := Self shr G_BIT and $FF;
end;

function TColorHelper.B: Byte;
begin
  Result := Self shr B_BIT and $FF;
end;

function TColorHelper.ToBGRA(const Alpha: Byte): TColorBGRA;
begin
  Result.R := (Self and R_MASK) shr R_BIT;
  Result.G := (Self and G_MASK) shr G_BIT;
  Result.B := (Self and B_MASK) shr B_BIT;
  Result.A := Alpha;
end;

function TColorHelper.ToRGB: TColorRGB;
begin
  Result.R := Self shr R_BIT and $FF;
  Result.G := Self shr G_BIT and $FF;
  Result.B := Self shr B_BIT and $FF;
end;

function TColorHelper.ToXYZ: TColorXYZ;
begin
  Result := TSimbaColorConversion.RGBToXYZ(R, G, B);
end;

function TColorHelper.ToLAB: TColorLAB;
begin
  Result := TSimbaColorConversion.RGBToLAB(R, G, B);
end;

function TColorHelper.ToLCH: TColorLCH;
begin
  Result := TSimbaColorConversion.RGBToLCH(R, G, B);
end;

function TColorHelper.ToHSV: TColorHSV;
begin
  Result := TSimbaColorConversion.RGBToHSV(R, G, B);
end;

function TColorHelper.ToHSL: TColorHSL;
begin
  Result := TSimbaColorConversion.RGBToHSL(R, G, B);
end;

function ColorToStr(Color: TColor): String;
begin
  Result := '$' + IntToHex(Color, 6);
end;

function StrToColor(Str: String): TColor;
begin
  Result := StrToIntDef(Str, 0);
end;

end.

