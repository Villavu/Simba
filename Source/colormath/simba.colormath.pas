{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Lots of code from: https://github.com/slackydev/colorlib
}
unit simba.colormath;

{$DEFINE B_BIT := 16}
{$DEFINE G_BIT := 8}
{$DEFINE R_BIT := 0}

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.mufasatypes;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  EColorSpace = (RGB, HSV, HSL, XYZ, LAB, LCH, DELTAE);
  PColorSpace = ^EColorSpace;
  {$POP}

  EColorSpaceHelper = type helper for EColorSpace
    function AsString: String;
  end;

  TColorHelper = type helper for TColor
    function ToBGRA: TColorBGRA;
    function ToRGB: TColorRGB;
    function ToXYZ: TColorXYZ;
    function ToLAB: TColorLAB;
    function ToLCH: TColorLCH;
    function ToHSV: TColorHSV;
    function ToHSL: TColorHSL;
  end;

  TColorRGB_Helper = record helper for TColorRGB
    function ToBGRA: TColorBGRA;
    function ToXYZ: TColorXYZ;
    function ToLAB: TColorLAB;
    function ToLCH: TColorLCH;
    function ToHSV: TColorHSV;
    function ToHSL: TColorHSL;
    function ToColor: TColor;
  end;

  TColorBGRA_Helper = record helper for TColorBGRA
    function Equals(const Other: TColorBGRA): Boolean; inline;
    function EqualsIgnoreAlpha(const Other: TColorBGRA): Boolean; inline;

    function ToRGB: TColorRGB;
    function ToXYZ: TColorXYZ;
    function ToLAB: TColorLAB;
    function ToLCH: TColorLCH;
    function ToHSV: TColorHSV;
    function ToHSL: TColorHSL;

    function ToColor: TColor;
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

implementation

uses
  TypInfo,
  simba.colormath_conversion;

function EColorSpaceHelper.AsString: String;
begin
  Result := GetEnumName(TypeInfo(Self), Ord(Self));
end;

function ColorToRGB(const Color: TColor): TColorRGB;
begin
  Result := TSimbaColorConversion.ColorToRGB(Color);
end;

function ColorToBGRA(const Color: TColor): TColorBGRA;
begin
  Result := TSimbaColorConversion.ColorToBGRA(Color);
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
  Result := (29  * (Color shr R_BIT and $FF) +
             150 * (Color shr G_BIT and $FF) +
             76  * (Color shr B_BIT and $FF) + 255) shr 8;
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
  Result := TSimbaColorConversion.HSVToRGB(Self);
end;

function TColorHSV_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.HSVToRGB(Self).ToColor();
end;

function TColorLAB_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.LABToRGB(Self);
end;

function TColorLAB_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.LABToRGB(Self).ToColor();
end;

function TColorLCH_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.LCHToRGB(Self);
end;

function TColorLCH_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.LCHToRGB(Self).ToColor();
end;

function TColorXYZ_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.XYZToRGB(Self);
end;

function TColorXYZ_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.XYZToRGB(Self).ToColor();
end;

function TColorHSL_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.HSLToRGB(Self);
end;

function TColorHSL_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.HSLToRGB(Self).ToColor();
end;

function TColorBGRA_Helper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.BGRAToRGB(Self);
end;

function TColorBGRA_Helper.ToXYZ: TColorXYZ;
begin
  Result := TSimbaColorConversion.RGBToXYZ(ToRGB());
end;

function TColorBGRA_Helper.ToLAB: TColorLAB;
begin
  Result := TSimbaColorConversion.RGBToLAB(ToRGB());
end;

function TColorBGRA_Helper.ToLCH: TColorLCH;
begin
  Result := TSimbaColorConversion.RGBToLCH(ToRGB());
end;

function TColorBGRA_Helper.ToHSV: TColorHSV;
begin
  Result := TSimbaColorConversion.RGBToHSV(ToRGB());
end;

function TColorBGRA_Helper.ToHSL: TColorHSL;
begin
  Result := TSimbaColorConversion.RGBToHSL(ToRGB());
end;

function TColorBGRA_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.RGBToColor(ToRGB());
end;

function TColorBGRA_Helper.Equals(const Other: TColorBGRA): Boolean;
begin
  Result := (AsInteger = Other.AsInteger);
end;

function TColorBGRA_Helper.EqualsIgnoreAlpha(const Other: TColorBGRA): Boolean;
begin
  Result := (AsInteger and $FFFFFF) = (Other.AsInteger and $FFFFFF);
end;

function TColorRGB_Helper.ToBGRA: TColorBGRA;
begin
  Result := TSimbaColorConversion.RGBToBGRA(Self);
end;

function TColorRGB_Helper.ToXYZ: TColorXYZ;
begin
  Result := TSimbaColorConversion.RGBToXYZ(Self);
end;

function TColorRGB_Helper.ToLAB: TColorLAB;
begin
  Result := TSimbaColorConversion.RGBToLAB(Self);
end;

function TColorRGB_Helper.ToLCH: TColorLCH;
begin
  Result := TSimbaColorConversion.RGBToLCH(Self);
end;

function TColorRGB_Helper.ToHSV: TColorHSV;
begin
  Result := TSimbaColorConversion.RGBToHSV(Self);
end;

function TColorRGB_Helper.ToHSL: TColorHSL;
begin
  Result := TSimbaColorConversion.RGBToHSL(Self);
end;

function TColorRGB_Helper.ToColor: TColor;
begin
  Result := TSimbaColorConversion.RGBToColor(Self);
end;

function TColorHelper.ToBGRA: TColorBGRA;
begin
  Result := TSimbaColorConversion.ColorToBGRA(Self);
end;

function TColorHelper.ToRGB: TColorRGB;
begin
  Result := TSimbaColorConversion.ColorToRGB(Self);
end;

function TColorHelper.ToXYZ: TColorXYZ;
begin
  Result := TSimbaColorConversion.RGBToXYZ(ToRGB());
end;

function TColorHelper.ToLAB: TColorLAB;
begin
  Result := TSimbaColorConversion.RGBToLAB(ToRGB());
end;

function TColorHelper.ToLCH: TColorLCH;
begin
  Result := TSimbaColorConversion.RGBToLCH(ToRGB());
end;

function TColorHelper.ToHSV: TColorHSV;
begin
  Result := TSimbaColorConversion.RGBToHSV(ToRGB());
end;

function TColorHelper.ToHSL: TColorHSL;
begin
  Result := TSimbaColorConversion.RGBToHSL(ToRGB());
end;

end.

