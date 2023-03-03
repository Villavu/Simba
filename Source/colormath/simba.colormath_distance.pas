{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Calculates the distance between two colors in RGB or HSL colorspace.
}
unit simba.colormath_distance;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.colormath_conversion;

function DistanceRGB(const Color1, Color2: TColorRGB): Integer; overload;
function DistanceRGB(const Color1, Color2: TColorBGRA): Integer; overload;
function DistanceRGB(const Color1, Color2: Integer): Integer; overload;

function DistanceHSL(const Color1, Color2: TColorHSL; const HueMod: Single = 0.2; const SatMod: Single = 0.2): Single; overload;
function DistanceHSL(const Color1, Color2: Integer; const HueMod: Single = 0.2; const SatMod: Single = 0.2): Single; overload;

implementation

function DistanceRGB(const Color1, Color2: TColorRGB): Integer;
begin
  Result := Sqr(Color1.R - Color2.R) + Sqr(Color1.G - Color2.G) + Sqr(Color1.B - Color2.B);
end;

function DistanceRGB(const Color1, Color2: TColorBGRA): Integer;
begin
  Result := Sqr(Color1.R - Color2.R) + Sqr(Color1.G - Color2.G) + Sqr(Color1.B - Color2.B);
end;

function DistanceRGB(const Color1, Color2: Integer): Integer;
begin
  Result := DistanceRGB(TColorRGB.Create(Color1), TColorRGB.Create(Color2));
end;

function DistanceHSL(const Color1, Color2: TColorHSL; const HueMod: Single; const SatMod: Single): Single;
var
  HueDiff: Single;
begin
  // ignore hue
  if (Color1.S = 0) or (Color2.S = 0) then
    Exit(Max(Abs(Color1.S - Color2.S) * 1 / SatMod, Abs(Color1.L - Color2.L)));

  // use hue
  if (Color1.H > Color2.H) then
    HueDiff := Min(Color1.H - Color2.H, Abs(Color1.H - (Color2.H + 100)))
  else
    HueDiff := Min(Color2.H - Color1.H, Abs(Color2.H - (Color1.H + 100)));

  Result := Max(Max(Abs(HueDiff * 1 / HueMod), Abs(Color1.S - Color2.S) * 1 / SatMod), Abs(Color1.L - Color2.L));
end;

function DistanceHSL(const Color1, Color2: Integer; const HueMod: Single; const SatMod: Single): Single;
begin
  Result := DistanceHSL(TColorHSL.Create(Color1), TColorHSL.Create(Color2), HueMod, SatMod);
end;

end.

