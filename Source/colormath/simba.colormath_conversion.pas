{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Convert a color represented as a integer (R,G,B) to RGB or HSL.
}
unit simba.colormath_conversion;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes;

type
  TColorRGB = record
    R, G, B: Byte;

    class function Create(const Color: Integer): TColorRGB; static; overload; inline;
    class function Create(const ColorBGRA: TColorBGRA): TColorRGB; static; overload; inline;
  end;

  TColorHSL = record
    H, S, L: Single;

    class function Create(const ColorRGB: TColorRGB): TColorHSL; static; overload; inline;
    class function Create(const ColorBGRA: TColorBGRA): TColorHSL; static; overload; inline;
    class function Create(const Color: Integer): TColorHSL; static; overload; inline;
  end;

implementation

class function TColorRGB.Create(const Color: Integer): TColorRGB;
begin
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
end;

class function TColorRGB.Create(const ColorBGRA: TColorBGRA): TColorRGB;
begin
  Result.R := ColorBGRA.R;
  Result.G := ColorBGRA.G;
  Result.B := ColorBGRA.B;
end;

class function TColorHSL.Create(const ColorRGB: TColorRGB): TColorHSL;
var
  R, G, B, deltaC, cMax, cMin: Single;
begin
  R := ColorRGB.R / 255;
  G := ColorRGB.G / 255;
  B := ColorRGB.B / 255;

  cMin   := Min(R, Min(G, B));
  cMax   := Max(R, Max(G, B));
  deltaC := cMax - cMin;

  Result.L := (cMax + cMin) * 0.5;

  if (deltaC = 0) then
  begin
    Result.H := 0;
    Result.S := 0;
  end else
  begin
    if (Result.L < 0.5) then Result.S := deltaC / (    cMax + cMin)
    else                     Result.S := deltaC / (2 - cMax - cMin);

    if      (R = cMax) then Result.H := (    (G - B) / deltaC)
    else if (G = cMax) then Result.H := (2 + (B - R) / deltaC)
    else                    Result.H := (4 + (R - G) / deltaC);

    Result.H := Result.H / 6;
    if (Result.H < 0) then
      Result.H := Result.H + 1;
  end;

  Result.H *= 100;
  Result.S *= 100;
  Result.L *= 100;
end;

class function TColorHSL.Create(const ColorBGRA: TColorBGRA): TColorHSL;
begin
  Result := TColorHSL.Create(TColorRGB.Create(ColorBGRA));
end;

class function TColorHSL.Create(const Color: Integer): TColorHSL;
begin
  Result := TColorHSL.Create(TColorRGB.Create(Color));
end;

end.

