{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Colour Conversion Utilities for the Mufasa Macro Library
}

unit colour_conv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, mufasatypes,
  Math;


Function RGBtoColor(r,g,b : byte) : TColor; overload; inline;
Function RGBtoColor(r,g,b : integer) : TColor; overload; inline;
Procedure ColorToRGB(Color : integer;out r,g,b : byte); overload; inline;
Procedure ColorToRGB(Color : integer;out r,g,b : integer); overload; inline;
Procedure RGBToXYZ(R,G,B : byte;out x,y,z : Extended); inline;
Procedure XYZToRGB(X,Y,Z : Extended;out R,G,B: byte); inline;
Procedure RGBToHSL(RR,GG,BB : byte;out H,S,L : Extended); inline;
Procedure RGBToHSLNonFixed(RR,GG,BB : byte;out H,S,L : Extended); inline;
Procedure HSLtoRGB(H,S,L : extended;out R,G,B : Byte); inline;
Procedure ColorToHSL(Col: Integer; out h, s, l: Extended); inline;
procedure ColorToXYZ(color: Integer; out X, Y, Z: Extended); inline;
function XYZToColor(X, Y, Z: Extended): TColor; inline;
function HSLToColor(H, S, L: Extended): TColor; inline;
function BGRToRGB(BGR : TRGB32) : TColor;inline;



implementation

Const
  OneDivThree = 1/3.0;
  TwoDivThree = 2 / 3.0;
  OneDivTwoPointFour = 1 / 2.4;
function BGRToRGB(BGR : TRGB32) : TColor;inline;
begin;
  Result := BGR.R or BGR.g shl 8 or BGR.b shl 16;
end;

Function RGBtoColor(r,g,b : byte): TColor; overload; inline;
begin;
  Result := R or g shl 8 or b shl 16;
end;

{/\
  Translates the given Red (R), Green (G) and Blue (B) components to a TColor.
  R, G and B are integers.
/\}

Function RGBtoColor(r,g,b : integer): TColor; overload; inline;
begin;
  Result := R or g shl 8 or b shl 16;
end;

{/\
   Translates the given win-32 color in the Red (R), Green (G) and Blue (B)
   components. R, G and B are bytes.
/\}

Procedure ColorToRGB(Color : integer;out r,g,b : byte); overload; inline;
begin
  R := Color and $ff;
  G := Color shr 8 and $ff;
  B := Color shr 16 and $ff;
end;

{/\
   Translates the given win-32 color in the Red (R), Green (G) and Blue (B)
   components. R, G and B are integers.
/\}

Procedure ColorToRGB(Color : integer;out r,g,b : integer); overload; inline;
begin
  R := Color and $ff;
  G := Color shr 8 and $ff;
  B := Color shr 16 and $ff;
end;

{/\
   Translates the given Red (R), Green (G) and Blue (B) components to
   X, Y and Z components.
/\}

Procedure RGBToXYZ(R,G,B : byte;out x,y,z : Extended); inline;
var
  Red,Green,Blue : Extended;
begin;
  Red := R / 255;
  Green := G / 255;
  Blue := B / 255;
  if Red > 0.04045  then
    Red := Power( ( Red + 0.055 ) / 1.055  , 2.4) * 100
  else
    Red := Red / 7.73994;
  if Green > 0.04045  then
    Green := Power( ( Green + 0.055 ) / 1.055 , 2.4) *  100
  else
    Green := Green / 7.73994;
  if  Blue > 0.04045 then
    Blue := Power(  ( Blue + 0.055 ) / 1.055  , 2.4) * 100
  else
    Blue := Blue / 7.73994;
  X := Red * 0.4124 + Green * 0.3576 + Blue * 0.1805;
  Y := Red * 0.2126 + Green * 0.7152 + Blue * 0.0722;
  Z := Red * 0.0193 + Green * 0.1192 + Blue * 0.9505;
end;

{/\
   Translates the given X, Y and Z components to
   Red (R), Green (G) and Blue (B) components.
/\}

Procedure XYZToRGB(X,Y,Z : Extended;out R,G,B: byte); inline;
var
  TempR,TempG,TempB,Tempx,tempy,tempz : Extended;
begin;
  Tempx := X / 100;
  tempy := Y / 100;
  tempz := Z / 100;
  TempR := Tempx *  3.2406 + tempy * -1.5372 + tempz * -0.4986;
  TempG := Tempx * -0.9689 + tempy *  1.8758 + tempz *  0.0415;
  TempB := Tempx *  0.0557 + tempy * -0.2040 + tempz *  1.0570;
  if TempR > 0.0031308  then
    TempR := 1.055 * ( Power(TempR, (OneDivTwoPointFour)) ) - 0.055
  else
    TempR := 12.92 * TempR;
  if TempG > 0.0031308 then
    TempG := 1.055 * ( Power(TempG, ( OneDivTwoPointFour)) ) - 0.055
  else
    TempG := 12.92 * TempG;
  if  TempB > 0.0031308 then
    TempB := 1.055 * ( Power(TempB , ( OneDivTwoPointFour )) ) - 0.055
  else
    TempB := 12.92 * TempB;
  R := Round(TempR * 255);
  G := Round(TempG * 255);
  B := Round(TempB * 255);
end;

{/\
   Translates the given Red (R), Green (G) and Blue (B) components to
   H (Hue), S (Saturation) and L (Luminance) components.
/\}

Procedure RGBToHSL(RR,GG,BB : byte;out H,S,L : Extended); inline;
var
  R,  G,  B,   D,  Cmax, Cmin: Extended;
begin
  R := RR / 255;
  G := GG / 255;
  B := BB / 255;
  CMin := R;
  if G < Cmin then Cmin := G;
  if B  < Cmin then Cmin := B;
  CMax := R;
  if G > Cmax then Cmax := G;
  if B  > Cmax then Cmax := B;
  L := 0.5 * (Cmax + Cmin);
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end else
  begin;
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
      if G = Cmax then
        H  := 2 + (B - R) / D
      else
        H := 4 +  (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
  H := H * 100;
  S := S * 100;
  L := L * 100;
end;

{/\
   Translates the given Red (R), Green (G) and Blue (B) components to
   H (Hue), S (Saturation) and L (Luminance) components.
   This function does not multiply it by 100.
/\}

Procedure RGBToHSLNonFixed(RR,GG,BB : byte;out H,S,L : Extended); inline;
var
  R,  G,  B,   D,  Cmax, Cmin: Extended;
begin
  R := RR / 255;
  G := GG / 255;
  B := BB / 255;
  CMin := R;
  if G < Cmin then Cmin := G;
  if B  < Cmin then Cmin := B;
  CMax := R;
  if G > Cmax then Cmax := G;
  if B  > Cmax then Cmax := B;
  L := 0.5 * (Cmax + Cmin);
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0;
  end else
  begin;
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
      if G = Cmax then
        H  := 2 + (B - R) / D
      else
        H := 4 +  (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

{/\
   Translates the given H (Hue), S (Saturation) and L (Luminance) components to
   Red (R), Green (G) and Blue (B) components.
/\}

procedure HSLtoRGB(H, S, L: extended; out R, G, B: Byte); inline;
var
  Temp,Temp2 : Extended;
//begin

Function Hue2RGB(TempHue : Extended) : integer;
begin;
  if TempHue < 0 then
    TempHue := TempHue + 1
  else if TempHue > 1 then
    TempHue := TempHue - 1;
  if ( ( 6 * TempHue ) < 1 ) then
    Result :=Round(255 * (( Temp + ( Temp2 - Temp ) * 6 * TempHue )))
  else if ( ( 2 * TempHue ) < 1 ) then
    Result :=Round(255 * Temp2)
  else if ( ( 3 * TempHue ) < 2 ) then
    Result :=Round(255 * (Temp + ( Temp2 - Temp ) * ( ( TwoDivThree ) - TempHue ) * 6))
  else
    Result :=Round(255 * Temp);
end;

begin;
  H := H / 100;
  S := S / 100;
  L := L / 100;
  if s = 0 then
  begin;
    R := Byte(Round(L * 255));
    G := R;
    B := R;
  end else
  begin;
    if (L < 0.5) then
      Temp2 := L * ( 1 + S )
    else
      Temp2 := (L + S) - ( S * L);
    Temp := 2 * L - Temp2;
    R := Hue2RGB( H + ( OneDivThree ) );
    G := Hue2RGB( H );
    B := Hue2RGB( H - ( OneDivThree ) );
  end;
end;

{/\
  Split the Given Color col in H, S, L components.
/\}

Procedure ColorToHSL(Col: Integer; out h, s, l: Extended); inline;
Var
  R, G, B: byte;
Begin
  ColorToRGB(Col, R, G, B);
  RGBToHSL(R, G, B, H, S, L);
End;

procedure ColorToXYZ(color: Integer; out X, Y, Z: Extended); inline;
var
  R, G, B: byte;
begin
  ColorToRGB(Color, R, G, B);
  RGBToXYZ(R, G, B, X, Y, Z);
end;

function HSLToColor(H, S, L: Extended): TColor; inline;
var
  r, g, b: byte;
begin
  HSLToRGB(H, S, L, r, g, b);
  Result := RGBToColor(r, g, b);
end;

function XYZToColor(X, Y, Z: Extended): TColor; inline;
var
  r, g, b: byte;
begin
  XYZToRGB(X, Y, Z, r, g, b);
  Result := RGBToColor(r, g, b);
end;

end.
