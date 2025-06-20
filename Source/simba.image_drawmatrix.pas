{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_drawmatrix;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.image,
  simba.image_utils,
  simba.colormath,
  simba.vartype_matrix;

procedure SimbaImage_FromMatrix(Image: TSimbaImage; Matrix: TSingleMatrix; ColorMapType: Integer = 0); overload;
procedure SimbaImage_FromMatrix(Image: TSimbaImage; Matrix: TIntegerMatrix); overload;

procedure GetMatrixColor(const Value: Single; const ColorMapType: Integer; out R,G,B: Byte);

// For use of less accurate (less colors available) FromMatrix with ColorMapType=0
// but much faster as lookup table is used.
// Value must be 0..1
function GetHeapmapColor(const Value: Single): TColorBGR; inline;

var
  HeatmapTable: array[0..500] of TColorBGR;

implementation

procedure HSLtoRGB(H,S,L: Single; out R,G,B: Byte);

  function Hue2RGB(M1, M2: Single; Hue: Single): Byte; inline;
  begin
    if (Hue < 0) then Hue += 1;
    if (Hue > 1) then Hue -= 1;

    if (6 * Hue < 1) then
      Result := Round(255 * (M1 + (M2 - M1) * 6 * Hue))
    else if (2 * Hue < 1) then
      Result := Round(255 * M2)
    else if (3 * Hue < 2) then
      Result := Round(255 * (M1 + (M2 - M1) * ((2.0 / 3.0) - Hue) * 6))
    else
      Result := Round(255 * M1);
  end;

var
  M1, M2: Single;
begin
  if (S > 0) then
  begin
    H /= 360;
    S /= 100;
    L /= 100;
    if (L < 0.5) then
      M2 := L * (1 + S)
    else
      M2 := (L + S) - (S * L);
    M1 := 2 * L - M2;

    R := Hue2RGB(M1, M2, H + 1.0 / 3.0);
    G := Hue2RGB(M1, M2, H);
    B := Hue2RGB(M1, M2, H - 1.0 / 3.0);
  end else
  begin
    R := Round(L * 2.55);
    G := R;
    B := R;
  end;
end;

procedure SimbaImage_FromMatrix(Image: TSimbaImage; Matrix: TSingleMatrix; ColorMapType: Integer);
var
  Width, Height, X, Y: Integer;
  Normed: TSingleMatrix;
  Pixel: PColorBGRA;
begin
  Matrix.GetSize(Width, Height);
  Image.SetSize(Width, Height);

  Normed := Matrix.NormMinMax(0, 1);

  Dec(Width);
  Dec(Height);
  for Y := 0 to Height do
    for X := 0 to Width do
    begin
      Pixel := @Image.Data[Y * Image.Width + X];
      Pixel^.A := ALPHA_OPAQUE;

      GetMatrixColor(Normed[Y, X], ColorMapType, Pixel^.R, Pixel^.G, Pixel^.B);
    end;
end;

procedure SimbaImage_FromMatrix(Image: TSimbaImage; Matrix: TIntegerMatrix);
var
  X, Y, Width, Height: Integer;
  Pixel: PColorBGRA;
begin
  Matrix.GetSize(Width, Height);
  Image.SetSize(Width, Height);

  Dec(Width);
  Dec(Height);
  for Y := 0 to Height do
    for X := 0 to Width do
    begin
      Pixel := @Image.Data[Y * Image.Width + X];
      Pixel^.A := ALPHA_OPAQUE;
      Pixel^.R := (Matrix[Y,X] and R_MASK) shr R_BIT;
      Pixel^.G := (Matrix[Y,X] and G_MASK) shr G_BIT;
      Pixel^.B := (Matrix[Y,X] and B_MASK) shr B_BIT;
    end;
end;

procedure GetMatrixColor(const Value: Single; const ColorMapType: Integer; out R, G, B: Byte);
begin
  case ColorMapType of
    0: HSLtoRGB((1 - Value) * 240, 40 + Value * 60, 50, R, G, B);   // cold blue to red
    1: HSLtoRGB((1 - Value) * 240, 100, Value * 50, R, G, B);       // black -> blue -> red
    2: HSLtoRGB((1 - Value) * 240, 100, 100 - Value * 50, R, G, B); // white -> blue -> red
    3: HSLtoRGB(0, 0, (1 - Value) * 100, R, G, B);                  // light (to white)
    4: HSLtoRGB(0, 0, Value * 100, R, G, B)                         // light (to black)
    else
       HSLtoRGB(ColorMapType, 100, Value * 100, R, G, B);           // custom black to hue to white
  end;
end;

function GetHeapmapColor(const Value: Single): TColorBGR;
begin
  if (Value >= 0) and (Value <= 1.0) then
  begin
    Assert(Round(Value * High(HeatmapTable)) < Length(HeatmapTable));
    Result := HeatmapTable[Round(Value * High(HeatmapTable))]; // float value to heatmaptable index
  end else
  begin
    Result.R := 255; // return white, should stick out as a error since value must be within 0..1
    Result.G := 255;
    Result.B := 255;
  end;
end;

procedure BuildHeatmapTable;
var
  I: Integer;
  Val: Single;
begin
  I := 0;
  Val := 0.0;
  while (Val <= 1) and (I < Length(HeatmapTable)) do
  begin
    HSLtoRGB((1 - Val) * 240, 40 + Val * 60, 50, HeatmapTable[I].R, HeatmapTable[I].G, HeatmapTable[I].B);
    Inc(I);
    Val := Val + 0.002; // will obv need tweaking depending on heatmap table size
  end;
  Assert(I = Length(HeatmapTable));
end;

initialization
  BuildHeatmapTable();

end.

