{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Reading and measuring a buffer's pixels
}
unit simba.finder_pixels;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.colormath;

{$PUSH}
{$SCOPEDENUMS ON}
type
  EBrightnessAlgo = (MEAN, MIN, MAX);
{$POP}

function SimbaFinder_GetColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint; Points: TPointArray): TColorArray;

function SimbaFinder_GetColorsMatrix(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer): TIntegerMatrix;

// the pixels whose colour differs from the one to their right or below by more than MinDiff
function SimbaFinder_FindEdges(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                               MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;

function SimbaFinder_GetBrightness(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Algo: EBrightnessAlgo): Integer;

implementation

uses
  simba.colormath_distance,
  simba.image_utils,
  simba.container_point;

function SimbaFinder_GetColors(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint; Points: TPointArray): TColorArray;
var
  Count, I, X, Y: Integer;
begin
  Result := [];
  if (Data = nil) then
    Exit;

  Count := 0;

  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
  begin
    X := Points[I].X - Offset.X;
    Y := Points[I].Y - Offset.Y;
    if (X >= 0) and (Y >= 0) and (X < AWidth) and (Y < AHeight) then
    begin
      Result[Count] := Data[Y * PixelsPerRow + X].ToColor;
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

function SimbaFinder_GetColorsMatrix(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer): TIntegerMatrix;
var
  Y: Integer;
begin
  Result := [];
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  SetLength(Result, AHeight, AWidth);
  for Y := 0 to AHeight - 1 do
    BGRAToColors(@Data[Y * PixelsPerRow], PInt32(Result[Y]), AWidth);
end;

function SimbaFinder_FindEdges(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Offset: TPoint;
                               MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TPointArray;
var
  X, Y, W, H: Integer;
  PointBuffer: TPointBuffer;
  First, Second, Third: TColor;
begin
  Result := [];
  if (Data = nil) then
    Exit;

  W := AWidth - 2;
  H := AHeight - 2;
  for Y := 0 to H do
    for X := 0 to W do
    begin
      First  := Data[Y * PixelsPerRow + X].ToColor;
      Second := Data[Y * PixelsPerRow + (X+1)].ToColor;
      Third  := Data[(Y+1) * PixelsPerRow + X].ToColor;

      if (not SimilarColors(First, Second, MinDiff, ColorSpace, Multipliers)) or
         (not SimilarColors(First, Third, MinDiff, ColorSpace, Multipliers)) then
        PointBuffer.Add(X + Offset.X, Y + Offset.Y);
    end;

  Result := PointBuffer.ToArray(False);
end;

function SimbaFinder_GetBrightness(Data: PColorBGRA; PixelsPerRow, AWidth, AHeight: Integer; Algo: EBrightnessAlgo): Integer;
var
  Y, Grey: Integer;
  Min,Max: Integer;
  Sum: UInt64;
  Ptr, RowEnd: PColorBGRA;
begin
  Result := 0;
  if (Data = nil) or (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  Min := 255;
  Max := 0;
  Sum := 0;

  for Y := 0 to AHeight - 1 do
  begin
    Ptr := @Data[Y * PixelsPerRow];
    RowEnd := Ptr + AWidth;
    while (Ptr < RowEnd) do
    begin
      Grey := PixelToGrey(Ptr^);
      if (Grey < Min) then Min := Grey;
      if (Grey > Max) then Max := Grey;
      Sum += Grey;
      Inc(Ptr);
    end;
  end;

  case Algo of
    EBrightnessAlgo.MIN:  Result := Min;
    EBrightnessAlgo.MAX:  Result := Max;
    EBrightnessAlgo.MEAN: Result := Sum div UInt64(AWidth * AHeight);
  end;
end;

end.
