{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Extends TMufasaBitmap with features from other Simba units.
}
unit simba.bitmap_helpers;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.matchtemplate, simba.bitmap;

type
  TMufasaBitmapHelpers = class helper for TMufasaBitmap
  public
    function MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
    function MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;

    function FindColors(out Points: TPointArray; Color: Integer): Boolean; overload;
    function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean; overload;
    function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended): Boolean; overload;

    function FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;
    function FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;

    function FindEdges(MinDiff: Integer): TPointArray;
    function FindEdgesHSL(MinDiff: Integer; HueMod: Single = 0.2; SatMod: Single = 0.2): TPointArray;
  end;

implementation

uses
  simba.finder_color, simba.finder_bitmap, simba.colormath_distance, simba.colormath_conversion,
  simba.overallocatearray;

function TMufasaBitmapHelpers.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
begin
  Result := simba.matchtemplate.MatchTemplate(Self.ToMatrixBGR(), Template.ToMatrixBGR(), Formula);
end;

function TMufasaBitmapHelpers.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
begin
  Result := simba.matchtemplate.MatchTemplateMask(Self.ToMatrixBGR(), Template.ToMatrixBGR(), Formula);
end;

function TMufasaBitmapHelpers.FindColors(out Points: TPointArray; Color: Integer): Boolean;
var
  Buffer: TFindColorBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.Find(Points, Color);
end;

function TMufasaBitmapHelpers.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean;
var
  Buffer: TFindColorBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.FindCTS1(Points, Color, Tolerance);
end;

function TMufasaBitmapHelpers.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended): Boolean;
var
  Buffer: TFindColorBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.FindCTS2(Points, Color, Tolerance, HueMod, SatMod);
end;

function TMufasaBitmapHelpers.FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;
var
  Buffer: TFindBitmapBuffer;
  Points: TPointArray;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.Find(Bitmap, Points, Tolerance);
  if Result then
  begin
    X := Points[0].X;
    Y := Points[0].Y;
  end;
end;

function TMufasaBitmapHelpers.FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;
var
  Buffer: TFindBitmapBuffer;
begin
  Buffer.Data := FData;
  Buffer.Width := FWidth;
  Buffer.SearchWidth := FWidth;
  Buffer.SearchHeight := FHeight;

  Result := Buffer.Find(Bitmap, Points, Tolerance);
end;

function TMufasaBitmapHelpers.FindEdges(MinDiff: Integer): TPointArray;
var
  X, Y ,W, H: Integer;
  Buffer: TSimbaPointBuffer;
begin
  Buffer.Init();

  MinDiff := Sqr(MinDiff);

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (X+1 < W) then
        if DistanceRGB(FData[Y*FWidth+X], FData[Y*FWidth+(X+1)]) > MinDiff then
        begin
          Buffer.Add(TPoint.Create(X, Y));

          Continue;
        end;

      if (Y+1 < H) then
        if DistanceRGB(FData[Y*FWidth+X], FData[(Y+1)*FWidth+X]) > MinDiff then
          Buffer.Add(TPoint.Create(X, Y));
    end;

  Result := Buffer.Trim();
end;

function TMufasaBitmapHelpers.FindEdgesHSL(MinDiff: Integer; HueMod: Single; SatMod: Single): TPointArray;
var
  X, Y ,W, H: Integer;
  Buffer: TSimbaPointBuffer;
  Color: TColorHSL;
begin
  Buffer.Init();

  W := FWidth - 1;
  H := FHeight - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Color := TColorHSL.Create(FData[Y*FWidth+X]);
      if (X+1 < W) then
        if DistanceHSL(Color, TColorHSL.Create(FData[Y*FWidth+(X+1)]), HueMod, SatMod) > MinDiff then
        begin
          Buffer.Add(TPoint.Create(X, Y));

          Continue;
        end;

      if (Y+1 < H) then
        if DistanceHSL(Color, TColorHSL.Create(FData[(Y+1)*FWidth+X]), HueMod, SatMod) > MinDiff then
          Buffer.Add(TPoint.Create(X, Y));
    end;

  Result := Buffer.Trim();
end;

end.

