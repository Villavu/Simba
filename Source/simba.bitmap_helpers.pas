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
  classes, sysutils,
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
  end;

implementation

uses
  simba.finder_color, simba.finder_bitmap;

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

end.

