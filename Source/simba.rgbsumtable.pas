{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.rgbsumtable;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.bitmap;

type
  TRGBSum = record
    R, G, B: UInt64;

    class operator :=(const Right: TColorBGRA): TRGBSum;

    class operator +(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;
    class operator -(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;

    class operator +(const Left, Right: TRGBSum): TRGBSum;
    class operator -(const Left, Right: TRGBSum): TRGBSum;
  end;
  TRGBSumTable = array of array of TRGBSum;

  TRGBSumTableHelper = type helper for TRGBSumTable
  public
    class function Create(Image: TSimbaImage): TRGBSumTable; static;

    function Query(Area: TBox): TRGBSum;
  end;

implementation

class operator TRGBSum.:=(const Right: TColorBGRA): TRGBSum;
begin
  Result.R := Right.R;
  Result.G := Right.G;
  Result.B := Right.B;
end;

class operator TRGBSum.+(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;
begin
  Result.R := Left.R + Right.R;
  Result.G := Left.G + Right.G;
  Result.B := Left.B + Right.B;
end;

class operator TRGBSum.-(const Left: TRGBSum; const Right: TColorBGRA): TRGBSum;
begin
  Result.R := Left.R - Right.R;
  Result.G := Left.G - Right.G;
  Result.B := Left.B - Right.B;
end;

class operator TRGBSum.+(const Left, Right: TRGBSum): TRGBSum;
begin
  Result.R := Left.R + Right.R;
  Result.G := Left.G + Right.G;
  Result.B := Left.B + Right.B;
end;

class operator TRGBSum.-(const Left, Right: TRGBSum): TRGBSum;
begin
  Result.R := Left.R - Right.R;
  Result.G := Left.G - Right.G;
  Result.B := Left.B - Right.B;
end;

class function TRGBSumTableHelper.Create(Image: TSimbaImage): TRGBSumTable;
var
  W, H, X, Y: Integer;
begin
  SetLength(Result, Image.Height, Image.Width);

  W := Image.Width - 1;
  H := Image.Height - 1;

  for Y := 0 to H do
    Result[Y, 0] := Image.Data[Y*Image.Width];

  for Y := 1 to H do
    for X := 0 to W do
      Result[Y, X] := Image.Data[Y*Image.Width+X] + Result[Y-1, X];

  for Y := 0 to H do
    for X := 1 to W do
      Result[Y, X] += Result[Y, X-1];
end;

function TRGBSumTableHelper.Query(Area: TBox): TRGBSum;
begin
  Result := Self[Area.Y2, Area.X2];

  if (Area.Y1 > 0) then
  begin
    Result.R := Result.R - Self[Area.Y1 - 1, Area.X2].R;
    Result.G := Result.G - Self[Area.Y1 - 1, Area.X2].G;
    Result.B := Result.B - Self[Area.Y1 - 1, Area.X2].B;
  end;

  if (Area.X1 > 0) then
  begin
    Result.R := Result.R - Self[Area.Y2, Area.X1 - 1].R;
    Result.G := Result.G - Self[Area.Y2, Area.X1 - 1].G;
    Result.B := Result.B - Self[Area.Y2, Area.X1 - 1].B;
  end;

  if (Area.Y1 > 0) and (Area.X1 > 0) then
  begin
    Result.R := Result.R + Self[Area.Y1 - 1, Area.X1 - 1].R;
    Result.G := Result.G + Self[Area.Y1 - 1, Area.X1 - 1].G;
    Result.B := Result.B + Self[Area.Y1 - 1, Area.X1 - 1].B;
  end;
end;

end.

