{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  --------------------------------------------------------------------------

  An integral image, also known as a summed area table, is a data structure
  used for quick and efficient calculation of the sum of values in a
  rectangular subset of an image. The value at each pixel in the integral image
  is the sum of all the pixels above and to the left of it in the original
  image.
}
unit simba.image_integral;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.image;

type
  TIntegralImageDataF = record Value: Double; ValueSqr: Double; end;

  TSimbaIntegralImageF = record
  public
  type
    TData = record Value: Double; ValueSqr: Double; end;
    TDataMatrix = array of array of TData;
  public
    Data: TDataMatrix;

    class function Create(const From: TByteMatrix): TSimbaIntegralImageF; static;
    function Query(const Left, Top, Right, Bottom: Integer; out Value, ValueSqr: Double): TSimbaIntegralImageF;
  end;

implementation

class function TSimbaIntegralImageF.Create(const From: TByteMatrix): TSimbaIntegralImageF;
var
  X, Y: Integer;
begin
  SetLength(Result.Data, From.Height, From.Width);

  // Compute the first row of the integral image
  for X := 0 to From.Width - 1 do
  begin
    Result.Data[0, X].Value := From[0, X];
    Result.Data[0, X].ValueSqr := Sqr(From[0, X]);
    if (X > 0) then
    begin
      Result.Data[0, X].Value += Result.Data[0, X - 1].Value;
      Result.Data[0, X].ValueSqr += Result.Data[0, X - 1].ValueSqr;
    end;
  end;

  // Compute the first column of the integral image
  for Y := 1 to From.Height - 1 do
  begin
    Result.Data[Y, 0].Value := From[Y, 0] + Result.Data[Y - 1, 0].Value;
    Result.Data[Y, 0].ValueSqr := Sqr(From[Y, 0]) + Result.Data[Y - 1, 0].ValueSqr;
  end;

  // Compute the rest of the integral image
  for Y := 1 to From.Height - 1 do
    for X := 1 to From.Width - 1 do
    begin
      Result.Data[Y, X].Value := From[Y, X] + Result.Data[Y - 1, X].Value + Result.Data[Y, X - 1].Value - Result.Data[Y - 1, X - 1].Value;
      Result.Data[Y, X].ValueSqr := Sqr(From[Y, X]) + Result.Data[Y - 1, X].ValueSqr + Result.Data[Y, X - 1].ValueSqr - Result.Data[Y - 1, X - 1].ValueSqr;
    end;
end;

function TSimbaIntegralImageF.Query(const Left, Top, Right, Bottom: Integer; out Value, ValueSqr: Double): TSimbaIntegralImageF;
const
  ZERO: TData = (Value: 0; ValueSqr: 0);
var
  A,B,C,D: TData;
begin
  A := ZERO;
  B := ZERO;
  C := ZERO;
  D := ZERO;

  if (Left - 1 >= 0) and (Top - 1 >= 0) then
    A := Data[Top - 1, Left - 1];
  if (Top - 1 >= 0) then
    B := Data[Top - 1, Right];
  if (Left - 1 >= 0) then
    C := Data[Bottom, Left - 1];
  D := Data[Bottom, Right];

  Value    := D.Value - B.Value - C.Value + A.Value;
  ValueSqr := D.ValueSqr - B.ValueSqr - C.ValueSqr + A.ValueSqr;
end;

end.

