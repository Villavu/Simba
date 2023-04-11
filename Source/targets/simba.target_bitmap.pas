{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target_bitmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.bitmap;

procedure BitmapTarget_GetDimensions(Target: Pointer; out W, H: Integer);
function BitmapTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

implementation

procedure BitmapTarget_GetDimensions(Target: Pointer; out W, H: Integer);
var
  Bitmap: TMufasaBitmap absolute Target;
begin
  W := Bitmap.Width;
  H := Bitmap.Height;
end;

function BitmapTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
var
  Bitmap: TMufasaBitmap absolute Target;
begin
  Result := True;

  Data := @Bitmap.Data[Y * Bitmap.Width + X];
  DataWidth := Bitmap.Width;
end;

end.

