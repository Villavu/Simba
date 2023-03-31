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

type
  TSimbaBitmapTarget = record
    Bitmap: TMufasaBitmap;

    procedure GetDimensions(out W, H: Integer);
    function GetImageData(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
  end;

implementation

procedure TSimbaBitmapTarget.GetDimensions(out W, H: Integer);
begin
  W := Bitmap.Width;
  H := Bitmap.Height;
end;

function TSimbaBitmapTarget.GetImageData(X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  Result := True;

  Data := @Bitmap.Data[Y * Bitmap.Width + X];
  DataWidth := Bitmap.Width;
end;

end.

