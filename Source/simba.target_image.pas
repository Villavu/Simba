{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target_image;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

procedure ImageTarget_GetDimensions(Target: Pointer; out W, H: Integer);
function ImageTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
function ImageTarget_IsValid(Target: Pointer): Boolean;

implementation

uses
  simba.image;

procedure ImageTarget_GetDimensions(Target: Pointer; out W, H: Integer);
var
  Image: TSimbaImage absolute Target;
begin
  W := Image.Width;
  H := Image.Height;
end;

function ImageTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; out Data: PColorBGRA; out DataWidth: Integer): Boolean;
var
  Image: TSimbaImage absolute Target;
begin
  Result := True;

  Data := Image.PixelPtr[X, Y];
  DataWidth := Image.Width;
end;

function ImageTarget_IsValid(Target: Pointer): Boolean;
begin
  Result := Assigned(Target);
end;

end.

