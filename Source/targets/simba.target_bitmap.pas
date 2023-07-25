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

procedure ImageTarget_GetDimensions(Target: Pointer; out W, H: Integer);
function ImageTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
function ImageTarget_IsValid(Target: Pointer): Boolean;

implementation

procedure ImageTarget_GetDimensions(Target: Pointer; out W, H: Integer);
var
  Image: TSimbaImage absolute Target;
begin
  W := Image.Width;
  H := Image.Height;
end;

function ImageTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
var
  Image: TSimbaImage absolute Target;
begin
  Result := True;

  Data := @Image.Data[Y * Image.Width + X];
  DataWidth := Image.Width;
end;

function ImageTarget_IsValid(Target: Pointer): Boolean;
begin
  Result := Assigned(Target);
end;

end.

