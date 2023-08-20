unit simba.ide_utils;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Menus, Graphics;

function ImageWidthForDPI(DPI: Integer): Integer;
procedure MenuItemHeight(Item: TMenuItem; Canvas: TCanvas; var Height: Integer);

implementation

uses
  simba.settings;

function ImageWidthForDPI(DPI: Integer): Integer;
begin
  if not SimbaSettings.General.CustomImageSize.IsDefault() then
  begin
    Result := SimbaSettings.General.CustomImageSize.Value;
    Exit;
  end;

  if (DPI <= 96) then
    Result := 16   // no scaling
  else
  if (DPI <= 168) then
    Result := 24  // 125%-175% (120-168 DPI): 150% scaling
  else
    Result := 32; // 200, 300, 400, ...
end;

procedure MenuItemHeight(Item: TMenuItem; Canvas: TCanvas; var Height: Integer);
begin
  if not Item.IsLine then
    Height := Round(ImageWidthForDPI(Canvas.Font.PixelsPerInch) * 1.3);
end;

end.

