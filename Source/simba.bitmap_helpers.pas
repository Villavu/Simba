{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmap_helpers;

{$i simba.inc}

interface

uses
  classes, sysutils, graphics;

type
  TBitmapHelper = class helper for TBitmap
  public
    procedure DrawTPA(constref TPA: TPointArray);
  end;

implementation

uses
  simba.bitmap, simba.colormath;

procedure TBitmapHelper.DrawTPA(constref TPA: TPointArray);
var
  RGB24: TRGB24;
  RGB32: TRGB32;
  Data: PByte;
begin
  BeginUpdate(True);

  Data := RawImage.Data;
  BytesPerLine := RawImage.Description.BytesPerLine;
  BytesPerPixel := RawImage.Description.BitsPerPixel div 8;

  case RawImage.Description.BitsPerPixel of
    24:
      begin
        RGB24.B := Color shr 16 and $FF;
        RGB24.G := Color shr 8 and $FF;
        RGB24.R := Color and $FF;

        for P in TPA do
          PRGB24(Data)[P.Y * BytesPerLine + P.X * BytesPerPixel] := RGB24;
      end;
    32:
      begin
        RGB32.B := Color shr 16 and $FF;
        RGB32.G := Color shr 8 and $FF;
        RGB32.R := Color and $FF;
        RGB32.A := 0;

        for P in TPA do
          PRGB32(Data)[P.Y * BytesPerLine + P.X * BytesPerPixel] := RGB32;
      end;
  end;

  EndUpdate(False);
end;

end.

