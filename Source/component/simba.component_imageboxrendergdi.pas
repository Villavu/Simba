{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  win32 image box blit.

  A BGRA buffer with top down rows is bit for bit what GDI calls a 32bpp
  BI_RGB DIB, so the frame goes to the device straight from the TSimbaImage
  with SetDIBitsToDevice: no handle, no intermediate bitmap, no copy.
}
unit simba.component_imageboxrendergdi;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.image,
  simba.component_imageboxrender;

type
  TSimbaImageBoxRendererGDI = class(TSimbaImageBoxRenderer)
  protected
    function Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean; override;
  end;

implementation

uses
  Windows;

function TSimbaImageBoxRendererGDI.Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean;
var
  Info: Windows.TBitmapInfo;
begin
  Info := Default(Windows.TBitmapInfo);
  Info.bmiHeader.biSize := SizeOf(Windows.TBitmapInfoHeader);
  Info.bmiHeader.biWidth := Src.Width; // the row pitch; only W columns are drawn
  Info.bmiHeader.biHeight := -H;       // negative: top down, only the rows we need
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biBitCount := 32;
  Info.bmiHeader.biCompression := BI_RGB;

  // The bits start at the first wanted row, not pixel, and xSrc picks the
  // columns: the header declares every row a full pitch wide, and from the first
  // wanted pixel the last row would run SrcX pixels past the end of the image.
  //
  // Returns the number of scan lines set; 0 means the upload failed, so recover
  // through the portable path rather than leaving the frame blank.
  if (Windows.SetDIBitsToDevice(
    Dest.Handle,
    0, 0, W, H,
    SrcX, 0,
    0, H,
    Src.PixelPtr[0, SrcY],
    Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS
  ) = 0) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));

  FBlitName := 'GDI';
  Result := True;
end;

end.
