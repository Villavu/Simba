{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imageboxrendergtk3;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.image,
  simba.component_imageboxrender;

type
  TSimbaImageBoxRendererGTK3 = class(TSimbaImageBoxRenderer)
  protected
    function Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean; override;
  end;

implementation

uses
  LazCairo1, Gtk3Objects;

function TSimbaImageBoxRendererGTK3.Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean;
var
  DevCtx: TGtk3DeviceContext;
  Cairo: Pcairo_t;
  Surface: Pcairo_surface_t;
begin
  DevCtx := TGtk3DeviceContext(Dest.Handle);
  if (DevCtx = nil) or (Src.Data = nil) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));
  Cairo := DevCtx.pcr;
  if (Cairo = nil) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));

  // RGB24 is a native endian XRGB word, which is BGRX in memory: the image's own layout
  Surface := cairo_image_surface_create_for_data(PByte(Src.PixelPtr[SrcX, SrcY]), CAIRO_FORMAT_RGB24, W, H, Src.BytesPerRow);
  if (cairo_surface_status(Surface) <> CAIRO_STATUS_SUCCESS) then
  begin
    cairo_surface_destroy(Surface);
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));
  end;

  cairo_save(Cairo);
  try
    cairo_set_operator(Cairo, CAIRO_OPERATOR_SOURCE);
    cairo_set_source_surface(Cairo, Surface, 0, 0);
    // a HiDPI window scales the frame up; keep the pixels sharp
    cairo_pattern_set_filter(cairo_get_source(Cairo), CAIRO_FILTER_NEAREST);
    cairo_rectangle(Cairo, 0, 0, W, H);
    cairo_fill(Cairo);
  finally
    cairo_restore(Cairo);
  end;
  cairo_surface_finish(Surface);
  cairo_surface_destroy(Surface);

  FBlitName := 'GTK3';
  Result := True;
end;

end.
