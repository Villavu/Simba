{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imageboxrendergtk2;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.image,
  simba.component_imageboxrender;

type
  TSimbaImageBoxRendererGTK2 = class(TSimbaImageBoxRenderer)
  protected
    function Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean; override;
  end;

implementation

uses
  Cairo, gdk2, Gtk2Def;

function TSimbaImageBoxRendererGTK2.Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean;
var
  DevCtx: TGtkDeviceContext;
  Cr: Pcairo_t;
  Surface: Pcairo_surface_t;
  Origin: TPoint;
begin
  DevCtx := TGtkDeviceContext(Dest.Handle);
  if (DevCtx = nil) or (DevCtx.Drawable = nil) or (Src.Data = nil) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));

  // RGB24 is a native endian XRGB word, which is BGRX in memory: the image's own layout
  Surface := cairo_image_surface_create_for_data(PByte(Src.PixelPtr[SrcX, SrcY]), CAIRO_FORMAT_RGB24, W, H, Src.BytesPerRow);
  if (cairo_surface_status(Surface) <> CAIRO_STATUS_SUCCESS) then
  begin
    cairo_surface_destroy(Surface);
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));
  end;

  // draw to the drawable itself, not to a pixbuf the context may be holding
  DevCtx.RemovePixbuf();

  Cr := gdk_cairo_create(DevCtx.Drawable);
  Origin := DevCtx.Offset;
  cairo_translate(Cr, Origin.X, Origin.Y);
  cairo_set_operator(Cr, CAIRO_OPERATOR_SOURCE);
  cairo_set_source_surface(Cr, Surface, 0, 0);
  cairo_rectangle(Cr, 0, 0, W, H);
  cairo_fill(Cr);
  cairo_destroy(Cr);
  cairo_surface_finish(Surface);
  cairo_surface_destroy(Surface);

  FBlitName := 'GTK2';
  Result := True;
end;

end.
