{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_imageboxrendercocoa;

{$i simba.inc}
{$modeswitch objectivec2}

interface

uses
  MacOSAll, CocoaAll, CocoaGDIObjects, cocoa_extra,
  Classes, SysUtils, Graphics,
  simba.base, simba.image,
  simba.component_imageboxrender;

type
  TSimbaImageBoxRendererCocoa = class(TSimbaImageBoxRenderer)
  protected
    FColorSpace: CGColorSpaceRef;

    function Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TSimbaImageBoxRendererCocoa.Create;
begin
  inherited Create();

  FColorSpace := CGColorSpaceCreateDeviceRGB();
end;

destructor TSimbaImageBoxRendererCocoa.Destroy;
begin
  if (FColorSpace <> nil) then
    CGColorSpaceRelease(FColorSpace);

  inherited Destroy();
end;

function TSimbaImageBoxRendererCocoa.Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean;
var
  Ctx: TCocoaContext;
  First: Integer;
  Provider: CGDataProviderRef;
  Image: CGImageRef;
  Rep: NSBitmapImageRep;
begin
  Ctx := TCocoaContext(Dest.Handle);
  if (Ctx = nil) or (Src.Data = nil) or (FColorSpace = nil) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));

  // The provider is told exactly what is left of the buffer from the first
  // wanted pixel; only W pixels of each row are ever read, so that always
  // covers the last row.
  First := SrcY * Src.Width + SrcX;
  Provider := CGDataProviderCreateWithData(nil, @Src.Data[First], (Src.Width * Src.Height - First) * SizeOf(TColorBGRA), nil);
  if (Provider = nil) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));

  Image := CGImageCreate(
    W, H,
    8, 32,                                                      // bits per component, bits per pixel
    Src.Width * SizeOf(TColorBGRA),                             // bytes per row: the image's pitch
    FColorSpace,
    kCGBitmapByteOrder32Little or kCGImageAlphaNoneSkipFirst,   // little endian XRGB = BGRX in memory
    Provider,
    nil,                                                        // decode array
    0,                                                          // should interpolate: a CBool, which is an integer here
    kCGRenderingIntentDefault
  );
  CGDataProviderRelease(Provider); // the image holds its own reference
  if (Image = nil) then
    Exit(inherited Blit(Dest, Src, SrcX, SrcY, W, H));

  Rep := NSBitmapImageRep(NSBitmapImageRep.alloc.initWithCGImage(Image));
  Result := (Rep <> nil) and Ctx.DrawImageRep(NSMakeRect(0, 0, W, H), NSMakeRect(0, 0, W, H), Rep);
  if (Rep <> nil) then
    Rep.release();
  CGImageRelease(Image);

  if Result then
    FBlitName := 'Cocoa'
  else
    Result := inherited Blit(Dest, Src, SrcX, SrcY, W, H);
end;

end.
