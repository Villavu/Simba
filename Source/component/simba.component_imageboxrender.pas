{
  Author: Raymond van Venetie and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Rendering back end for TSimbaImageBox.

  Turns "this rectangle of the background, at this whole pixel ratio" into
  pixels on a device context. Three separate paths, one per direction:

    RenderNormal     1:1
    RenderZoomedIn   one image pixel covers Ratio screen pixels
    RenderZoomedOut  Ratio image pixels collapse into one screen pixel
}
unit simba.component_imageboxrender;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.image, simba.canvas;

const
  // Buffers grow with this much slack so resizing the view does not realloc every frame.
  IMAGEBOX_BUFFER_SLACK = 150;

type
  TSimbaImageBoxOverlayEvent = procedure(ACanvas: TSimbaCanvas; R: TRect) of object;

  TSimbaImageBoxRenderLayer = record
    Pixels: TSimbaImage;
    Opacity: Byte;
  end;
  TSimbaImageBoxRenderLayers = array of TSimbaImageBoxRenderLayer;

  TSimbaImageBoxRenderer = class
  protected
    FCanvas: TSimbaCanvas;
    FCanvasImage: TSimbaImage;
    FScaleImage: TSimbaImage;   // the resized frame, when the frame has to be resized
    FBlitBitmap: TBitmap;       // the portable Blit's LCL bitmap
    FBlitName: String;          // which Blit put the last frame on screen

    procedure GrowImage(Img: TSimbaImage; W, H: Integer);

    procedure BeginCanvas(const R: TRect; W, H, Ratio: Integer);
    procedure EndCanvas;

    // Puts Src(SrcX, SrcY, W, H) onto Dest at 0, 0. Same size, always.
    function Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean; virtual;

    // composites every layer, bottom to top, over what is in the canvas
    procedure BlendLayers(const Layers: TSimbaImageBoxRenderLayers; SrcX, SrcY, Ratio, W, H: Integer);

    function RenderNormal(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; R: TRect; Overlay: TSimbaImageBoxOverlayEvent): TPoint;
    function RenderZoomedIn(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; R: TRect; Ratio: Integer; Overlay: TSimbaImageBoxOverlayEvent): TPoint;
    function RenderZoomedOut(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; R: TRect; Ratio: Integer; Overlay: TSimbaImageBoxOverlayEvent): TPoint;
  public
    constructor Create;
    destructor Destroy; override;

    // Draws Background[ImageRect] onto Dest at 0, 0 and returns the size actually
    // drawn, so the caller can clear whatever the image does not reach.
    function Render(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; const ImageRect: TRect;
                    Ratio: Integer; ZoomIn: Boolean;
                    Overlay: TSimbaImageBoxOverlayEvent): TPoint;

    // 'LCL', 'GDI', 'GTK2', 'GTK3', 'Cocoa'
    property BlitName: String read FBlitName;
  end;

function CreateImageBoxRenderer: TSimbaImageBoxRenderer;

implementation

uses
  LCLType, LCLIntf,
  simba.image_lazbridge,
  simba.image_utils
  {$IF DEFINED(WINDOWS)},  simba.component_imageboxrendergdi   {$ENDIF}
  {$IF DEFINED(LCLGtk2)},  simba.component_imageboxrendergtk2  {$ENDIF}
  {$IF DEFINED(LCLGtk3)},  simba.component_imageboxrendergtk3  {$ENDIF}
  {$IF DEFINED(LCLCocoa)}, simba.component_imageboxrendercocoa {$ENDIF};

function CreateImageBoxRenderer: TSimbaImageBoxRenderer;
begin
  {$IF DEFINED(WINDOWS)}
  Result := TSimbaImageBoxRendererGDI.Create();
  {$ELSEIF DEFINED(LCLGtk2)}
  Result := TSimbaImageBoxRendererGTK2.Create();
  {$ELSEIF DEFINED(LCLGtk3)}
  Result := TSimbaImageBoxRendererGTK3.Create();
  {$ELSEIF DEFINED(LCLCocoa)}
  Result := TSimbaImageBoxRendererCocoa.Create();
  {$ELSE}
  Result := TSimbaImageBoxRenderer.Create();
  {$ENDIF}
end;

// Dest(0, 0, W, H) := Src(SrcX, SrcY, W, H)
procedure CopyPixels(Src: TSimbaImage; SrcX, SrcY, W, H: Integer; Dest: TSimbaImage);
begin
  CopyRows(Dest.Data, Dest.BytesPerRow, Src.PixelPtr[SrcX, SrcY], Src.BytesPerRow, W, H);
end;

// Dest(0, 0, DestW, DestH) := every Ratio'th pixel of every Ratio'th row of Src from SrcX, SrcY. Nearest neighbour
procedure ScaleDown(Ratio: Integer; Src: TSimbaImage; SrcX, SrcY: Integer; Dest: TSimbaImage; DestW, DestH: Integer);
var
  SrcRow, DestRow, DestRowEnd, SrcPtr, DestPtr, DestEnd: PColorBGRA;
begin
  SrcRow     := Src.PixelPtr[SrcX, SrcY];
  DestRow    := Dest.Data;
  DestRowEnd := DestRow + (DestH * Dest.Width);

  while (DestRow < DestRowEnd) do
  begin
    SrcPtr  := SrcRow;
    DestPtr := DestRow;
    DestEnd := DestRow + DestW;
    while (DestPtr < DestEnd) do
    begin
      DestPtr^ := SrcPtr^;

      Inc(SrcPtr, Ratio);
      Inc(DestPtr);
    end;

    Inc(SrcRow, Src.Width * Ratio);
    Inc(DestRow, Dest.Width);
  end;
end;

// Dest(0, 0, SrcW * Ratio, SrcH * Ratio) := Src(SrcX, SrcY, SrcW, SrcH) with
// every pixel a Ratio x Ratio block.
procedure ScaleUp(Ratio: Integer; Src: TSimbaImage; SrcX, SrcY, SrcW, SrcH: Integer; Dest: TSimbaImage);
var
  SrcRow, SrcRowEnd, SrcPtr, SrcEnd, DestRow, DestPtr, DestEnd: PColorBGRA;
  Pixel: TColorBGRA;
begin
  SrcRow    := Src.PixelPtr[SrcX, SrcY];
  SrcRowEnd := SrcRow + (SrcH * Src.Width);
  DestRow   := Dest.Data;

  while (SrcRow < SrcRowEnd) do
  begin
    SrcPtr  := SrcRow;
    SrcEnd  := SrcRow + SrcW;
    DestPtr := DestRow;
    while (SrcPtr < SrcEnd) do
    begin
      Pixel   := SrcPtr^;
      DestEnd := DestPtr + Ratio;
      while (DestPtr < DestEnd) do
      begin
        DestPtr^ := Pixel;

        Inc(DestPtr);
      end;

      Inc(SrcPtr);
    end;

    DestPtr := DestRow + Dest.Width;
    DestEnd := DestRow + (Dest.Width * Ratio);
    while (DestPtr < DestEnd) do
    begin
      Move(DestRow^, DestPtr^, SrcW * Ratio * SizeOf(TColorBGRA));

      Inc(DestPtr, Dest.Width);
    end;

    Inc(SrcRow, Src.Width);
    Inc(DestRow, Dest.Width * Ratio);
  end;
end;

procedure BlendLayer(const Layer: TSimbaImageBoxRenderLayer; SrcX, SrcY, Ratio: Integer; Dest: TSimbaImage; DestW, DestH: Integer);
var
  SrcRow, DestRow, DestRowEnd, SrcPtr, DestPtr, DestEnd: PColorBGRA;
  Pixel: TColorBGRA;
begin
  SrcRow     := Layer.Pixels.PixelPtr[SrcX, SrcY];
  DestRow    := Dest.Data;
  DestRowEnd := DestRow + (DestH * Dest.Width);

  while (DestRow < DestRowEnd) do
  begin
    if (Ratio = 1) then
    begin
      if (Layer.Opacity = ALPHA_OPAQUE) then
        BlendData(DestRow, SrcRow, DestW)
      else
        BlendDataAlpha(DestRow, SrcRow, DestW, Layer.Opacity);
    end else
    begin
      SrcPtr  := SrcRow;
      DestPtr := DestRow;
      DestEnd := DestRow + DestW;
      while (DestPtr < DestEnd) do
      begin
        Pixel := SrcPtr^;
        if (Layer.Opacity <> ALPHA_OPAQUE) then
          Pixel.A := Pixel.A * Layer.Opacity div 255;

        if (Pixel.A = ALPHA_OPAQUE) then
          DestPtr^ := Pixel
        else if (Pixel.A <> ALPHA_TRANSPARENT) then
          BlendPixel(DestPtr, @Pixel);

        Inc(SrcPtr, Ratio);
        Inc(DestPtr);
      end;
    end;

    Inc(SrcRow, Layer.Pixels.Width * Ratio);
    Inc(DestRow, Dest.Width);
  end;
end;

procedure TSimbaImageBoxRenderer.BlendLayers(const Layers: TSimbaImageBoxRenderLayers; SrcX, SrcY, Ratio, W, H: Integer);
var
  I: Integer;
begin
  for I := 0 to High(Layers) do
    BlendLayer(Layers[I], SrcX, SrcY, Ratio, FCanvasImage, W, H);
end;

constructor TSimbaImageBoxRenderer.Create;
begin
  inherited Create();

  FCanvas := TSimbaCanvas.Create();
  FCanvasImage := TSimbaImage.Create();
  FScaleImage := TSimbaImage.Create();
end;

destructor TSimbaImageBoxRenderer.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FCanvasImage);
  FreeAndNil(FScaleImage);
  FreeAndNil(FBlitBitmap);

  inherited Destroy();
end;

function TSimbaImageBoxRenderer.Blit(Dest: TCanvas; Src: TSimbaImage; SrcX, SrcY, W, H: Integer): Boolean;
begin
  if (FBlitBitmap = nil) then
    FBlitBitmap := TBitmap.Create();

  LazImage_FromData(FBlitBitmap, Src.PixelPtr[SrcX, SrcY], W, H, Src.BytesPerRow);
  BitBlt(Dest.Handle, 0, 0, W, H, FBlitBitmap.Canvas.Handle, 0, 0, SRCCOPY);

  FBlitName := 'LCL';
  Result := True;
end;

procedure TSimbaImageBoxRenderer.GrowImage(Img: TSimbaImage; W, H: Integer);
begin
  if (Img.Width < W) or (Img.Height < H) then
    Img.SetSize(
      Max(Img.Width,  W + IMAGEBOX_BUFFER_SLACK),
      Max(Img.Height, H + IMAGEBOX_BUFFER_SLACK)
    );
end;

procedure TSimbaImageBoxRenderer.BeginCanvas(const R: TRect; W, H, Ratio: Integer);
var
  Shift: Integer;
begin
  Shift := 0;
  while ((1 shl Shift) < Ratio) do
    Inc(Shift);
  if ((1 shl Shift) <> Ratio) then
    SimbaException('SimbaImageBoxRenderer: ratio must be a power of two, got %d', [Ratio]);

  GrowImage(FCanvasImage, W, H);

  FCanvas.SetData(FCanvasImage.Data, FCanvasImage.Width, W, H);
  FCanvas.SetTransform(TPoint.Create(-R.Left, -R.Top), Shift);
end;

procedure TSimbaImageBoxRenderer.EndCanvas;
begin
  FCanvas.SetData(nil, 0, 0, 0);
end;

function TSimbaImageBoxRenderer.RenderNormal(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; R: TRect; Overlay: TSimbaImageBoxOverlayEvent): TPoint;
var
  W, H: Integer;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  if (W < 1) or (H < 1) then
    Exit(TPoint.Create(0, 0));

  Result := TPoint.Create(W, H);

  if Assigned(Overlay) or (Length(Layers) > 0) then
  begin
    BeginCanvas(R, W, H, 1);
    try
      CopyPixels(Background, R.Left, R.Top, W, H, FCanvasImage);
      BlendLayers(Layers, R.Left, R.Top, 1, W, H);
      if Assigned(Overlay) then
        Overlay(FCanvas, R);
    finally
      EndCanvas();
    end;

    Blit(Dest, FCanvasImage, 0, 0, W, H);
  end else
    // nothing draws on top: the background goes to screen
    Blit(Dest, Background, R.Left, R.Top, W, H);
end;

function TSimbaImageBoxRenderer.RenderZoomedIn(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; R: TRect; Ratio: Integer; Overlay: TSimbaImageBoxOverlayEvent): TPoint;
var
  W, H: Integer;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  if (W < 1) or (H < 1) then
    Exit(TPoint.Create(0, 0));

  Result := TPoint.Create(W * Ratio, H * Ratio);

  GrowImage(FScaleImage, Result.X, Result.Y);

  if Assigned(Overlay) or (Length(Layers) > 0) then
  begin
    BeginCanvas(R, W, H, 1);
    try
      CopyPixels(Background, R.Left, R.Top, W, H, FCanvasImage);
      BlendLayers(Layers, R.Left, R.Top, 1, W, H);
      if Assigned(Overlay) then
        Overlay(FCanvas, R);
    finally
      EndCanvas(); // see RenderNormal
    end;

    ScaleUp(Ratio, FCanvasImage, 0, 0, W, H, FScaleImage);
  end else
    ScaleUp(Ratio, Background, R.Left, R.Top, W, H, FScaleImage);

  Blit(Dest, FScaleImage, 0, 0, Result.X, Result.Y);
end;

function TSimbaImageBoxRenderer.RenderZoomedOut(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; R: TRect; Ratio: Integer; Overlay: TSimbaImageBoxOverlayEvent): TPoint;
var
  W, H: Integer;
begin
  W := (R.Right - R.Left) div Ratio;
  H := (R.Bottom - R.Top) div Ratio;
  if (W < 1) or (H < 1) then
    Exit(TPoint.Create(0, 0));

  Result := TPoint.Create(W, H);

  R.Right  := R.Left + (W * Ratio);
  R.Bottom := R.Top + (H * Ratio);

  if Assigned(Overlay) or (Length(Layers) > 0) then
  begin
    BeginCanvas(R, W, H, Ratio);
    try
      ScaleDown(Ratio, Background, R.Left, R.Top, FCanvasImage, W, H);
      BlendLayers(Layers, R.Left, R.Top, Ratio, W, H);
      if Assigned(Overlay) then
        Overlay(FCanvas, R);
    finally
      EndCanvas();
    end;

    Blit(Dest, FCanvasImage, 0, 0, W, H);
  end else
  begin
    GrowImage(FScaleImage, W, H);
    ScaleDown(Ratio, Background, R.Left, R.Top, FScaleImage, W, H);

    Blit(Dest, FScaleImage, 0, 0, W, H);
  end;
end;

function TSimbaImageBoxRenderer.Render(Dest: TCanvas; Background: TSimbaImage; const Layers: TSimbaImageBoxRenderLayers; const ImageRect: TRect;
                                      Ratio: Integer; ZoomIn: Boolean;
                                      Overlay: TSimbaImageBoxOverlayEvent): TPoint;
var
  R: TRect;
begin
  R.Left   := Max(ImageRect.Left, 0);
  R.Top    := Max(ImageRect.Top, 0);
  R.Right  := Min(ImageRect.Right, Background.Width);
  R.Bottom := Min(ImageRect.Bottom, Background.Height);

  if (Ratio <= 1) then
    Result := RenderNormal(Dest, Background, Layers, R, Overlay)
  else if ZoomIn then
    Result := RenderZoomedIn(Dest, Background, Layers, R, Ratio, Overlay)
  else
    Result := RenderZoomedOut(Dest, Background, Layers, R, Ratio, Overlay);
end;

end.
