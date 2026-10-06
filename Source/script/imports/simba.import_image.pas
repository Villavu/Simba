unit simba.import_image;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportSimbaImage(Script: TSimbaScript);

implementation

uses
  Graphics,
  lptypes,
  simba.baseclass, simba.canvas, simba.image, simba.image_drawtext,
  simba.script_objectutil;

type
  PSimbaCanvas = ^TSimbaCanvas;
  PBitmap = ^TBitmap;

(*
Image
=====
TImage is a data type that holds an image.

This is used manipulate and process an image such as resizing, rotating, bluring and much more.
Or simply get/set a pixel color at a given (x,y) coord.

```{note}
Images are now objects so there is no need to free them - it is done automatically once they go out of scope.
```
*)

(*
EImageResizeAlgo
----------------
```
enum(NEAREST_NEIGHBOUR, BILINEAR, LANCZOS, BOX, HAMMING, BICUBIC)
```
The resampling filter used by `TImage.Resize`, from fastest to highest quality:

- `NEAREST_NEIGHBOUR`: No blending - fastest, but blocky. Good for pixel-art.
- `BOX`: Simple averaging - cheap, mainly for downscaling.
- `BILINEAR`: Fast and smooth, but a little soft.
- `HAMMING`: A little sharper than bilinear.
- `BICUBIC`: Smooth and sharp - a good default.
- `LANCZOS`: Sharpest and highest quality, but the slowest.

![enlarging comparison](../../images/resample_upscale.png)
![downscale and rebuild comparison](../../images/resample_roundtrip.png)

```{note}
This enum is scoped so use like `EImageResizeAlgo.BILINEAR`
```
*)

(*
EImageThresholdAlgo
-------------------
```
enum(MEAN, WOLF, GAUSSIAN)
```
How `TImage.Threshold` picks each pixel's threshold from the window around it. All three adapt to the image, so uneven lighting, shadows and gradients are handled:

- `MEAN`: The window's average. The pixel is compared as the average of itself and its 8 neighbours, so grain and noise don't turn into speckles.
- `WOLF`: The window's average, lowered where the window has little contrast compared to the rest of the image. The cleanest background, but faint text and thin lines can drop out. The slowest of the three.
- `GAUSSIAN`: A Gaussian weighted average of the window, the nearer pixels counting more. The fastest, and good for text and game UI. Large solid dark areas come out as outlines.

![threshold comparison](../../images/threshold_algos.png)

```{note}
This enum is scoped so use like `EImageThresholdAlgo.GAUSSIAN`
```
*)

(*
TImage.Construct
----------------
```
function TImage.Construct: TImage; static;
function TImage.Construct(Width, Height: Integer): TImage; static;
function TImage.Construct(FileName: String): TImage; static;
```

Constructors for TImage. Use the `new` keyword for these.

```
var
  img: TImage;
begin
  img := new TImage(100, 200);
  WriteLn(img.Width);
  WriteLn(img.Height);
end;
```
*)
procedure _LapeImage_Construct1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := TSimbaImage.Create();
end;

procedure _LapeImage_Construct2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := TSimbaImage.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeImage_Construct3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := TSimbaImage.Create(PString(Params^[0])^);
end;

procedure _LapeImage_Destroy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  LapeObjectDestroy(PLapeObject(Params^[0]));
end;

(*
TImage.Data
-----------
```
property TImage.Data: PColorBGRA;
```
*)
procedure _LapeImage_Data_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PLapeObjectImage(Params^[0])^^.Data;
end;

(*
TImage.Width
------------
```
property TImage.Width: Integer;
```
*)
procedure _LapeImage_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectImage(Params^[0])^^.Width;
end;

(*
TImage.Height
-------------
```
property TImage.Height: Integer;
```
*)
procedure _LapeImage_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectImage(Params^[0])^^.Height;
end;

(*
TImage.Center
-------------
```
property TImage.Center: TPoint;
```
*)
procedure _LapeImage_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PLapeObjectImage(Params^[0])^^.Center;
end;

(*
TImage.Alpha
------------
```
property TImage.Alpha(X, Y: Integer): Byte;
property TImage.Alpha(X, Y: Integer; Alpha: Byte);
```
*)
procedure _LapeImage_GetAlpha(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PLapeObjectImage(Params^[0])^^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeImage_SetAlpha(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^] := PByte(Params^[3])^;
end;

(*
TImage.GetPixel
---------------
```
property TImage.Pixel(X, Y: Integer): TColor;
property TImage.Pixel(X, Y: Integer; Color: TColor);
```

A TColor has no alpha, so it is written opaque.
*)
procedure _LapeImage_GetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PLapeObjectImage(Params^[0])^^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeImage_SetPixel(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

(*
TImage.GetPixels
----------------
```
function TImage.GetPixels(Points: TPointArray): TColorArray;
```
*)
procedure _LapeImage_GetPixels(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PLapeObjectImage(Params^[0])^^.GetPixels(PPointArray(Params^[1])^);
end;

(*
TImage.SetPixels
----------------
```
procedure TImage.SetPixels(Points: TPointArray; Color: TColor);
```

A TColor has no alpha, so it is written opaque.
*)
procedure _LapeImage_SetPixels1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.SetPixels(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.SetPixels
----------------
```
procedure TImage.SetPixels(Points: TPointArray; Colors: TColorArray);
```

A TColor has no alpha, so it is written opaque.
*)
procedure _LapeImage_SetPixels2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TImage.InImage
--------------
```
function TImage.InImage(X, Y: Integer): Boolean;
```
*)
procedure _LapeImage_InImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectImage(Params^[0])^^.InImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.SetSize
--------------
```
procedure TImage.SetSize(AWidth, AHeight: Integer);
```
*)
procedure _LapeImage_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.SetExternalData
----------------------
```
procedure TImage.SetExternalData(NewData: PColorBGRA; DataWidth, DataHeight: Integer);
```

Point the image data to external data (ie. not data allocated by the image itself).

`NewData` of `nil` undoes this: the image owns its data again, as a new empty `DataWidth` by `DataHeight` image.
*)
procedure _LapeImage_SetExternalData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.SetExternalData(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;


(*
TImage.Fill
-----------
```
procedure TImage.Fill(Color: TColor);
```

Fill the entire image with a color. A TColor has no alpha, so it is written opaque.
*)
procedure _LapeImage_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Canvas.Fill(PColor(Params^[1])^);
end;

(*
TImage.FillWithAlpha
--------------------
```
procedure TImage.FillWithAlpha(Value: Byte);
```

Set the entire images alpha value.
*)
procedure _LapeImage_FillWithAlpha(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Canvas.FillWithAlpha(PByte(Params^[1])^);
end;

(*
TImage.Clear
------------
```
procedure TImage.Clear;
```

Fills the entire image with the default pixel.
*)
procedure _LapeImage_Clear1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Canvas.Clear();
end;

(*
TImage.Clear
------------
```
procedure TImage.Clear(Area: TBox);
```

Fills the given area with the default pixel.
*)
procedure _LapeImage_Clear2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Canvas.Clear(PBox(Params^[1])^);
end;

(*
TImage.ClearInverted
--------------------
```
procedure TImage.ClearInverted(Area: TBox);
```

Fills everything but given area with the default pixel.
*)
procedure _LapeImage_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Canvas.ClearInverted(PBox(Params^[1])^);
end;

(*
TImage.Copy
-----------
```
function TImage.Copy(Box: TBox): TImage;
```
*)
procedure _LapeImage_Copy1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Copy(PBox(Params^[1])^);
end;

(*
TImage.Copy
-----------
```
function TImage.Copy: TImage;
```
*)
procedure _LapeImage_Copy2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Copy();
end;

(*
TImage.Crop
-----------
```
procedure TImage.Crop(Box: TBox);
```
*)
procedure _LapeImage_Crop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Crop(PBox(Params^[1])^)
end;

(*
TImage.Pad
----------
```
procedure TImage.Pad(Amount: Integer);
```

Pad an `Amount` pixel border around the entire image.
*)
procedure _LapeImage_Pad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Pad(PInteger(Params^[1])^);
end;

(*
TImage.Offset
-------------
```
procedure TImage.Offset(X,Y: Integer);
```

Offset the entire images content within itself.
*)
procedure _LapeImage_Offset(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.ToChannels
-----------------
```
procedure TImage.ToChannels(var B,G,R: TByteArray);
```

Splits the image into one array per colour channel, each Width * Height long.
*)
procedure _LapeImage_ToChannels1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.ToChannels(PByteArray(Params^[1])^, PByteArray(Params^[2])^, PByteArray(Params^[3])^);
end;

(*
TImage.ToChannels
-----------------
```
procedure TImage.ToChannels(var B,G,R,A: TByteArray);
```

Splits the image into one array per channel, alpha included, each Width * Height long.
*)
procedure _LapeImage_ToChannels2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.ToChannels(PByteArray(Params^[1])^, PByteArray(Params^[2])^, PByteArray(Params^[3])^, PByteArray(Params^[4])^);
end;

(*
TImage.FromChannels
-------------------
```
procedure TImage.FromChannels(const B,G,R: TByteArray; W, H: Integer);
```

The image is sized W * H and built from the three channels, fully opaque.
*)
procedure _LapeImage_FromChannels1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromChannels(PByteArray(Params^[1])^, PByteArray(Params^[2])^, PByteArray(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

(*
TImage.FromChannels
-------------------
```
procedure TImage.FromChannels(const B,G,R,A: TByteArray; W, H: Integer);
```

The image is sized W * H and built from the four channels, alpha included.
*)
procedure _LapeImage_FromChannels2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromChannels(PByteArray(Params^[1])^, PByteArray(Params^[2])^, PByteArray(Params^[3])^, PByteArray(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

(*
TImage.ToColors
---------------
```
function TImage.ToColors: TColorArray;
function TImage.ToColors(Box: TBox): TColorArray;
```
*)
procedure _LapeImage_GetColors1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PLapeObjectImage(Params^[0])^^.ToColors();
end;

procedure _LapeImage_GetColors2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PLapeObjectImage(Params^[0])^^.ToColors(PBox(Params^[1])^);
end;

(*
TImage.ReplaceColor
-------------------
```
procedure TImage.ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single);
```
*)
procedure _LapeImage_ReplaceColor(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.ReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.ReplaceColorBinary
-------------------------
```
procedure TImage.ReplaceColorBinary(Invert: Boolean; Color: TColor; Tolerance: Single = 0);
```
*)
procedure _LapeImage_ReplaceColorBinary1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.ReplaceColorBinary(PBoolean(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.ReplaceColorBinary
-------------------------
```
procedure TImage.ReplaceColorBinary(Invert: Boolean; Colors: TColorArray; Tolerance: Single = 0);
```
*)
procedure _LapeImage_ReplaceColorBinary2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.ReplaceColorBinary(PBoolean(Params^[1])^, PColorArray(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.Resize
-------------
```
function TImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TImage;
```
Returns a new image resized to `NewWidth` by `NewHeight`, sampled with the given `Algo` filter (see `EImageResizeAlgo`). The original image is left unchanged.
*)
procedure _LapeImage_Resize1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Resize(EImageResizeAlgo(Params^[1]^), PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.Resize
-------------
```
function TImage.Resize(Algo: EImageResizeAlgo; Scale: Single): TImage;
```
Resizes by a `Scale` factor instead of absolute dimensions - e.g. `0.5` halves the width and height and `2.0` doubles them.
*)
procedure _LapeImage_Resize2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Resize(EImageResizeAlgo(Params^[1]^), PSingle(Params^[2])^);
end;

(*
TImage.Resize
-------------
```
function TImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer; IgnorePoints: TPointArray): TImage;
```
Resize, but source pixels listed in `IgnorePoints` are excluded from sampling.
*)
procedure _LapeImage_Resize3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Resize(EImageResizeAlgo(Params^[1]^), PInteger(Params^[2])^, PInteger(Params^[3])^, PPointArray(Params^[4])^);
end;

(*
TImage.Rotate
-------------
```
function TImage.Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TSimbaImage;
```
*)
procedure _LapeImage_Rotate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Rotate(EImageRotateAlgo(Params^[1]^), PSingle(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TImage.Mirror
-------------
```
function TImage.Mirror(Style: EImageMirrorStyle): TImage;
```
*)
procedure _LapeImage_Mirror(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Mirror(EImageMirrorStyle(Params^[1]^));
end;


(*
TImage.Sobel
------------
```
function TImage.Sobel: TImage;
```

Applies a sobel overator on the image, and returns it.
*)
procedure _LapeImage_Sobel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Sobel();
end;

(*
TImage.GreyScale
----------------
```
function TImage.GreyScale: TImage;
```
*)
procedure _LapeImage_GreyScale(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.GreyScale();
end;

(*
TImage.Brightness
-----------------
```
function TImage.Brightness(Value: Integer): TImage;
```
*)
procedure _LapeImage_Brightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Brightness(PInteger(Params^[1])^);
end;

(*
TImage.Invert
-------------
```
function TImage.Invert: TImage;
```
*)
procedure _LapeImage_Invert(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Invert();
end;

(*
TImage.Convolute
----------------
```
function TImage.Convolute(Matrix: TDoubleMatrix): TImage;
```

Returns a full convolution with the given mask (Srouce?mask).
```{hint}
Mask should not be very large, as that would be really slow to proccess.
```
*)
procedure _LapeImage_Convolute(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Convolute(PDoubleMatrix(Params^[1])^);
end;

(*
TImage.Threshold
----------------
```
function TImage.Threshold(Algo: EImageThresholdAlgo; Invert: Boolean = False; Radius: Integer = 10): TImage;
function TImage.Threshold(Algo: EImageThresholdAlgo; Invert: Boolean; Radius: Integer; C: Single): TImage;
```

Returns a black and white image: white where a pixel's grey value is at or above its threshold, black elsewhere (the other way round with `Invert`). `Algo` picks how the threshold is found, see `EImageThresholdAlgo`.

- `Radius`: The window is the square of `2 * Radius + 1` pixels around each pixel, for `GAUSSIAN` it sets the size of the Gaussian. Use a window a few times larger than the text or objects you want to keep.
- `C`: For `MEAN` and `GAUSSIAN` it is subtracted from the threshold, in grey levels, so larger values turn more pixels white. For `WOLF` it is its `k` weight, typically 0.2 to 0.5.

Without `C` each algorithm uses a value that suits most images: 10 for `MEAN` and `GAUSSIAN`, 0.25 for `WOLF`.

Example:

```
var
  img: TImage;
begin
  img := new TImage('page.png');
  img := img.Threshold(EImageThresholdAlgo.GAUSSIAN); // the default Radius and C
  img.Show();
end;
```
*)
procedure _LapeImage_Threshold1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Threshold(EImageThresholdAlgo(Params^[1]^), PBoolean(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeImage_Threshold2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Threshold(EImageThresholdAlgo(Params^[1]^), PBoolean(Params^[2])^, PInteger(Params^[3])^, PSingle(Params^[4])^);
end;

(*
TImage.BlendFromSurrounding
---------------------------
```
function TImage.BlendFromSurrounding(Points: TPointArray; Size: Integer): TImage;
```

Replaces each point in `Points` with the average colour of the pixels surrounding it.
Useful for erasing specks or blemishes by blending them into the background.

```{image} ../../images/blend_from_surrounding.png
:width: 65%
:alt: blend from surrounding
```
*)
procedure _LapeImage_Blend1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.BlendFromSurrounding(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.BlendFromSurrounding
---------------------------
```
function TImage.BlendFromSurrounding(Points: TPointArray; Size: Integer; IgnorePoints: TPointArray): TImage;
```

BlendFromSurrounding but points in `IgnorePoints` are not sampled from.
*)
procedure _LapeImage_Blend2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.BlendFromSurrounding(PPointArray(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^);
end;

(*
TImage.Blur
-----------
```
function TImage.Blur(Algo: EImageBlurAlgo; Radius: Single): TSimbaImage;
``
Algo can be either `EImageBlurAlgo.BOX` or `EImageBlurAlgo.GAUSS`.

```{note}
Gauss is not true gaussian blur it's an approximation (in linear time).
<https://blog.ivank.net/fastest-gaussian-blur.html>
```
*)
procedure _LapeImage_Blur(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Result)^^ := PLapeObjectImage(Params^[0])^^.Blur(EImageBlurAlgo(Params^[1]^), PSingle(Params^[2])^);
end;

(*
TImage.ToGreyMatrix
-------------------
```
function TImage.ToGreyMatrix: TByteMatrix;
```
*)
procedure _LapeImage_ToGreyMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteMatrix(Result)^ := PLapeObjectImage(Params^[0])^^.ToGreyMatrix();
end;

(*
TImage.ToMatrix
---------------
```
function TImage.ToMatrix: TIntegerMatrix;
```
*)
procedure _LapeImage_ToMatrix1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PLapeObjectImage(Params^[0])^^.ToMatrix();
end;

(*
TImage.ToMatrix
---------------
```
function TImage.ToMatrix(Box: TBox): TIntegerMatrix;
```
*)
procedure _LapeImage_ToMatrix2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PLapeObjectImage(Params^[0])^^.ToMatrix(PBox(Params^[1])^);
end;

(*
TImage.FromMatrix
-----------------
```
procedure TImage.FromMatrix(Matrix: TIntegerMatrix);
```

Resizes the image to the matrix dimensions and draws the matrix.
*)
procedure _LapeImage_FromMatrix1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromMatrix(PIntegerMatrix(Params^[1])^);
end;

(*
TImage.FromMatrix
-----------------
```
procedure TImage.FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0);
```

Resizes the image to the matrix dimensions and draws the matrix, mapping each
value (normalized to 0..1) to a colour with the chosen ColorMapType.

ColorMapType can be:
  0: cold blue -> red
  1: black -> blue -> red
  2: white -> blue -> red
  3: white -> black
  4: black -> white
  5: diverging (blue -> white -> red)
  6: traffic (green -> yellow -> red)
  7: rainbow
  else: the value is used as a hue in degrees (black -> that hue -> white)

![colormaps](../../images/matrix_colormaps.png)
*)
procedure _LapeImage_FromMatrix2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.FromString
-----------------
```
procedure TImage.FromString(Str: String);
```
*)
procedure _LapeImage_FromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromString(PString(Params^[1])^);
end;

(*
TImage.FromZip
--------------
```
procedure TImage.FromZip(ZipFile, ZipEntry: String);
```
*)
procedure _LapeImage_FromZip(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromZip(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TImage.FromData
---------------
```
procedure TImage.FromData(Src: PColorBGRA; SrcWidth, NewWidth, NewHeight: Integer);
```
*)
procedure _LapeImage_FromData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromData(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TImage.Load
-----------
```
procedure TImage.Load(FileName: String);
```
*)
procedure _LapeImage_Load1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Load(PString(Params^[1])^);
end;

(*
TImage.Load
-----------
```
procedure TImage.Load(FileName: String; Area: TBox);
```
*)
procedure _LapeImage_Load2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.Load(PString(Params^[1])^, PBox(Params^[2])^);
end;

(*
TImage.Save
-----------
```
function TImage.Save(FileName: String; OverwriteIfExists: Boolean = False): Boolean;
```
*)
procedure _LapeImage_Save(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectImage(Params^[0])^^.Save(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.ToString
---------------
```
function TImage.ToString: String;
```
*)
procedure _LapeImage_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectImage(Params^[0])^^.ToString();
end;

(*
TImage.Equals
-------------
```
function TImage.Equals(Other: TImage): Boolean;
```

Are the two images exactly equal?

```{note}
Alpha is not taken into account.
```
*)
procedure _LapeImage_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectImage(Params^[0])^^.Equals(PLapeObjectImage(Params^[1])^^);
end;

(*
TImage.Compare
--------------
```
function TImage.Compare(Other: TImage): Single;
```
*)
procedure _LapeImage_Compare(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PLapeObjectImage(Params^[0])^^.Compare(PLapeObjectImage(Params^[1])^^);
end;

(*
TImage.PixelDifference
----------------------
```
function TImage.PixelDifference(Other: TImage; Tolerance: Single = 0): TPointArra;
```
*)
procedure _LapeImage_PixelDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectImage(Params^[0])^^.PixelDifference(PLapeObjectImage(Params^[1])^^, PSingle(Params^[2])^);
end;

(*
TImage.ToLazBitmap
------------------
```
function TImage.ToLazBitmap: TLazBitmap;
```
*)
procedure _LapeImage_ToLazBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PLapeObjectImage(Params^[0])^^.ToLazBitmap();
end;

(*
TImage.FromLazBitmap
--------------------
```
procedure TImage.FromLazBitmap(LazBitmap: TLazBitmap);
```
*)
procedure _LapeImage_FromLazBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectImage(Params^[0])^^.FromLazBitmap(PBitmap(Params^[1])^);
end;

(*
TImage.FindColor
----------------
```
function TImage.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox): TPointArray;
```
*)
procedure _LapeImage_FindColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectImage(Params^[0])^^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TImage.FindImage
----------------
```
function TImage.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox): TPoint;
```
*)
procedure _LapeImage_FindImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PLapeObjectImage(Params^[0])^^.FindImage(PLapeObjectImage(Params^[1])^^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TImage.GetLoadedImages
----------------------
```
function GetLoadedImages: TImageArray;
```

Returns an array of all the loaded images.
*)
procedure _LapeImage_GetLoadedImages(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  TLapeObjectArray = array of TLapeObject;
var
  Imgs: TSimbaImageArray;
  Arr: TLapeObjectArray;
  I: Integer;
begin
  Imgs := TSimbaImageArray(GetSimbaObjectsOfClass(TSimbaImage));
  SetLength(Arr, Length(Imgs));
  for I := 0 to High(Imgs) do
    Arr[i] := LapeObjectAlloc(Imgs[I], False);

  TLapeObjectArray(Result^) := Arr;
end;

(*
TImage.Show
-----------
```
procedure TImage.Show(EnsureVisible: Boolean = True);
```

Show a image on the debug image.
*)

procedure _LapeCanvas_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
var
  Img: TSimbaImage;
begin
  Img := PLapeObjectImage(Params^[1])^^;
  PSimbaCanvas(Params^[0])^.DrawImage(Img.Data, Img.Width, Img.Height, PPoint(Params^[2])^);
end;

(*
TImage.Canvas
-------------
```
property TImage.Canvas: TCanvas;
```

Everything that draws on the image lives here: the colours, the font, and every DrawXX.

```
var
  img: TImage;
begin
  img := new TImage(100, 100);
  img.Canvas.DrawColor := $00FF00;
  img.Canvas.DrawBox([10, 10, 90, 90]);
end;
```
*)
procedure _LapeImage_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaCanvas(Result)^ := PLapeObjectImage(Params^[0])^^.Canvas;
end;

procedure ImportSimbaImage(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Image';

    LapeObjectImport(Script.Compiler, 'TImage');

    addGlobalType('array of TImage', 'TImageArray');
    addGlobalType('enum(WIDTH, HEIGHT, LINE)', 'EImageMirrorStyle');
    addGlobalType('enum(NEAREST_NEIGHBOUR, BILINEAR, LANCZOS, BOX, HAMMING, BICUBIC)', 'EImageResizeAlgo');
    addGlobalType('enum(NEAREST_NEIGHBOUR, BILINEAR)', 'EImageRotateAlgo');
    addGlobalType('enum(BOX, GAUSS)', 'EImageBlurAlgo');
    addGlobalType('enum(MEAN, WOLF, GAUSSIAN)', 'EImageThresholdAlgo');

    addGlobalFunc('function TImage.Construct: TImage; static; overload;', @_LapeImage_Construct1);
    addGlobalFunc('function TImage.Construct(Width, Height: Integer): TImage; static; overload', @_LapeImage_Construct2);
    addGlobalFunc('function TImage.Construct(FileName: String): TImage; static; overload', @_LapeImage_Construct3);
    addGlobalFunc('procedure TImage.Destroy;', @_LapeImage_Destroy);

    addProperty('TImage', 'Data', 'PColorBGRA', @_LapeImage_Data_Read);
    addProperty('TImage', 'Canvas', 'TCanvas', @_LapeImage_Canvas_Read);
    addGlobalFunc('procedure TCanvas.DrawImage(Image: TImage; Position: TPoint);', @_LapeCanvas_DrawImage);
    addProperty('TImage', 'Width', 'Integer', @_LapeImage_Width_Read);
    addProperty('TImage', 'Height', 'Integer', @_LapeImage_Height_Read);
    addProperty('TImage', 'Center', 'TPoint', @_LapeImage_Center_Read);



    addPropertyIndexed('TImage', 'Alpha', 'X, Y: Integer', 'Byte', @_LapeImage_GetAlpha, @_LapeImage_SetAlpha);
    addPropertyIndexed('TImage', 'Pixel', 'X, Y: Integer', 'TColor', @_LapeImage_GetPixel, @_LapeImage_SetPixel);

    addGlobalFunc('function TImage.GetPixels(Points: TPointArray): TColorArray;', @_LapeImage_GetPixels);
    addGlobalFunc('procedure TImage.SetPixels(Points: TPointArray; Color: TColor); overload', @_LapeImage_SetPixels1);
    addGlobalFunc('procedure TImage.SetPixels(Points: TPointArray; Colors: TColorArray); overload', @_LapeImage_SetPixels2);

    addGlobalFunc('function TImage.InImage(X, Y: Integer): Boolean', @_LapeImage_InImage);

    addGlobalFunc('procedure TImage.SetSize(NewWidth, NewHeight: Integer);', @_LapeImage_SetSize);
    addGlobalFunc('procedure TImage.SetExternalData(Data: PColorBGRA; DataWidth, DataHeight: Integer);', @_LapeImage_SetExternalData);

    addGlobalFunc('procedure TImage.Fill(Color: TColor);', @_LapeImage_Fill);
    addGlobalFunc('procedure TImage.FillWithAlpha(Value: Byte);', @_LapeImage_FillWithAlpha);
    addGlobalFunc('procedure TImage.Clear; overload', @_LapeImage_Clear1);
    addGlobalFunc('procedure TImage.Clear(Area: TBox); overload', @_LapeImage_Clear2);
    addGlobalFunc('procedure TImage.ClearInverted(Area: TBox);', @_LapeImage_ClearInverted);

    addGlobalFunc('function TImage.Copy(Box: TBox): TImage; overload', @_LapeImage_Copy1);
    addGlobalFunc('function TImage.Copy: TImage; overload', @_LapeImage_Copy2);
    addGlobalFunc('procedure TImage.Crop(Box: TBox);', @_LapeImage_Crop);
    addGlobalFunc('procedure TImage.Pad(Amount: Integer)', @_LapeImage_Pad);
    addGlobalFunc('procedure TImage.Offset(X,Y: Integer)', @_LapeImage_Offset);

    addGlobalFunc('procedure TImage.ToChannels(var B,G,R: TByteArray); overload', @_LapeImage_ToChannels1);
    addGlobalFunc('procedure TImage.ToChannels(var B,G,R,A: TByteArray); overload', @_LapeImage_ToChannels2);
    addGlobalFunc('procedure TImage.FromChannels(const B,G,R: TByteArray; W, H: Integer); overload', @_LapeImage_FromChannels1);
    addGlobalFunc('procedure TImage.FromChannels(const B,G,R,A: TByteArray; W, H: Integer); overload', @_LapeImage_FromChannels2);

    addGlobalFunc('function TImage.ToColors: TColorArray; overload', @_LapeImage_GetColors1);
    addGlobalFunc('function TImage.ToColors(Box: TBox): TColorArray; overload', @_LapeImage_GetColors2);

    addGlobalFunc('procedure TImage.ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single = 0)', @_LapeImage_ReplaceColor);
    addGlobalFunc('procedure TImage.ReplaceColorBinary(Invert: Boolean; Color: TColor; Tolerance: Single = 0); overload', @_LapeImage_ReplaceColorBinary1);
    addGlobalFunc('procedure TImage.ReplaceColorBinary(Invert: Boolean; Colors: TColorArray; Tolerance: Single = 0); overload', @_LapeImage_ReplaceColorBinary2);

    addGlobalFunc('function TImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TImage; overload;', @_LapeImage_Resize1);
    addGlobalFunc('function TImage.Resize(Algo: EImageResizeAlgo; Scale: Single): TImage; overload;', @_LapeImage_Resize2);
    addGlobalFunc('function TImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer; IgnorePoints: TPointArray): TImage; overload;', @_LapeImage_Resize3);
    addGlobalFunc('function TImage.Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TImage;', @_LapeImage_Rotate);
    addGlobalFunc('function TImage.Mirror(Style: EImageMirrorStyle): TImage', @_LapeImage_Mirror);







    




    addGlobalFunc('function TImage.Sobel: TImage', @_LapeImage_Sobel);
    addGlobalFunc('function TImage.GreyScale: TImage', @_LapeImage_GreyScale);
    addGlobalFunc('function TImage.Brightness(Value: Integer): TImage', @_LapeImage_Brightness);
    addGlobalFunc('function TImage.Invert: TImage', @_LapeImage_Invert);
    addGlobalFunc('function TImage.Convolute(Matrix: TDoubleMatrix): TImage', @_LapeImage_Convolute);
    addGlobalFunc('function TImage.Threshold(Algo: EImageThresholdAlgo; Invert: Boolean = False; Radius: Integer = 10): TImage; overload', @_LapeImage_Threshold1);
    addGlobalFunc('function TImage.Threshold(Algo: EImageThresholdAlgo; Invert: Boolean; Radius: Integer; C: Single): TImage; overload', @_LapeImage_Threshold2);
    addGlobalFunc('function TImage.BlendFromSurrounding(Points: TPointArray; Radius: Integer): TImage; overload', @_LapeImage_Blend1);
    addGlobalFunc('function TImage.BlendFromSurrounding(Points: TPointArray; Radius: Integer; IgnorePoints: TPointArray): TImage; overload', @_LapeImage_Blend2);
    addGlobalFunc('function TImage.Blur(Algo: EImageBlurAlgo; Radius: Single): TImage;', @_LapeImage_Blur);

    addGlobalFunc('function TImage.ToGreyMatrix: TByteMatrix', @_LapeImage_ToGreyMatrix);
    addGlobalFunc('function TImage.ToMatrix: TIntegerMatrix; overload', @_LapeImage_ToMatrix1);
    addGlobalFunc('function TImage.ToMatrix(Box: TBox): TIntegerMatrix; overload', @_LapeImage_ToMatrix2);
    addGlobalFunc('procedure TImage.FromMatrix(Matrix: TIntegerMatrix); overload', @_LapeImage_FromMatrix1);
    addGlobalFunc('procedure TImage.FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0); overload', @_LapeImage_FromMatrix2);
    addGlobalFunc('procedure TImage.FromString(Str: String)', @_LapeImage_FromString);
    addGlobalFunc('procedure TImage.FromZip(ZipFile: String; ZipEntry: String)', @_LapeImage_FromZip);
    addGlobalFunc('procedure TImage.FromData(Src: PColorBGRA; SrcWidth, NewWidth, NewHeight: Integer)', @_LapeImage_FromData);
    addGlobalFunc('procedure TImage.Load(FileName: String); overload', @_LapeImage_Load1);
    addGlobalFunc('procedure TImage.Load(FileName: String; Area: TBox); overload', @_LapeImage_Load2);
    addGlobalFunc('function TImage.Save(FileName: String; OverwriteIfExists: Boolean = False): Boolean;', @_LapeImage_Save);
    addGlobalFunc('function TImage.ToString: String;', @_LapeImage_ToString);

    addGlobalFunc('function TImage.Equals(Other: TImage): Boolean;', @_LapeImage_Equals);
    addGlobalFunc('function TImage.Compare(Other: TImage): Single;', @_LapeImage_Compare);
    addGlobalFunc('function TImage.PixelDifference(Other: TImage; Tolerance: Single = 0): TPointArray', @_LapeImage_PixelDifference);

    addGlobalFunc('function TImage.ToLazBitmap: TLazBitmap;', @_LapeImage_ToLazBitmap);
    addGlobalFunc('procedure TImage.FromLazBitmap(LazBitmap: TLazBitmap);', @_LapeImage_FromLazBitmap);

    addGlobalFunc('function TImage.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;', @_LapeImage_FindColor);
    addGlobalFunc('function TImage.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;', @_LapeImage_FindImage);

    DumpSection := '';

    addGlobalFunc('function GetLoadedImages: TImageArray', @_LapeImage_GetLoadedImages);
  end;
end;

end.
