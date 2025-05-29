unit simba.import_image;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.baseclass, simba.script;

procedure ImportSimbaImage(Script: TSimbaScript);

implementation

uses
  Graphics,
  lptypes,
  simba.image, simba.image_textdrawer, simba.colormath,
  simba.vartype_polygon, simba.vartype_quad, simba.vartype_circle, simba.script_importutil;

type
  PBitmap = ^TBitmap;
  PQuad = ^TQuad;
  PQuadArray = ^TQuadArray;
  PSimbaImage = ^TSimbaImage;
  PSimbaImageArray = ^TSimbaImageArray;

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
  PLapeObject(Result)^ := TSimbaImage.Create();
end;

procedure _LapeImage_Construct2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := TSimbaImage.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeImage_Construct3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := TSimbaImage.Create(PString(Params^[0])^);
end;

procedure _LapeImage_Destroy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if IsManaging(PLapeObject(Params^[0])) and Assigned(PSimbaImage(PLapeObject(Params^[0])^)^) then
    FreeAndNil(PSimbaImage(PLapeObject(Params^[0])^)^);
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
  PPointer(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Data;
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
  PInteger(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Width;
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
  PInteger(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Height;
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
  PPoint(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Center;
end;

(*
TImage.DefaultPixel
-------------------
```
property TImage.DefaultPixel: TColorBGRA;
property TImage.DefaultPixel(Value: TColorBGRA);
```
*)
procedure _LapeImage_DefaultPixel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorBGRA(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.DefaultPixel;
end;

procedure _LapeImage_DefaultPixel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DefaultPixel := PColorBGRA(Params^[1])^;
end;

(*
TImage.DrawColor
----------------
```
property TImage.DrawColor: TColor;
property TImage.DrawColor(Color: TColor);
```
The current drawing color.

```{note}
Red is the default value.
```
*)
procedure _LapeImage_DrawColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.DrawColor;
end;

procedure _LapeImage_DrawColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawColor := PColor(Params^[1])^;
end;

(*
TImage.DrawAlpha
----------------
```
property TImage.DrawAlpha: Byte;
property TImage.DrawAlpha(Value: Byte);
```

The current draw alpha.
0 is completely transparent and 255 is completely opauge.

```{note}
255 is the default value.
```
*)
procedure _LapeImage_DrawAlpha_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByte(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.DrawAlpha;
end;

procedure _LapeImage_DrawAlpha_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawAlpha := PByte(Params^[1])^;
end;

(*
TImage.FontName
---------------
```
property TImage.FontName: String;
property TImage.FontName(Value: String);
```
*)
procedure _LapeImage_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FontName;
end;

procedure _LapeImage_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FontName := PString(Params^[1])^;
end;

(*
TImage.FontSize
---------------
```
property TImage.FontSize: Single;
property TImage.FontSize(Value: Single);
```
*)
procedure _LapeImage_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FontSize;
end;

procedure _LapeImage_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FontSize := PSingle(Params^[1])^;
end;

(*
TImage.FontAntialiasing
-----------------------
```
property TImage.FontAntialiasing: Boolean;
property TImage.FontAntialiasing(Value: Boolean);
```
*)
procedure _LapeImage_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FontAntialiasing;
end;

procedure _LapeImage_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TImage.FontBold
---------------
```
property TImage.FontBold: Boolean;
property TImage.FontBold(Value: Boolean);
```
*)
procedure _LapeImage_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FontBold;
end;

procedure _LapeImage_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FontBold := PBoolean(Params^[1])^;
end;

(*
TImage.FontItalic
-----------------
```
property TImage.FontItalic: Boolean;
property TImage.FontItalic(Value: Boolean);
```
*)
procedure _LapeImage_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FontItalic;
end;

procedure _LapeImage_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FontItalic := PBoolean(Params^[1])^;
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
  PByte(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeImage_SetAlpha(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.Alpha[PInteger(Params^[1])^, PInteger(Params^[2])^] := PByte(Params^[3])^;
end;

(*
TImage.GetPixel
---------------
```
property TImage.Pixel(X, Y: Integer): TColor;
property TImage.Pixel(X, Y: Integer; Alpha: TColor);
```
*)
procedure _LapeImage_GetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeImage_SetPixel(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.Pixel[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
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
  PColorArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.GetPixels(PPointArray(Params^[1])^);
end;

(*
TImage.SetPixels
----------------
```
procedure TImage.SetPixels(Points: TPointArray; Color: TColor);
```
*)
procedure _LapeImage_SetPixels1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.SetPixels(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.SetPixels
----------------
```
procedure TImage.SetPixels(Points: TPointArray; Colors: TColorArray);
```
*)
procedure _LapeImage_SetPixels2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TImage.SetAlphas
----------------
```
procedure TImage.SetAlphas(Points: TPointArray; Value: Byte);
```
*)
procedure _LapeImage_SetAlphas(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.SetAlphas(PPointArray(Params^[1])^, PByte(Params^[2])^);
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
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.InImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.SetExternalData
----------------------
```
procedure TImage.SetExternalData(NewData: PColorBGRA; DataWidth, DataHeight: Integer);
```

Point the image data to external data (ie. not data allocated by the image itself).
*)
procedure _LapeImage_SetExternalData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.SetExternalData(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.ResetExternalData
------------------------
```
procedure TImage.ResetExternalData;
```

Remove the effects of `SetExternalData`.
*)
procedure _LapeImage_ResetExternalData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.ResetExternalData(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;


(*
TImage.Fill
-----------
```
procedure TImage.Fill(Color: TColor);
```

Fill the entire image with a color.
*)
procedure _LapeImage_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.Fill(PColor(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.FillWithAlpha(PByte(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Clear();
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Clear(PBox(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.ClearInverted(PBox(Params^[1])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Copy(PBox(Params^[1])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Copy();
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Crop(PBox(Params^[1])^)
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Pad(PInteger(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.isBinary
---------------
```
function TImage.isBinary: Boolean;
```

Binary in this context means the entire image is either filled with $000000 (black) or $FFFFFF (white) excluding Alpha.
*)
procedure _LapeImage_isBinary(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.isBinary();
end;

(*
TImage.SplitChannels
--------------------
```
procedure TImage.SplitChannels(var B,G,R: TByteArray);
```
*)
procedure _LapeImage_SplitChannels(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.SplitChannels(PByteArray(Params^[1])^, PByteArray(Params^[2])^, PByteArray(Params^[3])^);
end;

(*
TImage.FromChannels
--------------------
```
procedure TImage.FromChannels(const B,G,R: TByteArray; W, H: Integer);
```
*)
procedure _LapeImage_FromChannels(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FromChannels(PByteArray(Params^[1])^, PByteArray(Params^[2])^, PByteArray(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

(*
TImage.GetColors
----------------
```
function TImage.GetColors: TColorArray;
function TImage.GetColors(Box: TBox): TColorArray;
function TImage.GetColors(Points: TPointArray): TColorArray;
```
*)
procedure _LapeImage_GetColors1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.GetColors();
end;

procedure _LapeImage_GetColors2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.GetColors(PBox(Params^[1])^);
end;

procedure _LapeImage_GetColors3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.GetColors(PPointArray(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.ReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.ReplaceColorBinary(PBoolean(Params^[1])^, PColor(Params^[2])^, PSingle(Params^[3])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.ReplaceColorBinary(PBoolean(Params^[1])^, PColorArray(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.Resize
-------------
```
function TImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TSimbaImage;
```
*)
procedure _LapeImage_Resize1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Resize(EImageResizeAlgo(Params^[1]^), PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.Resize
-------------
```
function TImage.Resize(Algo: EImageResizeAlgo; Scale: Single): TSimbaImage;
```
*)
procedure _LapeImage_Resize2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Resize(EImageResizeAlgo(Params^[1]^), PSingle(Params^[2])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Rotate(EImageRotateAlgo(Params^[1]^), PSingle(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TImage.Downsample
-----------------
```
function TImage.Downsample(Scale: Integer): TImage;
```
*)
procedure _LapeImage_DownSample1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Downsample(PInteger(Params^[1])^);
end;

(*
TImage.Downsample
-----------------
```
function TImage.Downsample(Scale: Integer; IgnorePoints: TPointArray): TImage;
```

Downsample but points in `IgnorePoints` are not sampled from.
*)
procedure _LapeImage_DownSample2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Downsample(PInteger(Params^[1])^, PPointArray(Params^[2])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Mirror(EImageMirrorStyle(Params^[1]^));
end;

(*
TImage.TextWidth
----------------
```
function TImage.TextWidth(Text: String): Integer;
```
*)
procedure _LapeImage_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.TextWidth(PString(Params^[1])^);
end;

(*
TImage.TextHeight
-----------------
```
function TImage.TextHeight(Text: String): Integer;
```
*)
procedure _LapeImage_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.TextHeight(PString(Params^[1])^);
end;

(*
TImage.TextSize
---------------
```
function TImage.TextSize(Text: String): TPoint;
```
*)
procedure _LapeImage_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.TextSize(PString(Params^[1])^);
end;

(*
TImage.DrawText
---------------
```
procedure TImage.DrawText(Text: String; Position: TPoint; Color: TColor);
```
*)
procedure _LapeImage_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TImage.DrawText
---------------
```
procedure TImage.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign; Color: TColor);
```
*)
procedure _LapeImage_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, EImageTextAlign(Params^[3]^));
end;

(*
TImage.DrawTextLines
--------------------
```
procedure TImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
```
*)
procedure _LapeImage_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TImage.DrawImage
----------------
```
procedure TImage.DrawImage(Image: TImage; Position: TPoint);
```
*)
procedure _LapeImage_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawImage(PSimbaImage(PLapeObject(Params^[1])^)^, PPoint(Params^[2])^);
end;

(*
TImage.DrawATPA
---------------
```
procedure TImage.DrawATPA(ATPA: T2DPointArray);
```

Draws every TPA in the ATPA.
*)
procedure _LapeImage_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawATPA(P2DPointArray(Params^[1])^);
end;

(*
TImage.DrawTPA
--------------
```
procedure TImage.DrawTPA(Points: TPointArray);
```

Draws a TPointArray.
*)
procedure _LapeImage_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawTPA(PPointArray(Params^[1])^);
end;

(*
TImage.DrawLine
---------------
```
procedure TImage.DrawLine(Start, Stop: TPoint);
```
*)
procedure _LapeImage_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TImage.DrawLineGap
------------------
```
procedure TImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer);
```
*)
procedure _LapeImage_DrawLineGap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawLineGap(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.DrawCrosshairs
---------------------
```
procedure TImage.DrawCrosshairs(ACenter: TPoint; Size: Integer);
```
*)
procedure _LapeImage_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.DrawCross
----------------
```
procedure TImage.DrawCross(ACenter: TPoint; Radius: Integer);
```
*)
procedure _LapeImage_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.DrawBox
--------------
```
procedure TImage.DrawBox(B: TBox);
```
*)
procedure _LapeImage_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawBox(PBox(Params^[1])^);
end;

(*
TImage.DrawBoxFilled
--------------------
```
procedure TImage.DrawBoxFilled(B: TBox);
```
*)
procedure _LapeImage_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawBoxFilled(PBox(Params^[1])^);
end;

(*
TImage.DrawBoxInverted
----------------------
```
procedure TImage.DrawBoxInverted(B: TBox);
```
*)
procedure _LapeImage_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawBoxInverted(PBox(Params^[1])^);
end;

(*
TImage.DrawPolygon
------------------
```
procedure TImage.DrawPolygon(Points: TPolygon);
```
*)
procedure _LapeImage_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawPolygon(PPolygon(Params^[1])^);
end;

(*
TImage.DrawPolygonFilled
------------------------
```
procedure TImage.DrawPolygonFilled(Points: TPolygon);
```
*)
procedure _LapeImage_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawPolygonFilled(PPolygon(Params^[1])^);
end;

(*
TImage.DrawPolygonInverted
--------------------------
```
procedure TImage.DrawPolygonInverted(Points: TPolygon);
```
*)
procedure _LapeImage_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawPolygonInverted(PPolygon(Params^[1])^);
end;

(*
TImage.DrawQuad
---------------
```
procedure TImage.DrawQuad(B: TBox);
```
*)
procedure _LapeImage_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawQuad(PQuad(Params^[1])^);
end;

(*
TImage.DrawQuadFilled
---------------------
```
procedure TImage.DrawQuadFilled(B: TBox);
```
*)
procedure _LapeImage_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawQuadFilled(PQuad(Params^[1])^);
end;

(*
TImage.DrawQuadInverted
-----------------------
```
procedure TImage.DrawQuadInverted(B: TBox);
```
*)
procedure _LapeImage_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawQuadInverted(PQuad(Params^[1])^);
end;

(*
TImage.DrawCircle
-----------------
```
procedure TImage.DrawCircle(Center: TPoint; Radius: Integer);
procedure TImage.DrawCircle(Circle: TCircle);
```
*)
procedure _LapeImage_DrawCircle1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.DrawCircleFilled
-----------------------
```
procedure TImage.DrawCircleFilled(Center: TPoint; Radius: Integer);
procedure TImage.DrawCircleFilled(Circle: TCircle);
```
*)
procedure _LapeImage_DrawCircleFilled1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.DrawCircleInverted
-------------------------
```
procedure TImage.DrawCircleInverted(Center: TPoint; Radius: Integer);
procedure TImage.DrawCircleInverted(Circle: TCircle);
```
*)
procedure _LapeImage_DrawCircleInverted1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;


procedure _LapeImage_DrawCircle2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircle(Point(TCircle(Params^[1]^).X, TCircle(Params^[1]^).Y), TCircle(Params^[1]^).Radius);
end;

procedure _LapeImage_DrawCircleFilled2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleFilled(Point(TCircle(Params^[1]^).X, TCircle(Params^[1]^).Y), TCircle(Params^[1]^).Radius);
end;

procedure _LapeImage_DrawCircleInverted2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleInverted(Point(TCircle(Params^[1]^).X, TCircle(Params^[1]^).Y), TCircle(Params^[1]^).Radius);
end;

(*
TImage.DrawLineAA
-----------------
```
procedure TImage.DrawLineAA(Start, Stop: TPoint; Color: TColor; Thickness: Single = 1.5);
```
*)
procedure _LapeImage_DrawLineAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawLineAA(PPoint(Params^[1])^, PPoint(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.DrawEllipseAA
--------------------
```
procedure TImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Color: TColor; Thickness: Single = 1.5);
```
*)
procedure _LapeImage_DrawEllipseAA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawEllipseAA(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PSingle(Params^[4])^);
end;

(*
TImage.DrawCircleAA
--------------------
```
procedure TImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Color: TColor; Thickness: Single = 1.5);
procedure TImage.DrawCircleAA(Circle: TCircle; Color: TColor; Thickness: Single = 1.5);
```
*)
procedure _LapeImage_DrawCircleAA1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleAA(TPoint(Params^[1]^), Integer(Params^[2]^), PSingle(Params^[3])^);
end;

procedure _LapeImage_DrawCircleAA2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleAA(Point(TCircle(Params^[1]^).X, TCircle(Params^[1]^).Y), TCircle(Params^[1]^).Radius, PSingle(Params^[3])^);
end;

(*
TImage.DrawQuadArray
--------------------
```
procedure TImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean);
```
*)
procedure _LapeImage_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.DrawBoxArray
-------------------
```
procedure TImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);
```
*)
procedure _LapeImage_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.DrawPolygonArray
-----------------------
```
procedure TImage.DrawPolygonArray(Polygons: TPolygonArray; Filled: Boolean);
```
*)
procedure _LapeImage_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawPolygonArray(PPolygonArray(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.DrawCircleArray
----------------------
```
procedure TImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);
```
*)
procedure _LapeImage_DrawCircleArray1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^);
end;

(*
TImage.DrawCrossArray
---------------------
```
procedure TImage.DrawCrossArray(Points: TPointArray; Radius: Integer);
```
*)
procedure _LapeImage_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.DrawHSLCircle
--------------------
```
procedure TImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
```
*)
procedure _LapeImage_DrawHSLCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.DrawHSLCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Sobel();
end;

(*
TImage.Enhance
--------------
```
function TImage.Enhance(Enchantment: Byte; C: Single): TImage;
```

Enhances colors in the image by a given value.
 - `Enhancement`: How much to substraact or add to the color.
 - `C`: Based on the "mid"-value (127) if color is below then it gets weakened else enchanced.
*)
procedure _LapeImage_Enhance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Enhance(PByte(Params^[1])^, PSingle(Params^[2])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.GreyScale();
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Brightness(PInteger(Params^[1])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Invert();
end;

(*
TImage.Posterize
----------------
```
function TImage.Posterize(Value: Integer): TImage;
```
*)
procedure _LapeImage_Posterize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Posterize(PInteger(Params^[1])^);
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
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Convolute(PDoubleMatrix(Params^[1])^);
end;

(*
TImage.Threshold
----------------
```
function TImage.Threshold(Invert: Boolean = False; C: Integer = 0): TImage;
```

Otsu threshold algorithm.

Invert = Invert output
C = Constant value to add to computed threshold
*)
procedure _LapeImage_Threshold(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Threshold(PBoolean(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.ThresholdAdaptive
----------'-------------
```
function TImage.ThresholdAdaptive(Invert: Boolean = False; Radius: Integer = 25; C: Integer = 0): TImage;
```

Adapative thresholding using local average.

Invert = Invert output
Radius = Window size, must be odd (default = 25)
C      = Constant value to add to computed threshold
*)
procedure _LapeImage_ThresholdAdaptive(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.ThresholdAdaptive(PBoolean(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.ThresholdAdaptiveSauvola
-------------------------------
```
function TImage.ThresholdAdaptiveSauvola(Invert: Boolean = False; Radius: Integer = 25; C: Single = 0.2): TImage;
```

Sauvola binarization algorithm.

Invert = Invert output
Radius = Window size, must be odd (default = 25)
C      = Constant value (default = 0.2). Typical values are between 0.2 and 0.5.
*)
procedure _LapeImage_ThresholdAdaptiveSauvola(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.ThresholdAdaptiveSauvola(PBoolean(Params^[1])^, PInteger(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.Blend
------------
```
function TImage.Blend(Points: TPointArray; Size: Integer): TImage;
```
*)
procedure _LapeImage_Blend1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.Blend
------------
```
function TImage.Blend(Points: TPointArray; Size: Integer; IgnorePoints: TPointArray): TImage;
```

Blend but points in `IgnorePoints` are not sampled from.
*)
procedure _LapeImage_Blend2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^);
end;

(*
TImage.Blur
-----------
```
function TImage.Blur(Algo: EImageBlurAlgo; Radius: Integer): TSimbaImage;
```

Algo can be either EImageBlurAlgo.BOX, EImageBlurAlgo.GAUSS.

```{note}
EImageBlurAlgo.GAUSS is not true gaussian blur it's an approximation (in linear time).

<https://blog.ivank.net/fastest-gaussian-blur.html>
```
*)
procedure _LapeImage_Blur(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Blur(EImageBlurAlgo(Params^[1]^), PInteger(Params^[2])^);
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
  PByteMatrix(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.ToGreyMatrix();
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
  PIntegerMatrix(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.ToMatrix();
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
  PIntegerMatrix(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.ToMatrix(PBox(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.FromMatrix(PIntegerMatrix(Params^[1])^);
end;

(*
TImage.FromMatrix
-----------------
```
procedure TImage.FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0);
```

Resizes the image to the matrix dimensions and draws the matrix.

ColorMapType can be:
  0: cold blue to red
  1: black -> blue -> red
  2: white -> blue -> red
  3: light (to white)
  4: light (to black)
*)
procedure _LapeImage_FromMatrix2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FromMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.FromString(PString(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.FromZip(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TImage.FromData
---------------
```
procedure TImage.FromData(AWidth, AHeight: Integer; Memory: PColorBGRA; DataWidth: Integer);
```
*)
procedure _LapeImage_FromData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(PLapeObject(Params^[0])^)^.FromData(PInteger(Params^[1])^, PInteger(Params^[2])^, PPointer(Params^[3])^, PInteger(Params^[4])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Load(PString(Params^[1])^);
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
  PSimbaImage(PLapeObject(Params^[0])^)^.Load(PString(Params^[1])^, PBox(Params^[2])^);
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
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Save(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.SaveToString
-------------------
```
function TImage.SaveToString: String;
```
*)
procedure _LapeImage_SaveToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.SaveToString();
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
  PBoolean(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Equals(PSimbaImage(PLapeObject(Params^[1])^)^);
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
  PSingle(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.Compare(PSimbaImage(PLapeObject(Params^[1])^)^);
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
  PPointArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.PixelDifference(PSimbaImage(PLapeObject(Params^[1])^)^, PSingle(Params^[2])^);
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
  PBitmap(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.ToLazBitmap();
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
  PSimbaImage(PLapeObject(Params^[0])^)^.FromLazBitmap(PBitmap(Params^[1])^);
end;

(*
TImage.LoadFonts
----------------
```
function TImage.LoadFonts(Dir: String): Boolean; static;
```

Loads all ".ttf" fonts in the given directory.
*)
procedure _LapeImage_LoadFonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaImage.LoadFontsInDir(PString(Params^[0])^);
end;

(*
TImage.Fonts
------------
```
function TImage.Fonts: TStringArray; static;
```

Returns all the loaded font names.
*)
procedure _LapeImage_Fonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaImage.Fonts();
end;

(*
TImage.FindAlpha
----------------
```
function TImage.FindAlpha(Value: Byte): TPointArray;
```
*)
procedure _LapeImage_FindAlpha(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FindAlpha(PByte(Params^[1])^);
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
  PPointArray(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
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
  PPoint(Result)^ := PSimbaImage(PLapeObject(Params^[0])^)^.FindImage(PSimbaImage(PLapeObject(Params^[1])^)^, PSingle(Params^[2])^, PBox(Params^[3])^);
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
begin
  PSimbaImageArray(Result)^ := TSimbaImageArray(GetSimbaObjectsOfClass(TSimbaImage));
end;

(*
TImage.Show
-----------
```
procedure TImage.Show(EnsureVisible: Boolean = True);
```

Show a image on the debug image.
*)

procedure ImportSimbaImage(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Image';

    addGlobalType('object {%CODETOOLS OFF} Instance: Pointer; DontManage: Boolean; {%CODETOOLS ON} end;', 'TImage');
    addGlobalType('array of TImage', 'TImageArray');
    addGlobalType('enum(WIDTH, HEIGHT, LINE)', 'EImageMirrorStyle');
    addGlobalType('enum(NEAREST_NEIGHBOUR, BILINEAR)', 'EImageResizeAlgo');
    addGlobalType('enum(NEAREST_NEIGHBOUR, BILINEAR)', 'EImageRotateAlgo');
    addGlobalType('enum(BOX, GAUSS)', 'EImageBlurAlgo');
    addGlobalType('set of enum(LEFT, CENTER, RIGHT, JUSTIFY, TOP, VERTICAL_CENTER, BASE_LINE, BOTTOM)', 'EImageTextAlign');

    addGlobalFunc('function TImage.Construct: TImage; static; overload;', @_LapeImage_Construct1);
    addGlobalFunc('function TImage.Construct(Width, Height: Integer): TImage; static; overload', @_LapeImage_Construct2);
    addGlobalFunc('function TImage.Construct(FileName: String): TImage; static; overload', @_LapeImage_Construct3);
    addGlobalFunc('procedure TImage.Destroy;', @_LapeImage_Destroy);

    addProperty('TImage', 'Data', 'PColorBGRA', @_LapeImage_Data_Read);
    addProperty('TImage', 'Width', 'Integer', @_LapeImage_Width_Read);
    addProperty('TImage', 'Height', 'Integer', @_LapeImage_Height_Read);
    addProperty('TImage', 'Center', 'TPoint', @_LapeImage_Center_Read);
    addProperty('TImage', 'DefaultPixel', 'TColorBGRA', @_LapeImage_DefaultPixel_Read, @_LapeImage_DefaultPixel_Write);
    addProperty('TImage', 'DrawColor', 'TColor', @_LapeImage_DrawColor_Read, @_LapeImage_DrawColor_Write);
    addProperty('TImage', 'DrawAlpha', 'Byte', @_LapeImage_DrawAlpha_Read, @_LapeImage_DrawAlpha_Write);
    addProperty('TImage', 'FontName', 'String', @_LapeImage_FontName_Read, @_LapeImage_FontName_Write);
    addProperty('TImage', 'FontSize', 'Single', @_LapeImage_FontSize_Read, @_LapeImage_FontSize_Write);
    addProperty('TImage', 'FontAntialiasing', 'Boolean', @_LapeImage_FontAntialiasing_Read, @_LapeImage_FontAntialiasing_Write);
    addProperty('TImage', 'FontBold', 'Boolean', @_LapeImage_FontBold_Read, @_LapeImage_FontBold_Write);
    addProperty('TImage', 'FontItalic', 'Boolean', @_LapeImage_FontItalic_Read, @_LapeImage_FontItalic_Write);

    addPropertyIndexed('TImage', 'Alpha', 'X, Y: Integer', 'Byte', @_LapeImage_GetAlpha, @_LapeImage_SetAlpha);
    addPropertyIndexed('TImage', 'Pixel', 'X, Y: Integer', 'TColor', @_LapeImage_GetPixel, @_LapeImage_SetPixel);

    addGlobalFunc('function TImage.GetPixels(Points: TPointArray): TColorArray;', @_LapeImage_GetPixels);
    addGlobalFunc('procedure TImage.SetPixels(Points: TPointArray; Color: TColor); overload', @_LapeImage_SetPixels1);
    addGlobalFunc('procedure TImage.SetPixels(Points: TPointArray; Colors: TColorArray); overload', @_LapeImage_SetPixels2);
    addGlobalFunc('procedure TImage.SetAlphas(Points: TPointArray; Value: Byte)', @_LapeImage_SetAlphas);

    addGlobalFunc('function TImage.InImage(X, Y: Integer): Boolean', @_LapeImage_InImage);

    addGlobalFunc('procedure TImage.SetSize(NewWidth, NewHeight: Integer);', @_LapeImage_SetSize);
    addGlobalFunc('procedure TImage.SetExternalData(Data: PColorBGRA; DataWidth, DataHeight: Integer);', @_LapeImage_SetExternalData);
    addGlobalFunc('procedure TImage.ResetExternalData(NewWidth, NewHeight: Integer);', @_LapeImage_ResetExternalData);

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
    addGlobalFunc('function TImage.isBinary: Boolean;', @_LapeImage_isBinary);

    addGlobalFunc('procedure TImage.SplitChannels(var B,G,R: TByteArray)', @_LapeImage_SplitChannels);
    addGlobalFunc('procedure TImage.FromChannels(const B,G,R: TByteArray; W, H: Integer);', @_LapeImage_FromChannels);

    addGlobalFunc('function TImage.GetColors: TColorArray; overload', @_LapeImage_GetColors1);
    addGlobalFunc('function TImage.GetColors(Box: TBox): TColorArray; overload', @_LapeImage_GetColors2);
    addGlobalFunc('function TImage.GetColors(Points: TPointArray): TColorArray; overload', @_LapeImage_GetColors3);

    addGlobalFunc('procedure TImage.ReplaceColor(OldColor, NewColor: TColor; Tolerance: Single = 0)', @_LapeImage_ReplaceColor);
    addGlobalFunc('procedure TImage.ReplaceColorBinary(Invert: Boolean; Color: TColor; Tolerance: Single = 0); overload', @_LapeImage_ReplaceColorBinary1);
    addGlobalFunc('procedure TImage.ReplaceColorBinary(Invert: Boolean; Colors: TColorArray; Tolerance: Single = 0); overload', @_LapeImage_ReplaceColorBinary2);

    addGlobalFunc('function TImage.Resize(Algo: EImageResizeAlgo; NewWidth, NewHeight: Integer): TImage; overload;', @_LapeImage_Resize1);
    addGlobalFunc('function TImage.Resize(Algo: EImageResizeAlgo; Scale: Single): TImage; overload;', @_LapeImage_Resize2);
    addGlobalFunc('function TImage.Rotate(Algo: EImageRotateAlgo; Radians: Single; Expand: Boolean): TImage;', @_LapeImage_Rotate);
    addGlobalFunc('function TImage.Downsample(Scale: Integer): TImage; overload', @_LapeImage_Downsample1);
    addGlobalFunc('function TImage.Downsample(Scale: Integer; IgnorePoints: TPointArray): TImage; overload', @_LapeImage_Downsample2);
    addGlobalFunc('function TImage.Mirror(Style: EImageMirrorStyle): TImage', @_LapeImage_Mirror);

    addGlobalFunc('function TImage.TextWidth(Text: String): Integer;', @_LapeImage_TextWidth);
    addGlobalFunc('function TImage.TextHeight(Text: String): Integer;', @_LapeImage_TextHeight);
    addGlobalFunc('function TImage.TextSize(Text: String): TPoint;', @_LapeImage_TextSize);
    addGlobalFunc('procedure TImage.DrawText(Text: String; Position: TPoint); overload', @_LapeImage_DrawText);
    addGlobalFunc('procedure TImage.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign); overload', @_LapeImage_DrawTextEx);
    addGlobalFunc('procedure TImage.DrawTextLines(Text: TStringArray; Position: TPoint);', @_LapeImage_DrawTextLines);

    addGlobalFunc('procedure TImage.DrawImage(Image: TImage; Position: TPoint)', @_LapeImage_DrawImage);

    addGlobalFunc('procedure TImage.DrawATPA(ATPA: T2DPointArray);', @_LapeImage_DrawATPA);
    addGlobalFunc('procedure TImage.DrawTPA(TPA: TPointArray);', @_LapeImage_DrawTPA);

    addGlobalFunc('procedure TImage.DrawLine(Start, Stop: TPoint)', @_LapeImage_DrawLine);
    addGlobalFunc('procedure TImage.DrawLineGap(Start, Stop: TPoint; GapSize: Integer)', @_LapeImage_DrawLineGap);
    addGlobalFunc('procedure TImage.DrawCrosshairs(ACenter: TPoint; Size: Integer);', @_LapeImage_DrawCrosshairs);
    addGlobalFunc('procedure TImage.DrawCross(ACenter: TPoint; Radius: Integer);', @_LapeImage_DrawCross);

    addGlobalFunc('procedure TImage.DrawBox(B: TBox);', @_LapeImage_DrawBox);
    addGlobalFunc('procedure TImage.DrawBoxFilled(B: TBox);', @_LapeImage_DrawBoxFilled);
    addGlobalFunc('procedure TImage.DrawBoxInverted(B: TBox);', @_LapeImage_DrawBoxInverted);

    addGlobalFunc('procedure TImage.DrawPolygon(Points: TPolygon);', @_LapeImage_DrawPolygon);
    addGlobalFunc('procedure TImage.DrawPolygonFilled(Points: TPolygon);', @_LapeImage_DrawPolygonFilled);
    addGlobalFunc('procedure TImage.DrawPolygonInverted(Points: TPolygon);', @_LapeImage_DrawPolygonInverted);

    addGlobalFunc('procedure TImage.DrawQuad(Quad: TQuad);', @_LapeImage_DrawQuad);
    addGlobalFunc('procedure TImage.DrawQuadFilled(Quad: TQuad);', @_LapeImage_DrawQuadFilled);
    addGlobalFunc('procedure TImage.DrawQuadInverted(Quad: TQuad);', @_LapeImage_DrawQuadInverted);
    
    addGlobalFunc('procedure TImage.DrawCircle(Center: TPoint; Radius: Integer); overload', @_LapeImage_DrawCircle1);
    addGlobalFunc('procedure TImage.DrawCircleFilled(Center: TPoint; Radius: Integer); overload', @_LapeImage_DrawCircleFilled1);
    addGlobalFunc('procedure TImage.DrawCircleInverted(Center: TPoint; Radius: Integer); overload', @_LapeImage_DrawCircleInverted1);

    addGlobalFunc('procedure TImage.DrawCircle(Circle: TCircle); overload', @_LapeImage_DrawCircle2);
    addGlobalFunc('procedure TImage.DrawCircleFilled(Circle: TCircle); overload', @_LapeImage_DrawCircleFilled2);
    addGlobalFunc('procedure TImage.DrawCircleInverted(Circle: TCircle); overload', @_LapeImage_DrawCircleInverted2);

    addGlobalFunc('procedure TImage.DrawLineAA(Start, Stop: TPoint; Thickness: Single = 1.5);', @_LapeImage_DrawLineAA);
    addGlobalFunc('procedure TImage.DrawEllipseAA(ACenter: TPoint; XRadius, YRadius: Integer; Thickness: Single = 1.5);', @_LapeImage_DrawEllipseAA);
    addGlobalFunc('procedure TImage.DrawCircleAA(ACenter: TPoint; Radius: Integer; Thickness: Single = 1.5); overload', @_LapeImage_DrawCircleAA1);
    addGlobalFunc('procedure TImage.DrawCircleAA(Circle: TCircle; Thickness: Single = 1.5); overload', @_LapeImage_DrawCircleAA2);
    
    addGlobalFunc('procedure TImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean);', @_LapeImage_DrawQuadArray);
    addGlobalFunc('procedure TImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean);', @_LapeImage_DrawBoxArray);
    addGlobalFunc('procedure TImage.DrawPolygonArray(Polygons: TPolygonArray; Filled: Boolean);', @_LapeImage_DrawPolygonArray);
    addGlobalFunc('procedure TImage.DrawCircleArray(Centers: TPointArray; Radius: Integer; Filled: Boolean);', @_LapeImage_DrawCircleArray1);
    addGlobalFunc('procedure TImage.DrawCrossArray(Points: TPointArray; Radius: Integer);', @_LapeImage_DrawCrossArray);

    addGlobalFunc('procedure TImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer)', @_LapeImage_DrawHSLCircle);

    addGlobalFunc('function TImage.Sobel: TImage', @_LapeImage_Sobel);
    addGlobalFunc('function TImage.Enhance(Enchantment: Byte; C: Single): TImage', @_LapeImage_Enhance);
    addGlobalFunc('function TImage.GreyScale: TImage', @_LapeImage_GreyScale);
    addGlobalFunc('function TImage.Brightness(Value: Integer): TImage', @_LapeImage_Brightness);
    addGlobalFunc('function TImage.Invert: TImage', @_LapeImage_Invert);
    addGlobalFunc('function TImage.Posterize(Value: Integer): TImage', @_LapeImage_Posterize);
    addGlobalFunc('function TImage.Convolute(Matrix: TDoubleMatrix): TImage', @_LapeImage_Convolute);
    addGlobalFunc('function TImage.Threshold(Invert: Boolean = False; C: Integer = 0): TImage', @_LapeImage_Threshold);
    addGlobalFunc('function TImage.ThresholdAdaptive(Invert: Boolean = False; Radius: Integer = 25; C: Integer = 0): TImage', @_LapeImage_ThresholdAdaptive);
    addGlobalFunc('function TImage.ThresholdAdaptiveSauvola(Invert: Boolean = False; Radius: Integer = 25; C: Single = 0.2): TImage', @_LapeImage_ThresholdAdaptiveSauvola);
    addGlobalFunc('function TImage.Blend(Points: TPointArray; Radius: Integer): TImage; overload', @_LapeImage_Blend1);
    addGlobalFunc('function TImage.Blend(Points: TPointArray; Radius: Integer; IgnorePoints: TPointArray): TImage; overload', @_LapeImage_Blend2);
    addGlobalFunc('function TImage.Blur(Algo: EImageBlurAlgo; Radius: Integer): TImage;', @_LapeImage_Blur);

    addGlobalFunc('function TImage.ToGreyMatrix: TByteMatrix', @_LapeImage_ToGreyMatrix);
    addGlobalFunc('function TImage.ToMatrix: TIntegerMatrix; overload', @_LapeImage_ToMatrix1);
    addGlobalFunc('function TImage.ToMatrix(Box: TBox): TIntegerMatrix; overload', @_LapeImage_ToMatrix2);
    addGlobalFunc('procedure TImage.FromMatrix(Matrix: TIntegerMatrix); overload', @_LapeImage_FromMatrix1);
    addGlobalFunc('procedure TImage.FromMatrix(Matrix: TSingleMatrix; ColorMapType: Integer = 0); overload', @_LapeImage_FromMatrix2);
    addGlobalFunc('procedure TImage.FromString(Str: String)', @_LapeImage_FromString);
    addGlobalFunc('procedure TImage.FromZip(ZipFile: String; ZipEntry: String)', @_LapeImage_FromZip);
    addGlobalFunc('procedure TImage.FromData(AWidth, AHeight: Integer; AData: PColorBGRA; DataWidth: Integer)', @_LapeImage_FromData);
    addGlobalFunc('procedure TImage.Load(FileName: String); overload', @_LapeImage_Load1);
    addGlobalFunc('procedure TImage.Load(FileName: String; Area: TBox); overload', @_LapeImage_Load2);
    addGlobalFunc('function TImage.Save(FileName: String; OverwriteIfExists: Boolean = False): Boolean;', @_LapeImage_Save);
    addGlobalFunc('function TImage.SaveToString: String;', @_LapeImage_SaveToString);

    addGlobalFunc('function TImage.Equals(Other: TImage): Boolean;', @_LapeImage_Equals);
    addGlobalFunc('function TImage.Compare(Other: TImage): Single;', @_LapeImage_Compare);
    addGlobalFunc('function TImage.PixelDifference(Other: TImage; Tolerance: Single = 0): TPointArray', @_LapeImage_PixelDifference);

    addGlobalFunc('function TImage.ToLazBitmap: TLazBitmap;', @_LapeImage_ToLazBitmap);
    addGlobalFunc('procedure TImage.FromLazBitmap(LazBitmap: TLazBitmap);', @_LapeImage_FromLazBitmap);

    addGlobalFunc('function TImage.Fonts: TStringArray; static;', @_LapeImage_Fonts);
    addGlobalFunc('function TImage.LoadFonts(Dir: String): Boolean; static;', @_LapeImage_LoadFonts);

    addGlobalFunc('function TImage.FindAlpha(Value: Byte): TPointArray;', @_LapeImage_FindAlpha);
    addGlobalFunc('function TImage.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;', @_LapeImage_FindColor);
    addGlobalFunc('function TImage.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;', @_LapeImage_FindImage);

    DumpSection := '';

    addGlobalFunc('function GetLoadedImages: TImageArray', @_LapeImage_GetLoadedImages);
  end;
end;

end.
