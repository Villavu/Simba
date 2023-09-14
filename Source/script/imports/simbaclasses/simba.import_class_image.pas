unit simba.import_class_image;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportSimbaImage(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics,
  lptypes,
  simba.image;

type
  PBitmap = ^TBitmap;

(*
Image
=====
TImage is a data type that holds an image.

This is used anipulate and process an image such as resizing, rotating, bluring and much more.
Or simply get/set a pixel color at a given (x,y) coord.
*)

(*
TImage.InImage
~~~~~~~~~~~~~~
> function TImage.InImage(X, Y: Integer): Boolean;
*)
procedure _LapeImage_InImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.InImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.EnsureInImage
~~~~~~~~~~~~~~~~~~~~
> function TImage.EnsureInImage(var X, Y: Integer): Boolean;
*)
procedure _LapeImage_EnsureInImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.EnsureInImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.GetData
~~~~~~~~~~~~~~
> function TImage.GetData: PColorBGRA;
*)
procedure _LapeImage_Data_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaImage(Params^[0])^.Data;
end;

(*
TImage.GetName
~~~~~~~~~~~~~~
> function TImage.GetName: String;
*)
procedure _LapeImage_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.Name;
end;

(*
TImage.SetName
~~~~~~~~~~~~~~
> procedure TImage.SetName(Value: String);
*)
procedure _LapeImage_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Name := PString(Params^[1])^;
end;

(*
TImage.SetSize
~~~~~~~~~~~~~~
> procedure TImage.SetSize(AWidth, AHeight: Integer);
*)
procedure _LapeImage_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.ResizeNN
~~~~~~~~~~~~~~~
> function TImage.ResizeNN(AWidth, AHeight: Integer): TImage;
*)
procedure _LapeImage_ResizeNN(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ResizeNN(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.ResizeBilinear
~~~~~~~~~~~~~~~~~~~~~
> function TImage.ResizeBilinear(AWidth, AHeight: Integer): TImage;
*)
procedure _LapeImage_ResizeBilinear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ResizeBilinear(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.GetWidth
~~~~~~~~~~~~~~~
> function TImage.GetWidth: Integer;
*)
procedure _LapeImage_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.Width;
end;

(*
TImage.GetHeight
~~~~~~~~~~~~~~~~
> function TImage.GetHeight: Integer;
*)
procedure _LapeImage_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.Height;
end;

(*
TImage.SetExternalData
~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.SetExternalData(AData: PColorBGRA; AWidth, AHeight: Integer);
*)
procedure _LapeImage_SetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetExternalData(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.ResetExternalData
~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.ResetExternalData;
*)
procedure _LapeImage_ResetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ResetExternalData();
end;

(*
TImage.SaveToFile
~~~~~~~~~~~~~~~~~
> function TImage.SaveToFile(FileName: String; OverwriteIfExists: Boolean = False): Boolean;
*)
procedure _LapeImage_SaveToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.SaveToFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.SaveToString
~~~~~~~~~~~~~~~~~~~
> function TImage.SaveToString: String;
*)
procedure _LapeImage_SaveToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.SaveToString();
end;

(*
TImage.LoadFromFile
~~~~~~~~~~~~~~~~~~~
> procedure TImage.LoadFromFile(FileName: String);
*)
procedure _LapeImage_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

(*
TImage.LoadFromFile
~~~~~~~~~~~~~~~~~~~
> procedure TImage.LoadFromFile(FileName: String; Area: TBox);
*)
procedure _LapeImage_LoadFromFileEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromFile(PString(Params^[1])^, PBox(Params^[2])^);
end;

(*
TImage.DrawATPA
~~~~~~~~~~~~~~~
> procedure TImage.DrawATPA(ATPA: T2DPointArray; Color: TColor = -1);
*)
procedure _LapeImage_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawTPA
~~~~~~~~~~~~~~
> procedure TImage.DrawTPA(TPA: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.ReplaceColor
~~~~~~~~~~~~~~~~~~~
> procedure TImage.ReplaceColor(OldColor, NewColor: TColor);
*)
procedure _LapeImage_ReplaceColor(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.ReplaceColors
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.ReplaceColors(OldColors, NewColors: TColorArray);
*)
procedure _LapeImage_ReplaceColors(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ReplaceColors(PColorArray(Params^[1])^, PColorArray(Params^[2])^);
end;

(*
TImage.RotateNN
~~~~~~~~~~~~~~~
> function TImage.RotateNN(Radians: Single; Expand: Boolean): TImage;
*)
procedure _LapeImage_RotateNN(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.RotateNN(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.RotateBilinear
~~~~~~~~~~~~~~~~~~~~~
> function TImage.RotateBilinear(Radians: Single; Expand: Boolean): TImage;
*)
procedure _LapeImage_RotateBilinear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.RotateBilinear(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TImage.GreyScale
~~~~~~~~~~~~~~~~
> function TImage.GreyScale: TImage;
*)
procedure _LapeImage_GreyScale(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.GreyScale();
end;

(*
TImage.Brightness
~~~~~~~~~~~~~~~~~
> function TImage.Brightness(Value: Integer): TImage;
*)
procedure _LapeImage_Brightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Brightness(PInteger(Params^[1])^);
end;

(*
TImage.Invert
~~~~~~~~~~~~~
> function TImage.Invert: TImage;
*)
procedure _LapeImage_Invert(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Invert();
end;

(*
TImage.Posterize
~~~~~~~~~~~~~~~~
> function TImage.Posterize(Value: Integer): TImage;
*)
procedure _LapeImage_Posterize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Posterize(PInteger(Params^[1])^);
end;

(*
TImage.Convolute
~~~~~~~~~~~~~~~~
> function TImage.Convolute(Matrix: TDoubleMatrix): TImage;
*)
procedure _LapeImage_Convolute(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Convolute(PDoubleMatrix(Params^[1])^);
end;

(*
TImage.Copy
~~~~~~~~~~~
> function TImage.Copy(X1, Y1, X2, Y2: Integer): TImage;
*)
procedure _LapeImage_Copy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TImage.Copy
~~~~~~~~~~~
> function TImage.Copy: TImage;
*)
procedure _LapeImage_CopyEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Copy();
end;

(*
TImage.ToLazBitmap
~~~~~~~~~~~~~~~~~~
> function TImage.ToLazBitmap: TLazBitmap;
*)
procedure _LapeImage_ToLazBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSimbaImage(Params^[0])^.ToLazBitmap();
end;

(*
TImage.LoadFromLazBitmap
~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.LoadFromLazBitmap(LazBitmap: TLazBitmap);
*)
procedure _LapeImage_LoadFromLazBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromLazBitmap(PBitmap(Params^[1])^);
end;

(*
TImage.Crop
~~~~~~~~~~~
> procedure TImage.Crop(X1, Y1, X2, Y2: Integer);
*)
procedure _LapeImage_Crop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

(*
TImage.GetColors
~~~~~~~~~~~~~~~~
> function TImage.GetColors: TColorArray;
*)
procedure _LapeImage_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(Params^[0])^.GetColors();
end;

(*
TImage.ToMatrix
~~~~~~~~~~~~~~~
> function TImage.ToMatrix: TIntegerMatrix;
*)
procedure _LapeImage_ToMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaImage(Params^[0])^.ToMatrix();
end;

(*
TImage.ToMatrix
~~~~~~~~~~~~~~~
> function TImage.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
*)
procedure _LapeImage_ToMatrixEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaImage(Params^[0])^.ToMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TImage.DrawMatrix
~~~~~~~~~~~~~~~~~
> procedure TImage.DrawMatrix(Matrix: TIntegerMatrix);
*)
procedure _LapeImage_DrawMatrixI(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawMatrix(PIntegerMatrix(Params^[1])^);
end;

(*
TImage.DrawMatrix
~~~~~~~~~~~~~~~~~
> procedure TImage.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
*)
procedure _LapeImage_DrawMatrixF(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.ThresholdAdaptive
~~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: EImageThreshMethod; k: Integer): TImage;
*)
procedure _LapeImage_ThresholdAdaptive(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, ESimbaImageThreshMethod(Params^[4]^), PInteger(Params^[5])^);
end;

(*
TImage.ThresholdSauvola
~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single): TImage;
*)
procedure _LapeImage_ThresholdSauvola(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ThresholdSauvola(PInteger(Params^[1])^, PBoolean(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TImage.Pad
~~~~~~~~~~
> procedure TImage.Pad(Amount: Integer);
*)
procedure _LapeImage_Pad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

(*
TImage.SetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.SetTransparentColor(Value: TColor);
*)
procedure _LapeImage_TransparentColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.TransparentColor := PColor(Params^[1])^;
end;

(*
TImage.GetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.GetTransparentColor: TColor;
*)
procedure _LapeImage_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(Params^[0])^.TransparentColor;
end;

(*
TImage.GetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.GetTransparentColorActive: Boolean;
*)
procedure _LapeImage_TransparentColorActive_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.TransparentColorActive;
end;

(*
TImage.SetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.SetTransparentColorActive(Value: Boolean);
*)
procedure _LapeImage_TransparentColorActive_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.TransparentColorActive := PBoolean(Params^[1])^;
end;

(*
TImage.GetPixel
~~~~~~~~~~~~~~~
> function TImage.GetPixel(X, Y: Integer): TColor;
*)
procedure _LapeImage_GetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

(*
TImage.SetPixel
~~~~~~~~~~~~~~~
> procedure TImage.SetPixel(X, Y: Integer; Color: TColor);
*)
procedure _LapeImage_SetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

(*
TImage.SetPixels
~~~~~~~~~~~~~~~~
> procedure TImage.SetPixels(Points: TPointArray; Colors: TColorArray);
*)
procedure _LapeImage_SetPixels(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TImage.GetPixels
~~~~~~~~~~~~~~~~
> function TImage.GetPixels(Points: TPointArray): TColorArray;
*)
procedure _LapeImage_GetPixels(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(Params^[0])^.GetPixels(PPointArray(Params^[1])^);
end;

(*
TImage.Blur
~~~~~~~~~~~
> function TImage.Blur(Block: Integer): TImage;
*)
procedure _LapeImage_Blur(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

(*
TImage.Downsample
~~~~~~~~~~~~~~~~~
> function TImage.Downsample(Scale: Integer): TImage;
*)
procedure _LapeImage_DownSample(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Downsample(PInteger(Params^[1])^);
end;

(*
TImage.Free
~~~~~~~~~~~
> procedure TImage.Free;
*)
procedure _LapeImage_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Free();
end;

(*
TImage.DrawCross
~~~~~~~~~~~~~~~~
> procedure TImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawCrosshairs
~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawLine
~~~~~~~~~~~~~~~
> procedure TImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawLine
~~~~~~~~~~~~~~~
> procedure TImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawLineEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawPolygon
~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawPolygon(Points: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawPolygonFilled
~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawPolygonInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawCircle
~~~~~~~~~~~~~~~~~
> procedure TImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawCircleFilled
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawCircleInverted
~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawBox
~~~~~~~~~~~~~~
> procedure TImage.DrawBox(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawBoxFilled
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawBoxFilled(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawBoxInverted
~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawBoxInverted(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawQuad
~~~~~~~~~~~~~~~
> procedure TImage.DrawQuad(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawQuadFilled
~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawQuadFilled(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TImage.DrawQuadInverted
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawQuadInverted(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.DrawQuadArray
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawBoxArray
~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawPolygonArray
~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawCircleArray
~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

(*
TImage.DrawCrossArray
~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);
*)
procedure _LapeImage_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.Fill
~~~~~~~~~~~
> procedure TImage.Fill(Color: TColor);
*)
procedure _LapeImage_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Fill(PColor(Params^[1])^);
end;

(*
TImage.Clear
~~~~~~~~~~~~
> procedure TImage.Clear;
*)
procedure _LapeImage_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Clear();
end;

(*
TImage.Clear
~~~~~~~~~~~~
> procedure TImage.Clear(Area: TBox);
*)
procedure _LapeImage_ClearEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Clear(PBox(Params^[1])^);
end;

(*
TImage.ClearInverted
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.ClearInverted(Area: TBox);
*)
procedure _LapeImage_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

(*
TImage.Draw
~~~~~~~~~~~
> procedure TImage.Draw(Image: TImage; X, Y: Integer);
*)
procedure _LapeImage_Draw1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.Draw
~~~~~~~~~~~
> procedure TImage.Draw(Image: TImage; Position: TPoint);
*)
procedure _LapeImage_Draw2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TImage.Blend
~~~~~~~~~~~~
> function TImage.Blend(Points: TPointArray; Size: Integer): TImage;
*)
procedure _LapeImage_Blend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.GetCenter
~~~~~~~~~~~~~~~~
> function TImage.GetCenter: TPoint;
*)
procedure _LapeImage_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(Params^[0])^.Center;
end;

(*
TImage.GetFontName
~~~~~~~~~~~~~~~~~~
> function TImage.GetFontName: String;
*)
procedure _LapeImage_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.FontName;
end;

(*
TImage.SetFontName
~~~~~~~~~~~~~~~~~~
> procedure TImage.SetFontName(Value: String);
*)
procedure _LapeImage_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontName := PString(Params^[1])^;
end;

(*
TImage.GetFontSize
~~~~~~~~~~~~~~~~~~
> function TImage.GetFontSize: Single;
*)
procedure _LapeImage_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(Params^[0])^.FontSize;
end;

(*
TImage.SetFontSize
~~~~~~~~~~~~~~~~~~
> procedure TImage.SetFontSize(Value: Single);
*)
procedure _LapeImage_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

(*
TImage.GetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.GetFontAntialiasing: Boolean;
*)
procedure _LapeImage_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontAntialiasing;
end;

(*
TImage.SetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.SetFontAntialiasing(Value: Boolean);
*)
procedure _LapeImage_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TImage.GetFontBold
~~~~~~~~~~~~~~~~~~
> function TImage.GetFontBold: Boolean;
*)
procedure _LapeImage_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontBold;
end;

(*
TImage.SetFontBold
~~~~~~~~~~~~~~~~~~
> procedure TImage.SetFontBold(Value: Boolean);
*)
procedure _LapeImage_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

(*
TImage.GetFontItalic
~~~~~~~~~~~~~~~~~~~~
> function TImage.GetFontItalic: Boolean;
*)
procedure _LapeImage_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontItalic;
end;

(*
TImage.SetFontItalic
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.SetFontItalic(Value: Boolean);
*)
procedure _LapeImage_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

(*
TImage.TextWidth
~~~~~~~~~~~~~~~~
> function TImage.TextWidth(Text: String): Integer;
*)
procedure _LapeImage_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

(*
TImage.TextHeight
~~~~~~~~~~~~~~~~~
> function TImage.TextHeight(Text: String): Integer;
*)
procedure _LapeImage_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

(*
TImage.TextSize
~~~~~~~~~~~~~~~
> function TImage.TextSize(Text: String): TPoint;
*)
procedure _LapeImage_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(Params^[0])^.TextSize(PString(Params^[1])^);
end;

(*
TImage.DrawText
~~~~~~~~~~~~~~~
> procedure TImage.DrawText(Text: String; Position: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TImage.DrawText
~~~~~~~~~~~~~~~
> procedure TImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor);
*)
procedure _LapeImage_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

(*
TImage.DrawTextLines
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TImage.Mirror
~~~~~~~~~~~~~
> function TImage.Mirror(Style: EImageMirrorStyle): TImage;
*)
procedure _LapeImage_Mirror(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Mirror(ESimbaImageMirrorStyle(Params^[1]^));
end;

(*
TImage.Equals
~~~~~~~~~~~~~
> function TImage.Equals(Other: TImage): Boolean;

Are the two images exactly equal?
*)
procedure _LapeImage_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.Equals(PSimbaImage(Params^[1])^);
end;

(*
TImage.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~
> function TImage.PixelDifference(Other: TImage): Integer;
*)
procedure _LapeImage_PixelDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.PixelDifference(PSimbaImage(Params^[1])^);
end;

(*
TImage.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~
> function TImage.PixelDifference(Other: TImage; Tolerance: Integer): Integer;
*)
procedure _LapeImage_PixelDifferenceTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.PixelDifference(PSimbaImage(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TImage.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.PixelDifferenceTPA(Other: TImage): TPointArray;
*)
procedure _LapeImage_PixelDifferenceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
 PPointArray(Result)^ := PSimbaImage(Params^[0])^.PixelDifferenceTPA(PSimbaImage(Params^[1])^);
end;

(*
TImage.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.PixelDifferenceTPA(Other: TImage; Tolerance: Integer): TPointArray;
*)
procedure _LapeImage_PixelDifferenceToleranceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImage(Params^[0])^.PixelDifferenceTPA(PSimbaImage(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TImage.LoadFromString
~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.LoadFromString(Str: String);
*)
procedure _LapeImage_LoadFromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromString(PString(Params^[1])^);
end;

(*
TImage.LoadFromData
~~~~~~~~~~~~~~~~~~~
> procedure TImage.LoadFromData(AWidth, AHeight: Integer; Memory: PColorBGRA; DataWidth: Integer);
*)
procedure _LapeImage_LoadFromData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromData(PInteger(Params^[1])^, PInteger(Params^[2])^, PPointer(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TImage.LoadFromImage
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.LoadFromImage(Image: TImage);
*)
procedure _LapeImage_LoadFromImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromImage(PSimbaImage(Params^[1])^);
end;

(*
TImage.Create
~~~~~~~~~~~~~
> function TImage.Create: TImage; static;
*)
procedure _LapeImage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.Create();
end;

(*
TImage.Create
~~~~~~~~~~~~~
> function TImage.Create(Width, Height: Integer): TImage; static;
*)
procedure _LapeImage_CreateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TImage.CreateFromFile
~~~~~~~~~~~~~~~~~~~~~
> function TImage.CreateFromFile(FileName: String): TImage; static;
*)
procedure _LapeImage_CreateFromFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.CreateFromFile(PString(Params^[0])^);
end;

(*
TImage.CreateFromString
~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.CreateFromString(Str: String): TImage; static;
*)
procedure _LapeImage_CreateFromString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.CreateFromString(PString(Params^[0])^);
end;

(*
TImage.Compare
~~~~~~~~~~~~~~
> function TImage.Compare(Other: TImage): Single;
*)
procedure _LapeImage_Compare(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(Params^[0])^.Compare(PSimbaImage(Params^[1])^);
end;

(*
TImage.SaveUnfreedImages
~~~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.SaveUnfreedImages(Directory: String); static;

Saves unfreed images on script terminate.

Example:

```
  TImage.SaveUnfreedImages('some/directory/');
```
*)
procedure _LapeImage_SaveUnfreedImages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImage.SaveUnfreedImages := PString(Params^[0])^;
end;

(*
TImage.LoadFonts
~~~~~~~~~~~~~~~~
> function TImage.LoadFonts(Dir: String): Boolean; static;

Loads all ".ttf" fonts in the passed directory.
*)
procedure _LapeImage_LoadFonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaImage.LoadFonts(PString(Params^[0])^);
end;

(*
TImage.GetFontNames
~~~~~~~~~~~~~~~~~~~
> function TImage.GetFontNames: TStringArray; static;

Returns all the available font names.
*)
procedure _LapeImage_LoadedFontNames(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaImage.LoadedFontNames();
end;

(*
TImage.FreeOnTerminate
~~~~~~~~~~~~~~~~~~~~~~
> procedure TImage.FreeOnTerminate(Value: Boolean);
*)
procedure _LapeImage_FreeOnTerminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

(*
TImage.DrawHSLCircle
~~~~~~~~~~~~~~~~~~~~
> procedure TImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
*)
procedure _LapeImage_DrawHSLCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawHSLCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TImage.RowPtrs
~~~~~~~~~~~~~~
> function TImage.RowPtrs: TImageRowPtrs;
*)
procedure _LapeImage_RowPtrs(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImageRowPtrs(Result^) := PSimbaImage(Params^[0])^.RowPtrs();
end;

(*
TImage.Finder
~~~~~~~~~~~~~~~~~~
> function TImage.Finder: TSimbaFinder;

Returns a TSimbaFinder which is targetted to the image.
*)

(*
TImage.CreateFromTarget
~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.CreateFromTarget(Target: TSimbaTarget; Bounds: TBox = [-1,-1,-1,-1]): TImage; static;

Creates an image from the given target and bounds.

- The `Bounds` parameter defaults to the entire target.
*)

(*
TImage.CreateFromTarget
~~~~~~~~~~~~~~~~~~~~~~~
> function TImage.CreateFromTarget(Bounds: TBox = [-1,-1,-1,-1]): TImage; static;

Creates an image from the bounds of the current target.

- Current target is the global **Target** variable
- The `Bounds` parameter defaults to the entire target.
*)

(*
TImage.DrawTarget
~~~~~~~~~~~~~~~~~
> procedure TImage.DrawTarget(Target: TSimbaTarget; P: TPoint; Bounds: TBox = [-1,-1,-1,-1]);
*)

(*
TImage.DrawTarget
~~~~~~~~~~~~~~~~~
> procedure TImage.DrawTarget(P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;
*)

(*
TImage.Show
~~~~~~~~~~~
> procedure TImage.Show;

Show a image on the debug image.
*)

procedure ImportSimbaImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Image';

    addClass('TImage');

    addGlobalType('array of TImage', 'TImageArray');
    addGlobalType('array of PColorBGRA', 'TImageRowPtrs');
    addGlobalType('enum(WIDTH, HEIGHT, LINE)', 'EImageMirrorStyle');
    addGlobalType('enum(MEAN, MIN_MAX)', 'EImageThreshMethod');

    addClassVar('TImage', 'Data', 'PColorBGRA', @_LapeImage_Data_Read);
    addClassVar('TImage', 'Name', 'String', @_LapeImage_Name_Read, @_LapeImage_Name_Write);
    addClassVar('TImage', 'Width', 'Integer', @_LapeImage_Width_Read);
    addClassVar('TImage', 'Height', 'Integer', @_LapeImage_Height_Read);
    addClassVar('TImage', 'Center', 'TPoint', @_LapeImage_Center_Read);
    addClassVar('TImage', 'TransparentColor', 'TColor', @_LapeImage_TransparentColor_Read, @_LapeImage_TransparentColor_Write);
    addClassVar('TImage', 'TransparentColorActive', 'Boolean', @_LapeImage_TransparentColorActive_Read, @_LapeImage_TransparentColorActive_Write);

    addClassVar('TImage', 'FontName', 'String', @_LapeImage_FontName_Read, @_LapeImage_FontName_Write);
    addClassVar('TImage', 'FontSize', 'Single', @_LapeImage_FontSize_Read, @_LapeImage_FontSize_Write);
    addClassVar('TImage', 'FontAntialiasing', 'Boolean', @_LapeImage_FontAntialiasing_Read, @_LapeImage_FontAntialiasing_Write);
    addClassVar('TImage', 'FontBold', 'Boolean', @_LapeImage_FontBold_Read, @_LapeImage_FontBold_Write);
    addClassVar('TImage', 'FontItalic', 'Boolean', @_LapeImage_FontItalic_Read, @_LapeImage_FontItalic_Write);

    addGlobalFunc('function TImage.RowPtrs: TImageRowPtrs', @_LapeImage_RowPtrs);

    addGlobalFunc('function TImage.LoadedFontNames: TStringArray; static;', @_LapeImage_LoadedFontNames);
    addGlobalFunc('function TImage.LoadFonts(Dir: String): Boolean; static;', @_LapeImage_LoadFonts);

    addGlobalFunc('function TImage.InImage(X, Y: Integer): Boolean', @_LapeImage_InImage);
    addGlobalFunc('procedure TImage.EnsureInImage(var X, Y: Integer)', @_LapeImage_EnsureInImage);

    addGlobalFunc('function TImage.Create: TImage; static; overload', @_LapeImage_Create);
    addGlobalFunc('function TImage.Create(Width, Height: Integer): TImage; static; overload', @_LapeImage_CreateEx);
    addGlobalFunc('function TImage.CreateFromFile(FileName: String): TImage; static; overload', @_LapeImage_CreateFromFile);
    addGlobalFunc('function TImage.CreateFromString(Str: String): TImage; static; overload', @_LapeImage_CreateFromString);

    addGlobalFunc('function TImage.Equals(Other: TImage): Boolean;', @_LapeImage_Equals);

    addGlobalFunc('procedure TImage.SetPixel(X, Y: Integer; Color: TColor);', @_LapeImage_SetPixel);
    addGlobalFunc('function TImage.GetPixel(X, Y: Integer): TColor;', @_LapeImage_GetPixel);

    addGlobalFunc('procedure TImage.SetPixels(Points: TPointArray; Colors: TColorArray);', @_LapeImage_SetPixels);
    addGlobalFunc('function TImage.GetPixels(Points: TPointArray): TColorArray;', @_LapeImage_GetPixels);

    addGlobalFunc('function TImage.PixelDifference(Other: TImage): Integer; overload', @_LapeImage_PixelDifference);
    addGlobalFunc('function TImage.PixelDifference(Other: TImage; Tolerance: Single): Integer; overload', @_LapeImage_PixelDifferenceTolerance);

    addGlobalFunc('function TImage.PixelDifferenceTPA(Other: TImage): TPointArray; overload', @_LapeImage_PixelDifferenceTPA);
    addGlobalFunc('function TImage.PixelDifferenceTPA(Other: TImage; Tolerance: Single): TPointArray; overload', @_LapeImage_PixelDifferenceToleranceTPA);

    addGlobalFunc('function TImage.TextWidth(Text: String): Integer;', @_LapeImage_TextWidth);
    addGlobalFunc('function TImage.TextHeight(Text: String): Integer;', @_LapeImage_TextHeight);
    addGlobalFunc('function TImage.TextSize(Text: String): TPoint;', @_LapeImage_TextSize);
    addGlobalFunc('procedure TImage.DrawText(Text: String; Position: TPoint; Color: TColor); overload', @_LapeImage_DrawText);
    addGlobalFunc('procedure TImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor); overload', @_LapeImage_DrawTextEx);
    addGlobalFunc('procedure TImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);', @_LapeImage_DrawTextLines);

    addGlobalFunc('procedure TImage.DrawATPA(ATPA: T2DPointArray; Color: TColor = -1)', @_LapeImage_DrawATPA);
    addGlobalFunc('procedure TImage.DrawTPA(TPA: TPointArray; Color: TColor);', @_LapeImage_DrawTPA);

    addGlobalFunc('procedure TImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Thickness: Integer; Color: TColor);', @_LapeImage_DrawCrosshairs);
    addGlobalFunc('procedure TImage.DrawCross(ACenter: TPoint; Radius: Integer; Thickness: Integer; Color: TColor);', @_LapeImage_DrawCross);

    addGlobalFunc('procedure TImage.DrawLine(Start, Stop: TPoint; Color: TColor); overload', @_LapeImage_DrawLine);
    addGlobalFunc('procedure TImage.DrawLine(Start, Stop: TPoint; Thickness: Integer; Color: TColor); overload', @_LapeImage_DrawLineEx);

    addGlobalFunc('procedure TImage.DrawPolygon(Points: TPointArray; Color: TColor);', @_LapeImage_DrawPolygon);
    addGlobalFunc('procedure TImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);', @_LapeImage_DrawPolygonFilled);
    addGlobalFunc('procedure TImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);', @_LapeImage_DrawPolygonInverted);

    addGlobalFunc('procedure TImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor);', @_LapeImage_DrawCircle);
    addGlobalFunc('procedure TImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);', @_LapeImage_DrawCircleFilled);
    addGlobalFunc('procedure TImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor);', @_LapeImage_DrawCircleInverted);

    addGlobalFunc('procedure TImage.DrawBox(B: TBox; Color: TColor);', @_LapeImage_DrawBox);
    addGlobalFunc('procedure TImage.DrawBoxFilled(B: TBox; Color: TColor);', @_LapeImage_DrawBoxFilled);
    addGlobalFunc('procedure TImage.DrawBoxInverted(B: TBox; Color: TColor);', @_LapeImage_DrawBoxInverted);

    addGlobalFunc('procedure TImage.DrawQuad(Quad: TQuad; Color: TColor);', @_LapeImage_DrawQuad);
    addGlobalFunc('procedure TImage.DrawQuadFilled(Quad: TQuad; Color: TColor);', @_LapeImage_DrawQuadFilled);
    addGlobalFunc('procedure TImage.DrawQuadInverted(Quad: TQuad; Color: TColor);', @_LapeImage_DrawQuadInverted);

    addGlobalFunc('procedure TImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);', @_LapeImage_DrawQuadArray);
    addGlobalFunc('procedure TImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);', @_LapeImage_DrawBoxArray);
    addGlobalFunc('procedure TImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);', @_LapeImage_DrawPolygonArray);
    addGlobalFunc('procedure TImage.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);', @_LapeImage_DrawCircleArray);
    addGlobalFunc('procedure TImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Thickness: Integer; Color: TColor = -1);', @_LapeImage_DrawCrossArray);

    addGlobalFunc('procedure TImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer)', @_LapeImage_DrawHSLCircle);

    addGlobalFunc('procedure TImage.Clear; overload', @_LapeImage_Clear);
    addGlobalFunc('procedure TImage.Clear(Area: TBox); overload', @_LapeImage_ClearEx);
    addGlobalFunc('procedure TImage.ClearInverted(Area: TBox);', @_LapeImage_ClearInverted);

    addGlobalFunc('procedure TImage.Draw(Image: TImage; X, Y: Integer); overload', @_LapeImage_Draw1);
    addGlobalFunc('procedure TImage.Draw(Image: TImage; Position: TPoint); overload', @_LapeImage_Draw2);

    addGlobalFunc('procedure TImage.DrawMatrix(Matrix: TIntegerMatrix); overload', @_LapeImage_DrawMatrixI);
    addGlobalFunc('procedure TImage.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload', @_LapeImage_DrawMatrixF);

    addGlobalFunc('procedure TImage.SetSize(AWidth, AHeight: Integer);', @_LapeImage_SetSize);
    addGlobalFunc('procedure TImage.SetPersistentMemory(Memory: PtrUInt; AWidth, AHeight: Integer);', @_LapeImage_SetPersistentMemory);
    addGlobalFunc('procedure TImage.ResetPersistentMemory;', @_LapeImage_ResetPersistentMemory);

    addGlobalFunc('function TImage.ResizeNN(AWidth, AHeight: Integer): TImage', @_LapeImage_ResizeNN);
    addGlobalFunc('function TImage.ResizeBilinear(AWidth, AHeight: Integer): TImage', @_LapeImage_ResizeBilinear);

    addGlobalFunc('function TImage.RotateNN(Radians: Single; Expand: Boolean): TImage', @_LapeImage_RotateNN);
    addGlobalFunc('function TImage.RotateBilinear(Radians: Single; Expand: Boolean): TImage', @_LapeImage_RotateBilinear);

    addGlobalFunc('procedure TImage.Fill(Color: TColor);', @_LapeImage_Fill);
    addGlobalFunc('procedure TImage.ReplaceColor(OldColor, NewColor: TColor);', @_LapeImage_ReplaceColor);
    addGlobalFunc('procedure TImage.ReplaceColors(OldColors, NewColors: TColorArray);', @_LapeImage_ReplaceColors);

    addGlobalFunc('function TImage.GreyScale: TImage', @_LapeImage_GreyScale);
    addGlobalFunc('function TImage.Brightness(Value: Integer): TImage', @_LapeImage_Brightness);
    addGlobalFunc('function TImage.Invert: TImage', @_LapeImage_Invert);
    addGlobalFunc('function TImage.Posterize(Value: Integer): TImage', @_LapeImage_Posterize);
    addGlobalFunc('function TImage.Convolute(Matrix: TDoubleMatrix): TImage', @_LapeImage_Convolute);
    addGlobalFunc('function TImage.Mirror(Style: EImageMirrorStyle): TImage', @_LapeImage_Mirror);
    addGlobalFunc('function TImage.Blur(Block: Integer): TImage', @_LapeImage_Blur);
    addGlobalFunc('function TImage.Blend(Points: TPointArray; Radius: Integer): TImage', @_LapeImage_Blend);
    addGlobalFunc('function TImage.Downsample(Scale: Integer): TImage', @_LapeImage_Downsample);

    addGlobalFunc('function TImage.Copy(X1, Y1, X2, Y2: Integer): TImage; overload', @_LapeImage_Copy);
    addGlobalFunc('function TImage.Copy: TImage; overload', @_LapeImage_CopyEx);
    addGlobalFunc('procedure TImage.Crop(X1, Y1, X2, Y2: Integer);', @_LapeImage_Crop);
    addGlobalFunc('function TImage.GetColors: TColorArray;', @_LapeImage_GetColors);
    addGlobalFunc('function TImage.ToMatrix: TIntegerMatrix; overload', @_LapeImage_ToMatrix);
    addGlobalFunc('function TImage.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix; overload', @_LapeImage_ToMatrixEx);
    addGlobalFunc('function TImage.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: EImageThreshMethod; k: Integer): TImage', @_LapeImage_ThresholdAdaptive);
    addGlobalFunc('function TImage.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single): TImage', @_LapeImage_ThresholdSauvola);
    addGlobalFunc('procedure TImage.Pad(Amount: Integer)', @_LapeImage_Pad);

    addGlobalFunc('procedure TImage.LoadFromFile(FileName: String); overload', @_LapeImage_LoadFromFile);
    addGlobalFunc('procedure TImage.LoadFromFile(FileName: String; Area: TBox); overload', @_LapeImage_LoadFromFileEx);
    addGlobalFunc('procedure TImage.LoadFromString(Str: String)', @_LapeImage_LoadFromString);
    addGlobalFunc('procedure TImage.LoadFromData(AWidth, AHeight: Integer; AData: PColorBGRA; DataWidth: Integer)', @_LapeImage_LoadFromData);
    addGlobalFunc('procedure TImage.LoadFromImage(Image: TImage);', @_LapeImage_LoadFromImage);

    addGlobalFunc('function TImage.SaveToFile(FileName: String; OverwriteIfExists: Boolean = False): Boolean;', @_LapeImage_SaveToFile);
    addGlobalFunc('function TImage.SaveToString: String;', @_LapeImage_SaveToString);

    addGlobalFunc('function TImage.ToLazBitmap: TLazBitmap;', @_LapeImage_ToLazBitmap);
    addGlobalFunc('procedure TImage.LoadFromLazBitmap(LazBitmap: TLazBitmap);', @_LapeImage_LoadFromLazBitmap);
    addGlobalFunc('function TImage.Compare(Other: TImage): Single;', @_LapeImage_Compare);

    addGlobalFunc('procedure TImage.SaveUnfreedImages(Directory: String); static;', @_LapeImage_SaveUnfreedImages);
    addGlobalFunc('procedure TImage.FreeOnTerminate(Value: Boolean);', @_LapeImage_FreeOnTerminate);

    ImportingSection := '';
  end;
end;

end.
