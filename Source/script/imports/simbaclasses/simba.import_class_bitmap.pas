unit simba.import_class_bitmap;

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
  simba.bitmap;

type
  PBitmap = ^TBitmap;

(*
TSimbaImage
===========
TSimbaImage is an Image data type.
*)

(*
TSimbaImage.InImage
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.InImage(X, Y: Integer): Boolean;
*)
procedure _LapeImage_InImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.InImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.EnsureInImage
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.EnsureInImage(var X, Y: Integer): Boolean;
*)
procedure _LapeImage_EnsureInImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.EnsureInImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.GetData
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetData: PColorBGRA;
*)
procedure _LapeImage_Data_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaImage(Params^[0])^.Data;
end;

(*
TSimbaImage.GetName
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetName: String;
*)
procedure _LapeImage_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.Name;
end;

(*
TSimbaImage.SetName
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetName(Value: String);
*)
procedure _LapeImage_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Name := PString(Params^[1])^;
end;

(*
TSimbaImage.SetSize
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetSize(AWidth, AHeight: Integer);
*)
procedure _LapeImage_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.ResizeNN
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ResizeNN(AWidth, AHeight: Integer): TSimbaImage;
*)
procedure _LapeImage_ResizeNN(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ResizeNN(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.ResizeBilinear
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ResizeBilinear(AWidth, AHeight: Integer): TSimbaImage;
*)
procedure _LapeImage_ResizeBilinear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ResizeBilinear(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.GetWidth
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetWidth: Integer;
*)
procedure _LapeImage_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.Width;
end;

(*
TSimbaImage.GetHeight
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetHeight: Integer;
*)
procedure _LapeImage_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.Height;
end;

(*
TSimbaImage.SetExternalData
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetExternalData(AData: PColorBGRA; AWidth, AHeight: Integer);
*)
procedure _LapeImage_SetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetExternalData(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TSimbaImage.ResetExternalData
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ResetExternalData;
*)
procedure _LapeImage_ResetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ResetExternalData();
end;

(*
TSimbaImage.SaveToFile
~~~~~~~~~~~~~~~~~
function TSimbaImage.SaveToFile(FileName: String; OverwriteIfExists: Boolean = False): Boolean;
*)
procedure _LapeImage_SaveToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.SaveToFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaImage.SaveToString
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.SaveToString: String;
*)
procedure _LapeImage_SaveToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.SaveToString();
end;

(*
TSimbaImage.LoadFromFile
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromFile(FileName: String);
*)
procedure _LapeImage_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

(*
TSimbaImage.LoadFromFile
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromFile(FileName: String; Area: TBox);
*)
procedure _LapeImage_LoadFromFileEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromFile(PString(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaImage.DrawATPA
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray);
*)
procedure _LapeImage_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

(*
TSimbaImage.DrawATPA
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray; Color: TColor);
*)
procedure _LapeImage_DrawATPAEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawTPA
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTPA(TPA: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.ReplaceColor
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor);
*)
procedure _LapeImage_ReplaceColor(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.ReplaceColors
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ReplaceColors(OldColors, NewColors: TColorArray);
*)
procedure _LapeImage_ReplaceColors(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ReplaceColors(PColorArray(Params^[1])^, PColorArray(Params^[2])^);
end;

(*
TSimbaImage.RotateNN
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.RotateNN(Radians: Single; Expand: Boolean): TSimbaImage;
*)
procedure _LapeImage_RotateNN(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.RotateNN(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaImage.RotateBilinear
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.RotateBilinear(Radians: Single; Expand: Boolean): TSimbaImage;
*)
procedure _LapeImage_RotateBilinear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.RotateBilinear(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaImage.GreyScale
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GreyScale: TSimbaImage;
*)
procedure _LapeImage_GreyScale(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.GreyScale();
end;

(*
TSimbaImage.Brightness
~~~~~~~~~~~~~~~~~
function TSimbaImage.Brightness(Value: Integer): TSimbaImage;
*)
procedure _LapeImage_Brightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Brightness(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Invert
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Invert: TSimbaImage;
*)
procedure _LapeImage_Invert(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Invert();
end;

(*
TSimbaImage.Posterize
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Posterize(Value: Integer): TSimbaImage;
*)
procedure _LapeImage_Posterize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Posterize(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Convolute
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Convolute(Matrix: TDoubleMatrix): TSimbaImage;
*)
procedure _LapeImage_Convolute(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Convolute(PDoubleMatrix(Params^[1])^);
end;

(*
TSimbaImage.Copy
~~~~~~~~~~~~~~~~
function TSimbaImage.Copy(X1, Y1, X2, Y2: Integer): TSimbaImage;
*)
procedure _LapeImage_Copy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TSimbaImage.Copy
~~~~~~~~~~~~~~~~
function TSimbaImage.Copy: TSimbaImage;
*)
procedure _LapeImage_CopyEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Copy();
end;

(*
TSimbaImage.ToLazBitmap
~~~~~~~~~~~~~~~~~~
function TSimbaImage.ToLazBitmap: TLazBitmap;
*)
procedure _LapeImage_ToLazBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSimbaImage(Params^[0])^.ToLazBitmap();
end;

(*
TSimbaImage.LoadFromLazBitmap
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromLazBitmap(LazBitmap: TLazBitmap);
*)
procedure _LapeImage_LoadFromLazBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromLazBitmap(PBitmap(Params^[1])^);
end;

(*
TSimbaImage.Crop
~~~~~~~~~~~~~~~~
procedure TSimbaImage.Crop(X1, Y1, X2, Y2: Integer);
*)
procedure _LapeImage_Crop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

(*
TSimbaImage.GetColors
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetColors: TColorArray;
*)
procedure _LapeImage_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(Params^[0])^.GetColors();
end;

(*
TSimbaImage.ToMatrix
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ToMatrix: TIntegerMatrix;
*)
procedure _LapeImage_ToMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaImage(Params^[0])^.ToMatrix();
end;

(*
TSimbaImage.ToMatrix
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
*)
procedure _LapeImage_ToMatrixEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaImage(Params^[0])^.ToMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TSimbaImage.DrawMatrix
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawMatrix(Matrix: TIntegerMatrix);
*)
procedure _LapeImage_DrawMatrixI(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawMatrix(PIntegerMatrix(Params^[1])^);
end;

(*
TSimbaImage.DrawMatrix
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
*)
procedure _LapeImage_DrawMatrixF(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.ThresholdAdaptive
~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: EImageThreshMethod; k: Integer): TSimbaImage;
*)
procedure _LapeImage_ThresholdAdaptive(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, ESimbaImageThreshMethod(Params^[4]^), PInteger(Params^[5])^);
end;

(*
TSimbaImage.ThresholdSauvola
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single): TSimbaImage;
*)
procedure _LapeImage_ThresholdSauvola(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ThresholdSauvola(PInteger(Params^[1])^, PBoolean(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TSimbaImage.Pad
~~~~~~~~~~~~~~~
procedure TSimbaImage.Pad(Amount: Integer);
*)
procedure _LapeImage_Pad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

(*
TSimbaImage.SetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetTransparentColor(Value: TColor);
*)
procedure _LapeImage_TransparentColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.TransparentColor := PColor(Params^[1])^;
end;

(*
TSimbaImage.GetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetTransparentColor: TColor;
*)
procedure _LapeImage_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(Params^[0])^.TransparentColor;
end;

(*
TSimbaImage.GetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetTransparentColorActive: Boolean;
*)
procedure _LapeImage_TransparentColorActive_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.TransparentColorActive;
end;

(*
TSimbaImage.SetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetTransparentColorActive(Value: Boolean);
*)
procedure _LapeImage_TransparentColorActive_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.TransparentColorActive := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.GetPixel
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetPixel(X, Y: Integer): TColor;
*)
procedure _LapeImage_GetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

(*
TSimbaImage.SetPixel
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetPixel(X, Y: Integer; Color: TColor);
*)
procedure _LapeImage_SetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

(*
TSimbaImage.SetPixels
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetPixels(Points: TPointArray; Colors: TColorArray);
*)
procedure _LapeImage_SetPixels(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TSimbaImage.GetPixels
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetPixels(Points: TPointArray): TColorArray;
*)
procedure _LapeImage_GetPixels(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(Params^[0])^.GetPixels(PPointArray(Params^[1])^);
end;

(*
TSimbaImage.Blur
~~~~~~~~~~~~~~~~
function TSimbaImage.Blur(Block: Integer): TSimbaImage;
*)
procedure _LapeImage_Blur(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Downsample
~~~~~~~~~~~~~~~~~
function TSimbaImage.Downsample(Scale: Integer): TSimbaImage;
*)
procedure _LapeImage_DownSample(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Downsample(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Free
~~~~~~~~~~~~~~~~
procedure TSimbaImage.Free;
*)
procedure _LapeImage_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Free();
end;

(*
TSimbaImage.DrawCross
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCrosshairs
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawLine
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawLine
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawLineEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawPolygon
~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygon(Points: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawPolygonFilled
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawPolygonInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);
*)
procedure _LapeImage_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawCircle
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCircleFilled
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCircleInverted
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeImage_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawBox
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBox(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawBoxFilled
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBoxFilled(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawBoxInverted
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBoxInverted(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawQuad
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuad(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawQuadFilled
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuadFilled(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawQuadInverted
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuadInverted(B: TBox; Color: TColor);
*)
procedure _LapeImage_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.DrawQuadArray
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawBoxArray
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawPolygonArray
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCircleArray
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeImage_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

(*
TSimbaImage.DrawCrossArray
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);
*)
procedure _LapeImage_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.Fill
~~~~~~~~~~~~~~~~
procedure TSimbaImage.Fill(Color: TColor);
*)
procedure _LapeImage_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Fill(PColor(Params^[1])^);
end;

(*
TSimbaImage.Clear
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.Clear;
*)
procedure _LapeImage_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Clear();
end;

(*
TSimbaImage.Clear
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.Clear(Area: TBox);
*)
procedure _LapeImage_ClearEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Clear(PBox(Params^[1])^);
end;

(*
TSimbaImage.ClearInverted
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ClearInverted(Area: TBox);
*)
procedure _LapeImage_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

(*
TSimbaImage.Draw
~~~~~~~~~~~~~~
procedure TSimbaImage.Draw(Image: TSimbaImage; X, Y: Integer);
*)
procedure _LapeImage_Draw1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TSimbaImage.Draw
~~~~~~~~~~~~~~
procedure TSimbaImage.Draw(Image: TSimbaImage; Position: TPoint);
*)
procedure _LapeImage_Draw2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TSimbaImage.Blend
~~~~~~~~~~~~~~~~~
function TSimbaImage.Blend(Points: TPointArray; Size: Integer): TSimbaImage;
*)
procedure _LapeImage_Blend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.GetCenter
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetCenter: TPoint;
*)
procedure _LapeImage_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(Params^[0])^.Center;
end;

(*
TSimbaImage.GetFontName
~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontName: String;
*)
procedure _LapeImage_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.FontName;
end;

(*
TSimbaImage.SetFontName
~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontName(Value: String);
*)
procedure _LapeImage_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontName := PString(Params^[1])^;
end;

(*
TSimbaImage.GetFontSize
~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontSize: Single;
*)
procedure _LapeImage_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(Params^[0])^.FontSize;
end;

(*
TSimbaImage.SetFontSize
~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontSize(Value: Single);
*)
procedure _LapeImage_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

(*
TSimbaImage.GetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontAntialiasing: Boolean;
*)
procedure _LapeImage_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontAntialiasing;
end;

(*
TSimbaImage.SetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontAntialiasing(Value: Boolean);
*)
procedure _LapeImage_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.GetFontBold
~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontBold: Boolean;
*)
procedure _LapeImage_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontBold;
end;

(*
TSimbaImage.SetFontBold
~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontBold(Value: Boolean);
*)
procedure _LapeImage_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.GetFontItalic
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontItalic: Boolean;
*)
procedure _LapeImage_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontItalic;
end;

(*
TSimbaImage.SetFontItalic
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontItalic(Value: Boolean);
*)
procedure _LapeImage_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.TextWidth
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.TextWidth(Text: String): Integer;
*)
procedure _LapeImage_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

(*
TSimbaImage.TextHeight
~~~~~~~~~~~~~~~~~
function TSimbaImage.TextHeight(Text: String): Integer;
*)
procedure _LapeImage_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

(*
TSimbaImage.TextSize
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.TextSize(Text: String): TPoint;
*)
procedure _LapeImage_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(Params^[0])^.TextSize(PString(Params^[1])^);
end;

(*
TSimbaImage.DrawText
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawText(Text: String; Position: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawText
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor);
*)
procedure _LapeImage_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

(*
TSimbaImage.DrawTextLines
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
*)
procedure _LapeImage_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TSimbaImage.Mirror
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Mirror(Style: EImageMirrorStyle): TSimbaImage;
*)
procedure _LapeImage_Mirror(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Mirror(ESimbaImageMirrorStyle(Params^[1]^));
end;

(*
TSimbaImage.Equals
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Equals(Other: TSimbaImage): Boolean;

Are the two images exactly equal?
*)
procedure _LapeImage_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.Equals(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifference(Other: TSimbaImage): Integer;
*)
procedure _LapeImage_PixelDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.PixelDifference(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifference(Other: TSimbaImage; Tolerance: Integer): Integer;
*)
procedure _LapeImage_PixelDifferenceTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.PixelDifference(PSimbaImage(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSimbaImage.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage): TPointArray;
*)
procedure _LapeImage_PixelDifferenceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
 PPointArray(Result)^ := PSimbaImage(Params^[0])^.PixelDifferenceTPA(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage; Tolerance: Integer): TPointArray;
*)
procedure _LapeImage_PixelDifferenceToleranceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImage(Params^[0])^.PixelDifferenceTPA(PSimbaImage(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TSimbaImage.LoadFromString
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromString(AWidth, AHeight: Integer; Str: String);
*)
procedure _LapeImage_LoadFromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromString(PInteger(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^);
end;

(*
TSimbaImage.LoadFromData
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromData(AWidth, AHeight: Integer; Memory: PColorBGRA; DataWidth: Integer);
*)
procedure _LapeImage_LoadFromData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromData(PInteger(Params^[1])^, PInteger(Params^[2])^, PPointer(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TSimbaImage.LoadFromImage
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromImage(Image: TSimbaImage);
*)
procedure _LapeImage_LoadFromImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromImage(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.Create
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Create: TSimbaImage; static;
*)
procedure _LapeImage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.Create();
end;

(*
TSimbaImage.Create
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Create(Width, Height: Integer): TSimbaImage; static;
*)
procedure _LapeImage_CreateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TSimbaImage.CreateFromFile
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromFile(FileName: String): TSimbaImage; static;
*)
procedure _LapeImage_CreateFromFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.CreateFromFile(PString(Params^[0])^);
end;

(*
TSimbaImage.CreateFromString
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromString(Width, Height: Integer; Str: String): TSimbaImage; static;
*)
procedure _LapeImage_CreateFromString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.CreateFromString(PInteger(Params^[0])^, PInteger(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaImage.Compare
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Compare(Other: TSimbaImage): Single;
*)
procedure _LapeImage_Compare(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(Params^[0])^.Compare(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.SaveUnfreedImages
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SaveUnfreedImages(Directory: String); static;

Saves unfreed images on script terminate.

Example:

```
  TSimbaImage.SaveUnfreedImages('some/directory/');
```
*)
procedure _LapeImage_SaveUnfreedImages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImage.SaveUnfreedImages := PString(Params^[0])^;
end;

(*
TSimbaImage.LoadFonts
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.LoadFonts(Dir: String): Boolean; static;

Loads all ".ttf" fonts in the passed directory.
*)
procedure _LapeImage_LoadFonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaImage.LoadFonts(PString(Params^[0])^);
end;

(*
TSimbaImage.GetFontNames
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontNames: TStringArray; static;

Returns all the available font names.
*)
procedure _LapeImage_LoadedFontNames(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaImage.LoadedFontNames();
end;

(*
TSimbaImage.FreeOnTerminate
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.FreeOnTerminate(Value: Boolean);
*)
procedure _LapeImage_FreeOnTerminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.DrawHSLCircle
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
*)
procedure _LapeImage_DrawHSLCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawHSLCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.RowPtrs
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.RowPtrs: TImageRowPtrs;
*)
procedure _LapeImage_RowPtrs(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImageRowPtrs(Result^) := PSimbaImage(Params^[0])^.RowPtrs();
end;

(*
TSimbaImage.Finder
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Finder: TSimbaFinder;

Returns a TSimbaFinder which is targetted to the image.
*)

(*
TSimbaImage.CreateFromTarget
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromTarget(Target: TSimbaTarget; Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage; static;

Creates an image from the given target and bounds.

- The **Bounds** parameter defaults to the entire target.
*)

(*
TSimbaImage.CreateFromTarget
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromTarget(Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage; static;

Creates an image from the bounds of the current target.

- Current target is the global **Target** variable
- The **Bounds** parameter defaults to the entire target.
*)

(*
TSimbaImage.DrawTarget
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTarget(Target: TSimbaTarget; P: TPoint; Bounds: TBox = [-1,-1,-1,-1]);
*)

(*
TSimbaImage.DrawTarget
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTarget(P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;
*)

procedure ImportSimbaImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TImage';

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
    addGlobalFunc('function TImage.CreateFromString(Width, Height: Integer; Str: String): TImage; static; overload', @_LapeImage_CreateFromString);

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

    addGlobalFunc('procedure TImage.DrawATPA(ATPA: T2DPointArray); overload', @_LapeImage_DrawATPA);
    addGlobalFunc('procedure TImage.DrawATPA(ATPA: T2DPointArray; Color: TColor); overload', @_LapeImage_DrawATPAEx);
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
    addGlobalFunc('procedure TImage.LoadFromString(AWidth, AHeight: Integer; Str: String)', @_LapeImage_LoadFromString);
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

