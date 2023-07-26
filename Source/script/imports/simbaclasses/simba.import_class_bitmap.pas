unit simba.import_class_bitmap;

{$i simba.inc}

interface

implementation

uses
  Classes, SysUtils, Graphics,
  lptypes,
  simba.script_compiler, simba.mufasatypes, simba.bitmap;

type
  PCanvas = ^TCanvas;
  PBitmap = ^TBitmap;

(*
TSimbaImage
=============
TSimbaImage is an Image data type.
*)

(*
TSimbaImage.InImage
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.InImage(X, Y: Integer): Boolean;
*)
procedure _LapeMufasaBitmap_InImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.InImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.EnsureInImage
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.EnsureInImage(var X, Y: Integer): Boolean;
*)
procedure _LapeMufasaBitmap_EnsureInImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.EnsureInImage(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.GetData
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetData: PColorBGRA;
*)
procedure _LapeMufasaBitmap_Data_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaImage(Params^[0])^.Data;
end;

(*
TSimbaImage.GetName
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetName: String;
*)
procedure _LapeMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.Name;
end;

(*
TSimbaImage.SetName
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetName(Value: String);
*)
procedure _LapeMufasaBitmap_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Name := PString(Params^[1])^;
end;

(*
TSimbaImage.SetSize
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetSize(AWidth, AHeight: Integer);
*)
procedure _LapeMufasaBitmap_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.ResizeNN
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ResizeNN(AWidth, AHeight: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_ResizeNN(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ResizeNN(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.ResizeBilinear
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ResizeBilinear(AWidth, AHeight: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_ResizeBilinear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ResizeBilinear(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.GetWidth
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetWidth: Integer;
*)
procedure _LapeMufasaBitmap_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.Width;
end;

(*
TSimbaImage.GetHeight
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetHeight: Integer;
*)
procedure _LapeMufasaBitmap_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.Height;
end;

(*
TSimbaImage.SetExternalData
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetExternalData(AData: PColorBGRA; AWidth, AHeight: Integer);
*)
procedure _LapeMufasaBitmap_SetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetExternalData(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TSimbaImage.ResetExternalData
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ResetExternalData;
*)
procedure _LapeMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ResetExternalData();
end;

(*
TSimbaImage.SaveToFile
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.SaveToFile(FileName: String; OverwriteIfExists: Boolean = False): Boolean;
*)
procedure _LapeMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.SaveToFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaImage.SaveToString
~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.SaveToString: String;
*)
procedure _LapeMufasaBitmap_SaveToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.SaveToString();
end;

(*
TSimbaImage.LoadFromFile
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromFile(FileName: String);
*)
procedure _LapeMufasaBitmap_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

(*
TSimbaImage.LoadFromFile
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromFile(FileName: String; Area: TBox);
*)
procedure _LapeMufasaBitmap_LoadFromFileEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromFile(PString(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaImage.DrawATPA
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray);
*)
procedure _LapeMufasaBitmap_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

(*
TSimbaImage.DrawATPA
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawATPAEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawTPA
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTPA(TPA: TPointArray; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.ReplaceColor
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor);
*)
procedure _LapeMufasaBitmap_ReplaceColor(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.ReplaceColors
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ReplaceColors(OldColors, NewColors: TColorArray);
*)
procedure _LapeMufasaBitmap_ReplaceColors(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ReplaceColors(PColorArray(Params^[1])^, PColorArray(Params^[2])^);
end;

(*
TSimbaImage.RotateNN
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.RotateNN(Radians: Single; Expand: Boolean): TSimbaImage;
*)
procedure _LapeMufasaBitmap_RotateNN(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.RotateNN(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaImage.RotateBilinear
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.RotateBilinear(Radians: Single; Expand: Boolean): TSimbaImage;
*)
procedure _LapeMufasaBitmap_RotateBilinear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.RotateBilinear(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TSimbaImage.GreyScale
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GreyScale: TSimbaImage;
*)
procedure _LapeMufasaBitmap_GreyScale(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.GreyScale();
end;

(*
TSimbaImage.Brightness
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Brightness(Value: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Brightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Brightness(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Invert
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Invert: TSimbaImage;
*)
procedure _LapeMufasaBitmap_Invert(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Invert();
end;

(*
TSimbaImage.Posterize
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Posterize(Value: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Posterize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Posterize(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Convolute
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Convolute(Matrix: TDoubleMatrix): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Convolute(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Convolute(PDoubleMatrix(Params^[1])^);
end;

(*
TSimbaImage.Copy
~~~~~~~~~~~~~~~~
function TSimbaImage.Copy(X1, Y1, X2, Y2: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TSimbaImage.Copy
~~~~~~~~~~~~~~~~
function TSimbaImage.Copy: TSimbaImage;
*)
procedure _LapeMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Copy();
end;

(*
TSimbaImage.ToTBitmap
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ToTBitmap: TBitmap;
*)
procedure _LapeMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSimbaImage(Params^[0])^.ToTBitmap();
end;

(*
TSimbaImage.Crop
~~~~~~~~~~~~~~~~
procedure TSimbaImage.Crop(X1, Y1, X2, Y2: Integer);
*)
procedure _LapeMufasaBitmap_Crop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

(*
TSimbaImage.GetColors
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetColors: TColorArray;
*)
procedure _LapeMufasaBitmap_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(Params^[0])^.GetColors();
end;

(*
TSimbaImage.ToMatrix
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ToMatrix: TIntegerMatrix;
*)
procedure _LapeMufasaBitmap_ToMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaImage(Params^[0])^.ToMatrix();
end;

(*
TSimbaImage.ToMatrix
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
*)
procedure _LapeMufasaBitmap_ToMatrixEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaImage(Params^[0])^.ToMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TSimbaImage.DrawMatrix
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawMatrix(Matrix: TIntegerMatrix);
*)
procedure _LapeMufasaBitmap_DrawMatrixI(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawMatrix(PIntegerMatrix(Params^[1])^);
end;

(*
TSimbaImage.DrawMatrix
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
*)
procedure _LapeMufasaBitmap_DrawMatrixF(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.ThresholdAdaptive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: ESimbaImageThreshMethod; k: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_ThresholdAdaptive(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, ESimbaImageThreshMethod(Params^[4]^), PInteger(Params^[5])^);
end;

(*
TSimbaImage.ThresholdSauvola
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single): TSimbaImage;
*)
procedure _LapeMufasaBitmap_ThresholdSauvola(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.ThresholdSauvola(PInteger(Params^[1])^, PBoolean(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TSimbaImage.Pad
~~~~~~~~~~~~~~~
procedure TSimbaImage.Pad(Amount: Integer);
*)
procedure _LapeMufasaBitmap_Pad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

(*
TSimbaImage.LoadFromTBitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromTBitmap(bmp: TBitmap);
*)
procedure _LapeMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

(*
TSimbaImage.SetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetTransparentColor(Value: TColor);
*)
procedure _LapeMufasaBitmap_TransparentColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.TransparentColor := PColor(Params^[1])^;
end;

(*
TSimbaImage.GetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetTransparentColor: TColor;
*)
procedure _LapeMufasaBitmap_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(Params^[0])^.TransparentColor;
end;

(*
TSimbaImage.GetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetTransparentColorActive: Boolean;
*)
procedure _LapeMufasaBitmap_TransparentColorActive_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.TransparentColorActive;
end;

(*
TSimbaImage.SetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetTransparentColorActive(Value: Boolean);
*)
procedure _LapeMufasaBitmap_TransparentColorActive_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.TransparentColorActive := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.GetPixel
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetPixel(X, Y: Integer): TColor;
*)
procedure _LapeMufasaBitmap_GetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaImage(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

(*
TSimbaImage.SetPixel
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetPixel(X, Y: Integer; Color: TColor);
*)
procedure _LapeMufasaBitmap_SetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^] := PColor(Params^[3])^;
end;

(*
TSimbaImage.SetPixels
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetPixels(Points: TPointArray; Colors: TColorArray);
*)
procedure _LapeMufasaBitmap_SetPixels(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TSimbaImage.GetPixels
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetPixels(Points: TPointArray): TColorArray;
*)
procedure _LapeMufasaBitmap_GetPixels(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaImage(Params^[0])^.GetPixels(PPointArray(Params^[1])^);
end;

(*
TSimbaImage.Blur
~~~~~~~~~~~~~~~~
function TSimbaImage.Blur(Block: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Blur(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Downsample
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Downsample(Scale: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_DownSample(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Downsample(PInteger(Params^[1])^);
end;

(*
TSimbaImage.Free
~~~~~~~~~~~~~~~~
procedure TSimbaImage.Free;
*)
procedure _LapeMufasaBitmap_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Free();
end;

(*
TSimbaImage.DrawCross
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCross(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCrosshairs
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawLine
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawLine
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawLineEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawPolygon
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygon(Points: TPointArray; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawPolygonFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawPolygonInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawCircle
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCircleFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCircleInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawBox
~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBox(B: TBox; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBox(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawBoxFilled
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBoxFilled(B: TBox; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawBoxInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBoxInverted(B: TBox; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawQuad
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuad(B: TBox; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawQuadFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuadFilled(B: TBox; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PColor(Params^[2])^);
end;

(*
TSimbaImage.DrawQuadInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuadInverted(B: TBox; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.DrawQuadArray
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeMufasaBitmap_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawBoxArray
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeMufasaBitmap_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawPolygonArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeMufasaBitmap_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawCircleArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);
*)
procedure _LapeMufasaBitmap_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

(*
TSimbaImage.DrawCrossArray
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: TColor = -1);
*)
procedure _LapeMufasaBitmap_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.Fill
~~~~~~~~~~~~~~~~
procedure TSimbaImage.Fill(Color: TColor);
*)
procedure _LapeMufasaBitmap_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Fill(PColor(Params^[1])^);
end;

(*
TSimbaImage.Clear
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.Clear;
*)
procedure _LapeMufasaBitmap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Clear();
end;

(*
TSimbaImage.Clear
~~~~~~~~~~~~~~~~~
procedure TSimbaImage.Clear(Area: TBox);
*)
procedure _LapeMufasaBitmap_ClearEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Clear(PBox(Params^[1])^);
end;

(*
TSimbaImage.ClearInverted
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.ClearInverted(Area: TBox);
*)
procedure _LapeMufasaBitmap_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

(*
TSimbaImage.Draw
~~~~~~~~~~~~~~
procedure TSimbaImage.Draw(Image: TSimbaImage; X, Y: Integer);
*)
procedure _LapeMufasaBitmap_Draw1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TSimbaImage.Draw
~~~~~~~~~~~~~~
procedure TSimbaImage.Draw(Image: TSimbaImage; Position: TPoint);
*)
procedure _LapeMufasaBitmap_Draw2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.Draw(PSimbaImage(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TSimbaImage.Blend
~~~~~~~~~~~~~~~~~
function TSimbaImage.Blend(Points: TPointArray; Size: Integer): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Blend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.GetCenter
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetCenter: TPoint;
*)
procedure _LapeMufasaBitmap_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(Params^[0])^.Center;
end;

(*
TSimbaImage.GetFontName
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontName: String;
*)
procedure _LapeMufasaBitmap_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImage(Params^[0])^.FontName;
end;

(*
TSimbaImage.SetFontName
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontName(Value: String);
*)
procedure _LapeMufasaBitmap_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontName := PString(Params^[1])^;
end;

(*
TSimbaImage.GetFontSize
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontSize: Single;
*)
procedure _LapeMufasaBitmap_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(Params^[0])^.FontSize;
end;

(*
TSimbaImage.SetFontSize
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontSize(Value: Single);
*)
procedure _LapeMufasaBitmap_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

(*
TSimbaImage.GetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontAntialiasing: Boolean;
*)
procedure _LapeMufasaBitmap_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontAntialiasing;
end;

(*
TSimbaImage.SetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontAntialiasing(Value: Boolean);
*)
procedure _LapeMufasaBitmap_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.GetFontBold
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontBold: Boolean;
*)
procedure _LapeMufasaBitmap_FontBold_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontBold;
end;

(*
TSimbaImage.SetFontBold
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontBold(Value: Boolean);
*)
procedure _LapeMufasaBitmap_FontBold_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontBold := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.GetFontItalic
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontItalic: Boolean;
*)
procedure _LapeMufasaBitmap_FontItalic_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.FontItalic;
end;

(*
TSimbaImage.SetFontItalic
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SetFontItalic(Value: Boolean);
*)
procedure _LapeMufasaBitmap_FontItalic_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FontItalic := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.TextWidth
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.TextWidth(Text: String): Integer;
*)
procedure _LapeMufasaBitmap_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

(*
TSimbaImage.TextHeight
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.TextHeight(Text: String): Integer;
*)
procedure _LapeMufasaBitmap_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

(*
TSimbaImage.TextSize
~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.TextSize(Text: String): TPoint;
*)
procedure _LapeMufasaBitmap_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaImage(Params^[0])^.TextSize(PString(Params^[1])^);
end;

(*
TSimbaImage.DrawText
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawText(Text: String; Position: TPoint; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

(*
TSimbaImage.DrawText
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

(*
TSimbaImage.DrawTextLines
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);
*)
procedure _LapeMufasaBitmap_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TSimbaImage.Mirror
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Mirror(Style: ESimbaImageMirrorStyle): TSimbaImage;
*)
procedure _LapeMufasaBitmap_Mirror(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImage(Params^[0])^.Mirror(ESimbaImageMirrorStyle(Params^[1]^));
end;

(*
TSimbaImage.Equals
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Equals(Other: TSimbaImage): Boolean;

Are the two bitmaps exactly equal?
*)
procedure _LapeMufasaBitmap_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImage(Params^[0])^.Equals(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifference(Other: TSimbaImage): Integer;
*)
procedure _LapeMufasaBitmap_PixelDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.PixelDifference(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifference(Other: TSimbaImage; Tolerance: Integer): Integer;
*)
procedure _LapeMufasaBitmap_PixelDifferenceTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImage(Params^[0])^.PixelDifference(PSimbaImage(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage): TPointArray;
*)
procedure _LapeMufasaBitmap_PixelDifferenceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
 PPointArray(Result)^ := PSimbaImage(Params^[0])^.PixelDifferenceTPA(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage; Tolerance: Integer): TPointArray;
*)
procedure _LapeMufasaBitmap_PixelDifferenceToleranceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaImage(Params^[0])^.PixelDifferenceTPA(PSimbaImage(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.LoadFromString
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromString(AWidth, AHeight: Integer; Str: String);
*)
procedure _LapeMufasaBitmap_LoadFromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromString(PInteger(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^);
end;

(*
TSimbaImage.LoadFromData
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromData(AWidth, AHeight: Integer; Memory: PColorBGRA; DataWidth: Integer);
*)
procedure _LapeMufasaBitmap_LoadFromData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromData(PInteger(Params^[1])^, PInteger(Params^[2])^, PPointer(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TSimbaImage.LoadFromImage
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.LoadFromImage(Bitmap: TSimbaImage);
*)
procedure _LapeMufasaBitmap_LoadFromImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.LoadFromImage(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.Create
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Create: TSimbaImage; static;
*)
procedure _LapeMufasaBitmap_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.Create();
end;

(*
TSimbaImage.Create
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Create(Width, Height: Integer): TSimbaImage; static;
*)
procedure _LapeMufasaBitmap_CreateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TSimbaImage.CreateFromFile
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromFile(FileName: String): TSimbaImage; static;
*)
procedure _LapeMufasaBitmap_CreateFromFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.CreateFromFile(PString(Params^[0])^);
end;

(*
TSimbaImage.CreateFromString
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromString(Width, Height: Integer; Str: String): TSimbaImage; static;
*)
procedure _LapeMufasaBitmap_CreateFromString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := TSimbaImage.CreateFromString(PInteger(Params^[0])^, PInteger(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaImage.Compare
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.Compare(Other: TSimbaImage): Single;
*)
procedure _LapeMufasaBitmap_Compare(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PSimbaImage(Params^[0])^.Compare(PSimbaImage(Params^[1])^);
end;

(*
TSimbaImage.SaveUnfreedImages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.SaveUnfreedImages(Directory: String); static;

Saves unfreed images on script terminate.

Example:

```
  TSimbaImage.SaveUnfreedImages('some/directory/');
```
*)
procedure _LapeMufasaBitmap_SaveUnfreedImages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImage.SaveUnfreedImages := PString(Params^[0])^;
end;

(*
TSimbaImage.LoadFonts
~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.LoadFonts(Dir: String): Boolean; static;

Loads all ".ttf" fonts in the passed directory.
*)
procedure _LapeMufasaBitmap_LoadFonts(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaImage.LoadFonts(PString(Params^[0])^);
end;

(*
TSimbaImage.GetFontNames
~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.GetFontNames: TStringArray; static;

Returns all the available font names.
*)
procedure _LapeMufasaBitmap_LoadedFontNames(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := TSimbaImage.LoadedFontNames();
end;

(*
TSimbaImage.FreeOnTerminate
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.FreeOnTerminate(Value: Boolean);
*)
procedure _LapeMufasaBitmap_FreeOnTerminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

(*
TSimbaImage.DrawHSLCircle
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
*)
procedure _LapeMufasaBitmap_DrawHSLCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Params^[0])^.DrawHSLCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaImage.RowPtrs
~~~~~~~~~~~~~~~~~~~
function TSimbaImage.RowPtrs: TSimbaImageRowPtrs;
*)
procedure _LapeMufasaBitmap_RowPtrs(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImageRowPtrs(Result^) := PSimbaImage(Params^[0])^.RowPtrs();
end;

(*
TSimbaImage.Finder
~~~~~~~~~~~~~~~~~~
function TSimbaImage.Finder: TSimbaFinder;

Returns a TSimbaFinder which is targetted to the bitmap.
*)

(*
TSimbaImage.CreateFromTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromTarget(Target: TSimbaTarget; Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage; static;

Creates a bitmap from the given target and bounds.

- The **Bounds** parameter defaults to the entire target.
*)

(*
TSimbaImage.CreateFromTarget
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaImage.CreateFromTarget(Bounds: TBox = [-1,-1,-1,-1]): TSimbaImage; static;

Creates a bitmap from the bounds of the current target.

- Current target is the global **Target** variable
- The **Bounds** parameter defaults to the entire target.
*)

(*
TSimbaImage.DrawTarget
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTarget(Target: TSimbaTarget; P: TPoint; Bounds: TBox = [-1,-1,-1,-1]);
*)

(*
TSimbaImage.DrawTarget
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaImage.DrawTarget(P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;
*)

procedure ImportBitmap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TSimbaImage';

    addClass('TSimbaImage');

    addGlobalType('array of TSimbaImage', 'TSimbaImageArray');
    addGlobalType('array of PColorBGRA', 'TSimbaImageRowPtrs');
    addGlobalType('enum(WIDTH, HEIGHT, LINE)', 'ESimbaImageMirrorStyle');
    addGlobalType('enum(MEAN, MIN_MAX)', 'ESimbaImageThreshMethod');

    addClassVar('TSimbaImage', 'Data', 'PColorBGRA', @_LapeMufasaBitmap_Data_Read);
    addClassVar('TSimbaImage', 'Name', 'String', @_LapeMufasaBitmap_Name_Read, @_LapeMufasaBitmap_Name_Write);
    addClassVar('TSimbaImage', 'Width', 'Integer', @_LapeMufasaBitmap_Width_Read);
    addClassVar('TSimbaImage', 'Height', 'Integer', @_LapeMufasaBitmap_Height_Read);
    addClassVar('TSimbaImage', 'Center', 'TPoint', @_LapeMufasaBitmap_Center_Read);
    addClassVar('TSimbaImage', 'TransparentColor', 'Integer', @_LapeMufasaBitmap_TransparentColor_Read, @_LapeMufasaBitmap_TransparentColor_Write);
    addClassVar('TSimbaImage', 'TransparentColorActive', 'Boolean', @_LapeMufasaBitmap_TransparentColorActive_Read, @_LapeMufasaBitmap_TransparentColorActive_Write);

    addClassVar('TSimbaImage', 'FontName', 'String', @_LapeMufasaBitmap_FontName_Read, @_LapeMufasaBitmap_FontName_Write);
    addClassVar('TSimbaImage', 'FontSize', 'Single', @_LapeMufasaBitmap_FontSize_Read, @_LapeMufasaBitmap_FontSize_Write);
    addClassVar('TSimbaImage', 'FontAntialiasing', 'Boolean', @_LapeMufasaBitmap_FontAntialiasing_Read, @_LapeMufasaBitmap_FontAntialiasing_Write);
    addClassVar('TSimbaImage', 'FontBold', 'Boolean', @_LapeMufasaBitmap_FontBold_Read, @_LapeMufasaBitmap_FontBold_Write);
    addClassVar('TSimbaImage', 'FontItalic', 'Boolean', @_LapeMufasaBitmap_FontItalic_Read, @_LapeMufasaBitmap_FontItalic_Write);

    addGlobalFunc('function TSimbaImage.RowPtrs: TSimbaImageRowPtrs', @_LapeMufasaBitmap_RowPtrs);

    addGlobalFunc('function TSimbaImage.LoadedFontNames: TStringArray; static;', @_LapeMufasaBitmap_LoadedFontNames);
    addGlobalFunc('function TSimbaImage.LoadFonts(Dir: String): Boolean; static;', @_LapeMufasaBitmap_LoadFonts);

    addGlobalFunc('function TSimbaImage.InImage(X, Y: Integer): Boolean', @_LapeMufasaBitmap_InImage);
    addGlobalFunc('procedure TSimbaImage.EnsureInImage(var X, Y: Integer)', @_LapeMufasaBitmap_EnsureInImage);

    addGlobalFunc('function TSimbaImage.Create: TSimbaImage; static; overload', @_LapeMufasaBitmap_Create);
    addGlobalFunc('function TSimbaImage.Create(Width, Height: Integer): TSimbaImage; static; overload', @_LapeMufasaBitmap_CreateEx);
    addGlobalFunc('function TSimbaImage.CreateFromFile(FileName: String): TSimbaImage; static; overload', @_LapeMufasaBitmap_CreateFromFile);
    addGlobalFunc('function TSimbaImage.CreateFromString(Width, Height: Integer; Str: String): TSimbaImage; static; overload', @_LapeMufasaBitmap_CreateFromString);

    addGlobalFunc('function TSimbaImage.Equals(Other: TSimbaImage): Boolean;', @_LapeMufasaBitmap_Equals);

    addGlobalFunc('procedure TSimbaImage.SetPixel(X, Y: Integer; Color: TColor);', @_LapeMufasaBitmap_SetPixel);
    addGlobalFunc('function TSimbaImage.GetPixel(X, Y: Integer): TColor;', @_LapeMufasaBitmap_GetPixel);

    addGlobalFunc('procedure TSimbaImage.SetPixels(Points: TPointArray; Colors: TColorArray);', @_LapeMufasaBitmap_SetPixels);
    addGlobalFunc('function TSimbaImage.GetPixels(Points: TPointArray): TColorArray;', @_LapeMufasaBitmap_GetPixels);

    addGlobalFunc('function TSimbaImage.PixelDifference(Other: TSimbaImage): Integer; overload', @_LapeMufasaBitmap_PixelDifference);
    addGlobalFunc('function TSimbaImage.PixelDifference(Other: TSimbaImage; Tolerance: Integer): Integer; overload', @_LapeMufasaBitmap_PixelDifferenceTolerance);

    addGlobalFunc('function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage): TPointArray; overload', @_LapeMufasaBitmap_PixelDifferenceTPA);
    addGlobalFunc('function TSimbaImage.PixelDifferenceTPA(Other: TSimbaImage; Tolerance: Integer): TPointArray; overload', @_LapeMufasaBitmap_PixelDifferenceToleranceTPA);

    addGlobalFunc('function TSimbaImage.TextWidth(Text: String): Integer;', @_LapeMufasaBitmap_TextWidth);
    addGlobalFunc('function TSimbaImage.TextHeight(Text: String): Integer;', @_LapeMufasaBitmap_TextHeight);
    addGlobalFunc('function TSimbaImage.TextSize(Text: String): TPoint;', @_LapeMufasaBitmap_TextSize);
    addGlobalFunc('procedure TSimbaImage.DrawText(Text: String; Position: TPoint; Color: TColor); overload', @_LapeMufasaBitmap_DrawText);
    addGlobalFunc('procedure TSimbaImage.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor); overload', @_LapeMufasaBitmap_DrawTextEx);
    addGlobalFunc('procedure TSimbaImage.DrawTextLines(Text: TStringArray; Position: TPoint; Color: TColor);', @_LapeMufasaBitmap_DrawTextLines);

    addGlobalFunc('procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray); overload', @_LapeMufasaBitmap_DrawATPA);
    addGlobalFunc('procedure TSimbaImage.DrawATPA(ATPA: T2DPointArray; Color: TColor); overload', @_LapeMufasaBitmap_DrawATPAEx);
    addGlobalFunc('procedure TSimbaImage.DrawTPA(TPA: TPointArray; Color: TColor);', @_LapeMufasaBitmap_DrawTPA);

    addGlobalFunc('procedure TSimbaImage.DrawCrosshairs(ACenter: TPoint; Size: Integer; Thickness: Integer; Color: TColor);', @_LapeMufasaBitmap_DrawCrosshairs);
    addGlobalFunc('procedure TSimbaImage.DrawCross(ACenter: TPoint; Radius: Integer; Thickness: Integer; Color: TColor);', @_LapeMufasaBitmap_DrawCross);

    addGlobalFunc('procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Color: TColor); overload', @_LapeMufasaBitmap_DrawLine);
    addGlobalFunc('procedure TSimbaImage.DrawLine(Start, Stop: TPoint; Thickness: Integer; Color: TColor); overload', @_LapeMufasaBitmap_DrawLineEx);

    addGlobalFunc('procedure TSimbaImage.DrawPolygon(Points: TPointArray; Color: TColor);', @_LapeMufasaBitmap_DrawPolygon);
    addGlobalFunc('procedure TSimbaImage.DrawPolygonFilled(Points: TPointArray; Color: TColor);', @_LapeMufasaBitmap_DrawPolygonFilled);
    addGlobalFunc('procedure TSimbaImage.DrawPolygonInverted(Points: TPointArray; Color: TColor);', @_LapeMufasaBitmap_DrawPolygonInverted);

    addGlobalFunc('procedure TSimbaImage.DrawCircle(ACenter: TPoint; Radius: Integer; Color: TColor);', @_LapeMufasaBitmap_DrawCircle);
    addGlobalFunc('procedure TSimbaImage.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: TColor);', @_LapeMufasaBitmap_DrawCircleFilled);
    addGlobalFunc('procedure TSimbaImage.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: TColor);', @_LapeMufasaBitmap_DrawCircleInverted);

    addGlobalFunc('procedure TSimbaImage.DrawBox(B: TBox; Color: TColor);', @_LapeMufasaBitmap_DrawBox);
    addGlobalFunc('procedure TSimbaImage.DrawBoxFilled(B: TBox; Color: TColor);', @_LapeMufasaBitmap_DrawBoxFilled);
    addGlobalFunc('procedure TSimbaImage.DrawBoxInverted(B: TBox; Color: TColor);', @_LapeMufasaBitmap_DrawBoxInverted);

    addGlobalFunc('procedure TSimbaImage.DrawQuad(Quad: TQuad; Color: TColor);', @_LapeMufasaBitmap_DrawQuad);
    addGlobalFunc('procedure TSimbaImage.DrawQuadFilled(Quad: TQuad; Color: TColor);', @_LapeMufasaBitmap_DrawQuadFilled);
    addGlobalFunc('procedure TSimbaImage.DrawQuadInverted(Quad: TQuad; Color: TColor);', @_LapeMufasaBitmap_DrawQuadInverted);

    addGlobalFunc('procedure TSimbaImage.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: TColor = -1);', @_LapeMufasaBitmap_DrawQuadArray);
    addGlobalFunc('procedure TSimbaImage.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: TColor = -1);', @_LapeMufasaBitmap_DrawBoxArray);
    addGlobalFunc('procedure TSimbaImage.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: TColor = -1);', @_LapeMufasaBitmap_DrawPolygonArray);
    addGlobalFunc('procedure TSimbaImage.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: TColor = -1);', @_LapeMufasaBitmap_DrawCircleArray);
    addGlobalFunc('procedure TSimbaImage.DrawCrossArray(Points: TPointArray; Radius: Integer; Thickness: Integer; Color: TColor = -1);', @_LapeMufasaBitmap_DrawCrossArray);

    addGlobalFunc('procedure TSimbaImage.DrawHSLCircle(ACenter: TPoint; Radius: Integer)', @_LapeMufasaBitmap_DrawHSLCircle);

    addGlobalFunc('procedure TSimbaImage.Clear; overload', @_LapeMufasaBitmap_Clear);
    addGlobalFunc('procedure TSimbaImage.Clear(Area: TBox); overload', @_LapeMufasaBitmap_ClearEx);
    addGlobalFunc('procedure TSimbaImage.ClearInverted(Area: TBox);', @_LapeMufasaBitmap_ClearInverted);

    addGlobalFunc('procedure TSimbaImage.Draw(Image: TSimbaImage; X, Y: Integer); overload', @_LapeMufasaBitmap_Draw1);
    addGlobalFunc('procedure TSimbaImage.Draw(Image: TSimbaImage; Position: TPoint); overload', @_LapeMufasaBitmap_Draw2);

    addGlobalFunc('procedure TSimbaImage.DrawMatrix(Matrix: TIntegerMatrix); overload', @_LapeMufasaBitmap_DrawMatrixI);
    addGlobalFunc('procedure TSimbaImage.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload', @_LapeMufasaBitmap_DrawMatrixF);

    addGlobalFunc('procedure TSimbaImage.SetSize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetSize);
    addGlobalFunc('procedure TSimbaImage.SetPersistentMemory(Memory: PtrUInt; AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetPersistentMemory);
    addGlobalFunc('procedure TSimbaImage.ResetPersistentMemory;', @_LapeMufasaBitmap_ResetPersistentMemory);

    addGlobalFunc('function TSimbaImage.ResizeNN(AWidth, AHeight: Integer): TSimbaImage', @_LapeMufasaBitmap_ResizeNN);
    addGlobalFunc('function TSimbaImage.ResizeBilinear(AWidth, AHeight: Integer): TSimbaImage', @_LapeMufasaBitmap_ResizeBilinear);

    addGlobalFunc('function TSimbaImage.RotateNN(Radians: Single; Expand: Boolean): TSimbaImage', @_LapeMufasaBitmap_RotateNN);
    addGlobalFunc('function TSimbaImage.RotateBilinear(Radians: Single; Expand: Boolean): TSimbaImage', @_LapeMufasaBitmap_RotateBilinear);

    addGlobalFunc('procedure TSimbaImage.Fill(Color: TColor);', @_LapeMufasaBitmap_Fill);
    addGlobalFunc('procedure TSimbaImage.ReplaceColor(OldColor, NewColor: TColor);', @_LapeMufasaBitmap_ReplaceColor);
    addGlobalFunc('procedure TSimbaImage.ReplaceColors(OldColors, NewColors: TColorArray);', @_LapeMufasaBitmap_ReplaceColors);

    addGlobalFunc('function TSimbaImage.GreyScale: TSimbaImage', @_LapeMufasaBitmap_GreyScale);
    addGlobalFunc('function TSimbaImage.Brightness(Value: Integer): TSimbaImage', @_LapeMufasaBitmap_Brightness);
    addGlobalFunc('function TSimbaImage.Invert: TSimbaImage', @_LapeMufasaBitmap_Invert);
    addGlobalFunc('function TSimbaImage.Posterize(Value: Integer): TSimbaImage', @_LapeMufasaBitmap_Posterize);
    addGlobalFunc('function TSimbaImage.Convolute(Matrix: TDoubleMatrix): TSimbaImage', @_LapeMufasaBitmap_Convolute);
    addGlobalFunc('function TSimbaImage.Mirror(Style: ESimbaImageMirrorStyle): TSimbaImage', @_LapeMufasaBitmap_Mirror);
    addGlobalFunc('function TSimbaImage.Blur(Block: Integer): TSimbaImage', @_LapeMufasaBitmap_Blur);
    addGlobalFunc('function TSimbaImage.Blend(Points: TPointArray; Radius: Integer): TSimbaImage', @_LapeMufasaBitmap_Blend);
    addGlobalFunc('function TSimbaImage.Downsample(Scale: Integer): TSimbaImage', @_LapeMufasaBitmap_Downsample);

    addGlobalFunc('function TSimbaImage.Copy(X1, Y1, X2, Y2: Integer): TSimbaImage; overload', @_LapeMufasaBitmap_Copy);
    addGlobalFunc('function TSimbaImage.Copy: TSimbaImage; overload', @_LapeMufasaBitmap_CopyEx);
    addGlobalFunc('procedure TSimbaImage.Crop(X1, Y1, X2, Y2: Integer);', @_LapeMufasaBitmap_Crop);
    addGlobalFunc('function TSimbaImage.GetColors: TColorArray;', @_LapeMufasaBitmap_GetColors);
    addGlobalFunc('function TSimbaImage.ToMatrix: TIntegerMatrix; overload', @_LapeMufasaBitmap_ToMatrix);
    addGlobalFunc('function TSimbaImage.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix; overload', @_LapeMufasaBitmap_ToMatrixEx);
    addGlobalFunc('function TSimbaImage.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: ESimbaImageThreshMethod; k: Integer): TSimbaImage', @_LapeMufasaBitmap_ThresholdAdaptive);
    addGlobalFunc('function TSimbaImage.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single): TSimbaImage', @_LapeMufasaBitmap_ThresholdSauvola);
    addGlobalFunc('procedure TSimbaImage.Pad(Amount: Integer)', @_LapeMufasaBitmap_Pad);

    addGlobalFunc('procedure TSimbaImage.LoadFromFile(FileName: String); overload', @_LapeMufasaBitmap_LoadFromFile);
    addGlobalFunc('procedure TSimbaImage.LoadFromFile(FileName: String; Area: TBox); overload', @_LapeMufasaBitmap_LoadFromFileEx);
    addGlobalFunc('procedure TSimbaImage.LoadFromString(AWidth, AHeight: Integer; Str: String)', @_LapeMufasaBitmap_LoadFromString);
    addGlobalFunc('procedure TSimbaImage.LoadFromData(AWidth, AHeight: Integer; AData: PColorBGRA; DataWidth: Integer)', @_LapeMufasaBitmap_LoadFromData);
    addGlobalFunc('procedure TSimbaImage.LoadFromImage(Image: TSimbaImage);', @_LapeMufasaBitmap_LoadFromImage);

    addGlobalFunc('function TSimbaImage.SaveToFile(FileName: String; OverwriteIfExists: Boolean = False): Boolean;', @_LapeMufasaBitmap_SaveToFile);
    addGlobalFunc('function TSimbaImage.SaveToString: String;', @_LapeMufasaBitmap_SaveToString);

    addGlobalFunc('function TSimbaImage.ToTBitmap: TBitmap;', @_LapeMufasaBitmap_ToTBitmap);
    addGlobalFunc('procedure TSimbaImage.LoadFromTBitmap(bmp: TBitmap);', @_LapeMufasaBitmap_LoadFromTBitmap);
    addGlobalFunc('function TSimbaImage.Compare(Other: TSimbaImage): Single;', @_LapeMufasaBitmap_Compare);

    addGlobalFunc('procedure TSimbaImage.SaveUnfreedImages(Directory: String); static;', @_LapeMufasaBitmap_SaveUnfreedImages);
    addGlobalFunc('procedure TSimbaImage.FreeOnTerminate(Value: Boolean);', @_LapeMufasaBitmap_FreeOnTerminate);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportBitmap);

end.

