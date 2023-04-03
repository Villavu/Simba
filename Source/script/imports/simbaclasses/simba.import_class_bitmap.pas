unit simba.import_class_bitmap;

{$i simba.inc}

interface

implementation

uses
  Classes, SysUtils, Graphics,
  lptypes,
  simba.script_compiler, simba.mufasatypes, simba.bitmap, simba.colormath, simba.colormath_distance;

type
  PCanvas = ^TCanvas;
  PBitmap = ^TBitmap;

(*
TMufasaBitmap
=============
*)

(*
TMufasaBitmap.PointInBitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.PointInBitmap(P: TPoint): Boolean;
*)
procedure _LapeMufasaBitmap_PointInBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(PPoint(Params^[1])^);
end;

(*
TMufasaBitmap.PointInBitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.PointInBitmap(X, Y: Integer): Boolean;
*)
procedure _LapeMufasaBitmap_PointInBitmapEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.GetData
~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetData: PRGB32;
*)
procedure _LapeMufasaBitmap_Data_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PMufasaBitmap(Params^[0])^.Data;
end;

(*
TMufasaBitmap.GetName
~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetName: String;
*)
procedure _LapeMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.Name;
end;

(*
TMufasaBitmap.SetName
~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetName(Value: String);
*)
procedure _LapeMufasaBitmap_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Name := PString(Params^[1])^;
end;

(*
TMufasaBitmap.SetSize
~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetSize(AWidth, AHeight: Integer);
*)
procedure _LapeMufasaBitmap_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Resize
~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Resize(AWidth, AHeight: Integer);
*)
procedure _LapeMufasaBitmap_Resize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Resize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.ResizeBilinear
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ResizeBilinear(AWidth, AHeight: Integer);
*)
procedure _LapeMufasaBitmap_ResizeBilinear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ResizeBilinear(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.GetWidth
~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetWidth: Integer;
*)
procedure _LapeMufasaBitmap_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Width;
end;

(*
TMufasaBitmap.GetHeight
~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetHeight: Integer;
*)
procedure _LapeMufasaBitmap_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Height;
end;

(*
TMufasaBitmap.SetPersistentMemory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetPersistentMemory(Memory: PtrUInt; AWidth, AHeight: Integer);
*)
procedure _LapeMufasaBitmap_SetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.SetPersistentMemory(PPtrUInt(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.ResetPersistentMemory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ResetPersistentMemory;
*)
procedure _LapeMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ResetPersistentMemory();
end;

(*
TMufasaBitmap.SaveToFile
~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.SaveToFile(FileName: String): Boolean;
*)
procedure _LapeMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

(*
TMufasaBitmap.SaveToString
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.SaveToString: String;
*)
procedure _LapeMufasaBitmap_SaveToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.SaveToString();
end;

(*
TMufasaBitmap.LoadFromFile
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.LoadFromFile(FileName: String);
*)
procedure _LapeMufasaBitmap_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

(*
TMufasaBitmap.LoadFromFile
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.LoadFromFile(FileName: String; Area: TBox);
*)
procedure _LapeMufasaBitmap_LoadFromFileEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PString(Params^[1])^, PBox(Params^[2])^);
end;

(*
TMufasaBitmap.DrawATPA
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray);
*)
procedure _LapeMufasaBitmap_DrawATPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

(*
TMufasaBitmap.DrawATPA
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawATPAEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawTPA
~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawTPA(TPA: TPointArray; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawTPA(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawToCanvas
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawToCanvas(x, y: Integer; Canvas: TCanvas);
*)
procedure _LapeMufasaBitmap_DrawToCanvas(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawToCanvas(PInteger(Params^[1])^, PInteger(Params^[2])^, PCanvas(Params^[3])^);
end;

(*
TMufasaBitmap.ReplaceColor
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: Integer);
*)
procedure _LapeMufasaBitmap_ReplaceColor(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ReplaceColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.ReplaceColors
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ReplaceColors(OldColors, NewColors: TIntegerArray);
*)
procedure _LapeMufasaBitmap_ReplaceColors(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ReplaceColors(PIntegerArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TMufasaBitmap.Rotate
~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);
*)
procedure _LapeMufasaBitmap_Rotate1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Rotate(PSingle(Params^[1])^, PBoolean(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

(*
TMufasaBitmap.RotateBilinear
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);
*)
procedure _LapeMufasaBitmap_RotateBilinear1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.RotateBilinear(PSingle(Params^[1])^, PBoolean(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

(*
TMufasaBitmap.Rotate
~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean): TMufasaBitmap;
*)
procedure _LapeMufasaBitmap_Rotate2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Rotate(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TMufasaBitmap.RotateBilinear
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean): TMufasaBitmap;
*)
procedure _LapeMufasaBitmap_RotateBilinear2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.RotateBilinear(PSingle(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TMufasaBitmap.GreyScale
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap);
*)
procedure _LapeMufasaBitmap_GreyScale(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.GreyScale(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.GreyScale
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.GreyScale;
*)
procedure _LapeMufasaBitmap_GreyScaleEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.GreyScale();
end;

(*
TMufasaBitmap.Brightness
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Integer);
*)
procedure _LapeMufasaBitmap_Brightness(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Brightness(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Brightness
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Brightness(br: Integer);
*)
procedure _LapeMufasaBitmap_BrightnessEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Brightness(PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.Invert
~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap);
*)
procedure _LapeMufasaBitmap_Invert(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Invert(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.Invert
~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Invert;
*)
procedure _LapeMufasaBitmap_InvertEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Invert();
end;

(*
TMufasaBitmap.Posterize
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Integer);
*)
procedure _LapeMufasaBitmap_Posterize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Posterize(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Posterize
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Posterize(Po: Integer);
*)
procedure _LapeMufasaBitmap_PosterizeEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Posterize(PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.Convolute
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: TDoubleMatrix);
*)
procedure _LapeMufasaBitmap_Convolute(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Convolute(PMufasaBitmap(Params^[1])^, PDoubleMatrix(Params^[2])^);
end;

(*
TMufasaBitmap.Copy
~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Copy(X1, Y1, X2, Y2: Integer): TMufasaBitmap;
*)
procedure _LapeMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TMufasaBitmap.Copy
~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Copy: TMufasaBitmap;
*)
procedure _LapeMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy();
end;

(*
TMufasaBitmap.ToTBitmap
~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.ToTBitmap: TBitmap;
*)
procedure _LapeMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PMufasaBitmap(Params^[0])^.ToTBitmap();
end;

(*
TMufasaBitmap.Crop
~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Crop(X1, Y1, X2, Y2: Integer);
*)
procedure _LapeMufasaBitmap_Crop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

(*
TMufasaBitmap.GetColors
~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetColors: TIntegerArray;
*)
procedure _LapeMufasaBitmap_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetColors();
end;

(*
TMufasaBitmap.ToMatrix
~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.ToMatrix: TIntegerMatrix;
*)
procedure _LapeMufasaBitmap_ToMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix();
end;

(*
TMufasaBitmap.ToMatrix
~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;
*)
procedure _LapeMufasaBitmap_ToMatrixEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TMufasaBitmap.ToGreyMatrix
~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.ToGreyMatrix: TByteMatrix;
*)
procedure _LapeMufasaBitmap_ToGreyMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PByteMatrix(Result)^ := PMufasaBitmap(Params^[0])^.ToGreyMatrix();
end;

(*
TMufasaBitmap.DrawMatrix
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawMatrix(Matrix: TIntegerMatrix);
*)
procedure _LapeMufasaBitmap_DrawMatrix(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(PIntegerMatrix(Params^[1])^);
end;

(*
TMufasaBitmap.DrawMatrix
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
*)
procedure _LapeMufasaBitmap_DrawMatrixF(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TMufasaBitmap(Params^[0]^).DrawMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawMatrix
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawMatrix(Matrix: TByteMatrix);
*)
procedure _LapeMufasaBitmap_DrawMatrixB(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(PByteMatrix(Params^[1])^);
end;

(*
TMufasaBitmap.ThresholdAdaptive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: TBmpThreshMethod; k: Integer);
*)
procedure _LapeMufasaBitmap_ThresholdAdaptive(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInteger(Params^[5])^);
end;

(*
TMufasaBitmap.ThresholdSauvola
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single);
*)
procedure _LapeMufasaBitmap_ThresholdSauvola(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ThresholdSauvola(PInteger(Params^[1])^, PBoolean(Params^[2])^, PSingle(Params^[3])^);
end;

(*
TMufasaBitmap.Pad
~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Pad(Amount: Integer);
*)
procedure _LapeMufasaBitmap_Pad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.LoadFromTBitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);
*)
procedure _LapeMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.SetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetTransparentColor(Value: Integer);
*)
procedure _LapeMufasaBitmap_TransparentColor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.TransparentColor := PInteger(Params^[1])^;
end;

(*
TMufasaBitmap.GetTransparentColor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetTransparentColor: Integer;
*)
procedure _LapeMufasaBitmap_TransparentColor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColor;
end;

(*
TMufasaBitmap.GetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetTransparentColorActive: Boolean;
*)
procedure _LapeMufasaBitmap_TransparentColorActive_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColorActive;
end;

(*
TMufasaBitmap.SetTransparentColorActive
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetTransparentColorActive(Value: Boolean);
*)
procedure _LapeMufasaBitmap_TransparentColorActive_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.TransparentColorActive := PBoolean(Params^[1])^;
end;

(*
TMufasaBitmap.GetPixel
~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetPixel(X, Y: Integer): Integer;
*)
procedure _LapeMufasaBitmap_GetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

(*
TMufasaBitmap.SetPixel
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetPixel(X, Y: Integer; Color: Integer);
*)
procedure _LapeMufasaBitmap_SetPixel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^] := PInteger(Params^[3])^;
end;

(*
TMufasaBitmap.SetPixels
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetPixels(Points: TPointArray; Colors: TIntegerArray);
*)
procedure _LapeMufasaBitmap_SetPixels(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

(*
TMufasaBitmap.GetPixels
~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetPixels(Points: TPointArray): TIntegerArray;
*)
procedure _LapeMufasaBitmap_GetPixels(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetPixels(PPointArray(Params^[1])^);
end;

(*
TMufasaBitmap.Blur
~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Blur(Block: Integer);
*)
procedure _LapeMufasaBitmap_Blur(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.Blur
~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Blur(TargetBitmap: TMufasaBitmap; Block: Integer);
*)
procedure _LapeMufasaBitmap_BlurEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Blur(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Downsample
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Downsample(Scale: Integer);
*)
procedure _LapeMufasaBitmap_DownSample(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Downsample(PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.Downsample
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Downsample(TargetBitmap: TMufasaBitmap; Scale: Integer);
*)
procedure _LapeMufasaBitmap_DownSampleEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Downsample(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Free
~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Free;
*)
procedure _LapeMufasaBitmap_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Free();
end;

(*
TMufasaBitmap.DrawCross
~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCross(ACenter: TPoint; Radius: Integer; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawCross(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawCrosshairs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCrosshairs(ACenter: TPoint; Size: Integer; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawCrosshairs(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawLine
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawLine(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawLine
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawLineEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawPolygon
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawPolygon(Points: TPointArray; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawPolygon(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawPolygonFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawPolygonFilled(Points: TPointArray; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawPolygonFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawPolygonInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawPolygonInverted(Points: TPointArray; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawPolygonInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawCircle
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCircle(ACenter: TPoint; Radius: Integer; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawCircleFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawCircleFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawCircleInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawCircleInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawBox
~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawBox(B: TBox; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawBox(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawBox(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawBoxFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawBoxFilled(B: TBox; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawBoxFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawBoxInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawBoxInverted(B: TBox; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawBoxInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawQuad
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawQuad(B: TBox; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawQuad(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawQuad(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawQuadFilled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawQuadFilled(B: TBox; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawQuadFilled(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawQuadFilled(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawQuadInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawQuadInverted(B: TBox; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawQuadInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawQuadInverted(PQuad(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.DrawQuadArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: Integer = -1);
*)
procedure _LapeMufasaBitmap_DrawQuadArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawQuadArray(PQuadArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawBoxArray
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: Integer = -1);
*)
procedure _LapeMufasaBitmap_DrawBoxArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawPolygonArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: Integer = -1);
*)
procedure _LapeMufasaBitmap_DrawPolygonArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawCircleArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: Integer = -1);
*)
procedure _LapeMufasaBitmap_DrawCircleArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TMufasaBitmap.DrawCrossArray
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawCrossArray(Points: TPointArray; Radius: Integer; Color: Integer = -1);
*)
procedure _LapeMufasaBitmap_DrawCrossArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.Fill
~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Fill(Color: Integer);
*)
procedure _LapeMufasaBitmap_Fill(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Fill(PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.Clear
~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Clear;
*)
procedure _LapeMufasaBitmap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Clear();
end;

(*
TMufasaBitmap.Clear
~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Clear(Area: TBox);
*)
procedure _LapeMufasaBitmap_ClearEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Clear(PBox(Params^[1])^);
end;

(*
TMufasaBitmap.ClearInverted
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.ClearInverted(Area: TBox);
*)
procedure _LapeMufasaBitmap_ClearInverted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

(*
TMufasaBitmap.DrawBitmap
~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawBitmap(Bitmap: TMufasaBitmap; Position: TPoint);
*)
procedure _LapeMufasaBitmap_DrawBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawBitmap(PMufasaBitmap(Params^[1])^, PPoint(Params^[2])^);
end;

(*
TMufasaBitmap.Blend
~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Blend(Points: TPointArray; Size: Integer);
*)
procedure _LapeMufasaBitmap_Blend(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Blend
~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Blend(TargetBitmap: TMufasaBitmap; Points: TPointArray; Size: Integer);
*)
procedure _LapeMufasaBitmap_BlendEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Blend(PMufasaBitmap(Params^[1])^, PPointArray(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.GetCenter
~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetCenter: TPoint;
*)
procedure _LapeMufasaBitmap_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.Center;
end;

(*
TMufasaBitmap.GetFonts
~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetFonts: TStringArray;
*)
procedure _LapeMufasaBitmap_Fonts_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PMufasaBitmap(Params^[0])^.Fonts;
end;

(*
TMufasaBitmap.GetFontName
~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetFontName: String;
*)
procedure _LapeMufasaBitmap_FontName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.FontName;
end;

(*
TMufasaBitmap.SetFontName
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetFontName(Value: String);
*)
procedure _LapeMufasaBitmap_FontName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.FontName := PString(Params^[1])^;
end;

(*
TMufasaBitmap.GetFontSize
~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetFontSize: Single;
*)
procedure _LapeMufasaBitmap_FontSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PMufasaBitmap(Params^[0])^.FontSize;
end;

(*
TMufasaBitmap.SetFontSize
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetFontSize(Value: Single);
*)
procedure _LapeMufasaBitmap_FontSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

(*
TMufasaBitmap.GetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.GetFontAntialiasing: Boolean;
*)
procedure _LapeMufasaBitmap_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FontAntialiasing;
end;

(*
TMufasaBitmap.SetFontAntialiasing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.SetFontAntialiasing(Value: Boolean);
*)
procedure _LapeMufasaBitmap_FontAntialiasing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

(*
TMufasaBitmap.TextWidth
~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.TextWidth(Text: String): Integer;
*)
procedure _LapeMufasaBitmap_TextWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

(*
TMufasaBitmap.TextHeight
~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.TextHeight(Text: String): Integer;
*)
procedure _LapeMufasaBitmap_TextHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

(*
TMufasaBitmap.TextSize
~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.TextSize(Text: String): TPoint;
*)
procedure _LapeMufasaBitmap_TextSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.TextSize(PString(Params^[1])^);
end;

(*
TMufasaBitmap.DrawText
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawText(Text: String; Position: TPoint; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.DrawText
~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawText(Text: String; Box: TBox; Center: Boolean; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PInteger(Params^[4])^);
end;

(*
TMufasaBitmap.DrawTextLines
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawTextLines(Text: TStringArray; Position: TPoint; Color: Integer);
*)
procedure _LapeMufasaBitmap_DrawTextLines(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TMufasaBitmap.Mirror
~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Mirror(MirrorStyle: TBmpMirrorStyle);
*)
procedure _LapeMufasaBitmap_Mirror(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Mirror(PBmpMirrorStyle(Params^[1])^);
end;

(*
TMufasaBitmap.Mirror
~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.Mirror(TargetBitmap: TMufasaBitmap; MirrorStyle: TBmpMirrorStyle);
*)
procedure _LapeMufasaBitmap_MirrorEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.Mirror(PMufasaBitmap(Params^[1])^, PBmpMirrorStyle(Params^[2])^);
end;

(*
TMufasaBitmap.Equals
~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Equals(Other: TMufasaBitmap): Boolean;

Are the two bitmaps equal?
*)
procedure _LapeMufasaBitmap_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.Equals(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap): Integer;
*)
procedure _LapeMufasaBitmap_PixelDifference(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifference(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.PixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap; Tolerance: Integer): Integer;
*)
procedure _LapeMufasaBitmap_PixelDifferenceTolerance(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifference(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap): TPointArray;
*)
procedure _LapeMufasaBitmap_PixelDifferenceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
 PPointArray(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifferenceTPA(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.PixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap; Tolerance: Integer): TPointArray;
*)
procedure _LapeMufasaBitmap_PixelDifferenceToleranceTPA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifferenceTPA(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.LoadFromString
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.LoadFromString(AWidth, AHeight: Integer; Str: String);
*)
procedure _LapeMufasaBitmap_LoadFromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.LoadFromString(PInteger(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^);
end;

(*
TMufasaBitmap.LoadFromData
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.LoadFromData(AWidth, AHeight: Integer; Memory: PRGB32; CopyData: Boolean = True);
*)
procedure _LapeMufasaBitmap_LoadFromData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.LoadFromData(PInteger(Params^[1])^, PInteger(Params^[2])^, PPointer(Params^[3])^, PBoolean(Params^[4])^);
end;

(*
TMufasaBitmap.LoadFromBitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.LoadFromBitmap(Bitmap: TMufasaBitmap);
*)
procedure _LapeMufasaBitmap_LoadFromBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.LoadFromBitmap(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.Create
~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Create: TMufasaBitmap; static;
*)
procedure _LapeMufasaBitmap_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.Create();
end;

(*
TMufasaBitmap.Create
~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Create(Width, Height: Integer): TMufasaBitmap; static;
*)
procedure _LapeMufasaBitmap_CreateEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
TMufasaBitmap.CreateFromFile
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.CreateFromFile(FileName: String): TMufasaBitmap; static;
*)
procedure _LapeMufasaBitmap_CreateFromFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.CreateFromFile(PString(Params^[0])^);
end;

(*
TMufasaBitmap.CreateFromString
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.CreateFromString(Width, Height: Integer; Str: String): TMufasaBitmap; static;
*)
procedure _LapeMufasaBitmap_CreateFromString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.CreateFromString(PInteger(Params^[0])^, PInteger(Params^[1])^, PString(Params^[2])^);
end;

(*
TMufasaBitmap.Compare
~~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Compare(Other: TMufasaBitmap): Single;
*)
procedure _LapeMufasaBitmap_Compare(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingle(Result)^ := PMufasaBitmap(Params^[0])^.Compare(PMufasaBitmap(Params^[1])^);
end;

(*
TMufasaBitmap.DebugUnfreedBitmaps
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DebugUnfreedBitmaps(Directory: String); static;

Saves unfreed bitmaps on script terminate.

Example::

  TMufasaBitmap.DebugUnfreedBitmaps('some/directory/');
*)
procedure _LapeMufasaBitmap_DebugUnfreedBitmaps(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TMufasaBitmap.DebugUnfreedBitmaps := PString(Params^[0])^;
end;

(*
TMufasaBitmap.FreeOnTerminate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.FreeOnTerminate(Value: Boolean);
*)
procedure _LapeMufasaBitmap_FreeOnTerminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.FreeOnTerminate := PBoolean(Params^[1])^;
end;

(*
TMufasaBitmap.DrawHSLCircle
~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TMufasaBitmap.DrawHSLCircle(ACenter: TPoint; Radius: Integer);
*)
procedure _LapeMufasaBitmap_DrawHSLCircle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Params^[0])^.DrawHSLCircle(PPoint(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TMufasaBitmap.Finder
~~~~~~~~~~~~~~~~~~~~
function TMufasaBitmap.Finder: TSimbaFinder;
*)
// Done in simba.import_finder
//procedure _LapeMufasaBitmap_Finder(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
//begin
//  PSimbaFinder(Result)^ := PMufasaBitmap(Params^[0])^.Finder;
//end;

procedure ImportBitmap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TMufasaBitmap';

    addClass('TMufasaBitmap');

    addGlobalType('array of TMufasaBitmap', 'TMufasaBitmapArray');
    addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
    addGlobalType('(TM_Mean, TM_MinMax)', 'TBmpThreshMethod');

    addClassVar('TMufasaBitmap', 'Data', 'PColorBGRA', @_LapeMufasaBitmap_Data_Read);
    addClassVar('TMufasaBitmap', 'Name', 'String', @_LapeMufasaBitmap_Name_Read, @_LapeMufasaBitmap_Name_Write);
    addClassVar('TMufasaBitmap', 'Width', 'Integer', @_LapeMufasaBitmap_Width_Read);
    addClassVar('TMufasaBitmap', 'Height', 'Integer', @_LapeMufasaBitmap_Height_Read);
    addClassVar('TMufasaBitmap', 'Center', 'TPoint', @_LapeMufasaBitmap_Center_Read);
    addClassVar('TMufasaBitmap', 'TransparentColor', 'Integer', @_LapeMufasaBitmap_TransparentColor_Read, @_LapeMufasaBitmap_TransparentColor_Write);
    addClassVar('TMufasaBitmap', 'TransparentColorActive', 'Boolean', @_LapeMufasaBitmap_TransparentColorActive_Read, @_LapeMufasaBitmap_TransparentColorActive_Write);

    addClassVar('TMufasaBitmap', 'Fonts', 'TStringArray', @_LapeMufasaBitmap_Fonts_Read);
    addClassVar('TMufasaBitmap', 'FontName', 'String', @_LapeMufasaBitmap_FontName_Read, @_LapeMufasaBitmap_FontName_Write);
    addClassVar('TMufasaBitmap', 'FontSize', 'Single', @_LapeMufasaBitmap_FontSize_Read, @_LapeMufasaBitmap_FontSize_Write);
    addClassVar('TMufasaBitmap', 'FontAntialiasing', 'Boolean', @_LapeMufasaBitmap_FontAntialiasing_Read, @_LapeMufasaBitmap_FontAntialiasing_Write);

    addGlobalFunc('function TMufasaBitmap.PointInBitmap(P: TPoint): Boolean; overload', @_LapeMufasaBitmap_PointInBitmap);
    addGlobalFunc('function TMufasaBitmap.PointInBitmap(X, Y: Integer): Boolean; overload', @_LapeMufasaBitmap_PointInBitmapEx);

    addGlobalFunc('function TMufasaBitmap.Create: TMufasaBitmap; static; overload', @_LapeMufasaBitmap_Create);
    addGlobalFunc('function TMufasaBitmap.Create(Width, Height: Integer): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateEx);
    addGlobalFunc('function TMufasaBitmap.CreateFromFile(FileName: String): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromFile);
    addGlobalFunc('function TMufasaBitmap.CreateFromString(Width, Height: Integer; Str: String): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromString);

    addGlobalFunc('function TMufasaBitmap.Equals(Other: TMufasaBitmap): Boolean;', @_LapeMufasaBitmap_Equals);

    addGlobalFunc('procedure TMufasaBitmap.SetPixel(X, Y: Integer; Color: Integer);', @_LapeMufasaBitmap_SetPixel);
    addGlobalFunc('function TMufasaBitmap.GetPixel(X, Y: Integer): Integer;', @_LapeMufasaBitmap_GetPixel);

    addGlobalFunc('procedure TMufasaBitmap.SetPixels(Points: TPointArray; Colors: TIntegerArray);', @_LapeMufasaBitmap_SetPixels);
    addGlobalFunc('function TMufasaBitmap.GetPixels(Points: TPointArray): TIntegerArray;', @_LapeMufasaBitmap_GetPixels);

    addGlobalFunc('function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap): Integer; overload', @_LapeMufasaBitmap_PixelDifference);
    addGlobalFunc('function TMufasaBitmap.PixelDifference(Other: TMufasaBitmap; Tolerance: Integer): Integer; overload', @_LapeMufasaBitmap_PixelDifferenceTolerance);

    addGlobalFunc('function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap): TPointArray; overload', @_LapeMufasaBitmap_PixelDifferenceTPA);
    addGlobalFunc('function TMufasaBitmap.PixelDifferenceTPA(Other: TMufasaBitmap; Tolerance: Integer): TPointArray; overload', @_LapeMufasaBitmap_PixelDifferenceToleranceTPA);

    addGlobalFunc('function TMufasaBitmap.TextWidth(Text: String): Integer;', @_LapeMufasaBitmap_TextWidth);
    addGlobalFunc('function TMufasaBitmap.TextHeight(Text: String): Integer;', @_LapeMufasaBitmap_TextHeight);
    addGlobalFunc('function TMufasaBitmap.TextSize(Text: String): TPoint;', @_LapeMufasaBitmap_TextSize);
    addGlobalFunc('procedure TMufasaBitmap.DrawText(Text: String; Position: TPoint; Color: Integer); overload', @_LapeMufasaBitmap_DrawText);
    addGlobalFunc('procedure TMufasaBitmap.DrawText(Text: String; Box: TBox; Center: Boolean; Color: Integer); overload', @_LapeMufasaBitmap_DrawTextEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawTextLines(Text: TStringArray; Position: TPoint; Color: Integer);', @_LapeMufasaBitmap_DrawTextLines);

    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray); overload', @_LapeMufasaBitmap_DrawATPA);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Color: Integer); overload', @_LapeMufasaBitmap_DrawATPAEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawTPA(TPA: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawTPA);

    addGlobalFunc('procedure TMufasaBitmap.DrawCrosshairs(ACenter: TPoint; Size: Integer; Thickness: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCrosshairs);
    addGlobalFunc('procedure TMufasaBitmap.DrawCross(ACenter: TPoint; Radius: Integer; Thickness: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCross);

    addGlobalFunc('procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Color: Integer); overload', @_LapeMufasaBitmap_DrawLine);
    addGlobalFunc('procedure TMufasaBitmap.DrawLine(Start, Stop: TPoint; Thickness: Integer; Color: Integer); overload', @_LapeMufasaBitmap_DrawLineEx);

    addGlobalFunc('procedure TMufasaBitmap.DrawPolygon(Points: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawPolygon);
    addGlobalFunc('procedure TMufasaBitmap.DrawPolygonFilled(Points: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawPolygonFilled);
    addGlobalFunc('procedure TMufasaBitmap.DrawPolygonInverted(Points: TPointArray; Color: Integer);', @_LapeMufasaBitmap_DrawPolygonInverted);

    addGlobalFunc('procedure TMufasaBitmap.DrawCircle(ACenter: TPoint; Radius: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCircle);
    addGlobalFunc('procedure TMufasaBitmap.DrawCircleFilled(ACenter: TPoint; Radius: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCircleFilled);
    addGlobalFunc('procedure TMufasaBitmap.DrawCircleInverted(ACenter: TPoint; Radius: Integer; Color: Integer);', @_LapeMufasaBitmap_DrawCircleInverted);

    addGlobalFunc('procedure TMufasaBitmap.DrawBox(B: TBox; Color: Integer);', @_LapeMufasaBitmap_DrawBox);
    addGlobalFunc('procedure TMufasaBitmap.DrawBoxFilled(B: TBox; Color: Integer);', @_LapeMufasaBitmap_DrawBoxFilled);
    addGlobalFunc('procedure TMufasaBitmap.DrawBoxInverted(B: TBox; Color: Integer);', @_LapeMufasaBitmap_DrawBoxInverted);

    addGlobalFunc('procedure TMufasaBitmap.DrawQuad(Quad: TQuad; Color: Integer);', @_LapeMufasaBitmap_DrawQuad);
    addGlobalFunc('procedure TMufasaBitmap.DrawQuadFilled(Quad: TQuad; Color: Integer);', @_LapeMufasaBitmap_DrawQuadFilled);
    addGlobalFunc('procedure TMufasaBitmap.DrawQuadInverted(Quad: TQuad; Color: Integer);', @_LapeMufasaBitmap_DrawQuadInverted);

    addGlobalFunc('procedure TMufasaBitmap.DrawQuadArray(Quads: TQuadArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawQuadArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawBoxArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawPolygonArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawCircleArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawCrossArray(Points: TPointArray; Radius: Integer; Thickness: Integer; Color: Integer = -1);', @_LapeMufasaBitmap_DrawCrossArray);

    addGlobalFunc('procedure TMufasaBitmap.Clear; overload', @_LapeMufasaBitmap_Clear);
    addGlobalFunc('procedure TMufasaBitmap.Clear(Area: TBox); overload', @_LapeMufasaBitmap_ClearEx);
    addGlobalFunc('procedure TMufasaBitmap.ClearInverted(Area: TBox);', @_LapeMufasaBitmap_ClearInverted);

    addGlobalFunc('procedure TMufasaBitmap.DrawBitmap(Bitmap: TMufasaBitmap; Position: TPoint);', @_LapeMufasaBitmap_DrawBitmap);

    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TIntegerMatrix); overload', @_LapeMufasaBitmap_DrawMatrix);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload', @_LapeMufasaBitmap_DrawMatrixF);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TByteMatrix); overload', @_LapeMufasaBitmap_DrawMatrixB);

    addGlobalFunc('procedure TMufasaBitmap.SetSize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetSize);
    addGlobalFunc('procedure TMufasaBitmap.Resize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_Resize);
    addGlobalFunc('procedure TMufasaBitmap.ResizeBilinear(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_ResizeBilinear);
    addGlobalFunc('procedure TMufasaBitmap.SetPersistentMemory(Memory: PtrUInt; AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetPersistentMemory);
    addGlobalFunc('procedure TMufasaBitmap.ResetPersistentMemory;', @_LapeMufasaBitmap_ResetPersistentMemory);

    addGlobalFunc('procedure TMufasaBitmap.Fill(Color: Integer);', @_LapeMufasaBitmap_Fill);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: Integer);', @_LapeMufasaBitmap_ReplaceColor);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColors(OldColors, NewColors: TIntegerArray);', @_LapeMufasaBitmap_ReplaceColors);
    addGlobalFunc('procedure TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_Rotate1);
    addGlobalFunc('procedure TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_RotateBilinear1);

    addGlobalFunc('function TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean): TMufasaBitmap; overload', @_LapeMufasaBitmap_Rotate2);
    addGlobalFunc('function TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean): TMufasaBitmap; overload', @_LapeMufasaBitmap_RotateBilinear2);

    addGlobalFunc('procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_GreyScale);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale; overload', @_LapeMufasaBitmap_GreyScaleEx);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Integer); overload', @_LapeMufasaBitmap_Brightness);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(br: Integer); overload', @_LapeMufasaBitmap_BrightnessEx);
    addGlobalFunc('procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap);', @_LapeMufasaBitmap_Invert);
    addGlobalFunc('procedure TMufasaBitmap.Invert; overload', @_LapeMufasaBitmap_InvertEx);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Integer); overload', @_LapeMufasaBitmap_Posterize);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(Po: Integer); overload', @_LapeMufasaBitmap_PosterizeEx);
    addGlobalFunc('procedure TMufasaBitmap.Blur(Block: Integer); overload', @_LapeMufasaBitmap_Blur);
    addGlobalFunc('procedure TMufasaBitmap.Blur(TargetBitmap: TMufasaBitmap; Block: Integer); overload', @_LapeMufasaBitmap_BlurEx);
    addGlobalFunc('procedure TMufasaBitmap.Mirror(MirrorStyle: TBmpMirrorStyle); overload', @_LapeMufasaBitmap_Mirror);
    addGlobalFunc('procedure TMufasaBitmap.Mirror(TargetBitmap: TMufasaBitmap; MirrorStyle: TBmpMirrorStyle); overload', @_LapeMufasaBitmap_MirrorEx);
    addGlobalFunc('procedure TMufasaBitmap.Downsample(Scale: Integer); overload', @_LapeMufasaBitmap_DownSample);
    addGlobalFunc('procedure TMufasaBitmap.Downsample(TargetBitmap: TMufasaBitmap; Scale: Integer); overload', @_LapeMufasaBitmap_DownSampleEx);
    addGlobalFunc('procedure TMufasaBitmap.Blend(Points: TPointArray; Size: Integer); overload', @_LapeMufasaBitmap_Blend);
    addGlobalFunc('procedure TMufasaBitmap.Blend(TargetBitmap: TMufasaBitmap; Points: TPointArray; Size: Integer); overload', @_LapeMufasaBitmap_BlendEx);
    addGlobalFunc('procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: TDoubleMatrix);', @_LapeMufasaBitmap_Convolute);
    addGlobalFunc('function TMufasaBitmap.Copy(X1, Y1, X2, Y2: Integer): TMufasaBitmap; overload', @_LapeMufasaBitmap_Copy);
    addGlobalFunc('function TMufasaBitmap.Copy: TMufasaBitmap; overload', @_LapeMufasaBitmap_CopyEx);
    addGlobalFunc('procedure TMufasaBitmap.Crop(X1, Y1, X2, Y2: Integer);', @_LapeMufasaBitmap_Crop);
    addGlobalFunc('function TMufasaBitmap.GetColors: TIntegerArray;', @_LapeMufasaBitmap_GetColors);
    addGlobalFunc('function TMufasaBitmap.ToMatrix: TIntegerMatrix; overload', @_LapeMufasaBitmap_ToMatrix);
    addGlobalFunc('function TMufasaBitmap.ToMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix; overload', @_LapeMufasaBitmap_ToMatrixEx);
    addGlobalFunc('function TMufasaBitmap.ToGreyMatrix: TByteMatrix;', @_LapeMufasaBitmap_ToGreyMatrix);
    addGlobalFunc('procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; AInvert: Boolean; Method: TBmpThreshMethod; k: Integer);', @_LapeMufasaBitmap_ThresholdAdaptive);
    addGlobalFunc('procedure TMufasaBitmap.ThresholdSauvola(Radius: Integer; AInvert: Boolean; k: Single);', @_LapeMufasaBitmap_ThresholdSauvola);
    addGlobalFunc('procedure TMufasaBitmap.Pad(Amount: Integer)', @_LapeMufasaBitmap_Pad);

    addGlobalFunc('procedure TMufasaBitmap.DrawHSLCircle(ACenter: TPoint; Radius: Integer)', @_LapeMufasaBitmap_DrawHSLCircle);

    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(FileName: String); overload', @_LapeMufasaBitmap_LoadFromFile);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(FileName: String; Area: TBox); overload', @_LapeMufasaBitmap_LoadFromFileEx);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromString(AWidth, AHeight: Integer; Str: String)', @_LapeMufasaBitmap_LoadFromString);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromData(AWidth, AHeight: Integer; AData: PColorBGRA; CopyData: Boolean = True)', @_LapeMufasaBitmap_LoadFromData);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromBitmap(Bitmap: TMufasaBitmap);', @_LapeMufasaBitmap_LoadFromBitmap);

    addGlobalFunc('function TMufasaBitmap.SaveToFile(FileName: String): Boolean;', @_LapeMufasaBitmap_SaveToFile);
    addGlobalFunc('function TMufasaBitmap.SaveToString: String;', @_LapeMufasaBitmap_SaveToString);

    addGlobalFunc('function TMufasaBitmap.ToTBitmap: TBitmap;', @_LapeMufasaBitmap_ToTBitmap);
    addGlobalFunc('procedure TMufasaBitmap.DrawToCanvas(x, y: Integer; Canvas: TCanvas);', @_LapeMufasaBitmap_DrawToCanvas);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);', @_LapeMufasaBitmap_LoadFromTBitmap);
    addGlobalFunc('function TMufasaBitmap.Compare(Other: TMufasaBitmap): Single;', @_LapeMufasaBitmap_Compare);

    addGlobalFunc('procedure TMufasaBitmap.DebugUnfreedBitmaps(Directory: String); static;', @_LapeMufasaBitmap_DebugUnfreedBitmaps);
    addGlobalFunc('procedure TMufasaBitmap.FreeOnTerminate(Value: Boolean);', @_LapeMufasaBitmap_FreeOnTerminate);

    // Will be overriden later, in finder import
    addGlobalFunc(
      'function TMufasaBitmap.CreateFromFinder(Area: TBox = [-1,-1,-1,-1]): TMufasaBitmap; static;', [
      'begin',
      'end;'
    ]);

    addDelayedCode([
      'procedure Show(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True);',
      'begin',
      'end;',
      '',
      'procedure TMufasaBitmap.Show(EnsureVisible: Boolean = True);',
      'begin',
      '  Show(Self, EnsureVisible);',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportBitmap);

end.

