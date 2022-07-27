unit simba.import_class_bitmap;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, lptypes, lpeval,
  simba.script_compiler, simba.mufasatypes, simba.bitmap, simba.bitmap_helpers;

type
  PObject = ^TObject;
  PCanvas = ^TCanvas;
  PBitmap = ^TBitmap;

procedure _LapeMufasaBitmap_PointInBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(PPoint(Params^[1])^);
end;

procedure _LapeMufasaBitmap_PointInBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Data_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PMufasaBitmap(Params^[0])^.Data;
end;

procedure _LapeMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.Name;
end;

procedure _LapeMufasaBitmap_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Name := PString(Params^[1])^;
end;

procedure _LapeMufasaBitmap_Client_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Result)^ := PMufasaBitmap(Params^[0])^.Client;
end;

procedure _LapeMufasaBitmap_Client_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Client := PObject(Params^[1])^;
end;

procedure _LapeMufasaBitmap_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Resize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Resize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_ResizeBilinear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ResizeBilinear(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Width;
end;

procedure _LapeMufasaBitmap_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Height;
end;

procedure _LapeMufasaBitmap_SetPersistentMemory(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetPersistentMemory(PPtrUInt(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ResetPersistentMemory();
end;

procedure _LapeMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_SaveToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.SaveToString();
end;

procedure _LapeMufasaBitmap_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_LoadFromFileEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PString(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawATPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawToCanvas(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawToCanvas(PInteger(Params^[1])^, PInteger(Params^[2])^, PCanvas(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FindColors(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_FindColorsTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FindColorsTolerance(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FindBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_FindBitmaps(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FindBitmaps(PMufasaBitmap(Params^[1])^, PPointArray(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_ReplaceColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ReplaceColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_ReplaceColors(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ReplaceColors(PIntegerArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Rotate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Rotate(PExtended(Params^[1])^, PBoolean(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

procedure _LapeMufasaBitmap_RotateBilinear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.RotateBilinear(PSingle(Params^[1])^, PBoolean(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

procedure _LapeMufasaBitmap_Desaturate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Desaturate(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DesaturateEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Desaturate();
end;

procedure _LapeMufasaBitmap_GreyScale(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.GreyScale(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_GreyScaleEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.GreyScale();
end;

procedure _LapeMufasaBitmap_Brightness(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Brightness(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_BrightnessEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Brightness(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Contrast(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Contrast(PMufasaBitmap(Params^[1])^, PExtended(Params^[2])^);
end;

procedure _LapeMufasaBitmap_ContrastEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Contrast(PExtended(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Invert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Invert(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_InvertEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Invert();
end;

procedure _LapeMufasaBitmap_Posterize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Posterize(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_PosterizeEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Posterize(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Convolute(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Convolute(PMufasaBitmap(Params^[1])^, P2DExtendedArray(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy();
end;

procedure _LapeMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PMufasaBitmap(Params^[0])^.ToTBitmap();
end;

procedure _LapeMufasaBitmap_Crop(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

procedure _LapeMufasaBitmap_GetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetColors();
end;

procedure _LapeMufasaBitmap_ToMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix();
end;

procedure _LapeMufasaBitmap_ToMatrixEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_ToGreyMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByteMatrix(Result)^ := PMufasaBitmap(Params^[0])^.ToGreyMatrix();
end;

procedure _LapeMufasaBitmap_DrawMatrix(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(PIntegerMatrix(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawMatrixF(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMufasaBitmap(Params^[0]^).DrawMatrix(PSingleMatrix(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawMatrixB(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(PByteMatrix(Params^[1])^);
end;

procedure _LapeMufasaBitmap_ThresholdAdaptive(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeMufasaBitmap_ThresholdSauvola(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ThresholdSauvola(PInteger(Params^[1])^, PBoolean(Params^[2])^, PSingle(Params^[3])^);
end;

procedure _LapeMufasaBitmap_Pad(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_SetTransparentColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.TransparentColor := PInteger(Params^[1])^;
end;

procedure _LapeMufasaBitmap_GetTransparentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColor;
end;

procedure _LapeMufasaBitmap_GetTransparentColorActive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColorActive;
end;

procedure _LapeMufasaBitmap_SetTransparentColorActive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.TransparentColorActive := PBoolean(Params^[1])^;
end;

procedure _LapeMufasaBitmap_GetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure _LapeMufasaBitmap_SetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^[PInteger(Params^[1])^, PInteger(Params^[2])^] := PInteger(Params^[3])^;
end;

procedure _LapeMufasaBitmap_SetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeMufasaBitmap_GetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetPixels(PPointArray(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Blur(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_BlurEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blur(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DownSample(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Downsample(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DownSampleEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Downsample(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^ := TMufasaBitmap.Create();
end;

procedure _LapeMufasaBitmap_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Free();
end;

procedure _LapeMufasaBitmap_DrawCross(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCross(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_DrawCrosshairs(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCrosshairs(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_DrawLine(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawLineEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_DrawPolygon(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawPolygon(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawPolygonFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawPolygonFilled(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawPolygonInverted(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawPolygonInverted(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawCircle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawCircleFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCircleFilled(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawCircleInverted(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCircleInverted(PPoint(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBox(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawBoxFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawBoxInverted(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBoxInverted(PBox(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawBoxArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBoxArray(PBoxArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawPolygonArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawPolygonArray(P2DPointArray(Params^[1])^, PBoolean(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawCircleArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCircleArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PBoolean(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_DrawCrossArray(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCrossArray(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_Fill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Fill(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Clear();
end;

procedure _LapeMufasaBitmap_ClearEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Clear(PBox(Params^[1])^);
end;

procedure _LapeMufasaBitmap_ClearInverted(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ClearInverted(PBox(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBitmap(PMufasaBitmap(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeMufasaBitmap_AverageBrightness(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.AverageBrightness();
end;

procedure _LapeMufasaBitmap_PeakBrightness(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.PeakBrightness();
end;

procedure _LapeMufasaBitmap_FindColorsToleranceCTS1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FindColorsTolerance(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FindColorsToleranceCTS2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FindColorsTolerance(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeMufasaBitmap_Blend(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blend(PPointArray(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_BlendEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blend(PMufasaBitmap(Params^[1])^, PPointArray(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_Center_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.Center;
end;

procedure _LapeMufasaBitmap_Fonts_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := PMufasaBitmap(Params^[0])^.Fonts;
end;

procedure _LapeMufasaBitmap_FontName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.FontName;
end;

procedure _LapeMufasaBitmap_FontName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FontName := PString(Params^[1])^;
end;

procedure _LapeMufasaBitmap_FontSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PMufasaBitmap(Params^[0])^.FontSize;
end;

procedure _LapeMufasaBitmap_FontSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

procedure _LapeMufasaBitmap_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FontAntialiasing;
end;

procedure _LapeMufasaBitmap_FontAntialiasing_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

procedure _LapeMufasaBitmap_TextWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_TextHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_TextSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.TextSize(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawText(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawTextEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_DrawTextLines(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawTextLines(PStringArray(Params^[1])^, PPoint(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_Mirror(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Mirror(PBmpMirrorStyle(Params^[1])^);
end;

procedure _LapeMufasaBitmap_MirrorEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Mirror(PMufasaBitmap(Params^[1])^, PBmpMirrorStyle(Params^[2])^);
end;

procedure _LapeMufasaBitmap_Equals(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.Equals(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_PixelDifference(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifference(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_PixelDifferenceTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifference(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_PixelDifferenceTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifferenceTPA(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_PixelDifferenceToleranceTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMufasaBitmap(Params^[0])^.PixelDifferenceTPA(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_LoadFromString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromString(PInteger(Params^[1])^, PInteger(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeMufasaBitmap_LoadFromMemory(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromMemory(PInteger(Params^[1])^, PInteger(Params^[2])^, PPRGB32(Params^[3])^);
end;

procedure _LapeMufasaBitmap_LoadFromBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromBitmap(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawClient(PBox(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawClientEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawClient(PPoint(Params^[1])^);
end;

procedure _LapeMufasaBitmap_LoadFromClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromClient();
end;

procedure _LapeMufasaBitmap_LoadFromClientEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromClient(PBox(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.Create();
end;

procedure _LapeMufasaBitmap_CreateEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_CreateFromClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.CreateFromClient(PObject(Params^[0])^);
end;

procedure _LapeMufasaBitmap_CreateFromClientEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.CreateFromClient(PObject(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapeMufasaBitmap_CreateFromFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.CreateFromFile(PString(Params^[0])^);
end;

procedure _LapeMufasaBitmap_CreateFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := TMufasaBitmap.CreateFromString(PInteger(Params^[0])^, PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMufasaBitmap_PixelEdgesTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PMufasaBitmap(Params^[0])^.PixelEdgesTPA(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Compare(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PMufasaBitmap(Params^[0])^.Compare(PMufasaBitmap(Params^[1])^);
end;

procedure ImportBitmap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Bitmap');

    addClass('TMufasaBitmap');

    addGlobalType('array of TMufasaBitmap', 'TMufasaBitmapArray');
    addGlobalType('packed record B, G, R, A: Byte; end', 'TRGB32');
    addGlobalType('^TRGB32', 'PRGB32');
    addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
    addGlobalType('(TM_Mean, TM_MinMax)', 'TBmpThreshMethod');

    addClassVar('TMufasaBitmap', 'Data', 'PRGB32', @_LapeMufasaBitmap_Data_Read);
    addClassVar('TMufasaBitmap', 'Name', 'String', @_LapeMufasaBitmap_Name_Read, @_LapeMufasaBitmap_Name_Write);
    addClassVar('TMufasaBitmap', 'Width', 'Integer', @_LapeMufasaBitmap_Width_Read);
    addClassVar('TMufasaBitmap', 'Height', 'Integer', @_LapeMufasaBitmap_Height_Read);
    addClassVar('TMufasaBitmap', 'Center', 'TPoint', @_LapeMufasaBitmap_Center_Read);
    addClassVar('TMufasaBitmap', 'TransparentColor', 'Integer', @_LapeMufasaBitmap_GetTransparentColor, @_LapeMufasaBitmap_SetTransparentColor);
    addClassVar('TMufasaBitmap', 'TransparentColorActive', 'Boolean', @_LapeMufasaBitmap_GetTransparentColorActive, @_LapeMufasaBitmap_SetTransparentColorActive);
    addClassVar('TMufasaBitmap', 'Client', 'TObject', @_LapeMufasaBitmap_Client_Read, @_LapeMufasaBitmap_Client_Write);

    addClassVar('TMufasaBitmap', 'Fonts', 'TStringArray', @_LapeMufasaBitmap_Fonts_Read);
    addClassVar('TMufasaBitmap', 'FontName', 'String', @_LapeMufasaBitmap_FontName_Read, @_LapeMufasaBitmap_FontName_Write);
    addClassVar('TMufasaBitmap', 'FontSize', 'Single', @_LapeMufasaBitmap_FontSize_Read, @_LapeMufasaBitmap_FontSize_Write);
    addClassVar('TMufasaBitmap', 'FontAntialiasing', 'Boolean', @_LapeMufasaBitmap_FontAntialiasing_Read, @_LapeMufasaBitmap_FontAntialiasing_Write);

    addGlobalFunc('function TMufasaBitmap.PointInBitmap(P: TPoint): Boolean; overload', @_LapeMufasaBitmap_PointInBitmap);
    addGlobalFunc('function TMufasaBitmap.PointInBitmap(X, Y: Integer): Boolean; overload', @_LapeMufasaBitmap_PointInBitmapEx);

    addGlobalFunc('function TMufasaBitmap.Create: TMufasaBitmap; static; overload', @_LapeMufasaBitmap_Create);
    addGlobalFunc('function TMufasaBitmap.Create(Width, Height: Integer): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateEx);

    //addGlobalFunc('function TMufasaBitmap.CreateFromClient(Client: TObject): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromClient);
    //addGlobalFunc('function TMufasaBitmap.CreateFromClient(Client: TObject; Area: TBox): TMufasaBitmap; static; overload', @_LapeMufasaBitmap_CreateFromClientEx);

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

    addGlobalFunc('function TMufasaBitmap.PixelEdgesTPA(MinDiff: Integer): TPointArray;', @_LapeMufasaBitmap_PixelEdgesTPA);

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
    addGlobalFunc('procedure TMufasaBitmap.DrawBoxArray(Boxes: TBoxArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawBoxArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawPolygonArray(Polygons: T2DPointArray; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawPolygonArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawCircleArray(Points: TPointArray; Radius: Integer; Filled: Boolean; Color: Integer = -1);', @_LapeMufasaBitmap_DrawCircleArray);
    addGlobalFunc('procedure TMufasaBitmap.DrawCrossArray(Points: TPointArray; Radius: Integer; Thickness: Integer; Color: Integer = -1);', @_LapeMufasaBitmap_DrawCrossArray);

    addGlobalFunc('function TMufasaBitmap.FindColors(out Points: TPointArray; Color: Integer): Boolean;', @_LapeMufasaBitmap_FindColors);
    addGlobalFunc('function TMufasaBitmap.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean; overload', @_LapeMufasaBitmap_FindColorsToleranceCTS1);
    addGlobalFunc('function TMufasaBitmap.FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; HueMod, SatMod: Extended): Boolean; overload', @_LapeMufasaBitmap_FindColorsToleranceCTS2);

    addGlobalFunc('function TMufasaBitmap.FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean;', @_LapeMufasaBitmap_FindBitmap);
    addGlobalFunc('function TMufasaBitmap.FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean;', @_LapeMufasaBitmap_FindBitmaps);

    addGlobalFunc('procedure TMufasaBitmap.Clear; overload', @_LapeMufasaBitmap_Clear);
    addGlobalFunc('procedure TMufasaBitmap.Clear(Area: TBox); overload', @_LapeMufasaBitmap_ClearEx);
    addGlobalFunc('procedure TMufasaBitmap.ClearInverted(Area: TBox);', @_LapeMufasaBitmap_ClearInverted);

    addGlobalFunc('procedure TMufasaBitmap.DrawBitmap(Bitmap: TMufasaBitmap; Position: TPoint);', @_LapeMufasaBitmap_DrawBitmap);
    addGlobalFunc('procedure TMufasaBitmap.DrawClient(Area: TBox; Position: TPoint); overload', @_LapeMufasaBitmap_DrawClient);
    addGlobalFunc('procedure TMufasaBitmap.DrawClient(Position: TPoint); overload', @_LapeMufasaBitmap_DrawClientEx);

    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TIntegerMatrix); overload', @_LapeMufasaBitmap_DrawMatrix);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload', @_LapeMufasaBitmap_DrawMatrixF);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(Matrix: TByteMatrix); overload', @_LapeMufasaBitmap_DrawMatrixB);

    addGlobalFunc('function TMufasaBitmap.AverageBrightness: Integer;', @_LapeMufasaBitmap_AverageBrightness);
    addGlobalFunc('function TMufasaBitmap.PeakBrightness: Integer;', @_LapeMufasaBitmap_PeakBrightness);

    addGlobalFunc('procedure TMufasaBitmap.SetSize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetSize);
    addGlobalFunc('procedure TMufasaBitmap.Resize(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_Resize);
    addGlobalFunc('procedure TMufasaBitmap.ResizeBilinear(AWidth, AHeight: Integer);', @_LapeMufasaBitmap_ResizeBilinear);
    addGlobalFunc('procedure TMufasaBitmap.SetPersistentMemory(Memory: PtrUInt; AWidth, AHeight: Integer);', @_LapeMufasaBitmap_SetPersistentMemory);
    addGlobalFunc('procedure TMufasaBitmap.ResetPersistentMemory;', @_LapeMufasaBitmap_ResetPersistentMemory);

    addGlobalFunc('procedure TMufasaBitmap.Fill(Color: Integer);', @_LapeMufasaBitmap_Fill);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: Integer);', @_LapeMufasaBitmap_ReplaceColor);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColors(OldColors, NewColors: TIntegerArray);', @_LapeMufasaBitmap_ReplaceColors);
    addGlobalFunc('procedure TMufasaBitmap.Rotate(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);', @_LapeMufasaBitmap_Rotate);
    addGlobalFunc('procedure TMufasaBitmap.RotateBilinear(Radians: Single; Expand: Boolean; TargetBitmap: TMufasaBitmap);', @_LapeMufasaBitmap_RotateBilinear);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_Desaturate);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate; overload', @_LapeMufasaBitmap_DesaturateEx);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap); overload', @_LapeMufasaBitmap_GreyScale);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale; overload', @_LapeMufasaBitmap_GreyScaleEx);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Integer); overload', @_LapeMufasaBitmap_Brightness);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(br: Integer); overload', @_LapeMufasaBitmap_BrightnessEx);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended); overload', @_LapeMufasaBitmap_Contrast);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(co: Extended); overload', @_LapeMufasaBitmap_ContrastEx);
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
    addGlobalFunc('procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: T2DExtendedArray);', @_LapeMufasaBitmap_Convolute);
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

    addGlobalFunc('procedure TMufasaBitmap.LoadFromClient; overload', @_LapeMufasaBitmap_LoadFromClient);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromClient(Area: TBox); overload', @_LapeMufasaBitmap_LoadFromClientEx);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(FileName: String); overload', @_LapeMufasaBitmap_LoadFromFile);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(FileName: String; Area: TBox); overload', @_LapeMufasaBitmap_LoadFromFileEx);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromString(AWidth, AHeight: Integer; Str: String)', @_LapeMufasaBitmap_LoadFromString);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromMemory(AWidth, AHeight: Integer; Memory: PRGB32)', @_LapeMufasaBitmap_LoadFromMemory);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromBitmap(Bitmap: TMufasaBitmap);', @_LapeMufasaBitmap_LoadFromBitmap);

    addGlobalFunc('function TMufasaBitmap.SaveToFile(FileName: String): Boolean;', @_LapeMufasaBitmap_SaveToFile);
    addGlobalFunc('function TMufasaBitmap.SaveToString: String;', @_LapeMufasaBitmap_SaveToString);

    addGlobalFunc('function TMufasaBitmap.ToTBitmap: TBitmap;', @_LapeMufasaBitmap_ToTBitmap);
    addGlobalFunc('procedure TMufasaBitmap.DrawToCanvas(x, y: Integer; Canvas: TCanvas);', @_LapeMufasaBitmap_DrawToCanvas);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap);', @_LapeMufasaBitmap_LoadFromTBitmap);
    addGlobalFunc('function TMufasaBitmap.Compare(Other: TMufasaBitmap): Single;', @_LapeMufasaBitmap_Compare);

    //addGlobalFunc('procedure TMufasaBitmap.Free;', @_LapeMufasaBitmap_Free);

    addDelayedCode([
      'procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True);',
      'begin',
      'end;'
    ]);

    addDelayedCode([
      'procedure TMufasaBitmap.Init; deprecated ' + #39 + 'Use TMufasaBitmap.Create' + #39 + ';',
      'begin',
      '  Self := TMufasaBitmap.Create();',
      '  Self.SetClient(Client);',
      'end;',
      '',
      'function TMufasaBitmap.Create: TMufasaBitmap; static; override;',
      'begin',
      '  Result := inherited();',
      '  Result.SetClient(Client);',
      'end;',
      '',
      'function TMufasaBitmap.Create(Width, Height: Integer): TMufasaBitmap; static; override;',
      'begin',
      '  Result := inherited();',
      '  Result.SetClient(Client);',
      'end;',
      '',
      'function TMufasaBitmap.CreateFromClient: TMufasaBitmap; static; overload;',
      'begin',
      '  Result := TMufasaBitmap.Create();',
      '  Result.LoadFromClient();',
      'end;',
      '',
      'function TMufasaBitmap.CreateFromClient(Area: TBox): TMufasaBitmap; static; overload;',
      'begin',
      '  Result := TMufasaBitmap.Create();',
      '  Result.LoadFromClient(Area);',
      'end;',
      '',
      'procedure TMufasaBitmap.Show(EnsureVisible: Boolean = True);',
      'begin',
      '  ShowBitmap(Self, EnsureVisible);',
      'end;'
    ]);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportBitmap);

end.

