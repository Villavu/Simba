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
  PMufasaBitmap(Params^[0])^.DrawMatrix(P2DIntegerArray(Params^[1])^);
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
