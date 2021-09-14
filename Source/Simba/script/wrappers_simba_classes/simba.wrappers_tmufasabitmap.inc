procedure _LapeMufasaBitmap_Data_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PMufasaBitmap(Params^[0])^.Data;
end;

procedure _LapeMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.Name;
end;

procedure _LapeMufasaBitmap_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Name := PlpString(Params^[1])^;
end;

procedure _LapeMufasaBitmap_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMufasaBitmap(Params^[0])^.Index;
end;

procedure _LapeMufasaBitmap_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Index := Pinteger(Params^[1])^;
end;

procedure _LapeMufasaBitmap_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_StretchResize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.StretchResize(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
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
  PMufasaBitmap(Params^[0])^.SetPersistentMemory(PPtrUInt(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ResetPersistentMemory();
end;

procedure _LapeMufasaBitmap_PointInBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_ValidatePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ValidatePoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Rectangle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeMufasaBitmap_FloodFill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FloodFill(PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FastSetPixel(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastSetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FastSetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawATPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawToCanvas(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawToCanvas(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PCanvas(Params^[3])^);
end;

procedure _LapeMufasaBitmap_LineTo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(result)^ := PMufasaBitmap(Params^[0])^.FindColors(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeMufasaBitmap_FastGetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_FastGetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixels(PPointArray(Params^[1])^);
end;

procedure _LapeMufasaBitmap_GetAreaColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetAreaColors(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_GetHSLValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DHSLArray(Result)^ := PMufasaBitmap(Params^[0])^.GetHSLValues(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_FastDrawClear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastDrawClear(PColor(Params^[1])^);
end;

procedure _LapeMufasaBitmap_FastDrawTransparent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastDrawTransparent(PInteger(Params^[1])^, PInteger(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

procedure _LapeMufasaBitmap_FastReplaceColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeMufasaBitmap_CopyClientToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, Pboolean(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure _LapeMufasaBitmap_CopyClientToBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, Pboolean(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure _LapeMufasaBitmap_RotateBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.RotateBitmap(PExtended(Params^[1])^, PMufasaBitmap(Params^[2])^);
end;

procedure _LapeMufasaBitmap_RotateBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.RotateBitmapEx(PSingle(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^, PMufasaBitmap(Params^[4])^);
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
  PMufasaBitmap(Params^[0])^.Brightness(PMufasaBitmap(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_BrightnessEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Brightness(Pinteger(Params^[1])^);
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
  PMufasaBitmap(Params^[0])^.Posterize(PMufasaBitmap(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMufasaBitmap_PosterizeEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Posterize(Pinteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Convolute(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Convolute(PMufasaBitmap(Params^[1])^, P2DExtendedArray(Params^[2])^);
end;

procedure _LapeMufasaBitmap_CompareAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Extended(Result^) := TMufasaBitmap(Params^[0]^).CompareAt(TMufasaBitmap(Params^[1]^), TPoint(Params^[2]^), Int32(Params^[3]^));
end;

procedure _LapeMufasaBitmap_Downsample(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMufasaBitmap(Params^[0]^).Downsample(Int32(Params^[1]^), TMufasaBitmap(Params^[2]^));
end;

procedure _LapeMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy();
end;

procedure _LapeMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PMufasaBitmap(Params^[0])^.ToTBitmap();
end;

procedure _LapeMufasaBitmap_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.ToString();
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
  P2DIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix();
end;

procedure _LapeMufasaBitmap_ToGreyMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DByteArray(Result)^ := PMufasaBitmap(Params^[0])^.ToGreyMatrix();
end;

procedure _LapeMufasaBitmap_DrawMatrix(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(P2DIntegerArray(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawMatrixF(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMufasaBitmap(Params^[0]^).DrawMatrix(TSingleMatrix(Params^[1]^), Int32(Params^[2]^));
end;

procedure _LapeMufasaBitmap_ResizeEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpResizeMethod = ^TBmpResizeMethod;
begin
  PMufasaBitmap(Params^[0])^.ResizeEx(PBmpResizeMethod(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMufasaBitmap_ThresholdAdaptive(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpThreshMethod = ^TBmpThreshMethod;
begin
  PMufasaBitmap(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeMufasaBitmap_Pad(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_RowPtrs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32Array(Result)^ := PMufasaBitmap(Params^[0])^.RowPtrs();
end;

procedure _LapeMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

procedure _LapeMufasaBitmap_LoadFromRawImage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromRawImage(PRawImage(Params^[1])^);
end;

procedure _LapeMufasaBitmap_CreateTMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMask(Result)^ := PMufasaBitmap(Params^[0])^.CreateTMask();
end;

procedure _LapeMufasaBitmap_SetTransparentColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetTransparentColor(PColor(Params^[1])^);
end;

procedure _LapeMufasaBitmap_GetTransparentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.GetTransparentColor();
end;

procedure _LapeMufasaBitmap_TransparentColorSet_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColorSet;
end;

procedure _LapeMufasaBitmap_SetAlphaValue(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetAlphaValue(Pbyte(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Blur(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

procedure _LapeMufasaBitmap_BlurEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeMufasaBitmap_RectangleEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DownSampleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Downsample(PInt32(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMufasaBitmap_List_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMBitmaps(Result)^ := PMufasaBitmap(Params^[0])^.List;
end;

procedure _LapeMufasaBitmap_List_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.List := PMBitmaps(Params^[1])^;
end;

procedure _LapeMufasaBitmap_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^ := TMufasaBitmap.Create();
  
  if (PMBitmaps(Params^[1])^ <> nil) then
    PMBitmaps(Params^[1])^.addBMP(PMufasaBitmap(Params^[0])^);
end;

procedure _LapeMufasaBitmap_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Free();
end;

procedure _LapeMufasaBitmap_DrawPolyFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawPolyFilled(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawCircleFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCircleFilled(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PColor(Params^[5])^);
end;

procedure _LapeMufasaBitmap_Blend(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blend(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeMufasaBitmap_DrawBoxFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
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
  PInt32(Result)^ := PMufasaBitmap(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_TextHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := PMufasaBitmap(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_TextSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.TextSize(PString(Params^[1])^);
end;

procedure _LapeMufasaBitmap_DrawText(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeMufasaBitmap_DrawTextEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

