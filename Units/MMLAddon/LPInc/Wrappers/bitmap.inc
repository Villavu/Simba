procedure Lape_CreateBitmapString(const Params: PParamArray; const Result: Pointer);
begin
  Pstring(Result)^ := ps_CreateBitmapString(Pinteger(Params^[0])^);
end;

procedure Lape_GetMufasaBitmap(const Params: PParamArray; const Result: Pointer);
begin
  PMufasaBitmap(Result)^ := ps_GetMufasaBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_CreateBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_CreateBitmap(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_FreeBitmap(const Params: PParamArray);
begin
  ps_FreeBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_SaveBitmap(const Params: PParamArray);
begin
  ps_SaveBitmap(Pinteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_BitmapFromString(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_BitmapFromString(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_LoadBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_LoadBitmap(PString(Params^[0])^);
end;

procedure Lape_SetBitmapSize(const Params: PParamArray);
begin
  ps_SetBitmapSize(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_StretchBitmapResize(const Params: PParamArray);
begin
  ps_StretchBitmapResize(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_GetBitmapSize(const Params: PParamArray);
begin
  ps_GetBitmapSize(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_SetBitmapName(const Params: PParamArray);
begin
  ps_SetBitmapName(Pinteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_CreateMirroredBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_CreateMirroredBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_CreateMirroredBitmapEx(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_CreateMirroredBitmapEx(Pinteger(Params^[0])^, PBmpMirrorStyle(Params^[1])^);
end;

procedure Lape_FastGetPixel(const Params: PParamArray; const Result: Pointer);
begin
  PLongWord(Result)^ := ps_FastGetPixel(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_FastGetPixels(const Params: PParamArray; const Result: Pointer);
begin
  PIntegerArray(Result)^ := ps_FastGetPixels(Pinteger(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_GetBitmapAreaColors(const Params: PParamArray; const Result: Pointer);
begin
  P2DIntArray(Result)^ := ps_GetBitmapAreaColors(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure Lape_FastSetPixel(const Params: PParamArray);
begin
  ps_FastSetPixel(Pinteger(Params^[0])^, Pinteger(Params^[1])^, Pinteger(Params^[2])^, PColor(Params^[3])^);
end;

procedure Lape_FastSetPixels(const Params: PParamArray);
begin
  ps_FastSetPixels(Pinteger(Params^[0])^, PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure Lape_DrawTPABitmap(const Params: PParamArray);
begin
  ps_DrawTPABitmap(Pinteger(Params^[0])^, PPointArray(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure Lape_DrawATPABitmap(const Params: PParamArray);
begin
  ps_DrawATPABitmap(Pinteger(Params^[0])^, P2DPointArray(Params^[1])^);
end;

procedure Lape_DrawATPABitmapEx(const Params: PParamArray);
begin
  ps_DrawATPABitmapEx(Pinteger(Params^[0])^, P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure Lape_FastDrawClear(const Params: PParamArray);
begin
  ps_FastDrawClear(Pinteger(Params^[0])^, PColor(Params^[1])^);
end;

procedure Lape_DrawBitmap(const Params: PParamArray);
begin
  ps_DrawBitmap(PInteger(Params^[0])^, PCanvas(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_FastDrawTransparent(const Params: PParamArray);
begin
  ps_FastDrawTransparent(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_SetTransparentColor(const Params: PParamArray);
begin
  ps_SetTransparentColor(Pinteger(Params^[0])^, PColor(Params^[1])^);
end;

procedure Lape_GetTransparentColor(const Params: PParamArray; const Result: Pointer);
begin
  PColor(Result)^ := ps_GetTransparentColor(Pinteger(Params^[0])^);
end;

procedure Lape_FastReplaceColor(const Params: PParamArray);
begin
  ps_FastReplaceColor(PInteger(Params^[0])^, PColor(Params^[1])^, PColor(Params^[2])^);
end;

procedure Lape_CopyClientToBitmap(const Params: PParamArray);
begin
  ps_CopyClientToBitmap(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure Lape_BitmapFromClient(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_BitmapFromClient(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure Lape_FindBitmap(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmap(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_FindBitmapIn(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmapIn(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure Lape_FindBitmapToleranceIn(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmapToleranceIn(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

procedure Lape_FindBitmapSpiral(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmapSpiral(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

procedure Lape_FindBitmapsSpiralTolerance(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmapsSpiralTolerance(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure Lape_FindBitmapSpiralTolerance(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmapSpiralTolerance(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^);
end;

procedure Lape_RotateBitmap(const Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := ps_RotateBitmap(PInteger(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_Desaturate(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_Desaturate(Pinteger(Params^[0])^);
end;

procedure Lape_InvertBitmap(const Params: PParamArray);
begin
  ps_InvertBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_CopyBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_CopyBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_GreyScaleBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_GreyScaleBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_BrightnessBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_BrightnessBitmap(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_ContrastBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_ContrastBitmap(Pinteger(Params^[0])^, Pextended(Params^[1])^);
end;

procedure Lape_PosterizeBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_PosterizeBitmap(Pinteger(Params^[0])^, Pinteger(Params^[1])^);
end;

procedure Lape_CreateMaskFromBitmap(const Params: PParamArray; const Result: Pointer);
begin
  PMask(Result)^ := ps_CreateMaskFromBitmap(Pinteger(Params^[0])^);
end;

procedure Lape_FindMaskTolerance(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindMaskTolerance(PMask(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure Lape_FindBitmapMaskTolerance(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindBitmapMaskTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

procedure Lape_FindDeformedBitmapToleranceIn(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ps_FindDeformedBitmapToleranceIn(Pinteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PBoolean(Params^[9])^, PExtended(Params^[10])^);
end;

procedure Lape_RectangleBitmap(const Params: PParamArray);
begin
  ps_RectangleBitmap(Pinteger(Params^[0])^, PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure Lape_FloodFillBitmap(const Params: PParamArray);
begin
  ps_FloodFillBitmap(Pinteger(Params^[0])^, PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

procedure Lape_ConvoluteBitmap(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_ConvoluteBitmap(Pinteger(Params^[0])^, P2DExtendedArray(Params^[1])^);
end;

procedure Lape_CalculatePixelShift(const Params: PParamArray; const Result: Pointer);
begin
  Pinteger(Result)^ := ps_CalculatePixelShift(PInteger(Params^[0])^, PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure Lape_CalculatePixelTolerance(const Params: PParamArray; const Result: Pointer);
begin
  Pextended(Result)^ := ps_CalculatePixelTolerance(PInteger(Params^[0])^, PInteger(Params^[1])^, PBox(Params^[2])^, Pinteger(Params^[3])^);
end;
