procedure RUTIS_ps_CreateBitmapString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ps_CreateBitmapString(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetMufasaBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PMufasaBitmap(Result^.Data)^ := ps_GetMufasaBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_CreateBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_CreateBitmap(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_FreeBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FreeBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_SaveBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SaveBitmap(Pinteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_BitmapFromString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_BitmapFromString(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pstring(Params^[2].Data)^);
end;

procedure RUTIS_ps_LoadBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_LoadBitmap(PString(Params^[0].Data)^);
end;

procedure RUTIS_ps_SetBitmapSize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetBitmapSize(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_StretchBitmapResize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_StretchBitmapResize(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_GetBitmapSize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_GetBitmapSize(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_SetBitmapName(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetBitmapName(Pinteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_CreateMirroredBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_CreateMirroredBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_CreateMirroredBitmapEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_CreateMirroredBitmapEx(Pinteger(Params^[0].Data)^, PBmpMirrorStyle(Params^[1].Data)^);
end;

procedure RUTIS_ps_FastGetPixel(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PLongWord(Result^.Data)^ := ps_FastGetPixel(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_FastGetPixels(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PIntegerArray(Result^.Data)^ := ps_FastGetPixels(Pinteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetBitmapAreaColors(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  P2DIntArray(Result^.Data)^ := ps_GetBitmapAreaColors(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^);
end;

procedure RUTIS_ps_FastSetPixel(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FastSetPixel(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, PColor(Params^[3].Data)^);
end;

procedure RUTIS_ps_FastSetPixels(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FastSetPixels(Pinteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^, PIntegerArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_DrawTPABitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_DrawTPABitmap(Pinteger(Params^[0].Data)^, PPointArray(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_DrawATPABitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_DrawATPABitmap(Pinteger(Params^[0].Data)^, P2DPointArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_DrawATPABitmapEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_DrawATPABitmapEx(Pinteger(Params^[0].Data)^, P2DPointArray(Params^[1].Data)^, PIntegerArray(Params^[2].Data)^);
end;

procedure RUTIS_ps_FastDrawClear(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FastDrawClear(Pinteger(Params^[0].Data)^, PColor(Params^[1].Data)^);
end;

procedure RUTIS_ps_DrawBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_DrawBitmap(PInteger(Params^[0].Data)^, PCanvas(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^);
end;

procedure RUTIS_ps_FastDrawTransparent(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FastDrawTransparent(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^);
end;

procedure RUTIS_ps_SetTransparentColor(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetTransparentColor(Pinteger(Params^[0].Data)^, PColor(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetTransparentColor(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PColor(Result^.Data)^ := ps_GetTransparentColor(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_FastReplaceColor(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FastReplaceColor(PInteger(Params^[0].Data)^, PColor(Params^[1].Data)^, PColor(Params^[2].Data)^);
end;

procedure RUTIS_ps_CopyClientToBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_CopyClientToBitmap(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^);
end;

procedure RUTIS_ps_BitmapFromClient(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_BitmapFromClient(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^);
end;

procedure RUTIS_ps_FindBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmap(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_FindBitmapIn(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmapIn(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FindBitmapToleranceIn(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmapToleranceIn(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^);
end;

procedure RUTIS_ps_FindBitmapSpiral(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmapSpiral(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FindBitmapsSpiralTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmapsSpiralTolerance(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PPointArray(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^, PInteger(Params^[8].Data)^);
end;

procedure RUTIS_ps_FindBitmapSpiralTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmapSpiralTolerance(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, Pinteger(Params^[3].Data)^, Pinteger(Params^[4].Data)^, Pinteger(Params^[5].Data)^, Pinteger(Params^[6].Data)^, Pinteger(Params^[7].Data)^);
end;

procedure RUTIS_ps_RotateBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_RotateBitmap(PInteger(Params^[0].Data)^, PExtended(Params^[1].Data)^);
end;

procedure RUTIS_ps_Desaturate(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_Desaturate(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_InvertBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_InvertBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_CopyBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_CopyBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GreyScaleBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_GreyScaleBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_BrightnessBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_BrightnessBitmap(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_ContrastBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_ContrastBitmap(Pinteger(Params^[0].Data)^, Pextended(Params^[1].Data)^);
end;

procedure RUTIS_ps_PosterizeBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_PosterizeBitmap(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_CreateMaskFromBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PMask(Result^.Data)^ := ps_CreateMaskFromBitmap(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_FindMaskTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindMaskTolerance(PMask(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^, PInteger(Params^[8].Data)^);
end;

procedure RUTIS_ps_FindBitmapMaskTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindBitmapMaskTolerance(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^, PInteger(Params^[8].Data)^);
end;

procedure RUTIS_ps_FindDeformedBitmapToleranceIn(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindDeformedBitmapToleranceIn(Pinteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^, PInteger(Params^[8].Data)^, PBoolean(Params^[9].Data)^, PExtended(Params^[10].Data)^);
end;

procedure RUTIS_ps_RectangleBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_RectangleBitmap(Pinteger(Params^[0].Data)^, PBox(Params^[1].Data)^, PColor(Params^[2].Data)^);
end;

procedure RUTIS_ps_FloodFillBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FloodFillBitmap(Pinteger(Params^[0].Data)^, PPoint(Params^[1].Data)^, PColor(Params^[2].Data)^, PColor(Params^[3].Data)^);
end;

procedure RUTIS_ps_CalculatePixelShift(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_CalculatePixelShift(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PBox(Params^[2].Data)^);
end;

procedure RUTIS_ps_CalculatePixelTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pextended(Result^.Data)^ := ps_CalculatePixelTolerance(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PBox(Params^[2].Data)^, Pinteger(Params^[3].Data)^);
end;
