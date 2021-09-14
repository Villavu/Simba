procedure _LapeCreateBitmapString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PString(Result)^ := MBitmaps[PInt32(Params^[0])^].ToString();
end;

procedure _LapeGetMufasaBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PMufasaBitmap(Result)^ := MBitmaps[PInt32(Params^[0])^];
end;

procedure _LapeCreateBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateBMP(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeFreeBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Free();
end;

procedure _LapeSaveBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SaveToFile(PString(Params^[1])^);
end;

procedure _LapeBitmapFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateBMPFromString(PInt32(Params^[0])^, PInt32(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeLoadBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateBMPFromFile(PString(Params^[0])^);
end;

procedure _LapeSetBitmapSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SetSize(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeStretchBitmapResize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].StretchResize(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeGetBitmapSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Params^[1])^ := MBitmaps[PInt32(Params^[0])^].Width;
    PInt32(Params^[2])^ := MBitmaps[PInt32(Params^[0])^].Height;
  end;
end;

procedure _LapeSetBitmapName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Name := PString(Params^[1])^;
end;

procedure _LapeSetPersistentMemoryBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SetPersistentMemory(PPtrUInt(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeResetPersistentMemoryBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].ResetPersistentMemory();
end;

procedure _LapeCreateMirroredBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateMirroredBitmap(PInt32(Params^[0])^, MirrorWidth);
end;

procedure _LapeCreateMirroredBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBMPMirrorStyle = ^TBmpMirrorStyle;
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateMirroredBitmap(PInt32(Params^[0])^, PBMPMirrorStyle(Params^[1])^);
end;

procedure _LapeFastGetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps[PInt32(Params^[0])^].FastGetPixel(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeFastGetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].FastGetPixels(PPointArray(Params^[1])^);
end;

procedure _LapeGetBitmapAreaColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    P2DIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].GetAreaColors(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeFastSetPixel(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastSetPixel(PInt32(Params^[1])^, PInt32(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeFastSetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeDrawTPABitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeDrawATPABitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure _LapeDrawATPABitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure _LapeLineToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^,PInt32(Params^[3])^);
end;

procedure _LapeFastDrawClear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastDrawClear(PColor(Params^[1])^);
end;

procedure _LapeDrawBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawToCanvas(PInt32(Params^[1])^, PInt32(Params^[2])^, PCanvas(Params^[3])^);
end;

procedure _LapeFastDrawTransparent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[2])^].FastDrawTransparent(PInt32(Params^[0])^, PInt32(Params^[1])^, MBitmaps[PInt32(Params^[3])^]);
end;

procedure _LapeSetTransparentColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SetTransparentColor(PColor(Params^[1])^);
end;

procedure _LapeGetTransparentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PColor(Result)^ := MBitmaps[PInt32(Params^[0])^].GetTransparentColor();
end;

procedure _LapeFastReplaceColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeRotateBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].RotateBitmap(PExtended(Params^[1])^, MBitmaps[PInt32(Result)^]);
  end;
end;

procedure _LapeDesaturate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Desaturate(MBitmaps[PInt32(Result)^]);
  end;
end;

procedure _LapeInvertBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Invert();
end;

procedure _LapeCopyBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CopyBMP(PInt32(Params^[0])^);
end;

procedure _LapeGreyScaleBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].GreyScale(MBitmaps[PInt32(Result)^]);
  end;
end;

procedure _LapeBrightnessBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Brightness(MBitmaps[PInt32(Result)^], PInt32(Params^[1])^);
  end;
end;

procedure _LapeContrastBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Contrast(MBitmaps[PInt32(Result)^], PExtended(Params^[1])^);
  end;
end;

procedure _LapePosterizeBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Posterize(MBitmaps[PInt32(Result)^], PInt32(Params^[1])^);
  end;
end;

procedure _LapeCreateMaskFromBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PMask(Result)^ := MBitmaps[PInt32(Params^[0])^].CreateTMask();
end;

procedure _LapeRectangleBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Rectangle(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure _LapeFloodFillBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FloodFill(PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

procedure _LapeConvoluteBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Convolute(MBitmaps[PInt32(Result)^], P2DExtendedArray(Params^[1])^);
  end;
end;

procedure _LapeCalculatePixelShift(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := CalculatePixelShift(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PBox(Params^[2])^);
end;

procedure _LapeCalculatePixelShiftTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := CalculatePixelShiftTPA(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PPointArray(Params^[2])^);
end;

procedure _LapeCalculatePixelTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PExtended(Result)^ := CalculatePixelTolerance(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PBox(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeCalculatePixelToleranceTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PExtended(Result)^ := CalculatePixelToleranceTPA(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PPointArray(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeBitmapExists(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MBitmaps.ExistsBMP(PInt32(Params^[0])^);
end;

procedure _LapeFindColorsBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MBitmaps[PInt32(Params^[0])^].FindColors(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeCropBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Crop(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeGetColorsBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].GetColors();
end;

procedure _LapeBitmapToMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    P2DIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].ToMatrix();
end;

procedure _LapeDrawMatrixBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawMatrix(P2DIntegerArray(Params^[1])^);
end;

procedure _LapeResizeBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBMPResizeMethod = ^TBMPResizeMethod;
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].ResizeEx(PBmpResizeMethod(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeThresholdAdaptiveBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpThreshMethod = ^TBmpThreshMethod;
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInt32(Params^[5])^);
end;

procedure _LapeBlurBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Blur(PInt32(Params^[1])^);
end;

procedure _LapeRotateBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].RotateBitmapEx(PSingle(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^, MBitmaps[PInt32(Result)^]);
  end;
end;

procedure _LapeRectangleBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Rectangle(PBox(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^);
end;

procedure _LapeBitmapFromClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInteger(Result)^].CopyClientToBitmap(IOManager, True, PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
  end;
end;

procedure _LapeCopyClientToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].CopyClientToBitmap(IOManager, True, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure _LapeBitmapFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.AddBMP(MOCR.TextToFontBitmap(PString(Params^[0])^, PString(Params^[1])^));
end;

