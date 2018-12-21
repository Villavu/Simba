unit script_import_bitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, bitmaps, mufasatypes,
  graphics;

procedure Lape_CreateBitmapString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PString(Result)^ := MBitmaps[PInt32(Params^[1])^].ToString();
end;

procedure Lape_GetMufasaBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PMufasaBitmap(Result)^ := MBitmaps[PInt32(Params^[1])^];
end;

procedure Lape_CreateBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.CreateBMP(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_FreeBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Free();
end;

procedure Lape_SaveBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].SaveToFile(PString(Params^[2])^);
end;

procedure Lape_BitmapFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.CreateBMPFromString(PInt32(Params^[1])^, PInt32(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_LoadBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.CreateBMPFromFile(PString(Params^[1])^);
end;

procedure Lape_SetBitmapSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].SetSize(PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_StretchBitmapResize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].StretchResize(PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_GetBitmapSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Params^[2])^ := MBitmaps[PInt32(Params^[1])^].Width;
    PInt32(Params^[3])^ := MBitmaps[PInt32(Params^[1])^].Height;
  end;
end;

procedure Lape_SetBitmapName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Name := PString(Params^[2])^;
end;

procedure Lape_SetPersistentMemoryBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].SetPersistentMemory(PPtrUInt(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_ResetPersistentMemoryBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].ResetPersistentMemory();
end;

procedure Lape_CreateMirroredBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.CreateMirroredBitmap(PInt32(Params^[1])^, MirrorWidth);
end;

procedure Lape_CreateMirroredBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBMPMirrorStyle = ^TBmpMirrorStyle;
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.CreateMirroredBitmap(PInt32(Params^[1])^, PBMPMirrorStyle(Params^[2])^);
end;

procedure Lape_FastGetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps[PInt32(Params^[1])^].FastGetPixel(PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_FastGetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PIntegerArray(Result)^ := MBitmaps[PInt32(Params^[1])^].FastGetPixels(PPointArray(Params^[2])^);
end;

procedure Lape_GetBitmapAreaColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    P2DIntegerArray(Result)^ := MBitmaps[PInt32(Params^[1])^].GetAreaColors(PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_FastSetPixel(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].FastSetPixel(PInt32(Params^[2])^, PInt32(Params^[3])^, PColor(Params^[4])^);
end;

procedure Lape_FastSetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].FastSetPixels(PPointArray(Params^[2])^, PIntegerArray(Params^[3])^);
end;

procedure Lape_DrawTPABitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawTPA(PPointArray(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_DrawATPABitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawATPA(P2DPointArray(Params^[2])^);
end;

procedure Lape_DrawATPABitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawATPA(P2DPointArray(Params^[2])^, PIntegerArray(Params^[3])^);
end;

procedure Lape_LineToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].LineTo(PPoint(Params^[2])^, PPoint(Params^[3])^,PInt32(Params^[4])^);
end;

procedure Lape_FastDrawClear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].FastDrawClear(PColor(Params^[2])^);
end;

procedure Lape_DrawBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawToCanvas(PInt32(Params^[2])^, PInt32(Params^[3])^, PCanvas(Params^[4])^);
end;

procedure Lape_FastDrawTransparent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[3])^].FastDrawTransparent(PInt32(Params^[1])^, PInt32(Params^[2])^, MBitmaps[PInt32(Params^[4])^]);
end;

procedure Lape_SetTransparentColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].SetTransparentColor(PColor(Params^[2])^);
end;

procedure Lape_GetTransparentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PColor(Result)^ := MBitmaps[PInt32(Params^[1])^].GetTransparentColor();
end;

procedure Lape_FastReplaceColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].FastReplaceColor(PColor(Params^[2])^, PColor(Params^[3])^);
end;

procedure Lape_RotateBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].RotateBitmap(PExtended(Params^[2])^, MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_Desaturate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].Desaturate(MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_InvertBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Invert();
end;

procedure Lape_CopyBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.CopyBMP(PInt32(Params^[1])^);
end;

procedure Lape_GreyScaleBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].GreyScale(MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_BrightnessBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].Brightness(MBitmaps[PInt32(Result)^], PInt32(Params^[2])^);
  end;
end;

procedure Lape_ContrastBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].Contrast(MBitmaps[PInt32(Result)^], PExtended(Params^[2])^);
  end;
end;

procedure Lape_PosterizeBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].Posterize(MBitmaps[PInt32(Result)^], PInt32(Params^[2])^);
  end;
end;

procedure Lape_CreateMaskFromBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PMask(Result)^ := MBitmaps[PInt32(Params^[1])^].CreateTMask();
end;

procedure Lape_RectangleBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Rectangle(PBox(Params^[2])^, PColor(Params^[3])^);
end;

procedure Lape_FloodFillBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].FloodFill(PPoint(Params^[2])^, PColor(Params^[3])^, PColor(Params^[4])^);
end;

procedure Lape_ConvoluteBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].Convolute(MBitmaps[PInt32(Result)^], P2DExtendedArray(Params^[2])^);
  end;
end;

procedure Lape_CalculatePixelShift(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := CalculatePixelShift(MBitmaps[PInt32(Params^[1])^], MBitmaps[PInt32(Params^[2])^], PBox(Params^[3])^);
end;

procedure Lape_CalculatePixelShiftTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := CalculatePixelShiftTPA(MBitmaps[PInt32(Params^[1])^], MBitmaps[PInt32(Params^[2])^], PPointArray(Params^[3])^);
end;

procedure Lape_CalculatePixelTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PExtended(Result)^ := CalculatePixelTolerance(MBitmaps[PInt32(Params^[1])^], MBitmaps[PInt32(Params^[2])^], PBox(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_CalculatePixelToleranceTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PExtended(Result)^ := CalculatePixelToleranceTPA(MBitmaps[PInt32(Params^[1])^], MBitmaps[PInt32(Params^[2])^], PPointArray(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_BitmapExists(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MBitmaps.ExistsBMP(PInt32(Params^[1])^);
end;

procedure Lape_FindColorsBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PBoolean(Result)^ := MBitmaps[PInt32(Params^[1])^].FindColors(PPointArray(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_CropBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Crop(PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_GetColorsBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PIntegerArray(Result)^ := MBitmaps[PInt32(Params^[1])^].GetColors();
end;

procedure Lape_BitmapToMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    P2DIntegerArray(Result)^ := MBitmaps[PInt32(Params^[1])^].ToMatrix();
end;

procedure Lape_DrawMatrixBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawMatrix(P2DIntegerArray(Params^[2])^);
end;

procedure Lape_ResizeBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBMPResizeMethod = ^TBMPResizeMethod;
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].ResizeEx(PBmpResizeMethod(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_ThresholdAdaptiveBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpThreshMethod = ^TBmpThreshMethod;
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].ThresholdAdaptive(PByte(Params^[2])^, PByte(Params^[3])^, PBoolean(Params^[4])^, PBmpThreshMethod(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_BlurBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Blur(PInt32(Params^[2])^);
end;

procedure Lape_RotateBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[1])^].RotateBitmapEx(PSingle(Params^[2])^, PBoolean(Params^[3])^, PBoolean(Params^[4])^, MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_RectangleBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].Rectangle(PBox(Params^[2])^, PInt32(Params^[3])^, PExtended(Params^[4])^);
end;

procedure Lape_DrawTextBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawText(PString(Params^[2])^, PString(Params^[3])^, PPoint(Params^[4])^, PBoolean(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_DrawSystemTextBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].DrawSystemText(PString(Params^[2])^, PString(Params^[3])^, PInt32(Params^[4])^, PPoint(Params^[5])^, PBoolean(Params^[6])^, PInt32(Params^[7])^);
end;

procedure Lape_BitmapFromClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInteger(Result)^].CopyClientToBitmap(IOManager, True, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
  end;
end;

procedure Lape_CopyClientToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    MBitmaps[PInt32(Params^[1])^].CopyClientToBitmap(IOManager, True, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_BitmapFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := MBitmaps.AddBMP(MOCR.TextToFontBitmap(PString(Params^[1])^, PString(Params^[2])^));
end;

procedure Lape_Import_Bitmap(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function CreateBitmapString(bmp: Int32): string', @Lape_CreateBitmapString, Data);
    addGlobalMethod('function GetMufasaBitmap(bmp: Int32): TMufasaBitmap', @Lape_GetMufasaBitmap, Data);
    addGlobalMethod('function CreateBitmap(w,h: Int32): Int32', @Lape_CreateBitmap, Data);
    addGlobalMethod('procedure FreeBitmap(Number: Int32);', @Lape_FreeBitmap, Data);
    addGlobalMethod('procedure SaveBitmap(Bmp: Int32; path: string);', @Lape_SaveBitmap, Data);
    addGlobalMethod('function BitmapExists(Index : Int32) : boolean;', @Lape_BitmapExists, Data);
    addGlobalMethod('function BitmapFromString(Width,height: Int32; Data: string): Int32', @Lape_BitmapFromString, Data);
    addGlobalMethod('function LoadBitmap(Path: String): Int32', @Lape_LoadBitmap, Data);
    addGlobalMethod('procedure SetBitmapSize(Bmp,NewW,NewH: Int32);', @Lape_SetBitmapSize, Data);
    addGlobalMethod('procedure StretchBitmapResize(Bmp,NewW,NewH: Int32);', @Lape_StretchBitmapResize, Data);
    addGlobalMethod('procedure ResizeBitmapEx(const bmp: Int32; const Method: TBmpResizeMethod; const NewWidth, NewHeight: Int32);', @Lape_ResizeBitmapEx, Data);
    addGlobalMethod('procedure GetBitmapSize(Bmp: Int32; var BmpW,BmpH: Int32);', @Lape_GetBitmapSize, Data);
    addGlobalMethod('procedure SetPersistentMemoryBitmap(bmp: Int32; mem: PtrUInt; awidth, aheight: Int32);', @Lape_SetPersistentMemoryBitmap, Data);
    addGlobalMethod('procedure ResetPersistentMemoryBitmap(bmp: Int32);', @Lape_ResetPersistentMemoryBitmap, Data);
    addGlobalMethod('procedure SetBitmapName(Bmp: Int32; name: string);', @Lape_SetBitmapName, Data);
    addGlobalMethod('function CreateMirroredBitmap(Bmp: Int32): Int32', @Lape_CreateMirroredBitmap, Data);
    addGlobalMethod('function CreateMirroredBitmapEx(Bmp: Int32; MirrorStyle: TBmpMirrorStyle): Int32', @Lape_CreateMirroredBitmapEx, Data);
    addGlobalMethod('function FastGetPixel(bmp,x,y: Int32): LongWord', @Lape_FastGetPixel, Data);
    addGlobalMethod('function FastGetPixels(bmp: Int32; TPA: TPointArray): TIntegerArray', @Lape_FastGetPixels, Data);
    addGlobalMethod('function GetBitmapAreaColors(bmp,xs, ys, xe, ye: Int32): TIntegerArray', @Lape_GetBitmapAreaColors, Data);
    addGlobalMethod('function FindColorsBitmap(bmp: Int32; var points: TPointArray; const color: Int32): boolean;', @Lape_FindColorsBitmap, Data);
    addGlobalMethod('procedure FastSetPixel(Bmp,x,y: Int32; Color: TColor);', @Lape_FastSetPixel, Data);
    addGlobalMethod('procedure FastSetPixels(Bmp: Int32; TPA: TPointArray; Colors: TIntegerArray);', @Lape_FastSetPixels, Data);
    addGlobalMethod('procedure DrawTPABitmap(bitmap: Int32; TPA: TPointArray; Color: Int32);', @Lape_DrawTPABitmap, Data);
    addGlobalMethod('procedure DrawATPABitmap(bitmap: Int32; ATPA: T2DPointArray);', @Lape_DrawATPABitmap, Data);
    addGlobalMethod('procedure DrawATPABitmapEx(bitmap: Int32; ATPA: T2DPointArray; Colors: TIntegerArray);', @Lape_DrawATPABitmapEx, Data);
    addGlobalMethod('procedure FastDrawClear(bmp: Int32; Color: TColor);', @Lape_FastDrawClear, Data);
    addGlobalMethod('procedure DrawBitmap(Bmp: Int32; Dest: TCanvas; x, y: Int32);', @Lape_DrawBitmap, Data);
    addGlobalMethod('procedure FastDrawTransparent(x, y: Int32; SourceBitmap, TargetBitmap: Int32);', @Lape_FastDrawTransparent, Data);
    addGlobalMethod('procedure SetTransparentColor(Bmp: Int32; Color: TColor);', @Lape_SetTransparentColor, Data);
    addGlobalMethod('function GetTransparentColor(Bmp: Int32): TColor', @Lape_GetTransparentColor, Data);
    addGlobalMethod('procedure FastReplaceColor(bmp: Int32; OldColor, NewColor: TColor);', @Lape_FastReplaceColor, Data);
    addGlobalMethod('function RotateBitmap(bitmap: Int32; angle: Extended): Int32', @Lape_RotateBitmap, Data);
    addGlobalMethod('function RotateBitmapEx(Bitmap: Int32; Angle: Single; Expand: Boolean; Smooth: Boolean): Int32', @Lape_RotateBitmapEx, Data);
    addGlobalMethod('function Desaturate(Bitmap: Int32): Int32', @Lape_Desaturate, Data);
    addGlobalMethod('procedure InvertBitmap(Bitmap: Int32);', @Lape_InvertBitmap, Data);
    addGlobalMethod('function CopyBitmap(Bitmap: Int32): Int32', @Lape_CopyBitmap, Data);
    addGlobalMethod('function GreyScaleBitmap(Bitmap: Int32): Int32', @Lape_GreyScaleBitmap, Data);
    addGlobalMethod('function BrightnessBitmap(Bitmap, br: Int32): Int32', @Lape_BrightnessBitmap, Data);
    addGlobalMethod('function ContrastBitmap(bitmap: Int32; co: extended): Int32', @Lape_ContrastBitmap, Data);
    addGlobalMethod('function PosterizeBitmap(Bitmap: Int32; po: Int32): Int32', @Lape_PosterizeBitmap, Data);
    addGlobalMethod('function CreateMaskFromBitmap(Bitmap: Int32): TMask', @Lape_CreateMaskFromBitmap, Data);
    addGlobalMethod('procedure RectangleBitmap(bitmap: Int32; const box: TBox; Color: TColor);', @Lape_RectangleBitmap, Data);
    addGlobalMethod('procedure RectangleBitmapEx(const Bitmap: Int32; const Box: TBox; const Color: Int32; const Transparency: Extended);', @Lape_RectangleBitmapEx, Data);
    addGlobalMethod('procedure FloodFillBitmap(bitmap: Int32; const StartPoint: TPoint; const SearchCol,ReplaceCol: TColor);', @Lape_FloodFillBitmap, Data);
    addGlobalMethod('function ConvoluteBitmap(bitmap: Int32; matrix: T2DExtendedArray): Int32;', @Lape_ConvoluteBitmap, Data);
    addGlobalMethod('procedure ThresholdAdaptiveBitmap(const bmp: Int32; Alpha, Beta: Byte; Invert: Boolean; Method: TBmpThreshMethod; C: Int32);', @Lape_ThresholdAdaptiveBitmap, Data);
    addGlobalMethod('procedure CropBitmap(const bmp: Int32; const xs, ys, xe, ye: Int32);', @Lape_CropBitmap, Data);
    addGlobalMethod('procedure BlurBitmap(const bmp, block: Int32);', @Lape_BlurBitmap, Data);
    addGlobalMethod('procedure DrawTextBitmap(const Bitmap: Int32; const Text, FontName: string; const pnt: TPoint; const Shadow: Boolean; const Color: Int32);', @Lape_DrawTextBitmap, Data);
    addGlobalMethod('procedure DrawSystemTextBitmap(const Bitmap: Int32; const Text, FontName: string; const FontSize: Int32; const pnt: TPoint; const Shadow: Boolean; const Color: Int32); ', @Lape_DrawSystemTextBitmap, Data);
    addGlobalMethod('function CalculatePixelShift(Bmp1, Bmp2: Int32; CompareBox: TBox): Int32', @Lape_CalculatePixelShift, Data);
    addGlobalMethod('function CalculatePixelShiftTPA(Bmp1, Bmp2: Int32; CPoints: TPointArray) : Int32;', @Lape_CalculatePixelShiftTPA, Data);
    addGlobalMethod('function CalculatePixelTolerance(Bmp1, Bmp2: Int32; CompareBox: TBox; CTS: Int32): extended', @Lape_CalculatePixelTolerance, Data);
    addGlobalMethod('function CalculatePixelToleranceTPA(Bmp1, Bmp2: Int32; CPoints: TPointArray; CTS: Int32) : extended;', @Lape_CalculatePixelToleranceTPA, Data);
    addGlobalMethod('function GetColorsBitmap(const bmp: Int32): TIntegerArray;', @Lape_GetColorsBitmap, Data);
    addGlobalMethod('function BitmapToMatrix(const bmp: Int32): T2DIntegerArray;',@Lape_BitmapToMatrix, Data);
    addGlobalMethod('procedure DrawMatrixBitmap(const bmp: Int32; const Matrix: T2DIntegerArray);', @Lape_DrawMatrixBitmap, Data);
    addGlobalMethod('procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Int32);', @Lape_CopyClientToBitmap, Data);
    addGlobalMethod('function BitmapFromClient(const xs, ys, xe, ye: Int32): Int32', @Lape_BitmapFromClient, Data);
    addGlobalMethod('function BitmapFromText(const text, font: String): Int32', @Lape_BitmapFromText, Data);
  end;
end;

initialization
  ScriptImports.Add('Bitmap', @Lape_Import_Bitmap);

end.

