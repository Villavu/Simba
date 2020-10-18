unit simba.script_import_bitmap;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Bitmap(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.bitmap, graphics;

procedure Lape_CreateBitmapString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PString(Result)^ := MBitmaps[PInt32(Params^[0])^].ToString();
end;

procedure Lape_GetMufasaBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PMufasaBitmap(Result)^ := MBitmaps[PInt32(Params^[0])^];
end;

procedure Lape_CreateBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateBMP(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_FreeBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Free();
end;

procedure Lape_SaveBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SaveToFile(PString(Params^[1])^);
end;

procedure Lape_BitmapFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateBMPFromString(PInt32(Params^[0])^, PInt32(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_LoadBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateBMPFromFile(PString(Params^[0])^);
end;

procedure Lape_SetBitmapSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SetSize(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_StretchBitmapResize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].StretchResize(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_GetBitmapSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Params^[1])^ := MBitmaps[PInt32(Params^[0])^].Width;
    PInt32(Params^[2])^ := MBitmaps[PInt32(Params^[0])^].Height;
  end;
end;

procedure Lape_SetBitmapName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Name := PString(Params^[1])^;
end;

procedure Lape_SetPersistentMemoryBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SetPersistentMemory(PPtrUInt(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_ResetPersistentMemoryBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].ResetPersistentMemory();
end;

procedure Lape_CreateMirroredBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateMirroredBitmap(PInt32(Params^[0])^, MirrorWidth);
end;

procedure Lape_CreateMirroredBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBMPMirrorStyle = ^TBmpMirrorStyle;
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CreateMirroredBitmap(PInt32(Params^[0])^, PBMPMirrorStyle(Params^[1])^);
end;

procedure Lape_FastGetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps[PInt32(Params^[0])^].FastGetPixel(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_FastGetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].FastGetPixels(PPointArray(Params^[1])^);
end;

procedure Lape_GetBitmapAreaColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    P2DIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].GetAreaColors(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_FastSetPixel(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastSetPixel(PInt32(Params^[1])^, PInt32(Params^[2])^, PColor(Params^[3])^);
end;

procedure Lape_FastSetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure Lape_DrawTPABitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_DrawATPABitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure Lape_DrawATPABitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure Lape_LineToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^,PInt32(Params^[3])^);
end;

procedure Lape_FastDrawClear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastDrawClear(PColor(Params^[1])^);
end;

procedure Lape_DrawBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawToCanvas(PInt32(Params^[1])^, PInt32(Params^[2])^, PCanvas(Params^[3])^);
end;

procedure Lape_FastDrawTransparent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[2])^].FastDrawTransparent(PInt32(Params^[0])^, PInt32(Params^[1])^, MBitmaps[PInt32(Params^[3])^]);
end;

procedure Lape_SetTransparentColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].SetTransparentColor(PColor(Params^[1])^);
end;

procedure Lape_GetTransparentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PColor(Result)^ := MBitmaps[PInt32(Params^[0])^].GetTransparentColor();
end;

procedure Lape_FastReplaceColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FastReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

procedure Lape_RotateBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].RotateBitmap(PExtended(Params^[1])^, MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_Desaturate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Desaturate(MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_InvertBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Invert();
end;

procedure Lape_CopyBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.CopyBMP(PInt32(Params^[0])^);
end;

procedure Lape_GreyScaleBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].GreyScale(MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_BrightnessBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Brightness(MBitmaps[PInt32(Result)^], PInt32(Params^[1])^);
  end;
end;

procedure Lape_ContrastBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Contrast(MBitmaps[PInt32(Result)^], PExtended(Params^[1])^);
  end;
end;

procedure Lape_PosterizeBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Posterize(MBitmaps[PInt32(Result)^], PInt32(Params^[1])^);
  end;
end;

procedure Lape_CreateMaskFromBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PMask(Result)^ := MBitmaps[PInt32(Params^[0])^].CreateTMask();
end;

procedure Lape_RectangleBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Rectangle(PBox(Params^[1])^, PColor(Params^[2])^);
end;

procedure Lape_FloodFillBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].FloodFill(PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

procedure Lape_ConvoluteBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].Convolute(MBitmaps[PInt32(Result)^], P2DExtendedArray(Params^[1])^);
  end;
end;

procedure Lape_CalculatePixelShift(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := CalculatePixelShift(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PBox(Params^[2])^);
end;

procedure Lape_CalculatePixelShiftTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := CalculatePixelShiftTPA(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PPointArray(Params^[2])^);
end;

procedure Lape_CalculatePixelTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PExtended(Result)^ := CalculatePixelTolerance(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PBox(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_CalculatePixelToleranceTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PExtended(Result)^ := CalculatePixelToleranceTPA(MBitmaps[PInt32(Params^[0])^], MBitmaps[PInt32(Params^[1])^], PPointArray(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_BitmapExists(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MBitmaps.ExistsBMP(PInt32(Params^[0])^);
end;

procedure Lape_FindColorsBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MBitmaps[PInt32(Params^[0])^].FindColors(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_CropBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Crop(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_GetColorsBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].GetColors();
end;

procedure Lape_BitmapToMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    P2DIntegerArray(Result)^ := MBitmaps[PInt32(Params^[0])^].ToMatrix();
end;

procedure Lape_DrawMatrixBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawMatrix(P2DIntegerArray(Params^[1])^);
end;

procedure Lape_ResizeBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBMPResizeMethod = ^TBMPResizeMethod;
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].ResizeEx(PBmpResizeMethod(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_ThresholdAdaptiveBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpThreshMethod = ^TBmpThreshMethod;
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_BlurBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Blur(PInt32(Params^[1])^);
end;

procedure Lape_RotateBitmapEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInt32(Params^[0])^].RotateBitmapEx(PSingle(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^, MBitmaps[PInt32(Result)^]);
  end;
end;

procedure Lape_RectangleBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].Rectangle(PBox(Params^[1])^, PInt32(Params^[2])^, PExtended(Params^[3])^);
end;

procedure Lape_DrawTextBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawText(PString(Params^[1])^, PString(Params^[2])^, PPoint(Params^[3])^, PBoolean(Params^[4])^, PInt32(Params^[5])^);
end;

procedure Lape_DrawSystemTextBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].DrawSystemText(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^, PPoint(Params^[4])^, PBoolean(Params^[5])^, PInt32(Params^[6])^);
end;

procedure Lape_BitmapFromClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    MBitmaps[PInteger(Result)^].CopyClientToBitmap(IOManager, True, PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
  end;
end;

procedure Lape_CopyClientToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MBitmaps[PInt32(Params^[0])^].CopyClientToBitmap(IOManager, True, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_BitmapFromText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MBitmaps.AddBMP(MOCR.TextToFontBitmap(PString(Params^[0])^, PString(Params^[1])^));
end;

procedure Lape_Import_Bitmap(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Bitmap';

    addGlobalFunc('function CreateBitmapString(bmp: Int32): string', @Lape_CreateBitmapString);
    addGlobalFunc('function GetMufasaBitmap(bmp: Int32): TMufasaBitmap', @Lape_GetMufasaBitmap);
    addGlobalFunc('function CreateBitmap(w,h: Int32): Int32', @Lape_CreateBitmap);
    addGlobalFunc('procedure FreeBitmap(Number: Int32);', @Lape_FreeBitmap);
    addGlobalFunc('procedure SaveBitmap(Bmp: Int32; path: string);', @Lape_SaveBitmap);
    addGlobalFunc('function BitmapExists(Index : Int32) : boolean;', @Lape_BitmapExists);
    addGlobalFunc('function BitmapFromString(Width,height: Int32; Data: string): Int32', @Lape_BitmapFromString);
    addGlobalFunc('function LoadBitmap(Path: String): Int32', @Lape_LoadBitmap);
    addGlobalFunc('procedure SetBitmapSize(Bmp,NewW,NewH: Int32);', @Lape_SetBitmapSize);
    addGlobalFunc('procedure StretchBitmapResize(Bmp,NewW,NewH: Int32);', @Lape_StretchBitmapResize);
    addGlobalFunc('procedure ResizeBitmapEx(const bmp: Int32; const Method: TBmpResizeMethod; const NewWidth, NewHeight: Int32);', @Lape_ResizeBitmapEx);
    addGlobalFunc('procedure GetBitmapSize(Bmp: Int32; var BmpW,BmpH: Int32);', @Lape_GetBitmapSize);
    addGlobalFunc('procedure SetPersistentMemoryBitmap(bmp: Int32; mem: PtrUInt; awidth, aheight: Int32);', @Lape_SetPersistentMemoryBitmap);
    addGlobalFunc('procedure ResetPersistentMemoryBitmap(bmp: Int32);', @Lape_ResetPersistentMemoryBitmap);
    addGlobalFunc('procedure SetBitmapName(Bmp: Int32; name: string);', @Lape_SetBitmapName);
    addGlobalFunc('function CreateMirroredBitmap(Bmp: Int32): Int32', @Lape_CreateMirroredBitmap);
    addGlobalFunc('function CreateMirroredBitmapEx(Bmp: Int32; MirrorStyle: TBmpMirrorStyle): Int32', @Lape_CreateMirroredBitmapEx);
    addGlobalFunc('function FastGetPixel(bmp,x,y: Int32): LongWord', @Lape_FastGetPixel);
    addGlobalFunc('function FastGetPixels(bmp: Int32; TPA: TPointArray): TIntegerArray', @Lape_FastGetPixels);
    addGlobalFunc('function GetBitmapAreaColors(bmp,xs, ys, xe, ye: Int32): T2DIntegerArray', @Lape_GetBitmapAreaColors);
    addGlobalFunc('function FindColorsBitmap(bmp: Int32; var points: TPointArray; const color: Int32): boolean;', @Lape_FindColorsBitmap);
    addGlobalFunc('procedure FastSetPixel(Bmp,x,y: Int32; Color: TColor);', @Lape_FastSetPixel);
    addGlobalFunc('procedure FastSetPixels(Bmp: Int32; TPA: TPointArray; Colors: TIntegerArray);', @Lape_FastSetPixels);
    addGlobalFunc('procedure DrawTPABitmap(bitmap: Int32; TPA: TPointArray; Color: Int32);', @Lape_DrawTPABitmap);
    addGlobalFunc('procedure DrawATPABitmap(bitmap: Int32; ATPA: T2DPointArray);', @Lape_DrawATPABitmap);
    addGlobalFunc('procedure DrawATPABitmapEx(bitmap: Int32; ATPA: T2DPointArray; Colors: TIntegerArray);', @Lape_DrawATPABitmapEx);
    addGlobalFunc('procedure FastDrawClear(bmp: Int32; Color: TColor);', @Lape_FastDrawClear);
    addGlobalFunc('procedure DrawBitmap(Bmp: Int32; Dest: TCanvas; x, y: Int32);', @Lape_DrawBitmap);
    addGlobalFunc('procedure FastDrawTransparent(x, y: Int32; SourceBitmap, TargetBitmap: Int32);', @Lape_FastDrawTransparent);
    addGlobalFunc('procedure SetTransparentColor(Bmp: Int32; Color: TColor);', @Lape_SetTransparentColor);
    addGlobalFunc('function GetTransparentColor(Bmp: Int32): TColor', @Lape_GetTransparentColor);
    addGlobalFunc('procedure FastReplaceColor(bmp: Int32; OldColor, NewColor: TColor);', @Lape_FastReplaceColor);
    addGlobalFunc('function RotateBitmap(bitmap: Int32; angle: Extended): Int32', @Lape_RotateBitmap);
    addGlobalFunc('function RotateBitmapEx(Bitmap: Int32; Angle: Single; Expand: Boolean; Smooth: Boolean): Int32', @Lape_RotateBitmapEx);
    addGlobalFunc('function Desaturate(Bitmap: Int32): Int32', @Lape_Desaturate);
    addGlobalFunc('procedure InvertBitmap(Bitmap: Int32);', @Lape_InvertBitmap);
    addGlobalFunc('function CopyBitmap(Bitmap: Int32): Int32', @Lape_CopyBitmap);
    addGlobalFunc('function GreyScaleBitmap(Bitmap: Int32): Int32', @Lape_GreyScaleBitmap);
    addGlobalFunc('function BrightnessBitmap(Bitmap, br: Int32): Int32', @Lape_BrightnessBitmap);
    addGlobalFunc('function ContrastBitmap(bitmap: Int32; co: extended): Int32', @Lape_ContrastBitmap);
    addGlobalFunc('function PosterizeBitmap(Bitmap: Int32; po: Int32): Int32', @Lape_PosterizeBitmap);
    addGlobalFunc('function CreateMaskFromBitmap(Bitmap: Int32): TMask', @Lape_CreateMaskFromBitmap);
    addGlobalFunc('procedure RectangleBitmap(bitmap: Int32; const box: TBox; Color: TColor);', @Lape_RectangleBitmap);
    addGlobalFunc('procedure RectangleBitmapEx(const Bitmap: Int32; const Box: TBox; const Color: Int32; const Transparency: Extended);', @Lape_RectangleBitmapEx);
    addGlobalFunc('procedure FloodFillBitmap(bitmap: Int32; const StartPoint: TPoint; const SearchCol,ReplaceCol: TColor);', @Lape_FloodFillBitmap);
    addGlobalFunc('function ConvoluteBitmap(bitmap: Int32; matrix: T2DExtendedArray): Int32;', @Lape_ConvoluteBitmap);
    addGlobalFunc('procedure ThresholdAdaptiveBitmap(const bmp: Int32; Alpha, Beta: Byte; Invert: Boolean; Method: TBmpThreshMethod; C: Int32);', @Lape_ThresholdAdaptiveBitmap);
    addGlobalFunc('procedure CropBitmap(const bmp: Int32; const xs, ys, xe, ye: Int32);', @Lape_CropBitmap);
    addGlobalFunc('procedure BlurBitmap(const bmp, block: Int32);', @Lape_BlurBitmap);
    addGlobalFunc('procedure DrawTextBitmap(const Bitmap: Int32; const Text, FontName: string; const pnt: TPoint; const Shadow: Boolean; const Color: Int32);', @Lape_DrawTextBitmap);
    addGlobalFunc('procedure DrawSystemTextBitmap(const Bitmap: Int32; const Text, FontName: string; const FontSize: Int32; const pnt: TPoint; const Shadow: Boolean; const Color: Int32); ', @Lape_DrawSystemTextBitmap);
    addGlobalFunc('function CalculatePixelShift(Bmp1, Bmp2: Int32; CompareBox: TBox): Int32', @Lape_CalculatePixelShift);
    addGlobalFunc('function CalculatePixelShiftTPA(Bmp1, Bmp2: Int32; CPoints: TPointArray) : Int32;', @Lape_CalculatePixelShiftTPA);
    addGlobalFunc('function CalculatePixelTolerance(Bmp1, Bmp2: Int32; CompareBox: TBox; CTS: Int32): extended', @Lape_CalculatePixelTolerance);
    addGlobalFunc('function CalculatePixelToleranceTPA(Bmp1, Bmp2: Int32; CPoints: TPointArray; CTS: Int32) : extended;', @Lape_CalculatePixelToleranceTPA);
    addGlobalFunc('function GetColorsBitmap(const bmp: Int32): TIntegerArray;', @Lape_GetColorsBitmap);
    addGlobalFunc('function BitmapToMatrix(const bmp: Int32): T2DIntegerArray;',@Lape_BitmapToMatrix);
    addGlobalFunc('procedure DrawMatrixBitmap(const bmp: Int32; const Matrix: T2DIntegerArray);', @Lape_DrawMatrixBitmap);
    addGlobalFunc('procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Int32);', @Lape_CopyClientToBitmap);
    addGlobalFunc('function BitmapFromClient(const xs, ys, xe, ye: Int32): Int32', @Lape_BitmapFromClient);
    addGlobalFunc('function BitmapFromText(const text, font: String): Int32', @Lape_BitmapFromText);
  end;
end;

end.


