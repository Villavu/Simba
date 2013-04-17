unit lpTMufasaBitmap;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler;

procedure Register_TMufasaBitmap(Compiler: TLapeCompiler);

implementation

uses
  MufasaTypes, lptypes, lpClassHelper, bitmaps, Graphics, GraphType, mmlpsthread;

type
  PObject = ^TObject;
  PBitmap = ^TBitmap;
  PCanvas = ^TCanvas;

procedure TMufasaBitmap_Init(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^ := TMufasaBitmap.Create();
  CurrThread.Client.MBitmaps.addBMP(PMufasaBitmap(Params^[0])^);
end;

procedure TMufasaBitmap_Free(const Params: PParamArray); lape_extdecl
begin
  CurrThread.Client.MBitmaps.FreeBMP(PMufasaBitmap(Params^[0])^.Index);
  PMufasaBitmap(Params^[0])^ := nil;
end;

procedure TMufasaBitmap_Data_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPRGB32(Result)^ := PMufasaBitmap(Params^[0])^.FData;
end;

procedure TMufasaBitmap_Data_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FData := PPRGB32(Params^[1])^;
end;

procedure TMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.Name;
end;

procedure TMufasaBitmap_Name_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Name := PlpString(Params^[1])^;
end;

procedure TMufasaBitmap_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PUInt32(Result)^ := PMufasaBitmap(Params^[0])^.Index;
end;

procedure TMufasaBitmap_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Index := PUInt32(Params^[1])^;
end;

procedure TMufasaBitmap_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PUInt32(Result)^ := PMufasaBitmap(Params^[0])^.Width;
end;

procedure TMufasaBitmap_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PUInt32(Result)^ := PMufasaBitmap(Params^[0])^.Height;
end;

procedure TMufasaBitmap_TransparentColorSet_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColorSet;
end;

//procedure TMufasaBitmap.SetSize(Width, Height: UInt32);
procedure TMufasaBitmap_SetSize(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetSize(PUInt32(Params^[1])^, PUInt32(Params^[2])^);
end;

//procedure TMufasaBitmap.StretchResize(Width, Height: UInt32);
procedure TMufasaBitmap_StretchResize(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.StretchResize(PUInt32(Params^[1])^, PUInt32(Params^[2])^);
end;

//procedure TMufasaBitmap.SetPersistentMemory(Memory: PtrUInt; Width, Height: UInt32);
procedure TMufasaBitmap_SetPersistentMemory(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetPersistentMemory(PPtrUInt(Params^[1])^, PUInt32(Params^[2])^, PUInt32(Params^[3])^);
end;

//procedure TMufasaBitmap.ResetPersistentMemory();
procedure TMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.ResetPersistentMemory();
end;

//function TMufasaBitmap.PointInBitmap(x, y: UInt32): boolean;
procedure TMufasaBitmap_PointInBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(PUInt32(Params^[1])^, PUInt32(Params^[2])^);
end;

//procedure TMufasaBitmap.ValidatePoint(x, y: UInt32);
procedure TMufasaBitmap_ValidatePoint(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.ValidatePoint(PUInt32(Params^[1])^, PUInt32(Params^[2])^);
end;

//function TMufasaBitmap.SaveToFile(const FileName: string): boolean;
procedure TMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//procedure TMufasaBitmap.LoadFromFile(const FileName: string);
procedure TMufasaBitmap_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure TMufasaBitmap.Rectangle(const Box: TBox; FillCol: TColor);
procedure TMufasaBitmap_Rectangle(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PColor(Params^[2])^);
end;

//procedure TMufasaBitmap.FloodFill(const StartPT: TPoint; const SearchCol, ReplaceCol: TColor);
procedure TMufasaBitmap_FloodFill(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FloodFill(PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

//procedure TMufasaBitmap.SetPixel(x, y: UInt32; Color: TColor);
procedure TMufasaBitmap_FastSetPixel(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastSetPixel(PUInt32(Params^[1])^, PUInt32(Params^[2])^, PColor(Params^[3])^);
end;

//procedure TMufasaBitmap.SetPixels(Points: TPointArray; Colors: TIntegerArray);
procedure TMufasaBitmap_FastSetPixels(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Colors: TIntegerArray); overload;
procedure TMufasaBitmap_DtawATPA(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray); overload;
procedure TMufasaBitmap_DtawATPAEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

//procedure TMufasaBitmap.DrawTPA(Points: TPointArray; Color: TColor);
procedure TMufasaBitmap_DrawTPA(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

//procedure TMufasaBitmap.DrawToCanvas(x, y: UInt32; Canvas: TCanvas);
procedure TMufasaBitmap_DrawToCanvas(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawToCanvas(PUInt32(Params^[1])^, PUInt32(Params^[2])^, PCanvas(Params^[3])^);
end;

//procedure TMufasaBitmap.LineTo(Src, Dst: TPoint; Color: TColor);
procedure TMufasaBitmap_LineTo(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

//function TMufasaBitmap.CreateTPA(SearchCol: TColor) : TPointArray;
procedure TMufasaBitmap_CreateTPA(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointArray(Result)^ := PMufasaBitmap(Params^[0])^.CreateTPA(PColor(Params^[1])^);
end;

//function TMufasaBitmap.GetPixel(x, y: UInt32) : TColor;
procedure TMufasaBitmap_FastGetPixel(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixel(PUInt32(Params^[1])^, PUInt32(Params^[2])^);
end;

//function TMufasaBitmap.GetPixels(Points: TPointArray) : TIntegerArray;
procedure TMufasaBitmap_FastGetPixels(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixels(PPointArray(Params^[1])^);
end;

//function TMufasaBitmap.GetAreaColors(xs, ys, xe, ye: UInt32): T2DIntArray;
procedure TMufasaBitmap_GetAreaColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DIntArray(Result)^ := PMufasaBitmap(Params^[0])^.GetAreaColors(PUInt32(Params^[1])^, PUInt32(Params^[2])^, PUInt32(Params^[3])^, PUInt32(Params^[4])^);
end;

//function TMufasaBitmap.GetHSLValues(xs, ys, xe, ye: UInt32): T2DHSLArray;
procedure TMufasaBitmap_GetHSLValues(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DHSLArray(Result)^ := PMufasaBitmap(Params^[0])^.GetHSLValues(PUInt32(Params^[1])^, PUInt32(Params^[2])^, PUInt32(Params^[3])^, PUInt32(Params^[4])^);
end;

//procedure TMufasaBitmap.FastDrawClear(Color: TColor);
procedure TMufasaBitmap_FastDrawClear(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastDrawClear(PColor(Params^[1])^);
end;

//procedure TMufasaBitmap.FastDrawTransparent(x, y: UInt32; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_FastDrawTransparent(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastDrawTransparent(PUInt32(Params^[1])^, PUInt32(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

//procedure TMufasaBitmap.FastReplaceColor(OldColor, NewColor: TColor);
procedure TMufasaBitmap_FastReplaceColor(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

//procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: boolean; xs, ys, xe, ye: UInt32); overload;
procedure TMufasaBitmap_CopyClientToBitmap(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, PBoolean(Params^[2])^, PUInt32(Params^[3])^, PUInt32(Params^[4])^, PUInt32(Params^[5])^, PUInt32(Params^[6])^);
end;

//procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: boolean; x, y: UInt32; xs, ys, xe, ye: UInt32); overload;
procedure TMufasaBitmap_CopyClientToBitmapEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, PBoolean(Params^[2])^, PUInt32(Params^[3])^, PUInt32(Params^[4])^, PUInt32(Params^[5])^, PUInt32(Params^[6])^, PUInt32(Params^[7])^, PUInt32(Params^[8])^);
end;

//procedure TMufasaBitmap.RotateBitmap(Angle: Extended; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_RotateBitmap(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.RotateBitmap(PExtended(Params^[1])^, PMufasaBitmap(Params^[2])^);
end;

//procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap); overload;
procedure TMufasaBitmap_Desaturate(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Desaturate(PMufasaBitmap(Params^[1])^);
end;

//procedure TMufasaBitmap.Desaturate(); overload;
procedure TMufasaBitmap_DesaturateEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Desaturate();
end;

//procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap); overload;
procedure TMufasaBitmap_GreyScale(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.GreyScale(PMufasaBitmap(Params^[1])^);
end;

//procedure TMufasaBitmap.GreyScale();
procedure TMufasaBitmap_GreyScaleEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.GreyScale();
end;

//procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Int32); overload;
procedure TMufasaBitmap_Brightness(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Brightness(PMufasaBitmap(Params^[1])^, PInt32(Params^[2])^);
end;

//procedure TMufasaBitmap.Brightness(br: Int32); overload;
procedure TMufasaBitmap_BrightnessEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Brightness(PInt32(Params^[1])^);
end;

//procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended); overload;
procedure TMufasaBitmap_Contrast(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Contrast(PMufasaBitmap(Params^[1])^, PExtended(Params^[2])^);
end;

//procedure TMufasaBitmap.Contrast(co: Extended); overload;
procedure TMufasaBitmap_ContrastEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Contrast(PExtended(Params^[2])^);
end;

//procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap); overload;
procedure TMufasaBitmap_Invert(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Invert(PMufasaBitmap(Params^[1])^);
end;

//procedure TMufasaBitmap.Invert(); overload;
procedure TMufasaBitmap_InvertEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Invert();
end;

//procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Int32); overload;
procedure TMufasaBitmap_Posterize(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Posterize(PMufasaBitmap(Params^[1])^, PInt32(Params^[2])^);
end;

//procedure TMufasaBitmap.Posterize(Po: Int32); overload;
procedure TMufasaBitmap_PosterizeEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Posterize(PInt32(Params^[1])^);
end;

//procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: T2DExtendedArray);
procedure TMufasaBitmap_Convolute(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Convolute(PMufasaBitmap(Params^[1])^, P2DExtendedArray(Params^[2])^);
end;

//function TMufasaBitmap.Copy(const xs, ys, xe, ye: UInt32): TMufasaBitmap; overload;
procedure TMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy(PUInt32(Params^[1])^, PUInt32(Params^[2])^, PUInt32(Params^[3])^, PUInt32(Params^[4])^);
  CurrThread.Client.MBitmaps.addBMP(PMufasaBitmap(Result)^);
end;

//function TMufasaBitmap.Copy(): TMufasaBitmap; overload;
procedure TMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy();
  CurrThread.Client.MBitmaps.addBMP(PMufasaBitmap(Result)^);
end;

//function TMufasaBitmap.ToTBitmap(): TBitmap;
procedure TMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBitmap(Result)^ := PMufasaBitmap(Params^[0])^.ToTBitmap();
end;

//function TMufasaBitmap.ToString(): string;
procedure TMufasaBitmap_ToString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.ToString();
end;

//function TMufasaBitmap.RowPtrs(): TPRGB32Array;
procedure TMufasaBitmap_RowPtrs(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPRGB32Array(Result)^ := PMufasaBitmap(Params^[0])^.RowPtrs();
end;

//procedure TMufasaBitmap.LoadFromTBitmap(Bitmap: TBitmap);
procedure TMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

//procedure TMufasaBitmap.LoadFromRawImage(RawImage: TRawImage);
procedure TMufasaBitmap_LoadFromRawImage(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LoadFromRawImage(PRawImage(Params^[1])^);
end;

//function TMufasaBitmap.CreateTMask(): TMask;
procedure TMufasaBitmap_CreateTMask(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMask(Result)^ := PMufasaBitmap(Params^[0])^.CreateTMask();
end;

//procedure TMufasaBitmap.SetTransparentColor(Color: TColor);
procedure TMufasaBitmap_SetTransparentColor(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetTransparentColor(PColor(Params^[1])^);
end;

//function TMufasaBitmap.GetTransparentColor(): TColor;
procedure TMufasaBitmap_GetTransparentColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.GetTransparentColor();
end;

//procedure TMufasaBitmap.SetAlphaValue(const Value: UInt8);
procedure TMufasaBitmap_SetAlphaValue(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetAlphaValue(PUInt8(Params^[1])^);
end;

procedure Register_TMufasaBitmap(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMufasaBitmap', 'TObject');

    addGlobalFunc('procedure TMufasaBitmap.Init();', @TMufasaBitmap_Init);
    addGlobalFunc('procedure TMufasaBitmap.Free();', @TMufasaBitmap_Free);

    addGlobalType('packed record B, G, R: UInt32; end;', 'TRGB24');
    addGlobalType('^TRGB24', 'PRGB24');
    addGlobalType('packed record B, G, R, A: UInt32; end;', 'TRGB32');
    addGlobalType('^TRGB32', 'PRGB32');
    addGlobalType('array of TRGB32', 'TRGB32Array');
    addGlobalType('^TRGB32Array', 'PRGB32Array');
    addGlobalType('array of PRGB32', 'TPRGB32Array');
    addGlobalType('^TPRGB32Array', 'PPRGB32Array');
    addGlobalType('record H, S, L: extended; end;', 'THSL');
    addGlobalType('^THSL', 'PHSL');
    addGlobalType('array of THSL', 'THSLArray');
    addGlobalType('^THSLArray', 'PHSLArray');
    addGlobalType('array of array of THSL', 'T2DHSLArray');
    addGlobalType('^T2DHSLArray', 'P2DHSLArray');

    //addClassVar(Compiler, 'TMufasaBitmap', 'Data', 'PRGB32', @TMufasaBitmap_Data_Read, @TMufasaBitmap_Data_Write);
    addClassVar(Compiler, 'TMufasaBitmap', 'Name', 'string', @TMufasaBitmap_Name_Read, @TMufasaBitmap_Name_Write);
    addClassVar(Compiler, 'TMufasaBitmap', 'Index', 'UInt32', @TMufasaBitmap_Index_Read, @TMufasaBitmap_Index_Write);
    addClassVar(Compiler, 'TMufasaBitmap', 'Width', 'UInt32', @TMufasaBitmap_Width_Read);
    addClassVar(Compiler, 'TMufasaBitmap', 'Height', 'UInt32', @TMufasaBitmap_Height_Read);
    addClassVar(Compiler, 'TMufasaBitmap', 'TransparentColorSet', 'Boolean', @TMufasaBitmap_TransparentColorSet_Read);

    addGlobalFunc('procedure TMufasaBitmap.SetSize(Width, Height: UInt32);', @TMufasaBitmap_SetSize);
    addGlobalFunc('procedure TMufasaBitmap.StretchResize(Width, Height: UInt32);', @TMufasaBitmap_StretchResize);
    addGlobalFunc('procedure TMufasaBitmap.SetPersistentMemory(Memory: PtrUInt; Width, Height: UInt32);', @TMufasaBitmap_SetPersistentMemory);
    addGlobalFunc('procedure TMufasaBitmap.ResetPersistentMemory();', @TMufasaBitmap_ResetPersistentMemory);
    addGlobalFunc('function TMufasaBitmap.PointInBitmap(x, y: UInt32): boolean;', @TMufasaBitmap_PointInBitmap);
    addGlobalFunc('procedure TMufasaBitmap.ValidatePoint(x, y: UInt32);', @TMufasaBitmap_ValidatePoint);
    addGlobalFunc('function TMufasaBitmap.SaveToFile(const FileName: string): boolean;', @TMufasaBitmap_SaveToFile);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(const FileName: string);', @TMufasaBitmap_LoadFromFile);
    addGlobalFunc('procedure TMufasaBitmap.Rectangle(const Box: TBox; FillCol: TColor);', @TMufasaBitmap_Rectangle);
    addGlobalFunc('procedure TMufasaBitmap.FloodFill(const StartPT: TPoint; const SearchCol, ReplaceCol: TColor);', @TMufasaBitmap_FloodFill);
    addGlobalFunc('procedure TMufasaBitmap.SetPixel(x, y: UInt32; Color: TColor);', @TMufasaBitmap_FastSetPixel);
    addGlobalFunc('procedure TMufasaBitmap.SetPixels(Points: TPointArray; Colors: TIntegerArray);', @TMufasaBitmap_FastSetPixels);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Colors: TIntegerArray); overload;', @TMufasaBitmap_DtawATPA);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray); overload;', @TMufasaBitmap_DtawATPAEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawTPA(Points: TPointArray; Color: TColor);', @TMufasaBitmap_DrawTPA);
    //addGlobalFunc('procedure TMufasaBitmap.DrawToCanvas(x, y: UInt32; Canvas: TCanvas);', @TMufasaBitmap_DrawToCanvas);
    addGlobalFunc('procedure TMufasaBitmap.LineTo(Src, Dst: TPoint; Color: TColor);', @TMufasaBitmap_LineTo);
    addGlobalFunc('function TMufasaBitmap.CreateTPA(SearchCol: TColor) : TPointArray;', @TMufasaBitmap_CreateTPA);
    addGlobalFunc('function TMufasaBitmap.GetPixel(x, y: UInt32) : TColor;', @TMufasaBitmap_FastGetPixel);
    addGlobalFunc('function TMufasaBitmap.GetPixels(Points: TPointArray) : TIntegerArray;', @TMufasaBitmap_FastGetPixels);
    addGlobalFunc('function TMufasaBitmap.GetAreaColors(xs, ys, xe, ye: UInt32): T2DIntArray;', @TMufasaBitmap_GetAreaColors);
    addGlobalFunc('function TMufasaBitmap.GetHSLValues(xs, ys, xe, ye: UInt32): T2DHSLArray;', @TMufasaBitmap_GetHSLValues);
    addGlobalFunc('procedure TMufasaBitmap.DrawClear(Color: TColor);', @TMufasaBitmap_FastDrawClear);
    addGlobalFunc('procedure TMufasaBitmap.DrawTransparent(x, y: UInt32; TargetBitmap: TMufasaBitmap);', @TMufasaBitmap_FastDrawTransparent);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: TColor);', @TMufasaBitmap_FastReplaceColor);
    addGlobalFunc('procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: boolean; xs, ys, xe, ye: UInt32); overload;', @TMufasaBitmap_CopyClientToBitmap);
    addGlobalFunc('procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: boolean; x, y: UInt32; xs, ys, xe, ye: UInt32); overload;', @TMufasaBitmap_CopyClientToBitmapEx);
    addGlobalFunc('procedure TMufasaBitmap.RotateBitmap(Angle: Extended; TargetBitmap: TMufasaBitmap);', @TMufasaBitmap_RotateBitmap);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap); overload;', @TMufasaBitmap_Desaturate);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate; overload;', @TMufasaBitmap_DesaturateEx);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap); overload;', @TMufasaBitmap_GreyScale);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(); overload;', @TMufasaBitmap_GreyScaleEx);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: Int32); overload;', @TMufasaBitmap_Brightness);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(br: Int32); overload;', @TMufasaBitmap_BrightnessEx);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended); overload;', @TMufasaBitmap_Contrast);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(co: Extended); overload;', @TMufasaBitmap_ContrastEx);
    addGlobalFunc('procedure TMufasaBitmap.Invert(TargetBitmap: TMufasaBitmap); overload;', @TMufasaBitmap_Invert);
    addGlobalFunc('procedure TMufasaBitmap.Invert(); overload;', @TMufasaBitmap_InvertEx);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: Int32); overload;', @TMufasaBitmap_Posterize);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(Po: Int32); overload;', @TMufasaBitmap_PosterizeEx);
    addGlobalFunc('procedure TMufasaBitmap.Convolute(TargetBitmap: TMufasaBitmap; Matrix: T2DExtendedArray);', @TMufasaBitmap_Convolute);
    addGlobalFunc('function TMufasaBitmap.Copy(const xs, ys, xe, ye: UInt32): TMufasaBitmap; overload;', @TMufasaBitmap_Copy);
    addGlobalFunc('function TMufasaBitmap.Copy(): TMufasaBitmap; overload;', @TMufasaBitmap_CopyEx);
    //addGlobalFunc('function TMufasaBitmap.ToTBitmap(): TBitmap;', @TMufasaBitmap_ToTBitmap);
    addGlobalFunc('function TMufasaBitmap.ToString(): string;', @TMufasaBitmap_ToString);
    addGlobalFunc('function TMufasaBitmap.RowPtrs(): TPRGB32Array;', @TMufasaBitmap_RowPtrs);
    //addGlobalFunc('procedure TMufasaBitmap.LoadFromTBitmap(Bitmap: TBitmap);', @TMufasaBitmap_LoadFromTBitmap);
    //addGlobalFunc('procedure TMufasaBitmap.LoadFromRawImage(RawImage: TRawImage);', @TMufasaBitmap_LoadFromRawImage);
    addGlobalFunc('function TMufasaBitmap.CreateTMask(): TMask;', @TMufasaBitmap_CreateTMask);
    addGlobalFunc('procedure TMufasaBitmap.SetTransparentColor(Color: TColor);', @TMufasaBitmap_SetTransparentColor);
    addGlobalFunc('function TMufasaBitmap.GetTransparentColor(): TColor;', @TMufasaBitmap_GetTransparentColor);
    addGlobalFunc('procedure TMufasaBitmap.SetAlphaValue(const Value: UInt8);', @TMufasaBitmap_SetAlphaValue);
  end;
end;

end.

