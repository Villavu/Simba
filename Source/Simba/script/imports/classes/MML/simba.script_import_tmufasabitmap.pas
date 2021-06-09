unit simba.script_import_tmufasabitmap;
//Depends: TMufasaBitmap, TObject, procedure(Bitmap : TMufasaBitmap) of object, PRGB32, string, Int32, Int32, PtrUInt, TBox, TColor, TPoint, TPointArray, TIntegerArray, T2DPointArray, TCanvas, boolean, Extended, T2DExtendedArray, TBitmap, TRawImage, byte

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_TMufasaBitmap(Compiler: TSimbaScript_Compiler);

implementation

uses
  simba.bitmap, graphics, graphtype;

type
  PObject = ^TObject;
  PBitmap = ^TBitmap;
  PCanvas = ^TCanvas;
  PMBitmaps = ^TMBitmaps;

//Read: FData : PRGB32;
procedure TMufasaBitmap_Data_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PMufasaBitmap(Params^[0])^.Data;
end;

//Read: property Name : string read FName write FName;
procedure TMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.Name;
end;

//Write: property Name : string read FName write FName;
procedure TMufasaBitmap_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Name := PlpString(Params^[1])^;
end;

//Read: property Index : Int32 read FIndex write FIndex;
procedure TMufasaBitmap_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMufasaBitmap(Params^[0])^.Index;
end;

//Write: property Index : Int32 read FIndex write FIndex;
procedure TMufasaBitmap_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Index := Pinteger(Params^[1])^;
end;

//procedure SetSize(AWidth,AHeight : Int32);
procedure TMufasaBitmap_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure StretchResize(AWidth,AHeight : Int32);
procedure TMufasaBitmap_StretchResize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.StretchResize(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//Read: property Width : Int32 read w;
procedure TMufasaBitmap_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Width;
end;

//Read: property Height : Int32 read h;
procedure TMufasaBitmap_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Height;
end;

//procedure SetPersistentMemory(mem: PtrUInt; awidth, aheight: Int32);
procedure TMufasaBitmap_SetPersistentMemory(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetPersistentMemory(PPtrUInt(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//procedure ResetPersistentMemory;
procedure TMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ResetPersistentMemory();
end;

//function PointInBitmap(x,y : Int32) : boolean;
procedure TMufasaBitmap_PointInBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure ValidatePoint(x,y : Int32);
procedure TMufasaBitmap_ValidatePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.ValidatePoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function SaveToFile(const FileName : string) :boolean;
procedure TMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//procedure LoadFromFile(const FileName : string);
procedure TMufasaBitmap_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure Rectangle(const Box : TBox;FillCol : TColor);
procedure TMufasaBitmap_Rectangle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PColor(Params^[2])^);
end;

//procedure FloodFill(const StartPT : TPoint; const SearchCol, ReplaceCol : TColor);
procedure TMufasaBitmap_FloodFill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FloodFill(PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

//procedure FastSetPixel(x,y : Int32; Color : TColor);
procedure TMufasaBitmap_FastSetPixel(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastSetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PColor(Params^[3])^);
end;

//procedure FastSetPixels(Points : TPointArray; Colors : TIntegerArray);
procedure TMufasaBitmap_FastSetPixels(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);overload;
procedure TMufasaBitmap_DrawATPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//procedure DrawATPA(ATPA : T2DPointArray);overload;
procedure TMufasaBitmap_DrawATPAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

//procedure DrawTPA(Points : TPointArray; Color : TColor);
procedure TMufasaBitmap_DrawTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

//procedure DrawToCanvas(x,y : Int32; Canvas : TCanvas);
procedure TMufasaBitmap_DrawToCanvas(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawToCanvas(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PCanvas(Params^[3])^);
end;

//procedure LineTo(Src,Dst: TPoint;Color: TColor);
procedure TMufasaBitmap_LineTo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

//function TMufasaBitmap.FindColors(var points: TPointArray; const color: Int32): boolean;
procedure TMufasaBitmap_FindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(result)^ := PMufasaBitmap(Params^[0])^.FindColors(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

//function FastGetPixel(x,y : Int32) : TColor;
procedure TMufasaBitmap_FastGetPixel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function FastGetPixels(Points : TPointArray) : TIntegerArray;
procedure TMufasaBitmap_FastGetPixels(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixels(PPointArray(Params^[1])^);
end;

//function GetAreaColors(xs,ys,xe,ye : Int32) : T2DIntArray;
procedure TMufasaBitmap_GetAreaColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetAreaColors(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function GetHSLValues(xs, ys, xe, ye: Int32): T2DHSLArray;
procedure TMufasaBitmap_GetHSLValues(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DHSLArray(Result)^ := PMufasaBitmap(Params^[0])^.GetHSLValues(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure FastDrawClear(Color : TColor);
procedure TMufasaBitmap_FastDrawClear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastDrawClear(PColor(Params^[1])^);
end;

//procedure FastDrawTransparent(x, y: Int32; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_FastDrawTransparent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastDrawTransparent(PInteger(Params^[1])^, PInteger(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

//procedure FastReplaceColor(OldColor, NewColor: TColor);
procedure TMufasaBitmap_FastReplaceColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FastReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

//procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Int32);overload;
procedure TMufasaBitmap_CopyClientToBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, Pboolean(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

//procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean;x,y : Int32; xs, ys, xe, ye: Int32);overload;
procedure TMufasaBitmap_CopyClientToBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, Pboolean(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
procedure TMufasaBitmap_RotateBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.RotateBitmap(PExtended(Params^[1])^, PMufasaBitmap(Params^[2])^);
end;

//procedure RotateBitmapEx(Angle: Single; Expand: Boolean; Smooth: Boolean; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_RotateBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.RotateBitmapEx(PSingle(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^, PMufasaBitmap(Params^[4])^);
end;

//procedure Desaturate(TargetBitmap : TMufasaBitmap); overload;
procedure TMufasaBitmap_Desaturate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Desaturate(PMufasaBitmap(Params^[1])^);
end;

//procedure Desaturate;overload;
procedure TMufasaBitmap_DesaturateEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Desaturate();
end;

//procedure GreyScale(TargetBitmap : TMufasaBitmap);overload;
procedure TMufasaBitmap_GreyScale(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.GreyScale(PMufasaBitmap(Params^[1])^);
end;

//procedure GreyScale;
procedure TMufasaBitmap_GreyScaleEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.GreyScale();
end;

//procedure Brightness(TargetBitmap : TMufasaBitmap; br : Int32); overload;
procedure TMufasaBitmap_Brightness(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Brightness(PMufasaBitmap(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure Brightness(br: Int32);overload;
procedure TMufasaBitmap_BrightnessEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Brightness(Pinteger(Params^[1])^);
end;

//procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);overload;
procedure TMufasaBitmap_Contrast(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Contrast(PMufasaBitmap(Params^[1])^, PExtended(Params^[2])^);
end;

//procedure Contrast(co: Extended);overload;
procedure TMufasaBitmap_ContrastEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Contrast(PExtended(Params^[1])^);
end;

//procedure Invert(TargetBitmap : TMufasaBitmap);overload;
procedure TMufasaBitmap_Invert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Invert(PMufasaBitmap(Params^[1])^);
end;

//procedure Invert;overload;
procedure TMufasaBitmap_InvertEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Invert();
end;

//procedure Posterize(TargetBitmap : TMufasaBitmap; Po : Int32);overload;
procedure TMufasaBitmap_Posterize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Posterize(PMufasaBitmap(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure Posterize(Po : Int32);overload;
procedure TMufasaBitmap_PosterizeEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Posterize(Pinteger(Params^[1])^);
end;

//procedure Convolute(TargetBitmap : TMufasaBitmap; Matrix : T2DExtendedArray);
procedure TMufasaBitmap_Convolute(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Convolute(PMufasaBitmap(Params^[1])^, P2DExtendedArray(Params^[2])^);
end;

//function CompareAt(Other: TMufasaBitmap; Pt: TPoint; Tol: Int32): Extended;
procedure TMufasaBitmap_CompareAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Extended(Result^) := TMufasaBitmap(Params^[0]^).CompareAt(TMufasaBitmap(Params^[1]^), TPoint(Params^[2]^), Int32(Params^[3]^));
end;

//procedure Downsample(DownScale: Int32; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_Downsample(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMufasaBitmap(Params^[0]^).Downsample(Int32(Params^[1]^), TMufasaBitmap(Params^[2]^));
end;

//function Copy(const xs,ys,xe,ye : Int32) : TMufasaBitmap; overload;
procedure TMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function Copy: TMufasaBitmap;overload;
procedure TMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy();
end;

//function ToTBitmap: TBitmap;
procedure TMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PMufasaBitmap(Params^[0])^.ToTBitmap();
end;

//function ToString : string;
procedure TMufasaBitmap_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.ToString();
end;

//procedure Crop(xs, ys, xe, ye: Int32);
procedure TMufasaBitmap_Crop(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

//function GetColors(): TIntegerArray;
procedure TMufasaBitmap_GetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetColors();
end;

//function ToMatrix: T2DIntegerArray;
procedure TMufasaBitmap_ToMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix();
end;

procedure TMufasaBitmap_ToGreyMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DByteArray(Result)^ := PMufasaBitmap(Params^[0])^.ToGreyMatrix();
end;

//procedure DrawMatrix(matrix: T2DIntegerArray);
procedure TMufasaBitmap_DrawMatrix(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(P2DIntegerArray(Params^[1])^);
end;

//procedure DrawMatrix(matrix: T2DIntegerArray; ColorMapID: Int32 = 0); overload;
procedure TMufasaBitmap_DrawMatrixF(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMufasaBitmap(Params^[0]^).DrawMatrix(TSingleMatrix(Params^[1]^), Int32(Params^[2]^));
end;

//procedure ResizeEx(method: TBmpResizeMethod, newW, newH: Int32);
procedure TMufasaBitmap_ResizeEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpResizeMethod = ^TBmpResizeMethod;
begin
  PMufasaBitmap(Params^[0])^.ResizeEx(PBmpResizeMethod(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure ThresholdAdaptive(Alpha, Beta: Byte; Invert: Boolean; Method: TBmpThreshMethod; C: Int32);
procedure TMufasaBitmap_ThresholdAdaptive(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PBmpThreshMethod = ^TBmpThreshMethod;
begin
  PMufasaBitmap(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInteger(Params^[5])^);
end;

procedure TMufasaBitmap_Pad(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Pad(PInteger(Params^[1])^);
end;

//function RowPtrs : TPRGB32Array;
procedure TMufasaBitmap_RowPtrs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32Array(Result)^ := PMufasaBitmap(Params^[0])^.RowPtrs();
end;

//procedure LoadFromTBitmap(bmp: TBitmap);
procedure TMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

//procedure LoadFromRawImage(RawImage: TRawImage);
procedure TMufasaBitmap_LoadFromRawImage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.LoadFromRawImage(PRawImage(Params^[1])^);
end;

//function CreateTMask : TMask;
procedure TMufasaBitmap_CreateTMask(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMask(Result)^ := PMufasaBitmap(Params^[0])^.CreateTMask();
end;

//procedure SetTransparentColor(Col : TColor);
procedure TMufasaBitmap_SetTransparentColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetTransparentColor(PColor(Params^[1])^);
end;

//function GetTransparentColor : TColor;
procedure TMufasaBitmap_GetTransparentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.GetTransparentColor();
end;

//Read: property TransparentColorSet : boolean read FTransparentSet;
procedure TMufasaBitmap_TransparentColorSet_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColorSet;
end;

//procedure SetAlphaValue(const value : byte);
procedure TMufasaBitmap_SetAlphaValue(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.SetAlphaValue(Pbyte(Params^[1])^);
end;

procedure TMufasaBitmap_Blur(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

procedure TMufasaBitmap_BlurEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure TMufasaBitmap_RectangleEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^);
end;

procedure TMufasaBitmap_DownSampleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Downsample(PInt32(Params^[1])^, PBoolean(Params^[2])^);
end;

//Read: List: TMBitmaps;
procedure TMufasaBitmap_List_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMBitmaps(Result)^ := PMufasaBitmap(Params^[0])^.List;
end;

//Write: List: TMBitmaps;
procedure TMufasaBitmap_List_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.List := PMBitmaps(Params^[1])^;
end;

//constructor Create;
procedure TMufasaBitmap_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^ := TMufasaBitmap.Create();
  
  if (PMBitmaps(Params^[1])^ <> nil) then
    PMBitmaps(Params^[1])^.addBMP(PMufasaBitmap(Params^[0])^);
end;

//procedure Free();
procedure TMufasaBitmap_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Free();
end;

//DrawPolyFilled(Poly: TPointArray; Invert: Boolean; Color: TColor);
procedure TMufasaBitmap_DrawPolyFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawPolyFilled(PPointArray(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure TMufasaBitmap_DrawCircleFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawCircleFilled(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PColor(Params^[5])^);
end;

procedure TMufasaBitmap_Blend(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.Blend(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

procedure TMufasaBitmap_DrawBoxFilled(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawBoxFilled(PBox(Params^[1])^, PBoolean(Params^[2])^, PColor(Params^[3])^);
end;

procedure TMufasaBitmap_Center_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.Center;
end;

procedure TMufasaBitmap_Fonts_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := PMufasaBitmap(Params^[0])^.Fonts;
end;

procedure TMufasaBitmap_FontName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMufasaBitmap(Params^[0])^.FontName;
end;

procedure TMufasaBitmap_FontName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FontName := PString(Params^[1])^;
end;

procedure TMufasaBitmap_FontSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PMufasaBitmap(Params^[0])^.FontSize;
end;

procedure TMufasaBitmap_FontSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FontSize := PSingle(Params^[1])^;
end;

procedure TMufasaBitmap_FontAntialiasing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMufasaBitmap(Params^[0])^.FontAntialiasing;
end;

procedure TMufasaBitmap_FontAntialiasing_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.FontAntialiasing := PBoolean(Params^[1])^;
end;

procedure TMufasaBitmap_TextWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := PMufasaBitmap(Params^[0])^.TextWidth(PString(Params^[1])^);
end;

procedure TMufasaBitmap_TextHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := PMufasaBitmap(Params^[0])^.TextHeight(PString(Params^[1])^);
end;

procedure TMufasaBitmap_TextSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PMufasaBitmap(Params^[0])^.TextSize(PString(Params^[1])^);
end;

procedure TMufasaBitmap_DrawText(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

procedure TMufasaBitmap_DrawTextEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PBox(Params^[2])^, PBoolean(Params^[3])^, PColor(Params^[4])^);
end;

procedure Lape_Import_TMufasaBitmap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TMufasaBitmap');

    addGlobalType('packed record B, G, R: UInt8; end;', 'TRGB24');
    addGlobalType('^TRGB24', 'PRGB24');
    addGlobalType('packed record B, G, R, A: UInt8; end;', 'TRGB32');
    addGlobalType('^TRGB32', 'PRGB32');
    addGlobalType('array of TRGB32', 'TRGB32Array');
    addGlobalType('^TRGB32Array', 'PRGB32Array');
    addGlobalType('array of PRGB32', 'TPRGB32Array');
    addGlobalType('^TPRGB32Array', 'PPRGB32Array');
    addGlobalType('packed record H, S, L: extended; end;', 'THSL');
    addGlobalType('^THSL', 'PHSL');
    addGlobalType('array of THSL', 'THSLArray');
    addGlobalType('^THSLArray', 'PHSLArray');
    addGlobalType('array of array of THSL', 'T2DHSLArray');
    addGlobalType('^T2DHSLArray', 'P2DHSLArray');

    addGlobalType('(MirrorWidth, MirrorHeight, MirrorLine)', 'TBmpMirrorStyle');
    addGlobalType('(TM_Mean, TM_MinMax)', 'TBmpThreshMethod');
    addGlobalType('(RM_Nearest, RM_Bilinear)', 'TBmpResizeMethod');

    addGlobalType('record White, Black: TPointArray; WhiteHi, BlackHi: Int32; W, H: Int32; end', 'TMask');

    addClassVar('TMufasaBitmap', 'Data', 'PRGB32', @TMufasaBitmap_Data_Read);
    addClassVar('TMufasaBitmap', 'Name', 'string', @TMufasaBitmap_Name_Read, @TMufasaBitmap_Name_Write);
    addClassVar('TMufasaBitmap', 'Index', 'Int32', @TMufasaBitmap_Index_Read, @TMufasaBitmap_Index_Write);
    addClassVar('TMufasaBitmap', 'Width', 'Int32', @TMufasaBitmap_Width_Read);
    addClassVar('TMufasaBitmap', 'Height', 'Int32', @TMufasaBitmap_Height_Read);
    addClassVar('TMufasaBitmap', 'Center', 'TPoint', @TMufasaBitmap_Center_Read);

    addClassVar('TMufasaBitmap', 'Fonts', 'TStringArray', @TMufasaBitmap_Fonts_Read);
    addClassVar('TMufasaBitmap', 'FontName', 'String', @TMufasaBitmap_FontName_Read, @TMufasaBitmap_FontName_Write);
    addClassVar('TMufasaBitmap', 'FontSize', 'Single', @TMufasaBitmap_FontSize_Read, @TMufasaBitmap_FontSize_Write);
    addClassVar('TMufasaBitmap', 'FontAntialiasing', 'Boolean', @TMufasaBitmap_FontAntialiasing_Read, @TMufasaBitmap_FontAntialiasing_Write);

    addGlobalFunc('function TMufasaBitmap.TextWidth(Text: String): Int32; constref;', @TMufasaBitmap_TextWidth);
    addGlobalFunc('function TMufasaBitmap.TextHeight(Text: String): Int32; constref;', @TMufasaBitmap_TextHeight);
    addGlobalFunc('function TMufasaBitmap.TextSize(Text: String): TPoint; constref;', @TMufasaBitmap_TextSize);

    addGlobalFunc('procedure TMufasaBitmap.DrawText(Text: String; Position: TPoint; Color: TColor); constref; overload;', @TMufasaBitmap_DrawText);
    addGlobalFunc('procedure TMufasaBitmap.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor); constref; overload;', @TMufasaBitmap_DrawTextEx);

    addGlobalFunc('procedure TMufasaBitmap.SetSize(AWidth,AHeight : Int32); constref;', @TMufasaBitmap_SetSize);
    addGlobalFunc('procedure TMufasaBitmap.StretchResize(AWidth,AHeight : Int32); constref;', @TMufasaBitmap_StretchResize);
    addGlobalFunc('procedure TMufasaBitmap.SetPersistentMemory(mem: PtrUInt; awidth, aheight: Int32); constref;', @TMufasaBitmap_SetPersistentMemory);
    addGlobalFunc('procedure TMufasaBitmap.ResetPersistentMemory(); constref;', @TMufasaBitmap_ResetPersistentMemory);
    addGlobalFunc('function TMufasaBitmap.PointInBitmap(x,y : Int32): boolean; constref;', @TMufasaBitmap_PointInBitmap);
    addGlobalFunc('procedure TMufasaBitmap.ValidatePoint(x,y : Int32); constref;', @TMufasaBitmap_ValidatePoint);
    addGlobalFunc('function TMufasaBitmap.SaveToFile(const FileName : string): boolean; constref;', @TMufasaBitmap_SaveToFile);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(const FileName : string); constref;', @TMufasaBitmap_LoadFromFile);
    addGlobalFunc('procedure TMufasaBitmap.Rectangle(const B: TBox; Color : TColor); constref;', @TMufasaBitmap_Rectangle);
    addGlobalFunc('procedure TMufasaBitmap.Rectangle(const B: TBox; const Color: Int32; const Transparency: Extended); constref; overload;', @TMufasaBitmap_RectangleEx);
    addGlobalFunc('procedure TMufasaBitmap.FloodFill(const StartPT : TPoint; const SearchCol, ReplaceCol : TColor); constref;', @TMufasaBitmap_FloodFill);
    addGlobalFunc('procedure TMufasaBitmap.SetPixel(x,y : Int32; Color : TColor); constref;', @TMufasaBitmap_FastSetPixel);
    addGlobalFunc('procedure TMufasaBitmap.SetPixels(Points : TPointArray; Colors : TIntegerArray); constref;', @TMufasaBitmap_FastSetPixels);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray); constref;', @TMufasaBitmap_DrawATPA);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA : T2DPointArray); constref; overload;', @TMufasaBitmap_DrawATPAEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawTPA(Points : TPointArray; Color : TColor); constref;', @TMufasaBitmap_DrawTPA);
    addGlobalFunc('procedure TMufasaBitmap.DrawToCanvas(x,y : Int32; Canvas : TCanvas); constref;', @TMufasaBitmap_DrawToCanvas);
    addGlobalFunc('procedure TMufasaBitmap.LineTo(Src,Dst: TPoint;Color: TColor); constref;', @TMufasaBitmap_LineTo);
    addGlobalFunc('function TMufasaBitmap.FindColors(var points: TPointArray; const color: Int32): Boolean; constref;', @TMufasaBitmap_FindColors);
    addGlobalFunc('function TMufasaBitmap.GetPixel(x,y : Int32): TColor; constref;', @TMufasaBitmap_FastGetPixel);
    addGlobalFunc('function TMufasaBitmap.GetPixels(Points : TPointArray): TIntegerArray; constref;', @TMufasaBitmap_FastGetPixels);
    addGlobalFunc('function TMufasaBitmap.GetAreaColors(xs,ys,xe,ye : Int32): T2DIntArray; constref;', @TMufasaBitmap_GetAreaColors);
    addGlobalFunc('function TMufasaBitmap.GetHSLValues(xs, ys, xe, ye: Int32): T2DHSLArray; constref;', @TMufasaBitmap_GetHSLValues);
    addGlobalFunc('procedure TMufasaBitmap.DrawClear(Color : TColor); constref;', @TMufasaBitmap_FastDrawClear);
    addGlobalFunc('procedure TMufasaBitmap.DrawTransparent(x, y: Int32; TargetBitmap: TMufasaBitmap); constref;', @TMufasaBitmap_FastDrawTransparent);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: TColor); constref;', @TMufasaBitmap_FastReplaceColor);
    addGlobalFunc('procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Int32); constref;', @TMufasaBitmap_CopyClientToBitmap);
    addGlobalFunc('procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TObject;Resize : boolean;x,y : Int32; xs, ys, xe, ye: Int32); constref; overload;', @TMufasaBitmap_CopyClientToBitmapEx);
    addGlobalFunc('procedure TMufasaBitmap.RotateBitmap(angle: Extended; TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_RotateBitmap);
    addGlobalFunc('procedure TMufasaBitmap.RotateBitmapEx(Angle: Single; Expand: Boolean; Smooth: Boolean; TargetBitmap: TMufasaBitmap); constref;', @TMufasaBitmap_RotateBitmapEx);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate(TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_Desaturate);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate(); constref; overload;', @TMufasaBitmap_DesaturateEx);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_GreyScale);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(); constref; overload;', @TMufasaBitmap_GreyScaleEx);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(TargetBitmap : TMufasaBitmap; br : Int32); constref;', @TMufasaBitmap_Brightness);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(br: Int32); constref; overload;', @TMufasaBitmap_BrightnessEx);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(TargetBitmap : TMufasaBitmap; co : Extended); constref;', @TMufasaBitmap_Contrast);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(co: Extended); constref; overload;', @TMufasaBitmap_ContrastEx);
    addGlobalFunc('procedure TMufasaBitmap.Invert(TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_Invert);
    addGlobalFunc('procedure TMufasaBitmap.Invert(); constref; overload;', @TMufasaBitmap_InvertEx);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(TargetBitmap : TMufasaBitmap; Po : Int32); constref;', @TMufasaBitmap_Posterize);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(Po : Int32); constref; overload;', @TMufasaBitmap_PosterizeEx);
    addGlobalFunc('procedure TMufasaBitmap.Blur(const Block: Int32); constref;', @TMufasaBitmap_Blur);
    addGlobalFunc('procedure TMufasaBitmap.Blur(const Block, xs, ys, xe, ye: Int32); constref; overload;', @TMufasaBitmap_BlurEx);
    addGlobalFunc('procedure TMufasaBitmap.Convolute(TargetBitmap : TMufasaBitmap; Matrix : T2DExtendedArray); constref;', @TMufasaBitmap_Convolute);

    addGlobalFunc('function  TMufasaBitmap.CompareAt(Other: TMufasaBitmap; Pt: TPoint; Tol: Int32): Extended;', @TMufasaBitmap_CompareAt);
    addGlobalFunc('procedure TMufasaBitmap.Downsample(DownScale: Int32; TargetBitmap: TMufasaBitmap); overload;', @TMufasaBitmap_Downsample);
    addGlobalFunc('function TMufasaBitmap.Downsample(DownScale: Int32; BlendTransparentColor: Boolean = True): TMufasaBitmap; overload;', @TMufasaBitmap_DownSampleEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawPolyFilled(Poly: TPointArray; Invert: Boolean; Color: TColor); constref', @TMufasaBitmap_DrawPolyFilled);
    addGlobalFunc('procedure TMufasaBitmap.DrawCircleFilled(X, Y, Radius: Int32; Invert: Boolean; Color: TColor); constref', @TMufasaBitmap_DrawCircleFilled);
    addGlobalFunc('procedure TMufasaBitmap.DrawBoxFilled(const B: TBox; Invert: Boolean; Color: TColor); constref;', @TMufasaBitmap_DrawBoxFilled);
    addGlobalFunc('procedure TMufasaBitmap.Blend(Points: TPointArray; Size: Int32); constref', @TMufasaBitmap_Blend);
    addGlobalFunc('function TMufasaBitmap.Copy(const xs,ys,xe,ye : Int32): TMufasaBitmap; constref;', @TMufasaBitmap_Copy);
    addGlobalFunc('function TMufasaBitmap.Copy: TMufasaBitmap; constref; overload;', @TMufasaBitmap_CopyEx);
    addGlobalFunc('procedure TMufasaBitmap.Crop(xs, ys, xe, ye: Int32); constref;', @TMufasaBitmap_Crop);
    addGlobalFunc('function TMufasaBitmap.GetColors: TIntegerArray; constref;', @TMufasaBitmap_GetColors);
    addGlobalFunc('function TMufasaBitmap.ToMatrix: T2DIntArray; constref;', @TMufasaBitmap_ToMatrix);
    addGlobalFunc('function TMufasaBitmap.ToGreyMatrix: T2DByteArray; constref;', @TMufasaBitmap_ToGreyMatrix);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(const Matrix: T2DIntArray); constref;', @TMufasaBitmap_DrawMatrix);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(const Matrix: TSingleMatrix; ColorMapID: Int32 = 0); constref; overload;', @TMufasaBitmap_DrawMatrixF);
    addGlobalFunc('procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; DoInvert: Boolean; Method: TBmpThreshMethod; C: Int32); constref;', @TMufasaBitmap_ThresholdAdaptive);
    addGlobalFunc('procedure TMufasaBitmap.Pad(Amount: Int32);', @TMufasaBitmap_Pad);
    addGlobalFunc('procedure TMufasaBitmap.ResizeEx(Method: TBmpResizeMethod; NewW, NewH: Int32); constref;', @TMufasaBitmap_ResizeEx);
    addGlobalFunc('function TMufasaBitmap.ToTBitmap(): TBitmap; constref;', @TMufasaBitmap_ToTBitmap);
    addGlobalFunc('function TMufasaBitmap.ToString(): string; constref;', @TMufasaBitmap_ToString);
    addGlobalFunc('function TMufasaBitmap.RowPtrs(): TPRGB32Array; constref;', @TMufasaBitmap_RowPtrs);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromTBitmap(bmp: TBitmap); constref;', @TMufasaBitmap_LoadFromTBitmap);
    //addGlobalFunc('procedure TMufasaBitmap.LoadFromRawImage(RawImage: TRawImage); constref;', @TMufasaBitmap_LoadFromRawImage);
    addGlobalFunc('function TMufasaBitmap.CreateTMask(): TMask; constref;', @TMufasaBitmap_CreateTMask);
    addGlobalFunc('procedure TMufasaBitmap.SetTransparentColor(Col : TColor); constref;', @TMufasaBitmap_SetTransparentColor);
    addGlobalFunc('function TMufasaBitmap.GetTransparentColor(): TColor; constref;', @TMufasaBitmap_GetTransparentColor);
    addClassVar('TMufasaBitmap', 'TransparentColorSet', 'boolean', @TMufasaBitmap_TransparentColorSet_Read, nil);
    addGlobalFunc('procedure TMufasaBitmap.SetAlphaValue(const value : byte); constref;', @TMufasaBitmap_SetAlphaValue);
    addClassVar('TMufasaBitmap', 'List', 'TObject', @TMufasaBitmap_List_Read, @TMufasaBitmap_List_Write);
    addGlobalFunc('procedure TMufasaBitmap.Init(List: TObject = nil);', @TMufasaBitmap_Init);
    //addGlobalFunc('procedure TMufasaBitmap.Free(); constref;', @TMufasaBitmap_Free);
  end;
end;

end.

