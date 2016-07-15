unit lpTMufasaBitmap;
//Depends: TMufasaBitmap, TObject, procedure(Bitmap : TMufasaBitmap) of object, PRGB32, string, integer, Integer, PtrUInt, TBox, TColor, TPoint, TPointArray, TIntegerArray, T2DPointArray, TCanvas, boolean, Extended, T2DExtendedArray, TBitmap, TRawImage, byte

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMufasaBitmap(Compiler: TLapeCompiler);

implementation

uses
  MufasaTypes, bitmaps, Graphics, GraphType;

type
  PObject = ^TObject;
  PBitmap = ^TBitmap;
  PCanvas = ^TCanvas;
  PMBitmaps = ^TMBitmaps;

//Read: FData : PRGB32;
procedure TMufasaBitmap_FData_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPRGB32(Result)^ := PMufasaBitmap(Params^[0])^.FData;
end;

//Write: FData : PRGB32;
procedure TMufasaBitmap_FData_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FData := PPRGB32(Params^[1])^;
end;

//Read: property Name : string read FName write FName;
procedure TMufasaBitmap_Name_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.Name;
end;

//Write: property Name : string read FName write FName;
procedure TMufasaBitmap_Name_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Name := PlpString(Params^[1])^;
end;

//Read: property Index : integer read FIndex write FIndex;
procedure TMufasaBitmap_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMufasaBitmap(Params^[0])^.Index;
end;

//Write: property Index : integer read FIndex write FIndex;
procedure TMufasaBitmap_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Index := Pinteger(Params^[1])^;
end;

//procedure SetSize(AWidth,AHeight : integer);
procedure TMufasaBitmap_SetSize(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure StretchResize(AWidth,AHeight : integer);
procedure TMufasaBitmap_StretchResize(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.StretchResize(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//Read: property Width : Integer read w;
procedure TMufasaBitmap_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Width;
end;

//Read: property Height : Integer read h;
procedure TMufasaBitmap_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMufasaBitmap(Params^[0])^.Height;
end;

//procedure SetPersistentMemory(mem: PtrUInt; awidth, aheight: integer);
procedure TMufasaBitmap_SetPersistentMemory(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetPersistentMemory(PPtrUInt(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//procedure ResetPersistentMemory;
procedure TMufasaBitmap_ResetPersistentMemory(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.ResetPersistentMemory();
end;

//function PointInBitmap(x,y : integer) : boolean;
procedure TMufasaBitmap_PointInBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.PointInBitmap(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure ValidatePoint(x,y : integer);
procedure TMufasaBitmap_ValidatePoint(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.ValidatePoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function SaveToFile(const FileName : string) :boolean;
procedure TMufasaBitmap_SaveToFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//procedure LoadFromFile(const FileName : string);
procedure TMufasaBitmap_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure Rectangle(const Box : TBox;FillCol : TColor);
procedure TMufasaBitmap_Rectangle(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PColor(Params^[2])^);
end;

//procedure FloodFill(const StartPT : TPoint; const SearchCol, ReplaceCol : TColor);
procedure TMufasaBitmap_FloodFill(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FloodFill(PPoint(Params^[1])^, PColor(Params^[2])^, PColor(Params^[3])^);
end;

//procedure FastSetPixel(x,y : integer; Color : TColor);
procedure TMufasaBitmap_FastSetPixel(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastSetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PColor(Params^[3])^);
end;

//procedure FastSetPixels(Points : TPointArray; Colors : TIntegerArray);
procedure TMufasaBitmap_FastSetPixels(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);overload;
procedure TMufasaBitmap_DrawATPA(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//procedure DrawATPA(ATPA : T2DPointArray);overload;
procedure TMufasaBitmap_DrawATPAEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

//procedure DrawTPA(Points : TPointArray; Color : TColor);
procedure TMufasaBitmap_DrawTPA(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

//procedure DrawToCanvas(x,y : integer; Canvas : TCanvas);
procedure TMufasaBitmap_DrawToCanvas(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawToCanvas(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PCanvas(Params^[3])^);
end;

//procedure LineTo(Src,Dst: TPoint;Color: TColor);
procedure TMufasaBitmap_LineTo(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LineTo(PPoint(Params^[1])^, PPoint(Params^[2])^, PColor(Params^[3])^);
end;

//function TMufasaBitmap.FindColors(var points: TPointArray; const color: integer): boolean;
procedure TMufasaBitmap_FindColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(result)^ := PMufasaBitmap(Params^[0])^.FindColors(PPointArray(Params^[1])^, PColor(Params^[2])^);
end;

//function FastGetPixel(x,y : integer) : TColor;
procedure TMufasaBitmap_FastGetPixel(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixel(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function FastGetPixels(Points : TPointArray) : TIntegerArray;
procedure TMufasaBitmap_FastGetPixels(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.FastGetPixels(PPointArray(Params^[1])^);
end;

//function GetAreaColors(xs,ys,xe,ye : integer) : T2DIntArray;
procedure TMufasaBitmap_GetAreaColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DIntArray(Result)^ := PMufasaBitmap(Params^[0])^.GetAreaColors(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function GetHSLValues(xs, ys, xe, ye: integer): T2DHSLArray;
procedure TMufasaBitmap_GetHSLValues(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DHSLArray(Result)^ := PMufasaBitmap(Params^[0])^.GetHSLValues(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure FastDrawClear(Color : TColor);
procedure TMufasaBitmap_FastDrawClear(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastDrawClear(PColor(Params^[1])^);
end;

//procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_FastDrawTransparent(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastDrawTransparent(PInteger(Params^[1])^, PInteger(Params^[2])^, PMufasaBitmap(Params^[3])^);
end;

//procedure FastReplaceColor(OldColor, NewColor: TColor);
procedure TMufasaBitmap_FastReplaceColor(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.FastReplaceColor(PColor(Params^[1])^, PColor(Params^[2])^);
end;

//procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Integer);overload;
procedure TMufasaBitmap_CopyClientToBitmap(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, Pboolean(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

//procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);overload;
procedure TMufasaBitmap_CopyClientToBitmapEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.CopyClientToBitmap(PObject(Params^[1])^, Pboolean(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
procedure TMufasaBitmap_RotateBitmap(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.RotateBitmap(PExtended(Params^[1])^, PMufasaBitmap(Params^[2])^);
end;

//procedure RotateBitmapEx(Angle: Single; Expand: Boolean; Smooth: Boolean; TargetBitmap: TMufasaBitmap);
procedure TMufasaBitmap_RotateBitmapEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.RotateBitmapEx(PSingle(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^, PMufasaBitmap(Params^[4])^);
end;

//procedure Desaturate(TargetBitmap : TMufasaBitmap); overload;
procedure TMufasaBitmap_Desaturate(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Desaturate(PMufasaBitmap(Params^[1])^);
end;

//procedure Desaturate;overload;
procedure TMufasaBitmap_DesaturateEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Desaturate();
end;

//procedure GreyScale(TargetBitmap : TMufasaBitmap);overload;
procedure TMufasaBitmap_GreyScale(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.GreyScale(PMufasaBitmap(Params^[1])^);
end;

//procedure GreyScale;
procedure TMufasaBitmap_GreyScaleEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.GreyScale();
end;

//procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer); overload;
procedure TMufasaBitmap_Brightness(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Brightness(PMufasaBitmap(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure Brightness(br: integer);overload;
procedure TMufasaBitmap_BrightnessEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Brightness(Pinteger(Params^[1])^);
end;

//procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);overload;
procedure TMufasaBitmap_Contrast(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Contrast(PMufasaBitmap(Params^[1])^, PExtended(Params^[2])^);
end;

//procedure Contrast(co: Extended);overload;
procedure TMufasaBitmap_ContrastEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Contrast(PExtended(Params^[1])^);
end;

//procedure Invert(TargetBitmap : TMufasaBitmap);overload;
procedure TMufasaBitmap_Invert(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Invert(PMufasaBitmap(Params^[1])^);
end;

//procedure Invert;overload;
procedure TMufasaBitmap_InvertEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Invert();
end;

//procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);overload;
procedure TMufasaBitmap_Posterize(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Posterize(PMufasaBitmap(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure Posterize(Po : integer);overload;
procedure TMufasaBitmap_PosterizeEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Posterize(Pinteger(Params^[1])^);
end;

//procedure Convolute(TargetBitmap : TMufasaBitmap; Matrix : T2DExtendedArray);
procedure TMufasaBitmap_Convolute(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Convolute(PMufasaBitmap(Params^[1])^, P2DExtendedArray(Params^[2])^);
end;

//function Copy(const xs,ys,xe,ye : integer) : TMufasaBitmap; overload;
procedure TMufasaBitmap_Copy(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function Copy: TMufasaBitmap;overload;
procedure TMufasaBitmap_CopyEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMufasaBitmap(Result)^ := PMufasaBitmap(Params^[0])^.Copy();
end;

//function ToTBitmap: TBitmap;
procedure TMufasaBitmap_ToTBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBitmap(Result)^ := PMufasaBitmap(Params^[0])^.ToTBitmap();
end;

//function ToString : string;
procedure TMufasaBitmap_ToString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMufasaBitmap(Params^[0])^.ToString();
end;

//procedure Crop(xs, ys, xe, ye: integer);
procedure TMufasaBitmap_Crop(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Crop(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^)
end;

//function GetColors(): TIntegerArray;
procedure TMufasaBitmap_GetColors(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PIntegerArray(Result)^ := PMufasaBitmap(Params^[0])^.GetColors();
end;

//function ToMatrix: T2DIntegerArray;
procedure TMufasaBitmap_ToMatrix(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  P2DIntArray(Result)^ := PMufasaBitmap(Params^[0])^.ToMatrix();
end;

//procedure DrawMatrix(matrix: T2DIntegerArray);
procedure TMufasaBitmap_DrawMatrix(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawMatrix(P2DIntArray(Params^[1])^);
end;

//procedure ResizeEx(method: TBmpResizeMethod, newW, newH: integer);
procedure TMufasaBitmap_ResizeEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.ResizeEx(PBmpResizeMethod(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure ThresholdAdaptive(Alpha, Beta: Byte; Invert: Boolean; Method: TBmpThreshMethod; C: Integer);
procedure TMufasaBitmap_ThresholdAdaptive(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.ThresholdAdaptive(PByte(Params^[1])^, PByte(Params^[2])^, PBoolean(Params^[3])^, PBmpThreshMethod(Params^[4])^, PInteger(Params^[5])^);
end;

//function RowPtrs : TPRGB32Array;
procedure TMufasaBitmap_RowPtrs(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPRGB32Array(Result)^ := PMufasaBitmap(Params^[0])^.RowPtrs();
end;

//procedure LoadFromTBitmap(bmp: TBitmap);
procedure TMufasaBitmap_LoadFromTBitmap(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LoadFromTBitmap(PBitmap(Params^[1])^);
end;

//procedure LoadFromRawImage(RawImage: TRawImage);
procedure TMufasaBitmap_LoadFromRawImage(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.LoadFromRawImage(PRawImage(Params^[1])^);
end;

//function CreateTMask : TMask;
procedure TMufasaBitmap_CreateTMask(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMask(Result)^ := PMufasaBitmap(Params^[0])^.CreateTMask();
end;

//procedure SetTransparentColor(Col : TColor);
procedure TMufasaBitmap_SetTransparentColor(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetTransparentColor(PColor(Params^[1])^);
end;

//function GetTransparentColor : TColor;
procedure TMufasaBitmap_GetTransparentColor(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PMufasaBitmap(Params^[0])^.GetTransparentColor();
end;

//Read: property TransparentColorSet : boolean read FTransparentSet;
procedure TMufasaBitmap_TransparentColorSet_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMufasaBitmap(Params^[0])^.TransparentColorSet;
end;

//procedure SetAlphaValue(const value : byte);
procedure TMufasaBitmap_SetAlphaValue(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.SetAlphaValue(Pbyte(Params^[1])^);
end;

procedure TMufasaBitmap_Blur(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^);
end;

procedure TMufasaBitmap_BlurEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Blur(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure TMufasaBitmap_RectangleEx(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Rectangle(PBox(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^);
end;

procedure TMufasaBitmap_DrawText(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PString(Params^[2])^, PPoint(Params^[3])^, PBoolean(Params^[4])^, PInteger(Params^[5])^);
end;

procedure TMufasaBitmap_DrawSystemText(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.DrawSystemText(PString(Params^[1])^, PString(Params^[2])^, PInteger(Params^[3])^, PPoint(Params^[4])^, PBoolean(Params^[5])^, PInteger(Params^[6])^);
end;

//Read: List: TMBitmaps;
procedure TMufasaBitmap_List_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMBitmaps(Result)^ := PMufasaBitmap(Params^[0])^.List;
end;

//Write: List: TMBitmaps;
procedure TMufasaBitmap_List_Write(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.List := PMBitmaps(Params^[1])^;
end;

//constructor Create;
procedure TMufasaBitmap_Init(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^ := TMufasaBitmap.Create();
  
  if (PMBitmaps(Params^[1])^ <> nil) then
    PMBitmaps(Params^[1])^.addBMP(PMufasaBitmap(Params^[0])^);
end;

//procedure Free();
procedure TMufasaBitmap_Free(const Params: PParamArray); lape_extdecl
begin
  PMufasaBitmap(Params^[0])^.Free();
end;

procedure Register_TMufasaBitmap(Compiler: TLapeCompiler);
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

    addClassVar('TMufasaBitmap', 'Data', 'PRGB32', @TMufasaBitmap_FData_Read, @TMufasaBitmap_FData_Write);
    addClassVar('TMufasaBitmap', 'Name', 'string', @TMufasaBitmap_Name_Read, @TMufasaBitmap_Name_Write);
    addClassVar('TMufasaBitmap', 'Index', 'integer', @TMufasaBitmap_Index_Read, @TMufasaBitmap_Index_Write);
    addGlobalFunc('procedure TMufasaBitmap.SetSize(AWidth,AHeight : integer); constref;', @TMufasaBitmap_SetSize);
    addGlobalFunc('procedure TMufasaBitmap.StretchResize(AWidth,AHeight : integer); constref;', @TMufasaBitmap_StretchResize);
    addClassVar('TMufasaBitmap', 'Width', 'Integer', @TMufasaBitmap_Width_Read);
    addClassVar('TMufasaBitmap', 'Height', 'Integer', @TMufasaBitmap_Height_Read);
    addGlobalFunc('procedure TMufasaBitmap.SetPersistentMemory(mem: PtrUInt; awidth, aheight: integer); constref;', @TMufasaBitmap_SetPersistentMemory);
    addGlobalFunc('procedure TMufasaBitmap.ResetPersistentMemory(); constref;', @TMufasaBitmap_ResetPersistentMemory);
    addGlobalFunc('function TMufasaBitmap.PointInBitmap(x,y : integer): boolean; constref;', @TMufasaBitmap_PointInBitmap);
    addGlobalFunc('procedure TMufasaBitmap.ValidatePoint(x,y : integer); constref;', @TMufasaBitmap_ValidatePoint);
    addGlobalFunc('function TMufasaBitmap.SaveToFile(const FileName : string): boolean; constref;', @TMufasaBitmap_SaveToFile);
    addGlobalFunc('procedure TMufasaBitmap.LoadFromFile(const FileName : string); constref;', @TMufasaBitmap_LoadFromFile);
    addGlobalFunc('procedure TMufasaBitmap.Rectangle(const Box : TBox;FillCol : TColor); constref;', @TMufasaBitmap_Rectangle);
    addGlobalFunc('procedure TMufasaBitmap.Rectangle(const Box: TBox; const Color: Integer; const Transparency: Extended); constref; overload;', @TMufasaBitmap_RectangleEx);
    addGlobalFunc('procedure TMufasaBitmap.FloodFill(const StartPT : TPoint; const SearchCol, ReplaceCol : TColor); constref;', @TMufasaBitmap_FloodFill);
    addGlobalFunc('procedure TMufasaBitmap.SetPixel(x,y : integer; Color : TColor); constref;', @TMufasaBitmap_FastSetPixel);
    addGlobalFunc('procedure TMufasaBitmap.SetPixels(Points : TPointArray; Colors : TIntegerArray); constref;', @TMufasaBitmap_FastSetPixels);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray); constref;', @TMufasaBitmap_DrawATPA);
    addGlobalFunc('procedure TMufasaBitmap.DrawATPA(ATPA : T2DPointArray); constref; overload;', @TMufasaBitmap_DrawATPAEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawTPA(Points : TPointArray; Color : TColor); constref;', @TMufasaBitmap_DrawTPA);
    addGlobalFunc('procedure TMufasaBitmap.DrawToCanvas(x,y : integer; Canvas : TCanvas); constref;', @TMufasaBitmap_DrawToCanvas);
    addGlobalFunc('procedure TMufasaBitmap.LineTo(Src,Dst: TPoint;Color: TColor); constref;', @TMufasaBitmap_LineTo);
    addGlobalFunc('function TMufasaBitmap.FindColors(var points: TPointArray; const color: integer): Boolean; constref;', @TMufasaBitmap_FindColors);
    addGlobalFunc('function TMufasaBitmap.GetPixel(x,y : integer): TColor; constref;', @TMufasaBitmap_FastGetPixel);
    addGlobalFunc('function TMufasaBitmap.GetPixels(Points : TPointArray): TIntegerArray; constref;', @TMufasaBitmap_FastGetPixels);
    addGlobalFunc('function TMufasaBitmap.GetAreaColors(xs,ys,xe,ye : integer): T2DIntArray; constref;', @TMufasaBitmap_GetAreaColors);
    addGlobalFunc('function TMufasaBitmap.GetHSLValues(xs, ys, xe, ye: integer): T2DHSLArray; constref;', @TMufasaBitmap_GetHSLValues);
    addGlobalFunc('procedure TMufasaBitmap.DrawClear(Color : TColor); constref;', @TMufasaBitmap_FastDrawClear);
    addGlobalFunc('procedure TMufasaBitmap.DrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap); constref;', @TMufasaBitmap_FastDrawTransparent);
    addGlobalFunc('procedure TMufasaBitmap.ReplaceColor(OldColor, NewColor: TColor); constref;', @TMufasaBitmap_FastReplaceColor);
    addGlobalFunc('procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Integer); constref;', @TMufasaBitmap_CopyClientToBitmap);
    addGlobalFunc('procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TObject;Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer); constref; overload;', @TMufasaBitmap_CopyClientToBitmapEx);
    addGlobalFunc('procedure TMufasaBitmap.RotateBitmap(angle: Extended; TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_RotateBitmap);
    addGlobalFunc('procedure TMufasaBitmap.RotateBitmapEx(Angle: Single; Expand: Boolean; Smooth: Boolean; TargetBitmap: TMufasaBitmap); constref;', @TMufasaBitmap_RotateBitmapEx);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate(TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_Desaturate);
    addGlobalFunc('procedure TMufasaBitmap.Desaturate(); constref; overload;', @TMufasaBitmap_DesaturateEx);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_GreyScale);
    addGlobalFunc('procedure TMufasaBitmap.GreyScale(); constref; overload;', @TMufasaBitmap_GreyScaleEx);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(TargetBitmap : TMufasaBitmap; br : integer); constref;', @TMufasaBitmap_Brightness);
    addGlobalFunc('procedure TMufasaBitmap.Brightness(br: integer); constref; overload;', @TMufasaBitmap_BrightnessEx);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(TargetBitmap : TMufasaBitmap; co : Extended); constref;', @TMufasaBitmap_Contrast);
    addGlobalFunc('procedure TMufasaBitmap.Contrast(co: Extended); constref; overload;', @TMufasaBitmap_ContrastEx);
    addGlobalFunc('procedure TMufasaBitmap.Invert(TargetBitmap : TMufasaBitmap); constref;', @TMufasaBitmap_Invert);
    addGlobalFunc('procedure TMufasaBitmap.Invert(); constref; overload;', @TMufasaBitmap_InvertEx);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(TargetBitmap : TMufasaBitmap; Po : integer); constref;', @TMufasaBitmap_Posterize);
    addGlobalFunc('procedure TMufasaBitmap.Posterize(Po : integer); constref; overload;', @TMufasaBitmap_PosterizeEx);
    addGlobalFunc('procedure TMufasaBitmap.Blur(const Block: integer); constref;', @TMufasaBitmap_Blur);
    addGlobalFunc('procedure TMufasaBitmap.Blur(const Block, xs, ys, xe, ye: integer); constref; overload;', @TMufasaBitmap_BlurEx);
    addGlobalFunc('procedure TMufasaBitmap.DrawText(const Text, FontName: string; const pnt: TPoint; const Shadow: Boolean; const Color: Integer); constref;', @TMufasaBitmap_DrawText);
    addGlobalFunc('procedure TMufasaBitmap.DrawSystemText(const Text, FontName: string; const FontSize: Integer; const pnt: TPoint; const Shadow: Boolean; const Color: Integer); constref;', @TMufasaBitmap_DrawSystemText);
    addGlobalFunc('procedure TMufasaBitmap.Convolute(TargetBitmap : TMufasaBitmap; Matrix : T2DExtendedArray); constref;', @TMufasaBitmap_Convolute);
    addGlobalFunc('function TMufasaBitmap.Copy(const xs,ys,xe,ye : integer): TMufasaBitmap; constref;', @TMufasaBitmap_Copy);
    addGlobalFunc('function TMufasaBitmap.Copy(): TMufasaBitmap; constref; overload;', @TMufasaBitmap_CopyEx);
    addGlobalFunc('procedure TMufasaBitmap.Crop(xs, ys, xe, ye: integer); constref;', @TMufasaBitmap_Crop);
    addGlobalFunc('function TMufasaBitmap.GetColors(): TIntegerArray; constref;', @TMufasaBitmap_GetColors);
    addGlobalFunc('function TMufasaBitmap.ToMatrix(): T2DIntArray; constref;', @TMufasaBitmap_ToMatrix);
    addGlobalFunc('procedure TMufasaBitmap.DrawMatrix(const matrix: T2DIntArray); constref;', @TMufasaBitmap_DrawMatrix);
    addGlobalFunc('procedure TMufasaBitmap.ThresholdAdaptive(Alpha, Beta: Byte; Invert: Boolean; Method: TBmpThreshMethod; C: Integer); constref;', @TMufasaBitmap_ThresholdAdaptive);
    addGlobalFunc('procedure TMufasaBitmap.ResizeEx(Method: TBmpResizeMethod; NewW, NewH: integer); constref;', @TMufasaBitmap_ResizeEx);
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
    addGlobalFunc('procedure TMufasaBitmap.Free();', @TMufasaBitmap_Free);
  end;
end;

end.

