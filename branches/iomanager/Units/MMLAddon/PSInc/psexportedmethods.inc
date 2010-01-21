{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    psexportedmethods.inc for the Mufasa Macro Library
}

AddFunction(@ThreadSafeCall,'function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;');
AddFunction(nil,'procedure Writeln(x);'); //We use special function for this

{ DTM }
SetCurrSection('DTM');
AddFunction(@PrintpDTM, 'Procedure PrintpDTM(tDTM : pDTM);');
AddFunction(@ps_GetDTM ,'function GetDTM(index: Integer; out dtm: pDTM): Boolean;');
AddFunction(@pDTMToTDTM, 'Function pDTMToTDTM(DTM: pDTM): TDTM;');
AddFunction(@tDTMTopDTM, 'Function tDTMTopDTM(DTM: TDTM): pDTM;');
AddFunction(@ps_DTMFromString, 'function DTMFromString(DTMString: String): Integer;');
AddFunction(@ps_FreeDTM, 'procedure FreeDTM(DTM: Integer);');
AddFunction(@ps_FindDTM, 'function FindDTM(DTM: Integer; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean;');
AddFunction(@ps_FindDTMs, 'function FindDTMs(DTM: Integer; out p: TPointArray; x1, y1, x2, y2: Integer): Boolean;');
AddFunction(@ps_FindDTMRotated, 'function FindDTMRotated(DTM: Integer; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;');
AddFunction(@ps_FindDTMsRotated, 'function FindDTMsRotated(DTM: Integer; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray) : Boolean;');
AddFunction(@ps_addDTM, 'function AddDTM(d: TDTM): Integer;');
AddFunction(@ps_addpDTM, 'function AddpDTM(d: pDTM): Integer;');

{maths}
SetCurrSection('Math');
AddFunction(nil,'function Round(e:extended) : integer');
AddFunction(@ceil,'function ceil(e : extended) : integer');
AddFunction(@power,'function pow(base,exponent : extended) : extended');
AddFunction(@max,'function Max(a, b: Integer): Integer;');
AddFunction(@min,'function Min(a, b: Integer): Integer;');
AddFunction(@minE,'function MinE(a, b: extended): Extended;');
AddFunction(@IntToBox,'function IntToBox(x1,y1,x2,y2 : integer) : TBox;');
AddFunction(@pssqr,'function Sqr(e : extended) : extended;');
AddFunction(@classes.point,'function Point(x,y:integer) : TPoint;');
AddFunction(@Distance,'function Distance(x1,y1,x2,y2 : integer) : integer;');
AddFunction(@hypot,'function Hypot(X, Y: Extended): Extended;');
AddFunction(@ps_RandomRange,'function RandomRange(aFrom,aTo: Integer): Integer;');

{window}
SetCurrSection('Window');
AddFunction(@Freeze, 'function Freeze:boolean;');
AddFunction(@Unfreeze, 'function Unfreeze: boolean;');
AddFunction(@GetClientDimensions, 'procedure GetClientDimensions(out w, h:integer);');
AddFunction(@SetTargetBitmap,'procedure SetTargetBitmap(Bitmap : integer);');
AddFunction(@SetDesktopAsClient,'procedure SetDesktopAsClient');
AddFunction(@SetTargetArray, 'procedure SetTargetArray(P: Integer; w, h: integer);');
AddFunction(@ActivateClient, 'procedure activateclient;');

{files}
SetCurrSection('Files');
AddFunction(@ps_CreateFile, 'function CreateFile(Path: string): Integer;');
AddFunction(@ps_OpenFile, 'function OpenFile(Path: string; Shared: Boolean): Integer;');
AddFunction(@ps_RewriteFile, 'function RewriteFile(Path: string; Shared: Boolean): Integer;');
AddFunction(@ps_CloseFile, 'procedure CloseFile(FileNum: Integer);');
AddFunction(@ps_EndOfFile, 'function EndOfFile(FileNum: Integer): Boolean;');
AddFunction(@ps_FileSize, 'function FileSize(FileNum: Integer): LongInt;');
AddFunction(@ps_ReadFileString, 'function ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;');
AddFunction(@ps_WriteFileString, 'function WriteFileString(FileNum: Integer; s: string): Boolean;');
AddFunction(@ps_SetFileCharPointer, 'Function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;');
AddFunction(@ps_FilePointerPos, 'function FilePointerPos(FileNum: Integer): Integer;');

{other}
SetCurrSection('Other');
AddFunction(@SaveScreenshot,'procedure SaveScreenshot(FileName: string);');
AddFunction(@psWait, 'procedure wait(t: integer);');
AddFunction(@psWait, 'procedure Sleep(t: integer);');
AddFunction(@GetTickCount, 'function GetSystemTime: LongWord;');
AddFunction(@GetTickCount, 'function GetTickCount: LongWord;');
AddFunction(@GetTimeRunning,'function GetTimeRunning: LongWord;');
AddFunction(@ConvertTime,'procedure ConvertTime(Time: integer; var h, m, s: integer);');
AddFunction(@HakunaMatata,'procedure HakunaMatata;');
AddFunction(@TerminateScript,'procedure TerminateScript;');
AddFunction(@DisplayDebugImgWindow,'procedure DisplayDebugImgWindow(w, h: integer);');
AddFunction(@DrawBitmapDebugImg,'procedure DrawBitmapDebugImg(bmp: integer);');
AddFunction(@GetDebugBitmap,'function GetDebugBitmap: integer;');
AddFunction(@Random,'function Random(Int: integer): integer;');
AddFunction(@NewThreadCall,'function NewThreadCall(procname : string) : cardinal');
AddFunction(@ClearDebug,'procedure ClearDebug;');



{string}
SetCurrSection('String');
AddFunction(@Capitalize,'function Capitalize(str : string) : string;');
AddFunction(@psFormat,'function Format(const fmt : string;const args : array of const) : string;');
AddFunction(nil,'function ToStr(x) : string;');
AddFunction(@IntToStr, 'function IntToStr(value: Integer): String;');
AddFunction(@FloatToStr, 'function FloatToStr(value: Extended): String;');
AddFunction(@psBoolToStr, 'function BoolToStr(value: Boolean): String;');
AddFunction(@StrToInt, 'function StrToInt(value: String): Integer;');
AddFunction(@StrToIntDef, 'function StrToIntDef(value: String; default: Integer): Integer;');
AddFunction(@StrToFloat, 'function StrToFloat(value: String): Extended;');
AddFunction(@StrToFloatDef, 'function StrToFloatDef(value: String; default: Extended): Extended;');
AddFunction(@StrToBool, 'function StrToBool(value: String): Boolean;');
AddFunction(@StrToBoolDef, 'function StrToBoolDef(value: String; default: Boolean): Boolean;');

{web}
SetCurrSection('Web');
AddFunction(@OpenWebPage,'procedure OpenWebPage(url : string);');
AddFunction(@ps_GetPage,'function GetPage(url : string): string;');


{ Color Conversions and Speed }
SetCurrSection('Color Convert');
AddFunction(@Colour_Conv.ColorToRGB, 'procedure ColorToRGB(Color: integer; out r, g, b: Byte);');
AddFunction(@Colour_conv.RGBToColor, 'function RGBtoColor(r, g, b: Byte): TColor;');
AddFunction(@Colour_conv.ColorToHSL, 'procedure ColorToHSL(Color: Integer; out h, s, l: Extended);');
AddFunction(@Colour_conv.HSLToColor, 'function HSLToColor(H, S, L: Extended): TColor;');
AddFunction(@Colour_conv.ColorToXYZ, 'procedure ColorToXYZ(Color: Integer; out x, y, z: Extended);');
AddFunction(@Colour_conv.XYZToColor, 'function XYZToColor(X, Y, Z: Extended): TColor;');
AddFunction(@Colour_conv.RGBToHSL, 'procedure RGBToHSL(R, G, B: Byte; out h, s, l: Extended);');
AddFunction(@Colour_conv.HSLToRGB, 'procedure HSLtoRGB(H, S, L: extended; out R, G ,B: Byte);');
AddFunction(@Colour_conv.RGBToXYZ, 'procedure RGBToXYZ(R, G, B: Byte;out x, y ,z: Extended);');
AddFunction(@Colour_conv.XYZToRGB, 'procedure XYZToRGB(X, Y, Z: Extended; out R, G, B: Byte);');

{ Color Finding }
SetCurrSection('Color');
AddFunction(@SetColorToleranceSpeed, 'procedure SetColorToleranceSpeed(cts: integer);');
AddFunction(@GetToleranceSpeed, 'function GetToleranceSpeed: Integer;');
AddFunction(@SetToleranceSpeed2Modifiers, 'procedure SetToleranceSpeed2Modifiers(nHue, nSat: Extended);');
AddFunction(@GetToleranceSpeed2Modifiers, 'procedure GetToleranceSpeed2Modifiers(out hMod, sMod: Extended);');
AddFunction(@GetColor,'function GetColor(x, y: Integer): Integer;');
AddFunction(@FindColor, 'function FindColor(out x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
AddFunction(@findcolortoleranceOptimised, 'function FindColorToleranceOptimised(out x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
AddFunction(@FindColorTolerance, 'function FindColorTolerance(out x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
AddFunction(@FindColors, 'function FindColors(out TPA: TPointArray; color, x1, y1, x2, y2: integer): boolean;');
AddFunction(@SimilarColors,'function SimilarColors(Col1, Col2, Tolerance: integer): boolean');
AddFunction(@CountColor,'function CountColor(Color, xs, ys, xe, ye: Integer): Integer;');
AddFunction(@CountColorTolerance,'function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;');
AddFunction(@FindColorsToleranceOptimised,'function FindColorsToleranceOptimised(out Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
AddFunction(@FindColorsTolerance,'function FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
AddFunction(@FindColorSpiral,'function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@FindColorsSpiralTolerance,'function FindColorsSpiralTolerance(x, y: Integer; out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;');
AddFunction(@FindColoredArea, 'function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye, MinArea: Integer): Boolean');
AddFunction(@FindColoredAreaTolerance, 'function FindColoredAreaTolerance(var x, y : Integer; color, xs, ys, xe, ye, MinArea, Tolerance : Integer): Boolean');

{Mouse etc.}
SetCurrSection('Mouse');
AddFunction(@MoveMouse, 'procedure MoveMouse(x, y: integer);');
AddFunction(@GetMousePos, 'procedure GetMousePos(out x, y: integer);');
AddFunction(@HoldMouse, 'procedure HoldMouse(x, y: integer; clickType: integer);');
AddFunction(@ReleaseMouse, 'procedure ReleaseMouse(x, y: integer; clickType: integer);');
AddFunction(@ClickMouse, 'procedure ClickMouse(x, y: integer; clickType: integer);');


{Keyboard}
SetCurrSection('Keyboard');
AddFunction(@KeyDown, 'procedure KeyDown(key: Word);');
AddFunction(@KeyUp, 'procedure KeyUp(key: Word);');
AddFunction(@PressKey, 'procedure PressKey(key: Word);');
AddFunction(@SendKeys, 'procedure SendKeys(s: string);');
AddFunction(@isKeyDown, 'function IsKeyDown(key: Word): Boolean;');

{ OCR}
SetCurrSection('OCR');
AddFunction(@rs_GetUpText, 'function rs_GetUpText: string;');


{Bitmaps}
SetCurrSection('Bitmaps');
AddFunction(@GetMufasaBitmap,'function GetMufasaBitmap(bmp : integer) : TMufasaBitmap;');
AddFunction(@CreateBitmap,'function CreateBitmap(w,h :integer) : integer;');
AddFunction(@FreeBitmap,'procedure FreeBitmap(Bmp : integer);');
AddFunction(@SaveBitmap,'procedure SaveBitmap(Bmp : integer; path : string);');
AddFunction(@BitmapFromString,'function BitmapFromString(Width,Height : integer; Data : string): integer;');
AddFunction(@LoadBitmap,'function LoadBitmap(Path : string) : integer;');
AddFunction(@SetBitmapSize,'procedure SetBitmapSize(Bmp,NewW,NewH : integer);');
AddFunction(@GetBitmapSize,'procedure GetBitmapSize(Bmp : integer; out BmpW,BmpH : integer);');
AddFunction(@StretchBitmapResize,'procedure StretchBitmapResize(Bmp,NewW,NewH : integer);');
AddFunction(@CreateMirroredBitmap,'function CreateMirroredBitmap(Bmp : integer) : integer;');
AddFunction(@CreateMirroredBitmapEx,'function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;');
AddFunction(@FastSetPixel,'procedure FastSetPixel(bmp,x,y : integer; Color : TColor);');
AddFunction(@FastSetPixels,'procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);');
AddFunction(@FastGetPixel,'function FastGetPixel(bmp, x,y : integer) : TColor;');
AddFunction(@FastGetPixels,'function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;');
AddFunction(@FastDrawClear,'procedure FastDrawClear(bmp : integer; Color : TColor)');
AddFunction(@FastDrawTransparent,'procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);');
AddFunction(@SetTransparentColor,'procedure SetTransparentColor(bmp : integer; Color : TColor);');
AddFunction(@GetTransparentColor,'function GetTransparentColor(bmp: integer) : TColor;');
AddFunction(@FastReplaceColor,'procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);');
AddFunction(@ps_CopyClientToBitmap, 'procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer);');
AddFunction(@BitmapFromClient,'function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer;');
AddFunction(@SetBitmapName, 'procedure SetBitmapName(Bmp : integer; name : string);');
AddFunction(@FindBitmap,'function FindBitmap(bitmap: integer; out x, y: Integer): Boolean;');
AddFunction(@FindBitmapIn,'function FindBitmapIn(bitmap: integer; out x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@FindBitmapToleranceIn,'function FindBitmapToleranceIn(bitmap: integer; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;');
AddFunction(@FindBitmapSpiral,'function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@FindBitmapsSpiralTolerance,'function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; out Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;');
AddFunction(@FindBitmapSpiralTolerance,'function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;');
AddFunction(@RotateBitmap,'function RotateBitmap(bitmap: Integer; angle: Extended): Integer;');
AddFunction(@Desaturate,'function DesaturateBitmap(Bitmap : integer) : integer;');
AddFunction(@InvertBitmap,'procedure InvertBitmap(Bitmap : integer);');
AddFunction(@CopyBitmap,'function CopyBitmap(Bitmap:  integer) : integer)');
AddFunction(@GreyScaleBitmap,'function GreyScaleBitmap(bitmap : integer) : integer');
AddFunction(@BrightnessBitmap,'function BrightnessBitmap(Bitmap,br : integer) : integer;');
AddFunction(@ContrastBitmap,'function ContrastBitmap(bitmap : integer; co : extended) : integer;');
AddFunction(@PosterizeBitmap,'function PosterizeBitmap(Bitmap : integer; po : integer) : integer;');
AddFunction(@CreateBitmapMask,'function CreateBitmapMask(Bitmap : integer) : TMask;');
AddFunction(@FindMaskTolerance,'function FindMaskTolerance(mask: TMask; out x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
AddFunction(@FindBitmapMaskTolerance,'function FindBitmapMaskTolerance(mask: Integer; out x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
AddFunction(@FindDeformedBitmapToleranceIn,'function FindDeformedBitmapToleranceIn(bitmap: integer; out x,y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; out accuracy: Extended): Boolean;');
AddFunction(@DrawTPABitmap,'procedure DrawTPABitmap(bitmap: integer; TPA: TPointArray; Color: integer);');
AddFunction(@DrawATPABitmap,'procedure DrawATPABitmap(bitmap: integer; ATPA: T2DPointArray);');
AddFunction(@DrawATPABitmapEx,'procedure DrawATPABitmapEx(bitmap: integer; ATPA: T2DPointArray; Colors: TIntegerArray);');

{tpa}
SetCurrSection('TPA');
AddFunction(@tSwap,'procedure tSwap(var a, b: TPoint);');
AddFunction(@tpaSwap,'procedure tpaSwap(var a, b: TPointArray);');
AddFunction(@SwapE,'procedure SwapE(var a, b: Extended);');
AddFunction(@RAaSTPAEx,'procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);');
AddFunction(@RAaSTPA,'procedure RAaSTPA(var a: TPointArray; const Dist: Integer);');
AddFunction(@NearbyPointInArrayEx,'function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;');
AddFunction(@NearbyPointInArray,'function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;');
AddFunction(@ReArrangeandShortenArrayEx,'function ReArrangeandShortenArrayEx(a: TPointArray; w, h: Integer): TPointArray;');
AddFunction(@ReArrangeandShortenArray,'function ReArrangeandShortenArray(a: TPointArray; Dist: Integer): TPointArray;');
AddFunction(@TPAtoATPAEx,'function TPAtoATPAEx(TPA: TPointArray; w, h: Integer): T2DPointArray;');
AddFunction(@TPAtoATPA,'function TPAtoATPA(TPA: TPointArray; Dist: Integer): T2DPointArray;');
AddFunction(@QuickTPASort,'procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);');
AddFunction(@QuickATPASort,'procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);');
AddFunction(@SortTPAFrom,'procedure SortTPAFrom(var a: TPointArray; const From: TPoint);');
AddFunction(@SortATPAFrom,'procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);');
AddFunction(@SortATPAFromFirstPoint,'procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);');
AddFunction(@InvertTPA,'procedure InvertTPA(var a: TPointArray);');
AddFunction(@InvertATPA,'procedure InvertATPA(var a: T2DPointArray);');
AddFunction(@MiddleTPAEx,'function MiddleTPAEx(TPA: TPointArray; var x, y: Integer): Boolean;');
AddFunction(@MiddleTPA,'function MiddleTPA(tpa: TPointArray): TPoint;');
AddFunction(@SortATPASize,'procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);');
AddFunction(@SortATPAFromSize,'procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);');
AddFunction(@CombineTPA,'function CombineTPA(Ar1, Ar2: TPointArray): TPointArray;');
AddFunction(@CombineIntArray,'function CombineIntArray(Ar1, Ar2: TIntegerArray): TIntegerArray;');
AddFunction(@InIntArrayEx,'function InIntArrayEx(a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;');
AddFunction(@InIntArray,'function InIntArray(a: TIntegerArray; Number: Integer): Boolean;');
AddFunction(@ClearSameIntegers,'procedure ClearSameIntegers(var a: TIntegerArray);');
AddFunction(@ClearSameIntegersAndTPA,'procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);');
AddFunction(@SplitTPAEx,'function SplitTPAEx(arr: TPointArray; w, h: Integer): T2DPointArray;');
AddFunction(@SplitTPA,'function SplitTPA(arr: TPointArray; Dist: Integer): T2DPointArray;');
AddFunction(@FilterPointsPie,'procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);');
AddFunction(@RemoveDistTPointArray,'function RemoveDistTPointArray(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;');
AddFunction(@GetATPABounds,'function GetATPABounds(ATPA: T2DPointArray): TBox;');
AddFunction(@GetTPABounds,'function GetTPABounds(TPA: TPointArray): TBox;');
AddFunction(@FindTPAinTPA,'function FindTPAinTPA(SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;');
AddFunction(@FindTextTPAinTPA,'function FindTextTPAinTPA(Height : integer; SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;');
AddFunction(@FindGapsTPA,'function FindGapsTPA(TPA: TPointArray; MinPixels: Integer): T2DPointArray;');
AddFunction(@SortCircleWise,'procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);');
AddFunction(@LinearSort,'procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);');
AddFunction(@MergeATPA,'Function MergeATPA(ATPA : T2DPointArray)  : TPointArray;');
AddFunction(@TPAFromBox,'function TPAFromBox(const Box : TBox) : TPointArray;');
AddFunction(@RotatePoints,'Function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;');
AddFunction(@RotatePoint,'Function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint; inline;');
AddFunction(@FindTPAEdges,'function FindTPAEdges(p: TPointArray): TPointArray;');
AddFunction(@PointInTPA,'function PointInTPA(p: TPoint; arP: TPointArray): Boolean;');
AddFunction(@ClearTPAFromTPA,'function ClearTPAFromTPA(arP, ClearPoints: TPointArray): TPointArray;');
AddFunction(@ClearDoubleTPA,'procedure ClearDoubleTPA(var TPA: TPointArray);');
AddFunction(@ReturnPointsNotInTPA,'Function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;');
AddFunction(@TPACountSort,'Procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);');
AddFunction(@TPACountSortBase,'Procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);');
AddFunction(@InvertTIA,'procedure InvertTIA(var tI: TIntegerArray);');
AddFunction(@SumIntegerArray,'function SumIntegerArray(Ints : TIntegerArray): Integer;');
AddFunction(@AverageTIA,'function AverageTIA(tI: TIntegerArray): Integer;');
AddFunction(@AverageExtended,'function AverageExtended(tE: TExtendedArray): Extended;');
