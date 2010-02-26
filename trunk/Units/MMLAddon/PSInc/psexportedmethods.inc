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
AddFunction(@pswriteln,'procedure Writeln(x: string);'); //PS defines a special, keep this for CPascal

{ DTM }
SetCurrSection('DTM');
AddFunction(@ps_PrintpDTM, 'Procedure PrintpDTM(tDTM : pDTM);');
AddFunction(@ps_GetDTM ,'function GetDTM(index: Integer; out dtm: pDTM): Boolean;');
AddFunction(@pDTMToTDTM, 'Function pDTMToTDTM(DTM: pDTM): TDTM;');
AddFunction(@tDTMTopDTM, 'Function tDTMTopDTM(DTM: TDTM): pDTM;');
AddFunction(@ps_SetDTMName, 'procedure SetDTMName(DTM : integer; name : string);');
AddFunction(@ps_DTMFromString, 'function DTMFromString(DTMString: String): Integer;');
AddFunction(@ps_FreeDTM, 'procedure FreeDTM(DTM: Integer);');
AddFunction(@ps_FindDTM, 'function FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;');
AddFunction(@ps_FindDTMs, 'function FindDTMs(DTM: Integer; var p: TPointArray; x1, y1, x2, y2: Integer): Boolean;');
AddFunction(@ps_FindDTMRotated, 'function FindDTMRotated(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;');
AddFunction(@ps_FindDTMsRotated, 'function FindDTMsRotated(DTM: Integer; var Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;');
AddFunction(@ps_addDTM, 'function AddDTM(d: TDTM): Integer;');
AddFunction(@ps_addpDTM, 'function AddpDTM(d: pDTM): Integer;');


{maths}
SetCurrSection('Math');
AddFunction(nil,'function Round(e:extended) : integer');
AddFunction(@ps_ceil,'function ceil(e : extended) : integer');
AddFunction(@ps_floor,'function floor(e : extended) : integer');
AddFunction(@ps_pow,'function pow(base,exponent : extended) : extended');
AddFunction(@ps_max,'function Max(a, b: Integer): Integer;');
AddFunction(@ps_min,'function Min(a, b: Integer): Integer;');
AddFunction(@ps_minE,'function MinE(a, b: extended): Extended;');
AddFunction(@ps_maxE,'function MaxE(a, b: extended): Extended;');
AddFunction(@ps_iAbs,'function iAbs(a : integer) : integer;');
AddFunction(@ps_ArcTan2,'function ArcTan2(x,y : extended) : extended;');
AddFunction(@IntToBox,'function IntToBox(x1,y1,x2,y2 : integer) : TBox;');
AddFunction(@IntInBox,'function IntInBox(x, y: Integer; Box: TBox): Boolean;');
AddFunction(@PointToBox,'function PointToBox(PT1,PT2 : TPoint): TBox;');
AddFunction(@PointInBox,'function PointInBox(PT : TPoint; Box: TBox): Boolean;');
AddFunction(@ps_sqr,'function Sqr(e : extended) : extended;');
AddFunction(@ps_point,'function Point(x,y:integer) : TPoint;');
AddFunction(@ps_Distance,'function Distance(x1,y1,x2,y2 : integer) : integer;');
AddFunction(@ps_hypot,'function Hypot(X, Y: Extended): Extended;');
AddFunction(@ps_RandomRange,'function RandomRange(aFrom,aTo: Integer): Integer;');
AddFunction(@ps_incex,'procedure IncEx(var x : integer; increase : integer);');
AddFunction(@ps_DecEx,'procedure DecEx(var x : integer; Decrease : integer);');
AddFunction(@BinCoe,'function BinCoe(a, b: LongInt): Extended;');
AddFunction(@FixD,'function FixD(Degrees : extended) : Extended;');
AddFunction(@InRange,'function InRange(value,min,max : integer) : boolean;');

{window}
SetCurrSection('Window');
AddFunction(@Freeze, 'function Freeze: boolean;');
AddFunction(@Unfreeze, 'function Unfreeze: boolean;');
AddFunction(@GetClientDimensions, 'procedure GetClientDimensions(var w, h:integer);');
AddFunction(@SetTargetBitmap,'function SetTargetBitmap(Bitmap : integer): integer;');
AddFunction(@SetTargetArray, 'function SetTargetArray(P: Integer; w, h: integer): integer;');
AddFunction(@SetEIOSTarget, 'function SetEIOSTarget(name: string; initargs: Variant): integer;');
AddFunction(@SetImageTarget,'procedure SetImageTarget(idx: integer);');
AddFunction(@SetKeyMouseTarget,'procedure SetKeyMouseTarget(idx: integer);');
AddFunction(@GetImageTarget,'function GetImageTarget: integer;');
AddFunction(@GetKeyMouseTarget,'function GetKeyMouseTarget: integer;');
AddFunction(@ExportImageTarget,'function ExportImageTarget : TTarget_Exported;');
AddFunction(@ExportKeyMouseTarget,'function ExportKeyMouseTarget : TTarget_Exported;');
AddFunction(@FreeTarget,'procedure FreeTarget(idx: integer);');
AddFunction(@SetDesktopAsClient,'procedure SetDesktopAsClient;');
AddFunction(@ActivateClient, 'procedure ActivateClient;');
AddFunction(@IsTargetValid, 'function IsTargetValid: boolean;');

{files}
SetCurrSection('Files');
AddFunction(@ps_CreateFile, 'function CreateFile(Path: string): Integer;');
AddFunction(@ps_OpenFile, 'function OpenFile(Path: string; Shared: Boolean): Integer;');
AddFunction(@ps_RewriteFile, 'function RewriteFile(Path: string; Shared: Boolean): Integer;');
AddFunction(@ps_CloseFile, 'procedure CloseFile(FileNum: Integer);');
AddFunction(@ps_EndOfFile, 'function EndOfFile(FileNum: Integer): Boolean;');
AddFunction(@ps_FileSize, 'function FileSize(FileNum: Integer): LongInt;');
AddFunction(@ps_ReadFileString, 'function ReadFileString(FileNum: Integer; var s: string; x: Integer): Boolean;');
AddFunction(@ps_WriteFileString, 'function WriteFileString(FileNum: Integer; s: string): Boolean;');
AddFunction(@ps_SetFileCharPointer, 'Function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;');
AddFunction(@ps_FilePointerPos, 'function FilePointerPos(FileNum: Integer): Integer;');
AddFunction(@ps_DirectoryExists,'function DirectoryExists( const DirectoryName : string ) : Boolean;');
AddFunction(@ps_CreateDirectory,'function CreateDirectory( const DirectoryName : string) : boolean;');
AddFunction(@ps_FileExists,'function FileExists ( const FileName : string ) : Boolean;');
AddFunction(@ps_WriteINI,'procedure WriteINI(Section, KeyName, NewString, FileName: string);');
AddFunction(@ps_ReadINI,'function ReadINI(Section, KeyName, FileName: string): string;');
AddFunction(@ps_DeleteINI,'procedure DeleteINI(Section, KeyName, FileName: string);');

{other}
SetCurrSection('Other');
AddFunction(@SaveScreenshot,'procedure SaveScreenshot(FileName: string);');
AddFunction(@psWait, 'procedure wait(t: integer);');
AddFunction(@psWait, 'procedure Sleep(t: integer);');
AddFunction(@ps_now,'function Now: TDateTime;');
AddFunction(@ps_date,'function Date : TDateTime;');
AddFunction(@ps_GetTickCount, 'function GetSystemTime: LongWord;');
AddFunction(@ps_GetTickCount, 'function GetTickCount: LongWord;');
AddFunction(@GetTimeRunning,'function GetTimeRunning: LongWord;');
AddFunction(@ps_DecodeTime,'procedure DecodeTime(DateTime : TDateTime; var Hour,Min,Sec,MSec : word);');
AddFunction(@ps_DecodeDate,'procedure DecodeDate ( const SourceDate  : TDateTime; var Year, Month, Day  : Word );');
AddFunction(@ConvertTime,'procedure ConvertTime(Time: integer; var h, m, s: integer);');
AddFunction(@HakunaMatata,'procedure HakunaMatata;');
AddFunction(@TerminateScript,'procedure TerminateScript;');
AddFunction(@DisplayDebugImgWindow,'procedure DisplayDebugImgWindow(w, h: integer);');
AddFunction(@DrawBitmapDebugImg,'procedure DrawBitmapDebugImg(bmp: integer);');
AddFunction(@GetDebugBitmap,'function GetDebugBitmap: integer;');
AddFunction(@ps_Random,'function Random(Int: integer): integer;');
AddFunction(@ClearDebug,'procedure ClearDebug;');



{string}
SetCurrSection('String');
AddFunction(@ps_Capitalize,'function Capitalize(str : string) : string;');
AddFunction(@ps_Format,'function Format(const fmt : string;const args : array of const) : string;');
AddFunction(nil,'function ToStr(x) : string;');
AddFunction(@ps_Between,'function Between(s1, s2, str: string): string;');
AddFunction(@ps_IntToStr, 'function IntToStr(value: Integer): String;');
AddFunction(@ps_FloatToStr, 'function FloatToStr(value: Extended): String;');
AddFunction(@ps_BoolToStr, 'function BoolToStr(value: Boolean): String;');
AddFunction(@ps_StrToInt, 'function StrToInt(value: String): Integer;');
AddFunction(@ps_StrToIntDef, 'function StrToIntDef(value: String; default: Integer): Integer;');
AddFunction(@ps_StrToFloat, 'function StrToFloat(value: String): Extended;');
AddFunction(@ps_StrToFloatDef, 'function StrToFloatDef(value: String; default: Extended): Extended;');
AddFunction(@ps_StrToBool, 'function StrToBool(value: String): Boolean;');
AddFunction(@ps_StrToBoolDef, 'function StrToBoolDef(value: String; default: Boolean): Boolean;');
AddFunction(@ps_ExtractFromStr,'function ExtractFromStr( Str : string; Extract : StrExtr) : string;');
AddFunction(@ps_Replace,'function Replace(Text, FindStr, ReplaceStr: string; Flags: TReplaceFlags): string;');
AddFunction(@ps_Replace,'function ReplaceWrap(Text, FindStr, ReplaceStr: string; Flags: TReplaceFlags): string;');
AddFunction(@ps_Implode,'function Implode(Glue: string; Pieces: TStringArray): string;');
AddFunction(@ps_Explode,'function Explode(del, str: string): TStringArray;');
AddFunction(@ps_explodewrap,'procedure ExplodeWrap(del, str: string; var res : TStringArray);');

{web}
SetCurrSection('Web');
AddFunction(@OpenWebPage,'procedure OpenWebPage(url : string);');
AddFunction(@ps_GetPage,'function GetPage(url : string): string;');
AddFunction(@ps_InitializeHTTPClient,'function InitializeHTTPClient(HandleCookies, HandleRedirects: Boolean): Integer;');
AddFunction(@ps_FreeHTTPClient,'procedure FreeHTTPClient(Client: Integer);');
AddFunction(@ps_GetHTTPPage,'function GetHTTPPage(Client: Integer; URL: string): string;');
AddFunction(@ps_SetHTTPUserAgent,'procedure SetHTTPUserAgent(Client: Integer; Agent: string);');
AddFunction(@ps_PostHTTPPage,'function PostHTTPPage(Client: Integer; Url: string; PostData: string): string;');
AddFunction(@ps_PostHTTPPageEx,'function PostHTTPPageEx(Client: Integer; Url: string): string;');
AddFunction(@ps_ClearPostData,'procedure ClearPostData(Client: Integer);');
AddFunction(@ps_AddPostVariable,'procedure AddPostVariable(Client: Integer; VarName, VarValue: string);');
AddFunction(@ps_GetRawHeaders,'function GetRawHeaders(Client: Integer): string;');



{ Color Conversions and Speed }
SetCurrSection('Color Convert');
AddFunction(@Colour_Conv.ColorToRGB, 'procedure ColorToRGB(Color: integer; var r, g, b: Byte);');
AddFunction(@Colour_conv.RGBToColor, 'function RGBtoColor(r, g, b: Byte): TColor;');
AddFunction(@Colour_conv.ColorToHSL, 'procedure ColorToHSL(Color: Integer; var h, s, l: Extended);');
AddFunction(@Colour_conv.HSLToColor, 'function HSLToColor(H, S, L: Extended): TColor;');
AddFunction(@Colour_conv.ColorToXYZ, 'procedure ColorToXYZ(Color: Integer; var x, y, z: Extended);');
AddFunction(@Colour_conv.XYZToColor, 'function XYZToColor(X, Y, Z: Extended): TColor;');
AddFunction(@Colour_conv.RGBToHSL, 'procedure RGBToHSL(R, G, B: Byte; var h, s, l: Extended);');
AddFunction(@Colour_conv.HSLToRGB, 'procedure HSLtoRGB(H, S, L: extended; var R, G ,B: Byte);');
AddFunction(@Colour_conv.RGBToXYZ, 'procedure RGBToXYZ(R, G, B: Byte;var x, y ,z: Extended);');
AddFunction(@Colour_conv.XYZToRGB, 'procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Byte);');

{ Color Finding }
SetCurrSection('Color');
AddFunction(@SetColorToleranceSpeed, 'procedure SetColorToleranceSpeed(cts: integer);');
AddFunction(@GetToleranceSpeed, 'function GetToleranceSpeed: Integer;');
AddFunction(@SetToleranceSpeed2Modifiers, 'procedure SetToleranceSpeed2Modifiers(nHue, nSat: Extended);');
AddFunction(@GetToleranceSpeed2Modifiers, 'procedure GetToleranceSpeed2Modifiers(var hMod, sMod: Extended);');
AddFunction(@GetColor,'function GetColor(x, y: Integer): Integer;');
AddFunction(@GetColors,'function GetColors(Coords : TPointArray) : TIntegerArray;');
AddFunction(@GetColorsWrap,'procedure GetColorsWrap(Coords : TPointArray; var Colors :TIntegerArray);');
AddFunction(@FindColor, 'function FindColor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
AddFunction(@findcolortoleranceOptimised, 'function FindColorToleranceOptimised(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
AddFunction(@FindColorTolerance, 'function FindColorTolerance(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
AddFunction(@FindColors, 'function FindColors(var TPA: TPointArray; color, x1, y1, x2, y2: integer): boolean;');
AddFunction(@SimilarColors,'function SimilarColors(Col1, Col2, Tolerance: integer): boolean');
AddFunction(@CountColor,'function CountColor(Color, xs, ys, xe, ye: Integer): Integer;');
AddFunction(@CountColorTolerance,'function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;');
AddFunction(@FindColorsToleranceOptimised,'function FindColorsToleranceOptimised(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
AddFunction(@FindColorsTolerance,'function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
AddFunction(@FindColorSpiral,'function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@FindColorSpiralTolerance,'function FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye,Tol: Integer): Boolean;');
AddFunction(@FindColorsSpiralTolerance,'function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;');
AddFunction(@FindColoredArea, 'function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye, MinArea: Integer): Boolean');
AddFunction(@FindColoredAreaTolerance, 'function FindColoredAreaTolerance(var x, y : Integer; color, xs, ys, xe, ye, MinArea, Tolerance : Integer): Boolean');

{Mouse etc.}
SetCurrSection('Mouse');
AddFunction(@MoveMouse, 'procedure MoveMouse(x, y: integer);');
AddFunction(@GetMousePos, 'procedure GetMousePos(var x, y: integer);');
AddFunction(@HoldMouse, 'procedure HoldMouse(x, y: integer; clickType: integer);');
AddFunction(@ReleaseMouse, 'procedure ReleaseMouse(x, y: integer; clickType: integer);');
AddFunction(@ClickMouse, 'procedure ClickMouse(x, y: integer; clickType: integer);');
AddFunction(@IsMouseButtonDown,'function IsMouseButtonDown( button : integer) : boolean;');

{Keyboard}
SetCurrSection('Keyboard');
AddFunction(@KeyDown, 'procedure KeyDown(key: Word);');
AddFunction(@KeyUp, 'procedure KeyUp(key: Word);');
AddFunction(@PressKey, 'procedure PressKey(key: Word);');
AddFunction(@SendKeys, 'procedure SendKeys(s: string);');
AddFunction(@isKeyDown, 'function IsKeyDown(key: Word): Boolean;');
AddFunction(@GetKeyCode,'function GetKeyCode(c : char) : integer;');

{ OCR}
SetCurrSection('OCR');
AddFunction(@rs_GetUpText, 'function rs_GetUpText: string;');
AddFunction(@rs_GetUpTextAt, 'function rs_GetUpTextAt(x, y : integer): string;');
AddFunction(@BitmapFromText, 'function BitmapFromText(text, font: String): integer;');
AddFunction(@TPAFromText, 'function TPAFromText(text, font: String;var w,h : integer): TPointArray;');
AddFunction(@TPAFromTextWrap,'procedure TPAFromTextWrap(text, font: String;var w,h : integer;var TPA : TPointArray);');
AddFunction(@MaskFromText, 'function MaskFromText(text, font: String): TMask;');
AddFunction(@GetTextAt,'function GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing,color, tol, len: integer; font: string): string;');

{Bitmaps}
SetCurrSection('Bitmaps');
AddFunction(@CreateBitmapString,'function CreateBitmapString(bmp : integer) : string;');
AddFunction(@GetMufasaBitmap,'function GetMufasaBitmap(bmp : integer) : TMufasaBitmap;');
AddFunction(@CreateBitmap,'function CreateBitmap(w,h :integer) : integer;');
AddFunction(@FreeBitmap,'procedure FreeBitmap(Bmp : integer);');
AddFunction(@SaveBitmap,'procedure SaveBitmap(Bmp : integer; path : string);');
AddFunction(@BitmapFromString,'function BitmapFromString(Width,Height : integer; Data : string): integer;');
AddFunction(@LoadBitmap,'function LoadBitmap(Path : string) : integer;');
AddFunction(@SetBitmapSize,'procedure SetBitmapSize(Bmp,NewW,NewH : integer);');
AddFunction(@GetBitmapSize,'procedure GetBitmapSize(Bmp : integer; var BmpW,BmpH : integer);');
AddFunction(@StretchBitmapResize,'procedure StretchBitmapResize(Bmp,NewW,NewH : integer);');
AddFunction(@CreateMirroredBitmap,'function CreateMirroredBitmap(Bmp : integer) : integer;');
AddFunction(@CreateMirroredBitmapEx,'function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;');
AddFunction(@FastSetPixel,'procedure FastSetPixel(bmp,x,y : integer; Color : TColor);');
AddFunction(@FastSetPixels,'procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);');
AddFunction(@FastGetPixel,'function FastGetPixel(bmp, x,y : integer) : TColor;');
AddFunction(@FastGetPixels,'function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;');
AddFunction(@FastDrawClear,'procedure FastDrawClear(bmp : integer; Color : TColor);');
AddFunction(@FastDrawTransparent,'procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);');
AddFunction(@SetTransparentColor,'procedure SetTransparentColor(bmp : integer; Color : TColor);');
AddFunction(@GetTransparentColor,'function GetTransparentColor(bmp: integer) : TColor;');
AddFunction(@FastReplaceColor,'procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);');
AddFunction(@ps_CopyClientToBitmap, 'procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer);');
AddFunction(@BitmapFromClient,'function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer;');
AddFunction(@SetBitmapName, 'procedure SetBitmapName(Bmp : integer; name : string);');
AddFunction(@FindBitmap,'function FindBitmap(bitmap: integer; var x, y: Integer): Boolean;');
AddFunction(@FindBitmapIn,'function FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@FindBitmapToleranceIn,'function FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;');
AddFunction(@FindBitmapSpiral,'function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@FindBitmapsSpiralTolerance,'function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;');
AddFunction(@FindBitmapSpiralTolerance,'function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;');
AddFunction(@RotateBitmap,'function RotateBitmap(bitmap: Integer; angle: Extended): Integer;');
AddFunction(@Desaturate,'function DesaturateBitmap(Bitmap : integer) : integer;');
AddFunction(@InvertBitmap,'procedure InvertBitmap(Bitmap : integer);');
AddFunction(@CopyBitmap,'function CopyBitmap(Bitmap:  integer) : integer)');
AddFunction(@GreyScaleBitmap,'function GreyScaleBitmap(bitmap : integer) : integer');
AddFunction(@BrightnessBitmap,'function BrightnessBitmap(Bitmap,br : integer) : integer;');
AddFunction(@ContrastBitmap,'function ContrastBitmap(bitmap : integer; co : extended) : integer;');
AddFunction(@PosterizeBitmap,'function PosterizeBitmap(Bitmap : integer; po : integer) : integer;');
AddFunction(@CreateMaskFromBitmap,'function CreateMaskFromBitmap(Bitmap : integer) : TMask;');
AddFunction(@FindMaskTolerance,'function FindMaskTolerance(mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
AddFunction(@FindBitmapMaskTolerance,'function FindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
AddFunction(@FindDeformedBitmapToleranceIn,'function FindDeformedBitmapToleranceIn(bitmap: integer; var x,y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;');
AddFunction(@DrawTPABitmap,'procedure DrawTPABitmap(bitmap: integer; TPA: TPointArray; Color: integer);');
AddFunction(@DrawATPABitmap,'procedure DrawATPABitmap(bitmap: integer; ATPA: T2DPointArray);');
AddFunction(@DrawATPABitmapEx,'procedure DrawATPABitmapEx(bitmap: integer; ATPA: T2DPointArray; Colors: TIntegerArray);');
AddFunction(@DrawBitmap,'procedure DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);');

{tpa}
SetCurrSection('TPA');
AddFunction(@ps_Quicksort,'procedure Quicksort(var Arr : TIntegerArray);');
AddFunction(@tSwap,'procedure tSwap(var a, b: TPoint);');
AddFunction(@tpaSwap,'procedure tpaSwap(var a, b: TPointArray);');
AddFunction(@SwapE,'procedure SwapE(var a, b: Extended);');
AddFunction(@RAaSTPAEx,'procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);');
AddFunction(@RAaSTPA,'procedure RAaSTPA(var a: TPointArray; const Dist: Integer);');
AddFunction(@NearbyPointInArrayEx,'function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;');
AddFunction(@NearbyPointInArray,'function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;');
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
AddFunction(@InIntArrayEx,'function InIntArrayEx(a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;');
AddFunction(@InIntArray,'function InIntArray(a: TIntegerArray; Number: Integer): Boolean;');
AddFunction(@ClearSameIntegers,'procedure ClearSameIntegers(var a: TIntegerArray);');
AddFunction(@ClearSameIntegersAndTPA,'procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);');
AddFunction(@SplitTPAEx,'function SplitTPAEx(arr: TPointArray; w, h: Integer): T2DPointArray;');
AddFunction(@SplitTPA,'function SplitTPA(arr: TPointArray; Dist: Integer): T2DPointArray;');
AddFunction(@FilterPointsPie,'procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);');
AddFunction(@GetATPABounds,'function GetATPABounds(ATPA: T2DPointArray): TBox;');
AddFunction(@GetTPABounds,'function GetTPABounds(TPA: TPointArray): TBox;');
AddFunction(@FindTPAinTPA,'function FindTPAinTPA(SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;');
AddFunction(@FindTextTPAinTPA,'function FindTextTPAinTPA(Height : integer; SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;');
AddFunction(@SortCircleWise,'procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);');
AddFunction(@LinearSort,'procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);');
AddFunction(@RotatePoint,'Function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;');
AddFunction(@FindGapsTPA,'function FindGapsTPA(TPA: TPointArray; MinPixels: Integer): T2DPointArray;');
AddFunction(@RemoveDistTPointArray,'function RemoveDistTPointArray(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;');
AddFunction(@CombineTPA,'function CombineTPA(Ar1, Ar2: TPointArray): TPointArray;');
AddFunction(@ReArrangeandShortenArrayEx,'function ReArrangeandShortenArrayEx(a: TPointArray; w, h: Integer): TPointArray;');
AddFunction(@ReArrangeandShortenArray,'function ReArrangeandShortenArray(a: TPointArray; Dist: Integer): TPointArray;');
AddFunction(@TPAtoATPAEx,'function TPAtoATPAEx(TPA: TPointArray; w, h: Integer): T2DPointArray;');
AddFunction(@TPAtoATPA,'function TPAtoATPA(TPA: TPointArray; Dist: Integer): T2DPointArray;');
AddFunction(@CombineIntArray,'function CombineIntArray(Ar1, Ar2: TIntegerArray): TIntegerArray;');
AddFunction(@MergeATPA,'Function MergeATPA(ATPA : T2DPointArray)  : TPointArray;');
AddFunction(@TPAFromBox,'function TPAFromBox(const Box : TBox) : TPointArray;');
AddFunction(@RotatePoints,'Function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;');
AddFunction(@FindTPAEdges,'function FindTPAEdges(p: TPointArray): TPointArray;');
AddFunction(@ClearTPAFromTPA,'function ClearTPAFromTPA(arP, ClearPoints: TPointArray): TPointArray;');
AddFunction(@ReturnPointsNotInTPA,'Function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;');
AddFunction(@PointInTPA,'function PointInTPA(p: TPoint; arP: TPointArray): Boolean;');
AddFunction(@ClearDoubleTPA,'procedure ClearDoubleTPA(var TPA: TPointArray);');
AddFunction(@TPACountSort,'Procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);');
AddFunction(@TPACountSortBase,'Procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);');
AddFunction(@InvertTIA,'procedure InvertTIA(var tI: TIntegerArray);');
AddFunction(@SumIntegerArray,'function SumIntegerArray(Ints : TIntegerArray): Integer;');
AddFunction(@AverageTIA,'function AverageTIA(tI: TIntegerArray): Integer;');
AddFunction(@AverageExtended,'function AverageExtended(tE: TExtendedArray): Extended;');
AddFunction(@SplitTPAExWrap,'procedure SplitTPAExWrap(arr: TPointArray; w, h: Integer; var res : T2DPointArray);');
AddFunction(@SplitTPAWrap,'procedure SplitTPAWrap(arr: TPointArray; Dist: Integer; var res: T2DPointArray);');
AddFunction(@FindGapsTPAWrap,'procedure FindGapsTPAWrap(TPA: TPointArray; MinPixels: Integer; var Res : T2DPointArray);');
AddFunction(@RemoveDistTPointArrayWrap,'procedure RemoveDistTPointArrayWrap(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean; var Res :  TPointArray);');
AddFunction(@CombineTPAWrap,'procedure CombineTPAWrap(Ar1, Ar2: TPointArray; var Res :  TPointArray);');
AddFunction(@ReArrangeandShortenArrayExWrap,'procedure ReArrangeandShortenArrayExWrap(a: TPointArray; w, h: Integer; var Res :  TPointArray);');
AddFunction(@ReArrangeandShortenArrayWrap,'procedure ReArrangeandShortenArrayWrap(a: TPointArray; Dist: Integer; var Res :  TPointArray);');
AddFunction(@TPAtoATPAExWrap,'procedure TPAtoATPAExWrap(TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray);');
AddFunction(@TPAtoATPAWrap,'procedure TPAtoATPAWrap(TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray);');
AddFunction(@CombineIntArrayWrap, 'procedure CombineIntArrayWrap(Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray);');
AddFunction(@ReturnPointsNotInTPAWrap,'procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray);');
AddFunction(@MergeATPAWrap,'procedure MergeATPAWrap(ATPA : T2DPointArray; var Res: TPointArray);');
AddFunction(@TPAFromBoxWrap,'procedure TPAFromBoxWrap(const Box : TBox; var Res : TPointArray);');
AddFunction(@RotatePointsWrap,'procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray);');
AddFunction(@FindTPAEdgesWrap,'procedure FindTPAEdgesWrap(p: TPointArray; var Res :  TPointArray);');
AddFunction(@ClearTPAFromTPAWrap,'procedure ClearTPAFromTPAWrap(arP, ClearPoints: TPointArray;  var Res :  TPointArray);');
AddFunction(@FilterPointsLine,'procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);');
AddFunction(@SameTPA,'function SameTPA(aTPA, bTPA: TPointArray): Boolean;');
AddFunction(@TPAInATPA,'function TPAInATPA(TPA: TPointArray; InATPA: T2DPointArray; var Index: LongInt): Boolean;');
