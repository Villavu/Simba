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
AddFunction(@ps_SetDTMName, 'procedure SetDTMName(DTM : integer;const name : string);');
AddFunction(@ps_DTMFromString, 'function DTMFromString(const DTMString: String): Integer;');
AddFunction(@ps_FreeDTM, 'procedure FreeDTM(DTM: Integer);');
AddFunction(@ps_FindDTM, 'function FindDTM(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@ps_FindDTMs, 'function FindDTMs(DTM: Integer; var p: TPointArray; xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@ps_FindDTMRotatedSE, 'function FindDTMRotatedSE(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;');
AddFunction(@ps_FindDTMRotatedAlternating, 'function FindDTMRotatedAlternating(DTM: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;');
AddFunction(@ps_FindDTMsRotatedSE, 'function FindDTMsRotatedSE(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;');
AddFunction(@ps_FindDTMsRotatedAlternating, 'function FindDTMsRotatedAlternating(DTM: Integer; var Points: TPointArray; xs, ys, xe, ye: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;');
AddFunction(@ps_addDTM, 'function AddDTM(const d: TDTM): Integer;');
AddFunction(@ps_addpDTM, 'function AddpDTM(const d: pDTM): Integer;');
AddFunction(@ps_PrintpDTM, 'procedure PrintpDTM(const tDTM : pDTM);');
AddFunction(@ps_GetDTM ,'function GetDTM(index: Integer) : pDTM');
AddFunction(@ps_pDTMToTDTM, 'function pDTMToTDTM(const DTM: pDTM): TDTM;');
AddFunction(@ps_tDTMTopDTM, 'function tDTMTopDTM(const DTM: TDTM): pDTM;');

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
AddFunction(@ps_IntToBox,'function IntToBox(xs,ys,xe,ye : integer) : TBox;');
AddFunction(@ps_IntInBox,'function IntInBox(x, y: Integer; Box: TBox): Boolean;');
AddFunction(@ps_PointToBox,'function PointToBox(PT1,PT2 : TPoint): TBox;');
AddFunction(@ps_PointInBox,'function PointInBox(PT : TPoint; Box: TBox): Boolean;');
AddFunction(@ps_sqr,'function Sqr(e : extended) : extended;');
AddFunction(@ps_point,'function Point(x,y:integer) : TPoint;');
AddFunction(@ps_Distance,'function Distance(xs,ys,xe,ye : integer) : integer;');
AddFunction(@ps_hypot,'function Hypot(X, Y: Extended): Extended;');
AddFunction(@ps_RandomRange,'function RandomRange(aFrom,aTo: Integer): Integer;');
AddFunction(@ps_incex,'procedure IncEx(var x : integer; increase : integer);');
AddFunction(@ps_DecEx,'procedure DecEx(var x : integer; Decrease : integer);');
AddFunction(@ps_BinCoe,'function BinCoe(a, b: LongInt): Extended;');
AddFunction(@ps_FixD,'function FixD(Degrees : extended) : Extended;');
AddFunction(@ps_InRange,'function InRange(const value,min,max : integer) : boolean;');

{window}
SetCurrSection('Window');
AddFunction(@ps_Freeze, 'function Freeze: boolean;');
AddFunction(@ps_Unfreeze, 'function Unfreeze: boolean;');
AddFunction(@ps_GetClientDimensions, 'procedure GetClientDimensions(var w, h:integer);');
AddFunction(@ps_SetTargetBitmap,'function SetTargetBitmap(Bitmap : integer): integer;');
AddFunction(@ps_SetTargetArray, 'function SetTargetArray(P: Integer; w, h: integer): integer;');
AddFunction(@ps_SetEIOSTarget, 'function SetEIOSTarget(name: string; initargs: Variant): integer;');
AddFunction(@ps_SetImageTarget,'procedure SetImageTarget(idx: integer);');
AddFunction(@ps_SetKeyMouseTarget,'procedure SetKeyMouseTarget(idx: integer);');
AddFunction(@ps_GetImageTarget,'function GetImageTarget: integer;');
AddFunction(@ps_GetKeyMouseTarget,'function GetKeyMouseTarget: integer;');
AddFunction(@ps_ExportImageTarget,'function ExportImageTarget : TTarget_Exported;');
AddFunction(@ps_ExportKeyMouseTarget,'function ExportKeyMouseTarget : TTarget_Exported;');
AddFunction(@ps_FreeTarget,'procedure FreeTarget(idx: integer);');
AddFunction(@ps_SetDesktopAsClient,'procedure SetDesktopAsClient;');
AddFunction(@ps_ActivateClient, 'procedure ActivateClient;');
AddFunction(@ps_IsTargetValid, 'function IsTargetValid: boolean;');

{files}
SetCurrSection('Files');
AddFunction(@ps_CreateFile, 'function CreateFile(const Path: string): Integer;');
AddFunction(@ps_OpenFile, 'function OpenFile(const Path: string; Shared: Boolean): Integer;');
AddFunction(@ps_RewriteFile, 'function RewriteFile(const Path: string; Shared: Boolean): Integer;');
AddFunction(@ps_CloseFile, 'procedure CloseFile(FileNum: Integer);');
AddFunction(@ps_EndOfFile, 'function EndOfFile(FileNum: Integer): Boolean;');
AddFunction(@ps_FileSize, 'function FileSize(FileNum: Integer): LongInt;');
AddFunction(@ps_ReadFileString, 'function ReadFileString(FileNum: Integer; var s: string; x: Integer): Boolean;');
AddFunction(@ps_WriteFileString, 'function WriteFileString(FileNum: Integer; s: string): Boolean;');
AddFunction(@ps_SetFileCharPointer, 'function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;');
AddFunction(@ps_FilePointerPos, 'function FilePointerPos(FileNum: Integer): Integer;');
AddFunction(@ps_DirectoryExists,'function DirectoryExists(const DirectoryName : string ) : Boolean;');
AddFunction(@ps_CreateDirectory,'function CreateDirectory(const DirectoryName : string) : boolean;');
AddFunction(@ps_FileExists,'function FileExists (const FileName : string ) : Boolean;');
AddFunction(@ps_WriteINI,'procedure WriteINI(const Section, KeyName, NewString, FileName: string);');
AddFunction(@ps_ReadINI,'function ReadINI(const Section, KeyName, FileName: string): string;');
AddFunction(@ps_DeleteINI,'procedure DeleteINI(const Section, KeyName, FileName: string);');

{other}
SetCurrSection('Other');
AddFunction(@ps_SaveScreenshot,'procedure SaveScreenshot(FileName: string);');
AddFunction(@ps_Wait, 'procedure wait(t: integer);');
AddFunction(@ps_Wait, 'procedure Sleep(t: integer);');
AddFunction(@ps_now,'function Now: TDateTime;');
AddFunction(@ps_date,'function Date : TDateTime;');
AddFunction(@ps_GetTickCount, 'function GetSystemTime: LongWord;');
AddFunction(@ps_GetTickCount, 'function GetTickCount: LongWord;');
AddFunction(@ps_GetTimeRunning,'function GetTimeRunning: LongWord;');
AddFunction(@ps_DecodeTime,'procedure DecodeTime(DateTime : TDateTime; var Hour,Min,Sec,MSec : word);');
AddFunction(@ps_DecodeDate,'procedure DecodeDate ( const SourceDate  : TDateTime; var Year, Month, Day  : Word );');
AddFunction(@ps_ConvertTime,'procedure ConvertTime(Time: integer; var h, m, s: integer);');
AddFunction(@ps_HakunaMatata,'procedure HakunaMatata;');
AddFunction(@ps_TerminateScript,'procedure TerminateScript;');
AddFunction(@ps_DisplayDebugImgWindow,'procedure DisplayDebugImgWindow(w, h: integer);');
AddFunction(@ps_DrawBitmapDebugImg,'procedure DrawBitmapDebugImg(bmp: integer);');
AddFunction(@ps_GetDebugBitmap,'function GetDebugBitmap: integer;');
AddFunction(@ps_Random,'function Random(Int: integer): integer;');
AddFunction(@ps_ClearDebug,'procedure ClearDebug;');
AddFunction(@ps_PlaySound,'procedure PlaySound( Sound : string);');
AddFunction(@ps_StopSound,'procedure StopSound;');
AddFunction(@ps_SetScriptProp, 'function SetScriptProp(Prop : TSP_Property; Value: String): boolean;');
AddFunction(@ps_GetScriptProp, 'function GetScriptProp(Prop : TSP_Property;var Value: String): boolean;');



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
AddFunction(@ps_PadL,'function Padl(s: String; i: longInt): String;');
AddFunction(@Ps_Padz,'function Padz(s: String; i: longInt): String;');
AddFunction(@ps_padR,'function Padr(s: String; i: longInt): String;');

{web}
SetCurrSection('Web');
AddFunction(@ps_OpenWebPage,'procedure OpenWebPage(const url : string);');
AddFunction(@ps_GetPage,'function GetPage(const url : string): string;');
AddFunction(@ps_InitializeHTTPClient,'function InitializeHTTPClient(HandleCookies: Boolean): Integer;');
AddFunction(@ps_InitializeHTTPClient,'function InitializeHTTPClientWrap(HandleCookies: Boolean): Integer;');
AddFunction(@ps_FreeHTTPClient,'procedure FreeHTTPClient(Client: Integer);');
AddFunction(@ps_GetHTTPPage,'function GetHTTPPage(Client: Integer;const URL: string): string;');
AddFunction(@ps_SetHTTPUserAgent,'procedure SetHTTPUserAgent(Client: Integer;const Agent: string);');
AddFunction(@ps_PostHTTPPage,'function PostHTTPPage(Client: Integer;const Url,PostData: string): string;');
AddFunction(@ps_PostHTTPPageEx,'function PostHTTPPageEx(Client: Integer;const Url: string): string;');
AddFunction(@ps_ClearPostData,'procedure ClearPostData(Client: Integer);');
AddFunction(@ps_AddPostVariable,'procedure AddPostVariable(Client: Integer;const VarName, VarValue: string);');
AddFunction(@ps_GetRawHeaders,'function GetRawHeaders(Client: Integer): string;');



{ Color Conversions and Speed }
SetCurrSection('Color Convert');
AddFunction(@ps_ColorToRGB, 'procedure ColorToRGB(Color: integer; var r, g, b: Byte);');
AddFunction(@ps_RGBToColor, 'function RGBtoColor(r, g, b: Byte): TColor;');
AddFunction(@ps_ColorToHSL, 'procedure ColorToHSL(Color: Integer; var h, s, l: Extended);');
AddFunction(@ps_HSLToColor, 'function HSLToColor(H, S, L: Extended): TColor;');
AddFunction(@ps_ColorToXYZ, 'procedure ColorToXYZ(Color: Integer; var x, y, z: Extended);');
AddFunction(@ps_XYZToColor, 'function XYZToColor(X, Y, Z: Extended): TColor;');
AddFunction(@ps_RGBToHSL, 'procedure RGBToHSL(R, G, B: Byte; var h, s, l: Extended);');
AddFunction(@ps_HSLToRGB, 'procedure HSLtoRGB(H, S, L: extended; var R, G ,B: Byte);');
AddFunction(@ps_RGBToXYZ, 'procedure RGBToXYZ(R, G, B: Byte;var x, y ,z: Extended);');
AddFunction(@ps_XYZToRGB, 'procedure XYZToRGB(X, Y, Z: Extended; var R, G, B: Byte);');

{ Color Finding }
SetCurrSection('Color');
AddFunction(@ps_SetColorToleranceSpeed, 'procedure SetColorToleranceSpeed(cts: integer);');
AddFunction(@ps_GetToleranceSpeed, 'function GetToleranceSpeed: Integer;');
AddFunction(@ps_SetToleranceSpeed2Modifiers, 'procedure SetToleranceSpeed2Modifiers(nHue, nSat: Extended);');
AddFunction(@ps_GetToleranceSpeed2Modifiers, 'procedure GetToleranceSpeed2Modifiers(var hMod, sMod: Extended);');
AddFunction(@ps_GetColor,'function GetColor(x, y: Integer): Integer;');
AddFunction(@ps_GetColors,'function GetColors(const Coords : TPointArray) : TIntegerArray;');
AddFunction(@ps_GetColorsWrap,'procedure GetColorsWrap(Coords : TPointArray; var Colors :TIntegerArray);');
AddFunction(@ps_FindColor, 'function FindColor(var x, y: integer; color, xs, ys, xe, ye: integer): boolean;');
AddFunction(@ps_findcolortoleranceOptimised, 'function FindColorToleranceOptimised(var x, y: integer; color, xs, ys, xe, ye, tol: integer): boolean;');
AddFunction(@ps_FindColorTolerance, 'function FindColorTolerance(var x, y: integer; color, xs, ys, xe, ye, tol: integer): boolean;');
AddFunction(@ps_FindColors, 'function FindColors(var TPA: TPointArray; color, xs, ys, xe, ye: integer): boolean;');
AddFunction(@ps_SimilarColors,'function SimilarColors(Col1, Col2, Tolerance: integer): boolean');
AddFunction(@ps_CountColor,'function CountColor(Color, xs, ys, xe, ye: Integer): Integer;');
AddFunction(@ps_CountColorTolerance,'function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;');
AddFunction(@ps_FindColorsToleranceOptimised,'function FindColorsToleranceOptimised(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
AddFunction(@ps_FindColorsTolerance,'function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
AddFunction(@ps_FindColorSpiral,'function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@ps_FindColorSpiralTolerance,'function FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye,Tol: Integer): Boolean;');
AddFunction(@ps_FindColorsSpiralTolerance,'function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;');
AddFunction(@ps_FindColoredArea, 'function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye, MinArea: Integer): Boolean');
AddFunction(@ps_FindColoredAreaTolerance, 'function FindColoredAreaTolerance(var x, y : Integer; color, xs, ys, xe, ye, MinArea, Tolerance : Integer): Boolean');

{Mouse etc.}
SetCurrSection('Mouse');
AddFunction(@ps_MoveMouse, 'procedure MoveMouse(x, y: integer);');
AddFunction(@ps_GetMousePos, 'procedure GetMousePos(var x, y: integer);');
AddFunction(@ps_HoldMouse, 'procedure HoldMouse(x, y: integer; clickType: integer);');
AddFunction(@ps_ReleaseMouse, 'procedure ReleaseMouse(x, y: integer; clickType: integer);');
AddFunction(@ps_ClickMouse, 'procedure ClickMouse(x, y: integer; clickType: integer);');
AddFunction(@ps_IsMouseButtonDown,'function IsMouseButtonDown( button : integer) : boolean;');

{Keyboard}
SetCurrSection('Keyboard');
AddFunction(@ps_KeyDown, 'procedure KeyDown(key: Word);');
AddFunction(@ps_KeyUp, 'procedure KeyUp(key: Word);');
AddFunction(@ps_PressKey, 'procedure PressKey(key: Word);');
AddFunction(@ps_SendKeys, 'procedure SendKeys(const s: string);');
AddFunction(@ps_isKeyDown, 'function IsKeyDown(key: Word): Boolean;');
AddFunction(@ps_GetKeyCode,'function GetKeyCode(c : char) : integer;');

{ OCR}
SetCurrSection('OCR');
AddFunction(@ps_rs_GetUpText, 'function rs_GetUpText: string;');
AddFunction(@ps_rs_GetUpTextAt, 'function rs_GetUpTextAt(x, y : integer): string;');
AddFunction(@ps_BitmapFromText, 'function BitmapFromText(const text, font: String): integer;');
AddFunction(@ps_TPAFromText, 'function TPAFromText(const text, font: String;var w,h : integer): TPointArray;');
AddFunction(@ps_TPAFromTextWrap,'procedure TPAFromTextWrap(const text, font: String;var w,h : integer;var TPA : TPointArray);');
AddFunction(@ps_MaskFromText, 'function MaskFromText(const text, font: String): TMask;');
AddFunction(@ps_GetTextAt,'function GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing,color, tol, len: integer;const font: string): string;');
AddFunction(@ps_LoadSystemFont,'function LoadSystemFont(const SysFont : TFont; const FontName : string) : boolean;');

{Bitmaps}
SetCurrSection('Bitmaps');
AddFunction(@ps_CreateBitmapString,'function CreateBitmapString(bmp : integer) : string;');
AddFunction(@ps_GetMufasaBitmap,'function GetMufasaBitmap(bmp : integer) : TMufasaBitmap;');
AddFunction(@ps_CreateBitmap,'function CreateBitmap(w,h :integer) : integer;');
AddFunction(@ps_FreeBitmap,'procedure FreeBitmap(Bmp : integer);');
AddFunction(@ps_SaveBitmap,'procedure SaveBitmap(Bmp : integer; path : string);');
AddFunction(@ps_BitmapFromString,'function BitmapFromString(Width,Height : integer; Data : string): integer;');
AddFunction(@ps_LoadBitmap,'function LoadBitmap(Path : string) : integer;');
AddFunction(@ps_SetBitmapSize,'procedure SetBitmapSize(Bmp,NewW,NewH : integer);');
AddFunction(@ps_GetBitmapSize,'procedure GetBitmapSize(Bmp : integer; var BmpW,BmpH : integer);');
AddFunction(@ps_StretchBitmapResize,'procedure StretchBitmapResize(Bmp,NewW,NewH : integer);');
AddFunction(@ps_CreateMirroredBitmap,'function CreateMirroredBitmap(Bmp : integer) : integer;');
AddFunction(@ps_CreateMirroredBitmapEx,'function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;');
AddFunction(@ps_FastSetPixel,'procedure FastSetPixel(bmp,x,y : integer; Color : TColor);');
AddFunction(@ps_FastSetPixels,'procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);');
AddFunction(@ps_FastGetPixel,'function FastGetPixel(bmp, x,y : integer) : TColor;');
AddFunction(@ps_FastGetPixels,'function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;');
AddFunction(@ps_FastDrawClear,'procedure FastDrawClear(bmp : integer; Color : TColor);');
AddFunction(@ps_FastDrawTransparent,'procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);');
AddFunction(@ps_SetTransparentColor,'procedure SetTransparentColor(bmp : integer; Color : TColor);');
AddFunction(@ps_GetTransparentColor,'function GetTransparentColor(bmp: integer) : TColor;');
AddFunction(@ps_FastReplaceColor,'procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);');
AddFunction(@ps_CopyClientToBitmap, 'procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer);');
AddFunction(@ps_BitmapFromClient,'function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer;');
AddFunction(@ps_SetBitmapName, 'procedure SetBitmapName(Bmp : integer; name : string);');
AddFunction(@ps_FindBitmap,'function FindBitmap(bitmap: integer; var x, y: Integer): Boolean;');
AddFunction(@ps_FindBitmapIn,'function FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@ps_FindBitmapToleranceIn,'function FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;');
AddFunction(@ps_FindBitmapSpiral,'function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;');
AddFunction(@ps_FindBitmapsSpiralTolerance,'function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;');
AddFunction(@ps_FindBitmapSpiralTolerance,'function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;');
AddFunction(@ps_RotateBitmap,'function RotateBitmap(bitmap: Integer; angle: Extended): Integer;');
AddFunction(@ps_Desaturate,'function DesaturateBitmap(Bitmap : integer) : integer;');
AddFunction(@ps_InvertBitmap,'procedure InvertBitmap(Bitmap : integer);');
AddFunction(@ps_CopyBitmap,'function CopyBitmap(Bitmap:  integer) : integer)');
AddFunction(@ps_GreyScaleBitmap,'function GreyScaleBitmap(bitmap : integer) : integer');
AddFunction(@ps_BrightnessBitmap,'function BrightnessBitmap(Bitmap,br : integer) : integer;');
AddFunction(@ps_ContrastBitmap,'function ContrastBitmap(bitmap : integer; co : extended) : integer;');
AddFunction(@ps_PosterizeBitmap,'function PosterizeBitmap(Bitmap : integer; po : integer) : integer;');
AddFunction(@ps_CreateMaskFromBitmap,'function CreateMaskFromBitmap(Bitmap : integer) : TMask;');
AddFunction(@ps_FindMaskTolerance,'function FindMaskTolerance(const mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
AddFunction(@ps_FindBitmapMaskTolerance,'function FindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
AddFunction(@ps_FindDeformedBitmapToleranceIn,'function FindDeformedBitmapToleranceIn(bitmap: integer; var x,y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;');
AddFunction(@ps_DrawTPABitmap,'procedure DrawTPABitmap(bitmap: integer; TPA: TPointArray; Color: integer);');
AddFunction(@ps_DrawATPABitmap,'procedure DrawATPABitmap(bitmap: integer; ATPA: T2DPointArray);');
AddFunction(@ps_DrawATPABitmapEx,'procedure DrawATPABitmapEx(bitmap: integer; ATPA: T2DPointArray; Colors: TIntegerArray);');
AddFunction(@ps_DrawBitmap,'procedure DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);');

{tpa}
SetCurrSection('TPA');
AddFunction(@ps_Quicksort,'procedure Quicksort(var Arr : TIntegerArray);');
AddFunction(@ps_tSwap,'procedure tSwap(var a, b: TPoint);');
AddFunction(@ps_tpaSwap,'procedure tpaSwap(var a, b: TPointArray);');
AddFunction(@ps_SwapE,'procedure SwapE(var a, b: Extended);');
AddFunction(@ps_RAaSTPAEx,'procedure RAaSTPAEx(var a: TPointArray; const w, h: Integer);');
AddFunction(@ps_RAaSTPA,'procedure RAaSTPA(var a: TPointArray; const Dist: Integer);');
AddFunction(@ps_NearbyPointInArrayEx,'function NearbyPointInArrayEx(const P: TPoint; w, h:Integer; a: TPointArray): Boolean;');
AddFunction(@ps_NearbyPointInArray,'function NearbyPointInArray(const P: TPoint; Dist:Integer; a: TPointArray): Boolean;');
AddFunction(@ps_QuickTPASort,'procedure QuickTPASort(var A: TIntegerArray; var B: TPointArray; iLo, iHi: Integer; SortUp: Boolean);');
AddFunction(@ps_QuickATPASort,'procedure QuickATPASort(var A: TIntegerArray; var B: T2DPointArray; iLo, iHi: Integer; SortUp: Boolean);');
AddFunction(@ps_SortTPAFrom,'procedure SortTPAFrom(var a: TPointArray; const From: TPoint);');
AddFunction(@ps_SortATPAFrom,'procedure SortATPAFrom(var a: T2DPointArray; const From: TPoint);');
AddFunction(@ps_SortATPAFromFirstPoint,'procedure SortATPAFromFirstPoint(var a: T2DPointArray; const From: TPoint);');
AddFunction(@ps_InvertTPA,'procedure InvertTPA(var a: TPointArray);');
AddFunction(@ps_InvertATPA,'procedure InvertATPA(var a: T2DPointArray);');
AddFunction(@ps_MiddleTPAEx,'function MiddleTPAEx(TPA: TPointArray; var x, y: Integer): Boolean;');
AddFunction(@ps_MiddleTPA,'function MiddleTPA(tpa: TPointArray): TPoint;');
AddFunction(@ps_SortATPASize,'procedure SortATPASize(var a: T2DPointArray; const BigFirst: Boolean);');
AddFunction(@ps_SortATPAFromSize,'procedure SortATPAFromSize(var a: T2DPointArray; const Size: Integer; CloseFirst: Boolean);');
AddFunction(@ps_InIntArrayEx,'function InIntArrayEx(a: TIntegerArray; var Where: Integer; const Number: Integer): Boolean;');
AddFunction(@ps_InIntArray,'function InIntArray(a: TIntegerArray; Number: Integer): Boolean;');
AddFunction(@ps_ClearSameIntegers,'procedure ClearSameIntegers(var a: TIntegerArray);');
AddFunction(@ps_ClearSameIntegersAndTPA,'procedure ClearSameIntegersAndTPA(var a: TIntegerArray; var p: TPointArray);');
AddFunction(@ps_SplitTPAEx,'function SplitTPAEx(arr: TPointArray; w, h: Integer): T2DPointArray;');
AddFunction(@ps_SplitTPA,'function SplitTPA(arr: TPointArray; Dist: Integer): T2DPointArray;');
AddFunction(@ps_FilterPointsPie,'procedure FilterPointsPie(var Points: TPointArray; const SD, ED, MinR, MaxR: Extended; Mx, My: Integer);');
AddFunction(@ps_GetATPABounds,'function GetATPABounds(ATPA: T2DPointArray): TBox;');
AddFunction(@ps_GetTPABounds,'function GetTPABounds(TPA: TPointArray): TBox;');
AddFunction(@ps_FindTPAinTPA,'function FindTPAinTPA(SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;');
AddFunction(@ps_GetSamePointsATPA,'function GetSamePointsATPA( ATPA : T2DPointArray; var Matches : TPointArray) : boolean;');
AddFunction(@ps_FindTextTPAinTPA,'function FindTextTPAinTPA(Height : integer; SearchTPA, TotalTPA: TPointArray; var Matches: TPointArray): Boolean;');
AddFunction(@ps_SortCircleWise,'procedure SortCircleWise(var tpa: TPointArray; const cx, cy, StartDegree: Integer; SortUp, ClockWise: Boolean);');
AddFunction(@ps_LinearSort,'procedure LinearSort(var tpa: TPointArray; cx, cy, sd: Integer; SortUp: Boolean);');
AddFunction(@ps_RotatePoint,'function RotatePoint(Const p: TPoint; angle, mx, my: Extended): TPoint;');
AddFunction(@ps_FindGapsTPA,'function FindGapsTPA(TPA: TPointArray; MinPixels: Integer): T2DPointArray;');
AddFunction(@ps_RemoveDistTPointArray,'function RemoveDistTPointArray(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean): TPointArray;');
AddFunction(@ps_CombineTPA,'function CombineTPA(Ar1, Ar2: TPointArray): TPointArray;');
AddFunction(@ps_ReArrangeandShortenArrayEx,'function ReArrangeandShortenArrayEx(a: TPointArray; w, h: Integer): TPointArray;');
AddFunction(@ps_ReArrangeandShortenArray,'function ReArrangeandShortenArray(a: TPointArray; Dist: Integer): TPointArray;');
AddFunction(@ps_TPAtoATPAEx,'function TPAtoATPAEx(TPA: TPointArray; w, h: Integer): T2DPointArray;');
AddFunction(@ps_TPAtoATPA,'function TPAtoATPA(TPA: TPointArray; Dist: Integer): T2DPointArray;');
AddFunction(@ps_CombineIntArray,'function CombineIntArray(Ar1, Ar2: TIntegerArray): TIntegerArray;');
AddFunction(@ps_MergeATPA,'function MergeATPA(ATPA : T2DPointArray)  : TPointArray;');
AddFunction(@ps_TPAFromBox,'function TPAFromBox(const Box : TBox) : TPointArray;');
AddFunction(@ps_RotatePoints,'function RotatePoints(Const P: TPointArray; A, cx, cy: Extended): TPointArray ;');
AddFunction(@ps_FindTPAEdges,'function FindTPAEdges(p: TPointArray): TPointArray;');
AddFunction(@ps_ClearTPAFromTPA,'function ClearTPAFromTPA(arP, ClearPoints: TPointArray): TPointArray;');
AddFunction(@ps_ReturnPointsNotInTPA,'function ReturnPointsNotInTPA(Const TotalTPA: TPointArray; const Box: TBox): TPointArray;');
AddFunction(@ps_PointInTPA,'function PointInTPA(p: TPoint; arP: TPointArray): Boolean;');
AddFunction(@ps_ClearDoubleTPA,'procedure ClearDoubleTPA(var TPA: TPointArray);');
AddFunction(@ps_TPACountSort,'procedure TPACountSort(Var TPA: TPointArray;const max: TPoint;Const SortOnX : Boolean);');
AddFunction(@ps_TPACountSortBase,'procedure TPACountSortBase(Var TPA: TPointArray;const maxx, base: TPoint; const SortOnX : Boolean);');
AddFunction(@ps_InvertTIA,'procedure InvertTIA(var tI: TIntegerArray);');
AddFunction(@ps_SumIntegerArray,'function SumIntegerArray(Ints : TIntegerArray): Integer;');
AddFunction(@ps_AverageTIA,'function AverageTIA(tI: TIntegerArray): Integer;');
AddFunction(@ps_AverageExtended,'function AverageExtended(tE: TExtendedArray): Extended;');
AddFunction(@ps_SplitTPAExWrap,'procedure SplitTPAExWrap(arr: TPointArray; w, h: Integer; var res : T2DPointArray);');
AddFunction(@ps_SplitTPAWrap,'procedure SplitTPAWrap(arr: TPointArray; Dist: Integer; var res: T2DPointArray);');
AddFunction(@ps_FindGapsTPAWrap,'procedure FindGapsTPAWrap(TPA: TPointArray; MinPixels: Integer; var Res : T2DPointArray);');
AddFunction(@ps_RemoveDistTPointArrayWrap,'procedure RemoveDistTPointArrayWrap(x, y, dist: Integer; ThePoints: TPointArray; RemoveHigher: Boolean; var Res :  TPointArray);');
AddFunction(@ps_CombineTPAWrap,'procedure CombineTPAWrap(Ar1, Ar2: TPointArray; var Res :  TPointArray);');
AddFunction(@ps_ReArrangeandShortenArrayExWrap,'procedure ReArrangeandShortenArrayExWrap(a: TPointArray; w, h: Integer; var Res :  TPointArray);');
AddFunction(@ps_ReArrangeandShortenArrayWrap,'procedure ReArrangeandShortenArrayWrap(a: TPointArray; Dist: Integer; var Res :  TPointArray);');
AddFunction(@ps_TPAtoATPAExWrap,'procedure TPAtoATPAExWrap(TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray);');
AddFunction(@ps_TPAtoATPAWrap,'procedure TPAtoATPAWrap(TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray);');
AddFunction(@ps_CombineIntArrayWrap, 'procedure CombineIntArrayWrap(Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray);');
AddFunction(@ps_ReturnPointsNotInTPAWrap,'procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray);');
AddFunction(@ps_MergeATPAWrap,'procedure MergeATPAWrap(ATPA : T2DPointArray; var Res: TPointArray);');
AddFunction(@ps_TPAFromBoxWrap,'procedure TPAFromBoxWrap(const Box : TBox; var Res : TPointArray);');
AddFunction(@ps_RotatePointsWrap,'procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray);');
AddFunction(@ps_FindTPAEdgesWrap,'procedure FindTPAEdgesWrap(p: TPointArray; var Res :  TPointArray);');
AddFunction(@ps_ClearTPAFromTPAWrap,'procedure ClearTPAFromTPAWrap(arP, ClearPoints: TPointArray;  var Res :  TPointArray);');
AddFunction(@ps_FilterPointsLine,'procedure FilterPointsLine(var Points: TPointArray; Radial: Extended; Radius, MX, MY: Integer);');
AddFunction(@ps_SameTPA,'function SameTPA(aTPA, bTPA: TPointArray): Boolean;');
AddFunction(@ps_TPAInATPA,'function TPAInATPA(TPA: TPointArray; InATPA: T2DPointArray; var Index: LongInt): Boolean;');

SetCurrSection('Settings');
AddFunction(@ps_KeyIsSetting, 'function KeyIsSetting(const KeyName: String): Boolean;');
AddFunction(@ps_KeyIsDirectory, 'function KeyIsDirectory(const KeyName: String): Boolean;');
AddFunction(@ps_GetSettingValue, 'function GetSettingValue(const KeyName: String): String;');
AddFunction(@ps_GetSettingValueDef, 'function GetSettingValueDef(const KeyName, defVal: String): String;');
AddFunction(@ps_SetSettingValue,'function SetSettingValue(const KeyName,value : string) : boolean;');
AddFunction(@ps_ListSettings, 'function ListSettings(const KeyName: String; var KeyReturn: TStringArray) : boolean;');
AddFunction(@ps_DeleteSetting, 'function DeleteSetting(const KeyName: String): Boolean;');
AddFunction(@ps_DeleteSubSettings, 'function DeleteSubSettings(const KeyName: String): Boolean;');
