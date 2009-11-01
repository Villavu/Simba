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

    PSCompile.inc for the Mufasa Macro Library
}

Sender.Comp.AddConstantN('AppPath','string').SetString(CurrThread.AppPath);
Sender.Comp.AddConstantN('ScriptPath','string').SetString(CurrThread.ScriptPath);
Sender.Comp.AddTypeS('TIntegerArray', 'Array of integer');
Sender.Comp.AddTypeS('TPointArray','Array of TPoint');
Sender.Comp.AddTypeS('TBmpMirrorStyle','(MirrorWidth,MirrorHeight,MirrorLine)');
Sender.Comp.AddTypeS('TMask','record  White, Black : TPointArray; WhiteHi,BlackHi : integer; W,H : integer;end;');


Sender.Comp.AddTypes('TDTMPointDef', 'record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
Sender.Comp.AddTypes('TDTMPointDefArray', 'Array Of TDTMPointDef;');
Sender.Comp.AddTypes('TDTM','record MainPoint: TDTMPointDef; SubPoints: TDTMPointDefArray; end;');
Sender.Comp.AddTypeS('pDTM','record p: TPointArray; c, t, asz, ash: TIntegerArray; end');
Sender.Comp.AddTypeS('T2DExtendedArray', 'array of array of extended');

Sender.AddFunction(@ThreadSafeCall,'function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;');
Sender.AddFunction(@psWriteln,'procedure writeln(s : string);');

{ DTM }

Sender.AddFunction(@PrintpDTM, 'Procedure PrintpDTM(tDTM : pDTM);');
Sender.AddFunction(@ps_GetDTM ,'function GetDTM(index: Integer; var dtm: pDTM): Boolean;');
Sender.AddFunction(@pDTMToTDTM, 'Function pDTMToTDTM(Const DTM: pDTM): TDTM;');
Sender.AddFunction(@tDTMTopDTM, 'Function tDTMTopDTM(Const DTM: TDTM): pDTM;');
Sender.AddFunction(@ps_DTMFromString, 'function DTMFromString(DTMString: String): Integer;');
Sender.AddFunction(@ps_FreeDTM, 'procedure FreeDTM(DTM: Integer);');
Sender.AddFunction(@ps_FindDTM, 'function FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;');
Sender.AddFunction(@ps_FindDTMs, 'function FindDTMs(DTM: Integer; var p: TPointArray; x1, y1, x2, y2: Integer): Boolean;');
Sender.AddFunction(@ps_FindDTMRotated, 'function FindDTMRotated(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; var aFound: Extended): Boolean;');
Sender.AddFunction(@ps_FindDTMsRotated, 'function FindDTMsRotated(DTM: Integer; var Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; var aFound: T2DExtendedArray) : Boolean;');
Sender.AddFunction(@ps_addDTM, 'function AddDTM(d: TDTM): Integer;');
Sender.AddFunction(@ps_addpDTM, 'function AddpDTM(d: pDTM): Integer;');

{maths}
sender.AddFunction(@power,'function pow(base,exponent : extended) : extended');
Sender.AddFunction(@max,'function Max(a, b: Integer): Integer;');
Sender.AddFunction(@min,'function Min(a, b: Integer): Integer;');
Sender.AddFunction(@pssqr,'function Sqr(e : extended) : extended;');
Sender.AddFunction(@classes.point,'function Point(x,y:integer) : TPoint;');
Sender.AddFunction(@Distance,'function Distance(x1,y1,x2,y2 : integer) : integer;');

{window}
Sender.AddFunction(@Freeze, 'function freeze:boolean;');
Sender.AddFunction(@Unfreeze, 'function unfreeze: boolean;');
Sender.AddFunction(@GetClientDimensions, 'procedure GetClientDimensions(var w, h:integer);');
Sender.AddFunction(@SetTargetBitmap,'procedure SetTargetBitmap(Bitmap : integer);');
Sender.AddFunction(@SetDesktopAsClient,'procedure SetDesktopAsClient');
Sender.AddFunction(@SetTargetArray, 'procedure SetTargetArray(P: Integer; Size: TPoint);');


{other}
Sender.AddFunction(@SaveScreenshot,'procedure SaveScreenshot(FileName: string);');
Sender.AddFunction(@Wait, 'procedure wait(t: integer);');
Sender.AddFunction(@GetTickCount, 'function GetSystemTime: Integer;');
Sender.AddFunction(@CreateForm,'function CreateForm : TForm;');
Sender.AddFunction(@CreateButton,'function CreateButton(Owner : TComponent) : TButton');
Sender.AddFunction(@HakunaMatata,'procedure HakunaMatata;');

{web}
Sender.AddFunction(@OpenWebPage,'procedure OpenWebPage(url : string);');

{Color + Color Finders}
Sender.AddFunction(@GetColor,'function GetColor(x, y: Integer): Integer;');
Sender.AddFunction(@FindColor, 'function findcolor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@FindColorTolerance, 'function findcolortolerance(var x, y: integer; color, x1, y1, x2, y2, tol: integer): boolean;');
Sender.AddFunction(@FindColors, 'function findcolors(var TPA: TPointArray; color, x1, y1, x2, y2: integer): boolean;');
Sender.AddFunction(@SimilarColors,'function SimilarColors(Col1,Col2,Tolerance : integer) : boolean');
Sender.AddFunction(@CountColorTolerance,'function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;');
Sender.AddFunction(@FindColorsTolerance,'function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tolerance: Integer): Boolean;');
Sender.AddFunction(@FindColorSpiral,'function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;');
Sender.AddFunction(@FindColorsSpiralTolerance,'function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;');
Sender.AddFunction(@SetColorToleranceSpeed, 'procedure SetColorToleranceSpeed(cts: integer);');

{Mouse etc.}
Sender.AddFunction(@MoveMouse, 'procedure MoveMouse(x, y: integer);');
Sender.AddFunction(@GetMousePos, 'procedure GetMousePos(var x, y: integer);');


{Bitmaps}
Sender.AddFunction(@CreateBitmap,'function CreateBitmap(w,h :integer) : integer;');
Sender.AddFunction(@FreeBitmap,'procedure FreeBitmap(Bmp : integer);');
Sender.AddFunction(@SaveBitmap,'procedure SaveBitmap(Bmp : integer; path : string);');
Sender.AddFunction(@BitmapFromString,'function BitmapFromString(Width,Height : integer; Data : string): integer;');
Sender.AddFunction(@LoadBitmap,'function LoadBitmap(Path : string) : integer;');
Sender.AddFunction(@SetBitmapSize,'procedure SetBitmapSize(Bmp,NewW,NewH : integer);');
Sender.AddFunction(@GetBitmapSize,'procedure GetBitmapSize(Bmp : integer; Var BmpW,BmpH : integer);');
Sender.AddFunction(@StretchBitmapResize,'procedure StretchBitmapResize(Bmp,NewW,NewH : integer);');
Sender.AddFunction(@CreateMirroredBitmap,'function CreateMirroredBitmap(Bmp : integer) : integer;');
Sender.AddFunction(@CreateMirroredBitmapEx,'function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;');
Sender.AddFunction(@FastSetPixel,'procedure FastSetPixel(bmp,x,y : integer; Color : TColor);');
Sender.AddFunction(@FastSetPixels,'procedure FastSetPixels(bmp : integer; TPA : TPointArray; Colors : TIntegerArray);');
Sender.AddFunction(@FastGetPixel,'function FastGetPixel(bmp, x,y : integer) : TColor;');
Sender.AddFunction(@FastGetPixels,'function FastGetPixels(Bmp : integer; TPA : TPointArray) : TIntegerArray;');
Sender.AddFunction(@FastDrawClear,'procedure FastDrawClear(bmp : integer; Color : TColor)');
Sender.AddFunction(@FastDrawTransparent,'procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);');
Sender.AddFunction(@SetTransparentColor,'procedure SetTransparentColor(bmp : integer; Color : TColor);');
Sender.AddFunction(@GetTransparentColor,'function GetTransparentColor(bmp: integer) : TColor;');
Sender.AddFunction(@FastReplaceColor,'procedure FastReplaceColor(Bmp : integer; OldColor,NewColor : TColor);');
Sender.AddFunction(@ps_CopyClientToBitmap, 'procedure CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer);');
Sender.AddFunction(@SetBitmapName, 'procedure SetBitmapName(Bmp : integer; name : string);');
Sender.AddFunction(@FindBitmap,'function FindBitmap(bitmap: integer; var x, y: Integer): Boolean;');
Sender.AddFunction(@FindBitmapIn,'function FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;');
sender.AddFunction(@FindBitmapToleranceIn,'function FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;');
Sender.AddFunction(@FindBitmapSpiral,'function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;');
Sender.AddFunction(@FindBitmapsSpiralTolerance,'function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;');
Sender.AddFunction(@FindBitmapSpiralTolerance,'function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;');
Sender.AddFunction(@RotateBitmap,'function RotateBitmap(bitmap: Integer; angle: Extended): Integer;');
Sender.AddFunction(@Desaturate,'function DesaturateBitmap(Bitmap : integer) : integer;');
sender.AddFunction(@InvertBitmap,'procedure InvertBitmap(Bitmap : integer);');
Sender.AddFunction(@CopyBitmap,'function CopyBitmap(Bitmap:  integer) : integer)');
Sender.AddFunction(@GreyScaleBitmap,'function GreyScaleBitmap(bitmap : integer) : integer');
Sender.AddFunction(@BrightnessBitmap,'function BrightnessBitmap(Bitmap,br : integer) : integer;');
Sender.AddFunction(@ContrastBitmap,'function ContrastBitmap(bitmap : integer; co : extended) : integer;');
Sender.AddFunction(@PosterizeBitmap,'function PosterizeBitmap(Bitmap : integer; po : integer) : integer;');
Sender.AddFunction(@CreateBitmapMask,'function CreateBitmapMask(Bitmap : integer) : TMask;');
Sender.AddFunction(@FindMaskTolerance,'function FindMaskTolerance(mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
Sender.AddFunction(@FindBitmapMaskTolerance,'function FindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;');
Sender.AddFunction(@FindDeformedBitmapToleranceIn,'function FindDeformedBitmapToleranceIn(bitmap: integer; var x,y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;');
