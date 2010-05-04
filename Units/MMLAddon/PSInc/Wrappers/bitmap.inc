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

    Bitmap.inc for the Mufasa Macro Library
}
function ps_CreateBitmapString(bmp : integer) : string; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[bmp].ToString;
end;

function ps_GetMufasaBitmap(bmp : integer): TMufasaBitmap; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[bmp];
end;

function ps_CreateBitmap(w,h : integer):integer; extdecl;
begin
  result := CurrThread.Client.MBitmaps.CreateBMP(w,h);
end;

procedure ps_FreeBitmap(Number : integer); extdecl;
begin
  CurrThread.Client.MBitmaps.FreeBMP(Number);
end;

procedure ps_SaveBitmap(Bmp : integer; path : string); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bmp].SaveToFile(Path);
end;

function ps_BitmapFromString(Width,height : integer; Data : string) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateBMPFromString(Width,Height,Data);
end;

function ps_LoadBitmap(Path : String) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateBMPFromFile(Path);
end;

procedure ps_SetBitmapSize(Bmp,NewW,NewH : integer); extdecl;
begin;
  if (NewW>=0) and (NewH >=0) then
    CurrThread.Client.MBitmaps[Bmp].SetSize(NewW,NewH)
  else
    raise exception.createfmt('Wrong Width or Height in SetBitmapSize: (%d,%d)',[NewW,NewH]);
end;

procedure ps_StretchBitmapResize(Bmp,NewW,NewH : integer); extdecl;
begin;
  if (NewW>=0) and (NewH >=0) then
    CurrThread.Client.MBitmaps[Bmp].StretchResize(NewW,NewH)
  else
    raise exception.createfmt('Wrong Width or Height in ScretchResize: (%d,%d)',[NewW,NewH]);
end;

procedure ps_GetBitmapSize(Bmp : integer; var BmpW,BmpH : integer); extdecl;
begin;
  With CurrThread.Client.MBitmaps[bmp] do
  begin;
    BmpW := width;
    BmpH := Height;
  end;
end;

procedure ps_SetBitmapName(Bmp : integer; name : string); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bmp].Name:= name;
end;

function ps_CreateMirroredBitmap(Bmp : integer) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp, MirrorWidth);
end;

function ps_CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp,MirrorStyle);
end;

function ps_FastGetPixel(bmp,x,y : integer) : LongWord; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps[Bmp].FastGetPixel(x,y);
end;

function ps_FastGetPixels(bmp : integer; TPA : TPointArray) : TIntegerArray; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[Bmp].FastGetPixels(TPA);
end;

function ps_GetBitmapAreaColors(bmp,xs, ys, xe, ye: Integer): T2DIntArray;extdecl;
begin
  result := CurrThread.Client.MBitmaps[bmp].GetAreaColors(xs,ys,xe,ye);
end;

procedure ps_FastSetPixel(Bmp,x,y : integer; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[bmp].FastSetPixel(x,y,color);
end;

procedure ps_FastSetPixels(Bmp : integer; TPA : TPointArray; Colors : TIntegerArray); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bmp].FastSetPixels(TPA,Colors);
end;

procedure ps_DrawTPABitmap(bitmap : integer; TPA : TPointArray; Color : integer); extdecl;
begin
  CurrThread.Client.MBitmaps[Bitmap].DrawTPA(TPA,Color);
end;
procedure ps_DrawATPABitmap(bitmap : integer; ATPA : T2DPointArray); extdecl;
begin
  CurrThread.Client.MBitmaps[bitmap].DrawATPA(ATPA);
end;

procedure ps_DrawATPABitmapEx(bitmap : integer; ATPA : T2DPointArray; Colors : TIntegerArray); extdecl;
begin
  CurrThread.Client.MBitmaps[bitmap].DrawATPA(ATPA,Colors);
end;

procedure ps_FastDrawClear(bmp : integer; Color : TColor); extdecl;
begin;
  CurrThread.Client.MBitmaps[bmp].FastDrawClear(Color);
end;

procedure ps_DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);extdecl;
begin
  CurrThread.Client.MBitmaps[bmp].DrawToCanvas(x,y,dest);
end;

procedure ps_FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer); extdecl;
begin;
  CurrThread.Client.MBitmaps[SourceBitmap].FastDrawTransparent(x,y,CurrThread.Client.MBitmaps[TargetBitmap]);
end;

procedure ps_SetTransparentColor(Bmp : integer; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[Bmp].SetTransparentColor(Color);
end;

function ps_GetTransparentColor(Bmp : integer) : TColor; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps[bmp].GetTransparentColor;
end;

procedure ps_FastReplaceColor(bmp: Integer; OldColor, NewColor: TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[Bmp].FastReplaceColor(OldColor,NewColor);
end;

procedure ps_CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer); extdecl;
begin
  CurrThread.Client.MBitmaps.GetBMP(bmp).CopyClientToBitmap(CurrThread.Client.IOManager, True, xs, ys, xe, ye);
end;

function ps_BitmapFromClient(const xs, ys, xe, ye: Integer): Integer; extdecl;
begin;
  result := CurrThread.Client.MBitmaps.CreateBMP(0,0);
  CurrThread.Client.MBitmaps[result].CopyClientToBitmap(CurrThread.Client.IOManager,True,xs,ys,xe,ye);
end;

function ps_FindBitmap(Bitmap: integer; var x, y: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmap( MBitmaps[bitmap],x,y);
end;

function ps_FindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapIn( MBitmaps[bitmap],x,y,xs,ys,xe,ye);
end;

function ps_FindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapToleranceIn( MBitmaps[bitmap],x,y,xs,ys,xe,ye,tolerance);
end;


function ps_FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapSpiral(Mbitmaps[bitmap],x,y,xs,ys,xe,ye);
end;

function ps_FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapsSpiralTolerance(MBitmaps[bitmap],x,y,points,xs,ys,xe,ye,tolerance);
end;

function ps_FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapSpiralTolerance(MBitmaps[bitmap],x,y,xs,ys,xe,ye,tolerance);
end;

function ps_RotateBitmap(bitmap: Integer; angle: Extended): Integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    Result := CreateBMP(0,0);
    Bmp[Bitmap].RotateBitmap(angle,Bmp[result]);
  end;
end;

function ps_Desaturate(Bitmap : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].Desaturate(Bmp[result]);
  end;
end;
procedure ps_InvertBitmap(Bitmap : integer); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bitmap].Invert;
end;
function ps_CopyBitmap(Bitmap : integer) : integer; extdecl;
begin;
  result := Currthread.Client.MBitmaps.CopyBMP(bitmap);
end;
function ps_GreyScaleBitmap(Bitmap : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].GreyScale(Bmp[result]);
  end;
end;
function ps_BrightnessBitmap(Bitmap,br : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].Brightness(Bmp[result],br);
  end;
end;
function ps_ContrastBitmap(bitmap : integer; co : extended) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].Contrast(Bmp[result],co);
  end;
end;

function ps_PosterizeBitmap(Bitmap : integer; po : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin
    result := CreateBMP(0,0);
    Bmp[bitmap].Posterize(Bmp[result],po);
  end;
end;
function ps_CreateMaskFromBitmap(Bitmap : integer) : TMask; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[Bitmap].CreateTMask;
end;

function ps_FindMaskTolerance(const mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindMaskTolerance(Mask,x,y,xs,ys,xe,ye,tolerance,contourtolerance);
end;
function ps_FindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; extdecl;
begin;
  {$ifdef mDebug}
  mDebugLn('Better be using FindMaskTolerance in combination with CreateMaskFromBitmap, more efficient.');
  {$endif}
  with CurrThread.Client do
    result := MFinder.FindMaskTolerance(MBitmaps[mask].CreateTMask,x,y,xs,ys,xe,ye,tolerance,contourtolerance);
end;

function ps_FindDeformedBitmapToleranceIn(bitmap: integer; var x,
  y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer;
  AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindDeformedBitmapToleranceIn(CurrThread.Client.MBitmaps[Bitmap],x,y,xs,ys,xe,ye,tolerance,range,AllowPartialAccuracy,accuracy);
end;

procedure ps_RectangleBitmap(bitmap : integer; const box : TBox; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[bitmap].Rectangle(box,Color);
end;

procedure ps_FloodFillBitmap(bitmap : integer; const StartPoint : TPoint; const SearchCol,ReplaceCol : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[bitmap].FloodFill(startPoint,searchcol,replacecol);
end;

function ps_CalculatePixelShift(Bmp1,Bmp2 : Integer; CompareBox : TBox) : integer;extdecl;
begin
  with CurrThread.Client.MBitmaps do
    result := CalculatePixelShift(GetBMP(bmp1),GetBMP(bmp2),comparebox);
end;

function ps_CalculatePixelTolerance(Bmp1,Bmp2 : Integer; CompareBox : TBox; CTS : integer) : extended;extdecl;
begin
  with CurrThread.Client.MBitmaps do
    result := CalculatePixelTolerance(GetBMP(bmp1),GetBMP(bmp2),comparebox,cts);
end;
