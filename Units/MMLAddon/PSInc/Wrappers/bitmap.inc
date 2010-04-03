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
function CreateBitmapString(bmp : integer) : string; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[bmp].ToString;
end;

function GetMufasaBitmap(bmp : integer): TMufasaBitmap; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[bmp];
end;

function CreateBitmap(w,h : integer):integer; extdecl;
begin
  result := CurrThread.Client.MBitmaps.CreateBMP(w,h);
end;

procedure FreeBitmap(Number : integer); extdecl;
begin
  CurrThread.Client.MBitmaps.FreeBMP(Number);
end;

procedure SaveBitmap(Bmp : integer; path : string); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bmp].SaveToFile(Path);
end;

function BitmapFromString(Width,height : integer; Data : string) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateBMPFromString(Width,Height,Data);
end;

function LoadBitmap(Path : String) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateBMPFromFile(Path);
end;

procedure SetBitmapSize(Bmp,NewW,NewH : integer); extdecl;
begin;
  if (NewW>=0) and (NewH >=0) then
    CurrThread.Client.MBitmaps[Bmp].SetSize(NewW,NewH)
  else
    raise exception.createfmt('Wrong Width or Height in SetBitmapSize: (%d,%d)',[NewW,NewH]);
end;

procedure StretchBitmapResize(Bmp,NewW,NewH : integer); extdecl;
begin;
  if (NewW>=0) and (NewH >=0) then
    CurrThread.Client.MBitmaps[Bmp].StretchResize(NewW,NewH)
  else
    raise exception.createfmt('Wrong Width or Height in ScretchResize: (%d,%d)',[NewW,NewH]);
end;

procedure GetBitmapSize(Bmp : integer; out BmpW,BmpH : integer); extdecl;
begin;
  With CurrThread.Client.MBitmaps[bmp] do
  begin;
    BmpW := width;
    BmpH := Height;
  end;
end;

procedure SetBitmapName(Bmp : integer; name : string); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bmp].Name:= name;
end;

function CreateMirroredBitmap(Bmp : integer) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp, MirrorWidth);
end;

function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp,MirrorStyle);
end;

function FastGetPixel(bmp,x,y : integer) : LongWord; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps[Bmp].FastGetPixel(x,y);
end;

function FastGetPixels(bmp : integer; TPA : TPointArray) : TIntegerArray; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[Bmp].FastGetPixels(TPA);
end;

procedure FastSetPixel(Bmp,x,y : integer; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[bmp].FastSetPixel(x,y,color);
end;

procedure FastSetPixels(Bmp : integer; TPA : TPointArray; Colors : TIntegerArray); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bmp].FastSetPixels(TPA,Colors);
end;

procedure DrawTPABitmap(bitmap : integer; TPA : TPointArray; Color : integer); extdecl;
begin
  CurrThread.Client.MBitmaps[Bitmap].DrawTPA(TPA,Color);
end;
procedure DrawATPABitmap(bitmap : integer; ATPA : T2DPointArray); extdecl;
begin
  CurrThread.Client.MBitmaps[bitmap].DrawATPA(ATPA);
end;

procedure DrawATPABitmapEx(bitmap : integer; ATPA : T2DPointArray; Colors : TIntegerArray); extdecl;
begin
  CurrThread.Client.MBitmaps[bitmap].DrawATPA(ATPA,Colors);
end;

procedure FastDrawClear(bmp : integer; Color : TColor); extdecl;
begin;
  CurrThread.Client.MBitmaps[bmp].FastDrawClear(Color);
end;

procedure DrawBitmap(Bmp: Integer; Dest: TCanvas; x, y: Integer);extdecl;
begin
  CurrThread.Client.MBitmaps[bmp].DrawToCanvas(x,y,dest);
end;

procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer); extdecl;
begin;
  CurrThread.Client.MBitmaps[SourceBitmap].FastDrawTransparent(x,y,CurrThread.Client.MBitmaps[TargetBitmap]);
end;

procedure SetTransparentColor(Bmp : integer; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[Bmp].SetTransparentColor(Color);
end;

function GetTransparentColor(Bmp : integer) : TColor; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps[bmp].GetTransparentColor;
end;

procedure FastReplaceColor(bmp: Integer; OldColor, NewColor: TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[Bmp].FastReplaceColor(OldColor,NewColor);
end;

procedure ps_CopyClientToBitmap(bmp, xs, ys, xe, ye: Integer); extdecl;
begin
  CurrThread.Client.MBitmaps.GetBMP(bmp).CopyClientToBitmap(CurrThread.Client.IOManager, True, xs, ys, xe, ye);
end;

function BitmapFromClient(const xs, ys, xe, ye: Integer): Integer; extdecl;
begin;
  result := CurrThread.Client.MBitmaps.CreateBMP(0,0);
  CurrThread.Client.MBitmaps[result].CopyClientToBitmap(CurrThread.Client.IOManager,True,xs,ys,xe,ye);
end;

function FindBitmap(Bitmap: integer; out x, y: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmap( MBitmaps[bitmap],x,y);
end;

function FindBitmapIn(bitmap: integer; out x, y: Integer;  xs, ys, xe, ye: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapIn( MBitmaps[bitmap],x,y,xs,ys,xe,ye);
end;

function FindBitmapToleranceIn(bitmap: integer; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapToleranceIn( MBitmaps[bitmap],x,y,xs,ys,xe,ye,tolerance);
end;


function FindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapSpiral(Mbitmaps[bitmap],x,y,xs,ys,xe,ye);
end;

function FindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; out Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapsSpiralTolerance(MBitmaps[bitmap],x,y,points,xs,ys,xe,ye,tolerance);
end;

function FindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapSpiralTolerance(MBitmaps[bitmap],x,y,xs,ys,xe,ye,tolerance);
end;

function RotateBitmap(bitmap: Integer; angle: Extended): Integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    Result := CreateBMP(0,0);
    Bmp[Bitmap].RotateBitmap(angle,Bmp[result]);
  end;
end;

function Desaturate(Bitmap : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].Desaturate(Bmp[result]);
  end;
end;
procedure InvertBitmap(Bitmap : integer); extdecl;
begin;
  CurrThread.Client.MBitmaps[Bitmap].Invert;
end;
function CopyBitmap(Bitmap : integer) : integer; extdecl;
begin;
  result := Currthread.Client.MBitmaps.CopyBMP(bitmap);
end;
function GreyScaleBitmap(Bitmap : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].GreyScale(Bmp[result]);
  end;
end;
function BrightnessBitmap(Bitmap,br : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].Brightness(Bmp[result],br);
  end;
end;
function ContrastBitmap(bitmap : integer; co : extended) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[Bitmap].Contrast(Bmp[result],co);
  end;
end;

function PosterizeBitmap(Bitmap : integer; po : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin
    result := CreateBMP(0,0);
    Bmp[bitmap].Posterize(Bmp[result],po);
  end;
end;
function CreateMaskFromBitmap(Bitmap : integer) : TMask; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[Bitmap].CreateTMask;
end;

function FindMaskTolerance(const mask: TMask; out x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindMaskTolerance(Mask,x,y,xs,ys,xe,ye,tolerance,contourtolerance);
end;
function FindBitmapMaskTolerance(mask: Integer; out x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; extdecl;
begin;
  {$ifdef mDebug}
  mDebugLn('Better be using FindMaskTolerance in combination with CreateMaskFromBitmap, more efficient.');
  {$endif}
  with CurrThread.Client do
    result := MFinder.FindMaskTolerance(MBitmaps[mask].CreateTMask,x,y,xs,ys,xe,ye,tolerance,contourtolerance);
end;

function FindDeformedBitmapToleranceIn(bitmap: integer; out x,
  y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer;
  AllowPartialAccuracy: Boolean; out accuracy: Extended): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindDeformedBitmapToleranceIn(CurrThread.Client.MBitmaps[Bitmap],x,y,xs,ys,xe,ye,tolerance,range,AllowPartialAccuracy,accuracy);
end;
