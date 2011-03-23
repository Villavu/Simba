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

procedure lpCreateBitmapString(Params: PParamArray; Result: Pointer);
begin;
  PlpString(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].ToString;
end;

procedure lpGetMufasaBitmap(Params: PParamArray; Result: Pointer);
begin;
  PMufasaBitmap(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^];
end;

procedure lpCreateBitmap(Params: PParamArray; Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MBitmaps.CreateBMP(PLongInt(Params^[0])^, PLongInt(Params^[1])^);
end;

procedure lpFreeBitmap(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps.FreeBMP(PLongInt(Params^[0])^);
end;

procedure lpSaveBitmap(Params: PParamArray);
begin;
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].SaveToFile(PlpString(Params^[1])^);
end;

procedure lpBitmapFromString(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MBitmaps.CreateBMPFromString(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PlpString(Params^[2])^);
end;

procedure lpLoadBitmap(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MBitmaps.CreateBMPFromFile(PlpString(Params^[0])^);
end;

procedure lpSetBitmapSize(Params: PParamArray);
begin;
  if (PLongInt(Params^[1])^ >= 0) and (PLongInt(Params^[2])^ >= 0) then
    CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].SetSize(PLongInt(Params^[1])^, PLongInt(Params^[2])^)
  else
    raise exception.createfmt('Wrong Width or Height in SetBitmapSize: (%d, %d)', [PLongInt(Params^[1])^, PLongInt(Params^[2])^]);
end;

procedure lpStretchBitmapResize(Params: PParamArray);
begin;
  if (PLongInt(Params^[1])^ >= 0) and (PLongInt(Params^[2])^ >= 0) then
    CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].StretchResize(PLongInt(Params^[1])^, PLongInt(Params^[2])^)
  else
    raise exception.createfmt('Wrong Width or Height in ScretchResize: (%d, %d)', [PLongInt(Params^[1])^, PLongInt(Params^[2])^]);
end;

procedure lpGetBitmapSize(Params: PParamArray);
begin;
  With CurrThread.Client.MBitmaps[PLongInt(Params^[0])^] do
  begin;
    PLongInt(Params^[1])^ := Width;
    PLongInt(Params^[2])^ := Height;
  end;
end;

procedure lpSetBitmapName(Params: PParamArray);
begin;
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].Name:= PlpString(Params^[1])^;
end;

procedure lpCreateMirroredBitmap(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MBitmaps.CreateMirroredBitmap(PLongInt(Params^[0])^, MirrorWidth);
end;

procedure lpCreateMirroredBitmapEx(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MBitmaps.CreateMirroredBitmap(PLongInt(Params^[0])^, PBmpMirrorStyle(Params^[1])^);
end;

procedure lpFastGetPixel(Params: PParamArray; Result: Pointer);
begin;
  PLongWord(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastGetPixel(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpFastGetPixels(Params: PParamArray; Result: Pointer);
begin;
  PIntegerArray(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastGetPixels(PPointArray(Params^[1])^);
end;

procedure lpGetBitmapAreaColors(Params: PParamArray; Result: Pointer);
begin
  P2DIntArray(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].GetAreaColors(PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^);
end;

procedure lpFastSetPixel(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastSetPixel(PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^);
end;

procedure lpFastSetPixels(Params: PParamArray);
begin;
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastSetPixels(PPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure lpDrawTPABitmap(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].DrawTPA(PPointArray(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpDrawATPABitmap(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].DrawATPA(P2DPointArray(Params^[1])^);
end;

procedure lpDrawATPABitmapEx(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].DrawATPA(P2DPointArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

procedure lpFastDrawClear(Params: PParamArray);
begin;
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastDrawClear(PLongInt(Params^[1])^);
end;

procedure lpDrawBitmap(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].DrawToCanvas(PLongInt(Params^[2])^, PLongInt()^, PCanvas(Params^[1])^);
end;

procedure lpFastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer); extdecl;
begin;
  CurrThread.Client.MBitmaps[SourceBitmap].FastDrawTransparent(x,y,CurrThread.Client.MBitmaps[TargetBitmap]);
end;

procedure lpSetTransparentColor(Bmp : integer; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].SetTransparentColor(Color);
end;

procedure lpGetTransparentColor(Bmp : integer) : TColor; extdecl;
begin;
  Result := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].GetTransparentColor;
end;

procedure lpFastReplaceColor(bmp: Integer; OldColor, NewColor: TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastReplaceColor(OldColor,NewColor);
end;

procedure lpCopyClientToBitmap(bmp, xs, ys, xe, ye: Integer); extdecl;
begin
  CurrThread.Client.MBitmaps.GetBMP(PLongInt(Params^[0])^).CopyClientToBitmap(CurrThread.Client.IOManager, True, xs, ys, xe, ye);
end;

procedure lpBitmapFromClient(const xs, ys, xe, ye: Integer): Integer; extdecl;
begin;
  result := CurrThread.Client.MBitmaps.CreateBMP(0,0);
  CurrThread.Client.MBitmaps[result].CopyClientToBitmap(CurrThread.Client.IOManager,True,xs,ys,xe,ye);
end;

procedure lpFindBitmap(Bitmap: integer; var x, y: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmap(MBitmaps[PLongInt(Params^[0])^],x,y);
end;

procedure lpFindBitmapIn(bitmap: integer; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapIn(MBitmaps[PLongInt(Params^[0])^],x,y,xs,ys,xe,ye);
end;

procedure lpFindBitmapToleranceIn(bitmap: integer; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapToleranceIn( MBitmaps[PLongInt(Params^[0])^],x,y,xs,ys,xe,ye,tolerance);
end;


procedure lpFindBitmapSpiral(bitmap: Integer; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapSpiral(Mbitmaps[PLongInt(Params^[0])^],x,y,xs,ys,xe,ye);
end;

procedure lpFindBitmapsSpiralTolerance(bitmap: integer; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapsSpiralTolerance(MBitmaps[PLongInt(Params^[0])^],x,y,points,xs,ys,xe,ye,tolerance);
end;

procedure lpFindBitmapSpiralTolerance(bitmap: integer; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean; extdecl;
begin;
  with CurrThread.Client do
    result := MFinder.FindBitmapSpiralTolerance(MBitmaps[PLongInt(Params^[0])^],x,y,xs,ys,xe,ye,tolerance);
end;

procedure lpRotateBitmap(bitmap: Integer; angle: Extended): Integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    Result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].RotateBitmap(angle,Bmp[result]);
  end;
end;

procedure lpDesaturate(Bitmap : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].Desaturate(Bmp[result]);
  end;
end;

procedure lpInvertBitmap(Bitmap : integer); extdecl;
begin;
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].Invert;
end;

procedure lpCopyBitmap(Bitmap : integer) : integer; extdecl;
begin;
  result := Currthread.Client.MBitmaps.CopyBMP(PLongInt(Params^[0])^);
end;

procedure lpGreyScaleBitmap(Bitmap : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].GreyScale(Bmp[result]);
  end;
end;

procedure lpBrightnessBitmap(Bitmap,br : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].Brightness(Bmp[result],br);
  end;
end;

procedure lpContrastBitmap(bitmap : integer; co : extended) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].Contrast(Bmp[result],co);
  end;
end;

procedure lpPosterizeBitmap(Bitmap : integer; po : integer) : integer; extdecl;
begin;
  with CurrThread.Client.MBitmaps do
  begin
    result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].Posterize(Bmp[result],po);
  end;
end;

procedure lpCreateMaskFromBitmap(Bitmap : integer) : TMask; extdecl;
begin;
  result := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].CreateTMask;
end;

procedure lpFindMaskTolerance(const mask: TMask; var x, y: Integer; xs,ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindMaskTolerance(PLongInt(Params^[0])^,x,y,xs,ys,xe,ye,tolerance,contourtolerance);
end;

procedure lpFindBitmapMaskTolerance(mask: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; extdecl;
begin;
  {$ifdef mDebug}
  mDebugLn('Better be using FindMaskTolerance in combination with CreateMaskFromBitmap, more efficient.');
  {$endif}
  with CurrThread.Client do
    result := MFinder.FindMaskTolerance(MBitmaps[PLongInt(Params^[0])^].CreateTMask,x,y,xs,ys,xe,ye,tolerance,contourtolerance);
end;

procedure lpFindDeformedBitmapToleranceIn(bitmap: integer; var x,
  y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer;
  AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean; extdecl;
begin;
  result := CurrThread.Client.MFinder.FindDeformedBitmapToleranceIn(CurrThread.Client.MBitmaps[PLongInt(Params^[0])^],x,y,xs,ys,xe,ye,tolerance,range,AllowPartialAccuracy,accuracy);
end;

procedure lpRectangleBitmap(bitmap : integer; const box : TBox; Color : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].Rectangle(box,Color);
end;

procedure lpFloodFillBitmap(bitmap : integer; const StartPoint : TPoint; const SearchCol,ReplaceCol : TColor); extdecl;
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FloodFill(startPoint,searchcol,replacecol);
end;

procedure lpConvoluteBitmap(bitmap : integer; matrix : T2DExtendedArray) : integer; extdecl;
begin
  with CurrThread.Client.MBitmaps do
  begin
    result := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].Convolute(Bmp[Result],matrix);
  end;
end;

procedure lpCalculatePixelShift(Bmp1,Bmp2 : Integer; CompareBox : TBox) : integer;extdecl;
begin
  with CurrThread.Client.MBitmaps do
    result := CalculatePixelShift(GetBMP(PLongInt(Params^[0])^),GetBMP(bmp2),comparebox);
end;

procedure lpCalculatePixelTolerance(Bmp1,Bmp2 : Integer; CompareBox : TBox; CTS : integer) : extended;extdecl;
begin
  with CurrThread.Client.MBitmaps do
    result := CalculatePixelTolerance(GetBMP(PLongInt(Params^[0])^),GetBMP(bmp2),comparebox,cts);
end;
