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
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].DrawToCanvas(PLongInt(Params^[2])^, PLongInt(Params^[3])^, PCanvas(Params^[1])^);
end;

procedure lpFastDrawTransparent(Params: PParamArray);
begin;
  with CurrThread.Client do
    MBitmaps[PLongInt(Params^[2])^].FastDrawTransparent(PLongInt(Params^[0])^, PLongInt(Params^[1])^, MBitmaps[PLongInt(Params^[3])^]);
end;

procedure lpSetTransparentColor(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].SetTransparentColor(PLongInt(Params^[1])^);
end;

procedure lpGetTransparentColor(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].GetTransparentColor;
end;

procedure lpFastReplaceColor(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FastReplaceColor(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpCopyClientToBitmap(Params: PParamArray);
begin
  with CurrThread.Client do
    MBitmaps.GetBMP(PLongInt(Params^[0])^).CopyClientToBitmap(IOManager, True, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^);
end;

procedure lpBitmapFromClient(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
  begin
    PLongInt(Result)^ := MBitmaps.CreateBMP(0, 0);
    MBitmaps[PLongInt(Result)^].CopyClientToBitmap(IOManager, True, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^);
  end;
end;

procedure lpFindBitmap(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindBitmap(MBitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpFindBitmapIn(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindBitmapIn(MBitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindBitmapToleranceIn(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindBitmapToleranceIn( MBitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^);
end;


procedure lpFindBitmapSpiral(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiral(Mbitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^);
end;

procedure lpFindBitmapsSpiralTolerance(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindBitmapsSpiralTolerance(MBitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PPointArray(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^);
end;

procedure lpFindBitmapSpiralTolerance(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindBitmapSpiralTolerance(MBitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^);
end;

procedure lpRotateBitmap(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    PLongInt(Result)^ := CreateBMP(0, 0);
    Bmp[PLongInt(Params^[0])^].RotateBitmap(PExtended(Params^[1])^, Bmp[PLongInt(Result)^]);
  end;
end;

procedure lpDesaturate(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    PLongInt(Result)^ := CreateBMP(0, 0);
    Bmp[PLongInt(Params^[0])^].Desaturate(Bmp[PLongInt(Result)^]);
  end;
end;

procedure lpInvertBitmap(Params: PParamArray);
begin;
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].Invert;
end;

procedure lpCopyBitmap(Params: PParamArray; Result: Pointer);
begin;
  PLongInt(Result)^ := Currthread.Client.MBitmaps.CopyBMP(PLongInt(Params^[0])^);
end;

procedure lpGreyScaleBitmap(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    PLongInt(Result)^ := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].GreyScale(Bmp[PLongInt(Result)^]);
  end;
end;

procedure lpBrightnessBitmap(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    PLongInt(Result)^ := CreateBMP(0,0);
    Bmp[PLongInt(Params^[0])^].Brightness(Bmp[PLongInt(Result)^], PLongInt(Params^[1])^);
  end;
end;

procedure lpContrastBitmap(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client.MBitmaps do
  begin;
    PLongInt(Result)^ := CreateBMP(0, 0);
    Bmp[PLongInt(Params^[0])^].Contrast(Bmp[PLongInt(Result)^], PExtended(Params^[1])^);
  end;
end;

procedure lpPosterizeBitmap(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client.MBitmaps do
  begin
    PLongInt(Result)^ := CreateBMP(0, 0);
    Bmp[PLongInt(Params^[0])^].Posterize(Bmp[PLongInt(Result)^], PLongInt(Params^[1])^);
  end;
end;

procedure lpCreateMaskFromBitmap(Params: PParamArray; Result: Pointer);
begin;
  PMask(Result)^ := CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].CreateTMask;
end;

procedure lpFindMaskTolerance(Params: PParamArray; Result: Pointer);
begin;
  PBoolean(Result)^ := CurrThread.Client.MFinder.FindMaskTolerance(PMask(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^);
end;

procedure lpFindBitmapMaskTolerance(Params: PParamArray; Result: Pointer);
begin;
  {$IFDEF mDebug}mDebugLn('Better be using FindMaskTolerance in combination with CreateMaskFromBitmap, more efficient.');{$ENDIF}
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindMaskTolerance(MBitmaps[PLongInt(Params^[0])^].CreateTMask, PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^);
end;

procedure lpFindDeformedBitmapToleranceIn(Params: PParamArray; Result: Pointer);
begin;
  with CurrThread.Client do
    PBoolean(Result)^ := MFinder.FindDeformedBitmapToleranceIn(MBitmaps[PLongInt(Params^[0])^], PLongInt(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^, PLongInt(Params^[4])^, PLongInt(Params^[5])^, PLongInt(Params^[6])^, PLongInt(Params^[7])^, PLongInt(Params^[8])^, PBoolean(Params^[9])^, PExtended(Params^[10])^);
end;

procedure lpRectangleBitmap(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].Rectangle(PBox(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpFloodFillBitmap(Params: PParamArray);
begin
  CurrThread.Client.MBitmaps[PLongInt(Params^[0])^].FloodFill(PPoint(Params^[1])^, PLongInt(Params^[2])^, PLongInt(Params^[3])^);
end;

procedure lpConvoluteBitmap(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client.MBitmaps do
  begin
    PLongInt(Result)^ := CreateBMP(0, 0);
    Bmp[PLongInt(Params^[0])^].Convolute(Bmp[PLongInt(Result)^], P2dExtendedArray(Params^[1])^);
  end;
end;

procedure lpCalculatePixelShift(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client.MBitmaps do
    PLongInt(Result)^ := CalculatePixelShift(GetBMP(PLongInt(Params^[0])^), GetBMP(PLongInt(Params^[1])^), PBox(Params^[2])^);
end;

procedure lpCalculatePixelTolerance(Params: PParamArray; Result: Pointer);
begin
  with CurrThread.Client.MBitmaps do
    PExtended(Result)^ := CalculatePixelTolerance(GetBMP(PLongInt(Params^[0])^), GetBMP(PLongInt(Params^[1])^), PBox(Params^[2])^, PLongInt(Params^[3])^);
end;
