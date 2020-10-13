unit simba.matchtemplate;
{==============================================================================]
  Copyright © 2018, Jarl Krister Holta
  
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
[==============================================================================}
{$I header.inc}

interface

uses
  sysutils,
  simba.matchtemplate_core;

const
  MATCH_CCORR         = 0;
  MATCH_CCORR_NORMED  = 1;
  MATCH_CCOEFF        = 2;
  MATCH_CCOEFF_NORMED = 3;
  MATCH_SQDIFF        = 4;
  MATCH_SQDIFF_NORMED = 5;

type
  PMatchTemplateImageCache = ^TMatchTemplateImageCache;
  TMatchTemplateImageCache = packed record
    W, H: Int32;
    R, G, B: packed record
      Spec: T2DComplexArray;
      Sum: T2DDoubleArray;
      SumSquared: T2DDoubleArray;
    end;
  end;

function MatchTemplate(constref Image, Templ: T2DIntArray; TMFormula: Int32): T2DSingleArray;
function MatchTemplate(constref Image, Templ: T2DIntArray; TMFormula: Int32; var Cache: TMatchTemplateImageCache): T2DSingleArray;

implementation

uses
  math, utf8process,
  simba.matchtemplate_matrix, simba.threadpool, simba.FFTPACK4;

// --------------------------------------------------------------------------------
// Helpers

function DoFFT2(I: T2DSingleArray; outW, outH: Int32): T2DComplexArray;
var
  x,y,W,H: Int32;
begin
  Size(I, W,H);

  SetLength(Result, FFTPACK.OptimalDFTSize(outH), FFTPACK.OptimalDFTSize(outW));
  for y:=0 to H-1 do for x:=0 to W-1 do Result[y,x].re := I[y,x];
    Result := FFTPACK.FFT2(Result);
end;

function DoIFFT2(Spec: T2DComplexArray; outW, outH: Int32): T2DSingleArray;
var
  x,y: Int32;
begin
  Spec := FFTPACK.IFFT2(Spec);
  SetLength(Result, outH, outW);
  for y:=0 to outH-1 do
    for x:=0 to outW-1 do Result[y,x] := Spec[y,x].re;
end;

procedure InitMatrix(out a: T2DSingleArray; H,W: Int32; InitValue:Int32);
var
  x,y: Int32;
begin
  SetLength(a, H,W);
  for y:=0 to H-1 do
    for x:=0 to W-1 do a[y,x] := InitValue;
end;


// --------------------------------------------------------------------------------
// a * conj(b)

procedure Parallel_MulSpecConj(Params: PParamArray; iLow, iHigh: Int32);
var
  x,y: Int32;
  a,b,r: T2DComplexArray;
  re,im: Single;
begin
  a := T2DComplexArray(Params^[0]^);
  b := T2DComplexArray(Params^[1]^);
  r := T2DComplexArray(Params^[2]^);
  for y:=iLow to iHigh do
    for x:=0 to High(a[y]) do
      begin
        re := (a[y,x].re *  b[y,x].re) - (a[y,x].im * -b[y,x].im);
        im := (a[y,x].re * -b[y,x].im) + (a[y,x].im *  b[y,x].re);
        r[y,x].re := re;
        r[y,x].im := im;
      end;
end;

function MulSpectrumConj(a,b: T2DComplexArray): T2DComplexArray;
var
  W,H: Int32;
begin
  Size(a,W,H);
  SetLength(Result, H,W);
  SimbaThreadPool.RunParallel(@Parallel_MulSpecConj, [@a,@b,@Result], Low(a), High(a), GetSystemThreadCount(), Area(a) < 300*300);
end;


// --------------------------------------------------------------------------------
// cross correlate

function CCORR(Image, Templ: T2DSingleArray; out ImageSpec: T2DComplexArray): T2DSingleArray;
var
  iw,ih,tw,th: Int32;
  b: T2DComplexArray;
begin
  Size(Image, iw,ih);
  Size(Templ, tw,th);

  imageSpec := DoFFT2(Image, iw,ih);
  b := DoFFT2(Templ, iw,ih);
  b := MulSpectrumConj(imageSpec,b);
  Result := DoIFFT2(b, iw-tw+1, ih-th+1);
end;

function CCORR_CACHE(iw,ih: Int32; ImageSpec: T2DComplexArray; Templ: T2DSingleArray): T2DSingleArray;
var
  tw,th: Int32;
  b: T2DComplexArray;
begin
  Size(Templ, tw,th);

  b := DoFFT2(Templ, iw,ih);
  b := MulSpectrumConj(imageSpec,b);
  Result := DoIFFT2(b, iw-tw+1, ih-th+1);
end;

// ----------------------------------------------------------------------------
// Cross correlation (coefficient) of single channel matrices
//
//function CCOEFF_1C(a,t: T2DSingleArray; Normed: Boolean): T2DSingleArray;
//var
//  x,y,tw,th,aw,ah: Int32;
//  invSize, numer, denom, tplSdv, tplMean, tplSigma, wndDiff, wndSum: Double;
//  xcorr: T2DSingleArray;
//  sum, sum2: T2DDoubleArray;
//begin
//  xcorr := CCORR(a,t);
//  Size(t, tw,th);
//
//  invSize := Double(1.0) / Double(tw*th);
//  MeanStdev(t, tplMean, tplSdv);
//  tplSigma := tplSdv / Sqrt(invSize);
//
//  if tplSdv < 0.00001 then
//  begin
//    InitMatrix(Result, Length(xcorr), Length(xcorr[0]), 1);
//    Exit;
//  end;
//
//  sum := SumsPd(a, sum2);
//  Size(sum, aw,ah);
//  SetLength(Result, ah-th, aw-tw);
//  for y:=0 to ah-th-1 do
//    for x:=0 to aw-tw-1 do
//    begin
//      wndSum := sum[Y,X] - sum[Y,X+tw] - sum[Y+th,X] + sum[Y+th,X+tw];
//      numer  := xcorr[y,x] - (wndSum * tplMean);
//
//      if Normed then
//      begin
//        wndDiff := (sum2[Y,X] - sum2[Y,X+tw] - sum2[Y+th,X] + sum2[Y+th,X+tw]) - (Sqr(wndSum) * invSize);
//        if wndDiff < 0.1 then Continue; //shortcut - assume float error
//
//        denom := tplSigma * Sqrt(wndDiff);
//
//        if abs(numer) < denom then
//          Result[y,x] := numer / denom
//        else if abs(numer) < denom*1.25 then
//          if numer > 0 then Result[y,x] := 1 else Result[y,x] := -1;
//      end else
//        Result[y,x] := numer;
//    end;
//end;

// ----------------------------------------------------------------------------
// Cross correlation of R,G,B channels

function CCORR_RGB_HELPER(Image, Templ: T2DIntArray; out aR,aG,aB, tR,tG,tB: T2DSingleArray; out isa, isb, isg: T2DComplexArray): T2DSingleArray;
var
  x,y,W,H: Int32;
  xR,xG,xB: T2DSingleArray;
begin
  SplitRGB(Image, aR,aG,aB);
  SplitRGB(Templ, tR,tG,tB);

  xR := CCORR(aR,tR,isa);
  xG := CCORR(aG,tG,isb);
  xB := CCORR(aB,tB,isg);

  Result := xR;
  Size(Result, W,H);
  for y:=0 to H-1 do
    for x:=0 to W-1 do
      Result[y,x] := xR[y,x] + xG[y,x] + xB[y,x];
end;

function CCORR_RGB_HELPER_CACHE(Image: TMatchTemplateImageCache; Templ: T2DIntArray; out tR,tG,tB: T2DSingleArray; out isa, isb, isg: T2DComplexArray): T2DSingleArray;
var
  x,y,W,H: Int32;
  xR,xG,xB: T2DSingleArray;
begin
  SplitRGB(Templ, tR,tG,tB);

  xR := CCORR_CACHE(Image.w, image.h, image.R.Spec, tr);
  xG := CCORR_CACHE(Image.w, image.h, image.G.Spec, tG);
  xB := CCORR_CACHE(Image.w, image.h, image.B.Spec, tB);

  Result := xR;
  Size(Result, W,H);
  for y:=0 to H-1 do
    for x:=0 to W-1 do
      Result[y,x] := xR[y,x] + xG[y,x] + xB[y,x];
end;

function CCORR_RGB_CREATE_CACHE(Image, Templ: T2DIntArray): TMatchTemplateImageCache;
var
  aR,aG,aB: T2DSingleArray;
  tR,tG,tB: T2DSingleArray;
begin
  Size(Image, Result.W, Result.H);

  CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB, Result.R.Spec,Result.G.Spec,Result.B.Spec);

  Result.R.Sum := SumsPd(aR, Result.R.SumSquared);
  Result.G.Sum := SumsPd(aG, Result.G.SumSquared);
  Result.B.Sum := SumsPd(aB, Result.B.SumSquared);
end;


// ----------------------------------------------------------------------------
// [Normalized] cross correlation of R,G,B channels

function CCORR_RGB_CACHE(Image: TMatchTemplateImageCache; Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, mR,mG,mB, sR,sG,sB, wndSum2: Double;
  sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, tR,tG,tB: T2DSingleArray;
  isa, isb, isg: T2DComplexArray;
begin
  xcorr := CCORR_RGB_HELPER_CACHE(Image, Templ, tR,tG,tB, isa,isb,isg);
  if not Normed then
    Exit(xcorr);

  Size(Templ, tw,th);
  invSize := Double(1.0) / Double(tw*th);

  MeanStdev(tR, mR, sR); tR := nil;
  MeanStdev(tG, mG, sG); tG := nil;
  MeanStdev(tB, mB, sB); tB := nil;

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);

  sum2r := Image.R.SumSquared;
  sum2g := Image.G.SumSquared;
  sum2b := Image.B.SumSquared;

  Size(sum2r, aw,ah);
  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSum2 := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
      wndSum2 += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
      wndSum2 += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

      numer := xcorr[y,x];
      denom := tplSigma * Sqrt(wndSum2);

      if abs(numer) < denom then
        Result[y,x] := numer / denom
      else if abs(numer) < denom*1.25 then
        if numer > 0 then Result[y,x] := 1 else Result[y,x] := -1;
    end;
end;

function CCORR_RGB(Image, Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, mR,mG,mB, sR,sG,sB, wndSum2: Double;
  sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, aR,aG,aB, tR,tG,tB: T2DSingleArray;
  isa, isb, isg: T2DComplexArray;
begin
  xcorr := CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB, isa,isb,isg);
  if not Normed then
    Exit(xcorr);

  Size(Templ, tw,th);
  invSize := Double(1.0) / Double(tw*th);

  MeanStdev(tR, mR, sR); tR := nil;
  MeanStdev(tG, mG, sG); tG := nil;
  MeanStdev(tB, mB, sB); tB := nil;

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);

  SumsPd(aR, sum2r); aR := nil;
  SumsPd(aG, sum2g); aG := nil;
  SumsPd(aB, sum2b); aB := nil;

  Size(sum2r, aw,ah);
  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSum2 := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
      wndSum2 += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
      wndSum2 += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

      numer := xcorr[y,x];
      denom := tplSigma * Sqrt(wndSum2);

      if abs(numer) < denom then
        Result[y,x] := numer / denom
      else if abs(numer) < denom*1.25 then
        if numer > 0 then Result[y,x] := 1 else Result[y,x] := -1;
    end;
end;

// ----------------------------------------------------------------------------
// [Normalized] Cross correlation (coefficient) of R,G,B channels

function CCOEFF_RGB_CACHE(Image: TMatchTemplateImageCache; Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  mR,sR, mG,sG, mB,sB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, tR,tG,tB: T2DSingleArray;
  isa,isb,isg: T2DComplexArray;
begin
  xcorr := CCORR_RGB_HELPER_CACHE(Image, Templ, tR,tG,tB, isa,isb,isg);

  Size(Templ, tw,th);
  invSize := Double(1.0) / Double(tw*th);

  if not Normed then
  begin
    mR := Mean(tR); tR := nil;
    mG := Mean(tG); tG := nil;
    mB := Mean(tB); tB := nil;
    tplSigma := 0;
  end else
  begin
    MeanStdev(tR, mR, sR); tR := nil;
    MeanStdev(tG, mG, sG); tG := nil;
    MeanStdev(tB, mB, sB); tB := nil;

    tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

    if tplSdv < 0.00001 then
    begin
      InitMatrix(Result, Length(xcorr), Length(xcorr[0]), 1);
      Exit;
    end;

    tplSigma := Sqrt(tplSdv) / Sqrt(invSize);
  end;

  SumR := Image.R.Sum;
  SumG := Image.G.Sum;
  SumB := Image.B.Sum;

  sum2r := Image.R.SumSquared;
  sum2g := Image.G.SumSquared;
  sum2b := Image.B.SumSquared;

  Size(sumR, aw,ah);
  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSumR  := sumR[Y,X] - sumR[Y,X+tw] - sumR[Y+th,X] + sumR[Y+th,X+tw];
      wndSumG  := sumG[Y,X] - sumG[Y,X+tw] - sumG[Y+th,X] + sumG[Y+th,X+tw];
      wndSumB  := sumB[Y,X] - sumB[Y,X+tw] - sumB[Y+th,X] + sumB[Y+th,X+tw];

      numer    := xcorr[y,x] - ((wndSumR * mR) + (wndSumG * mG) + (wndSumB * mB));
      if Normed then
      begin
        wndSum2  := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
        wndSum2  += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
        wndSum2  += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

        wndMean2 := Sqr(wndSumR) + Sqr(wndSumG) + Sqr(wndSumB);
        wndMean2 := wndMean2 * invSize;

        denom := tplSigma * Sqrt(Max(0, wndSum2 - wndMean2));
        if abs(numer) < denom then
          Result[y,x] := numer / denom
        else if abs(numer) < denom*1.25 then
          if numer > 0 then Result[y,x] := 1 else Result[y,x] := -1;
      end else
        Result[y,x] := numer;
    end;
end;

function CCOEFF_RGB(Image, Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  mR,sR, mG,sG, mB,sB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, aR,aG,aB, tR,tG,tB: T2DSingleArray;
  isa,isb,isg: T2DComplexArray;
begin
  xcorr := CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB, isa,isb,isg);

  Size(Templ, tw,th);
  invSize := Double(1.0) / Double(tw*th);

  if not Normed then
  begin
    mR := Mean(tR); tR := nil;
    mG := Mean(tG); tG := nil;
    mB := Mean(tB); tB := nil;
    tplSigma := 0;
  end else
  begin
    MeanStdev(tR, mR, sR); tR := nil;
    MeanStdev(tG, mG, sG); tG := nil;
    MeanStdev(tB, mB, sB); tB := nil;

    tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

    if tplSdv < 0.00001 then
    begin
      InitMatrix(Result, Length(xcorr), Length(xcorr[0]), 1);
      Exit;
    end;

    tplSigma := Sqrt(tplSdv) / Sqrt(invSize);
  end;

  sumR := SumsPd(aR, sum2r); aR := nil;
  sumG := SumsPd(aG, sum2g); aG := nil;
  sumB := SumsPd(aB, sum2b); aB := nil;

  Size(sumR, aw,ah);
  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSumR  := sumR[Y,X] - sumR[Y,X+tw] - sumR[Y+th,X] + sumR[Y+th,X+tw];
      wndSumG  := sumG[Y,X] - sumG[Y,X+tw] - sumG[Y+th,X] + sumG[Y+th,X+tw];
      wndSumB  := sumB[Y,X] - sumB[Y,X+tw] - sumB[Y+th,X] + sumB[Y+th,X+tw];

      numer    := xcorr[y,x] - ((wndSumR * mR) + (wndSumG * mG) + (wndSumB * mB));
      if Normed then
      begin
        wndSum2  := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
        wndSum2  += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
        wndSum2  += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

        wndMean2 := Sqr(wndSumR) + Sqr(wndSumG) + Sqr(wndSumB);
        wndMean2 := wndMean2 * invSize;

        denom := tplSigma * Sqrt(Max(0, wndSum2 - wndMean2));
        if abs(numer) < denom then
          Result[y,x] := numer / denom
        else if abs(numer) < denom*1.25 then
          if numer > 0 then Result[y,x] := 1 else Result[y,x] := -1;
      end else
        Result[y,x] := numer;
    end;
end;

// ----------------------------------------------------------------------------
// [Normalized] square difference of R,G,B channels

function SQDIFF_RGB_CACHE(Image: TMatchTemplateImageCache; Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, mR,sR, mG,sG, mB,sB:Double;
  sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, tR,tG,tB: T2DSingleArray;
  isa,isb,isg: T2DComplexArray;
begin
  xcorr := CCORR_RGB_HELPER_CACHE(Image, Templ, tR,tG,tB, isa,isb,isg);

  Size(Templ, tw,th);
  invSize := Double(1.0) / Double(tw*th);

  MeanStdev(tR, mR, sR); tR := nil;
  MeanStdev(tG, mG, sG); tG := nil;
  MeanStdev(tB, mB, sB); tB := nil;

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);
  tplSum2  := (tplSdv + tplMean) / invSize;

  sum2r := Image.R.SumSquared;
  sum2g := Image.G.SumSquared;
  sum2b := Image.B.SumSquared;

  Size(sum2r, aw,ah);
  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSum2 := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
      wndSum2 += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
      wndSum2 += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

      numer   := Max(0, wndSum2 - Double(2.0)*xcorr[y,x] + tplSum2);
      if Normed then begin
        denom := tplSigma * Sqrt(wndSum2);
        if abs(numer) < denom then
          Result[y,x] := numer / denom
        else
          Result[y,x] := 1;
      end else
        Result[y,x] := numer;
    end;
end;

function SQDIFF_RGB(Image, Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, mR,sR, mG,sG, mB,sB:Double;
  sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, aR,aG,aB, tR,tG,tB: T2DSingleArray;
  isa,isb,isg: T2DComplexArray;
begin
  xcorr := CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB, isa,isb,isg);

  Size(Templ, tw,th);
  invSize := Double(1.0) / Double(tw*th);

  MeanStdev(tR, mR, sR); tR := nil;
  MeanStdev(tG, mG, sG); tG := nil;
  MeanStdev(tB, mB, sB); tB := nil;

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);
  tplSum2  := (tplSdv + tplMean) / invSize;
  
  SumsPd(aR, sum2r); aR := nil;
  SumsPd(aG, sum2g); aG := nil;
  SumsPd(aB, sum2b); aB := nil;

  Size(sum2r, aw,ah);
  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSum2 := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
      wndSum2 += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
      wndSum2 += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

      numer   := Max(0, wndSum2 - Double(2.0)*xcorr[y,x] + tplSum2);
      if Normed then begin
        denom := tplSigma * Sqrt(wndSum2);
        if abs(numer) < denom then
          Result[y,x] := numer / denom
        else
          Result[y,x] := 1;
      end else
        Result[y,x] := numer;
    end;
end;


// ----------------------------------------------------------------------------
// Template matching using some of the above formulas

function MatchTemplate(constref Image, Templ: T2DIntArray; TMFormula: Int32): T2DSingleArray;
begin
  case TMFormula of
    MATCH_CCORR:         Result := CCORR_RGB(Image, Templ, False);
    MATCH_CCORR_NORMED:  Result := CCORR_RGB(Image, Templ, True);
    MATCH_CCOEFF:        Result := CCOEFF_RGB(Image, Templ, False);
    MATCH_CCOEFF_NORMED: Result := CCOEFF_RGB(Image, Templ, True);
    MATCH_SQDIFF:        Result := SQDIFF_RGB(Image, Templ, False);
    MATCH_SQDIFF_NORMED: Result := SQDIFF_RGB(Image, Templ, True);
    else
      raise Exception.Create('Formula not implemented');
  end;
end;

function MatchTemplate(constref Image, Templ: T2DIntArray; TMFormula: Int32; var Cache: TMatchTemplateImageCache): T2DSingleArray;
begin
  if (Cache.W = 0) and (Cache.H = 0) then
    Cache := CCORR_RGB_CREATE_CACHE(Image, Templ);

  case TMFormula of
    MATCH_CCORR:         Result := CCORR_RGB_CACHE(Cache, Templ, False);
    MATCH_CCORR_NORMED:  Result := CCORR_RGB_CACHE(Cache, Templ, True);
    MATCH_CCOEFF:        Result := CCOEFF_RGB_CACHE(Cache, Templ, False);
    MATCH_CCOEFF_NORMED: Result := CCOEFF_RGB_CACHE(Cache, Templ, True);
    MATCH_SQDIFF:        Result := SQDIFF_RGB_CACHE(Cache, Templ, False);
    MATCH_SQDIFF_NORMED: Result := SQDIFF_RGB_CACHE(Cache, Templ, True);
    else
      raise Exception.Create('Formula not implemented');
  end;
end;

end.
