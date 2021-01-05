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

  2021: Olly brings mask template matching thanks to OpenCV:
    https://github.com/opencv/opencv/blob/5b095dfcb669f87c2a767806a3dd9bd56e035a1c/modules/imgproc/src/templmatch.cpp#L837

    - Currently only TM_CCOEFF and TM_CCOEFF_NORMED.
    - Greyscale & RGB Image.
    - Cache system if Image & Mask are static.

[==============================================================================}
{$I header.inc}

interface

uses
  classes, sysutils, math,
  simba.mufasatypes, simba.matchtemplate_matrix, simba.bitmap;

type
  PTMFormula = ^ETMFormula;
  ETMFormula = (
    TM_CCORR,
    TM_CCORR_NORMED,
    TM_CCOEFF,
    TM_CCOEFF_NORMED,
    TM_SQDIFF,
    TM_SQDIFF_NORMED
  );

  PMatchTemplateGreyCache = ^TMatchTemplateGreyCache;
  TMatchTemplateGreyCache = packed record
    Width: Int32;
    Height: Int32;

    ImageSpec: record
      Grey: TComplexMatrix;
    end;
    ImageSpecSquared: record
      Grey: TComplexMatrix;
    end;

    MaskChannel: TSingleMatrix;
    MaskSum: Double;
    MaskSumSquared: Double;

    ImgNormCorr: TSingleMatrix;
    ImgMaskCorr: TSingleMatrix;

    class function Create(constref Image, Mask: TByteMatrix): TMatchTemplateGreyCache; static;
  end;

  PMatchTemplateRGBCache = ^TMatchTemplateRGBCache;
  TMatchTemplateRGBCache = packed record
    Width: Int32;
    Height: Int32;

    ImageSpec: record
      R, G, B: TComplexMatrix;
    end;
    ImageSpecSquared: record
      R, G, B: TComplexMatrix;
    end;

    MaskChannel: TRGBMatrix;
    MaskSum: TDoubleArray;
    MaskSumSquared: TDoubleArray;

    ImgNormCorr: TSingleMatrix;
    ImgMaskCorr: TRGBMatrix;

    class function Create(constref Image, Mask: TIntegerMatrix): TMatchTemplateRGBCache; static;
  end;

function MatchTemplateMask(constref Cache: TMatchTemplateGreyCache; constref Templ: TByteMatrix;  Formula: ETMFormula): TSingleMatrix;
function MatchTemplateMask(constref Image, Templ, Mask: TByteMatrix; Formula: ETMFormula): TSingleMatrix;

function MatchTemplateMask(constref Cache: TMatchTemplateRGBCache; constref Templ: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
function MatchTemplateMask(constref Image, Templ, Mask: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;

function MatchTemplate(constref Image, Templ: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;

type
  TMufasaBitmap_Helper = class helper for TMufasaBitmap
    function MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
    function MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
    function FindTemplate(Template: TMufasaBitmap; Formula: ETMFormula; MinMatch: Extended): TPoint;
    function FindTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula; MinMatch: Extended): TPoint;
  end;

implementation

uses
  simba.threadpool, simba.FFTPACK4, simba.matrix, simba.colormath;

{$DEFINE VALIDATE :=
  if (Image.Width = 0) or (Image.Height = 0) then
    raise Exception.Create('MatchTemplate: Image is empty');
  if (Templ.Width = 0) or (Templ.Height = 0) then
    raise Exception.Create('MatchTemplate: Template is empty');
  if (Templ.Width > Image.Width) or (Templ.Height > Templ.Height) then
    raise Exception.Create('MatchTemplate: Template is bigger than Image');
}

{$DEFINE VALIDATE_MASK :=
  if (Cache.Width = 0) or (Cache.Height = 0) then
    raise Exception.Create('MatchTemplateMask: Cache is empty');
  if (Templ.Width = 0) or (Templ.Height = 0) then
    raise Exception.Create('MatchTemplateMask: Template is empty');
  if (Templ.Width > Cache.Width) or (Templ.Height > Cache.Height) then
    raise Exception.Create('MatchTemplateMask: Template must be smaller than Image');
  if (Templ.Width <> Cache.MaskChannel.Width) or (Templ.Height <> Cache.MaskChannel.Height) then
    raise Exception.Create('MatchTemplateMask: Mask and template must be equal sizes');
}

{$DEFINE VALIDATE_MASK_CACHE :=
  if (Image.Width = 0) or (Image.Height = 0) then
    raise Exception.Create('MatchTemplateMaskCache: Image is empty');
  if (Mask.Width = 0) or (Mask.Height = 0) then
    raise Exception.Create('MatchTemplateMaskCache: Mask is empty');
  if (Mask.Width > Image.Width) or (Mask.Height > Image.Height) then
    raise Exception.Create('MatchTemplateMaskCache: Mask must be smaller than image');
}

// -----------------------------------------------------------------------------
// Helpers

function DoFFT2(Matrix: TSingleMatrix; outW, outH: Int32): TComplexMatrix;
var
  X, Y, W, H: Int32;
  Spec: TComplexMatrix;
begin
  W := Matrix.Width - 1;
  H := Matrix.Height - 1;

  SetLength(Spec, FFTPACK.OptimalDFTSize(outH), FFTPACK.OptimalDFTSize(outW));
  for Y := 0 to H do
    for X := 0 to W do
      Spec[Y, X].Re := Matrix[Y, X];

  Result := FFTPACK.FFT2(Spec);
end;

function DoIFFT2(Spec: TComplexMatrix; outW, outH: Int32): TSingleMatrix;
var
  X, Y: Int32;
begin
  SetLength(Result, outH, outW);

  Dec(outW);
  Dec(outH);

  Spec := FFTPACK.IFFT2(Spec);

  for Y := 0 to outH do
    for X := 0 to outW do
      Result[Y, X] := Spec[Y, X].Re;
end;

procedure InitMatrix(out Matrix: TSingleMatrix; H, W: Int32; InitValue: Int32);
var
  X, Y: Int32;
begin
  SetLength(Matrix, H, W);

  Dec(W);
  Dec(H);

  for Y := 0 to H do
    for X := 0 to W do
      Matrix[Y, X] := InitValue;
end;

// -----------------------------------------------------------------------------
// a * conj(b)

procedure Parallel_MulSpecConj(Params: PParamArray; iLow, iHigh: Int32);
var
  x,y,w: Int32;
  a,b,r: TComplexMatrix;
  re,im: Single;
begin
  a := TComplexMatrix(Params^[0]^);
  b := TComplexMatrix(Params^[1]^);
  r := TComplexMatrix(Params^[2]^);

  w := a.Width - 1;

  for y := iLow to iHigh do
    for x := 0 to w do
      begin
        re := (a[y,x].re *  b[y,x].re) - (a[y,x].im * -b[y,x].im);
        im := (a[y,x].re * -b[y,x].im) + (a[y,x].im *  b[y,x].re);
        r[y,x].re := re;
        r[y,x].im := im;
      end;
end;

function MulSpectrumConj(constref A, B: TComplexMatrix): TComplexMatrix;
begin
  SetLength(Result, A.Height, A.Width);

  SimbaThreadPool.RunParallel(@Parallel_MulSpecConj, [@A, @B, @Result], Low(A), High(B), Area(A) < 300*300, 2);
end;

// -----------------------------------------------------------------------------
// cross correlate

function CCORR(constref Image, Templ: TSingleMatrix): TSingleMatrix;
var
  Spec: TComplexMatrix;
begin
  Spec := MulSpectrumConj(DoFFT2(Image, Image.Width, Image.Height), DoFFT2(Templ, Image.Width, Image.Height));

  Result := DoIFFT2(Spec, Image.Width - Templ.Width + 1, Image.Height - Templ.Height + 1);
end;

function CCORR(ImageWidth, ImageHeight: Int32; constref Image: TComplexMatrix; constref Templ: TSingleMatrix): TSingleMatrix;
var
  Spec: TComplexMatrix;
begin
  Spec := MulSpectrumConj(Image, DoFFT2(Templ, ImageWidth, ImageHeight));

  Result := DoIFFT2(Spec, ImageWidth - Templ.Width + 1, ImageHeight - Templ.Height + 1);
end;

// -----------------------------------------------------------------------------
// Cross correlation of R,G,B channels

function CCORR_RGB(Image, Templ: TIntegerMatrix; out aR,aG,aB, tR,tG,tB: TSingleMatrix): TSingleMatrix;
var
  x,y,W,H: Int32;
  xR,xG,xB: TSingleMatrix;
begin
  SplitRGB(Image, aR,aG,aB);
  SplitRGB(Templ, tR,tG,tB);

  xR := CCORR(aR,tR);
  xG := CCORR(aG,tG);
  xB := CCORR(aB,tB);

  Result := xR;

  W := Result.Width - 1;
  H := Result.Height - 1;

  for y:=0 to H do
    for x:=0 to W do
      Result[y,x] := xR[y,x] + xG[y,x] + xB[y,x];
end;

function CCORR_RGB(ImageWidth, ImageHeight: Int32; constref ImageR, ImageG, ImageB: TComplexMatrix; Templ: TRGBMatrix): TRGBMatrix;
begin
  Result.R := CCORR(ImageWidth, ImageHeight, ImageR, Templ.R);
  Result.G := CCORR(ImageWidth, ImageHeight, ImageG, Templ.G);
  Result.B := CCORR(ImageWidth, ImageHeight, ImageB, Templ.B);
end;

// ----------------------------------------------------------------------------
// [Normalized] cross correlation of R,G,B channels

function CCORR_RGB(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, mR,mG,mB, sR,sG,sB, wndSum2: Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  xcorr, aR,aG,aB, tR,tG,tB: TSingleMatrix;
begin
  xcorr := CCORR_RGB(Image, Templ, aR,aG,aB, tR,tG,tB);

  if not Normed then
    Exit(xcorr);

  tw := Templ.Width;
  th := Templ.Height;

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

  aw := sum2r.Width;
  ah := sum2r.Height;

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

// -----------------------------------------------------------------------------
// Cross correlation (coefficient) of single channel matrices

function CCOEFF_1C(Image, Templ: TSingleMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, wndDiff, wndSum: Double;
  xcorr: TSingleMatrix;
  sum, sum2: TDoubleMatrix;
begin
  xcorr := CCORR(Image, Templ);

  tw := Templ.Width;
  th := Templ.Height;

  invSize := Double(1.0) / Double(tw*th);
  MeanStdev(Templ, tplMean, tplSdv);
  tplSigma := tplSdv / Sqrt(invSize);

  if tplSdv < 0.00001 then
  begin
    InitMatrix(Result, Length(xcorr), Length(xcorr[0]), 1);
    Exit;
  end;

  sum := SumsPd(Image, sum2);

  aw := sum.Width;
  ah := sum.Height;

  SetLength(Result, ah-th, aw-tw);
  for y:=0 to ah-th-1 do
    for x:=0 to aw-tw-1 do
    begin
      wndSum := sum[Y,X] - sum[Y,X+tw] - sum[Y+th,X] + sum[Y+th,X+tw];
      numer  := xcorr[y,x] - (wndSum * tplMean);

      if Normed then
      begin
        wndDiff := (sum2[Y,X] - sum2[Y,X+tw] - sum2[Y+th,X] + sum2[Y+th,X+tw]) - (Sqr(wndSum) * invSize);
        if wndDiff < 0.1 then Continue; //shortcut - assume float error

        denom := tplSigma * Sqrt(wndDiff);

        if abs(numer) < denom then
          Result[y,x] := numer / denom
        else if abs(numer) < denom*1.25 then
          if numer > 0 then Result[y,x] := 1 else Result[y,x] := -1;
      end else
        Result[y,x] := numer;
    end;
end;

// -----------------------------------------------------------------------------
// [Normalized] Cross correlation (coefficient) of R,G,B channels

function CCOEFF_RGB(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  mR,sR, mG,sG, mB,sB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: TDoubleMatrix;
  xcorr, aR,aG,aB, tR,tG,tB: TSingleMatrix;
begin
  xcorr := CCORR_RGB(Image, Templ, aR,aG,aB, tR,tG,tB);

  tw := Templ.Width;
  th := Templ.Height;

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

  aw := sumR.Width;
  ah := sumR.Height;

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

function SQDIFF_RGB(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, mR,sR, mG,sG, mB,sB:Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  xcorr, aR,aG,aB, tR,tG,tB: TSingleMatrix;
begin
  xcorr := CCORR_RGB(Image, Templ, aR,aG,aB, tR,tG,tB);

  tw := Templ.Width;
  th := Templ.Height;

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

  aw := sum2r.Width;
  ah := sum2r.Height;

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

class function TMatchTemplateGreyCache.Create(constref Image, Mask: TByteMatrix): TMatchTemplateGreyCache;
var
  Grey: TSingleMatrix;
  X, Y: Int32;
begin
  VALIDATE_MASK_CACHE

  Result.Width := Length(Image[0]);
  Result.Height := Length(Image);

  SetLength(Grey, Result.Height, Result.Width);
  for Y := 0 to Result.Height-1 do
    for X := 0 to Result.Width-1 do
      Grey[Y, X] := Image[Y, X];

  Result.ImageSpec.Grey := DoFFT2(Grey, Result.Width, Result.Height);
  Result.ImageSpecSquared.Grey := DoFFT2(Grey*Grey, Result.Width, Result.Height);

  SetLength(Result.MaskChannel, Length(Mask), Length(Mask[0]));
  for Y := 0 to High(Mask) do
    for X := 0 to High(Mask[0]) do
    begin
      Result.MaskChannel[Y, X] := Mask[Y, X];
      if (Result.MaskChannel[Y, X] > 0) then
        Result.MaskChannel[Y, X] := 1;
    end;

  Result.MaskSum := Sum(Result.MaskChannel);
  Result.MaskSumSquared := Sum(Result.MaskChannel * Result.MaskChannel);

  Result.ImgMaskCorr := CCORR(Result.Width, Result.Height, Result.ImageSpec.Grey, Result.MaskChannel);
  Result.ImgNormCorr := Sqrt(
    CCORR(Result.Width, Result.Height, Result.ImageSpecSquared.Grey, Result.MaskChannel * Result.MaskChannel) +
         (Result.ImgMaskCorr * 1 / Result.MaskSum) *
         (Result.ImgMaskCorr * Result.MaskSumSquared / Result.MaskSum - CCORR(Result.Width, Result.Height, Result.ImageSpec.Grey, Result.MaskChannel * Result.MaskChannel) * 2)
  );
end;

class function TMatchTemplateRGBCache.Create(constref Image, Mask: TIntegerMatrix): TMatchTemplateRGBCache;
var
  R, G, B: TSingleMatrix;
  X, Y, W, H: Int32;
begin
  VALIDATE_MASK_CACHE

  SplitRGB(Image, R, G, B);

  Result.Width := Image.Width;
  Result.Height := Image.Height;

  Result.ImageSpec.R := DoFFT2(R, Result.Width, Result.Height);
  Result.ImageSpec.G := DoFFT2(G, Result.Width, Result.Height);
  Result.ImageSpec.B := DoFFT2(B, Result.Width, Result.Height);

  Result.ImageSpecSquared.R := DoFFT2(R*R, Result.Width, Result.Height);
  Result.ImageSpecSquared.G := DoFFT2(G*G, Result.Width, Result.Height);
  Result.ImageSpecSquared.B := DoFFT2(B*B, Result.Width, Result.Height);

  SplitRGB(Mask, Result.MaskChannel.R, Result.MaskChannel.G, Result.MaskChannel.B);

  W := Result.MaskChannel.Width - 1;
  H := Result.MaskChannel.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      if Result.MaskChannel.R[Y, X] <> 0 then
        Result.MaskChannel.R[Y, X] := 1;
      if Result.MaskChannel.G[Y, X] <> 0 then
        Result.MaskChannel.G[Y, X] := 1;
      if Result.MaskChannel.B[Y, X] <> 0 then
        Result.MaskChannel.B[Y, X] := 1;
    end;

  Result.MaskSum := Sum(Result.MaskChannel);
  Result.MaskSumSquared := Sum(Result.MaskChannel * Result.MaskChannel);

  Result.ImgMaskCorr := CCORR_RGB(Result.Width, Result.Height, Result.ImageSpec.R, Result.ImageSpec.G, Result.ImageSpec.B, Result.MaskChannel);
  Result.ImgNormCorr := Sqrt(
    CCORR_RGB(Result.Width, Result.Height, Result.ImageSpecSquared.R, Result.ImageSpecSquared.G, Result.ImageSpecSquared.B, Result.MaskChannel * Result.MaskChannel) +
             (Result.ImgMaskCorr * [1 / Result.MaskSum[0], 1 / Result.MaskSum[1], 1 / Result.MaskSum[2]]) *
             (Result.ImgMaskCorr * [Result.MaskSumSquared[0] / Result.MaskSum[0], Result.MaskSumSquared[1] / Result.MaskSum[1], Result.MaskSumSquared[2] / Result.MaskSum[2]] - CCORR_RGB(Result.Width, Result.Height, Result.ImageSpec.R, Result.ImageSpec.G, Result.ImageSpec.B, Result.MaskChannel * Result.MaskChannel) * [2, 2, 2])
  );
end;

function MatchTemplateMask(constref Cache: TMatchTemplateGreyCache; constref Templ: TByteMatrix; Formula: ETMFormula): TSingleMatrix;
var
  TemplChannel, TemplxMask: TSingleMatrix;
  TemplxMaskSum: Double;
  X, Y, W, H: Int32;
begin
  VALIDATE_MASK

  W := Templ.Width - 1;
  H := Templ.Height - 1;

  SetLength(TemplChannel, H + 1, W + 1);
  for Y := 0 to H do
    for X := 0 to W do
      TemplChannel[Y, X] := Templ[Y, X];

  case Formula of
    TM_CCOEFF, TM_CCOEFF_NORMED:
      begin
        TemplxMask := Cache.MaskChannel * (Cache.MaskChannel * (TemplChannel - Sum(Cache.MaskChannel * TemplChannel) / Cache.MaskSum));
        TemplxMaskSum := Sum(TemplxMask);

        Result := CCORR(Cache.Width, Cache.Height, Cache.ImageSpec.Grey, TemplxMask) - Cache.ImgMaskCorr * TemplxMaskSum / Cache.MaskSum;
        if (Formula = TM_CCOEFF_NORMED) then
          Result /= Cache.ImgNormCorr * Norm(TemplxMask);
      end;

    else
      raise Exception.Create('MatchTemplateMask: Formula not implemented');
  end;
end;

function MatchTemplateMask(constref Image, Templ, Mask: TByteMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Result := MatchTemplateMask(TMatchTemplateGreyCache.Create(Image, Mask), Templ, Formula);
end;

function MatchTemplateMask(constref Cache: TMatchTemplateRGBCache; constref Templ: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
var
  TemplChannel, TemplxMask, Corr: TRGBMatrix;
  MaskTemplSum, TemplxMaskSum: TDoubleArray;
  X, Y, W, H: Int32;
begin
  VALIDATE_MASK

  SplitRGB(Templ, TemplChannel.R, TemplChannel.G, TemplChannel.B);

  case Formula of
    TM_CCOEFF, TM_CCOEFF_NORMED:
      begin
        MaskTemplSum := Sum(Cache.MaskChannel * TemplChannel);

        TemplxMask := Cache.MaskChannel * (Cache.MaskChannel * (TemplChannel - [MaskTemplSum[0] / Cache.MaskSum[0], MaskTemplSum[1] / Cache.MaskSum[1], MaskTemplSum[2] / Cache.MaskSum[2]]));
        TemplxMaskSum := Sum(TemplxMask);

        Corr := CCORR_RGB(Cache.Width, Cache.Height, Cache.ImageSpec.R, Cache.ImageSpec.G, Cache.ImageSpec.B, TemplxMask) -
                          Cache.ImgMaskCorr * [TemplxMaskSum[0] / Cache.MaskSum[0], TemplxMaskSum[1] / Cache.MaskSum[1], TemplxMaskSum[2] / Cache.MaskSum[2]];

        Result := Corr.R;

        W := Result.Width - 1;
        H := Result.Height - 1;

        for Y := 0 to H do
          for X := 0 to W do
            Result[Y, X] += Corr.G[Y, X] + Corr.B[Y, X];

        if (Formula = TM_CCOEFF_NORMED) then
          Result /= Cache.ImgNormCorr * Norm(TemplxMask);
      end;

    else
      raise Exception.Create('MatchTemplateMask: Formula not implemented');
  end;
end;

function MatchTemplateMask(constref Image, Templ, Mask: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  Result := MatchTemplateMask(TMatchTemplateRGBCache.Create(Image, Mask), Templ, Formula);
end;

function MatchTemplate(constref Image, Templ: TIntegerMatrix; Formula: ETMFormula): TSingleMatrix;
begin
  VALIDATE

  case Formula of
    TM_CCORR:         Result := CCORR_RGB(Image, Templ, False);
    TM_CCORR_NORMED:  Result := CCORR_RGB(Image, Templ, True);
    TM_CCOEFF:        Result := CCOEFF_RGB(Image, Templ, False);
    TM_CCOEFF_NORMED: Result := CCOEFF_RGB(Image, Templ, True);
    TM_SQDIFF:        Result := SQDIFF_RGB(Image, Templ, False);
    TM_SQDIFF_NORMED: Result := SQDIFF_RGB(Image, Templ, True);
    else
      raise Exception.Create('MatchTemplate: Formula not implemented');
  end;
end;

function TMufasaBitmap_Helper.MatchTemplate(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
var
  Image, Templ: TIntegerMatrix;
  Y: Int32;
begin
  SetLength(Image, Self.Height, Self.Width);
  SetLength(Templ, Template.Height, Template.Width);

  for Y := 0 to Self.Height-1 do
    Move(Self.Data[Y * Self.Width], Image[Y, 0], Self.Width * SizeOf(Int32));

  for Y := 0 to Template.Height-1 do
    Move(Template.Data[Y * Template.Width], Templ[Y, 0], Template.Width * SizeOf(Int32));

  Result := simba.matchtemplate.MatchTemplate(Image, Templ, Formula);
end;

function TMufasaBitmap_Helper.MatchTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula): TSingleMatrix;
var
  Image, Templ, Mask: TIntegerMatrix;
  X, Y, W, H: Int32;
  Transparent: Int32;
begin
  SetLength(Image, Self.Height, Self.Width);
  SetLength(Templ, Template.Height, Template.Width);
  SetLength(Mask, Template.Height, Template.Width);

  for Y := 0 to Self.Height-1 do
    Move(Self.Data[Y * Self.Width], Image[Y, 0], Self.Width * SizeOf(Int32));

  for Y := 0 to Template.Height-1 do
    Move(Template.Data[Y * Template.Width], Templ[Y, 0], Template.Width * SizeOf(Int32));

  if Template.TransparentColorSet then
    Transparent := Int32(RGBToBGR(Template.GetTransparentColor))
  else
    Transparent := 0; // Need something?

  W := Template.Width - 1;
  H := Template.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      if (Templ[Y, X] and $FFFFFF) <> Transparent then // Mask out transparent color
        Mask[Y, X] := $FFFFFF
      else
        Mask[Y, X] := 0;
    end;

  Result := simba.matchtemplate.MatchTemplateMask(Image, Templ, Mask, Formula);
end;

function TMufasaBitmap_Helper.FindTemplate(Template: TMufasaBitmap; Formula: ETMFormula; MinMatch: Extended): TPoint;
var
  Corr: TSingleMatrix;
begin
  Corr := Self.MatchTemplate(Template, Formula);

  if Formula in [TM_SQDIFF, TM_SQDIFF_NORMED] then
  begin
    Result := MatrixArgMin(Corr);
    if Corr[Result.Y, Result.X] > MinMatch then
      Result := Point(-1, -1);
  end else
  begin
    Result := MatrixArgMax(Corr);
    if Corr[Result.Y, Result.X] < MinMatch then
      Result := Point(-1, -1);
  end;
end;

function TMufasaBitmap_Helper.FindTemplateMask(Template: TMufasaBitmap; Formula: ETMFormula; MinMatch: Extended): TPoint;
var
  Corr: TSingleMatrix;
begin
  Corr := Self.MatchTemplateMask(Template, Formula);

  if Formula in [TM_SQDIFF, TM_SQDIFF_NORMED] then
  begin
    Result := MatrixArgMin(Corr);
    if Corr[Result.Y, Result.X] > MinMatch then
      Result := Point(-1, -1);
  end else
  begin
    Result := MatrixArgMax(Corr);
    if Corr[Result.Y, Result.X] < MinMatch then
      Result := Point(-1, -1);
  end;
end;

end.
