unit simba.matchtemplate;
{==============================================================================]
  Copyright © 2021, Jarl Krister Holta

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
    - Cache system if Image & Mask are static.

[==============================================================================}
{$DEFINE SIMBA_O4}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

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

  PMatchTemplateCache = ^TMatchTemplateCache;
  TMatchTemplateCache = class
  public
    constructor Create(const Image, Templ: TIntegerMatrix); virtual; abstract;
  end;
  TMatchTemplateCacheClass = class of TMatchTemplateCache;

  TSimbaMatchTemplateClass = class of TSimbaMatchTemplate;
  TSimbaMatchTemplate = class
  protected
    class procedure Validate(const ImageWidth, ImageHeight, TemplateWidth, TemplateHeight: Integer);
  public
    class function CacheClass: TMatchTemplateCacheClass; virtual;

    class function MatchTemplateMask(const Cache: TMatchTemplateCache; const Template: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix; virtual; overload;
    class function MatchTemplateMask(const Image, Template: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix; virtual; overload;

    class function MatchTemplate(const Image, Template: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix; virtual;
  end;

var
  SimbaMatchTemplate: TSimbaMatchTemplateClass;

implementation

uses
  math,
  simba.FFTPACK4, simba.matrixhelpers, simba.matchtemplate_matrix, simba.matchtemplate_multithread;

// -----------------------------------------------------------------------------
// Helpers

function DoFFT2(const Matrix: TSingleMatrix; const outW, outH: Integer): TComplexMatrix;
var
  Spec: TComplexMatrix;
  X, Y, W, H: Integer;
begin
  Spec.SetSize(FFTPACK.OptimalDFTSize(OutW), FFTPACK.OptimalDFTSize(OutH));

  W := Matrix.Width - 1;
  H := Matrix.Height - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Spec[Y, X].Re := Matrix[Y, X];

  Result := FFTPACK.FFT2(Spec);
end;

function DoIFFT2(Spec: TComplexMatrix; const outW, outH: Integer): TSingleMatrix;
var
  X, Y, W, H: Integer;
begin
  Spec := FFTPACK.IFFT2(Spec);

  Result.SetSize(OutW, OutH);

  W := OutW - 1;
  H := OutH - 1;
  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := Spec[Y, X].Re;
end;

// -----------------------------------------------------------------------------
// a * conj(b)

function MulSpectrumConj(const A, B: TComplexMatrix): TComplexMatrix;
var
  X, Y, W, H: Integer;
begin
  Result.SetSize(A.Width, A.Height);

  W := A.Width - 1;
  H := A.Height - 1;

  for Y := 0 to H do
    for X := 0 to W do
    begin
      Result[y,x].re := (a[y,x].re *  b[y,x].re) - (a[y,x].im * -b[y,x].im);
      Result[y,x].im := (a[y,x].re * -b[y,x].im) + (a[y,x].im *  b[y,x].re);
    end;
end;

// -----------------------------------------------------------------------------
// Cross correlate

function CCORR(const Image, Templ: TSingleMatrix): TSingleMatrix;
var
  Spec: TComplexMatrix;
begin
  Spec := MulSpectrumConj(DoFFT2(Image, Image.Width, Image.Height), DoFFT2(Templ, Image.Width, Image.Height));

  Result := DoIFFT2(Spec, Image.Width - Templ.Width + 1, Image.Height - Templ.Height + 1);
end;

function CCORR(ImageWidth, ImageHeight: Integer; const Image: TComplexMatrix; const Templ: TSingleMatrix): TSingleMatrix;
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
  x,y,W,H: Integer;
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

  for Y := 0 to H do
    for X := 0 to W do
      Result[Y, X] := xR[Y, X] + xG[Y, X] + xB[Y, X];
end;

function CCORR_RGB(ImageWidth, ImageHeight: Integer; const ImageR, ImageG, ImageB: TComplexMatrix; Templ: TRGBMatrix): TRGBMatrix;
begin
  Result.R := CCORR(ImageWidth, ImageHeight, ImageR, Templ.R);
  Result.G := CCORR(ImageWidth, ImageHeight, ImageG, Templ.G);
  Result.B := CCORR(ImageWidth, ImageHeight, ImageB, Templ.B);
end;

// ----------------------------------------------------------------------------
// [Normalized] cross correlation of R,G,B channels

function CCORR_RGB(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,w,h,tw,th,iw,ih: Integer;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, mR,mG,mB, sR,sG,sB, wndSum2: Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  xcorr, aR,aG,aB, tR,tG,tB: TSingleMatrix;
begin
  xcorr := CCORR_RGB(Image, Templ, aR,aG,aB, tR,tG,tB);
  if not Normed then
    Exit(xcorr);

  tw := Templ.Width;
  th := Templ.Height;

  invSize := Double(1.0) / Double(tw * th);

  tR.MeanStdev(mR, sR);
  tG.MeanStdev(mG, sG);
  tB.MeanStdev(mB, sB);

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);

  SumsPd(aR, sum2r);
  SumsPd(aG, sum2g);
  SumsPd(aB, sum2b);

  iw := sum2r.Width;
  ih := sum2r.Height;

  Result.SetSize(iw-tw, ih-th);

  w := iw-tw-1;
  h := ih-th-1;
  for y := 0 to h do
    for x := 0 to w do
    begin
      wndSum2 := sum2r[Y,X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
      wndSum2 += sum2g[Y,X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
      wndSum2 += sum2b[Y,X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

      numer := xcorr[Y, X];
      denom := tplSigma * Sqrt(wndSum2);

      if Abs(numer) < denom then
        Result[Y, X] := numer / denom
      else if abs(numer) < denom*1.25 then
        if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
    end;
end;

// -----------------------------------------------------------------------------
// [Normalized] Cross correlation (coefficient) of R,G,B channels

function CCOEFF_RGB(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,w,h,tw,th,iw,ih: Integer;
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
    mR := tR.Mean();
    mG := tG.Mean();
    mB := tB.Mean();
    tplSigma := 0;
  end else
  begin
    tR.MeanStdev(mR, sR);
    tG.MeanStdev(mG, sG);
    tB.MeanStdev(mB, sB);

    tplSdv := Sqr(sR) + Sqr(sG) + Sqr(sB);
    if tplSdv < 0.00001 then
    begin
      Result.SetSize(xcorr.Width, xcorr.Height);
      Result.Fill(1);
      Exit;
    end;

    tplSigma := Sqrt(tplSdv) / Sqrt(invSize);
  end;

  sumR := SumsPd(aR, sum2r);
  sumG := SumsPd(aG, sum2g);
  sumB := SumsPd(aB, sum2b);

  iw := sum2r.Width;
  ih := sum2r.Height;

  Result.SetSize(iw-tw, ih-th);

  w := iw-tw-1;
  h := ih-th-1;
  for y := 0 to h do
    for x := 0 to w do
    begin
      wndSumR  := sumR[Y, X] - sumR[Y,X+tw] - sumR[Y+th,X] + sumR[Y+th,X+tw];
      wndSumG  := sumG[Y, X] - sumG[Y,X+tw] - sumG[Y+th,X] + sumG[Y+th,X+tw];
      wndSumB  := sumB[Y, X] - sumB[Y,X+tw] - sumB[Y+th,X] + sumB[Y+th,X+tw];

      numer    := xcorr[Y, X] - ((wndSumR * mR) + (wndSumG * mG) + (wndSumB * mB));
      if Normed then
      begin
        wndSum2  := sum2r[Y, X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
        wndSum2  += sum2g[Y, X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
        wndSum2  += sum2b[Y, X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

        wndMean2 := Sqr(wndSumR) + Sqr(wndSumG) + Sqr(wndSumB);
        wndMean2 := wndMean2 * invSize;

        denom := tplSigma * Sqrt(Max(0, wndSum2 - wndMean2));
        if abs(numer) < denom then
          Result[Y, X] := numer / denom
        else if abs(numer) < denom*1.25 then
          if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
      end else
        Result[Y, X] := numer;
    end;
end;

// ----------------------------------------------------------------------------
// [Normalized] square difference of R,G,B channels

function SQDIFF_RGB(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,w,h,tw,th,iw,ih: Integer;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, mR,sR, mG,sG, mB,sB:Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  xcorr, aR,aG,aB, tR,tG,tB: TSingleMatrix;
begin
  xcorr := CCORR_RGB(Image, Templ, aR,aG,aB, tR,tG,tB);

  tw := Templ.Width;
  th := Templ.Height;

  invSize := Double(1.0) / Double(tw*th);

  tR.MeanStdev(mR, sR);
  tG.MeanStdev(mG, sG);
  tB.MeanStdev(mB, sB);

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);
  tplSum2  := (tplSdv + tplMean) / invSize;

  SumsPd(aR, sum2r);
  SumsPd(aG, sum2g);
  SumsPd(aB, sum2b);

  iw := sum2r.Width;
  ih := sum2r.Height;

  Result.SetSize(iw-tw, ih-th);

  w := iw-tw-1;
  h := ih-th-1;
  for y := 0 to h do
    for x := 0 to w do
    begin
      wndSum2 := sum2r[Y, X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
      wndSum2 += sum2g[Y, X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
      wndSum2 += sum2b[Y, X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

      numer   := Max(0, wndSum2 - Double(2.0) * xcorr[Y, X] + tplSum2);
      if Normed then
      begin
        denom := tplSigma * Sqrt(wndSum2);
        if abs(numer) < denom then
          Result[Y, X] := numer / denom
        else
          Result[Y, X] := 1;
      end else
        Result[Y, X] := numer;
    end;
end;

type
  TMatchTemplateCachedImage = class(TMatchTemplateCache)
  protected
    Width: Integer;
    Height: Integer;

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
  public
    constructor Create(const Image, Templ: TIntegerMatrix); override;
  end;

constructor TMatchTemplateCachedImage.Create(const Image, Templ: TIntegerMatrix);
var
  R, G, B: TSingleMatrix;
  X, Y: Integer;
begin
  SplitRGB(Image, R, G, B);

  Width := Image.Width;
  Height := Image.Height;

  ImageSpec.R := DoFFT2(R, Width, Height);
  ImageSpec.G := DoFFT2(G, Width, Height);
  ImageSpec.B := DoFFT2(B, Width, Height);

  ImageSpecSquared.R := DoFFT2(R*R, Width, Height);
  ImageSpecSquared.G := DoFFT2(G*G, Width, Height);
  ImageSpecSquared.B := DoFFT2(B*B, Width, Height);

  // Template to mask. Binary
  MaskChannel.R.SetSize(Templ.Width, Templ.Height);
  MaskChannel.G.SetSize(Templ.Width, Templ.Height);
  MaskChannel.B.SetSize(Templ.Width, Templ.Height);

  SplitRGB(Templ, MaskChannel.R, MaskChannel.G, MaskChannel.B);

  for Y := 0 to Templ.Height - 1 do
    for X := 0 to Templ.Width - 1 do
    begin
      if (Templ[Y, X] and $FFFFFF) <> 0 then // Mask out Alpha
      begin
        MaskChannel.R[Y, X] := 1;
        MaskChannel.G[Y, X] := 1;
        MaskChannel.B[Y, X] := 1;
      end;
    end;

  MaskSum := Sum(MaskChannel);
  MaskSumSquared := Sum(MaskChannel * MaskChannel);

  ImgMaskCorr := CCORR_RGB(Width, Height, ImageSpec.R, ImageSpec.G, ImageSpec.B, MaskChannel);
  ImgNormCorr := Sqrt(
    CCORR_RGB(Width, Height, ImageSpecSquared.R, ImageSpecSquared.G, ImageSpecSquared.B, MaskChannel * MaskChannel) +
             (ImgMaskCorr * [1 / MaskSum[0], 1 / MaskSum[1], 1 / MaskSum[2]]) *
             (ImgMaskCorr * [MaskSumSquared[0] / MaskSum[0], MaskSumSquared[1] / MaskSum[1], MaskSumSquared[2] / MaskSum[2]] - CCORR_RGB(Width, Height, ImageSpec.R, ImageSpec.G, ImageSpec.B, MaskChannel * MaskChannel) * [2, 2, 2])
  );
end;

class function TSimbaMatchTemplate.MatchTemplateMask(const Cache: TMatchTemplateCache; const Template: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix;
var
  TemplChannel, TemplxMask, Corr: TRGBMatrix;
  MaskTemplSum, TemplxMaskSum: TDoubleArray;
  X, Y, W, H: Integer;
begin
  if (not (Cache is TMatchTemplateCachedImage)) then
    raise Exception.Create('Invalid Cache');

  with Cache as TMatchTemplateCachedImage do
  begin
    Validate(Width, Height, Template.Width, Template.Height);

    SplitRGB(Template, TemplChannel.R, TemplChannel.G, TemplChannel.B);

    case Formula of
      TM_CCOEFF, TM_CCOEFF_NORMED:
        begin
          MaskTemplSum := Sum(MaskChannel * TemplChannel);

          TemplxMask := MaskChannel * (MaskChannel * (TemplChannel - [MaskTemplSum[0] / MaskSum[0], MaskTemplSum[1] / MaskSum[1], MaskTemplSum[2] / MaskSum[2]]));
          TemplxMaskSum := Sum(TemplxMask);

          Corr := CCORR_RGB(Width, Height, ImageSpec.R, ImageSpec.G, ImageSpec.B, TemplxMask) -
                            ImgMaskCorr * [TemplxMaskSum[0] / MaskSum[0], TemplxMaskSum[1] / MaskSum[1], TemplxMaskSum[2] / MaskSum[2]];

          Result := Corr.R;

          W := Result.Width - 1;
          H := Result.Height - 1;

          for Y := 0 to H do
            for X := 0 to W do
              Result[Y, X] += Corr.G[Y, X] + Corr.B[Y, X];

          if (Formula = TM_CCOEFF_NORMED) then
            Result /= ImgNormCorr * Norm(TemplxMask);
        end;

      else
        raise Exception.Create('MatchTemplateMask: Formula not implemented');
    end;
  end;
end;

class function TSimbaMatchTemplate.MatchTemplateMask(const Image, Template: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix;
var
  Cache: TMatchTemplateCache;
begin
  Cache := CacheClass.Create(Image, Template);

  try
    Result := MatchTemplateMask(Cache, Template, Formula);
  finally
    Cache.Free();
  end;
end;

class function TSimbaMatchTemplate.MatchTemplate(const Image, Template: TIntegerMatrix; const Formula: ETMFormula): TSingleMatrix;
begin
  Validate(Image.Width, Image.Height, Template.Width, Template.Height);

  case Formula of
    TM_CCORR:         Result := CCORR_RGB(Image, Template, False);
    TM_CCORR_NORMED:  Result := CCORR_RGB(Image, Template, True);
    TM_CCOEFF:        Result := CCOEFF_RGB(Image, Template, False);
    TM_CCOEFF_NORMED: Result := CCOEFF_RGB(Image, Template, True);
    TM_SQDIFF:        Result := SQDIFF_RGB(Image, Template, False);
    TM_SQDIFF_NORMED: Result := SQDIFF_RGB(Image, Template, True);
  end;
end;

class procedure TSimbaMatchTemplate.Validate(const ImageWidth, ImageHeight, TemplateWidth, TemplateHeight: Integer);
begin
  if (ImageWidth = 0) or (ImageHeight = 0) then
    raise Exception.Create('MatchTemplate: Image is empty');
  if (TemplateWidth = 0) or (TemplateHeight = 0) then
    raise Exception.Create('MatchTemplate: Template is empty');
  if (TemplateWidth > ImageWidth) or (TemplateHeight > ImageHeight) then
    raise Exception.Create('MatchTemplate: Template must be smaller than image');
end;

class function TSimbaMatchTemplate.CacheClass: TMatchTemplateCacheClass;
begin
  Result := TMatchTemplateCachedImage;
end;

initialization
  SimbaMatchTemplate := TSimbaMultithreadedMatchTemplate;

end.
