{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

{
  MatchTemplateMask implemented from OpenCV's templmatch.cpp

  Copyright (C) 2000-2022, Intel Corporation, all rights reserved.
  Copyright (C) 2009-2011, Willow Garage Inc., all rights reserved.
  Copyright (C) 2009-2016, NVIDIA Corporation, all rights reserved.
  Copyright (C) 2010-2013, Advanced Micro Devices, Inc., all rights reserved.
  Copyright (C) 2015-2023, OpenCV Foundation, all rights reserved.
  Copyright (C) 2008-2016, Itseez Inc., all rights reserved.
  Copyright (C) 2019-2023, Xperience AI, all rights reserved.
  Copyright (C) 2019-2022, Shenzhen Institute of Artificial Intelligence and Robotics for Society, all rights reserved.
  Copyright (C) 2022-2023, Southern University of Science And Technology, all rights reserved.

  Third party copyrights are property of their respective owners.
}

unit simba.matchtemplate_ccoeff;

{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  Classes, SysUtils,
  simba.base, simba.matchtemplate_core;

function MatchTemplate_CCOEFF(var Cache: TMatchTemplateCache; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCOEFF(var Cache: TMatchTemplateCache; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.vartype_matrix;

// MatchTemplate_CCOEFF -- image spectrum + integral images come from the cache (a throwaway one in the
// Image overload, or a persistent var-param cache). imgW/imgH = IMAGE dims.
function MatchTemplate_CCOEFF(var Cache: TMatchTemplateCache; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y, lastX,lastY, templW,templH, imgW,imgH, outW,outH: Integer;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2, diff2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  meanR,stdevR, meanG,stdevG, meanB,stdevB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: TDoubleMatrix;
  templR,templG,templB: TSingleMatrix;
  crossCorr: TSingleMatrix;
begin
  SplitChannels(Template, templR, templG, templB);

  imgW := Cache.Width;
  imgH := Cache.Height;
  outW := imgW - templR.Width  + 1;
  outH := imgH - templR.Height + 1;
  crossCorr := CorrelateChannelsSum(Cache.Spectra,
                                    ForwardTransformChannels(templR, templG, templB, imgW, imgH), outW, outH);

  templW := Template.Width;
  templH := Template.Height;

  invSize := Double(1.0) / Double(templW*templH);

  if not Normed then
  begin
    meanR := templR.Mean;
    meanG := templG.Mean;
    meanB := templB.Mean;
    tplSigma := 0;
  end else
  begin
    templR.MeanStdev(meanR, stdevR);
    templG.MeanStdev(meanG, stdevG);
    templB.MeanStdev(meanB, stdevB);

    tplSdv := Sqr(stdevR) + Sqr(stdevG) + Sqr(stdevB);
    if tplSdv < DBL_EPSILON then   // OpenCV: templNorm < DBL_EPSILON => degenerate template
    begin
      Result.SetSize(crossCorr.Width, crossCorr.Height);
      Result.Fill(1);
      Exit;
    end;

    tplSigma := Sqrt(tplSdv) / Sqrt(invSize);
  end;

  sumR  := Cache.Sum[0];    sumG  := Cache.Sum[1];    sumB  := Cache.Sum[2];
  sum2r := Cache.SumSq[0];  sum2g := Cache.SumSq[1];  sum2b := Cache.SumSq[2];

  imgW := sumR.Width;
  imgH := sumR.Height;

  Result.SetSize(imgW-templW, imgH-templH);

  lastX := imgW-templW-1;
  lastY := imgH-templH-1;
  for y := 0 to lastY do
    for x := 0 to lastX do
    begin
      wndSumR  := sumR[Y, X] - sumR[Y,X+templW] - sumR[Y+templH,X] + sumR[Y+templH,X+templW];
      wndSumG  := sumG[Y, X] - sumG[Y,X+templW] - sumG[Y+templH,X] + sumG[Y+templH,X+templW];
      wndSumB  := sumB[Y, X] - sumB[Y,X+templW] - sumB[Y+templH,X] + sumB[Y+templH,X+templW];

      numer    := crossCorr[Y, X] - ((wndSumR * meanR) + (wndSumG * meanG) + (wndSumB * meanB));
      if Normed then
      begin
        wndSum2  := sum2r[Y, X] - sum2r[Y,X+templW] - sum2r[Y+templH,X] + sum2r[Y+templH,X+templW];
        wndSum2  += sum2g[Y, X] - sum2g[Y,X+templW] - sum2g[Y+templH,X] + sum2g[Y+templH,X+templW];
        wndSum2  += sum2b[Y, X] - sum2b[Y,X+templW] - sum2b[Y+templH,X] + sum2b[Y+templH,X+templW];

        wndMean2 := Sqr(wndSumR) + Sqr(wndSumG) + Sqr(wndSumB);
        wndMean2 := wndMean2 * invSize;

        diff2 := Max(0, wndSum2 - wndMean2);
        // OpenCV common_matchTemplate: zero the denom on tiny-variance windows; clamp band = denom*1.125
        if diff2 <= Min(0.5, 10 * FLT_EPSILON * wndSum2) then
          denom := 0
        else
          denom := tplSigma * Sqrt(diff2);

        if Abs(numer) < denom then
          Result[Y, X] := numer / denom
        else if Abs(numer) < denom * 1.125 then
          if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
      end else
        Result[Y, X] := numer;
    end;
end;


// MatchTemplateMask_CCOEFF
// Image-side spectra (ImgSpectra = FFT of I_c, ImgSqSpectra = FFT of I_c^2) passed in -- computed fresh
// by the non-cached wrapper or pulled from a TMatchTemplateCache. imgW/imgH are the IMAGE dims.
function __MatchTemplateMask_CCOEFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  templR,templG,templB, Mask, Mask2: TSingleMatrix;
  templMaskR,templMaskG,templMaskB: TSingleMatrix;     // mean-subtracted masked template per channel
  numerCorr: TSingleMatrix;
  maskTemplSumR,maskTemplSumG,maskTemplSumB: Double;
  templMaskSumR,templMaskSumG,templMaskSumB: Double;
  MaskSum, MaskSumSquared: Single;
  invMaskSum, maskSqOverSum, kR, kG, kB, normTempl: Single;   // loop-invariant scalars (Single)
  cIM, cIMSq, cISqMSq, e, nrm, imgNorm, numer, denom: Single; // per-pixel scratch (Single)
  x, y, ch, imgW, imgH, outW, outH: Integer;
  corrImgMask, corrImgSqMaskSq, corrImgMaskSq: TChannelCorrelations;
begin
  imgW := Cache.Width;
  imgH := Cache.Height;

  Mask    := MaskFromTemplate(Template);   // single-channel mask
  MaskSum := Sum(Mask);

  SplitChannels(Template, templR, templG, templB);

  outW := imgW - Template.Width  + 1;
  outH := imgH - Template.Height + 1;

  Mask2          := MultiplyElements(Mask, Mask);   // computed once, reused for the FFT operand and the sum
  MaskSumSquared := Sum(Mask2);

  // mean-subtracted masked template (no FFT yet)
  maskTemplSumR := SumDouble(MultiplyElements(Mask, templR));
  maskTemplSumG := SumDouble(MultiplyElements(Mask, templG));
  maskTemplSumB := SumDouble(MultiplyElements(Mask, templB));
  templMaskR := MultiplyElements(Mask, MultiplyElements(Mask, SubtractScalar(templR, maskTemplSumR / MaskSum)));
  templMaskG := MultiplyElements(Mask, MultiplyElements(Mask, SubtractScalar(templG, maskTemplSumG / MaskSum)));
  templMaskB := MultiplyElements(Mask, MultiplyElements(Mask, SubtractScalar(templB, maskTemplSumB / MaskSum)));
  templMaskSumR := SumDouble(templMaskR);
  templMaskSumG := SumDouble(templMaskG);
  templMaskSumB := SumDouble(templMaskB);

  // three normaliser correlations, each against a COMMON mask operand (M or M^2) -> per channel.
  // These depend only on (image, mask), so the cache memoises them across calls with a STATIC mask
  // (rebuilt automatically if the mask changes) -- leaving only the template numerator below per call.
  corrImgMask     := Cache.MaskCorrIM(Mask, Mask2, outW, outH);      // CCorr(I_c, M)
  corrImgSqMaskSq := Cache.MaskCorrISqMSq(Mask, Mask2, outW, outH);  // CCorr(I_c^2, M^2)
  corrImgMaskSq   := Cache.MaskCorrIMSq(Mask, Mask2, outW, outH);    // CCorr(I_c, M^2)

  // numerator correlation Sum_c CCorr(I_c, T'_c) -- template-dependent, recomputed every call
  numerCorr := CorrelateChannelsSum(Cache.Spectra, ForwardTransformChannels(templMaskR, templMaskG, templMaskB, imgW, imgH), outW, outH);

  // loop-invariant scalars, rounded to Single to match the matrix-operator coercions exactly
  invMaskSum    := 1 / MaskSum;
  maskSqOverSum := MaskSumSquared / MaskSum;
  kR := templMaskSumR / MaskSum;
  kG := templMaskSumG / MaskSum;
  kB := templMaskSumB / MaskSum;
  normTempl := NormChannels(templMaskR, templMaskG, templMaskB);

  // Fuse the whole normalisation into ONE pass (was ~26 operator-overload output-sized temporaries):
  //   numerator = numerCorr - Sum_c CCorr(I_c,M)*(templMaskSum_c/sumM)
  //   norm(I')  = sqrt{ Sum_c CCorr(I_c^2,M^2) + (CCorr(I_c,M)/sumM)*(sumM2/sumM*CCorr(I_c,M) - 2*CCorr(I_c,M^2)) }
  //   result    = numerator / (norm(I') * norm(T'))      [only when Normed]
  Result.SetSize(outW, outH);
  for y := 0 to outH - 1 do
    for x := 0 to outW - 1 do
    begin
      numer := numerCorr[y, x]
             - (corrImgMask[0][y, x] * kR + corrImgMask[1][y, x] * kG + corrImgMask[2][y, x] * kB);

      if Normed then
      begin
        nrm := 0;
        for ch := 0 to 2 do
        begin
          cIM     := corrImgMask[ch][y, x];       // CCorr(I_c, M)
          cIMSq   := corrImgMaskSq[ch][y, x];     // CCorr(I_c, M^2)
          cISqMSq := corrImgSqMaskSq[ch][y, x];   // CCorr(I_c^2, M^2)
          e   := (cIM * invMaskSum) * (cIM * maskSqOverSum - (cIMSq + cIMSq));   // (cIMSq+cIMSq) = exact 2*cIMSq
          nrm := nrm + (cISqMSq + e);
        end;
        if nrm > 0 then imgNorm := Sqrt(nrm) else imgNorm := 0; 
        denom := imgNorm * normTempl;
        if denom > 0 then
          Result[y, x] := numer / denom
        else
          Result[y, x] := 0;                                   
      end
      else
        Result[y, x] := numer;
    end;

  if Normed then
    Result.ReplaceNaNAndInf(0);
end;

function MatchTemplateMask_CCOEFF(var Cache: TMatchTemplateCache; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := __MatchTemplateMask_CCOEFF(Cache, Template, Normed);
end;

end.
