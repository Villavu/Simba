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

interface

uses
  Classes, SysUtils,
  simba.base, simba.matchtemplate_core;

function MatchTemplate_CCOEFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCOEFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.vartype_matrix;

function MatchTemplate_CCOEFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y, lastX,lastY, templW,templH, imgW,imgH, outW,outH: Integer;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2, diff2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  meanR,stdevR, meanG,stdevG, meanB,stdevB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: TDoubleMatrix;
  templR,templG,templB: TSingleMatrix;
  crossCorr: TSingleMatrix;
  curSumR,curSumRBot, curSumG,curSumGBot, curSumB,curSumBBot: PDouble;
  curSum2R,curSum2RBot, curSum2G,curSum2GBot, curSum2B,curSum2BBot: PDouble;
  curCross, curResult: PSingle;
begin
  SplitChannels(Template, templR, templG, templB);

  imgW := Cache.Width;
  imgH := Cache.Height;
  outW := imgW - templR.Width  + 1;
  outH := imgH - templR.Height + 1;
  crossCorr := CorrelateChannelsSum(
    Cache.Spectra,
    ForwardTransformChannels(templR, templG, templB, imgW, imgH),
    outW, outH
  );

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
    if (tplSdv < DBL_EPSILON) then
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
  begin
    curSumR := @sumR[Y][0]; curSumRBot := @sumR[Y+templH][0]; curSumG := @sumG[Y][0]; curSumGBot := @sumG[Y+templH][0];
    curSumB := @sumB[Y][0]; curSumBBot := @sumB[Y+templH][0];
    curSum2R := @sum2r[Y][0]; curSum2RBot := @sum2r[Y+templH][0]; curSum2G := @sum2g[Y][0]; curSum2GBot := @sum2g[Y+templH][0];
    curSum2B := @sum2b[Y][0]; curSum2BBot := @sum2b[Y+templH][0];
    curCross := @crossCorr[Y][0]; curResult := @Result[Y][0];
    for x := 0 to lastX do
    begin
      wndSumR  := curSumR[X] - curSumR[X+templW] - curSumRBot[X] + curSumRBot[X+templW];
      wndSumG  := curSumG[X] - curSumG[X+templW] - curSumGBot[X] + curSumGBot[X+templW];
      wndSumB  := curSumB[X] - curSumB[X+templW] - curSumBBot[X] + curSumBBot[X+templW];

      numer    := curCross[X] - ((wndSumR * meanR) + (wndSumG * meanG) + (wndSumB * meanB));
      if Normed then
      begin
        wndSum2  := curSum2R[X] - curSum2R[X+templW] - curSum2RBot[X] + curSum2RBot[X+templW];
        wndSum2  += curSum2G[X] - curSum2G[X+templW] - curSum2GBot[X] + curSum2GBot[X+templW];
        wndSum2  += curSum2B[X] - curSum2B[X+templW] - curSum2BBot[X] + curSum2BBot[X+templW];

        wndMean2 := Sqr(wndSumR) + Sqr(wndSumG) + Sqr(wndSumB);
        wndMean2 := wndMean2 * invSize;

        diff2 := Max(0, wndSum2 - wndMean2);
        if diff2 <= Min(0.5, 10 * FLT_EPSILON * wndSum2) then
          denom := 0
        else
          denom := tplSigma * Sqrt(diff2);

        if Abs(numer) < denom then
          curResult[X] := numer / denom
        else if Abs(numer) < denom * 1.125 then
          if numer > 0 then curResult[X] := 1 else curResult[X] := -1;
      end else
        curResult[X] := numer;
    end;
  end;
end;

function MatchTemplateMask_CCOEFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
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
  curIM, curIMSq, curISqMSq: array[0..2] of PSingle;
  curNumer, curResult: PSingle;
begin
  imgW := Cache.Width;
  imgH := Cache.Height;

  Mask    := MaskFromTemplate(Template);
  MaskSum := Sum(Mask);

  SplitChannels(Template, templR, templG, templB);

  outW := imgW - Template.Width  + 1;
  outH := imgH - Template.Height + 1;

  if MaskSum <= 0 then
  begin
    Result.SetSize(outW, outH);
    Exit;
  end;

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
  numerCorr := CorrelateChannelsSum(
    Cache.Spectra,
    ForwardTransformChannels(templMaskR, templMaskG, templMaskB, imgW, imgH),
    outW, outH
  );

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
  begin
    for ch := 0 to 2 do
    begin
      curIM[ch] := @corrImgMask[ch][y][0]; curIMSq[ch] := @corrImgMaskSq[ch][y][0]; curISqMSq[ch] := @corrImgSqMaskSq[ch][y][0];
    end;
    curNumer := @numerCorr[y][0]; curResult := @Result[y][0];
    for x := 0 to outW - 1 do
    begin
      numer := curNumer[x] - (curIM[0][x] * kR + curIM[1][x] * kG + curIM[2][x] * kB);

      if Normed then
      begin
        nrm := 0;
        for ch := 0 to 2 do
        begin
          cIM     := curIM[ch][x];       // CCorr(I_c, M)
          cIMSq   := curIMSq[ch][x];     // CCorr(I_c, M^2)
          cISqMSq := curISqMSq[ch][x];   // CCorr(I_c^2, M^2)
          e   := (cIM * invMaskSum) * (cIM * maskSqOverSum - (cIMSq + cIMSq));   // (cIMSq+cIMSq) = exact 2*cIMSq
          nrm := nrm + (cISqMSq + e);
        end;
        if nrm > 0 then imgNorm := Sqrt(nrm) else imgNorm := 0;
        denom := imgNorm * normTempl;
        if denom > 0 then
          curResult[x] := numer / denom
        else
          curResult[x] := 0;
      end
      else
        curResult[x] := numer;
    end;
  end;

  if Normed then
    Result.ReplaceNaNAndInf(0);
end;

end.
