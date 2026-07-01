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

unit simba.matchtemplate_sqdiff;

{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  Classes, SysUtils,
  simba.base, simba.matchtemplate_core;

function MatchTemplate_SQDIFF(var Cache: TMatchTemplateCache; Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_SQDIFF(var Cache: TMatchTemplateCache; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.vartype_matrix;

// MatchTemplate_SQDIFF
function MatchTemplate_SQDIFF(var Cache: TMatchTemplateCache; Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y, lastX,lastY, templW,templH, imgW,imgH, outW,outH: Integer;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, meanR,stdevR, meanG,stdevG, meanB,stdevB: Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  crossCorr: TSingleMatrix;
  templR,templG,templB: TSingleMatrix;
begin
  SplitChannels(Templ, templR, templG, templB);

  imgW := Cache.Width;
  imgH := Cache.Height;
  outW := imgW - templR.Width  + 1;
  outH := imgH - templR.Height + 1;
  crossCorr := CorrelateChannelsSum(Cache.Spectra,
                                    ForwardTransformChannels(templR, templG, templB, imgW, imgH), outW, outH);

  templW := Templ.Width;
  templH := Templ.Height;

  invSize := Double(1.0) / Double(templW*templH);

  templR.MeanStdev(meanR, stdevR);
  templG.MeanStdev(meanG, stdevG);
  templB.MeanStdev(meanB, stdevB);

  tplMean := Sqr(meanR) + Sqr(meanG) + Sqr(meanB);
  tplSdv  := Sqr(stdevR) + Sqr(stdevG) + Sqr(stdevB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);
  tplSum2  := (tplSdv + tplMean) / invSize;

  sum2r := Cache.SumSq[0];
  sum2g := Cache.SumSq[1];
  sum2b := Cache.SumSq[2];

  imgW := sum2r.Width;
  imgH := sum2r.Height;

  Result.SetSize(imgW-templW, imgH-templH);

  lastX := imgW-templW-1;
  lastY := imgH-templH-1;
  for y := 0 to lastY do
    for x := 0 to lastX do
    begin
      wndSum2 := sum2r[Y, X] - sum2r[Y,X+templW] - sum2r[Y+templH,X] + sum2r[Y+templH,X+templW];
      wndSum2 += sum2g[Y, X] - sum2g[Y,X+templW] - sum2g[Y+templH,X] + sum2g[Y+templH,X+templW];
      wndSum2 += sum2b[Y, X] - sum2b[Y,X+templW] - sum2b[Y+templH,X] + sum2b[Y+templH,X+templW];

      numer   := Max(0, wndSum2 - Double(2.0) * crossCorr[Y, X] + tplSum2);
      if Normed then
      begin
        if wndSum2 > 0 then
          denom := tplSigma * Sqrt(wndSum2)
        else
          denom := 0;
        if Abs(numer) < denom then
          Result[Y, X] := numer / denom
        else
          Result[Y, X] := 1;
      end else
        Result[Y, X] := numer;
    end;
end;


// MatchTemplateMask_SQDIFF
// Image-side spectra passed in (fresh from the wrapper or from a TMatchTemplateCache); imgW/imgH = IMAGE dims.
function __MatchTemplateMask_SQDIFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  templR,templG,templB, Mask, Mask2, TempResult: TSingleMatrix;
  channelCorr: TChannelCorrelations;
  Templ2Mask2Sum, maxEnergy: Double;
  templ2Mask2SumS, energy, negTwoCorr: Single;
  x, y, imgW, imgH, outW, outH: Integer;
begin
  imgW := Cache.Width;
  imgH := Cache.Height;

  Mask := MaskFromTemplate(Template);     // single-channel mask
  SplitChannels(Template, templR, templG, templB);

  Mask2 := MultiplyElements(Mask, Mask);     // mask.mul(mask)
  outW := imgW - templR.Width  + 1;
  outH := imgH - templR.Height + 1;

  // double templ2_mask2_sum = norm(templ.mul(mask), NORM_L2SQR);
  Templ2Mask2Sum := SumOfSquaresChannels(MultiplyElements(templR, Mask), MultiplyElements(templG, Mask), MultiplyElements(templB, Mask));

  // CCorr(I_c^2, M^2) per channel (its channel-sum is the window energy TempResult); mask-only -> cached.
  channelCorr := Cache.MaskCorrISqMSq(Mask, Mask2, outW, outH);
  Result := CorrelateChannelsSum(Cache.Spectra,
                                 ForwardTransformChannels(MultiplyElements(templR, Mask2), MultiplyElements(templG, Mask2), MultiplyElements(templB, Mask2), imgW, imgH), outW, outH);

  // fuse the channel-sum and the SQDIFF combine into ONE pass (was 4 operator-overload temporaries);
  // also track maxEnergy so NormalizeMasked needn't scan for it:
  //   TempResult = Sum_c CCorr(I_c^2, M^2);   Result = -2*numerator + TempResult + templ2Mask2Sum
  templ2Mask2SumS := Templ2Mask2Sum;
  maxEnergy := 0;
  TempResult.SetSize(outW, outH);
  for y := 0 to outH - 1 do
    for x := 0 to outW - 1 do
    begin
      energy := (channelCorr[0][y, x] + channelCorr[1][y, x]) + channelCorr[2][y, x];
      TempResult[y, x] := energy;
      if energy > maxEnergy then maxEnergy := energy;
      negTwoCorr := -2 * Result[y, x];
      Result[y, x] := (negTwoCorr + energy) + templ2Mask2SumS;
    end;

  if Normed then
  begin
    // result /= sqrt(templ2_mask2_sum * temp_result); degenerate windows -> 1 (worst, so the
    // argmin can't land on a near-black window); clamp negatives (rounding) to 0.
    NormalizeMasked(Result, TempResult, maxEnergy, Templ2Mask2Sum, 1, 0, 1e30);
    Result.ReplaceNaNAndInf(0);   // only the normed division can produce NaN/Inf
  end;
end;

function MatchTemplateMask_SQDIFF(var Cache: TMatchTemplateCache; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := __MatchTemplateMask_SQDIFF(Cache, Template, Normed);
end;

end.
