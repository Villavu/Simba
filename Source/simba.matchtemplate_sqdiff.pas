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

interface

uses
  Classes, SysUtils,
  simba.base, simba.matchtemplate_core;

function MatchTemplate_SQDIFF(var Cache: TMatchTemplateCache; const Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_SQDIFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.vartype_matrix;

function MatchTemplate_SQDIFF(var Cache: TMatchTemplateCache; const Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y, lastX,lastY, templW,templH, imgW,imgH, outW,outH: Integer;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, meanR,stdevR, meanG,stdevG, meanB,stdevB: Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  crossCorr: TSingleMatrix;
  templR,templG,templB: TSingleMatrix;
  curSum2R,curSum2RBot, curSum2G,curSum2GBot, curSum2B,curSum2BBot: PDouble;
  curCross, curResult: PSingle;
begin
  SplitChannels(Templ, templR, templG, templB);

  imgW := Cache.Width;
  imgH := Cache.Height;
  outW := imgW - templR.Width  + 1;
  outH := imgH - templR.Height + 1;
  crossCorr := CorrelateChannelsSum(
    Cache.Spectra,
    ForwardTransformChannels(templR, templG, templB, imgW, imgH),
    outW, outH
  );

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
  begin
    curSum2R := @sum2r[Y][0]; curSum2RBot := @sum2r[Y+templH][0]; curSum2G := @sum2g[Y][0]; curSum2GBot := @sum2g[Y+templH][0];
    curSum2B := @sum2b[Y][0]; curSum2BBot := @sum2b[Y+templH][0];
    curCross := @crossCorr[Y][0]; curResult := @Result[Y][0];
    for x := 0 to lastX do
    begin
      wndSum2 := curSum2R[X] - curSum2R[X+templW] - curSum2RBot[X] + curSum2RBot[X+templW];
      wndSum2 += curSum2G[X] - curSum2G[X+templW] - curSum2GBot[X] + curSum2GBot[X+templW];
      wndSum2 += curSum2B[X] - curSum2B[X+templW] - curSum2BBot[X] + curSum2BBot[X+templW];

      numer   := Max(0, wndSum2 - Double(2.0) * curCross[X] + tplSum2);
      if Normed then
      begin
        if wndSum2 > 0 then
          denom := tplSigma * Sqrt(wndSum2)
        else
          denom := 0;
        if Abs(numer) < denom then
          curResult[X] := numer / denom
        else
          curResult[X] := 1;
      end else
        curResult[X] := numer;
    end;
  end;
end;

function MatchTemplateMask_SQDIFF(var Cache: TMatchTemplateCache; const Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  templR,templG,templB, Mask, Mask2, TempResult: TSingleMatrix;
  channelCorr: TChannelCorrelations;
  Templ2Mask2Sum, maxEnergy: Double;
  templ2Mask2SumS, energy, negTwoCorr: Single;
  x, y, imgW, imgH, outW, outH: Integer;
  curCorr0, curCorr1, curCorr2, curTemp, curResult: PSingle;
begin
  imgW := Cache.Width;
  imgH := Cache.Height;

  Mask := MaskFromTemplate(Template);
  SplitChannels(Template, templR, templG, templB);

  Mask2 := MultiplyElements(Mask, Mask); // mask.mul(mask)
  outW := imgW - templR.Width  + 1;
  outH := imgH - templR.Height + 1;

  // double templ2_mask2_sum = norm(templ.mul(mask), NORM_L2SQR);
  Templ2Mask2Sum := SumOfSquaresChannels(MultiplyElements(templR, Mask), MultiplyElements(templG, Mask), MultiplyElements(templB, Mask));

  // CCorr(I_c^2, M^2) per channel (its channel-sum is the window energy TempResult); mask-only -> cached.
  channelCorr := Cache.MaskCorrISqMSq(Mask, Mask2, outW, outH);
  Result := CorrelateChannelsSum(
    Cache.Spectra,
    ForwardTransformChannels(MultiplyElements(templR, Mask2), MultiplyElements(templG, Mask2), MultiplyElements(templB, Mask2), imgW, imgH),
    outW, outH
  );

  // TempResult = Sum_c CCorr(I_c^2, M^2);   Result = -2*numerator + TempResult + templ2Mask2Sum
  templ2Mask2SumS := Templ2Mask2Sum;
  maxEnergy := 0;
  TempResult.SetSize(outW, outH);
  for y := 0 to outH - 1 do
  begin
    curCorr0 := @channelCorr[0][y][0]; curCorr1 := @channelCorr[1][y][0]; curCorr2 := @channelCorr[2][y][0];
    curTemp := @TempResult[y][0]; curResult := @Result[y][0];
    for x := 0 to outW - 1 do
    begin
      energy := (curCorr0[x] + curCorr1[x]) + curCorr2[x];
      curTemp[x] := energy;
      if energy > maxEnergy then
        maxEnergy := energy;
      negTwoCorr := -2 * curResult[x];
      curResult[x] := (negTwoCorr + energy) + templ2Mask2SumS;
    end;
  end;

  if Normed then
  begin
    // result /= sqrt(templ2_mask2_sum * temp_result);
    NormalizeMasked(Result, TempResult, maxEnergy, Templ2Mask2Sum, 1, 0, 1e30);
    Result.ReplaceNaNAndInf(0);
  end;
end;

end.
