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

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.matchtemplate, simba.matchtemplate_matrix, simba.matchtemplate_helpers;

function MatchTemplate_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function MatchTemplateMask_CCOEFF_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
function MatchTemplateMask_CCOEFF_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.threadpool, simba.simplelock;

// MatchTemplate_CCOEFF
function __MatchTemplate_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,w,h,tw,th,iw,ih: Integer;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  mR,sR, mG,sG, mB,sB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: TDoubleMatrix;
  ImageChannels, TemplChannels: TRGBMatrix;
  Corr: TSingleMatrix;
begin
  ImageChannels := TRGBMatrix.Create(Image);
  TemplChannels := TRGBMatrix.Create(Template);
  Corr := CrossCorrRGB(ImageChannels, TemplChannels).Merge();

  tw := Template.Width;
  th := Template.Height;

  invSize := Double(1.0) / Double(tw*th);

  if not Normed then
  begin
    mR := TemplChannels.R.Mean();
    mG := TemplChannels.G.Mean();
    mB := TemplChannels.B.Mean();
    tplSigma := 0;
  end else
  begin
    TemplChannels.R.MeanStdev(mR, sR);
    TemplChannels.G.MeanStdev(mG, sG);
    TemplChannels.B.MeanStdev(mB, sB);

    tplSdv := Sqr(sR) + Sqr(sG) + Sqr(sB);
    if tplSdv < 0.00001 then
    begin
      Result.SetSize(Corr.Width, Corr.Height);
      Result.Fill(1);
      Exit;
    end;

    tplSigma := Sqrt(tplSdv) / Sqrt(invSize);
  end;

  sumR := SumsPd(ImageChannels.R, sum2r);
  sumG := SumsPd(ImageChannels.G, sum2g);
  sumB := SumsPd(ImageChannels.B, sum2b);

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

      numer    := Corr[Y, X] - ((wndSumR * mR) + (wndSumG * mG) + (wndSumB * mB));
      if Normed then
      begin
        wndSum2  := sum2r[Y, X] - sum2r[Y,X+tw] - sum2r[Y+th,X] + sum2r[Y+th,X+tw];
        wndSum2  += sum2g[Y, X] - sum2g[Y,X+tw] - sum2g[Y+th,X] + sum2g[Y+th,X+tw];
        wndSum2  += sum2b[Y, X] - sum2b[Y,X+tw] - sum2b[Y+th,X] + sum2b[Y+th,X+tw];

        wndMean2 := Sqr(wndSumR) + Sqr(wndSumG) + Sqr(wndSumB);
        wndMean2 := wndMean2 * invSize;

        denom := tplSigma * Sqrt(Max(0, wndSum2 - wndMean2));
        if Abs(numer) < denom then
          Result[Y, X] := numer / denom
        else if Abs(numer) < denom*1.25 then
          if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
      end else
        Result[Y, X] := numer;
    end;
end;

function MatchTemplate_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Template, @__MatchTemplate_CCOEFF, Normed);
end;

// MatchTemplateMask_CCOEFF
function __MatchTemplateMask_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  MaskChannels, TemplChannels, ImageChannels: TRGBMatrix;
  ImgNormCorr, ImgMaskCorr: TSingleMatrix;
  MaskTemplSum, TemplxMaskSum: TDoubleArray;
  TemplxMask: TRGBMatrix;
  ImageMaskCorr: TRGBMatrix;
  MaskSum, MaskSumSquared: Single;
begin
  MaskChannels   := MaskFromTemplate(Template);
  MaskSum        := Sum(MaskChannels.R);
  MaskSumSquared := Sum(MaskChannels.R * MaskChannels.R);

  ImageChannels  := TRGBMatrix.Create(Image);
  ImageMaskCorr  := CrossCorrRGB(ImageChannels, MaskChannels);

  ImgMaskCorr := ImageChannels.Merge();

  // double norm_templx = norm(mask.mul(templ - sum(mask.mul(templ)).div(mask_sum)), NORM_L2);
  // temp_res = img_mask_corr.mul(Scalar(1.0, 1.0, 1.0, 1.0).div(mask_sum)).mul(img_mask_corr.mul(mask2_sum.div(mask_sum)) - 2 * img_mask2_corr);
  // sqrt(norm_imgx, norm_imgx);
  ImgNormCorr := Sqrt(
    CrossCorrRGB(ImageChannels * ImageChannels, MaskChannels * MaskChannels) +
                (ImageMaskCorr * (1 / MaskSum)) *
                (ImageMaskCorr * (MaskSumSquared / MaskSum) - CrossCorrRGB(ImageChannels, MaskChannels * MaskChannels) * 2)
  );

  TemplChannels := TRGBMatrix.Create(Template);

  MaskTemplSum := Sum(MaskChannels * TemplChannels);

  // Mat templx_mask = mask.mul(mask.mul(templ - sum(mask.mul(templ)).div(mask_sum)));
  TemplxMask := MaskChannels * (MaskChannels * (TemplChannels - [MaskTemplSum[0] / MaskSum, MaskTemplSum[1] / MaskSum, MaskTemplSum[2] / MaskSum]));
  TemplxMaskSum := Sum(TemplxMask);

  // Mat temp_res = img_mask_corr.mul(sum(templx_mask).div(mask_sum));
  Result := CrossCorrRGB(ImageChannels, TemplxMask).Merge() - (ImgMaskCorr * (TemplxMaskSum[0] / MaskSum) + (TemplxMaskSum[1] / MaskSum) + (TemplxMaskSum[2] / MaskSum));
  if Normed then
    Result /= ImgNormCorr * Norm(TemplxMask); // result /= norm_imgx * norm_templx;

  Result.ReplaceNaNAndInf(0);
end;

function MatchTemplateMask_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Template, @__MatchTemplateMask_CCOEFF, Normed);
end;

// MatchTemplateMask_CCOEFF_Cache
type
  TMatchTemplateCache_CCOEFF = class(TMatchTemplateCacheBase)
  public
  type
    TSliceCache = record
      Lo, Hi: Integer;
      Img: TSingleMatrix;
      ImgFFT: TRGBComplexMatrix;
      ImgNormCorr: TSingleMatrix;
    end;
  public
    Lock: TSimpleEnterableLock;
    SliceCaches: array of TSliceCache;

    ImageChannels: TRGBMatrix;
    ImgNormCorr: TSingleMatrix;

    MaskChannels: TRGBMatrix;
    MaskSum, MaskSquaredSum: Single;

    Img: TIntegerMatrix;

    function GetSliceCache(Lo,Hi: Integer): TSliceCache;

    constructor Create(Image, Template: TIntegerMatrix);
  end;

function TMatchTemplateCache_CCOEFF.GetSliceCache(Lo, Hi: Integer): TSliceCache;
var
  I, Y: Integer;
  ImgSlice: TRGBMatrix;
begin
  Lock.Enter();

  try
    for I := 0 to High(SliceCaches) do
      if (SliceCaches[I].Lo = Lo) and (SliceCaches[I].Hi = Hi) then
        Exit(SliceCaches[I]);

    //DebugLn('TMatchTemplateCache_CCOEFF.GetSliceCache(%d, %d)', [Lo, Hi]);

    ImgSlice := ImageChannels.Copy(Lo, Hi);

    Result.Lo := Lo;
    Result.Hi := Hi;
    Result.Img := ImgSlice.Merge();
    Result.ImgFFT := FFT2_RGB(ImgSlice);

    Result.ImgNormCorr := ImgNormCorr.Copy(Lo, Lo + (Hi - Lo) - MaskChannels.Height + 1);

    SetLength(SliceCaches, Length(SliceCaches) + 1);
    SliceCaches[High(SliceCaches)] := Result;
  finally
    Lock.Leave();
  end;
end;

constructor TMatchTemplateCache_CCOEFF.Create(Image, Template: TIntegerMatrix);
var
  ImageMaskCorr: TRGBMatrix;
begin
  inherited Create();

  Img := Image;

  ImageChannels  := TRGBMatrix.Create(Image);

  MaskChannels   := MaskFromTemplate(Template);
  MaskSum        := Sum(MaskChannels.R);
  MaskSquaredSum := Sum(MaskChannels.R * MaskChannels.R);

  ImageMaskCorr  := CrossCorrRGB(ImageChannels, MaskChannels);

  ImgNormCorr := Sqrt(
    CrossCorrRGB(ImageChannels * ImageChannels, MaskChannels * MaskChannels) +
                (ImageMaskCorr * (1 / MaskSum)) *
                (ImageMaskCorr * (MaskSquaredSum / MaskSum) - CrossCorrRGB(ImageChannels, MaskChannels * MaskChannels) * 2)
    );

  Width := Image.Width;
  Height := Image.Height;
end;

function MatchTemplateMask_CCOEFF_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
begin
  Result := TMatchTemplateCache_CCOEFF.Create(Image, Template);
end;

function MatchTemplateMask_CCOEFF_Cache_Sliced(Cache: TMatchTemplateCache_CCOEFF; Template: TIntegerMatrix; Normed: Boolean; ImgStartY, ImgEndY: Integer): TSingleMatrix;
var
  TemplChannels, TemplxMask: TRGBMatrix;
  MaskTemplSum, TemplxMaskSum: TDoubleArray;
  SliceCache: TMatchTemplateCache_CCOEFF.TSliceCache;
begin
  SliceCache := Cache.GetSliceCache(ImgStartY, ImgEndY);
  TemplChannels := TRGBMatrix.Create(Template);

  MaskTemplSum := Sum(Cache.MaskChannels * TemplChannels);

  TemplxMask := Cache.MaskChannels * (Cache.MaskChannels * (TemplChannels - [MaskTemplSum[0] / Cache.MaskSum, MaskTemplSum[1] / Cache.MaskSum, MaskTemplSum[2] / Cache.MaskSum]));
  TemplxMaskSum := Sum(TemplxMask);

  Result := CrossCorrRGB(SliceCache.ImgFFT, TemplxMask).Merge() - (SliceCache.Img * (TemplxMaskSum[0] / Cache.MaskSum) + (TemplxMaskSum[1] / Cache.MaskSum) + (TemplxMaskSum[2] / Cache.MaskSum));
  if Normed then
    Result /= SliceCache.ImgNormCorr * Norm(TemplxMask);

  Result.ReplaceNaNAndInf(0);
end;

function MatchTemplateMask_CCOEFF_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  Cache: TMatchTemplateCache_CCOEFF absolute ACache;
  RowSize: Integer;

  procedure Execute(const Index, Lo, Hi: Integer);
  var
    Mat: TSingleMatrix;
    Y: Integer;
  begin
    Mat := MatchTemplateMask_CCOEFF_Cache_Sliced(Cache, Template, Normed, Lo, Min(Hi + Template.Height, Cache.Height));
    for Y := 0 to Mat.Height - 1 do
      Move(Mat[Y,0], Result[Lo+Y,0], RowSize);
  end;

begin
  if (not (ACache is TMatchTemplateCache_CCOEFF)) then
    raise Exception.Create('[MatchTemplateMask_CCOEFF_Cache]: Invalid cache');

  Result.SetSize(
    (Cache.Width - Template.Width) + 1,
    (Cache.Height - Template.Height) + 1
  );
  RowSize := Result.Width * SizeOf(Single);

  SimbaThreadPool.RunParallel(CalculateSlices(Cache.Width, Cache.Height), 0, Cache.Height, @Execute);
end;

end.

