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

unit simba.matchtemplate_ccorr;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  Classes, SysUtils,
  simba.base, simba.matchtemplate, simba.matchtemplate_matrix, simba.matchtemplate_helpers;

function MatchTemplate_CCORR(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCORR(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function MatchTemplateMask_CCORR_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
function MatchTemplateMask_CCORR_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.threading,
  simba.vartype_floatmatrix, simba.vartype_ordmatrix;

// MatchTemplate_CCORR
function __MatchTemplate_CCORR(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  x,y,w,h,tw,th,iw,ih: Integer;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, mR,mG,mB, sR,sG,sB, wndSum2: Double;
  sum2r, sum2g, sum2b: TDoubleMatrix;
  Corr: TSingleMatrix;
  ImageChannels, TemplChannels: TRGBMatrix;
begin
  ImageChannels := TRGBMatrix.Create(Image);
  TemplChannels := TRGBMatrix.Create(Templ);
  Corr := CrossCorrRGB(ImageChannels, TemplChannels).Merge();
  if not Normed then
    Exit(Corr);

  tw := Templ.Width;
  th := Templ.Height;

  invSize := Double(1.0) / Double(tw * th);

  TemplChannels.R.MeanStdev(mR, sR);
  TemplChannels.G.MeanStdev(mG, sG);
  TemplChannels.B.MeanStdev(mB, sB);

  tplMean := Sqr(mR) + Sqr(mG) + Sqr(mB);
  tplSdv  := Sqr(sR) + Sqr(sG) + Sqr(sB);

  tplSigma := Sqrt(tplSdv + tplMean) / Sqrt(invSize);

  SumsPd(ImageChannels.R, sum2r);
  SumsPd(ImageChannels.G, sum2g);
  SumsPd(ImageChannels.B, sum2b);

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

      numer := Corr[Y, X];
      denom := tplSigma * Sqrt(wndSum2);

      if Abs(numer) < denom then
        Result[Y, X] := numer / denom
      else if Abs(numer) < denom*1.25 then
        if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
    end;
end;

function MatchTemplate_CCORR(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Templ, @__MatchTemplate_CCORR, Normed);
end;

// MatchTemplateMask_CCORR
function __MatchTemplateMask_CCORR(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  MaskChannels, TemplateChannels, ImageChannels: TRGBMatrix;
  Img2, Mask2: TRGBMatrix;
  Templ2Mask2Sum: Double;
  TempResult: TSingleMatrix;
  W, H, X, Y: Integer;
begin
  MaskChannels     := MaskFromTemplate(Template);
  TemplateChannels := TRGBMatrix.Create(Template);
  ImageChannels    := TRGBMatrix.Create(Image);

  Result := CrossCorrRGB(ImageChannels, TemplateChannels * (MaskChannels * MaskChannels)).Merge();

  if Normed then
  begin
    // img.mul(img);
    Img2 := (ImageChannels * ImageChannels);
    // mask.mul(mask);
    Mask2 := (MaskChannels * MaskChannels);
    // double templ2_mask2_sum = norm(templ.mul(mask), NORM_L2SQR);
    Templ2Mask2Sum := SumOfSquares(TemplateChannels * MaskChannels);

    // crossCorr(img2, mask2, temp_result, Point(0, 0), 0, 0);
    TempResult := CrossCorrRGB(Img2, Mask2).Merge();

    // sqrt(templ2_mask2_sum * temp_result, temp_result);
    W := Result.Width - 1;
    H := Result.Height - 1;
    for Y := 0 to H do
      for X := 0 to W do
        Result[Y, X] := Result[Y, X] / Sqrt(Templ2Mask2Sum * TempResult[Y, X]);
  end;

  Result.ReplaceNaNAndInf(0);
end;

function MatchTemplateMask_CCORR(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Template, @__MatchTemplateMask_CCORR, Normed);
end;

// MatchTemplateMask_CCORR_Cache
type
  TMatchTemplateCache_CCORR = class(TMatchTemplateCacheBase)
  public
  type
    TSliceCache = record
      Lo, Hi: Integer;
      Img: TSingleMatrix;
      TempResult: TSingleMatrix;
      ImgFFT: TRGBComplexMatrix;
    end;
  public
    Lock: TEnterableLock;
    SliceCaches: array of TSliceCache;

    Img2, Mask2: TRGBMatrix;
    TempResult: TSingleMatrix;

    ImageChannels: TRGBMatrix;
    MaskChannels: TRGBMatrix;

    Img: TIntegerMatrix;

    function GetSliceCache(Lo, Hi: Integer): TSliceCache;

    constructor Create(Image, Template: TIntegerMatrix);
  end;

function TMatchTemplateCache_CCORR.GetSliceCache(Lo, Hi: Integer): TSliceCache;
var
  ImgSlice: TRGBMatrix;
  I: Integer;
begin
  Lock.Enter();

  try
    for I := 0 to High(SliceCaches) do
      if (SliceCaches[I].Lo = Lo) and (SliceCaches[I].Hi = Hi) then
        Exit(SliceCaches[I]);

    //DebugLn('TMatchTemplateCache_CCORR.GetSliceCache(%d, %d)', [Lo, Hi]);

    ImgSlice := ImageChannels.Copy(Lo, Hi);

    Result.Lo := Lo;
    Result.Hi := Hi;
    Result.Img := ImgSlice.Merge();
    Result.ImgFFT := FFT2_RGB(ImgSlice);

    Result.TempResult := TempResult.Copy(Lo, Lo + (Hi - Lo) - MaskChannels.Height + 1);

    SetLength(SliceCaches, Length(SliceCaches) + 1);
    SliceCaches[High(SliceCaches)] := Result;
  finally
    Lock.Leave();
  end;
end;

constructor TMatchTemplateCache_CCORR.Create(Image, Template: TIntegerMatrix);
begin
  inherited Create();

  Img := Image;

  ImageChannels := TRGBMatrix.Create(Image);
  MaskChannels  := MaskFromTemplate(Template);

  Img2 := (ImageChannels * ImageChannels);
  Mask2 := (MaskChannels * MaskChannels);
  TempResult := CrossCorrRGB(Img2, Mask2).Merge();

  Width := Image.Width;
  Height := Image.Height;
end;

function MatchTemplateMask_CCORR_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
begin
  Result := TMatchTemplateCache_CCORR.Create(Image, Template);
end;

function MatchTemplateMask_CCORR_Cache_Sliced(Cache: TMatchTemplateCache_CCORR; Template: TIntegerMatrix; Normed: Boolean; ImgStartY, ImgEndY: Integer): TSingleMatrix;
var
  TemplChannels: TRGBMatrix;
  SliceCache: TMatchTemplateCache_CCORR.TSliceCache;
  Templ2Mask2Sum: Double;
  X, Y, W, H: Integer;
begin
  SliceCache := Cache.GetSliceCache(ImgStartY, ImgEndY);
  TemplChannels := TRGBMatrix.Create(Template);

  Result := CrossCorrRGB(SliceCache.ImgFFT, TemplChannels * Cache.Mask2).Merge();

  if Normed then
  begin
    Templ2Mask2Sum := SumOfSquares(TemplChannels * Cache.MaskChannels);

    Result.GetSizeMinusOne(W, H);
    for Y := 0 to H do
      for X := 0 to W do
        Result[Y, X] := Result[Y, X] / Sqrt(Templ2Mask2Sum * SliceCache.TempResult[Y, X]);
  end;

  Result.ReplaceNaNAndInf(0);
end;

function MatchTemplateMask_CCORR_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  Cache: TMatchTemplateCache_CCORR absolute ACache;
  RowSize: Integer;

  procedure Execute(const Index, Lo, Hi: Integer);
  var
    Mat: TSingleMatrix;
    Y: Integer;
  begin
    Mat := MatchTemplateMask_CCORR_Cache_Sliced(Cache, Template, Normed, Lo, Min(Hi + Template.Height, Cache.Height));
    for Y := 0 to Mat.Height - 1 do
      Move(Mat[Y, 0], Result[Lo + Y, 0], RowSize);
  end;

begin
  if (not (ACache is TMatchTemplateCache_CCORR)) then
    raise Exception.Create('[MatchTemplateMask_CCORR_Cache]: Invalid cache type');

  Result.SetSize(
    (Cache.Width - Template.Width) + 1,
    (Cache.Height - Template.Height) + 1
  );
  RowSize := Result.Width * SizeOf(Single);

  SimbaThreadPool.RunParallel(CalculateSlices(Cache.Width, Cache.Height), 0, Cache.Height - Template.Height, @Execute);
end;

end.

