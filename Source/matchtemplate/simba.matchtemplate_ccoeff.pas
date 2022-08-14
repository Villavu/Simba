{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matchtemplate_ccoeff;

{$DEFINE SIMBA_O4}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils, math,
  simba.mufasatypes, simba.matchtemplate, simba.matchtemplate_matrix, simba.matchtemplate_helpers;

function MatchTemplate_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplate_CCOEFF_MT(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function MatchTemplateMask_CCOEFF_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
function MatchTemplateMask_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCOEFF_MT(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function MatchTemplateMask_CCOEFF_CreateCache_MT(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
function MatchTemplateMask_CCOEFF_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.threadpool;

type
  TMatchTemplateCache_CCOEFF = class(TMatchTemplateCacheBase)
  public
    Slices: array of record
      ImgFFT: TRGBComplexMatrix;
      MaskChannels: TRGBMatrix;
      MaskSum, MaskSquaredSum: Single;
      ImgChannels: TRGBMatrix;
      ImgMaskCorr: TSingleMatrix;
      ImgNormCorr: TSingleMatrix;
    end;

    constructor Create(Image, Template: TIntegerMatrix; Multithread: Boolean);
  end;

constructor TMatchTemplateCache_CCOEFF.Create(Image, Template: TIntegerMatrix; Multithread: Boolean);
var
  ImageChannels, ImageMaskCorr: TRGBMatrix;
  ImageSlices: TImageSlices;
  I: Integer;
begin
  inherited Create();

  if Multithread then
    ImageSlices := SliceImage(Image, Template)
  else
    ImageSlices := [Image];

  Width := Image.Width;
  Height := Image.Height;

  SetLength(Slices, Length(ImageSlices));
  for I := 0 to High(ImageSlices) do
    with Slices[I] do
    begin
      MaskChannels   := MaskFromTemplate(Template);
      MaskSum        := Sum(MaskChannels.R);
      MaskSquaredSum := Sum(MaskChannels.R * MaskChannels.R);

      ImageChannels  := TRGBMatrix.Create(ImageSlices[I]);
      ImageMaskCorr  := CrossCorrRGB(ImageChannels, MaskChannels);

      ImgFFT := FFT2_RGB(ImageChannels);

      ImgChannels := ImageChannels;
      ImgMaskCorr := ImageChannels.Merge();
      ImgNormCorr := Sqrt(
        CrossCorrRGB(ImageChannels * ImageChannels, MaskChannels * MaskChannels) +
                    (ImageMaskCorr * (1 / MaskSum)) *
                    (ImageMaskCorr * (MaskSquaredSum / MaskSum) - CrossCorrRGB(ImageChannels, MaskChannels * MaskChannels) * 2)
        );
    end;
end;

function MatchTemplateMask_CCOEFF_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
begin
  Result := TMatchTemplateCache_CCOEFF.Create(Image, Template, False);
end;

function MatchTemplateMask_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
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
  ImgNormCorr := Sqrt(
    CrossCorrRGB(ImageChannels * ImageChannels, MaskChannels * MaskChannels) +
              (ImageMaskCorr * (1 / MaskSum)) *
              (ImageMaskCorr * (MaskSumSquared / MaskSum) - CrossCorrRGB(ImageChannels, MaskChannels * MaskChannels) * 2)
  );

  TemplChannels := TRGBMatrix.Create(Template);

  MaskTemplSum := Sum(MaskChannels * TemplChannels);

  TemplxMask := MaskChannels * (MaskChannels * (TemplChannels - [MaskTemplSum[0] / MaskSum, MaskTemplSum[1] / MaskSum, MaskTemplSum[2] / MaskSum]));
  TemplxMaskSum := Sum(TemplxMask);

  Result := CrossCorrRGB(ImageChannels, TemplxMask).Merge() - (ImgMaskCorr * (TemplxMaskSum[0] / MaskSum) + (TemplxMaskSum[1] / MaskSum) + (TemplxMaskSum[2] / MaskSum));
  if Normed then
    Result /= ImgNormCorr * Norm(TemplxMask);
end;

function MatchTemplateMask_CCOEFF_MT(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Template, @MatchTemplateMask_CCOEFF, Normed);
end;

function MatchTemplateMask_CCOEFF_CreateCache_MT(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
begin
  Result := TMatchTemplateCache_CCOEFF.Create(Image, Template, True);
end;

function MatchTemplateMask_CCOEFF_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  Cache: TMatchTemplateCache_CCOEFF absolute ACache;
  RowSize: Integer;

  procedure DoMatchTemplate(SliceIndex: Integer);
  var
    TemplChannel: TRGBMatrix;
    MaskTemplSum, TemplxMaskSum: TDoubleArray;
    TemplxMask: TRGBMatrix;
    Mat: TSingleMatrix;
    SliceOffset, Y: Integer;
  begin
    with Cache.Slices[SliceIndex] do
    begin
      TemplChannel := TRGBMatrix.Create(Template);

      MaskTemplSum := Sum(MaskChannels * TemplChannel);

      TemplxMask := MaskChannels * (MaskChannels * (TemplChannel - [MaskTemplSum[0] / MaskSum, MaskTemplSum[1] / MaskSum, MaskTemplSum[2] / MaskSum]));
      TemplxMaskSum := Sum(TemplxMask);
      Mat := CrossCorrRGB(ImgFFT, TemplxMask).Merge() -
                         (ImgMaskCorr * (TemplxMaskSum[0] / MaskSum) + (TemplxMaskSum[1] / MaskSum) + (TemplxMaskSum[2] / MaskSum));
      if Normed then
        Mat /= ImgNormCorr * Norm(TemplxMask);
    end;

    SliceOffset := (Cache.Height div Length(Cache.Slices)) * SliceIndex;
    for Y := 0 to Mat.Height - 1 do
      Move(Mat[Y, 0], Result[SliceOffset + Y, 0], RowSize);
  end;

var
  Tasks: TSimbaThreadPoolTasks;
  I: Integer;
begin
  if (not (ACache is TMatchTemplateCache_CCOEFF)) then
    raise Exception.Create('[MatchTemplateMask_CCOEFF]: Invalid cache');

  Result.SetSize(
    (Cache.Width - Template.Width) + 1,
    (Cache.Height - Template.Height) + 1
  );
  RowSize := Result.Width * SizeOf(Single);

  if Length(Cache.Slices) > 1 then
  begin
    SetLength(Tasks, Length(Cache.Slices));
    for I := 0 to High(Tasks) do
      Tasks[I] := TSimbaThreadPoolTask.Create(@DoMatchTemplate);

    SimbaThreadPool.RunParallel(Tasks);
  end else
    DoMatchTemplate(0);
end;

function MatchTemplate_CCOEFF(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
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
        if abs(numer) < denom then
          Result[Y, X] := numer / denom
        else if abs(numer) < denom*1.25 then
          if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
      end else
        Result[Y, X] := numer;
    end;
end;

function MatchTemplate_CCOEFF_MT(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Template, @MatchTemplate_CCOEFF, Normed);
end;

end.

