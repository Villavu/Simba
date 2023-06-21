{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.matchtemplate_ccorr;

{$i simba.inc}

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.matchtemplate,
  simba.matchtemplate_matrix, simba.matchtemplate_helpers;

function MatchTemplateMask_CCORR_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
function MatchTemplateMask_CCORR_CreateCache_MT(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;

function MatchTemplateMask_CCORR(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCORR_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplateMask_CCORR_MT(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

function MatchTemplate_CCORR(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
function MatchTemplate_CCORR_MT(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;

implementation

uses
  simba.threadpool;

type
  TMatchTemplateCache_CCORR = class(TMatchTemplateCacheBase)
  public
    Slices: array of record
      ImageChannels, MaskChannels: TRGBMatrix;
      Mask2: TRGBMatrix;
      TempResult: TSingleMatrix;
    end;

    constructor Create(Image, Template: TIntegerMatrix; Multithread: Boolean);
  end;

constructor TMatchTemplateCache_CCORR.Create(Image, Template: TIntegerMatrix; Multithread: Boolean);
var
  Img2: TRGBMatrix;
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
      MaskChannels := MaskFromTemplate(Template);
      ImageChannels := TRGBMatrix.Create(ImageSlices[I]);
      Img2 := (ImageChannels * ImageChannels);
      Mask2 := (MaskChannels * MaskChannels);
      TempResult := CrossCorrRGB(Img2, Mask2).Merge();
    end;
end;

function MatchTemplateMask_CCORR_CreateCache(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
begin
  Result := TMatchTemplateCache_CCORR.Create(Image, Template, False);
end;

function MatchTemplateMask_CCORR_CreateCache_MT(Image, Template: TIntegerMatrix): TMatchTemplateCacheBase;
begin
  Result := TMatchTemplateCache_CCORR.Create(Image, Template, True);
end;

function MatchTemplateMask_CCORR(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
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

function MatchTemplateMask_CCORR_Cache(ACache: TMatchTemplateCacheBase; Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
var
  Cache: TMatchTemplateCache_CCORR absolute ACache;
  RowSize: Integer;

  procedure DoMatchTemplate(SliceIndex: Integer);
  var
    Mat: TSingleMatrix;
    SliceOffset, X, Y, W, H: Integer;
    TemplChannels: TRGBMatrix;
    Templ2Mask2Sum: Double;
  begin
    with Cache.Slices[SliceIndex] do
    begin
      TemplChannels := TRGBMatrix.Create(Template);
      Mat := CrossCorrRGB(ImageChannels, TemplChannels * Mask2).Merge();

      if Normed then
      begin
        Templ2Mask2Sum := SumOfSquares(TemplChannels * MaskChannels);

        W := Mat.Width - 1;
        H := Mat.Height - 1;
        for Y := 0 to H do
          for X := 0 to W do
            Mat[Y, X] := Mat[Y, X] / Sqrt(Templ2Mask2Sum * TempResult[Y, X]);
      end;
    end;

    SliceOffset := (Cache.Height div Length(Cache.Slices)) * SliceIndex;
    for Y := 0 to Mat.Height - 1 do
      Move(Mat[Y, 0], Result[SliceOffset + Y, 0], RowSize);
  end;

var
  Tasks: TSimbaThreadPoolTasks;
  I: Integer;
begin
  if (not (ACache is TMatchTemplateCache_CCORR)) then
    raise Exception.Create('[MatchTemplateMask_CCORR]: Invalid cache');

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

  Result.ReplaceNaNAndInf(0);
end;

function MatchTemplateMask_CCORR_MT(Image, Template: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Template, @MatchTemplateMask_CCORR, Normed);
end;

function MatchTemplate_CCORR(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
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
      else if abs(numer) < denom*1.25 then
        if numer > 0 then Result[Y, X] := 1 else Result[Y, X] := -1;
    end;
end;

function MatchTemplate_CCORR_MT(Image, Templ: TIntegerMatrix; Normed: Boolean): TSingleMatrix;
begin
  Result := Multithread(Image, Templ, @MatchTemplate_CCORR, Normed);
end;

end.

