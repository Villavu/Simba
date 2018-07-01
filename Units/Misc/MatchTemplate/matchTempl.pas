unit matchTempl;
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
[==============================================================================}
{$I header.inc}

interface

uses
  SysUtils, mtCore;

const
  MATCH_CCORR         = 0;
  MATCH_CCORR_NORMED  = 1;
  MATCH_CCOEFF        = 2;
  MATCH_CCOEFF_NORMED = 3;
  MATCH_SQDIFF        = 4;
  MATCH_SQDIFF_NORMED = 5;


function  MatchTemplate(constref Image, Templ: T2DIntArray; TMFormula: Int32): T2DSingleArray;
procedure ClearCache();
function  LoadFFTWFrom(constref Path: String): Boolean;
procedure SetMaxFFTThreads(MaxThreads: Int32);
procedure DisableFFTW();
function  EnableFFTW(): Boolean;
procedure EnableFFTCache(Enabled: Boolean);

implementation

uses
  math, mtMatrix, mtThreading, mtcpuinfo, FFTPACK4, FFTW3;

type
  TRANSFORM_CACHE = record
    W,H: Int32;
    IsR2C: Boolean;
    Data: T2DSingleArray;
    Spec: T2DComplexArray;
  end;

var
  TM_USE_CACHE: Boolean = False;
  FFTCache: array [0..2] of TRANSFORM_CACHE;


// --------------------------------------------------------------------------------
// Alternative Cache

procedure ClearCache();
var i: Int32;
begin
  for i:=0 to High(FFTCache) do
  begin
    FFTCache[i].W := 0;
    FFTCache[i].H := 0;
    FFTCache[i].IsR2C := False;
    SetLength(FFTCache[i].Data, 0);
    SetLength(FFTCache[i].Spec, 0);
  end;
end;

function TryGetSpec(Image: T2DSingleArray; out Spec: T2DComplexArray; IsR2C: Boolean): Boolean;
var
  i,w,h: Int32;
  function Equals(a,b: T2DSingleArray): Boolean; inline;
  var x,y: Int32;
  begin
    Result := True;
    for y:=0 to H-1 do
      for x:=0 to W-1 do
        if Abs(a[y,x]-b[y,x]) > 0.001 then Exit(False);
  end;
begin
  Size(Image, W,H);
  Result := False;

  for i:=0 to High(FFTCache) do
  begin
    if (FFTCache[i].IsR2C <> IsR2C) then // R2C results can't be mixed with C2C
      continue;

    if (FFTCache[i].W <> W) or (FFTCache[i].H <> H) then
      continue;

    if not Equals(Image, FFTCache[i].Data) then
      continue;

    Spec := FFTCache[i].Spec;
    Exit(True);
  end;
end;

procedure Push2Cache(Data: T2DSingleArray; Spec: T2DComplexArray; W,H: Int32; IsR2C: Boolean);
var i: Int32;
begin
  for i:=High(FFTCache)-1 downto 0 do
    FFTCache[i+1] := FFTCache[i];

  FFTCache[0].Data  := Data;
  FFTCache[0].Spec  := Spec;
  FFTCache[0].IsR2C := IsR2C;
  FFTCache[0].W := W;
  FFTCache[0].H := H;
end;


// --------------------------------------------------------------------------------
// Helpers

function DoFFT2(I: T2DSingleArray; outW, outH: Int32; UseCache: Boolean = False): T2DComplexArray;
var
  x,y,W,H: Int32;
begin
  if UseCache and TryGetSpec(I, Result, FFTW.IsLoaded) then
    Exit(Result);

  Size(I, W,H);
  if FFTW.IsLoaded then
  begin
    SetLength(I, FFTW.OptimalDFTSize(outH), FFTW.OptimalDFTSize(outW));
    Result := FFTW.FFT2_R2C(I);
  end else
  begin
    SetLength(Result, FFTPACK.OptimalDFTSize(outH), FFTPACK.OptimalDFTSize(outW));
    for y:=0 to H-1 do for x:=0 to W-1 do Result[y,x].re := I[y,x];
    Result := FFTPACK.FFT2(Result);
  end;

  if UseCache then
    Push2Cache(I, Result, W,H, FFTW.IsLoaded);
end;

function DoIFFT2(Spec: T2DComplexArray; outW, outH: Int32): T2DSingleArray;
var
  x,y: Int32;
begin
  if FFTW.IsLoaded then
  begin
    Result := FFTW.FFT2_C2R(Spec);
    SetLength(Result, outH, outW);
  end else
  begin
    Spec := FFTPACK.IFFT2(Spec);
    SetLength(Result, outH, outW);
    for y:=0 to outH-1 do
      for x:=0 to outW-1 do Result[y,x] := Spec[y,x].re;
  end;
end;

procedure InitMatrix(out a: T2DSingleArray; H,W: Int32; InitValue:Int32);
var
  x,y: Int32;
begin
  SetLength(a, H,W);
  for y:=0 to H-1 do
    for x:=0 to W-1 do a[y,x] := InitValue;
end;


// --------------------------------------------------------------------------------
// a * conj(b)

procedure Parallel_MulSpecConj(Params: PParamArray; iLow, iHigh: Int32);
var
  x,y: Int32;
  a,b,r: T2DComplexArray;
  re,im: Single;
begin
  a := T2DComplexArray(Params^[0]^);
  b := T2DComplexArray(Params^[1]^);
  r := T2DComplexArray(Params^[2]^);
  for y:=iLow to iHigh do
    for x:=0 to High(a[y]) do
      begin
        re := (a[y,x].re *  b[y,x].re) - (a[y,x].im * -b[y,x].im);
        im := (a[y,x].re * -b[y,x].im) + (a[y,x].im *  b[y,x].re);
        r[y,x].re := re;
        r[y,x].im := im;
      end;
end;

function MulSpectrumConj(a,b: T2DComplexArray): T2DComplexArray;
var
  tc,W,H: Int32;
begin
  Size(a,W,H);
  tc := GetSystemThreadCount div 2;
  SetLength(Result, H,W);
  ThreadPool.DoParallel(@Parallel_MulSpecConj, [@a,@b,@Result], Low(a), High(a), tc, Area(a) < 300*300);
end;


// --------------------------------------------------------------------------------
// cross correlate

function CCORR(Image, Templ: T2DSingleArray): T2DSingleArray;
var
  iw,ih,tw,th: Int32;
  a,b: T2DComplexArray;
begin
  Size(Image, iw,ih);
  Size(Templ, tw,th);

  a := DoFFT2(Image, iw,ih, TM_USE_CACHE);
  b := DoFFT2(Templ, iw,ih);
  b := MulSpectrumConj(a,b);
  Result := DoIFFT2(b, iw-tw+1, ih-th+1);
end;

// ----------------------------------------------------------------------------
// Cross correlation (coefficient) of single channel matrices

function CCOEFF_1C(a,t: T2DSingleArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, wndDiff, wndSum: Double;
  xcorr: T2DSingleArray;
  sum, sum2: T2DDoubleArray;
begin
  xcorr := CCORR(a,t);
  Size(t, tw,th);

  invSize := Double(1.0) / Double(tw*th);
  MeanStdev(t, tplMean, tplSdv);
  tplSigma := tplSdv / Sqrt(invSize);

  if tplSdv < 0.00001 then
  begin
    InitMatrix(Result, Length(xcorr), Length(xcorr[0]), 1);
    Exit;
  end;

  sum := SumsPd(a, sum2);
  Size(sum, aw,ah);
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



// ----------------------------------------------------------------------------
// Cross correlation of R,G,B channels

function CCORR_RGB_HELPER(Image, Templ: T2DIntArray; out aR,aG,aB, tR,tG,tB: T2DSingleArray): T2DSingleArray;
var
  x,y,W,H: Int32;
  xR,xG,xB: T2DSingleArray;
begin
  SplitRGB(Image, aR,aG,aB);
  SplitRGB(Templ, tR,tG,tB);

  xR := CCORR(aR,tR);
  xG := CCORR(aG,tG);
  xB := CCORR(aB,tB);

  Result := xR;
  Size(Result, W,H);
  for y:=0 to H-1 do
    for x:=0 to W-1 do
      Result[y,x] := xR[y,x] + xG[y,x] + xB[y,x];
end;


// ----------------------------------------------------------------------------
// [Normalized] cross correlation of R,G,B channels

function CCORR_RGB(Image, Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplMean, tplSigma, mR,mG,mB, sR,sG,sB, wndSum2: Double;
  sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, aR,aG,aB, tR,tG,tB: T2DSingleArray;
begin
  xcorr := CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB);

  if not Normed then
    Exit(xcorr);

  Size(Templ, tw,th);
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

  Size(sum2r, aw,ah);
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

// ----------------------------------------------------------------------------
// [Normalized] Cross correlation (coefficient) of R,G,B channels

function CCOEFF_RGB(Image, Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSdv, tplSigma, wndSum2, wndMean2: Double;
  wndSumR, wndSumG, wndSumB: Double;
  mR,sR, mG,sG, mB,sB: Double;
  sumR, sumG, sumB, sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, aR,aG,aB, tR,tG,tB: T2DSingleArray;
begin
  xcorr := CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB);

  Size(Templ, tw,th);
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

  Size(sumR, aw,ah);
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

function SQDIFF_RGB(Image, Templ: T2DIntArray; Normed: Boolean): T2DSingleArray;
var
  x,y,tw,th,aw,ah: Int32;
  invSize, numer, denom, tplSigma, tplSum2, wndSum2: Double;
  tplMean, tplSdv, mR,sR, mG,sG, mB,sB:Double;
  sum2r, sum2g, sum2b: T2DDoubleArray;
  xcorr, aR,aG,aB, tR,tG,tB: T2DSingleArray;
begin
  xcorr := CCORR_RGB_HELPER(Image, Templ, aR,aG,aB, tR,tG,tB);

  Size(Templ, tw,th);
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

  Size(sum2r, aw,ah);
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


// ----------------------------------------------------------------------------
// Template matching using some of the above formulas

function MatchTemplate(constref Image, Templ: T2DIntArray; TMFormula: Int32): T2DSingleArray;
begin
  if FFTW.IsLoaded then
    FFTW.PrepareThreads(Area(Image));

  case TMFormula of
    MATCH_CCORR:         Result := CCORR_RGB(Image, Templ, False);
    MATCH_CCORR_NORMED:  Result := CCORR_RGB(Image, Templ, True);
    MATCH_CCOEFF:        Result := CCOEFF_RGB(Image, Templ, False);
    MATCH_CCOEFF_NORMED: Result := CCOEFF_RGB(Image, Templ, True);
    MATCH_SQDIFF:        Result := SQDIFF_RGB(Image, Templ, False);
    MATCH_SQDIFF_NORMED: Result := SQDIFF_RGB(Image, Templ, True);
    else
      raise Exception.Create('Not implemented');
  end;
end;


// ----------------------------------------------------------------------------
// FFT related stuff

function LoadFFTWFrom(constref Path: String): Boolean;
begin
  FFTW.Free();
  Result := FFTW.Init([Path], Min(4, GetSystemThreadCount()));
end;

procedure SetMaxFFTThreads(MaxThreads: Int32);
begin
  FFTW.MaxThreads := MaxThreads;
  FFTPACK.MaxThreads := MaxThreads;
end;

procedure DisableFFTW();
begin
  FFTW.IsLoaded := False;
end;

function EnableFFTW(): Boolean;
begin
  Result := False;
  if FFTW.Handle <> 0 then
  begin
    FFTW.IsLoaded := True;
    Result := True;
  end;
end;

procedure EnableFFTCache(Enabled: Boolean);
begin
  TM_USE_CACHE := Enabled;
end;


initialization
   ClearCache();

end.
