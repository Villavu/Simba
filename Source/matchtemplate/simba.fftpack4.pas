unit simba.fftpack4;
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
[==============================================================================}
{$i simba.inc}

{$MODESWITCH ARRAYOPERATORS OFF}

interface

uses
  sysutils, math,
  simba.math, simba.mufasatypes, simba.matchtemplate_matrix;

type
  TFFTPACK = record
    function OptimalDFTSize(const target: Integer): Integer;

    function InitFFT(const n: Integer): TComplexArray;
    function FFT(const a, wsave: TComplexArray; const Inplace: Boolean = False): TComplexArray;
    function IFFT(const a, wsave: TComplexArray; const Inplace: Boolean = False): TComplexArray;

    function InitRFFT(const n: Integer): TSingleArray;
    function RFFT(const a, wsave: TSingleArray; const Inplace: Boolean = False): TSingleArray;
    function IRFFT(const a, wsave: TSingleArray; const Inplace: Boolean = False): TSingleArray;

    function FFT2(const m: TComplexMatrix): TComplexMatrix;
    function IFFT2(const m: TComplexMatrix): TComplexMatrix;
  end;

var
  FFTPACK: TFFTPACK;

implementation

uses
  simba.fftpack4_core;

const
  __OptimalDFT: array[0..168] of Integer = (
    8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40, 45, 48,
    50, 54, 60, 64, 72, 75, 80, 81, 90, 96, 100, 108, 120, 125, 128,
    135, 144, 150, 160, 162, 180, 192, 200, 216, 225, 240, 243, 250,
    256, 270, 288, 300, 320, 324, 360, 375, 384, 400, 405, 432, 450,
    480, 486, 500, 512, 540, 576, 600, 625, 640, 648, 675, 720, 729,
    750, 768, 800, 810, 864, 900, 960, 972, 1000, 1024, 1080, 1125,
    1152, 1200, 1215, 1250, 1280, 1296, 1350, 1440, 1458, 1500, 1536,
    1600, 1620, 1728, 1800, 1875, 1920, 1944, 2000, 2025, 2048, 2160,
    2187, 2250, 2304, 2400, 2430, 2500, 2560, 2592, 2700, 2880, 2916,
    3000, 3072, 3125, 3200, 3240, 3375, 3456, 3600, 3645, 3750, 3840,
    3888, 4000, 4050, 4096, 4320, 4374, 4500, 4608, 4800, 4860, 5000,
    5120, 5184, 5400, 5625, 5760, 5832, 6000, 6075, 6144, 6250, 6400,
    6480, 6561, 6750, 6912, 7200, 7290, 7500, 7680, 7776, 8000, 8100,
    8192, 8640, 8748, 9000, 9216, 9375, 9600, 9720, 10000
  );

{$DEFINE CPLX_BUFFSZ := 2*n + 15}
{$DEFINE REAL_BUFFSZ := 2*n + 15}

// --------------------------------------------------------------------------------
// Compute the optimal size for FFT

function TFFTPACK.OptimalDFTSize(const target: Integer): Integer;
var
  n,match,quotient,p2,p5,p35: Integer;
begin
  if (target <= 6) then
    Exit(target);
  
  if NextPower2(target) = target then
    Exit(target);
  
  n := 0;
  if target <= __OptimalDFT[High(__OptimalDFT)] then
  begin
    while __OptimalDFT[n] < target do Inc(n);
    Exit(__OptimalDFT[n]);
  end;

  match := $7FFFFFFF;
  p5 := 1;
  while p5 < target do
  begin
    p35 := p5;
    while p35 < target do
    begin
      quotient := Ceil(target / p35);
      p2 := NextPower2(quotient);
      N := p2 * p35;

      if N = target then Exit(N);
      if N < match  then match := N;

      p35 *= 3;
      if p35 = target then Exit(p35)
    end;
    if p35 < match then
      match := p35;
    
    p5 *= 5;
    if p5 = target then
      Exit(p5);
  end;
  Result := Min(p5, match);
end;

// --------------------------------------------------------------------------------
// complex 2 complex FFT

function TFFTPACK.InitFFT(const n: Integer): TComplexArray;
begin
  SetLength(Result, CPLX_BUFFSZ);
  cffti(n, @Result[0]);
end;

function TFFTPACK.FFT(const a, wsave: TComplexArray; const Inplace: Boolean): TComplexArray;
var n: Integer;
begin
  if Inplace then Result := a
  else            Result := Copy(a);
  n := Length(a); 
  Assert(Length(wsave) = CPLX_BUFFSZ, Format('Invalid work array for fft size (a: %d, w: %d)',[CPLX_BUFFSZ, Length(wsave)]));
  cfftf(n, @Result[0], @wsave[0]);
end;

function TFFTPACK.IFFT(const a, wsave: TComplexArray; const Inplace: Boolean): TComplexArray;
var
  n: Integer;
  f: Single;
begin
  if Inplace then Result := a
  else            Result := Copy(a);
  n := Length(a);
  Assert(Length(wsave) = CPLX_BUFFSZ, Format('Invalid work array for fft size (a: %d, w: %d)',[CPLX_BUFFSZ, Length(wsave)]));
  cfftb(n, @Result[0], @wsave[0]);

  f := 1.0 / n;
  for n:=0 to High(a) do
  begin
    Result[n].re *= f;
    Result[n].im *= f;
  end;
end;

// --------------------------------------------------------------------------------
// real 2 real FFT

function TFFTPACK.InitRFFT(const n: Integer): TSingleArray;
begin
  SetLength(Result, REAL_BUFFSZ);
  rffti(n, @Result[0]);
end;

function TFFTPACK.RFFT(const a, wsave: TSingleArray; const Inplace: Boolean): TSingleArray;
var n: Integer;
begin
  if Inplace then Result := a
  else            Result := Copy(a);
  n := Length(a);
  Assert(Length(wsave) = REAL_BUFFSZ, Format('Invalid work array for fft size (a: %d, w: %d)',[REAL_BUFFSZ, Length(wsave)]));
  rfftf(n, @Result[0], @wsave[0]);
end;

function TFFTPACK.IRFFT(const a, wsave: TSingleArray; const Inplace: Boolean): TSingleArray;
var
  n: Integer;
  f: Single;
begin
  if Inplace then Result := a
  else            Result := Copy(a);
  n := Length(a);
  Assert(Length(wsave) = REAL_BUFFSZ, Format('Invalid work array for fft size (a: %d, w: %d)',[REAL_BUFFSZ, Length(wsave)]));
  rfftb(n, @Result[0], @wsave[0]);

  f := 1.0 / n;
  for n:=0 to High(a) do Result[n] *= f;
end;

// --------------------------------------------------------------------------------
// 2d complex fft

function TFFTPACK.FFT2(const m: TComplexMatrix): TComplexMatrix;
var
  Y: Integer;
  plan: TComplexArray;
  rot: TComplexMatrix;
begin
  plan := InitFFT(m.Width);
  for Y := 0 to m.Height - 1 do
    FFTPACK.FFT(m[Y], Plan, True);

  rot := Rot90(m);
  plan := InitFFT(rot.Width);
  for Y := 0 to rot.Height - 1 do
    FFTPACK.FFT(rot[Y], Plan, True);

  Result := Rot90(rot);
end;

function TFFTPACK.IFFT2(const m: TComplexMatrix): TComplexMatrix;
var
  Y: Integer;
  plan: TComplexArray;
  rot: TComplexMatrix;
begin
  plan := InitFFT(m.Width);
  for Y := 0 to m.Height - 1 do
    FFTPACK.IFFT(m[Y], Plan, True);

  rot := Rot90(m);
  plan := InitFFT(rot.Width);
  for Y := 0 to rot.Height - 1 do
    FFTPACK.IFFT(rot[Y], Plan, True);

  Result := Rot90(rot);
end;

end.
