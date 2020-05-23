unit simba.fftw3;
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

{$I ../header.inc}

interface

uses
  sysutils, dynlibs,
  simba.matchtemplate_core;


type
  FFTW_PLAN  = type Pointer;

  TFFTW = record
    IsLoaded: Boolean;
    SearchPaths: TStrArray;
    Handle: TLibHandle;
    MaxThreads: Int32;
    MinThreadingSize: Int32;

    function Init(ASearchPaths: array of String; AMaxThreads:Int32): Boolean;
    procedure Free();

    function OptimalDFTSize(target: Int32): Int32;
    function OptimalEvenDFTSize(target: Int32): Int32;

    procedure PrepareThreads(ForSize: Int32);

    function FFT2(m: T2DComplexArray): T2DComplexArray;
    function IFFT2(m: T2DComplexArray): T2DComplexArray;
    function FFT2_R2C(m: T2DSingleArray): T2DComplexArray;
    function FFT2_C2R(m: T2DComplexArray): T2DSingleArray;
  end;

const
  MIN_THREDING_SZ = 333*333;

  FFTW_FORWARD         =-1;
  FFTW_BACKWARD        = 1;

  FFTW_MEASURE         = 0;
  FFTW_DESTROY_INPUT   = 1;   {1U << 0}
  FFTW_UNALIGNED       = 2;   {1U << 1}
  FFTW_CONSERVE_MEMORY = 4;   {1U << 2}
  FFTW_EXHAUSTIVE      = 8;   {1U << 3} {NO_EXHAUSTIVE is default }
  FFTW_PRESERVE_INPUT  = 16;  {1U << 4} {cancels FFTW_DESTROY_INPUT}
  FFTW_PATIENT         = 32;  {1U << 5} {IMPATIENT is default }
  FFTW_ESTIMATE        = 64;  {1U << 6}   

  {$IF Defined(WIN32)}
  LIB_NAME = 'libfftw3f-3_32.dll';
  {$ELSEIF Defined(WIN64)}
  LIB_NAME = 'libfftw3f-3_64.dll';
  {$ELSEIF Defined(CPU386) and Defined(LINUX)}
  LIB_NAME = 'libfftw3f-3_32.so';
  {$ELSEIF Defined(CPUX86_64) and Defined(LINUX)}
  LIB_NAME = 'libfftw3f-3_64.so';
  {$ENDIF}

  {$IF Defined(Windows)}
  ALT_LIB_NAME = 'libfftw3f-3.dll';
  {$ELSEIF Defined(Linux)}
  ALT_LIB_NAME = 'libfftw3f-3.so';
  {$ENDIF}


var
  FFTW: TFFTW;

  fftw_init_threads:       function(): Int32; cdecl;
  fftw_plan_with_nthreads: procedure(nthreads: Int32); cdecl;
  fftw_cleanup_threads:    procedure(); cdecl;
  fftw_malloc:             function(n: Int32): Pointer; cdecl;
  fftw_free:               procedure(p: Pointer); cdecl;
  fftw_plan_dft1:          function(n: Int32; inData, outData: PComplex; sign: Int32; flags: UInt32): FFTW_PLAN; cdecl;
  fftw_plan_dft2:          function(n0, n1: Int32; inData, outData: PComplex; sign: Int32; flags: UInt32): FFTW_PLAN; cdecl;
  fftw_plan_dft3:          function(n0, n1, n2: Int32; inData, outData: PComplex; sign: Int32; flags: UInt32): FFTW_PLAN; cdecl;
  fftw_plan_dft2_r2c:      function(n0, n1: Int32; inData: PSingle; outData: PComplex; flags: UInt32): FFTW_PLAN; cdecl;
  fftw_plan_dft2_c2r:      function(n0, n1: Int32; inData: PComplex; outData: PSingle; flags: UInt32): FFTW_PLAN; cdecl;
  fftw_exec:               procedure(plan: FFTW_PLAN); cdecl;
  fftw_exec_dft:           procedure(plan: FFTW_PLAN; inData, outData: PComplex); cdecl;
  fftw_free_plan:          procedure(plan: FFTW_PLAN); cdecl;
  

implementation

uses
  math, utf8process,
  simba.matchtemplate_matrix;


const
  __OptimalDFT: array[0..168] of Int32 = (
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


function TFFTW.Init(ASearchPaths: array of String; AMaxThreads:Int32): Boolean;
var
  i:Int32;
begin
  Self.Handle := 0;
  Self.IsLoaded := False;
  Self.MinThreadingSize := MIN_THREDING_SZ;
  Self.MaxThreads := AMaxThreads;

  SetLength(Self.SearchPaths, Length(ASearchPaths));
  for i:=0 to High(ASearchPaths) do
  begin
    Self.SearchPaths[i] := ASearchPaths[i];
    if (not Self.IsLoaded) and ((Self.SearchPaths[i] = '') or DirectoryExists(Self.SearchPaths[i])) then
    begin
      Self.Handle := LoadLibrary(Self.SearchPaths[i] + LIB_NAME);
      if Self.Handle = 0 then
        Self.Handle := LoadLibrary(Self.SearchPaths[i] + ALT_LIB_NAME);

      Self.IsLoaded := Self.Handle <> 0;
    end;
  end;

  if Self.IsLoaded then
  begin
    WriteLn('Found FFTW3.3 - Loaded successfully');

    Pointer(fftw_init_threads)       := GetProcAddress(Self.Handle, 'fftwf_init_threads');
    Pointer(fftw_plan_with_nthreads) := GetProcAddress(Self.Handle, 'fftwf_plan_with_nthreads');
    Pointer(fftw_cleanup_threads)    := GetProcAddress(Self.Handle, 'fftwf_cleanup_threads');
    Pointer(fftw_malloc)        := GetProcAddress(Self.Handle, 'fftwf_malloc');
    Pointer(fftw_free)          := GetProcAddress(Self.Handle, 'fftwf_free');
    Pointer(fftw_plan_dft1)     := GetProcAddress(Self.Handle, 'fftwf_plan_dft_1d');
    Pointer(fftw_plan_dft2)     := GetProcAddress(Self.Handle, 'fftwf_plan_dft_2d');
    Pointer(fftw_plan_dft3)     := GetProcAddress(Self.Handle, 'fftwf_plan_dft_3d');
    Pointer(fftw_plan_dft2_r2c) := GetProcAddress(Self.Handle, 'fftwf_plan_dft_r2c_2d');
    Pointer(fftw_plan_dft2_c2r) := GetProcAddress(Self.Handle, 'fftwf_plan_dft_c2r_2d');
    Pointer(fftw_free_plan)     := GetProcAddress(Self.Handle, 'fftwf_destroy_plan');
    Pointer(fftw_exec)          := GetProcAddress(Self.Handle, 'fftwf_execute');
    Pointer(fftw_exec_dft)      := GetProcAddress(Self.Handle, 'fftwf_execute_dft');

    fftw_init_threads();
  end;

  Result := Self.IsLoaded;
end;


procedure TFFTW.Free();
begin
  if Self.IsLoaded then
    FreeLibrary(Self.Handle);

  Self.Handle := 0;
  Self.IsLoaded := False;
  Self.MinThreadingSize := 0;
  Self.MaxThreads := 0;
end;


// --------------------------------------------------------------------------------
// Compute the optimal size for FFTW

function TFFTW.OptimalDFTSize(target: Int32): Int32;
var
  n,match,quotient,p2,p5,p35: Int32;
begin
  if (target <= 6) then
    Exit(target);
  
  if NextPow2(target) = target then
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
      p2 := NextPow2(quotient);
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

function TFFTW.OptimalEvenDFTSize(target: Int32): Int32;
var
  n: Int32;
begin
  n := Target;
  while True do
  begin
    n := OptimalDFTSize(n);
    if n and 1 = 0 then Exit(n);
    Inc(n);
  end;
end;


// --------------------------------------------------------------------------------
// Prepare FFTW for multithreaded FFT

procedure TFFTW.PrepareThreads(ForSize: Int32);
begin
  if (ForSize < Self.MinThreadingSize) then
    fftw_plan_with_nthreads(1)
  else
    fftw_plan_with_nthreads(Self.MaxThreads)
end;



// ----------------------------------------------------------------------------
// FFT2D - Complex 2 Complex

function TFFTW.FFT2(m: T2DComplexArray): T2DComplexArray;
var
  y,w,h,row,sz: Int32;
  _data, _res: PComplex;  
  plan: FFTW_PLAN;
begin
  Size(m, W,H);
  if W = 0 then Exit;

  row := SizeOf(Complex) * W;
  sz  := row * H;

  _data := fftw_malloc(sz);
  _res  := fftw_malloc(sz);
  plan  := fftw_plan_dft2(H,W, _data, _res, FFTW_FORWARD, FFTW_ESTIMATE);

  for y:=0 to h-1 do Move(m[y,0], _data[y*w], row);
  fftw_exec(plan);
  for y:=0 to h-1 do Move(_res[y*w], m[y,0], row);

  fftw_free(_data);
  fftw_free(_res);
  fftw_free_plan(plan);

  Result := m;
end;

function TFFTW.IFFT2(m: T2DComplexArray): T2DComplexArray;
var
  x,y,row,sz,w,h: Int32;
  f: Single;   
  _data, _res: PComplex;  
  plan: FFTW_PLAN;
begin
  Size(m, W,H);
  if W = 0 then Exit;
  
  row := SizeOf(Complex) * W;
  sz  := row * H;

  _data := fftw_malloc(sz);
  _res  := fftw_malloc(sz);
  plan  := fftw_plan_dft2(H,W, _data, _res, FFTW_BACKWARD, FFTW_ESTIMATE);
  
  for y:=0 to h-1 do Move(m[y,0], _data[y*w], row);
  fftw_exec(plan);
  for y:=0 to h-1 do Move(_res[y*w], m[y,0], row);

  fftw_free(_data);
  fftw_free(_res);
  fftw_free_plan(plan);

  Result := m;
  f := 1.0 / (W*H);
  for y:=0 to H-1 do
    for x:=0 to W-1 do
    begin
      Result[y,x].re := Result[y,x].re * f;
      Result[y,x].im := Result[y,x].im * f;
    end;
end;


// ----------------------------------------------------------------------------
// FFT 2D - Real 2 Complex  &  Complex 2 Real

function TFFTW.FFT2_R2C(m: T2DSingleArray): T2DComplexArray;
var
  y,w,h,newW: Int32;
  input: PSingle;
  output: PComplex;
  plan: FFTW_PLAN;
begin
  Size(m, W,H);
  if W = 0 then Exit;
  SetLength(Result, H,W);

  newW := W div 2+1;

  input  := fftw_malloc(SizeOf(Single)   * H*W);
  output := fftw_malloc(SizeOf(Complex) * H*newW);
  plan := fftw_plan_dft2_r2c(H,W, input, output, FFTW_ESTIMATE);

  for y:=0 to H-1 do Move(m[y,0], input[y*w], SizeOf(Single) * W);
  fftw_exec(plan);
  for y:=0 to H-1 do Move(output[y*newW], Result[y,0], SizeOf(Complex) * newW);

  fftw_free(input);
  fftw_free(output);
  fftw_free_plan(plan);
end;

function TFFTW.FFT2_C2R(m: T2DComplexArray): T2DSingleArray;
var
  x,y,w,h,newW: Int32;
  input: PComplex;
  output: PSingle;
  plan: FFTW_PLAN;
  f: Single;
begin
  Size(m, W,H);
  if W = 0 then Exit;
  SetLength(Result, H,W);

  newW := W div 2+1;

  input  := fftw_malloc(SizeOf(Complex) * H*newW);
  output := fftw_malloc(SizeOf(Single)   * H*W);
  plan  := fftw_plan_dft2_c2r(H,W, input, output, FFTW_ESTIMATE);

  for y:=0 to h-1 do Move(m[y,0], input[y*newW], SizeOf(Complex) * newW);
  fftw_exec(plan);
  for y:=0 to h-1 do Move(output[y*w], Result[y,0], SizeOf(Single) * W);

  fftw_free(input);
  fftw_free(output);
  fftw_free_plan(plan);

  f := 1.0 / (W*H);
  for y:=0 to H-1 do
    for x:=0 to W-1 do Result[y,x] := Result[y,x] * f;
end;



// ----------------------------------------------------------------------------
// Initialize library

initialization
  FFTW.Init(['','../', 'Plugins/', 'Includes/', 'Scripts/'], Min(4, GetSystemThreadCount()));


end.
