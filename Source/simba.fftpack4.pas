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
unit simba.fftpack4;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.base,
  simba.math;

const
  FFT_PLAN_ALIGN = 32;                     // Align plan to 32 bit
  FFT_THREADING: Boolean = True;           // runtime on/off switch
  FFT_THREADING_DEBUG: Boolean = False;    // live debug on threading status
  FFT_THREADING_MIN_AREA: Integer = 40000; // W*H below this always runs single-threaded (200x200)
  FFT_MAX_THREADS: Integer = 6;            // max threads to use; users may raise it

type
  TComplex = record
    Re, Im: Single;
  end;
  TComplexArray = array of TComplex;

  // flattened (contiguous) complex matrix: element (Y,X) = Data[Y*Width+X]
  TComplexMatrix = record
    Data: TComplexArray;
    FWidth, FHeight: Integer;
    function Width: Integer; inline;
    function Height: Integer; inline;
    procedure SetSize(AWidth, AHeight: Integer);
  end;

// picks a 5-smooth transform size
function OptimalDFTSize(const target: Integer): Integer;
// forward 2D transform
function FFT2(const m: TComplexMatrix): TComplexMatrix;
// inverse 2D transform
function IFFT2(const m: TComplexMatrix): TComplexMatrix;

implementation

uses
  syncobjs,
  simba.fftpack4_core,
  simba.threading;

{$DEFINE CPLX_BUFFSZ := 2*n + 15 + (FFT_PLAN_ALIGN div SizeOf(TComplex))}

const
  OptimalDFTs: array of Integer = (
    8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, 40, 45, 48,
    50, 54, 60, 64, 72, 75, 80, 81, 90, 96, 108, 120, 128, 144, 150,
    160, 180, 192, 200, 225, 256, 300, 320, 324, 375, 400, 432, 500,
    576, 648, 675, 729, 768, 810, 864, 972, 1080, 1152, 1296, 1350,
    1440, 1620, 1728, 1800, 1920, 1944, 2025, 2048, 2160, 2187, 2250,
    2304, 2400, 2430, 2500, 2560, 2592, 2700, 2880, 2916, 3000, 3072,
    3125, 3200, 3240, 3375, 3456, 3600, 3645, 3750, 3840, 3888, 4000,
    4050, 4096, 4320, 4374, 4500, 4608, 4800, 4860, 5000, 5120, 5184,
    5400, 5625, 5760, 5832, 6000, 6075, 6144, 6250, 6400, 6480, 6561,
    6750, 6912, 7200, 7290, 7500, 7680, 7776, 8000, 8100, 8192, 8640,
    8748, 9000, 9216, 9375, 9600, 9720, 10000
  );

function TComplexMatrix.Width: Integer;
begin
  Result := FWidth;
end;

function TComplexMatrix.Height: Integer;
begin
  Result := FHeight;
end;

procedure TComplexMatrix.SetSize(AWidth, AHeight: Integer);
begin
  FWidth  := AWidth;
  FHeight := AHeight;
  SetLength(Data, AHeight * AWidth);
end;

// Transposes a matrix of complex numbers using a cache-blocked (tiled) algorithm.
// The matrix is processed in small sub-blocks rather than element-by-element
// improving memory locality and performance on large matrices.
// Output is the standard transpose: element at (i, j) is moved to (j, i)
procedure TransposeComplexBlocked(const src, dst: PInt64; const SrcH, SrcW: Integer);
const
  B = 8;
var
  y, x, yEnd, xEnd: Integer;
  srcRow, dstCol, cur, curDest, srcRowEnd, curEnd: PInt64;
begin
  y := 0;
  while (y < SrcH) do
  begin
    yEnd := y + B;
    if yEnd > SrcH then
      yEnd := SrcH;

    x := 0;
    while (x < SrcW) do
    begin
      xEnd := x + B;
      if xEnd > SrcW then
        xEnd := SrcW;

      srcRow    := @src[y * SrcW + x];   // src[y][x]
      srcRowEnd := @src[yEnd * SrcW + x];
      dstCol    := @dst[x * SrcH + y];   // dst[x][y]
      while (PtrUInt(srcRow) < PtrUInt(srcRowEnd)) do   // each source row in the tile
      begin
        cur     := srcRow;
        curDest := dstCol;
        curEnd  := @srcRow[xEnd - x];
        while (PtrUInt(cur) < PtrUInt(curEnd)) do
        begin
          curDest^ := cur^;
          Inc(cur);              // contiguous source
          Inc(curDest, SrcH);    // strided destination column
        end;
        Inc(srcRow, SrcW);
        Inc(dstCol);
      end;
      x := x + B;
    end;
    y := y + B;
  end;
end;

function OptimalDFTSize(const target: Integer): Integer;
var
  n,match,quotient,p2,p5,p35: Integer;
begin
  if (target <= 6) then
    Exit(target);

  n := 0;
  if (target <= OptimalDFTs[High(OptimalDFTs)]) then
  begin
    while (OptimalDFTs[n] < target) do
      Inc(n);
    Exit(OptimalDFTs[n]);
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

function AlignPlan(const w: TComplexArray): PSingle; inline;
begin
  Result := PSingle((PtrUInt(@w[0]) + (FFT_PLAN_ALIGN - 1)) and not PtrUInt(FFT_PLAN_ALIGN - 1));
end;

procedure InitFFT(var Plan: TComplexArray; const n: Integer); inline;
begin
  if (Length(Plan) < CPLX_BUFFSZ) then
    SetLength(Plan, CPLX_BUFFSZ);
  cffti(n, AlignPlan(Plan));
end;

function ShouldParallelFFT(const Area: Int64): Boolean; inline;
begin
  {$IFDEF MT_THREADING}
  Result := FFT_THREADING and (Area >= FFT_THREADING_MIN_AREA);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

type
  TFFTPass = procedure(const Lo, Hi: Integer) of object;

  // a 2D-FFT engine with its own scratch buffers (Work/Spec) + twiddle tables (PlanW/PlanH) the FFT2/IFFT2 ops.
  TFFT2D = record
  private
    Work, Spec: TComplexArray;
    W, H: Integer;
    PlanW, PlanH: TComplexArray;
    PlanWdim, PlanHdim: Integer;
    procedure EnsureW(len: Integer); // build only when the length changes
    procedure EnsureH(len: Integer); // ..
    procedure Prepare(AW, AH: Integer);
    procedure RowFwd(const Lo, Hi: Integer); // the four 1-D passes (forward/inverse x rows/cols)
    procedure ColFwd(const Lo, Hi: Integer);
    procedure ColInv(const Lo, Hi: Integer);
    procedure RowInv(const Lo, Hi: Integer);
    procedure TransposeForward;
    procedure TransposeInverse;
    procedure RunForward;
    procedure RunInverse;
  public
    function FFT2(const m: TComplexMatrix): TComplexMatrix;
    function IFFT2(const m: TComplexMatrix): TComplexMatrix;
  end;

  TFFTWorker = class(TThread)
  public
    Wake, Done: TSimpleEvent;
    Pass: TFFTPass;
    Lo, Hi: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TFFTThreadPool = class
  private
    FWorkers: array of TFFTWorker;
    FLock: TCriticalSection;        // guards EnsureSetup
    FAcquireLock: TCriticalSection; // held by the ONE transform currently using the workers
    FReady: Boolean;
    FBuiltThreads: Integer;         // worker count the live pool was built for (tracks FFT_MAX_THREADS)

    function WantWorkers: Integer; inline; // FFT_MAX_THREADS clamped to the available cores
    function ThreadsForArea(const Area: Int64): Integer; // scale worker count with transform size
    procedure EnsureSetup;
    procedure Run(const Hi: Integer; const Pass: TFFTPass; const MaxN: Integer); // split [0,Hi] across up to MaxN (caller + workers)
  public
    constructor Create;
    destructor Destroy; override;

    function Acquire: Boolean;
    procedure Leave;
    // Parallel forward / inverse transform on Engine. The caller must already hold Acquire
    procedure RunForward(var Eng: TFFT2D);
    procedure RunInverse(var Eng: TFFT2D);
  end;

var
  ThreadPool: TFFTThreadPool;

threadvar
  FFTEngine: TFFT2D; // Each thread's own FFT engine (scratch buffers + plan)
                     // reused across that thread's FFT2/IFFT2 calls.

constructor TFFTWorker.Create;
begin
  Wake := TSimpleEvent.Create();
  Done := TSimpleEvent.Create();
  inherited Create(False, 256 * 1024); // all data on heap, dont need big stack size
  Priority := tpHigher;
end;

destructor TFFTWorker.Destroy;
begin
  Terminate();
  Wake.SetEvent(); // unblock so Execute sees Terminated
  inherited Destroy(); // WaitFor
  Wake.Free();
  Done.Free();
end;

procedure TFFTWorker.Execute;
begin
  SetThreadPCore(); // maybe pin this thread to PCores

  while (not Terminated) do
  begin
    Wake.WaitFor(INFINITE);
    if Terminated then
      Break;
    Wake.ResetEvent();
    try
      Pass(Lo, Hi);
    except
      on E: Exception do
        DebugLn('[FFT Threading]: Pass(%d..%d) exception: %s', [Lo, Hi, E.Message]);
    end;
    Done.SetEvent();
  end;
end;

constructor TFFTThreadPool.Create;
begin
  inherited Create();

  FLock := TCriticalSection.Create();
  FAcquireLock := TCriticalSection.Create();
end;

destructor TFFTThreadPool.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FWorkers) do
    FWorkers[i].Free();
  FWorkers := nil;
  FAcquireLock.Free();
  FLock.Free();

  inherited Destroy();
end;

function TFFTThreadPool.Acquire: Boolean;
begin
  Result := FAcquireLock.TryEnter();
end;

procedure TFFTThreadPool.Leave;
begin
  FAcquireLock.Release();
end;

function TFFTThreadPool.WantWorkers: Integer;
begin
  Result := FFT_MAX_THREADS;                    // user-controlled; may be raised above the P-core count
  if (Result > SimbaCPUInfo.ThreadCount) then   // never spawn more workers than logical CPUs
    Result := SimbaCPUInfo.ThreadCount;
  if (Result < 1) then
    Result := 1;
end;

function TFFTThreadPool.ThreadsForArea(const Area: Int64): Integer;
begin
  if (FFT_THREADING_MIN_AREA < 1) then // disabled - use the whole pool
    Result := FBuiltThreads
  else
  begin
    // scale threads to use count with one thread per FFT_THREADING_MIN_AREA of area
    // e.g. 200x200 -> 2, 300x300 -> 3, >=512x512 -> FBuiltThreads
    Result := 1 + (Area div FFT_THREADING_MIN_AREA);
    if (Result > FBuiltThreads) then
      Result := FBuiltThreads;
  end;
  if (Result < 1) then
    Result := 1;
end;

procedure TFFTThreadPool.EnsureSetup;
var
  i, want: Integer;
begin
  want := WantWorkers();
  if FReady and (FBuiltThreads = want) then // already built for the current FFT_MAX_THREADS
    Exit;

  FLock.Acquire();
  try
    want := WantWorkers();
    if FReady and (FBuiltThreads = want) then
      Exit;

    if FFT_THREADING_DEBUG then
      DebugLn('[FFT Threading]: Setup %d sized thread pool', [want]);

    for i := 0 to High(FWorkers) do
      FWorkers[i].Free();
    SetLength(FWorkers, want - 1); // the caller is the +1
    for i := 0 to High(FWorkers) do
      FWorkers[i] := TFFTWorker.Create();

    FBuiltThreads := want;
    FReady := True;
  finally
    FLock.Release();
  end;
end;

procedure TFFTThreadPool.Run(const Hi: Integer; const Pass: TFFTPass; const MaxN: Integer);
var
  total, n, per, i, clo, chi, started: Integer;
begin
  total := Hi + 1;
  if (total <= 0) then
    Exit;

  n := Length(FWorkers) + 1;
  if (n > MaxN) then
    n := MaxN;
  if (n > total) then
    n := total;

  if (n <= 1) then // pool has only the caller -> run the whole range directly
  begin
    Pass(0, Hi);
    Exit;
  end;

  per := (total + n - 1) div n;
  started := 0;
  for i := 1 to n - 1 do
  begin
    clo := i * per;
    if (clo > Hi) then
      Break;
    chi := clo + per - 1;
    if (chi > Hi) then
      chi := Hi;

    FWorkers[i - 1].Pass := Pass;
    FWorkers[i - 1].Lo   := clo;
    FWorkers[i - 1].Hi   := chi;
    FWorkers[i - 1].Done.ResetEvent();
    FWorkers[i - 1].Wake.SetEvent();

    Inc(started);
  end;

  chi := per - 1;
  if (chi > Hi) then
    chi := Hi;

  Pass(0, chi); // caller runs chunk 0
  for i := 0 to started - 1 do
    FWorkers[i].Done.WaitFor(INFINITE);
end;

procedure TFFTThreadPool.RunForward(var Eng: TFFT2D);
var
  n: Integer;
begin
  EnsureSetup();
  n := ThreadsForArea(Int64(Eng.W) * Eng.H);
  if FFT_THREADING_DEBUG then
    DebugLn('[FFT Threading]: Using %d/%d threads', [n, FBuiltThreads]);
  Run(Eng.H - 1, @Eng.RowFwd, n);
  Eng.TransposeForward;
  Run(Eng.W - 1, @Eng.ColFwd, n);
end;

procedure TFFTThreadPool.RunInverse(var Eng: TFFT2D);
var
  n: Integer;
begin
  EnsureSetup();
  n := ThreadsForArea(Int64(Eng.W) * Eng.H);
  if FFT_THREADING_DEBUG then
    DebugLn('[FFT Threading]: Using %d/%d threads', [n, FBuiltThreads]);
  Run(Eng.W - 1, @Eng.ColInv, n);
  Eng.TransposeInverse;
  Run(Eng.H - 1, @Eng.RowInv, n);
end;

procedure TFFT2D.EnsureW(len: Integer);
begin
  if (PlanWdim <> len) then
  begin
    InitFFT(PlanW, len);
    PlanWdim := len;
  end;
end;

procedure TFFT2D.EnsureH(len: Integer);
begin
  if (PlanHdim <> len) then
  begin
    InitFFT(PlanH, len);
    PlanHdim := len;
  end;
end;

procedure TFFT2D.Prepare(AW, AH: Integer);
var
  need, want: Integer;
begin
  W := AW;
  H := AH;
  need := AW * AH;
  if (Length(Work) < need) then
  begin
    want := need + need div 10; // over allocate a tad
    SetLength(Work, want);
    SetLength(Spec, want);
  end;
end;

procedure TFFT2D.RowFwd(const Lo, Hi: Integer);
var
  y: Integer;
begin
  FFTEngine.EnsureW(Self.W);
  for y := Lo to Hi do
    cfftf(Self.W, PSingle(@Self.Work[y * Self.W]), AlignPlan(FFTEngine.PlanW));
end;

procedure TFFT2D.ColFwd(const Lo, Hi: Integer);
var
  y: Integer;
begin
  FFTEngine.EnsureH(Self.H);
  for y := Lo to Hi do
    cfftf(Self.H, PSingle(@Self.Spec[y * Self.H]), AlignPlan(FFTEngine.PlanH));
end;

procedure TFFT2D.ColInv(const Lo, Hi: Integer);
var
  y, i: Integer;
  pr: PSingle;
  f: Single;
begin
  FFTEngine.EnsureH(Self.H);
  f := 1.0 / Self.H;
  for y := Lo to Hi do
  begin
    pr := PSingle(@Self.Work[y * Self.H]);
    cfftb(Self.H, pr, AlignPlan(FFTEngine.PlanH));
    for i := 0 to 2 * Self.H - 1 do
      pr[i] *= f;
  end;
end;

procedure TFFT2D.RowInv(const Lo, Hi: Integer);
var
  y, i: Integer;
  pr: PSingle;
  f: Single;
begin
  FFTEngine.EnsureW(Self.W);
  f := 1.0 / Self.W;
  for y := Lo to Hi do
  begin
    pr := PSingle(@Self.Spec[y * Self.W]);
    cfftb(Self.W, pr, AlignPlan(FFTEngine.PlanW));
    for i := 0 to 2 * Self.W - 1 do
      pr[i] *= f;
  end;
end;

procedure TFFT2D.TransposeForward;
begin
  TransposeComplexBlocked(PInt64(@Work[0]), PInt64(@Spec[0]), H, W);
end;

procedure TFFT2D.TransposeInverse;
begin
  TransposeComplexBlocked(PInt64(@Work[0]), PInt64(@Spec[0]), W, H);
end;

procedure TFFT2D.RunForward;
begin
  RowFwd(0, H - 1);
  TransposeForward();
  ColFwd(0, W - 1);
end;

procedure TFFT2D.RunInverse;
begin
  ColInv(0, W - 1);
  TransposeInverse();
  RowInv(0, H - 1);
end;

// 2D FFT, leaving the spectrum stored TRANSPOSED (skips the final transpose-back) so
// the FFT2 -> multiply -> IFFT2 chain halves its transposes.
function TFFT2D.FFT2(const m: TComplexMatrix): TComplexMatrix;
var
  n: Integer;
begin
  Result.SetSize(m.Height, m.Width);
  n := m.Width * m.Height;
  if (n = 0) then
    Exit;

  Prepare(m.Width, m.Height);
  Move(m.Data[0], Work[0], n * SizeOf(TComplex));
  if ShouldParallelFFT(Int64(W) * H) and ThreadPool.Acquire() then // big enough + got the workers
    try
      ThreadPool.RunForward(Self);
    finally
      ThreadPool.Leave();
    end
  else
  begin
    if FFT_THREADING_DEBUG and FFT_THREADING then
      if ShouldParallelFFT(Int64(W) * H) then
        DebugLn('[FFT Threading]: Pool not available')
      else
        DebugLn('[FFT Threading]: Area < FFT_THREADING_MIN_AREA');

    Self.RunForward(); // no threading
  end;

  Move(Spec[0], Result.Data[0], n * SizeOf(TComplex));
end;

// Inverse 2D FFT consuming a transposed spectrum and returning the natural result.
function TFFT2D.IFFT2(const m: TComplexMatrix): TComplexMatrix;
var
  n: Integer;
begin
  Result.SetSize(m.Height, m.Width);
  n := m.Width * m.Height;
  if (n = 0) then
    Exit;

  Prepare(m.Height, m.Width);
  Move(m.Data[0], Work[0], n * SizeOf(TComplex));
  if ShouldParallelFFT(Int64(W) * H) and ThreadPool.Acquire() then // big enough + got the workers
    try
      ThreadPool.RunInverse(Self);
    finally
      ThreadPool.Leave();
    end
  else
  begin
    if FFT_THREADING_DEBUG and FFT_THREADING then
      if ShouldParallelFFT(Int64(W) * H) then
        DebugLn('[FFT Threading]: Pool not available')
      else
        DebugLn('[FFT Threading]: Area < FFT_THREADING_MIN_AREA');

    Self.RunInverse(); // no threading
  end;

  Move(Spec[0], Result.Data[0], n * SizeOf(TComplex));
end;

function FFT2(const m: TComplexMatrix): TComplexMatrix;
begin
  Result := FFTEngine.FFT2(m);
end;

function IFFT2(const m: TComplexMatrix): TComplexMatrix;
begin
  Result := FFTEngine.IFFT2(m);
end;

initialization
  if (SimbaCPUInfo.PCoreCount > 0) and (SimbaCPUInfo.PCoreCount < FFT_MAX_THREADS) then
    FFT_MAX_THREADS := SimbaCPUInfo.PCoreCount;
  ThreadPool := TFFTThreadPool.Create();

finalization
  FreeAndNil(ThreadPool);

end.
