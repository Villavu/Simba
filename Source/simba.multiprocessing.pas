{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.multiprocessing;

{$i simba.inc}

interface

uses
  Classes, SysUtils, syncobjs,
  simba.base,
  simba.threading;

const
  FINDER_THREADING: Boolean = True;           // runtime on/off switch
  FINDER_THREADING_DEBUG: Boolean = False;    // live debug on threading status
  FINDER_THREADING_MIN_AREA: Integer = 40000; // W*H below this always runs single-threaded (200x200)
  FINDER_THREADING_MIN_ROWS: Integer = 8;     // the search is split into horizontal bands, so each band needs >= this many rows to be worth a thread; keeps a wide-short target (e.g. 5000x10) single-threaded regardless of area
  FINDER_MAX_THREADS: Integer = 6;            // max threads to use; users may raise it

type
  TSimbaMultiprocessingMethod = procedure(const Index, Lo, Hi: Integer) is nested;
  TSimbaMultiprocessing = class
  protected
  type
    TPoolWorker = class(TThread)
    protected
      procedure Execute; override;
    public
      Wake, Done: TSimpleEvent;
      Busy: Boolean; // claim state only read/written under the pools FLock so is guarded

      Method: TSimbaMultiprocessingMethod;
      Index, Lo, Hi: Integer;

      constructor Create; reintroduce;
      destructor Destroy; override;
    end;
    TPoolWorkerArray = array of TPoolWorker;
  protected
    FWorkers: TPoolWorkerArray;
    FLock: TCriticalSection; // guards EnsureSetup + the per-worker Busy claim flags
    FBuiltThreads: Integer;  // total slices the pool is built for (workers + the caller); tracks FINDER_MAX_THREADS, grow-only

    function ClaimWorkers(Max: Integer): TPoolWorkerArray; // returns up to `Max` idle workers
    procedure EnsureSetup;                                 // build/grow the worker pool to WantThreads() (mirrors the FFT TFFTThreadPool)
  public
    constructor Create();
    destructor Destroy; override;

    function WantThreads: Integer; // FINDER_MAX_THREADS but clamped to threadcount
    function ThreadsForArea(Width, Height: Integer): Integer;
    function Run(MaxThreads: Integer; Lo, Hi: Integer; Method: TSimbaMultiprocessingMethod): Integer;
  end;

var
  SimbaMultiprocessing: TSimbaMultiprocessing;

implementation

procedure InvokeSlice(const Method: TSimbaMultiprocessingMethod; const Index, Lo, Hi: Integer);
begin
  try
    Method(Index, Lo, Hi);
  except
    on E: Exception do
    begin
      DebugLn('[Finder Threading]: slice %d (rows %d..%d) exception: %s', [Index, Lo, Hi, E.Message]);
      {$IFDEF SIMBA_HAS_DEBUGINFO}
      DumpExceptionBacktrace(Output);
      {$ENDIF}
    end;
  end;
end;

function TSimbaMultiprocessing.ClaimWorkers(Max: Integer): TPoolWorkerArray;
var
  I, Count: Integer;
begin
  Result := nil;
  if (Max < 1) then
    Exit;

  SetLength(Result, Max);
  Count := 0;

  FLock.Enter();
  try
    for I := 0 to High(FWorkers) do
      if not FWorkers[I].Busy then
      begin
        FWorkers[I].Busy := True;
        Result[Count] := FWorkers[I];
        Inc(Count);
        if (Count = Max) then
          Break;
      end;
  finally
    FLock.Leave();
  end;

  SetLength(Result, Count);
end;

constructor TSimbaMultiprocessing.Create;
begin
  inherited Create();

  FLock := TCriticalSection.Create();
end;

destructor TSimbaMultiprocessing.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FWorkers) do
    if (FWorkers[I] <> nil) then // only the workers we actually spun up
      FWorkers[I].Free();
  FWorkers := nil;
  FLock.Free();

  inherited Destroy();
end;

function TSimbaMultiprocessing.WantThreads: Integer;
begin
  Result := FINDER_MAX_THREADS;                 // can be raised above the P-core count
  if (Result > SimbaCPUInfo.ThreadCount) then   // but never spawn more workers than logical CPUs
    Result := SimbaCPUInfo.ThreadCount;
  if (Result < 1) then
    Result := 1;
end;

function TSimbaMultiprocessing.ThreadsForArea(Width, Height: Integer): Integer;
var
  Area: Int64;
  Cap, ByArea, ByRows: Integer;
begin
  if not FINDER_THREADING then
  begin
    if FINDER_THREADING_DEBUG then
      DebugLn('[Finder Threading]: FINDER_THREADING is disabled');
    Exit(1);
  end;

  Cap  := WantThreads();
  Area := Int64(Width) * Height;

  if (FINDER_THREADING_MIN_AREA < 1) then   // area gate disabled -> whole pool
    ByArea := Cap
  else
    ByArea := Area div FINDER_THREADING_MIN_AREA;

  if (FINDER_THREADING_MIN_ROWS > 0) then   // horizontal-band split -> bounded by the row count
    ByRows := Height div FINDER_THREADING_MIN_ROWS
  else
    ByRows := Cap;

  Result := ByArea;
  if (Result > ByRows) then Result := ByRows;
  if (Result > Cap)    then Result := Cap;
  if (Result < 1)      then Result := 1;

  if FINDER_THREADING_DEBUG then
    DebugLn('[Finder Threading]: %dx%d area=%d -> %d threads (byArea=%d, byRows=%d, cap=%d)', [Width, Height, Area, Result, ByArea, ByRows, Cap]);
end;

procedure TSimbaMultiprocessing.EnsureSetup;
var
  I, Want: Integer;
begin
  Want := WantThreads();
  if (FBuiltThreads >= Want) then // already big enough -> no lock
    Exit;

  FLock.Enter();
  try
    Want := WantThreads();
    if (FBuiltThreads >= Want) then
      Exit;

    SetLength(FWorkers, Want - 1);   // caller is the +1
    for I := 0 to High(FWorkers) do
      if (FWorkers[I] = nil) then    // only spin up the newly-added slots
        FWorkers[I] := TPoolWorker.Create();
    FBuiltThreads := Want;

    if FINDER_THREADING_DEBUG then
      DebugLn('[Finder Threading]: Setup pool for %d threads (%d workers + caller)', [FBuiltThreads, Length(FWorkers)]);
  finally
    FLock.Leave();
  end;
end;

function TSimbaMultiprocessing.Run(MaxThreads: Integer; Lo, Hi: Integer; Method: TSimbaMultiprocessingMethod): Integer;
var
  Workers: TPoolWorkerArray;
  Slices, Total, Per, Rem, I, Cnt, Cursor, CallerLo, CallerHi: Integer;
begin
  Total := (Hi - Lo) + 1;

  EnsureSetup(); // build/grow the pool for a (possibly raised) FINDER_MAX_THREADS

  // Calling thread runs one slice so we only need to borrow `Slices - 1`
  Slices := Min(FBuiltThreads, MaxThreads);
  if (Slices > Total) then Slices := Total;
  if (Slices < 1)     then Slices := 1;

  if (Slices > 1) then
    Workers := ClaimWorkers(Slices - 1) // may hand back fewer if the pool is busy
  else
    Workers := [];

  Slices := Length(Workers) + 1;
  Result := Slices;

  if FINDER_THREADING_DEBUG then
    DebugLn('[Finder Threading]: Using %d/%d threads (rows %d..%d, requested %d)', [Result, FBuiltThreads, Lo, Hi, MaxThreads]);

  Per := Total div Slices;
  Rem := Total mod Slices;

  CallerLo := Lo;
  CallerHi := Lo - 1;
  Cursor   := Lo;
  for I := 0 to Slices - 1 do
  begin
    Cnt := Per;
    if (I < Rem) then Inc(Cnt);             // spread the remainder over the first slices

    if (I = 0) then
    begin
      CallerLo := Cursor;                   // slice 0 -> the caller
      CallerHi := Cursor + Cnt - 1;
    end else
    begin
      Workers[I-1].Index  := I;
      Workers[I-1].Lo     := Cursor;
      Workers[I-1].Hi     := Cursor + Cnt - 1;
      Workers[I-1].Method := Method;
      Workers[I-1].Done.ResetEvent();
      Workers[I-1].Wake.SetEvent();         // hand off the slice + wake the worker
    end;

    Inc(Cursor, Cnt);
  end;

  InvokeSlice(Method, 0, CallerLo, CallerHi); // caller talks part with a slice
  for I := 0 to High(Workers) do
    Workers[I].Done.WaitFor(INFINITE); // wait for all workers

  if (Length(Workers) > 0) then // unlock workers
  begin
    FLock.Enter();
    try
      for I := 0 to High(Workers) do
        Workers[I].Busy := False;
    finally
      FLock.Leave();
    end;
  end;
end;

procedure TSimbaMultiprocessing.TPoolWorker.Execute;
begin
  SetThreadPCore(); // try pin to pcore if possible

  while (not Terminated) do
  begin
    Wake.WaitFor(INFINITE);
    if Terminated then
      Break;
    Wake.ResetEvent();

    InvokeSlice(Method, Index, Lo, Hi);

    Done.SetEvent();
  end;
end;

constructor TSimbaMultiprocessing.TPoolWorker.Create;
begin
  Wake := TSimpleEvent.Create();
  Done := TSimpleEvent.Create();

  inherited Create(False, 512 * 512);
end;

destructor TSimbaMultiprocessing.TPoolWorker.Destroy;
begin
  Terminate();         // set the flag first...
  Wake.SetEvent();     // ...then wake, so Execute is guaranteed to see it and break
  inherited Destroy(); // WaitFor
  Wake.Free();
  Done.Free();
end;

initialization
  if (SimbaCPUInfo.PCoreCount > 0) and (SimbaCPUInfo.PCoreCount < FINDER_MAX_THREADS) then
    FINDER_MAX_THREADS := SimbaCPUInfo.PCoreCount;
  SimbaMultiprocessing := TSimbaMultiprocessing.Create();

finalization
  FreeAndNil(SimbaMultiprocessing);

end.

