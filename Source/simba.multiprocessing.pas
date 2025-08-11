{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.multiprocessing;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.threading;

type
  TSimbaMultiprocessingStrategy = record
    TemplateFinder: record
      Enabled: Boolean;
      SliceWidth: Integer;
      SliceHeight: Integer;
    end;
    ColorFinder: record
      Enabled: Boolean;
      SliceWidth: Integer;
      SliceHeight: Integer;
    end;
    ImageFinder: record
      Enabled: Boolean;
      SliceWidth: Integer;
      SliceHeight: Integer;
    end;

    function SlicesFor(Enabled: Boolean; SliceWidth, SliceHeight, SearchWidth, SearchHeight: Integer): Integer;
    function SlicesForTemplateFinder(SearchWidth, SearchHeight: Integer): Integer;
    function SlicesForColorFinder(SearchWidth, SearchHeight: Integer): Integer;
    function SlicesForImageFinder(SearchWidth, SearchHeight: Integer): Integer;

    class function Create: TSimbaMultiprocessingStrategy; static;
  end;

  TSimbaMultiprocessingMethod = procedure(const Index, Lo, Hi: Integer) is nested;
  TSimbaMultiprocessing = class
  protected
  type
    TPoolThread = class(TThread)
    protected
      procedure Execute; override;
    public
      IdleLock: TWaitableLock;   // Locked = thread is being used right now.
      MethodLock: TWaitableLock; // Locked = idle, waiting for method to call

      Index: Integer;
      Lo: Integer;
      Hi: Integer;

      Method: TSimbaMultiprocessingMethod;

      constructor Create; reintroduce;
      destructor Destroy; override;
    end;
    TPoolThreadArray = array of TPoolThread;
  protected
    FThreadCount: Integer;
    FThreads: TPoolThreadArray;
    FLock: TEnterableLock;

    function GetIdleThreads(MaxThreads: Integer): TPoolThreadArray;
  public
    constructor Create(AThreadCount: Integer);
    destructor Destroy; override;

    property ThreadCount: Integer read FThreadCount;

    function Run(MaxThreads: Integer; Lo, Hi: Integer; Method: TSimbaMultiprocessingMethod): Integer;
  end;

var
  SimbaMultiprocessingStrategy: TSimbaMultiprocessingStrategy;
  SimbaMultiprocessing: TSimbaMultiprocessing;

implementation

uses
  simba.initializations;

function TSimbaMultiprocessingStrategy.SlicesFor(Enabled: Boolean; SliceWidth, SliceHeight, SearchWidth, SearchHeight: Integer): Integer;
var
  I: Integer;
begin
  Result := 1;

  if Enabled and (SearchWidth >= SliceWidth) and (SearchHeight >= SliceHeight) then // not worth
  begin
    for I := SimbaMultiprocessing.ThreadCount - 1 downto 2 do
      if (SearchHeight div I) > SliceHeight then // Each slice is at least `SliceHeight` pixels
        Exit(I);
  end;
  // not possible to slice into at least `SliceHeight` pixels so 1 thread it is
end;

function TSimbaMultiprocessingStrategy.SlicesForTemplateFinder(SearchWidth, SearchHeight: Integer): Integer;
begin
  with TemplateFinder do
    Result := SlicesFor(Enabled, SliceWidth, SliceHeight, SearchWidth, SearchHeight);
end;

function TSimbaMultiprocessingStrategy.SlicesForColorFinder(SearchWidth, SearchHeight: Integer): Integer;
begin
  with ColorFinder do
    Result := SlicesFor(Enabled, SliceWidth, SliceHeight, SearchWidth, SearchHeight);
end;

function TSimbaMultiprocessingStrategy.SlicesForImageFinder(SearchWidth, SearchHeight: Integer): Integer;
begin
  with ImageFinder do
    Result := SlicesFor(Enabled, SliceWidth, SliceHeight, SearchWidth, SearchHeight);
end;

class function TSimbaMultiprocessingStrategy.Create: TSimbaMultiprocessingStrategy;
begin
  Result := Default(TSimbaMultiprocessingStrategy);

  Result.TemplateFinder.Enabled := True;
  Result.TemplateFinder.SliceWidth := 250;
  Result.TemplateFinder.SliceHeight := 250;

  Result.ColorFinder.Enabled := True;
  Result.ColorFinder.SliceWidth := 250;
  Result.ColorFinder.SliceHeight := 250;

  Result.ImageFinder.Enabled := True;
  Result.ImageFinder.SliceWidth := 250;
  Result.ImageFinder.SliceHeight := 250;
end;

function TSimbaMultiprocessing.GetIdleThreads(MaxThreads: Integer): TPoolThreadArray;
var
  I, Count: Integer;
begin
  SetLength(Result, MaxThreads);
  Count := 0;

  FLock.Enter();
  try
    for I := 0 to High(FThreads) do
    begin
      if FThreads[I].IdleLock.IsLocked() then
        Continue;

      Result[Count] := FThreads[I];
      Result[Count].IdleLock.Lock();
      Inc(Count);

      if (Count = MaxThreads) then
        Break;
    end;
  finally
    FLock.Leave();
  end;

  SetLength(Result, Count);
end;

constructor TSimbaMultiprocessing.Create(AThreadCount: Integer);
var
  I: Integer;
begin
  inherited Create();

  FThreadCount := AThreadCount;
  SetLength(FThreads, FThreadCount);
  for I := 0 to High(FThreads) do
    FThreads[I] := TPoolThread.Create();
end;

destructor TSimbaMultiprocessing.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FThreads) do
    FThreads[I].Free();
  FThreads := nil;

  inherited Destroy();
end;

function TSimbaMultiprocessing.Run(MaxThreads: Integer; Lo, Hi: Integer; Method: TSimbaMultiprocessingMethod): Integer;
var
  Threads: TPoolThreadArray;
  I, Size: Integer;
begin
  if (MaxThreads > 1) then
    Threads := GetIdleThreads(Min(FThreadCount, MaxThreads))
  else
    Threads := [];

  Result := Max(1, Length(Threads));

  if (Length(Threads) > 1) then
  begin
    Size := ((Hi - Lo) + 1) div Result;

    for I := 0 to High(Threads) do
    begin
      Threads[I].Index := I;
      Threads[I].Method := Method;

      if (I = 0) then
      begin
        Threads[I].Lo := 0;
        Threads[I].Hi := Size;
      end else
      begin
        Threads[I].Lo := Threads[I-1].Hi + 1;
        Threads[I].Hi := Threads[I-1].Hi + Size;
      end;

      if (I = High(Threads)) then
        Threads[I].Hi := Hi;

      Threads[I].MethodLock.Unlock();
      if Threads[I].Suspended then
        Threads[I].Start();
    end;

    for I := 0 to High(Threads) do
      Threads[I].IdleLock.WaitLocked();
  end else
    Method(0, Lo, Hi);
end;

procedure TSimbaMultiprocessing.TPoolThread.Execute;
begin
  while True do
  begin
    MethodLock.WaitLocked();
    if Terminated then
      Break;

    if Assigned(Method) then
    try
      Method(Index, Lo, Hi);
    except
      on E: Exception do
      begin
        DebugLn('[SimbaMultiprocessing]: Exception occurred while executing a method: ' + E.Message);
        {$IFDEF SIMBA_HAS_DEBUGINFO}
        DumpExceptionBacktrace(Output);
        {$ENDIF}
      end;
    end;

    Method := nil;
    MethodLock.Lock();
    IdleLock.Unlock();
  end;
end;

constructor TSimbaMultiprocessing.TPoolThread.Create;
begin
  inherited Create(True, 512 * 512); // default = 4MiB, we set 256KiB
                                     // also start suspended until we need it.
  MethodLock.Lock();
end;

destructor TSimbaMultiprocessing.TPoolThread.Destroy;
begin
  IdleLock.WaitLocked(); // Wait if running something
  MethodLock.Unlock();   // Wake `Execute` loop if not running

  if (not Suspended) then
  begin
    Terminate();
    WaitFor();
  end;

  inherited Destroy();
end;

procedure DoCreate;
begin
  SimbaMultiprocessingStrategy := TSimbaMultiprocessingStrategy.Create();
  SimbaMultiprocessing := TSimbaMultiprocessing.Create(SimbaCPUInfo.CoreCount);
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaMultiprocessing);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.CREATE, @DoCreate, 'SimbaMultiprocessing');
  SimbaInitialization_Add(ESimbaInit.DESTROY, @DoDestroy, 'SimbaMultiprocessing');

end.

