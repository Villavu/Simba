{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple managed locking types.
}
unit simba.simplelock;

{$i simba.inc}

interface

uses
  classes, sysutils,
  syncobjs;

type
  TSimpleLock = record
  private
    FLock: Integer;
  public
    procedure IncLock;
    procedure DecLock;

    function IsLocked: Boolean;

    class operator Initialize(var Self: TSimpleLock);
  end;

  TSimpleWaitableLock = record
  private
    FLock: TSimpleEvent;
  public
    procedure Lock;
    procedure Unlock;
    procedure WaitLocked; overload;
    function WaitLocked(Timeout: Integer): Boolean; overload; // Returns True if unlocked!

    function IsLocked: Boolean;

    class operator Initialize(var Self: TSimpleWaitableLock);
    class operator Finalize(var Self: TSimpleWaitableLock);
  end;

  TSimpleEnterableLock = record
  private
    FLock: TCriticalSection;
  public
    procedure Enter;
    procedure Leave;

    class operator Initialize(var Self: TSimpleEnterableLock);
    class operator Finalize(var Self: TSimpleEnterableLock);
  end;

  TSimpleThreadsafeLimit = record
  private
    FCounter: Integer;
    FLimit: Integer;
  public
    procedure Inc; inline;
    function Reached: Boolean; inline;

    class function Create(Limit: Integer): TSimpleThreadsafeLimit; static;
  end;

implementation

procedure TSimpleThreadsafeLimit.Inc;
begin
  if (FLimit > 0) then
    InterlockedIncrement(FCounter);
end;

function TSimpleThreadsafeLimit.Reached: Boolean;
begin
  Result := (FLimit > 0) and (InterlockedCompareExchange(FCounter, FLimit, FLimit) >= FLimit);
end;

class function TSimpleThreadsafeLimit.Create(Limit: Integer): TSimpleThreadsafeLimit;
begin
  Result.FCounter := 0;
  Result.FLimit   := Limit;
end;

procedure TSimpleEnterableLock.Enter;
begin
  FLock.Enter();
end;

procedure TSimpleEnterableLock.Leave;
begin
  FLock.Leave();
end;

class operator TSimpleEnterableLock.Initialize(var Self: TSimpleEnterableLock);
begin
  Self.FLock := TCriticalSection.Create();
end;

class operator TSimpleEnterableLock.Finalize(var Self: TSimpleEnterableLock);
begin
  if (Self.FLock <> nil) then
    FreeAndNil(Self.FLock);
end;

procedure TSimpleLock.IncLock;
begin
  InterlockedIncrement(FLock);
end;

procedure TSimpleLock.DecLock;
begin
  InterlockedDecrement(FLock);
end;

function TSimpleLock.IsLocked: Boolean;
begin
  Result := InterlockedCompareExchange(FLock, 0, 0) > 0;
end;

class operator TSimpleLock.Initialize(var Self: TSimpleLock);
begin
  Self.FLock := 0;
end;

procedure TSimpleWaitableLock.Lock;
begin
  FLock.ResetEvent();
end;

procedure TSimpleWaitableLock.Unlock;
begin
  FLock.SetEvent();
end;

procedure TSimpleWaitableLock.WaitLocked;
begin
  FLock.WaitFor(INFINITE);
end;

function TSimpleWaitableLock.WaitLocked(Timeout: Integer): Boolean;
begin
  Result := FLock.WaitFor(Timeout) = wrSignaled;
end;

function TSimpleWaitableLock.IsLocked: Boolean;
begin
  Result := FLock.WaitFor(0) = wrTimeout;
end;

class operator TSimpleWaitableLock.Initialize(var Self: TSimpleWaitableLock);
begin
  Self.FLock := TSimpleEvent.Create();
  Self.FLock.SetEvent();
end;

class operator TSimpleWaitableLock.Finalize(var Self: TSimpleWaitableLock);
begin
  FreeAndNil(Self.FLock);
end;

end.

