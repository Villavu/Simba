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
    procedure WaitLocked;

    function IsLocked: Boolean;

    class operator Initialize(var Self: TSimpleWaitableLock);
    class operator Finalize(var Self: TSimpleWaitableLock);
  end;

implementation

procedure TSimpleLock.IncLock;
begin
  InterlockedIncrement(FLock)
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

