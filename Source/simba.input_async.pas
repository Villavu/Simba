{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.input_async;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.threading, simba.target;

type
  TSimbaASyncMouseThread = class(TThread)
  protected
    FLock: TWaitableLock;
    FMethod: TThreadMethod;

    procedure Execute; override;
  public
    constructor Create(Method: TThreadMethod); reintroduce;

    function IsActive: Boolean;
    procedure Wake;
  end;

  TSimbaASyncMouse = class(TObject)
  protected
    FThread: TSimbaASyncMouseThread;
    FTarget: TSimbaTarget;
    FDest: TPoint;
    FStop: Boolean;
    FAccuracy: Single;

    procedure DoMouseMoving(var X, Y, DestX, DestY: Double; out Stop: Boolean);
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ChangeDest(Dest: TPoint);
    function IsMoving: Boolean;
    procedure WaitMoving;
    procedure Stop;

    procedure Move(Target: TSimbaTarget; Dest: TPoint; Accuracy: Single = 0.5);
  end;

var
  ASyncMouse: TSimbaASyncMouse;

implementation

uses
  Math,
  simba.target_movemouse;

procedure TSimbaASyncMouseThread.Execute;
begin
  while (not Terminated) do
  begin
    if FLock.WaitLocked(1000) then
    try
      FMethod();
    finally
      FLock.Lock();
    end;
  end;
end;

constructor TSimbaASyncMouseThread.Create(Method: TThreadMethod);
begin
  inherited Create(True, 512*512);

  FMethod := Method;
  FLock.Lock();
end;

function TSimbaASyncMouseThread.IsActive: Boolean;
begin
  Result := not FLock.IsLocked;
end;

procedure TSimbaASyncMouseThread.Wake;
begin
  if Suspended then
    Start();

  FLock.Unlock();
end;

procedure TSimbaASyncMouse.DoMouseMoving(var X, Y, DestX, DestY: Double; out Stop: Boolean);
begin
  DestX := FDest.X;
  DestY := FDest.Y;
  if (Hypot(X - DestX, Y - DestY) <= FAccuracy) then
    FStop := True;
  Stop  := FStop;
end;

procedure TSimbaASyncMouse.Execute;
begin
  MoveMouseOnTarget(FTarget, FDest, @DoMouseMoving);
end;

constructor TSimbaASyncMouse.Create;
begin
  inherited Create();

  FThread := TSimbaASyncMouseThread.Create(@Execute);
end;

destructor TSimbaASyncMouse.Destroy;
begin
  if not FThread.Suspended then
  begin
    FThread.Terminate();
    Stop();
    FThread.WaitFor();
  end;
  FThread.Free();

  inherited Destroy();
end;

procedure TSimbaASyncMouse.ChangeDest(Dest: TPoint);
begin
  FDest := Dest;
end;

function TSimbaASyncMouse.IsMoving: Boolean;
begin
  Result := FThread.IsActive();
end;

procedure TSimbaASyncMouse.WaitMoving;
begin
  while FThread.IsActive() do
    Sleep(15);
end;

procedure TSimbaASyncMouse.Move(Target: TSimbaTarget; Dest: TPoint; Accuracy: Single);
begin
  FTarget := Target;
  FStop := False;
  FDest := Dest;
  FAccuracy := Max(0.5, Accuracy);

  FThread.Wake();
end;

procedure TSimbaASyncMouse.Stop;
begin
  FStop := True;

  WaitMoving();
end;

initialization
  ASyncMouse := TSimbaASyncMouse.Create();

finalization
  FreeAndNil(ASyncMouse);

end.

