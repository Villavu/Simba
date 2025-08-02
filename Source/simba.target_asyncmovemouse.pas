{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.target_asyncmovemouse;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.baseclass,
  simba.threading,
  simba.target;

type
  PASyncMouse = ^TASyncMouse;
  TASyncMouse = class(TSimbaBaseClass)
  protected
    FThread: TIdleThread;
    FTarget: TSimbaTarget;
    FDest: TPoint;
    FStop: Boolean;
    FAccuracy: Single;

    procedure DoMoving(var X, Y, DestX, DestY: Double; out Stop: Boolean);
    procedure Execute;
    function GetIsMoving: Boolean;
    function GetDestination: TPoint;
    procedure SetDestination(Value: TPoint);
  public
    constructor Create(Target: TSimbaTarget);
    destructor Destroy; override;

    procedure Move(Dest: TPoint; Accuracy: Single = 0.5);
    procedure Stop;
    function Wait(Timeout: Integer = -1): Boolean;

    property IsMoving: Boolean read GetIsMoving;
    property Destination: TPoint read GetDestination write SetDestination;
  end;

implementation

uses
  Math,
  simba.target_movemouse;

procedure TASyncMouse.DoMoving(var X, Y, DestX, DestY: Double; out Stop: Boolean);
begin
  DestX := FDest.X;
  DestY := FDest.Y;
  if (Hypot(X - DestX, Y - DestY) <= FAccuracy) then
    FStop := True;
  Stop := FStop;
end;

procedure TASyncMouse.Execute;
begin
  MoveMouseOnTarget(FTarget, FDest, @DoMoving);
end;

function TASyncMouse.GetIsMoving: Boolean;
begin
  Result := not FThread.IsIdle;
end;

function TASyncMouse.GetDestination: TPoint;
begin
  Result := FDest;
end;

procedure TASyncMouse.SetDestination(Value: TPoint);
begin
  FDest := Value;
end;

constructor TASyncMouse.Create(Target: TSimbaTarget);
begin
  inherited Create();

  FTarget := Target;
  FThread := TIdleThread.Create(@Execute);
end;

destructor TASyncMouse.Destroy;
begin
  FThread.Free(); // todo

  inherited Destroy();
end;

function TASyncMouse.Wait(Timeout: Integer = -1): Boolean;
var
  T: UInt64;
begin
  Result := False;

  if (Timeout > -1) then
  begin
    T := GetTickCount64() + Timeout;
    while (T > GetTickCount64()) do
    begin
      if FThread.IsIdle then
        Exit(True);
      Sleep(15);
    end;
  end else
  begin
    while (not FThread.IsIdle) do
      Sleep(15);
    Result := True;
  end;
end;

procedure TASyncMouse.Stop;
begin
  FStop := True;

  Wait();
end;

procedure TASyncMouse.Move(Dest: TPoint; Accuracy: Single);
begin
  FStop := False;
  FDest := Dest;
  FAccuracy := Max(0.5, Accuracy);

  FThread.Wake();
end;

end.

