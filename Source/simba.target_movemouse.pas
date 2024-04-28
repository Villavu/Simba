{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Move the mouse in a "human like" way.

  Author: BenLand100
    https://github.com/BenLand100/SMART/blob/master/src/EventNazi.java#L201
    https://ben.land/post/2021/04/25/windmouse-human-mouse-movement
}
unit simba.target_movemouse;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.target;

procedure MoveMouseOnTarget(constref Target: TSimbaTarget; Dest: TPoint);

implementation

uses
  Math,
  simba.math, simba.random, simba.nativeinterface;

procedure MoveMouseOnTarget(constref Target: TSimbaTarget; Dest: TPoint);

  procedure Move(const X, Y, Idle: Double);
  var
    P: TPoint;
  begin
    P.X := Round(X);
    P.Y := Round(Y);

    Target.MouseTeleport(P);
    SimbaNativeInterface.PreciseSleep(Round(Idle));
  end;

  procedure DoMovingEvent(var X, Y, DestX, DestY: Double; out Stop: Boolean);
  var
    I: Integer;
  begin
    Stop := False;

    for I := 0 to High(Target.MouseOptions.MovingEvents) do
    begin
      TMouseMovingEvent(Target.MouseOptions.MovingEvents[I])(@Target, X, Y, DestX, DestY, Stop);
      if Stop then
        Exit;
    end;
  end;

  procedure WindMouse(X1, Y1, X2, Y2, Gravity, Wind, MinWait, MaxWait, MaxStep, TargetArea: Double; Timeout: Integer);
  var
    X, Y, DestX, DestY: Double;
    VeloX, VeloY, WindX, WindY, VeloMag, RandomDist, Idle: Double;
    RemainingDist, Acc, Step: Double;
    T: UInt64;
    Stop: Boolean;
  begin
    VeloX := 0; VeloY := 0;
    WindX := 0; WindY := 0;

    X := X1;
    Y := Y1;
    DestX := X2;
    DestY := Y2;

    Acc := Target.MouseOptions.Accuracy + 0.5;
    Step := MaxStep;

    T := GetTickCount64() + Timeout;
    while (T > GetTickCount64()) do
    begin
      DoMovingEvent(X, Y, X2, Y2, Stop);
      if Stop then
        Exit;

      RemainingDist := Hypot(X - X2, Y - Y2);
      if (RemainingDist <= Acc) then
        Break;

      // If destination changed ensure Step is appropriate
      if ((X2 <> DestX) or (Y2 <> DestY)) and (RemainingDist > TargetArea) then
        Step := MaxStep;
      DestX := X2;
      DestY := Y2;

      if (RemainingDist > TargetArea) then
      begin
        Wind := Min(Wind, RemainingDist);
        WindX := WindX / SQRT_3 + (Random(Round(Wind) * 2 + 1) - Wind) / SQRT_5;
        WindY := WindY / SQRT_3 + (Random(Round(Wind) * 2 + 1) - Wind) / SQRT_5;
      end else
      begin
        WindX /= SQRT_3;
        WindY /= SQRT_3;
        if (Step < 3) then
          Step := 3 + (Random() * 3)
        else
          Step /= SQRT_5;
      end;

      VeloX := VeloX + WindX;
      VeloY := VeloY + WindY;
      VeloX := VeloX + Gravity * (X2 - X) / RemainingDist;
      VeloY := VeloY + Gravity * (Y2 - Y) / RemainingDist;

      if (Hypot(VeloX, VeloY) > Step) then
      begin
        RandomDist := Step / 3.0 + (Step / 2 * Random());

        VeloMag := Sqrt(VeloX * VeloX + VeloY * VeloY);
        VeloX := (VeloX / VeloMag) * RandomDist;
        VeloY := (VeloY / VeloMag) * RandomDist;
      end;

      X := X + VeloX;
      Y := Y + VeloY;
      Idle := (MaxWait - MinWait) * (Hypot(VeloX, VeloY) / Step) + MinWait;

      Move(X, Y, Idle);
    end;

    if (GetTickCount64() >= T) then
      SimbaException('MouseMove timed out after %dms. Start: (%d,%d), Dest: (%d,%d)', [Timeout, Round(X1), Round(Y1), Round(X2), Round(Y2)]);
  end;

var
  Speed, Gravity, Wind: Double;
  Timeout: Integer;
  Start: TPoint;
  RandSpeed, Expo: Double;
begin
  Speed   := IfThen(Target.MouseOptions.Speed   = 0, DEFAULT_MOUSE_SPEED,   Target.MouseOptions.Speed);
  Gravity := IfThen(Target.MouseOptions.Gravity = 0, DEFAULT_MOUSE_GRAVITY, Target.MouseOptions.Gravity);
  Wind    := IfThen(Target.MouseOptions.Wind    = 0, DEFAULT_MOUSE_WIND,    Target.MouseOptions.Wind);
  Timeout := IfThen(Target.MouseOptions.Timeout = 0, DEFAULT_MOUSE_TIMEOUT, Target.MouseOptions.Timeout);

  Start := Target.MouseXY;
  Expo := 1 + Power(Hypot(Start.X - Dest.X, Start.Y - Dest.Y), 0.5) / 50; // Further the distance the faster we move

  RandSpeed := RandomLeft(Speed, Speed * 1.5);
  RandSpeed *= Expo;
  RandSpeed /= 10;

  WindMouse(
    Start.X, Start.Y, Dest.X, Dest.Y,
    Gravity, Wind,
    5 / RandSpeed, 10 / RandSpeed, 10 * RandSpeed, 15 * RandSpeed,
    Timeout
  );
end;

end.

