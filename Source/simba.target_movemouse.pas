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

type
  TMoveMouseEvent = procedure(var X, Y, DestX, DestY: Double; out Stop: Boolean) of object;

procedure MoveMouseOnTarget(Target: TSimbaTarget; Dest: TPoint; MouseMoveEvent: TMoveMouseEvent = nil);

implementation

uses
  Math,
  simba.nativeinterface;

const
  HESITATE_CHANCE = 0.03; // ~3% long moves pause mid-way (further scaled down by distance) < 120px will never fire.
  GUST_CHANCE     = 0.05; // ~5% moves get a stronger "wind gust"
  SUBSTEP_CHANCE  = 0.02; // ~2% steps add an extra mid-step

type
  // WindMouse by BenLand100 but enhanced by AI.
  // Speed   = pace
  // Wind    = wander (0 = straight)
  // Gravity = pull to line
  TWindMouse = record
    Target: TSimbaTarget;

    // move inputs (constant for the move; read across Plan/Step)
    DestX, DestY, Speed, Wind, Gravity: Double;

    // live cursor position, velocity and progress
    X, Y, MoveX, MoveY, TotalDist, RemainingDist, Progress: Double;

    // per-move plan (rebuilt by Plan)
    MaxStep, EndStep, Gap: Double;
    DecelRate, Inertia, LaunchScale: Double;
    ArcScale, ArcAmp, ArcDecay, ArcFreq, ArcSkew: Double;
    HesitateAt: Double;          // >1 = none this move, else the trigger progress
    Coasting, Overshot: Boolean; // will overshoot past the dest / is correcting back

    class function RandFloat(const Lo, Hi: Double): Double; static;
    class function RandSkew(const Near, Far: Double): Double; static; // biased toward Near
    class function SmoothStep(const T: Double): Double; static;       // Hermite, t in [0, 1]
    class function Clamp(const V, Lo, Hi: Double): Double; static;

    procedure Teleport(AX, AY: Double; Sleep: Double = -1); // Sleep < 0 = pace by Gap
    procedure Flick(Scale: Double = 1.0); // Scale > 1 = a larger kick
    procedure Plan(const FromX, FromY: Double);
    function HesitationStep: Boolean;
    procedure Step;
    procedure Run(ATarget: TSimbaTarget; ADest: TPoint; OnMove: TMoveMouseEvent);
  end;

class function TWindMouse.RandFloat(const Lo, Hi: Double): Double;
begin
  Result := Lo + Random() * (Hi - Lo);
end;

class function TWindMouse.RandSkew(const Near, Far: Double): Double;
begin
  Result := Near + (Far - Near) * Random() * Random();
end;

class function TWindMouse.SmoothStep(const T: Double): Double;
begin
  Result := T * T * (3 - 2 * T);
end;

class function TWindMouse.Clamp(const V, Lo, Hi: Double): Double;
begin
  Result := Min(Hi, Max(Lo, V));
end;

procedure TWindMouse.Teleport(AX, AY: Double; Sleep: Double);
var
  P: TPoint;
begin
  P.X := Round(AX);
  P.Y := Round(AY);
  Target.MouseTeleport(P);

  if (Sleep < 0) then
  begin
    Sleep := Gap * RandFloat(0.925, 1.075);
    if (Random() < 0.015) then
      Sleep := Sleep * RandFloat(1.5, 2.5); // ~1.5% randomness
  end;

  SimbaNativeInterface.PreciseSleep(Sleep);
end;

// Nudge the motion in a random direction (reabsorbed by inertia)
procedure TWindMouse.Flick(Scale: Double);
var
  Angle, Mag: Double;
begin
  Mag := Min(Sqr(Random()) * 6.5 * ArcScale, MaxStep) * Scale;
  Angle := Random() * 2 * PI;
  MoveX := MoveX + Cos(Angle) * Mag;
  MoveY := MoveY + Sin(Angle) * Mag;
end;

// (Re)build the per-move plan:
// Either runs at the start and again if the dest jumps mid-move.
procedure TWindMouse.Plan(const FromX, FromY: Double);
var
  MoveTime: Double;
  N: Integer;
begin
  TotalDist := Max(1.0, Hypot(FromX - DestX, FromY - DestY));

  // Total move time = reaction floor + sqrt-distance travel, speed-scaled, +/-22%
  MoveTime := (100.0 + 14.0 * Sqrt(TotalDist) / (Speed / 10.0)) * RandFloat(0.78, 1.22);
  Gap := RandFloat(6.0, 9.0);
  N := Max(3, Round(MoveTime / Gap));
  MaxStep := Min((TotalDist / N) / 0.80, TotalDist * 0.5);

  // Wind off the line vs gravity back
  ArcScale := Clamp(2.8 * Wind / Gravity, 0.4, 2.5);
  if (Random() < GUST_CHANCE) then // gust: wanders harder
    ArcScale := ArcScale * (1.4 + Random() * 0.5);

  LaunchScale := RandFloat(0.15, 0.30);
  DecelRate := RandFloat(0.42, 0.65);   // arrival slowdown speed
  if (Random() < 0.22) then             // but 22% of moves get an much harder stop
    DecelRate := RandFloat(0.7, 0.88);
  Inertia := 0.31 + 0.17 * Sqr(Random());
  EndStep := Min(2.0 + Sqr(Random()) * 2.5, MaxStep);

  // Hesitation chance scaled up by distance (but none below ~120px)
  HesitateAt := 2.0;
  if (Random() < HESITATE_CHANCE * Clamp((TotalDist - 120) / 130, 0, 1)) then
    HesitateAt := RandFloat(0.20, 0.75);

  Coasting := (Random() < 0.06) and (TotalDist > 3.0);
  Overshot := False;

  // Arc: sideways offset, decay, skew (crest position), then pick a side
  ArcAmp   := Min((0.030 + Sqr(Random()) * 0.130) * ArcScale, 0.45) * TotalDist;
  ArcDecay := 1.45 * Clamp(Gravity / 12.0, 0.3, 3.0);
  ArcSkew  := 0.50 + Sqr(Random()) * 0.58;
  if (Random() >= 0.5) then
    ArcAmp := -ArcAmp;

  // Arc shape: mostly a single bow held to one side
  if (Random() < 0.80) then
    ArcFreq := 1.0
  else
  begin
    ArcFreq := 2.0;
    ArcAmp  := ArcAmp * 0.6; // gentle S: crosses the line but only a small distance each side
  end;
end;

// Mid-move hesitation: brake to rest, sleep then later re-accelerate.
// True = handled this step, do not do a normal step.
function TWindMouse.HesitationStep: Boolean;
begin
  Result := (HesitateAt <= 1.0) and (Progress >= HesitateAt);
  if (not Result) then
    Exit;

  // brake toward rest: velocity decays by (1 - Inertia) each step until it stalls and the pause fires
  MoveX := MoveX * (1 - Inertia);
  MoveY := MoveY * (1 - Inertia);
  X := X + MoveX;
  Y := Y + MoveY;
  if (Hypot(MoveX, MoveY) < 0.75) then // at rest: sleep the hesitation, done
  begin
    HesitateAt := 2.0;
    if (Random() < 0.60) then
      Flick(1.0 + Sqr(Random()) * 5.0); // hesitation twitch
    Teleport(X, Y, RandSkew(80, 600));
  end else
    Teleport(X, Y);
end;

procedure TWindMouse.Step;
var
  Envelope, CurStep: Double;
  DirX, DirY, MoveLen, ArcOffset, AimX, AimY, AimLen: Double;
begin
  if HesitationStep() then // mid-move hesitation did this step
    Exit;

  DirX := (DestX - X) / RemainingDist;
  DirY := (DestY - Y) / RemainingDist;

  if (Overshot or (RemainingDist * DecelRate <= MaxStep)) then
  begin
    // Decelerate into the target
    // A coasting move overshoots the dest. The same branch then corrects it back (Overshot).
    MoveLen := Max(RemainingDist * DecelRate * RandFloat(0.7, 1.3), EndStep);
    if Coasting and (MoveLen >= RemainingDist - 0.5) then
    begin
      MoveLen := RemainingDist + Clamp(MaxStep * 0.3, 3.0, 8.0);
      Coasting := False;
      Overshot := True;
    end else
      MoveLen := Min(MoveLen, RemainingDist);
    MoveX := DirX * MoveLen;
    MoveY := DirY * MoveLen;
  end else
  begin
    Envelope := Max(SmoothStep(Clamp(Progress / 0.012, 0, 1)), LaunchScale);
    CurStep := MaxStep * Envelope;
    // aim at a decaying sideways offset, faded to 0 over the last MaxStep px + clamped so it terminates
    ArcOffset := ArcAmp * Exp(-ArcDecay * Progress) * Sin(ArcFreq * PI * Power(Progress, ArcSkew));
    ArcOffset := ArcOffset * Clamp((RemainingDist - MaxStep) / MaxStep, 0, 1);
    ArcOffset := Clamp(ArcOffset, -0.5 * RemainingDist, 0.5 * RemainingDist);
    AimX := (DestX - DirY * ArcOffset) - X;
    AimY := (DestY + DirX * ArcOffset) - Y;
    AimLen := Max(1.0, Hypot(AimX, AimY));
    // ease velocity toward the aim (length CurStep) by Inertia
    MoveX := MoveX + ((AimX / AimLen) * CurStep - MoveX) * Inertia;
    MoveY := MoveY + ((AimY / AimLen) * CurStep - MoveY) * Inertia;
  end;

  X := X + MoveX;
  Y := Y + MoveY;

  // ~2% chance of a sub-step
  if (Random() < SUBSTEP_CHANCE) and (Hypot(MoveX, MoveY) > 3.0) then
  begin
    Envelope := RandFloat(0.35, 0.70); // reuse local as the split fraction
    Teleport(X - MoveX * Envelope, Y - MoveY * Envelope, Gap * RandFloat(0.3, 0.6));
  end;

  Teleport(X, Y);
end;

procedure TWindMouse.Run(ATarget: TSimbaTarget; ADest: TPoint; OnMove: TMoveMouseEvent);
var
  StartX, StartY, LastDestX, LastDestY: Double;
  Timeout: UInt64;
  Stop: Boolean;
begin
  Self := Default(TWindMouse);

  Target  := ATarget;
  DestX   := ADest.X;
  DestY   := ADest.Y;
  Speed   := Clamp(ATarget.Options.MouseSpeed, 2.0, 30.0);
  Wind    := Clamp(ATarget.Options.MouseWind, 0.0, 15.0);
  Gravity := Clamp(ATarget.Options.MouseGravity, 1.0, 30.0);
  Timeout := GetTickCount64() + ATarget.Options.MouseTimeout;

  StartX := ATarget.MouseX;
  StartY := ATarget.MouseY;
  X := StartX;
  Y := StartY;
  Plan(StartX, StartY);

  MoveY := (LaunchScale * MaxStep) * (DestY - StartY) / TotalDist;
  MoveX := (LaunchScale * MaxStep) * (DestX - StartX) / TotalDist;
  if (Random() < 0.50) then
    Flick();
  LastDestX := DestX;
  LastDestY := DestY;

  while (Timeout > GetTickCount64()) do
  begin
    if Assigned(OnMove) then
    begin
      OnMove(X, Y, DestX, DestY, Stop);
      if Stop then
        Exit;
      // dest jumped > 25% of the remaining distance -> re-plan from here
      if (Hypot(DestX - LastDestX, DestY - LastDestY) > Hypot(X - LastDestX, Y - LastDestY) * 0.25) then
        Plan(X, Y);

      LastDestX := DestX;
      LastDestY := DestY;
    end;

    RemainingDist := Hypot(X - DestX, Y - DestY);
    if (RemainingDist <= 2.0) and (not Coasting) then // within 2px -> just land on the dest (unless coasting past)
      Break;

    if (RemainingDist > TotalDist) then
      TotalDist := RemainingDist;
    Progress := Clamp(1 - RemainingDist / TotalDist, 0, 1);

    Step;
  end;

  if (GetTickCount64() >= Timeout) and (Hypot(X - DestX, Y - DestY) > EndStep) then
    SimbaException(
      'MouseMove timed out after %dms. Start=%d,%d Dest=%d,%d XY=%d,%d',
      [ATarget.Options.MouseTimeout, Round(StartX), Round(StartY), Round(DestX), Round(DestY), Round(X), Round(Y)]
    );

  // Step directly on the dest if needed
  if (Round(X) <> DestX) or (Round(Y) <> DestY) then
    Teleport(DestX, DestY);
end;

procedure MoveMouseOnTarget(Target: TSimbaTarget; Dest: TPoint; MouseMoveEvent: TMoveMouseEvent);
var
  WindMouse: TWindMouse;
begin
  WindMouse.Run(Target, Dest, MouseMoveEvent);
end;

end.
