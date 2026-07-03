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

procedure MoveMouseOnTarget(Target: TSimbaTarget; Dest: TPoint; MouseMoveEvent: TMoveMouseEvent);

  procedure Move(const X, Y, Idle: Double);
  var
    P: TPoint;
  begin
    P.X := Round(X);
    P.Y := Round(Y);

    Target.MouseTeleport(P);
    SimbaNativeInterface.PreciseSleep(Round(Idle));
  end;

  // Hermite smoothstep: 0..1 with zero slope at both ends
  function SmoothStep(const Edge0, Edge1, Value: Double): Double;
  var
    T: Double;
  begin
    if (Edge1 <= Edge0) then
      Exit(Ord(Value >= Edge1));
    T := EnsureRange((Value - Edge0) / (Edge1 - Edge0), 0, 1);
    Result := T * T * (3 - 2 * T);
  end;

  // Random in [Near, Far], biased toward Near
  function RandSkew(const Near, Far: Double): Double;
  begin
    Result := Near + (Far - Near) * Random() * Random();
  end;

  // WindMouse by BenLand100 but enhanced:
  // The random walk is replaced by a per-move plan + feedback loop.
  // Same knobs: Speed = pace, Wind = wander (0 = straight).
  // Gravity = pull back toward the line. Changes from the original:
  //   * Speed envelope (ease in / hold / ease out) with a two-sine ripple on the
  //     pace; scales with distance (Fitts); geometric crawl floor - no inching in
  //   * Path is a decaying 1-3 half-wave arc, random side/peak/span, may rejoin
  //     the line early; floored (never laser-straight), capped (never orbits)
  //   * Start flick; ~10% hesitation on long moves (brake or dead stop, with skid,
  //     twitches, re-engage jolt); end-zone tremor; misaligned arrivals that
  //     visibly correct; at most one overshoot
  //   * Velocity inertia smooths all steering; a destination moved live by
  //     MouseMoveEvent is tracked at pace, even if it keeps escaping
  procedure WindMouseEnhanced(X1, Y1, X2, Y2, Speed, Wind, Gravity: Double; Timeout: Integer);
  const
    FLICK_CHANCE  = 0.65; // start with a directional flick kick
    TAPER_CHANCE  = 0.45; // keep some arc into the endgame (arrive misaligned, then correct)
    PAUSE_CHANCE  = 0.10; // hesitate mid-move (further scaled down by short distance)
    GUST_CHANCE   = 0.10; // gust of much stronger wind
    JOLT_CHANCE   = 0.60; // skid off-path when braking, and re-engage jolt on pickup
    TWITCH_CHANCE = 0.04; // micro-twitch per held step during a stall
  var
    X, Y, DirX, DirY: Double;
    MoveX, MoveY, MoveLen, AimX, AimY, AimLen: Double;
    Vmax, Vcur, Vmin, ArcScale, ArcPull, EndZone, Inertia, Progress, Env, Bow: Double;
    SegDist, RemainingDist: Double;
    AccelAt, DecelAt, AccelShape, DecelShape: Double;
    ArcAmp, ArcSkew, ArcDecay, ArcFreq, ArcSpan: Double;
    FlickAngle, FlickMag, TaperFloor, EndWobble, EndAngle, U: Double;
    Ripple, RippleAmp, RippleRate1, RippleRate2, RipplePhase1, RipplePhase2: Double;
    PauseAt, PauseDepth: Double;
    PauseHold, PauseLeft: Integer;
    PrevR: Double;
    RecedeN: Integer;
    T: UInt64;
    Stop, Overshot, Chasing: Boolean;
  begin
    X := X1; Y := Y1;
    MoveX := 0; MoveY := 0;
    SegDist := Max(1.0, Hypot(X1 - X2, Y1 - Y2));

    // Peak px/step: ~10-13 per 10 Speed, higher for longer moves (600px = 1x reference)
    Vmax := RandSkew(10.0, 10.0 * 1.30) * (Speed / 10.0) *
            EnsureRange(Sqrt(SegDist / 600.0), 0.65, 2.3);

    // Wind pushes off the line, gravity pulls back (defaults 4/12 -> ArcScale ~0.93)
    ArcScale := 2.8 * Wind / Max(1.0, Gravity);
    ArcPull  := EnsureRange(Gravity / 12.0, 0.3, 3.0);
    // Gust: some moves wander much harder, as if the wind briefly ~doubled - an
    // occasional wide sweep among the normal arcs (Wind stays 0 = dead straight)
    if (Random() < GUST_CHANCE) then
      ArcScale := ArcScale * (1.7 + Random() * 0.6);

    Vmin := Max(1.0, Vmax * 0.07); // absolute crawl floor
    EndZone := Max(4.0, Vmax);     // within this distance: straight final approach

    // Envelope: where full speed is reached / where the ease-out starts / ramp shapes
    AccelShape := 0.60 + Random() * 0.80;
    DecelShape := 0.60 + Random() * 0.80;
    AccelAt    := 0.32 * Power(Random(), 3.5);
    DecelAt    := 0.99 - 0.40 * Power(Random(), 3.0);
    Inertia    := 0.31 + 0.17 * Sqr(Random()); // arm weight

    // Start flick: an initial kick in a random direction, reabsorbed by inertia
    if (Random() < FLICK_CHANCE) then
    begin
      FlickAngle := Random() * 2 * PI;
      FlickMag   := Min(Sqr(Random()) * 6.5 * ArcScale, Vmax * 1.5);
      MoveX := Cos(FlickAngle) * FlickMag;
      MoveY := Sin(FlickAngle) * FlickMag;
    end;

    // End traits: some arc may survive into the endgame (arrive misaligned, correct),
    // plus a small angular tremor while homing in
    if (Random() < TAPER_CHANCE) then
      TaperFloor := Random() * 0.35
    else
      TaperFloor := 0;
    EndWobble := Sqr(Random()) * 0.22;

    // Hesitation: a deep mid-move brake, held for a beat (steps, so it always ends).
    // Chance fades in with distance: 0 below ~120px, ~10% from 250px up.
    if (Random() < PAUSE_CHANCE * EnsureRange((SegDist - 120) / 130, 0, 1)) then
    begin
      PauseAt    := 0.20 + Random() * 0.55;          // trigger point along the path
      PauseDepth := 0.90 + Random() * 0.10;          // slow smear .. dead stop
      PauseHold  := 30 + Round(Sqr(Random()) * 120); // ~80..400ms
    end
    else
    begin
      PauseAt    := 2.0; // never triggers
      PauseDepth := 0;
      PauseHold  := 0;
    end;
    PauseLeft := 0;

    // Arc: peak sideways offset (floored so no move is laser-straight, capped so
    // hostile Wind/Gravity can't orbit), where it peaks, how fast it's pulled in,
    // and how much of the path it spans - a short span rejoins the line early and
    // rides it in, instead of only meeting it at the target
    ArcAmp   := Min((0.030 + Sqr(Random()) * 0.130) * ArcScale, 0.45);
    ArcSkew  := 0.55 + Random() * 0.60;
    ArcDecay := (0.5 + Random() * 1.9) * ArcPull;
    ArcSpan  := 1.0 - Sqr(Random()) * 0.45;
    if (Random() >= 0.5) then ArcAmp := -ArcAmp; // which side of the line

    // 1..3 half-waves: bow / S-curve / double wave
    U := Random();
    if (U < 0.45) then ArcFreq := 1.0
    else if (U < 0.85) then ArcFreq := 2.0
    else ArcFreq := 3.0;

    // Speed ripple: two slow sines swell/dip the pace (band-limited, no stutter)
    RippleAmp    := 0.20 + Sqr(Random()) * 0.35;
    RippleRate1  := 0.12 + Random() * 0.28;
    RippleRate2  := RippleRate1 * (1.5 + Random() * 0.8);
    RipplePhase1 := Random() * 2 * PI;
    RipplePhase2 := Random() * 2 * PI;

    Overshot := False; // at most one overshoot of the target
    PrevR := 1e30;
    RecedeN := 0;
    Chasing := False;

    T := GetTickCount64() + Timeout;
    while (T > GetTickCount64()) do
    begin
      if Assigned(MouseMoveEvent) then
      begin
        MouseMoveEvent(X, Y, X2, Y2, Stop);
        if Stop then
          Exit;
      end;

      RemainingDist := Hypot(X - X2, Y - Y2);
      if (RemainingDist <= 1.0) then
        Break;

      // A destination moved live by MouseMoveEvent needs no re-plan: direction is
      // recomputed from it every step, and the segment only ever grows to cover it
      // (also handles the overshoot pass) - progress tracks the actual approach
      if (RemainingDist > SegDist) then
        SegDist := RemainingDist;

      Progress := EnsureRange(1 - RemainingDist / SegDist, 0, 1);

      // Envelope times ripple; the ripple's gate is full on the plateau, 0.25 floor
      // through the ramps and crawl
      Env    := Power(SmoothStep(0, AccelAt, Progress), AccelShape) *
                Power(1 - SmoothStep(DecelAt, 1, Progress), DecelShape);
      Ripple := 1.0 + RippleAmp * (0.65 * Sin(RipplePhase1) + 0.35 * Sin(RipplePhase2)) *
                      (0.25 + 0.75 * Sqrt(Env));

      // While easing out, floor the crawl at ~7% of what's left (geometric approach;
      // pixel-fine steps only near the very end)
      if (Progress > DecelAt) then
        U := EnsureRange(RemainingDist * 0.07, Vmin, Max(4.0, Vmax * 0.25))
      else
        U := Vmin;
      Vcur := (U + (Vmax - U) * Env) * Ripple;
      RipplePhase1 := RipplePhase1 + RippleRate1;
      RipplePhase2 := RipplePhase2 + RippleRate2;

      // A destination that keeps escaping (no net approach for several steps - can't
      // happen on a static move) would outrun the ease-in/crawl forever: latch onto
      // it at pace until the envelope has caught up (past AccelAt = plateau)
      if (RemainingDist > EndZone) and (RemainingDist >= PrevR) then
        Inc(RecedeN)
      else
        RecedeN := 0;
      if (RecedeN >= 5) then
        Chasing := True;
      if (Progress > AccelAt) then
        Chasing := False;
      if Chasing then
        Vcur := Max(Vcur, 0.6 * Vmax * Ripple);
      PrevR := RemainingDist;

      // Hesitation: brake for PauseHold steps once PauseAt is crossed; jolts on
      // braking (skid off the path), while held (twitch) and on pickup
      if (Progress >= PauseAt) then
      begin
        PauseAt   := 2.0; // one hesitation per move
        PauseLeft := PauseHold;
        if (Random() < JOLT_CHANCE) then
        begin
          FlickAngle := Random() * 2 * PI;
          FlickMag   := Min((0.5 + Sqr(Random()) * 4.5) * ArcScale, Vmax);
          MoveX := MoveX + Cos(FlickAngle) * FlickMag;
          MoveY := MoveY + Sin(FlickAngle) * FlickMag;
        end;
      end;
      if (PauseLeft > 0) then
      begin
        Dec(PauseLeft);
        Vcur := Vcur * (1.0 - PauseDepth);
        if (Random() < TWITCH_CHANCE) then
        begin
          FlickAngle := Random() * 2 * PI;
          MoveX := MoveX + Cos(FlickAngle) * (0.4 + Random());
          MoveY := MoveY + Sin(FlickAngle) * (0.4 + Random());
        end;
        if (PauseLeft = 0) and (Random() < JOLT_CHANCE) then
        begin
          FlickAngle := Random() * 2 * PI;
          FlickMag   := Min(Sqr(Random()) * 6.5 * ArcScale, Vmax);
          MoveX := MoveX + Cos(FlickAngle) * FlickMag;
          MoveY := MoveY + Sin(FlickAngle) * FlickMag;
        end;
      end;

      // Direction to the live target
      DirX := (X2 - X) / RemainingDist;
      DirY := (Y2 - Y) / RemainingDist;

      if (RemainingDist <= EndZone) then
      begin
        // Final approach: home in with a tremor; allow a single momentum overshoot,
        // after that only ever close the distance
        if (not Overshot) then
          MoveLen := Min(Vcur, RemainingDist + Sqr(RemainingDist) / EndZone)
        else
          MoveLen := Min(Vcur, RemainingDist);
        if (MoveLen > RemainingDist) then
          Overshot := True;
        EndAngle := (Random() * 2 - 1) * EndWobble;
        MoveX := (DirX * Cos(EndAngle) - DirY * Sin(EndAngle)) * MoveLen;
        MoveY := (DirX * Sin(EndAngle) + DirY * Cos(EndAngle)) * MoveLen;
      end else
      begin
        // Arc: aim at a point offset sideways from the target; decaying 1..3
        // half-waves, tapered off toward EndZone (down to TaperFloor)
        Bow := ArcAmp * SegDist * Exp(-ArcDecay * Progress) *
               Sin(ArcFreq * PI * Power(Min(1.0, Progress / ArcSpan), ArcSkew));
        Bow := Bow * (TaperFloor + (1.0 - TaperFloor) * EnsureRange((RemainingDist - EndZone) / EndZone, 0, 1));
        // Never offset more than half the distance left: keeps the aim leaning
        // homeward, so the move terminates even under hostile Wind/Gravity
        Bow := EnsureRange(Bow, -0.5 * RemainingDist, 0.5 * RemainingDist);
        AimX := (X2 - DirY * Bow) - X;
        AimY := (Y2 + DirX * Bow) - Y;
        AimLen := Max(1.0, Hypot(AimX, AimY));

        // Inertia: ease the emitted motion toward the desired velocity
        MoveX := MoveX + ((AimX / AimLen) * Vcur - MoveX) * Inertia;
        MoveY := MoveY + ((AimY / AimLen) * Vcur - MoveY) * Inertia;
        MoveLen := Hypot(MoveX, MoveY);
      end;

      X := X + MoveX;
      Y := Y + MoveY;

      // Idle: ~2.7..5.4ms cadence scaled by step length
      Move(X, Y, 2.7 * (MoveLen / Ripple / Vmax) + 2.7);
    end;

    if (GetTickCount64() >= T) then
      SimbaException('MouseMove timed out after %dms. Start: (%d,%d), Dest: (%d,%d)', [Timeout, Round(X1), Round(Y1), Round(X2), Round(Y2)]);
  end;

begin
  WindMouseEnhanced(
    Target.MouseX, Target.MouseY,
    Dest.X, Dest.Y,
    Target.Options.MouseSpeed,
    Target.Options.MouseWind,
    Target.Options.MouseGravity,
    Target.Options.MouseTimeout
  );
end;

end.
