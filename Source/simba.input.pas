{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.input;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Math,
  simba.mufasatypes, simba.target;

type
  PSimbaInput = ^TSimbaInput;
  TSimbaInput = packed record
  type
    TMouseTeleportEvent = procedure(var Input: TSimbaInput; X, Y: Integer) of object;
    TMouseMovingEvent = function(var Input: TSimbaInput; var X, Y: Double): Boolean of object;
  const
    DEFAULT_KEY_PRESS_MIN = 30;
    DEFAULT_KEY_PRESS_MAX = 140;

    DEFAULT_CLICK_MIN = 40;
    DEFAULT_CLICK_MAX = 220;

    DEFAULT_SPEED   = 12;
    DEFAULT_GRAVITY = 9;
    DEFAULT_WIND    = 6;
  public
    FTarget: TSimbaTarget;

    FTeleportingEvents: array of TMouseTeleportEvent;
    FMovingEvents: array of TMouseMovingEvent;

    function GetRandomKeyPressTime: Integer;
    function GetRandomMouseClickTime: Integer;

    function GetSpeed: Integer;
    function GetGravity: Double;
    function GetWind: Double;

    procedure CallOnTeleportEvents(X, Y: Integer);
    procedure CallOnMovingEvents(var X, Y: Double);
  public
    KeyPressMin: Integer;
    KeyPressMax: Integer;

    MouseClickMin: Integer;
    MouseClickMax: Integer;

    Speed: Integer;
    Gravity: Double;
    Wind: Double;

    function IsTargetValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    function MousePosition: TPoint;
    function MousePressed(Button: MouseButton): Boolean;
    procedure MouseMove(Dest: TPoint);
    procedure MouseClick(Button: MouseButton);
    procedure MouseTeleport(X, Y: Integer); overload;
    procedure MouseTeleport(P: TPoint); overload;
    procedure MouseDown(Button: MouseButton);
    procedure MouseUp(Button: MouseButton);
    procedure MouseScroll(Scrolls: Integer);

    procedure KeySend(Text: String);
    procedure KeyPress(Key: KeyCode);
    procedure KeyDown(Key: KeyCode);
    procedure KeyUp(Key: KeyCode);
    function KeyPressed(Key: KeyCode): Boolean;

    function CharToKeyCode(C: Char): KeyCode;

    function AddOnMouseTeleport(Event: TMouseTeleportEvent): TMouseTeleportEvent;
    function AddOnMouseMoving(Event: TMouseMovingEvent): TMouseMovingEvent;
    procedure RemoveOnMouseTeleport(Event: TMouseTeleportEvent);
    procedure RemoveOnMouseMoving(Event: TMouseMovingEvent);

    class operator Initialize(var Self: TSimbaInput);
  end;

implementation

uses
  simba.math, simba.nativeinterface, simba.random;

function TSimbaInput.GetRandomKeyPressTime: Integer;
begin
  if (KeyPressMin = 0) and (KeyPressMax = 0) then
    Result := RandomLeft(DEFAULT_KEY_PRESS_MIN, DEFAULT_KEY_PRESS_MAX)
  else
    Result := RandomLeft(KeyPressMin, KeyPressMax);
end;

function TSimbaInput.GetRandomMouseClickTime: Integer;
begin
  if (MouseClickMin = 0) and (MouseClickMax = 0) then
    Result := RandomLeft(DEFAULT_CLICK_MIN, DEFAULT_CLICK_MAX)
  else
    Result := RandomLeft(MouseClickMin, MouseClickMax);
end;

function TSimbaInput.GetSpeed: Integer;
begin
  if (Speed = 0) then
    Result := DEFAULT_SPEED
  else
    Result := Speed;
end;

function TSimbaInput.GetGravity: Double;
begin
  if (Gravity = 0) then
    Result := DEFAULT_GRAVITY
  else
    Result := Gravity;
end;

function TSimbaInput.GetWind: Double;
begin
  if (Wind = 0) then
    Result := DEFAULT_WIND
  else
    Result := Wind;
end;

function TSimbaInput.IsTargetValid: Boolean;
begin
  Result := FTarget.IsValid();
end;

function TSimbaInput.IsFocused: Boolean;
begin
  Result := FTarget.IsFocused();
end;

function TSimbaInput.Focus: Boolean;
begin
  Result := FTarget.Focus();
end;

function TSimbaInput.MousePosition: TPoint;
begin
  Result := FTarget.MousePosition();
end;

function TSimbaInput.MousePressed(Button: MouseButton): Boolean;
begin
  Result := FTarget.MousePressed(Button);
end;

procedure TSimbaInput.MouseMove(Dest: TPoint);

  // Credit: BenLand100 (https://github.com/BenLand100/SMART/blob/master/src/EventNazi.java#L201)
  procedure WindMouse(xs, ys, xe, ye, gravity, wind, minWait, maxWait, maxStep, targetArea: Double);
  var
    x, y: Double;
    veloX, veloY, windX, windY, veloMag, randomDist, step, idle: Double;
    traveledDistance, remainingDistance: Double;
  begin
    veloX := 0; veloY := 0;
    windX := 0; windY := 0;

    x := xs;
    y := ys;

    while True do
    begin
      CallOnMovingEvents(X, Y);

      traveledDistance := Hypot(x - xs, y - ys);
      remainingDistance := Hypot(x - xe, y - ye);
      if (remainingDistance <= 1) then
        Break;

      wind := Min(wind, remainingDistance);
      windX := windX / SQRT_3 + (Random(Round(wind) * 2 + 1) - wind) / SQRT_5;
      windY := windY / SQRT_3 + (Random(Round(wind) * 2 + 1) - wind) / SQRT_5;

      if (remainingDistance < targetArea) then
        step := (remainingDistance / 2) + (Random() * 6 - 3)
      else
      if (traveledDistance < targetArea) then
      begin
        if (traveledDistance < 3) then
          traveledDistance := 10 * Random();

        step := traveledDistance * (1 + Random() * 3);
      end else
        step := maxStep;

      step := Min(step, maxStep);
      if (step < 3) then
        step := 3 + (Random() * 3);

      veloX := veloX + windX;
      veloY := veloY + windY;
      veloX := veloX + gravity * (xe - x) / remainingDistance;
      veloY := veloY + gravity * (ye - y) / remainingDistance;

      if (Hypot(veloX, veloY) > step) then
      begin
        randomDist := step / 3.0 + (step / 2 * Random());

        veloMag := sqrt(veloX * veloX + veloY * veloY);
        veloX := (veloX / veloMag) * randomDist;
        veloY := (veloY / veloMag) * randomDist;
      end;

      idle := (maxWait - minWait) * (Hypot(veloX, veloY) / maxStep) + minWait;

      x := x + veloX;
      y := y + veloY;

      Self.MouseTeleport(Round(x), Round(y));

      SimbaNativeInterface.PreciseSleep(Round(idle));
    end;

    Self.MouseTeleport(Round(xe), Round(ye));
  end;

var
  Start: TPoint;
  RandSpeed, Exponential: Double;
begin
  Start := MousePosition();

  // Further the distance the faster we move.
  Exponential := Power(Hypot(Start.X - Dest.X, Start.Y - Dest.Y), 0.33) / 10;

  RandSpeed := RandomLeft(GetSpeed(), GetSpeed() * 1.65);
  RandSpeed *= Exponential;
  RandSpeed /= 10;

  WindMouse(
    Start.X, Start.Y, Dest.X, Dest.Y,
    GetGravity(), GetWind(),
    5 / RandSpeed, 10 / RandSpeed, 20 * RandSpeed, 20 * RandSpeed
  );
end;

procedure TSimbaInput.MouseClick(Button: MouseButton);
begin
  FTarget.MouseDown(Button);
  SimbaNativeInterface.PreciseSleep(GetRandomMouseClickTime());
  FTarget.MouseUp(Button);
end;

procedure TSimbaInput.MouseTeleport(P: TPoint);
begin
  FTarget.MouseTeleport(P);

  CallOnTeleportEvents(P.X, P.Y);
end;

procedure TSimbaInput.MouseTeleport(X, Y: Integer);
begin
  MouseTeleport(Point(X, Y));
end;

procedure TSimbaInput.MouseDown(Button: MouseButton);
begin
  FTarget.MouseDown(Button);
end;

procedure TSimbaInput.MouseUp(Button: MouseButton);
begin
  FTarget.MouseUp(Button);
end;

procedure TSimbaInput.MouseScroll(Scrolls: Integer);
begin
  FTarget.MouseScroll(Scrolls);
end;

procedure TSimbaInput.KeySend(Text: String);
var
  I: Integer;
begin
  for I := 1 to Length(Text) do
    FTarget.KeySend(Text[I], GetRandomKeyPressTime() div 2, GetRandomKeyPressTime() div 2, GetRandomKeyPressTime() div 2, GetRandomKeyPressTime() div 2);
end;

procedure TSimbaInput.KeyPress(Key: KeyCode);
begin
  FTarget.KeyDown(Key);
  SimbaNativeInterface.PreciseSleep(GetRandomKeyPressTime());
  FTarget.KeyUp(Key);
end;

procedure TSimbaInput.KeyDown(Key: KeyCode);
begin
  FTarget.KeyDown(Key);
end;

procedure TSimbaInput.KeyUp(Key: KeyCode);
begin
  FTarget.KeyUp(Key);
end;

function TSimbaInput.KeyPressed(Key: KeyCode): Boolean;
begin
  Result := FTarget.KeyPressed(Key);
end;

function TSimbaInput.CharToKeyCode(C: Char): KeyCode;
begin
  case C of
    '0'..'9': Result := KeyCode(Ord(KeyCode.NUM_0) + Ord(C) - Ord('0'));
    'a'..'z': Result := KeyCode(Ord(KeyCode.A) + Ord(C) - Ord('a'));
    'A'..'Z': Result := KeyCode(Ord(KeyCode.A) + Ord(C) - Ord('A'));
    #34, #39: Result := KeyCode.OEM_7;
    #32: Result := KeyCode.SPACE;
    '!': Result := KeyCode.NUM_1;
    '#': Result := KeyCode.NUM_3;
    '$': Result := KeyCode.NUM_4;
    '%': Result := KeyCode.NUM_5;
    '&': Result := KeyCode.NUM_7;
    '(': Result := KeyCode.NUM_9;
    ')': Result := KeyCode.NUM_0;
    '*': Result := KeyCode.NUM_8;
    '+': Result := KeyCode.ADD;
    ',': Result := KeyCode.OEM_COMMA;
    '-': Result := KeyCode.OEM_MINUS;
    '.': Result := KeyCode.OEM_PERIOD;
    '/': Result := KeyCode.OEM_2;
    ':': Result := KeyCode.OEM_1;
    ';': Result := KeyCode.OEM_1;
    '<': Result := KeyCode.OEM_COMMA;
    '=': Result := KeyCode.ADD;
    '>': Result := KeyCode.OEM_PERIOD;
    '?': Result := KeyCode.OEM_2;
    '@': Result := KeyCode.NUM_2;
    '[': Result := KeyCode.OEM_4;
    '\': Result := KeyCode.OEM_5;
    ']': Result := KeyCode.OEM_6;
    '^': Result := KeyCode.NUM_6;
    '_': Result := KeyCode.OEM_MINUS;
    '`': Result := KeyCode.OEM_3;
    '{': Result := KeyCode.OEM_4;
    '|': Result := KeyCode.OEM_5;
    '}': Result := KeyCode.OEM_6;
    '~': Result := KeyCode.OEM_3;
    else
      Result := KeyCode.UNKNOWN;
  end;
end;

procedure TSimbaInput.CallOnTeleportEvents(X, Y: Integer);
var
  Event: TMouseTeleportEvent;
begin
  for Event in FTeleportingEvents do
    Event(Self, X, Y);
end;

procedure TSimbaInput.CallOnMovingEvents(var X, Y: Double);
var
  Event: TMouseMovingEvent;
begin
  for Event in FMovingEvents do
    Event(Self, X, Y);
end;

function TSimbaInput.AddOnMouseTeleport(Event: TMouseTeleportEvent): TMouseTeleportEvent;
begin
  Result := Event;

  FTeleportingEvents += [Event];
end;

function TSimbaInput.AddOnMouseMoving(Event: TMouseMovingEvent): TMouseMovingEvent;
begin
  Result := Event;

  FMovingEvents += [Event];
end;

procedure TSimbaInput.RemoveOnMouseTeleport(Event: TMouseTeleportEvent);
var
  I: Integer;
begin
  for I := High(FTeleportingEvents) downto 0 do
    if (Event = FTeleportingEvents[I]) then
      Delete(FTeleportingEvents, I, 1);
end;

procedure TSimbaInput.RemoveOnMouseMoving(Event: TMouseMovingEvent);
var
  I: Integer;
begin
  for I := High(FMovingEvents) downto 0 do
    if (Event = FMovingEvents[I]) then
      Delete(FMovingEvents, I, 1);
end;

class operator TSimbaInput.Initialize(var Self: TSimbaInput);
begin
  Self := Default(TSimbaInput);
end;

end.

