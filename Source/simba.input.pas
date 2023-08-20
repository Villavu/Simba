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
    TMouseButtonEvent = procedure(var Input: TSimbaInput; Button: EMouseButton) of object;
    {$SCOPEDENUMS ON}
    EMouseEventType = (TELEPORT, MOVING, CLICK, DOWN, UP);
    {$SCOPEDENUMS OFF}
  const
    DEFAULT_KEY_PRESS_MIN = 30;
    DEFAULT_KEY_PRESS_MAX = 140;

    DEFAULT_CLICK_MIN = 40;
    DEFAULT_CLICK_MAX = 220;

    DEFAULT_MOUSE_TIMEOUT = 15000;
    DEFAULT_MOUSE_SPEED   = 12;
    DEFAULT_MOUSE_GRAVITY = 9;
    DEFAULT_MOUSE_WIND    = 6;
  public
    FTarget: TSimbaTarget;

    FMouseEvents: array[EMouseEventType] of array of TMethod;

    function AddEvent(EventType: EMouseEventType; Method: TMethod): TMethod;
    function RemoveEvent(EventType: EMouseEventType; Method: TMethod): Boolean;

    function GetRandomKeyPressTime: Integer;
    function GetRandomMouseClickTime: Integer;

    function GetSpeed: Double;
    function GetGravity: Double;
    function GetWind: Double;
    function GetMouseTimeout: Integer;

    procedure CallOnMouseClickEvents(Button: EMouseButton);
    procedure CallOnMouseUpEvents(Button: EMouseButton);
    procedure CallOnMouseDownEvents(Button: EMouseButton);
    procedure CallOnTeleportEvents(X, Y: Integer);
    function CallOnMovingEvents(var X, Y: Double): Boolean;
  public
    KeyPressMin: Integer;
    KeyPressMax: Integer;

    MouseClickMin: Integer;
    MouseClickMax: Integer;

    MouseSpeed: Double;
    MouseGravity: Double;
    MouseWind: Double;

    MouseTimeout: Integer;

    function IsTargetValid: Boolean;
    function IsFocused: Boolean;
    function Focus: Boolean;

    function MousePosition: TPoint;
    function MousePressed(Button: EMouseButton): Boolean;
    procedure MouseMove(Dest: TPoint);
    procedure MouseClick(Button: EMouseButton);
    procedure MouseTeleport(X, Y: Integer); overload;
    procedure MouseTeleport(P: TPoint); overload;
    procedure MouseDown(Button: EMouseButton);
    procedure MouseUp(Button: EMouseButton);
    procedure MouseScroll(Scrolls: Integer);

    procedure KeySend(Text: String);
    procedure KeyPress(Key: EKeyCode);
    procedure KeyDown(Key: EKeyCode);
    procedure KeyUp(Key: EKeyCode);
    function KeyPressed(Key: EKeyCode): Boolean;

    function CharToKeyCode(C: Char): EKeyCode;

    function AddOnMouseTeleport(Event: TMouseTeleportEvent): TMouseTeleportEvent;
    function AddOnMouseMoving(Event: TMouseMovingEvent): TMouseMovingEvent;
    function AddOnMouseDown(Event: TMouseButtonEvent): TMouseButtonEvent;
    function AddOnMouseUp(Event: TMouseButtonEvent): TMouseButtonEvent;
    function AddOnMouseClick(Event: TMouseButtonEvent): TMouseButtonEvent;

    function RemoveOnMouseTeleport(Event: TMouseTeleportEvent): Boolean;
    function RemoveOnMouseMoving(Event: TMouseMovingEvent): Boolean;
    function RemoveOnMouseDown(Event: TMouseButtonEvent): Boolean;
    function RemoveOnMouseUp(Event: TMouseButtonEvent): Boolean;
    function RemoveOnMouseClick(Event: TMouseButtonEvent): Boolean;

    class operator Initialize(var Self: TSimbaInput);
  end;

implementation

uses
  simba.math, simba.nativeinterface, simba.random;

function TSimbaInput.AddEvent(EventType: EMouseEventType; Method: TMethod): TMethod;
begin
  Result := Method;

  FMouseEvents[EventType] += [Method];
end;

function TSimbaInput.RemoveEvent(EventType: EMouseEventType; Method: TMethod): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := High(FMouseEvents[EventType]) downto 0 do
    if (Method.Code = FMouseEvents[EventType][I].Code) and (Method.Data = FMouseEvents[EventType][I].Data) then
    begin
      Delete(FMouseEvents[EventType], I, 1);

      Result := True;
    end;
end;

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

function TSimbaInput.GetSpeed: Double;
begin
  if (MouseSpeed = 0) then
    Result := DEFAULT_MOUSE_SPEED
  else
    Result := MouseSpeed;
end;

function TSimbaInput.GetGravity: Double;
begin
  if (MouseGravity = 0) then
    Result := DEFAULT_MOUSE_GRAVITY
  else
    Result := MouseGravity;
end;

function TSimbaInput.GetWind: Double;
begin
  if (MouseWind = 0) then
    Result := DEFAULT_MOUSE_WIND
  else
    Result := MouseWind;
end;

function TSimbaInput.GetMouseTimeout: Integer;
begin
  if (MouseTimeout = 0) then
    Result := DEFAULT_MOUSE_TIMEOUT
  else
    Result := MouseTimeout;
end;

procedure TSimbaInput.CallOnMouseClickEvents(Button: EMouseButton);
var
  Method: TMethod;
begin
  for Method in FMouseEvents[EMouseEventType.CLICK] do
    TMouseButtonEvent(Method)(Self, Button);
end;

procedure TSimbaInput.CallOnMouseUpEvents(Button: EMouseButton);
var
  Method: TMethod;
begin
  for Method in FMouseEvents[EMouseEventType.UP] do
    TMouseButtonEvent(Method)(Self, Button);
end;

procedure TSimbaInput.CallOnMouseDownEvents(Button: EMouseButton);
var
  Method: TMethod;
begin
  for Method in FMouseEvents[EMouseEventType.DOWN] do
    TMouseButtonEvent(Method)(Self, Button);
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

function TSimbaInput.MousePressed(Button: EMouseButton): Boolean;
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
    Timeout: UInt64;
  begin
    veloX := 0; veloY := 0;
    windX := 0; windY := 0;

    x := xs;
    y := ys;

    Timeout := GetTickCount64() + GetMouseTimeout();

    while CallOnMovingEvents(X, Y) do
    begin
      if (GetTickCount64() > Timeout) then
        SimbaException('MouseMove timed out after %dms. Start: (%d,%d), Dest: (%d,%d)', [GetMouseTimeout(), Round(xs), Round(ys), Round(xe), Round(ye)]);

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

      if (step >= maxStep) then
        step := maxStep - (Random() * (maxStep / 4));
      if (step < 3) then
        step := 3 + (Random() * 3);

      veloX := veloX + windX;
      veloY := veloY + windY;
      veloX := veloX + gravity * (xe - x) / remainingDistance;
      veloY := veloY + gravity * (ye - y) / remainingDistance;

      if (Hypot(veloX, veloY) > step) then
      begin
        randomDist := step / 3.0 + (step / 2 * Random());

        veloMag := Sqrt(veloX * veloX + veloY * veloY);
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
    5 / RandSpeed, 10 / RandSpeed, 25 * RandSpeed, 20 * RandSpeed
  );
end;

procedure TSimbaInput.MouseClick(Button: EMouseButton);
begin
  CallOnMouseClickEvents(Button);

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

procedure TSimbaInput.MouseDown(Button: EMouseButton);
begin
  CallOnMouseDownEvents(Button);

  FTarget.MouseDown(Button);
end;

procedure TSimbaInput.MouseUp(Button: EMouseButton);
begin
  CallOnMouseUpEvents(Button);

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

procedure TSimbaInput.KeyPress(Key: EKeyCode);
begin
  FTarget.KeyDown(Key);
  SimbaNativeInterface.PreciseSleep(GetRandomKeyPressTime());
  FTarget.KeyUp(Key);
end;

procedure TSimbaInput.KeyDown(Key: EKeyCode);
begin
  FTarget.KeyDown(Key);
end;

procedure TSimbaInput.KeyUp(Key: EKeyCode);
begin
  FTarget.KeyUp(Key);
end;

function TSimbaInput.KeyPressed(Key: EKeyCode): Boolean;
begin
  Result := FTarget.KeyPressed(Key);
end;

function TSimbaInput.CharToKeyCode(C: Char): EKeyCode;
begin
  case C of
    '0'..'9': Result := EKeyCode(Ord(EKeyCode.NUM_0) + Ord(C) - Ord('0'));
    'a'..'z': Result := EKeyCode(Ord(EKeyCode.A) + Ord(C) - Ord('a'));
    'A'..'Z': Result := EKeyCode(Ord(EKeyCode.A) + Ord(C) - Ord('A'));
    #34, #39: Result := EKeyCode.OEM_7;
    #32: Result := EKeyCode.SPACE;
    '!': Result := EKeyCode.NUM_1;
    '#': Result := EKeyCode.NUM_3;
    '$': Result := EKeyCode.NUM_4;
    '%': Result := EKeyCode.NUM_5;
    '&': Result := EKeyCode.NUM_7;
    '(': Result := EKeyCode.NUM_9;
    ')': Result := EKeyCode.NUM_0;
    '*': Result := EKeyCode.NUM_8;
    '+': Result := EKeyCode.ADD;
    ',': Result := EKeyCode.OEM_COMMA;
    '-': Result := EKeyCode.OEM_MINUS;
    '.': Result := EKeyCode.OEM_PERIOD;
    '/': Result := EKeyCode.OEM_2;
    ':': Result := EKeyCode.OEM_1;
    ';': Result := EKeyCode.OEM_1;
    '<': Result := EKeyCode.OEM_COMMA;
    '=': Result := EKeyCode.ADD;
    '>': Result := EKeyCode.OEM_PERIOD;
    '?': Result := EKeyCode.OEM_2;
    '@': Result := EKeyCode.NUM_2;
    '[': Result := EKeyCode.OEM_4;
    '\': Result := EKeyCode.OEM_5;
    ']': Result := EKeyCode.OEM_6;
    '^': Result := EKeyCode.NUM_6;
    '_': Result := EKeyCode.OEM_MINUS;
    '`': Result := EKeyCode.OEM_3;
    '{': Result := EKeyCode.OEM_4;
    '|': Result := EKeyCode.OEM_5;
    '}': Result := EKeyCode.OEM_6;
    '~': Result := EKeyCode.OEM_3;
    else
      Result := EKeyCode.UNKNOWN;
  end;
end;

procedure TSimbaInput.CallOnTeleportEvents(X, Y: Integer);
var
  Method: TMethod;
begin
  for Method in FMouseEvents[EMouseEventType.TELEPORT] do
    TMouseTeleportEvent(Method)(Self, X, Y);
end;

function TSimbaInput.CallOnMovingEvents(var X, Y: Double): Boolean;
var
  Method: TMethod;
begin
  Result := True;

  for Method in FMouseEvents[EMouseEventType.MOVING] do
    if not TMouseMovingEvent(Method)(Self, X, Y) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSimbaInput.AddOnMouseTeleport(Event: TMouseTeleportEvent): TMouseTeleportEvent;
begin
  Result := TMouseTeleportEvent(AddEvent(EMouseEventType.TELEPORT, TMethod(Event)));
end;

function TSimbaInput.AddOnMouseMoving(Event: TMouseMovingEvent): TMouseMovingEvent;
begin
  Result := TMouseMovingEvent(AddEvent(EMouseEventType.MOVING, TMethod(Event)));
end;

function TSimbaInput.AddOnMouseDown(Event: TMouseButtonEvent): TMouseButtonEvent;
begin
  Result := TMouseButtonEvent(AddEvent(EMouseEventType.DOWN, TMethod(Event)));
end;

function TSimbaInput.AddOnMouseUp(Event: TMouseButtonEvent): TMouseButtonEvent;
begin
  Result := TMouseButtonEvent(AddEvent(EMouseEventType.UP, TMethod(Event)));
end;

function TSimbaInput.AddOnMouseClick(Event: TMouseButtonEvent): TMouseButtonEvent;
begin
  Result := TMouseButtonEvent(AddEvent(EMouseEventType.CLICK, TMethod(Event)));
end;

function TSimbaInput.RemoveOnMouseTeleport(Event: TMouseTeleportEvent): Boolean;
begin
  Result := RemoveEvent(EMouseEventType.TELEPORT, TMethod(Event));
end;

function TSimbaInput.RemoveOnMouseMoving(Event: TMouseMovingEvent): Boolean;
begin
  Result := RemoveEvent(EMouseEventType.MOVING, TMethod(Event));
end;

function TSimbaInput.RemoveOnMouseDown(Event: TMouseButtonEvent): Boolean;
begin
  Result := RemoveEvent(EMouseEventType.DOWN, TMethod(Event));
end;

function TSimbaInput.RemoveOnMouseUp(Event: TMouseButtonEvent): Boolean;
begin
  Result := RemoveEvent(EMouseEventType.UP, TMethod(Event));
end;

function TSimbaInput.RemoveOnMouseClick(Event: TMouseButtonEvent): Boolean;
begin
  Result := RemoveEvent(EMouseEventType.CLICK, TMethod(Event));
end;

class operator TSimbaInput.Initialize(var Self: TSimbaInput);
begin
  Self := Default(TSimbaInput);
end;

end.

