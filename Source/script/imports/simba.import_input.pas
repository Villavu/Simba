unit simba.import_input;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportInput(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  ffi,
  simba.input, simba.target;

(*
Input
=====
Input (Mouse & Keyboard) related methods.

- If the `TInput.Target` field is set, the global "Target" variable will be used. Which by default is set to Simba's target selection.
- There is a pre-defined variable `Input` to use.

Example:

```
  Input.MouseMove([200,200]); // Move the mouse to x: 200, y: 200
  Input.MouseClick(EMouseButton.LEFT); // And left click
```

----

| Key codes and mouse buttons are both **scoped** enums.
| For scoped a enum you **must** include the type of the enum.

Example:

```
  EMouseButton.LEFT
  EKeyCode.A
```

Available Mouse Buttons:

```
  EMouseButton.LEFT
  EMouseButton.RIGHT
  EMouseButton.MIDDLE
  EMouseButton.SCROLL_UP
  EMouseButton.SCROLL_DOWN
```

Available Key Codes:

```
  EKeyCode.UNKNOWN
  EKeyCode.LBUTTON
  EKeyCode.RBUTTON
  EKeyCode.CANCEL
  EKeyCode.MBUTTON
  EKeyCode.XBUTTON1
  EKeyCode.XBUTTON2
  EKeyCode.BACK
  EKeyCode.TAB
  EKeyCode.CLEAR
  EKeyCode.RETURN
  EKeyCode.SHIFT
  EKeyCode.CONTROL
  EKeyCode.MENU
  EKeyCode.PAUSE
  EKeyCode.CAPITAL
  EKeyCode.ESCAPE
  EKeyCode.CONVERT
  EKeyCode.NONCONVERT
  EKeyCode.ACCEPT
  EKeyCode.MODECHANGE
  EKeyCode.SPACE
  EKeyCode.PRIOR
  EKeyCode.NEXT
  EKeyCode.END_KEY
  EKeyCode.HOME
  EKeyCode.LEFT
  EKeyCode.UP
  EKeyCode.RIGHT
  EKeyCode.DOWN
  EKeyCode.SELECT
  EKeyCode.PRINT
  EKeyCode.EXECUTE
  EKeyCode.SNAPSHOT
  EKeyCode.INSERT
  EKeyCode.DELETE
  EKeyCode.HELP
  EKeyCode.NUM_0
  EKeyCode.NUM_1
  EKeyCode.NUM_2
  EKeyCode.NUM_3
  EKeyCode.NUM_4
  EKeyCode.NUM_5
  EKeyCode.NUM_6
  EKeyCode.NUM_7
  EKeyCode.NUM_8
  EKeyCode.NUM_9
  EKeyCode.A
  EKeyCode.B
  EKeyCode.C
  EKeyCode.D
  EKeyCode.E
  EKeyCode.F
  EKeyCode.G
  EKeyCode.H
  EKeyCode.I
  EKeyCode.J
  EKeyCode.K
  EKeyCode.L
  EKeyCode.M
  EKeyCode.N
  EKeyCode.O
  EKeyCode.P
  EKeyCode.Q
  EKeyCode.R
  EKeyCode.S
  EKeyCode.T
  EKeyCode.U
  EKeyCode.V
  EKeyCode.W
  EKeyCode.X
  EKeyCode.Y
  EKeyCode.Z
  EKeyCode.LWIN
  EKeyCode.RWIN
  EKeyCode.APPS
  EKeyCode.SLEEP
  EKeyCode.NUMPAD_0
  EKeyCode.NUMPAD_1
  EKeyCode.NUMPAD_2
  EKeyCode.NUMPAD_3
  EKeyCode.NUMPAD_4
  EKeyCode.NUMPAD_5
  EKeyCode.NUMPAD_6
  EKeyCode.NUMPAD_7
  EKeyCode.NUMPAD_8
  EKeyCode.NUMPAD_9
  EKeyCode.MULTIPLY
  EKeyCode.ADD
  EKeyCode.SEPARATOR
  EKeyCode.SUBTRACT
  EKeyCode.DECIMAL
  EKeyCode.DIVIDE
  EKeyCode.F1
  EKeyCode.F2
  EKeyCode.F3
  EKeyCode.F4
  EKeyCode.F5
  EKeyCode.F6
  EKeyCode.F7
  EKeyCode.F8
  EKeyCode.F9
  EKeyCode.F10
  EKeyCode.F11
  EKeyCode.F12
  EKeyCode.F13
  EKeyCode.F14
  EKeyCode.F15
  EKeyCode.F16
  EKeyCode.F17
  EKeyCode.F18
  EKeyCode.F19
  EKeyCode.F20
  EKeyCode.F21
  EKeyCode.F22
  EKeyCode.F23
  EKeyCode.F24
  EKeyCode.NUMLOCK
  EKeyCode.SCROLL
  EKeyCode.LSHIFT
  EKeyCode.RSHIFT
  EKeyCode.LCONTROL
  EKeyCode.RCONTROL
  EKeyCode.LMENU
  EKeyCode.RMENU
  EKeyCode.BROWSER_BACK
  EKeyCode.BROWSER_FORWARD
  EKeyCode.BROWSER_REFRESH
  EKeyCode.BROWSER_STOP
  EKeyCode.BROWSER_SEARCH
  EKeyCode.BROWSER_FAVORITES
  EKeyCode.BROWSER_HOME
  EKeyCode.VOLUME_MUTE
  EKeyCode.VOLUME_DOWN
  EKeyCode.VOLUME_UP
  EKeyCode.MEDIA_NEXT_TRACK
  EKeyCode.MEDIA_PREV_TRACK
  EKeyCode.MEDIA_STOP
  EKeyCode.MEDIA_PLAY_PAUSE
  EKeyCode.LAUNCH_MAIL
  EKeyCode.LAUNCH_MEDIA_SELECT
  EKeyCode.LAUNCH_APP1
  EKeyCode.LAUNCH_APP2
  EKeyCode.OEM_1
  EKeyCode.OEM_PLUS
  EKeyCode.OEM_COMMA
  EKeyCode.OEM_MINUS
  EKeyCode.OEM_PERIOD
  EKeyCode.OEM_2
  EKeyCode.OEM_3
  EKeyCode.OEM_4
  EKeyCode.OEM_5
  EKeyCode.OEM_6
  EKeyCode.OEM_7
  EKeyCode.OEM_8
  EKeyCode.OEM_102
  EKeyCode.PROCESSKEY
  EKeyCode.ATTN
  EKeyCode.CRSEL
  EKeyCode.EXSEL
  EKeyCode.EREOF
  EKeyCode.PLAY
  EKeyCode.ZOOM
```
*)

(*
TInput.MousePosition
--------------------
> function TInput.MousePosition: TPoint;
*)
procedure _LapeInput_MousePosition(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaInput(Params^[0])^.MousePosition();
end;

(*
TInput.MousePressed
-------------------
> function TInput.MousePressed(Button: EMouseButton): Boolean;
*)
procedure _LapeInput_MousePressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaInput(Params^[0])^.MousePressed(PMouseButton(Params^[1])^);
end;

(*
TInput.MouseTeleport
--------------------
> procedure TInput.MouseTeleport(P: TPoint);

Instantly moves the mouse to `P`
*)
procedure _LapeInput_MouseTeleport(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseTeleport(PPoint(Params^[1])^);
end;

(*
TInput.MouseClick
-----------------
> procedure TInput.MouseClick(Button: EMouseButton);
*)
procedure _LapeInput_MouseClick(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseClick(PMouseButton(Params^[1])^);
end;

(*
TInput.MouseDown
----------------
> procedure TInput.MouseDown(Button: EMouseButton);
*)
procedure _LapeInput_MouseDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseDown(PMouseButton(Params^[1])^);
end;

(*
TInput.MouseUp
--------------
> procedure TInput.MouseUp(Button: EMouseButton);
*)
procedure _LapeInput_MouseUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseUp(PMouseButton(Params^[1])^);
end;

(*
TInput.MouseScroll
------------------
> procedure TInput.MouseScroll(Scrolls: Integer);
*)
procedure _LapeInput_MouseScroll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseScroll(PInteger(Params^[1])^);
end;

(*
TInput.MouseMove
----------------
> procedure TInput.MouseMove(Dest: TPoint);

Move the mouse in a human-like way.

Speed, Gravity and Wind variables affects this.

Note:: The algorithm used is WindMouse. For more details see <https://ben.land/post/2021/04/25/windmouse-human-mouse-movement>
*)
procedure _LapeInput_MouseMove(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseMove(PPoint(Params^[1])^);
end;

(*
TInput.KeyDown
--------------
> procedure TInput.KeyDown(Key: EKeyCode);
*)
procedure _LapeInput_KeyDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeyDown(PKeyCode(Params^[1])^);
end;

(*
TInput.KeyUp
------------
> procedure TInput.KeyUp(Key: EKeyCode);
*)
procedure _LapeInput_KeyUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeyUp(PKeyCode(Params^[1])^);
end;

(*
TInput.KeyPress
---------------
> procedure TInput.KeyPress(Key: EKeyCode);
*)
procedure _LapeInput_KeyPress(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeyPress(PKeyCode(Params^[1])^);
end;

(*
TInput.KeyPressed
-----------------
> function TInput.KeyPressed(Key: EKeyCode): Boolean;
*)
procedure _LapeInput_KeyPressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaInput(Params^[0])^.KeyPressed(PKeyCode(Params^[1])^);
end;

(*
TInput.KeySend
--------------
> procedure TInput.KeySend(Text: String);
*)
procedure _LapeInput_KeySend(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeySend(PString(Params^[1])^);
end;

(*
TInput.CharToKeyCode
--------------------
> function TInput.CharToKeyCode(C: Char): EKeyCode;
*)
procedure _LapeInput_CharToKeyCode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKeyCode(Result)^ := PSimbaInput(Params^[0])^.CharToKeyCode(PChar(Params^[1])^);
end;

(*
TInput.AddOnMouseTeleport
-------------------------
> function TInput.AddOnMouseTeleport(Event: TMouseTeleportEvent): TMouseTeleportEvent;

Add a event to be called everytime the mouse is teleported.
*)
procedure _LapeInput_AddOnMouseTeleport(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaInput.TMouseTeleportEvent(Result^) := PSimbaInput(Params^[0])^.AddOnMouseTeleport(TSimbaInput.TMouseTeleportEvent(Params^[1]^));
end;

(*
TInput.AddOnMouseMoving
-----------------------
> function TInput.AddOnMouseMoving(Event: TMouseMovingEvent): TMouseTeleportEvent;

Add a event to be called while the mouse is moving so that the destination can be changed.
*)
procedure _LapeInput_AddOnMouseMoving(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaInput.TMouseMovingEvent(Result^) := PSimbaInput(Params^[0])^.AddOnMouseMoving(TSimbaInput.TMouseMovingEvent(Params^[1]^));
end;

(*
TInput.AddOnMouseDown
---------------------
> function TInput.AddOnMouseDown(Event: TMouseButtonEvent): TMouseButtonEvent;
*)
procedure _LapeInput_AddOnMouseDown(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaInput.TMouseButtonEvent(Result^) := PSimbaInput(Params^[0])^.AddOnMouseDown(TSimbaInput.TMouseButtonEvent(Params^[1]^));
end;

(*
TInput.AddOnMouseUp
-------------------
> function TInput.AddOnMouseUp(Event: TMouseButtonEvent): TMouseButtonEvent;
*)
procedure _LapeInput_AddOnMouseUp(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaInput.TMouseButtonEvent(Result^) := PSimbaInput(Params^[0])^.AddOnMouseUp(TSimbaInput.TMouseButtonEvent(Params^[1]^));
end;

(*
TInput.AddOnMouseClick
----------------------
> function TInput.AddOnMouseClick(Event: TMouseButtonEvent): TMouseButtonEvent;
*)
procedure _LapeInput_AddOnMouseClick(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaInput.TMouseButtonEvent(Result^) := PSimbaInput(Params^[0])^.AddOnMouseClick(TSimbaInput.TMouseButtonEvent(Params^[1]^));
end;

(*
TInput.RemoveOnMouseTeleport
----------------------------
> procedure TInput.RemoveOnMouseTeleport(Event: TMouseTeleportEvent);
*)
procedure _LapeInput_RemoveOnMouseTeleport(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.RemoveOnMouseTeleport(TSimbaInput.TMouseTeleportEvent(Params^[1]^));
end;

(*
TInput.RemoveOnMouseMoving
--------------------------
> procedure TInput.RemoveOnMouseMoving(Event: TMouseMovingEvent);
*)
procedure _LapeInput_RemoveOnMouseMoving(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.RemoveOnMouseMoving(TSimbaInput.TMouseMovingEvent(Params^[1]^));
end;

(*
TInput.RemoveOnMouseDown
------------------------
> procedure TInput.RemoveOnMouseDown(Event: TMouseButtonEvent);
*)
procedure _LapeInput_RemoveOnMouseDown(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.RemoveOnMouseDown(TSimbaInput.TMouseButtonEvent(Params^[1]^));
end;

(*
TInput.RemoveOnMouseUp
----------------------
> procedure TInput.RemoveOnMouseUp(Event: TMouseButtonEvent);
*)
procedure _LapeInput_RemoveOnMouseUp(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.RemoveOnMouseUp(TSimbaInput.TMouseButtonEvent(Params^[1]^));
end;

(*
TInput.RemoveOnMouseClick
-------------------------
> procedure TInput.RemoveOnMouseClick(Event: TMouseButtonEvent);
*)
procedure _LapeInput_RemoveOnMouseClick(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.RemoveOnMouseClick(TSimbaInput.TMouseButtonEvent(Params^[1]^));
end;

procedure ImportInput(Compiler: TSimbaScript_Compiler);

  procedure addInputMethod(Header: lpString; Addr: Pointer);
  begin
    Compiler.addGlobalFunc(Header, Addr);
    Compiler.addGlobalFuncOverride(Header, [
      'begin',
      '  if Self.Target.IsDefault() then',
      '  try',
      '    Self.Target := System.Target;',
      '    {$IFDECL Result}Result:={$ENDIF}inherited();',
      '  finally',
      '    Self.Target := [];',
      '  end else',
      '    {$IFDECL Result}Result:={$ENDIF}inherited();',
      'end;'
    ]);
  end;

begin
  with Compiler do
  begin
    ImportingSection := 'Input';

    // Must equal TSimbaInput
    addGlobalType([
      'record',
      '  const DEFAULT_KEY_PRESS_MIN = 30;',
      '  const DEFAULT_KEY_PRESS_MAX = 140;',
      '  const DEFAULT_CLICK_MIN     = 40;',
      '  const DEFAULT_CLICK_MAX     = 220;',
      '  const DEFAULT_MOUSE_TIMEOUT = 15000;',
      '  const DEFAULT_MOUSE_SPEED   = 12;',
      '  const DEFAULT_MOUSE_GRAVITY = 9;',
      '  const DEFAULT_MOUSE_WIND    = 6;',
      '',
      '  Target: TTarget;',
      '',
      '  TeleportEvents: array of TMethod;',
      '  MovingEvents: array of TMethod;',
      '  ClickEvents: array of TMethod;',
      '  DownEvents: array of TMethod;',
      '  UpEvents: array of TMethod;',
      '',
      '  KeyPressMin: Integer;',
      '  KeyPressMax: Integer;',
      '',
      '  MouseClickMin: Integer;',
      '  MouseClickMax: Integer;',
      '',
      '  MouseSpeed: Double;',
      '  MouseGravity: Double;',
      '  MouseWind: Double;',
      '  MouseAccuracy: Double;',
      '  MouseTimeout: Integer;',
      'end;'],
      'TInput'
    );

    with addGlobalVar('TInput', '[]', 'Input') do
    begin
      if (VarType.Size <> SizeOf(TSimbaInput)) then
        SimbaException('SizeOf(TInput) is wrong!');

      Used := duTrue;
    end;

    addGlobalType([
      'enum(',
      '  LEFT, RIGHT, MIDDLE, SCROLL_UP, SCROLL_DOWN',
      ');'],
      'EMouseButton'
    );
    addGlobalType([
      'enum(',
      '  UNKNOWN             = 0,',
      '  LBUTTON             = 1,',
      '  RBUTTON             = 2,',
      '  CANCEL              = 3,',
      '  MBUTTON             = 4,',
      '  XBUTTON1            = 5,',
      '  XBUTTON2            = 6,',
      '  BACK                = 8,',
      '  TAB                 = 9,',
      '  CLEAR               = 12,',
      '  RETURN              = 13,',
      '  SHIFT               = 16,',
      '  CONTROL             = 17,',
      '  MENU                = 18,',
      '  PAUSE               = 19,',
      '  CAPITAL             = 20,',
      '  ESCAPE              = 27,',
      '  CONVERT             = 28,',
      '  NONCONVERT          = 29,',
      '  ACCEPT              = 30,',
      '  MODECHANGE          = 31,',
      '  SPACE               = 32,',
      '  PRIOR               = 33,',
      '  NEXT                = 34,',
      '  END_KEY             = 35,',
      '  HOME                = 36,',
      '  LEFT                = 37,',
      '  UP                  = 38,',
      '  RIGHT               = 39,',
      '  DOWN                = 40,',
      '  SELECT              = 41,',
      '  PRINT               = 42,',
      '  EXECUTE             = 43,',
      '  SNAPSHOT            = 44,',
      '  INSERT              = 45,',
      '  DELETE              = 46,',
      '  HELP                = 47,',
      '  NUM_0               = 48,',
      '  NUM_1               = 49,',
      '  NUM_2               = 50,',
      '  NUM_3               = 51,',
      '  NUM_4               = 52,',
      '  NUM_5               = 53,',
      '  NUM_6               = 54,',
      '  NUM_7               = 55,',
      '  NUM_8               = 56,',
      '  NUM_9               = 57,',
      '  A                   = 65,',
      '  B                   = 66,',
      '  C                   = 67,',
      '  D                   = 68,',
      '  E                   = 69,',
      '  F                   = 70,',
      '  G                   = 71,',
      '  H                   = 72,',
      '  I                   = 73,',
      '  J                   = 74,',
      '  K                   = 75,',
      '  L                   = 76,',
      '  M                   = 77,',
      '  N                   = 78,',
      '  O                   = 79,',
      '  P                   = 80,',
      '  Q                   = 81,',
      '  R                   = 82,',
      '  S                   = 83,',
      '  T                   = 84,',
      '  U                   = 85,',
      '  V                   = 86,',
      '  W                   = 87,',
      '  X                   = 88,',
      '  Y                   = 89,',
      '  Z                   = 90,',
      '  LWIN                = 91,',
      '  RWIN                = 92,',
      '  APPS                = 93,',
      '  SLEEP               = 95,',
      '  NUMPAD_0            = 96,',
      '  NUMPAD_1            = 97,',
      '  NUMPAD_2            = 98,',
      '  NUMPAD_3            = 99,',
      '  NUMPAD_4            = 100,',
      '  NUMPAD_5            = 101,',
      '  NUMPAD_6            = 102,',
      '  NUMPAD_7            = 103,',
      '  NUMPAD_8            = 104,',
      '  NUMPAD_9            = 105,',
      '  MULTIPLY            = 106,',
      '  ADD                 = 107,',
      '  SEPARATOR           = 108,',
      '  SUBTRACT            = 109,',
      '  DECIMAL             = 110,',
      '  DIVIDE              = 111,',
      '  F1                  = 112,',
      '  F2                  = 113,',
      '  F3                  = 114,',
      '  F4                  = 115,',
      '  F5                  = 116,',
      '  F6                  = 117,',
      '  F7                  = 118,',
      '  F8                  = 119,',
      '  F9                  = 120,',
      '  F10                 = 121,',
      '  F11                 = 122,',
      '  F12                 = 123,',
      '  F13                 = 124,',
      '  F14                 = 125,',
      '  F15                 = 126,',
      '  F16                 = 127,',
      '  F17                 = 128,',
      '  F18                 = 129,',
      '  F19                 = 130,',
      '  F20                 = 131,',
      '  F21                 = 132,',
      '  F22                 = 133,',
      '  F23                 = 134,',
      '  F24                 = 135,',
      '  NUMLOCK             = 144,',
      '  SCROLL              = 145,',
      '  LSHIFT              = 160,',
      '  RSHIFT              = 161,',
      '  LCONTROL            = 162,',
      '  RCONTROL            = 163,',
      '  LMENU               = 164,',
      '  RMENU               = 165,',
      '  BROWSER_BACK        = 166,',
      '  BROWSER_FORWARD     = 167,',
      '  BROWSER_REFRESH     = 168,',
      '  BROWSER_STOP        = 169,',
      '  BROWSER_SEARCH      = 170,',
      '  BROWSER_FAVORITES   = 171,',
      '  BROWSER_HOME        = 172,',
      '  VOLUME_MUTE         = 173,',
      '  VOLUME_DOWN         = 174,',
      '  VOLUME_UP           = 175,',
      '  MEDIA_NEXT_TRACK    = 176,',
      '  MEDIA_PREV_TRACK    = 177,',
      '  MEDIA_STOP          = 178,',
      '  MEDIA_PLAY_PAUSE    = 179,',
      '  LAUNCH_MAIL         = 180,',
      '  LAUNCH_MEDIA_SELECT = 181,',
      '  LAUNCH_APP1         = 182,',
      '  LAUNCH_APP2         = 183,',
      '  OEM_1               = 186,',
      '  OEM_PLUS            = 187,',
      '  OEM_COMMA           = 188,',
      '  OEM_MINUS           = 189,',
      '  OEM_PERIOD          = 190,',
      '  OEM_2               = 191,',
      '  OEM_3               = 192,',
      '  OEM_4               = 219,',
      '  OEM_5               = 220,',
      '  OEM_6               = 221,',
      '  OEM_7               = 222,',
      '  OEM_8               = 223,',
      '  OEM_102             = 226,',
      '  PROCESSKEY          = 231,',
      '  ATTN                = 246,',
      '  CRSEL               = 247,',
      '  EXSEL               = 248,',
      '  EREOF               = 249,',
      '  PLAY                = 250,',
      '  ZOOM                = 251',
      ');'],
      'EKeyCode'
    );

    addGlobalType('procedure(var Input: TInput; P: TPoint) of object', 'TMouseTeleportEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(var Input: TInput; var X, Y, DestX, DestY: Double; var Stop: Boolean) of object', 'TMouseMovingEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(var Input: TInput; Button: EMouseButton) of object', 'TMouseButtonEvent', FFI_DEFAULT_ABI);

    // Dont need addInputMethod for these
    addGlobalFunc('function TInput.AddOnMouseTeleportEvent(Event: TMouseTeleportEvent): TMouseTeleportEvent;', @_LapeInput_AddOnMouseTeleport);
    addGlobalFunc('function TInput.AddOnMouseMovingEvent(Event: TMouseMovingEvent): TMouseTeleportEvent;', @_LapeInput_AddOnMouseMoving);
    addGlobalFunc('function TInput.AddOnMouseDown(Event: TMouseButtonEvent): TMouseButtonEvent;', @_LapeInput_AddOnMouseDown);
    addGlobalFunc('function TInput.AddOnMouseUp(Event: TMouseButtonEvent): TMouseButtonEvent;', @_LapeInput_AddOnMouseUp);
    addGlobalFunc('function TInput.AddOnMouseClick(Event: TMouseButtonEvent): TMouseButtonEvent;', @_LapeInput_AddOnMouseClick);

    addGlobalFunc('function TInput.RemoveOnMouseTeleportEvent(Event: TMouseTeleportEvent): Boolean;', @_LapeInput_RemoveOnMouseTeleport);
    addGlobalFunc('function TInput.RemoveOnMouseMovingEvent(Event: TMouseMovingEvent): Boolean;', @_LapeInput_RemoveOnMouseMoving);
    addGlobalFunc('function TInput.RemoveOnMouseDown(Event: TMouseButtonEvent): Boolean;', @_LapeInput_RemoveOnMouseDown);
    addGlobalFunc('function TInput.RemoveOnMouseUp(Event: TMouseButtonEvent): Boolean;', @_LapeInput_RemoveOnMouseUp);
    addGlobalFunc('function TInput.RemoveOnMouseClick(Event: TMouseButtonEvent): Boolean;', @_LapeInput_RemoveOnMouseClick);

    addInputMethod('function TInput.MousePressed(Button: EMouseButton): Boolean', @_LapeInput_MousePressed);
    addInputMethod('function TInput.MousePosition: TPoint', @_LapeInput_MousePosition);
    addInputMethod('procedure TInput.MouseTeleport(P: TPoint)', @_LapeInput_MouseTeleport);
    addInputMethod('procedure TInput.MouseClick(Button: EMouseButton)', @_LapeInput_MouseClick);
    addInputMethod('procedure TInput.MouseDown(Button: EMouseButton)', @_LapeInput_MouseDown);
    addInputMethod('procedure TInput.MouseUp(Button: EMouseButton)', @_LapeInput_MouseUp);
    addInputMethod('procedure TInput.MouseScroll(Scrolls: Integer)', @_LapeInput_MouseScroll);
    addInputMethod('procedure TInput.MouseMove(Dest: TPoint);', @_LapeInput_MouseMove);

    addInputMethod('procedure TInput.KeySend(Text: String)', @_LapeInput_KeySend);
    addInputMethod('procedure TInput.KeyPress(Key: EKeyCode)', @_LapeInput_KeyPress);
    addInputMethod('procedure TInput.KeyDown(Key: EKeyCode)', @_LapeInput_KeyDown);
    addInputMethod('procedure TInput.KeyUp(Key: EKeyCode)', @_LapeInput_KeyUp);
    addInputMethod('function TInput.KeyPressed(Key: EKeyCode): Boolean', @_LapeInput_KeyPressed);

    addInputMethod('function TInput.CharToKeyCode(C: Char): EKeyCode', @_LapeInput_CharToKeyCode);

    ImportingSection := '';
  end;
end;

end.
