unit simba.import_input;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

implementation

uses
  lptypes,
  ffi,
  simba.script_compiler, simba.mufasatypes, simba.input, simba.target;

(*
Input
=====
Input (Mouse & Keyboard) related methods.

- If the `TSimbaInput.Target` field is set, the global "Target" variable will be used. Which by default is set to Simba's target selection.
- There is a pre-defined variable `Input` to use.

Example:

```
  Input.MouseMove([200,200]); // Move the mouse to x: 200, y: 200
  Input.MouseClick(MouseButton.LEFT); // And left click
```

----

| Key codes and mouse buttons are both **scoped** enums.
| For scoped a enum you **must** include the type of the enum.

Example:

```
  MouseButton.LEFT
  KeyCode.A
```

Available Mouse Buttons:

```
  MouseButton.LEFT
  MouseButton.RIGHT
  MouseButton.MIDDLE
  MouseButton.SCROLL_UP
  MouseButton.SCROLL_DOWN
```

Available Key Codes:

```
  KeyCode.UNKNOWN
  KeyCode.LBUTTON
  KeyCode.RBUTTON
  KeyCode.CANCEL
  KeyCode.MBUTTON
  KeyCode.XBUTTON1
  KeyCode.XBUTTON2
  KeyCode.BACK
  KeyCode.TAB
  KeyCode.CLEAR
  KeyCode.RETURN
  KeyCode.SHIFT
  KeyCode.CONTROL
  KeyCode.MENU
  KeyCode.PAUSE
  KeyCode.CAPITAL
  KeyCode.ESCAPE
  KeyCode.CONVERT
  KeyCode.NONCONVERT
  KeyCode.ACCEPT
  KeyCode.MODECHANGE
  KeyCode.SPACE
  KeyCode.PRIOR
  KeyCode.NEXT
  KeyCode.END_KEY
  KeyCode.HOME
  KeyCode.LEFT
  KeyCode.UP
  KeyCode.RIGHT
  KeyCode.DOWN
  KeyCode.SELECT
  KeyCode.PRINT
  KeyCode.EXECUTE
  KeyCode.SNAPSHOT
  KeyCode.INSERT
  KeyCode.DELETE
  KeyCode.HELP
  KeyCode.NUM_0
  KeyCode.NUM_1
  KeyCode.NUM_2
  KeyCode.NUM_3
  KeyCode.NUM_4
  KeyCode.NUM_5
  KeyCode.NUM_6
  KeyCode.NUM_7
  KeyCode.NUM_8
  KeyCode.NUM_9
  KeyCode.A
  KeyCode.B
  KeyCode.C
  KeyCode.D
  KeyCode.E
  KeyCode.F
  KeyCode.G
  KeyCode.H
  KeyCode.I
  KeyCode.J
  KeyCode.K
  KeyCode.L
  KeyCode.M
  KeyCode.N
  KeyCode.O
  KeyCode.P
  KeyCode.Q
  KeyCode.R
  KeyCode.S
  KeyCode.T
  KeyCode.U
  KeyCode.V
  KeyCode.W
  KeyCode.X
  KeyCode.Y
  KeyCode.Z
  KeyCode.LWIN
  KeyCode.RWIN
  KeyCode.APPS
  KeyCode.SLEEP
  KeyCode.NUMPAD_0
  KeyCode.NUMPAD_1
  KeyCode.NUMPAD_2
  KeyCode.NUMPAD_3
  KeyCode.NUMPAD_4
  KeyCode.NUMPAD_5
  KeyCode.NUMPAD_6
  KeyCode.NUMPAD_7
  KeyCode.NUMPAD_8
  KeyCode.NUMPAD_9
  KeyCode.MULTIPLY
  KeyCode.ADD
  KeyCode.SEPARATOR
  KeyCode.SUBTRACT
  KeyCode.DECIMAL
  KeyCode.DIVIDE
  KeyCode.F1
  KeyCode.F2
  KeyCode.F3
  KeyCode.F4
  KeyCode.F5
  KeyCode.F6
  KeyCode.F7
  KeyCode.F8
  KeyCode.F9
  KeyCode.F10
  KeyCode.F11
  KeyCode.F12
  KeyCode.F13
  KeyCode.F14
  KeyCode.F15
  KeyCode.F16
  KeyCode.F17
  KeyCode.F18
  KeyCode.F19
  KeyCode.F20
  KeyCode.F21
  KeyCode.F22
  KeyCode.F23
  KeyCode.F24
  KeyCode.NUMLOCK
  KeyCode.SCROLL
  KeyCode.LSHIFT
  KeyCode.RSHIFT
  KeyCode.LCONTROL
  KeyCode.RCONTROL
  KeyCode.LMENU
  KeyCode.RMENU
  KeyCode.BROWSER_BACK
  KeyCode.BROWSER_FORWARD
  KeyCode.BROWSER_REFRESH
  KeyCode.BROWSER_STOP
  KeyCode.BROWSER_SEARCH
  KeyCode.BROWSER_FAVORITES
  KeyCode.BROWSER_HOME
  KeyCode.VOLUME_MUTE
  KeyCode.VOLUME_DOWN
  KeyCode.VOLUME_UP
  KeyCode.MEDIA_NEXT_TRACK
  KeyCode.MEDIA_PREV_TRACK
  KeyCode.MEDIA_STOP
  KeyCode.MEDIA_PLAY_PAUSE
  KeyCode.LAUNCH_MAIL
  KeyCode.LAUNCH_MEDIA_SELECT
  KeyCode.LAUNCH_APP1
  KeyCode.LAUNCH_APP2
  KeyCode.OEM_1
  KeyCode.OEM_PLUS
  KeyCode.OEM_COMMA
  KeyCode.OEM_MINUS
  KeyCode.OEM_PERIOD
  KeyCode.OEM_2
  KeyCode.OEM_3
  KeyCode.OEM_4
  KeyCode.OEM_5
  KeyCode.OEM_6
  KeyCode.OEM_7
  KeyCode.OEM_8
  KeyCode.OEM_102
  KeyCode.PROCESSKEY
  KeyCode.ATTN
  KeyCode.CRSEL
  KeyCode.EXSEL
  KeyCode.EREOF
  KeyCode.PLAY
  KeyCode.ZOOM
```
*)

(*
TSimbaInput.MousePosition
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaInput.MousePosition: TPoint;
*)
procedure _LapeSimbaInput_MousePosition(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaInput(Params^[0])^.MousePosition();
end;

(*
TSimbaInput.MousePressed
~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaInput.MousePressed(Button: MouseButton): Boolean;
*)
procedure _LapeSimbaInput_MousePressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaInput(Params^[0])^.MousePressed(PMouseButton(Params^[1])^);
end;

(*
TSimbaInput.MouseTeleport
~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.MouseTeleport(P: TPoint);

Instantly moves the mouse to `P`
*)
procedure _LapeSimbaInput_MouseTeleport(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseTeleport(PPoint(Params^[1])^);
end;

(*
TSimbaInput.MouseClick
~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.MouseClick(Button: MouseButton);
*)
procedure _LapeSimbaInput_MouseClick(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseClick(PMouseButton(Params^[1])^);
end;

(*
TSimbaInput.MouseDown
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.MouseDown(Button: MouseButton);
*)
procedure _LapeSimbaInput_MouseDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseDown(PMouseButton(Params^[1])^);
end;

(*
TSimbaInput.MouseUp
~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.MouseUp(Button: MouseButton);
*)
procedure _LapeSimbaInput_MouseUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseUp(PMouseButton(Params^[1])^);
end;

(*
TSimbaInput.MouseScroll
~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.MouseScroll(Scrolls: Integer);
*)
procedure _LapeSimbaInput_MouseScroll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseScroll(PInteger(Params^[1])^);
end;

(*
TSimbaInput.MouseMove
~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.MouseMove(Dest: TPoint);

Move the mouse in a human-like way.

Speed, Gravity and Wind variables affects this.

Note: The algorithm used is WindMouse. For more details see <https://ben.land/post/2021/04/25/windmouse-human-mouse-movement>
*)
procedure _LapeSimbaInput_MouseMove(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.MouseMove(PPoint(Params^[1])^);
end;

(*
TSimbaInput.KeyDown
~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.KeyDown(Key: KeyCode);
*)
procedure _LapeSimbaInput_KeyDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeyDown(PKeyCode(Params^[1])^);
end;

(*
TSimbaInput.KeyUp
~~~~~~~~~~~~~~~~~
procedure TSimbaInput.KeyUp(Key: KeyCode);
*)
procedure _LapeSimbaInput_KeyUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeyUp(PKeyCode(Params^[1])^);
end;

(*
TSimbaInput.KeyPress
~~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.KeyPress(Key: KeyCode);
*)
procedure _LapeSimbaInput_KeyPress(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeyPress(PKeyCode(Params^[1])^);
end;

(*
TSimbaInput.KeyPressed
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaInput.KeyPressed(Key: KeyCode): Boolean;
*)
procedure _LapeSimbaInput_KeyPressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaInput(Params^[0])^.KeyPressed(PKeyCode(Params^[1])^);
end;

(*
TSimbaInput.KeySend
~~~~~~~~~~~~~~~~~~~
procedure TSimbaInput.KeySend(Text: String);
*)
procedure _LapeSimbaInput_KeySend(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaInput(Params^[0])^.KeySend(PString(Params^[1])^);
end;

(*
TSimbaInput.CharToKeyCode
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaInput.CharToKeyCode(C: Char): KeyCode;
*)
procedure _LapeSimbaInput_CharToKeyCode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKeyCode(Result)^ := PSimbaInput(Params^[0])^.CharToKeyCode(PChar(Params^[1])^);
end;

procedure ImportInput(Compiler: TSimbaScript_Compiler);

  // If target field is default value, use global target variable.
  function GetOverrideBody(Name, Params: lpString; isFunction: Boolean): String;
  begin
    Result := 'begin'                                                                                                 + LineEnding +
              '  if Self.Target.IsDefault() then'                                                                     + LineEnding +
              '    ' + BoolToStr(isFunction, 'Result := ', '') + 'Self.GetGlobalInput().' + Name + '(' + Params + ')' + LineEnding +
              '  else'                                                                                                + LineEnding +
              '    ' + BoolToStr(isFunction, 'Result := ', '') + 'inherited();'                                       + LineEnding +
              'end;';
  end;

  procedure addInputMethod(Header: lpString; Addr: Pointer);
  begin
    Compiler.addGlobalFunc(Header, Addr);
    Compiler.addOverrideMethod(Header, @GetOverrideBody);
  end;

begin
  with Compiler do
  begin
    ImportingSection := 'Input';

    addGlobalType('procedure(Sender: Pointer; X, Y: Integer)', 'TMouseTeleportEvent', FFI_DEFAULT_ABI);
    addGlobalType('function(Sender: Pointer; var X, Y: Double): Boolean', 'TMouseMovingEvent', FFI_DEFAULT_ABI);

    addGlobalType([
      'packed record',
      '  Target: TSimbaTarget;',
      '',
      '  MinPressTime: Integer;',
      '  MaxPressTime: Integer;',
      '',
      '  MinClickTime: Integer;',
      '  MaxClickTime: Integer;',
      '',
      '  Speed: Integer;',
      '  Gravity: Double;',
      '  Wind: Double;',
      '',
      '  OnTeleport: TMouseTeleportEvent;',
      '  OnMoving: TMouseMovingEvent;',
      'end;'],
      'TSimbaInput'
    );
    if (getGlobalType('TSimbaInput').Size <> SizeOf(TSimbaInput)) then
      raise Exception.Create('SizeOf(TSimbaInput) is wrong!');

    with addGlobalVar('TSimbaInput', '[]', 'Input') do
      Used := duTrue;

    addGlobalType([
      'enum(',
      '  LEFT, RIGHT, MIDDLE, SCROLL_UP, SCROLL_DOWN',
      ');'],
      'MouseButton'
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
      'KeyCode'
    );

    addGlobalFunc(
      'function TSimbaInput.GetGlobalInput: TSimbaInput;', [
      'begin',
      '  Result := Self;',
      '  Result.Target := System.Target;',
      'end;'
    ]);

    addInputMethod('function TSimbaInput.MousePressed(Button: MouseButton): Boolean', @_LapeSimbaInput_MousePressed);
    addInputMethod('function TSimbaInput.MousePosition: TPoint', @_LapeSimbaInput_MousePosition);
    addInputMethod('procedure TSimbaInput.MouseTeleport(P: TPoint)', @_LapeSimbaInput_MouseTeleport);
    addInputMethod('procedure TSimbaInput.MouseClick(Button: MouseButton)', @_LapeSimbaInput_MouseClick);
    addInputMethod('procedure TSimbaInput.MouseDown(Button: MouseButton)', @_LapeSimbaInput_MouseDown);
    addInputMethod('procedure TSimbaInput.MouseUp(Button: MouseButton)', @_LapeSimbaInput_MouseUp);
    addInputMethod('procedure TSimbaInput.MouseScroll(Scrolls: Integer)', @_LapeSimbaInput_MouseScroll);
    addInputMethod('procedure TSimbaInput.MouseMove(Dest: TPoint);', @_LapeSimbaInput_MouseMove);

    addInputMethod('procedure TSimbaInput.KeySend(Text: String)', @_LapeSimbaInput_KeySend);
    addInputMethod('procedure TSimbaInput.KeyPress(Key: KeyCode)', @_LapeSimbaInput_KeyPress);
    addInputMethod('procedure TSimbaInput.KeyDown(Key: KeyCode)', @_LapeSimbaInput_KeyDown);
    addInputMethod('procedure TSimbaInput.KeyUp(Key: KeyCode)', @_LapeSimbaInput_KeyUp);
    addInputMethod('function TSimbaInput.KeyPressed(Key: KeyCode): Boolean', @_LapeSimbaInput_KeyPressed);

    addInputMethod('function TSimbaInput.CharToKeyCode(C: Char): KeyCode', @_LapeSimbaInput_CharToKeyCode);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportInput);

end.

