unit simba.import_target;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);

implementation

uses
  TypInfo,
  lptypes, lpvartypes, ffi,
  simba.colormath, simba.dtm, simba.misc,
  simba.image, simba.target, simba.externalcanvas, simba.finder_image, simba.finder_color;

type
  PMouseButton = ^EMouseButton;
  PKeyCode = ^EKeyCode;

(*
Target
======
Target related methods.
*)

(*
TTarget.SetDesktop
------------------
```
procedure TTarget.SetDesktop;
```

Sets the desktop as the target.
*)
procedure _LapeTarget_SetDesktop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetDesktop();
end;

(*
TTarget.SetImage
----------------
```
procedure TTarget.SetImage(TImage: TImage);
```

Sets the TSimbaImage as a target.

```{note}
Ownership of the TSimbaImage is **not** taken. Make sure you do not free the image while using this target.
```
*)
procedure _LapeTarget_SetImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetImage(PSimbaImage(Params^[1])^);
end;

(*
TTarget.SetWindow
-----------------
```
procedure TTarget.SetWindow(Window: TWindowHandle);
```

Sets a window handle as a target.
*)
procedure _LapeTarget_SetWindow(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetWindow(PWindowHandle(Params^[1])^);
end;

(*
TTarget.SetEIOS
---------------
```
procedure TTarget.SetEIOS(Plugin, Args: String);
```

Sets a plugin (via EIOS API) as the target.
*)
procedure _LapeTarget_SetEIOS(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetEIOS(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TTarget.SetPlugin
-----------------
```
procedure TTarget.SetPlugin(FileName, Args: String);
```

Sets a plugin (via SimbaPluginTarget API) as the target.
For more details about the API see <http://villavu.github.io/Simba/tutorials/plugins/plugin-target.html>
*)
procedure _LapeTarget_SetPlugin1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetPlugin(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TTarget.SetPlugin
-----------------
```
procedure TTarget.SetPlugin(FileName, Args: String; out DebugImage: TExternalCanvas);
```

Overloaded version that returns a "external canvas" to draw on.
*)
procedure _LapeTarget_SetPlugin2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.SetPlugin(PString(Params^[1])^, PString(Params^[2])^, TSimbaExternalCanvas(Params^[3]^));
end;

(*
TTarget.RemoveTargetInvalidEvent
--------------------------------
```
procedure TTarget.RemoveTargetInvalidEvent(Event: TTargetEvent);
```
*)
procedure _Lape_Target_RemoveTargetInvalidEvent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveTargetInvalidEvent(TTargetEvent(Params^[1]^));
end;

(*
TTarget.AddTargetChangeEvent
----------------------------
```
function TTarget.AddTargetChangeEvent(Event: TTargetEvent): TTargetEvent;
```
*)
procedure _Lape_Target_AddTargetChangeEvent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TTargetEvent(Result^) := PSimbaTarget(Params^[0])^.AddTargetChangeEvent(TTargetEvent(Params^[1]^));
end;

(*
TTarget.RemoveTargetChangeEvent
-------------------------------
```
procedure TTarget.RemoveTargetChangeEvent(Event: TTargetEvent);
```
*)
procedure _Lape_Target_RemoveTargetChangeEvent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveTargetChangeEvent(TTargetEvent(Params^[1]^));
end;

(*
TTarget.FreezeImage
-------------------
```
procedure TTarget.FreezeImage(ABounds: TBox);
```
*)
procedure _LapeTarget_FreezeImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.FreezeImage(PBox(Params^[1])^);
end;

(*
TTarget.UnFreezeImage
---------------------
```
procedure TTarget.UnFreezeImage;
```
*)
procedure _LapeTarget_UnFreezeImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.UnFreezeImage();
end;

(*
TTarget.IsImageFrozen
---------------------
```
function TTarget.IsImageFrozen: Boolean;
```
*)
procedure _LapeTarget_IsImageFrozen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsImageFrozen();
end;

(*
TTarget.GetImage
----------------
```
function TTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TImage;
```
*)
procedure _LapeTarget_GetImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaTarget(Params^[0])^.GetImage(PBox(Params^[1])^);
end;

(*
TTarget.IsValid
---------------
```
function TTarget.IsValid: Boolean;
```
*)
procedure _LapeTarget_IsValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsValid();
end;

(*
TTarget.IsFocused
-----------------
```
function TTarget.IsFocused: Boolean;
```
*)
procedure _LapeTarget_IsFocused(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.IsFocused();
end;

(*
TTarget.Focus
-------------
```
function TTarget.Focus: Boolean;
```
*)
procedure _LapeTarget_Focus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.Focus();
end;

(*
TTarget.ToString
----------------
```
function TTarget.ToString: String;
```
*)
procedure _LapeTarget_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaTarget(Params^[0])^.ToString();
end;

(*
TTarget.TargetKind
------------------
```
property TTarget.TargetKind: ETargetKind;
```
*)
procedure _LapeTarget_GetTargetKind(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  ESimbaTargetKind(Result^) := PSimbaTarget(Params^[0])^.TargetKind;
end;

(*
TTarget.TargetWindow
--------------------
```
property TTarget.TargetWindow: TWindowHandle;
```
*)
procedure _LapeTarget_GetTargetWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWindowHandle(Result)^ := PSimbaTarget(Params^[0])^.TargetWindow;
end;

(*
TTarget.TargetImage
-------------------
```
property TTarget.TargetImage: TImage;
```
*)
procedure _LapeTarget_GetTargetImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaTarget(Params^[0])^.TargetImage;
end;

(*
TTarget.CustomClientArea
------------------------
```
property TTarget.CustomClientArea: TBox;
```
```
property TTarget.CustomClientArea(Value: TBox);
```

Set a custom client area within the bounds of the target.

```
Target.CustomClientArea := [100,100,600,600];
Input.MouseMove([1,1]); // Will move the mouse to [101,101] on the "real" bounds
```
*)
procedure _LapeTarget_SetCustomClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.CustomClientArea := PBox(Params^[1])^;
end;

procedure _LapeTarget_GetCustomClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PSimbaTarget(Params^[0])^.CustomClientArea;
end;

(*
TTarget.AutoFocus
-----------------
```
property TTarget.AutoFocus(Value: Boolean);
```
```
property TTarget.AutoFocus: Boolean;
```
*)
procedure _LapeTarget_SetAutoFocus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.AutoFocus := PBoolean(Params^[1])^;
end;

procedure _LapeTarget_GetAutoFocus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.AutoFocus;
end;

(*
TTarget.Bounds
--------------
```
property TTarget.Bounds: TBox;
```
*)
procedure _LapeTarget_Bounds(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBox(Result)^ := PSimbaTarget(Params^[0])^.Bounds;
end;

(*
TTarget.Width
-------------
```
property TTarget.Width: Integer;
```
*)
procedure _LapeTarget_Width(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.Width;
end;

(*
TTarget.Height
--------------
```
property TTarget.Height: Integer;
```
*)
procedure _LapeTarget_Height(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.Height;
end;

(*
TTarget.Size
------------
```
property TTarget.Size: TSize;
```

Returns the targets dimensions as in a TSize.
*)
procedure _LapeTarget_Size(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSize(Result)^ := PSimbaTarget(Params^[0])^.Size;
end;

(*
TTarget.AddMouseEvent
---------------------
```
function TTarget.AddMouseEvent(Event: TMouseButtonEvent): TMouseButtonEvent;
```
```
function TTarget.AddMouseEvent(Event: TMouseTeleportEvent): TMouseTeleportEvent;
```
```
function TTarget.AddMouseEvent(Event: TMouseMovingEvent): TMouseMovingEvent;
```
*)
procedure _LapeTarget_AddMouseEvent1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TMouseTeleportEvent(Result^) := PSimbaTarget(Params^[0])^.AddMouseEvent(TMouseTeleportEvent(Params^[1]^));
end;

procedure _LapeTarget_AddMouseEvent2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TMouseButtonEvent(Result^) := PSimbaTarget(Params^[0])^.AddMouseEvent(TMouseButtonEvent(Params^[1]^));
end;

procedure _LapeTarget_AddMouseEvent3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TMouseMovingEvent(Result^) := PSimbaTarget(Params^[0])^.AddMouseEvent(TMouseMovingEvent(Params^[1]^));
end;

(*
TTarget.RemoveMouseEvent
------------------------
```
procedure TTarget.RemoveMouseEvent(Event: TMouseButtonEvent); overload;
```
```
procedure TTarget.RemoveMouseEvent(Event: TMouseTeleportEvent); overload;
```
```
procedure TTarget.RemoveMouseEvent(Event: TMouseMovingEvent); overload;
```
*)
procedure _LapeTarget_RemoveMouseEvent1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveMouseEvent(TMouseTeleportEvent(Params^[1]^));
end;

procedure _LapeTarget_RemoveMouseEvent2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveMouseEvent(TMouseButtonEvent(Params^[1]^));
end;

procedure _LapeTarget_RemoveMouseEvent3(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.RemoveMouseEvent(TMouseMovingEvent(Params^[1]^));
end;

(*
TTarget.MouseTeleport
---------------------
```
procedure TTarget.MouseTeleport(P: TPoint);
```

Instantly moves the mouse to `P`
*)
procedure _LapeTarget_MouseTeleport(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseTeleport(PPoint(Params^[1])^);
end;

(*
TTarget.MouseClick
------------------
```
procedure TTarget.MouseClick(Button: EMouseButton);
```
*)
procedure _LapeTarget_MouseClick(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseClick(PMouseButton(Params^[1])^);
end;

(*
TTarget.MouseDown
-----------------
```
procedure TTarget.MouseDown(Button: EMouseButton);
```
*)
procedure _LapeTarget_MouseDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseDown(PMouseButton(Params^[1])^);
end;

(*
TTarget.MouseUp
---------------
```
procedure TTarget.MouseUp(Button: EMouseButton);
```
*)
procedure _LapeTarget_MouseUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseUp(PMouseButton(Params^[1])^);
end;

(*
TTarget.MouseScroll
-------------------
```
procedure TTarget.MouseScroll(Scrolls: Integer);
```
*)
procedure _LapeTarget_MouseScroll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseScroll(PInteger(Params^[1])^);
end;

(*
TTarget.MousePressed
--------------------
```
function TTarget.MousePressed(Button: EMouseButton): Boolean;
```
*)
procedure _LapeTarget_MousePressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.MousePressed(PMouseButton(Params^[1])^);
end;

(*
TTarget.MouseMove
-----------------
```
procedure TTarget.MouseMove(Dest: TPoint);
```

Move the mouse in a human-like way.

Speed, Gravity and Wind variables affects this.

```{note}
The algorithm used is WindMouse. For more details see <https://ben.land/post/2021/04/25/windmouse-human-mouse-movement>
```
*)
procedure _LapeTarget_MouseMove1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseMove(PPoint(Params^[1])^);
end;

(*
TTarget.MouseMove
-----------------
```
procedure MouseMove(Box: TBox; ForcedMove: Boolean = False); overload;
```
```
procedure MouseMove(Quad: TQuad; ForcedMove: Boolean = False); overload;
```

`MouseMove` overloads.
Use `ForcedMove` to determine if the mouse will still move if it's already inside the box/quad.
*)
procedure _LapeTarget_MouseMove2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseMove(PBox(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeTarget_MouseMove3(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseMove(PQuad(Params^[1])^, PBoolean(Params^[2])^);
end;

(*
TTarget.MouseXY
---------------
```
property TTarget.MouseXY: TPoint;
```
```
property TTarget.MouseXY(Value: TPoint);
```
*)
procedure _LapeTarget_MouseXY_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaTarget(Params^[0])^.MouseXY;
end;

procedure _LapeTarget_MouseXY_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseXY := PPoint(Params^[1])^;
end;

(*
TTarget.MouseX
--------------
```
property TTarget.MouseX: Integer;
```
```
property TTarget.MouseX(Value: Integer);
```
*)
procedure _LapeTarget_MouseX_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.MouseX;
end;

procedure _LapeTarget_MouseX_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseX := PInteger(Params^[1])^;
end;

(*
TTarget.MouseY
--------------
```
property TTarget.MouseY: Integer;
```
```
property TTarget.MouseY(Value: Integer);
```
*)
procedure _LapeTarget_MouseY_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.MouseY;
end;

procedure _LapeTarget_MouseY_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.MouseY := PInteger(Params^[1])^;
end;

(*
TTarget.AddTargetInvalidEvent
-----------------------------
```
function TTarget.AddTargetInvalidEvent(Event: TTargetEvent): TTargetEvent;
```
*)
procedure _Lape_Target_AddTargetInvalidEvent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TTargetEvent(Result^) := PSimbaTarget(Params^[0])^.AddTargetInvalidEvent(TTargetEvent(Params^[1]^));
end;

(*
TTarget.KeySend
---------------
```
procedure TTarget.KeySend(Text: String);
```
*)
procedure _LapeTarget_KeySend(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.KeySend(PString(Params^[1])^);
end;

(*
TTarget.KeyDown
---------------
```
procedure TTarget.KeyDown(Key: EKeyCode);
```
*)
procedure _LapeTarget_KeyDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.KeyDown(PKeyCode(Params^[1])^);
end;

(*
TTarget.KeyUp
-------------
```
procedure TTarget.KeyUp(Key: EKeyCode);
```
*)
procedure _LapeTarget_KeyUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.KeyUp(PKeyCode(Params^[1])^);
end;

(*
TTarget.KeyPress
----------------
```
procedure TTarget.KeyPress(Key: EKeyCode);
```
*)
procedure _LapeTarget_KeyPress(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaTarget(Params^[0])^.KeyPress(PKeyCode(Params^[1])^);
end;

(*
TTarget.KeyPressed
------------------
```
function TTarget.KeyPressed(Key: EKeyCode): Boolean;
```
*)
procedure _LapeTarget_KeyPressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.KeyPressed(PKeyCode(Params^[1])^);
end;

(*
TTarget.KeyCodeFromChar
-----------------------
```
function TTarget.KeyCodeFromChar(C: Char): EKeyCode;
```
*)
procedure _LapeTarget_KeyCodeFromChar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PKeyCode(Result)^ := PSimbaTarget(Params^[0])^.KeyCodeFromChar(PChar(Params^[1])^);
end;

(*
TTarget.MatchColor
------------------
```
function TTarget.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox): TSingleMatrix;
```
*)
procedure _LapeTarget_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaTarget(Params^[0])^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TTarget.FindColor
-----------------
```
function TTarget.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.FindColor
-----------------
```
function TTarget.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TTarget.FindColor
-----------------
```
function TTarget.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TTarget.CountColor
------------------
```
function TTarget.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeTarget_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.CountColor
------------------
```
function TTarget.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeTarget_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TTarget.CountColor
------------------
```
function TTarget.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeTarget_CountColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.CountColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TTarget.HasColor
----------------
```
function TTarget.HasColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
```
*)
procedure _LapeTarget_HasColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.HasColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TTarget.HasColor
----------------
```
function TTarget.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
```
*)
procedure _LapeTarget_HasColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.HasColor(PColor(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TTarget.HasColor
----------------
```
function TTarget.HasColor(Color: TColorTolerance; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload;
```
*)
procedure _LapeTarget_HasColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.HasColor(PColorTolerance(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.GetColor
----------------
```
function TTarget.GetColor(P: TPoint): TColor;
```
*)
procedure _LapeTarget_GetColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaTarget(Params^[0])^.GetColor(PPoint(Params^[1])^);
end;

(*
TTarget.GetColors
-----------------
```
function TTarget.GetColors(Points: TPointArray): TColorArray;
```
*)
procedure _LapeTarget_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaTarget(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

(*
TTarget.GetColorsMatrix
-----------------------
```
function TTarget.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix;
```
*)
procedure _LapeTarget_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaTarget(Params^[0])^.GetColorsMatrix(PBox(Params^[1])^);
end;

(*
TTarget.HasImage
----------------
```
function TTarget.HasImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox  = [-1,-1,-1,-1]): Boolean;
```
*)
procedure _LapeTarget_HasImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.HasImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TTarget.HasImage
----------------
```
function TTarget.HasImage(Image: TSimbaImage; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox  = [-1,-1,-1,-1]): Boolean;
```
*)
procedure _LapeTarget_HasImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.HasImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TTarget.FindImage
-----------------
```
function TTarget.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```
*)
procedure _LapeTarget_FindImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaTarget(Params^[0])^.FindImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.FindImage
-----------------
```
function TTarget.FindImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```
*)
procedure _LapeTarget_FindImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaTarget(Params^[0])^.FindImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TTarget.FindImageEx
-------------------
```
function TTarget.FindImageEx(Image: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindImageEx1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindImageEx(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TTarget.FindImageEx
-------------------
```
function TTarget.FindImageEx(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindImageEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindImageEx(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TTarget.FindTemplate
--------------------
```
function TTarget.FindTemplate(Image: TImage; out Match: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```
*)
procedure _LapeTarget_FindTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaTarget(Params^[0])^.FindTemplate(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.HasTemplate
--------------------
```
function TTarget.HasTemplate(Image: TImage; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
```
*)
procedure _LapeTarget_HasTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaTarget(Params^[0])^.HasTemplate(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.FindDTM
---------------
```
function TTarget.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```
*)
procedure _LapeTarget_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaTarget(Params^[0])^.FindDTM(PDTM(Params^[1])^, PBox(Params^[2])^);
end;

(*
TTarget.FindDTMEx
-----------------
```
function TTarget.FindDTMEx(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindDTMEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindDTMEx(PDTM(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.FindDTMRotated
----------------------
```
function TTarget.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```
*)
procedure _LapeTarget_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaTarget(Params^[0])^.FindDTMRotated(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PBox(Params^[6])^);
end;

(*
TTarget.FindDTMRotatedEx
------------------------
```
function TTarget.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeTarget_FindDTMRotatedEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindDTMRotatedEx(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PInteger(Params^[6])^, PBox(Params^[7])^);
end;

(*
TTarget.FindEdges
-----------------
```
function TTarget.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeFinder_FindEdges1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindEdges(PSingle(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TTarget.FindEdges
-----------------
```
function TTarget.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeFinder_FindEdges2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.FindEdges(PSingle(Params^[1])^, PBox(Params^[2])^);
end;

(*
TTarget.GetPixelDifference
--------------------------
```
function TTarget.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeFinder_GetPixelDifference1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TTarget.GetPixelDifference
--------------------------
```
function TTarget.GetPixelDifference(WaitTime, Tolerance: Single; Area: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeFinder_GetPixelDifference2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.GetPixelDifferenceTPA
-----------------------------
```
function TTarget.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeFinder_GetPixelDifferenceTPA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TTarget.GetPixelDifferenceTPA
-----------------------------
```
function TTarget.GetPixelDifferenceTPA(WaitTime, Tolerance: Single; Area: TBox = [-1,-1,-1,-1]): TPointArray;
```
*)
procedure _LapeFinder_GetPixelDifferenceTPA2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaTarget(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TTarget.AverageBrightness
-------------------------
```
function TTarget.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeFinder_AverageBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.AverageBrightness(PBox(Params^[1])^);
end;

(*
TTarget.PeakBrightness
----------------------
```
function TTarget.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
```
*)
procedure _LapeFinder_PeakBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaTarget(Params^[0])^.PeakBrightness(PBox(Params^[1])^);
end;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Target';

    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FTarget: array[1..' + IntToStr(SizeOf(TSimbaTargetInfo)) + '] of Byte;',
      '  FFrozen: record',
      '    Bounds: TBox;',
      '    DataWidth: Integer;',
      '    Data: array of TColorBGRA;',
      '  end;',
      '  FInvalidEvents: array of TMethod;',
      '  FChangeEvents: array of TMethod;',
      '  FCustomClientArea: TBox;',
      '  FAutoFocus: Boolean;',
      '  {%CODETOOLS ON}',
      '',
      '  MouseOptions: record',
      '    {%CODETOOLS OFF}',
      '    MouseButtonEvents: array of TMethod;',
      '    MouseTeleportEvents: array of TMethod;',
      '    MouseMovingEvents: array of TMethod;',
      '    {%CODETOOLS ON}',
      '    MinClickTime: Integer;',
      '    MaxClickTime: Integer;',
      '    Speed: Double;',
      '    Gravity: Double;',
      '    Wind: Double;',
      '    Accuracy: Double;',
      '    Timeout: Integer;',
      '  end;',
      '',
      '  KeyOptions: record',
      '    MinPressTime: Integer;',
      '    MaxPressTime: Integer;',
      '  end;',
      'end;'],
      'TTarget'
    );

    with addGlobalVar('TTarget', '[]', 'Target') do
    begin
      Used := duTrue;
      if (Size <> SizeOf(TSimbaTarget)) then
        SimbaException('SizeOf(TTarget)=%d should be %d', [Size, SizeOf(TSimbaTarget)]);
    end;

    addGlobalType(specialize GetEnumDecl<ESimbaTargetKind>(True, False), 'ETargetKind');
    addGlobalType(specialize GetEnumDecl<EMouseButton>(True, False), 'EMouseButton');
    addGlobalType(specialize GetEnumDecl<EKeyCode>(True, True), 'EKeyCode');

    addGlobalType('procedure(var Input: TTarget; P: TPoint) of object', 'TMouseTeleportEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(var Input: TTarget; Button: EMouseButton; Down: Boolean) of object', 'TMouseButtonEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(var Input: TTarget; var X, Y, DestX, DestY: Double; var Stop: Boolean) of object', 'TMouseMovingEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(var Target: TTarget) of object', 'TTargetEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('procedure TTarget.SetDesktop', @_LapeTarget_SetDesktop);
    addGlobalFunc('procedure TTarget.SetImage(TImage: TImage)', @_LapeTarget_SetImage);
    addGlobalFunc('procedure TTarget.SetWindow(Window: TWindowHandle)', @_LapeTarget_SetWindow);
    addGlobalFunc('procedure TTarget.SetEIOS(Plugin, Args: String)', @_LapeTarget_SetEIOS);
    addGlobalFunc('procedure TTarget.SetPlugin(Plugin, Args: String); overload', @_LapeTarget_SetPlugin1);
    addGlobalFunc('procedure TTarget.SetPlugin(Plugin, Args: String; out DebugImage: TExternalCanvas); overload', @_LapeTarget_SetPlugin2);

    addGlobalFunc('function TTarget.AddTargetChangeEvent(Event: TTargetEvent): TTargetEvent', @_Lape_Target_AddTargetChangeEvent);
    addGlobalFunc('function TTarget.AddTargetInvalidEvent(Event: TTargetEvent): TTargetEvent', @_Lape_Target_AddTargetInvalidEvent);
    addGlobalFunc('procedure TTarget.RemoveTargetChangeEvent(Event: TTargetEvent)', @_Lape_Target_RemoveTargetChangeEvent);
    addGlobalFunc('procedure TTarget.RemoveTargetInvalidEvent(Event: TTargetEvent)', @_Lape_Target_RemoveTargetInvalidEvent);

    addGlobalFunc('procedure TTarget.FreezeImage(ABounds: TBox);', @_LapeTarget_FreezeImage);
    addGlobalFunc('procedure TTarget.UnFreezeImage;', @_LapeTarget_UnFreezeImage);
    addGlobalFunc('function TTarget.IsImageFrozen: Boolean;', @_LapeTarget_IsImageFrozen);
    addGlobalFunc('function TTarget.GetImage(Bounds: TBox = [-1,-1,-1,-1]): TImage', @_LapeTarget_GetImage);

    addGlobalFunc('function TTarget.IsValid: Boolean', @_LapeTarget_IsValid);
    addGlobalFunc('function TTarget.IsFocused: Boolean', @_LapeTarget_IsFocused);
    addGlobalFunc('function TTarget.Focus: Boolean', @_LapeTarget_Focus);

    addGlobalFunc('function TTarget.ToString: String;', @_LapeTarget_ToString);

    addProperty('TTarget', 'AutoFocus', 'Boolean', @_LapeTarget_GetAutoFocus, @_LapeTarget_SetAutoFocus);
    addProperty('TTarget', 'CustomClientArea', 'Boolean', @_LapeTarget_SetCustomClientArea, @_LapeTarget_GetCustomClientArea);

    addProperty('TTarget', 'Bounds', 'TBox', @_LapeTarget_Bounds);
    addProperty('TTarget', 'Width', 'Integer', @_LapeTarget_Width);
    addProperty('TTarget', 'Height', 'Integer', @_LapeTarget_Height);
    addProperty('TTarget', 'Size', 'TSize', @_LapeTarget_Size);

    addProperty('TTarget', 'TargetKind', 'ETargetKind', @_LapeTarget_GetTargetKind);
    addProperty('TTarget', 'TargetWindow', 'TWindowHandle', @_LapeTarget_GetTargetWindow);
    addProperty('TTarget', 'TargetImage', 'TImage', @_LapeTarget_GetTargetImage);

    addGlobalFunc('function TTarget.AddMouseEvent(Event: TMouseTeleportEvent): TMouseTeleportEvent; overload', @_LapeTarget_AddMouseEvent1);
    addGlobalFunc('function TTarget.AddMouseEvent(Event: TMouseButtonEvent): TMouseButtonEvent; overload', @_LapeTarget_AddMouseEvent2);
    addGlobalFunc('function TTarget.AddMouseEvent(Event: TMouseMovingEvent): TMouseMovingEvent; overload', @_LapeTarget_AddMouseEvent3);

    addGlobalFunc('procedure TTarget.RemoveMouseEvent(Event: TMouseTeleportEvent); overload', @_LapeTarget_RemoveMouseEvent1);
    addGlobalFunc('procedure TTarget.RemoveMouseEvent(Event: TMouseButtonEvent); overload', @_LapeTarget_RemoveMouseEvent2);
    addGlobalFunc('procedure TTarget.RemoveMouseEvent(Event: TMouseMovingEvent); overload', @_LapeTarget_RemoveMouseEvent3);

    addGlobalFunc('procedure TTarget.MouseTeleport(P: TPoint)', @_LapeTarget_MouseTeleport);
    addGlobalFunc('procedure TTarget.MouseClick(Button: EMouseButton)', @_LapeTarget_MouseClick);
    addGlobalFunc('procedure TTarget.MouseDown(Button: EMouseButton)', @_LapeTarget_MouseDown);
    addGlobalFunc('procedure TTarget.MouseUp(Button: EMouseButton)', @_LapeTarget_MouseUp);
    addGlobalFunc('procedure TTarget.MouseScroll(Scrolls: Integer)', @_LapeTarget_MouseScroll);
    addGlobalFunc('procedure TTarget.MouseMove(Dest: TPoint); overload', @_LapeTarget_MouseMove1);
    addGlobalFunc('procedure TTarget.MouseMove(Box: TBox; ForcedMove: Boolean = False); overload', @_LapeTarget_MouseMove2);
    addGlobalFunc('procedure TTarget.MouseMove(Quad: TQuad; ForcedMove: Boolean = False); overload', @_LapeTarget_MouseMove3);
    addGlobalFunc('function TTarget.MousePressed(Button: EMouseButton): Boolean', @_LapeTarget_MousePressed);

    addProperty('TTarget', 'MouseXY', 'TPoint', @_LapeTarget_MouseXY_Read, @_LapeTarget_MouseXY_Write);
    addProperty('TTarget', 'MouseX', 'Integer', @_LapeTarget_MouseX_Read, @_LapeTarget_MouseX_Write);
    addProperty('TTarget', 'MouseY', 'Integer', @_LapeTarget_MouseY_Read, @_LapeTarget_MouseY_Write);

    // KEY
    addGlobalFunc('procedure TTarget.KeySend(Text: String)', @_LapeTarget_KeySend);
    addGlobalFunc('procedure TTarget.KeyPress(Key: EKeyCode)', @_LapeTarget_KeyPress);
    addGlobalFunc('procedure TTarget.KeyDown(Key: EKeyCode)', @_LapeTarget_KeyDown);
    addGlobalFunc('procedure TTarget.KeyUp(Key: EKeyCode)', @_LapeTarget_KeyUp);
    addGlobalFunc('function TTarget.KeyPressed(Key: EKeyCode): Boolean', @_LapeTarget_KeyPressed);
    addGlobalFunc('function TTarget.KeyCodeFromChar(C: Char): EKeyCode', @_LapeTarget_KeyCodeFromChar);

    addGlobalFunc('function TTarget.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox= [-1,-1,-1,-1]): TSingleMatrix;', @_LapeTarget_MatchColor);

    addGlobalFunc('function TTarget.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeTarget_FindColor1);
    addGlobalFunc('function TTarget.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeTarget_FindColor2);
    addGlobalFunc('function TTarget.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeTarget_FindColor3);

    addGlobalFunc('function TTarget.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeTarget_CountColor1);
    addGlobalFunc('function TTarget.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeTarget_CountColor2);
    addGlobalFunc('function TTarget.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeTarget_CountColor3);

    addGlobalFunc('function TTarget.HasColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload', @_LapeTarget_HasColor1);
    addGlobalFunc('function TTarget.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload', @_LapeTarget_HasColor2);
    addGlobalFunc('function TTarget.HasColor(Color: TColorTolerance; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload;', @_LapeTarget_HasColor3);

    addGlobalFunc('function TTarget.GetColor(P: TPoint): TColor', @_LapeTarget_GetColor);
    addGlobalFunc('function TTarget.GetColors(Points: TPointArray): TColorArray', @_LapeTarget_GetColors);
    addGlobalFunc('function TTarget.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix', @_LapeTarget_GetColorsMatrix);

    addGlobalFunc('function TTarget.FindImageEx(Image: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeTarget_FindImageEx1);
    addGlobalFunc('function TTarget.FindImageEx(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeTarget_FindImageEx2);
    addGlobalFunc('function TTarget.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeTarget_FindImage1);
    addGlobalFunc('function TTarget.FindImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeTarget_FindImage2);
    addGlobalFunc('function TTarget.HasImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeTarget_HasImage1);
    addGlobalFunc('function TTarget.HasImage(Image: TImage; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeTarget_HasImage2);

    addGlobalFunc('function TTarget.FindTemplate(Templ: TImage; out Match: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeTarget_FindTemplate);
    addGlobalFunc('function TTarget.HasTemplate(Templ: TImage; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): Boolean', @_LapeTarget_HasTemplate);

    addGlobalFunc('function TTarget.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeTarget_FindDTM);
    addGlobalFunc('function TTarget.FindDTMEx(DTM: TDTM; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeTarget_FindDTMEx);
    addGlobalFunc('function TTarget.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeTarget_FindDTMRotated);
    addGlobalFunc('function TTarget.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeTarget_FindDTMRotatedEx);

    addGlobalFunc('function TTarget.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload;', @_LapeFinder_FindEdges1);
    addGlobalFunc('function TTarget.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload;', @_LapeFinder_FindEdges2);

    addGlobalFunc('function TTarget.GetPixelDifference(WaitTime: Integer; Area: TBox): Integer; overload;', @_LapeFinder_GetPixelDifference1);
    addGlobalFunc('function TTarget.GetPixelDifference(WaitTime: Integer; Tolerance: Single; Area: TBox): Integer; overload;', @_LapeFinder_GetPixelDifference2);
    addGlobalFunc('function TTarget.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox): TPointArray; overload;', @_LapeFinder_GetPixelDifferenceTPA1);
    addGlobalFunc('function TTarget.GetPixelDifferenceTPA(WaitTime: Integer; Tolerance: Single; Area: TBox): TPointArray; overload;', @_LapeFinder_GetPixelDifferenceTPA2);

    addGlobalFunc('function TTarget.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;', @_LapeFinder_AverageBrightness);
    addGlobalFunc('function TTarget.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;', @_LapeFinder_PeakBrightness);

    addDelayedCode([
      'function ToString(constref Target: TTarget): String; override;',
      'begin',
      '  Result := Target.ToString();',
      'end;'
    ]);

    ImportingSection := 'Image';

    addGlobalFunc(
      'function TImage.CreateFromTarget(Target: TTarget; Bounds: TBox = [-1,-1,-1,-1]): TImage; static; overload;', [
      'begin',
      '  Result := Target.GetImage(Bounds);',
      'end;'
    ]);
    addGlobalFunc(
      'function TImage.CreateFromTarget(Bounds: TBox = [-1,-1,-1,-1]): TImage; static; overload;', [
      'begin',
      '  Result := TImage.CreateFromTarget(System.Target, Bounds);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure TImage.DrawTarget(Target: TTarget; P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;', [
      'var',
      '  Image: TImage;',
      'begin',
      '  Image := TImage.CreateFromTarget(Target, Bounds);',
      '  Self.DrawImage(Image, P);',
      '  Image.Free();',
      'end;'
    ]);
    addGlobalFunc(
      'procedure TImage.DrawTarget(P: TPoint; Bounds: TBox = [-1,-1,-1,-1]); overload;', [
      'begin',
      '  Self.DrawTarget(System.Target, P, Bounds);',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

end.
