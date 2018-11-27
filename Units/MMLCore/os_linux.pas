{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

      Linux OS specific implementation for Mufasa Macro Library
}
{$mode objfpc}{$H+}

unit os_linux;

interface

uses
  classes, sysutils, lcltype,
  xlib, x, xutil, xlibhelpers, keysym,
  mufasatypes, iomanager;

type
  TWindow = class(TWindow_Abstract)
  public
    procedure GetTargetDimensions(out w, h: integer); override;
    procedure GetTargetPosition(out left, top: integer); override;
    function ReturnData(xs, ys, width, height: Integer): TRetData; override;
    procedure FreeReturnData; override;

    function TargetValid: boolean; override;

    function MouseSetClientArea(x1, y1, x2, y2: integer): boolean; override;
    procedure MouseResetClientArea; override;
    function ImageSetClientArea(x1, y1, x2, y2: integer): boolean; override;
    procedure ImageResetClientArea; override;

    procedure ActivateClient; override;
    procedure GetMousePosition(out x,y: integer); override;
    procedure ScrollMouse(x, y: integer; Lines: integer); override;
    procedure MoveMouse(x,y: integer); override;
    procedure HoldMouse(x,y: integer; button: TClickType); override;
    procedure ReleaseMouse(x,y: integer; button: TClickType); override;
    function  IsMouseButtonHeld( button : TClickType) : boolean;override;

    procedure SendString(str: string; keywait, keymodwait: integer); override;
    procedure HoldKey(key: integer); override;
    procedure ReleaseKey(key: integer); override;
    function IsKeyHeld(key: integer): boolean; override;
    function GetKeyCode(c : char) : integer;override;

    function GetHandle: PtrUInt; override;
  private
    Window: HWND;
    Buffer: PXImage;

    { (Forced) Client Area }
    mx1, my1, mx2, my2: integer;
    ix1, iy1, ix2, iy2: integer;
    mcaset, icaset: Boolean;

    procedure MouseApplyAreaOffset(var x, y: integer);
    procedure ImageApplyAreaOffset(var x, y: integer);
  public
    constructor Create(AWindow: HWND);
    destructor Destroy; override;
  end;

  TIOManager = class(TIOManager_Abstract)
  public
    function GetProcesses: TSysProcArr; override;
    function SetTarget(Handle: HWND): Int32; overload;

    procedure SetTargetEx(Proc: TSysProc); override;
    procedure SetDesktop; override;
  end;

implementation

var
  SpecialXKeys: array [0..255] of Int32;

constructor TWindow.Create(AWindow: HWND);
begin
  inherited Create();

  Self.Window := AWindow;
end;

destructor TWindow.Destroy;
begin
  Self.FreeReturnData();

  inherited Destroy();
end;

function TWindow.GetHandle: PtrUInt;
begin
  Result := Self.Window;
end;

procedure TWindow.GetTargetDimensions(out W, H: integer);
var
  Attributes: TXWindowAttributes;
begin
  W := -1;
  H := -1;

  if icaset then
  begin
    W := ix2 - ix1;
    H := iy2 - iy1;
  end else
  if _XGetWindowAttributes(Self.Window, Attributes) then
  begin
    W := Attributes.Width;
    H := Attributes.Height;
  end;
end;

procedure TWindow.GetTargetPosition(out left, top: integer);
var
  Attributes: TXWindowAttributes;
begin
  Left := 0;
  Top := 0;

  if _XGetWindowAttributes(Self.Window, Attributes) Then
  begin
    Left := Attributes.X;
    Top := Attributes.Y;
  end;
end;

function TWindow.TargetValid: boolean;
begin
  Result := _XIsWindowVaild(Self.Window);
end;

{ SetClientArea allows you to use a part of the actual client as a virtual
  client. Im other words, all mouse,find functions will be relative to the
  virtual client.

  XXX:
  I realise I can simply add a[x,y]1 to all of the functions rather than check
  for caset (since they're 0 if it's not set), but I figured it would be more
  clear this way.
}
function TWindow.MouseSetClientArea(x1, y1, x2, y2: integer): boolean;
var w, h: integer;
begin
  { TODO: What if the client resizes (shrinks) and our ``area'' is too large? }
  GetTargetDimensions(w, h);
  if ((x2 - x1) > w) or ((y2 - y1) > h) then
    exit(False);
  if (x1 < 0) or (y1 < 0) then
    exit(False);

  mx1 := x1; my1 := y1; mx2 := x2; my2 := y2;
  mcaset := True;
end;

procedure TWindow.MouseResetClientArea;
begin
  mx1 := 0; my1 := 0; mx2 := 0; my2 := 0;
  mcaset := False;
end;

function TWindow.ImageSetClientArea(x1, y1, x2, y2: integer): boolean;
var w, h: integer;
begin
  { TODO: What if the client resizes (shrinks) and our ``area'' is too large? }
  GetTargetDimensions(w, h);
  if ((x2 - x1) > w) or ((y2 - y1) > h) then
    exit(False);
  if (x1 < 0) or (y1 < 0) then
    exit(False);

  ix1 := x1; iy1 := y1; ix2 := x2; iy2 := y2;
  icaset := True;
end;

procedure TWindow.ImageResetClientArea;
begin
  ix1 := 0; iy1 := 0; ix2 := 0; iy2 := 0;
  icaset := False;
end;

procedure TWindow.MouseApplyAreaOffset(var x, y: integer);
begin
  if mcaset then
  begin
    x := x + mx1;
    y := y + my1;
  end;
end;

procedure TWindow.ImageApplyAreaOffset(var x, y: integer);
begin
  if icaset then
  begin
    x := x + ix1;
    y := y + iy1;
  end;
end;

procedure TWindow.ActivateClient;
begin
  _XSetActiveWindow(_XGetRootWindow(Self.Window));
end;

function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;
var
  w, h: integer;
begin
  GetTargetDimensions(w,h);
  if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) or (width < 0) or (height < 0) then
    raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);

  ImageApplyAreaOffset(xs, ys);

  Result.Ptr := nil;
  Result.IncPtrWith := 0;

  if (Self.Buffer <> nil) then
    raise Exception.Create('TIOManager.FreeReturnData must be called for each TIOManager.ReturnData call');

  Self.Buffer := _XGetImage(Self.Window, xs, ys, width, height, AllPlanes, ZPixmap);
  if (Self.Buffer = nil) then
    raise Exception.Create('TIOManager.ReturnData: XGetImage returned nothing!');

  Result.Ptr := PRGB32(buffer^.data);
  Result.IncPtrWith := 0;
  Result.RowLen := width;
end;

procedure TWindow.FreeReturnData;
begin
  if (Self.Buffer <> nil) then
    XDestroyImage(Self.Buffer);
  Self.Buffer := nil;
end;

procedure TWindow.GetMousePosition(out x, y: integer);
var
  event: TXButtonEvent;
begin
  Event := Default(TXButtonEvent);

  _XQueryPointer(Self.Window,
                 Event.Root, Event.Window,
                 Event.X_Root, Event.Y_Root,
                 Event.X, Event.Y,
                 Event.State);

  x := Event.x;
  y := Event.y;

  x := x - mx1;
  y := y - my1;
end;

procedure TWindow.ScrollMouse(x, y: integer; Lines: integer);
var
  Button: TClickType;
  i: Int32;
begin
  MouseApplyAreaOffset(x, y);

  if Lines > 0 then
    Button := mouse_ScrollDown
  else
  if Lines < 0 then
    Button := mouse_ScrollUp;

  for i := 1 to Abs(Lines) do
  begin
    HoldMouse(x, y, Button);
    ReleaseMouse(x, y, Button);
  end;
end;

procedure TWindow.MoveMouse(x,y: integer);
begin
  MouseApplyAreaOffset(x, y);

  _XWarpPointer(None, window, 0, 0, 0, 0, X, Y);
  _XFlush();
end;

procedure TWindow.HoldMouse(x, y: integer; button: TClickType);
var
  Event: TXButtonPressedEvent;
begin
  Event := Default(TXButtonPressedEvent);

  case button of
    mouse_Left:   Event.button := Button1;
    mouse_Middle: Event.button := Button2;
    mouse_Right:  Event.button := Button3;
    mouse_ScrollDown: Event.button := Button5;
    mouse_ScrollUp: Event.Button := Button4;
  end;
  Event.subwindow := Self.Window;

  while (Event.subwindow <> None) do
  begin
    Event.window := Event.subwindow;

    _XQueryPointer(Event.window,
                   Event.root, Event.subwindow,
                   Event.x_root, Event.y_root,
                   Event.x, Event.y,
                   Event.state);
  end;

  Event._type := ButtonPress;

  _XSendEvent(PointerWindow, True, ButtonPressMask, @Event);
  _XFlush();
end;

procedure TWindow.ReleaseMouse(x,y: integer; button: TClickType);
var
  Event: TXButtonPressedEvent;
begin
  Event := Default(TXButtonPressedEvent);

  case button of
    mouse_Left:   Event.button := Button1;
    mouse_Middle: Event.button := Button2;
    mouse_Right:  Event.button := Button3;
    mouse_ScrollDown: Event.button := Button5;
    mouse_ScrollUp: Event.Button := Button4;
  end;
  Event.subwindow := Self.Window;

  while (Event.subwindow <> None) do
  begin
    Event.window := Event.subwindow;

    _XQueryPointer(Event.window,
                   Event.root, Event.subwindow,
                   Event.x_root, Event.y_root,
                   Event.x, Event.y,
                   Event.state);
  end;

  Event._type := ButtonRelease;

  _XSendEvent(PointerWindow, True, ButtonReleaseMask, @Event);
  _XFlush();
end;

function TWindow.IsMouseButtonHeld(button: TClickType): boolean;
var
  Event: TXButtonEvent;
begin
  Event := Default(TXButtonEvent);

  _XQueryPointer(Event.window,
                 Event.root, Event.window,
                 Event.x_root, Event.y_root,
                 Event.x, event.y,
                 Event.state);

  case Button of
    mouse_Left:       Result := ((Event.state and Button1Mask) > 0);
    mouse_Middle:     Result := ((Event.state and Button2Mask) > 0);
    mouse_Right:      Result := ((Event.state and Button3Mask) > 0);
    mouse_ScrollDown: Result := ((Event.state and Button4Mask) > 0);
    mouse_ScrollUp:   Result := ((Event.state and Button5Mask) > 0);
    else
      Result := False;
  end;
end;

procedure TWindow.SendString(str: string; keywait, keymodwait: integer);
var
  i, Index: Int32;
  SpecialKS, KS: TKeySym;
  KC: TKeyCode;

  function GetModifierKC(n: Int32): TKeyCode;
  begin
     if n = ShiftMapIndex then
       Exit(_XKeysymToKeycode(XK_Shift_L))
     else if n >= Mod1MapIndex then
       Exit(_XKeysymToKeycode(XK_ISO_Level3_Shift))
     else
       raise Exception.CreateFmt('SendString - Unsupported modifier for key: `%s`', [str[i]]);
  end;

begin
  for i := 1 to Length(str) do
  begin
    SpecialKS := SpecialXKeys[Ord(str[i])];
    if SpecialKS <> 0 then
      KS := SpecialKS
    else
      KS := _XGetKeySym(str[i]);

    KC := _XKeysymToKeycode(KS);

    Index := 0;
    while (Index < 8) and (_XKeycodeToKeysym(KC, Index) <> KS) do
      Inc(Index);

    if (Index <> 0) then
    begin
      _XKeyDown(GetModifierKC(Index-1));

      Sleep(keymodwait shr 1);
    end;

    _XKeyDown(KC);

    if (KeyWait > 0) then
      Sleep(KeyWait);

    _XKeyUp(KC);

    if (Index <> 0) then
    begin
      _XKeyUp(GetModifierKC(Index-1));

      Sleep(keymodwait shr 1);
    end;
  end;
end;

procedure TWindow.HoldKey(key: integer);
begin
  _XKeyDown(_XKeySymToKeyCode(VirtualKeyToXKeySym(Key)));
end;

procedure TWindow.ReleaseKey(key: integer);
begin
  _XKeyUp(_XKeySymToKeyCode(VirtualKeyToXKeySym(Key)));
end;

function TWindow.IsKeyHeld(key: integer): boolean;
var
  KC: TKeyCode;
  Keys: array [0..31] of Byte;
begin
  KC := _XKeySymToKeyCode(VirtualKeyToXKeySym(key));
  if (KC = 0) then
    Exit(False);

  _XQueryKeymap(CharArr32(Keys));

  Result := (Keys[KC shr 3] shr (KC and $07)) and $01 > 0;
end;

function TWindow.GetKeyCode(c: Char): integer;
begin
  case C of
    '0'..'9': Result := VK_0 + Ord(C) - Ord('0');
    'a'..'z': Result := VK_A + Ord(C) - Ord('a');
    'A'..'Z': Result := VK_A + Ord(C) - Ord('A');
    ' ' :     Result := VK_SPACE;
  else
    raise Exception.CreateFMT('GetSimpleKeyCode - char (%s) is not in A..z',[c]);
  end;
end;

procedure TIOManager.SetDesktop;
begin
  SetBothTargets(TWindow.Create(_XGetDesktopWindow()));
end;

function TIOManager.SetTarget(Handle: HWND): Int32;
begin
  Result := SetBothTargets(TWindow.Create(Handle));
end;

function TIOManager.GetProcesses: TSysProcArr;
begin
  raise Exception.Create('GetProcesses: Not implemented on linux');
end;

procedure TIOManager.SetTargetEx(Proc: TSysProc);
begin
  raise Exception.Create('SetTargetEx: Not implemented on linux');
end;

initialization
  SpecialXKeys[Ord(#9)]  := (XK_TAB);
  SpecialXKeys[Ord(#10)] := (XK_RETURN);
  SpecialXKeys[Ord(#32)] := (XK_SPACE);
  SpecialXKeys[Ord(#34)] := (XK_QUOTEDBL);
  SpecialXKeys[Ord(#39)] := (XK_APOSTROPHE);
  SpecialXKeys[Ord('!')] := (XK_EXCLAM);
  SpecialXKeys[Ord('#')] := (XK_NUMBERSIGN);
  SpecialXKeys[Ord('%')] := (XK_PERCENT);
  SpecialXKeys[Ord('$')] := (XK_DOLLAR);
  SpecialXKeys[Ord('&')] := (XK_AMPERSAND);
  SpecialXKeys[Ord('(')] := (XK_PARENLEFT);
  SpecialXKeys[Ord(')')] := (XK_PARENRIGHT);
  SpecialXKeys[Ord('=')] := (XK_EQUAL);
  SpecialXKeys[Ord(',')] := (XK_COMMA);
  SpecialXKeys[Ord('.')] := (XK_PERIOD);
  SpecialXKeys[Ord(':')] := (XK_COLON);
  SpecialXKeys[Ord(';')] := (XK_SEMICOLON);
  SpecialXKeys[Ord('<')] := (XK_LESS);
  SpecialXKeys[Ord('>')] := (XK_GREATER);
  SpecialXKeys[Ord('?')] := (XK_QUESTION);
  SpecialXKeys[Ord('@')] := (XK_AT);
  SpecialXKeys[Ord('[')] := (XK_BRACKETLEFT);
  SpecialXKeys[Ord(']')] := (XK_BRACKETRIGHT);
  SpecialXKeys[Ord('\')] := (XK_BACKSLASH);
  SpecialXKeys[Ord('^')] := (XK_ASCIICIRCUM);
  SpecialXKeys[Ord('_')] := (XK_UNDERSCORE);
  SpecialXKeys[Ord('`')] := (XK_GRAVE);
  SpecialXKeys[Ord('{')] := (XK_BRACELEFT);
  SpecialXKeys[Ord('|')] := (XK_BAR);
  SpecialXKeys[Ord('}')] := (XK_BRACERIGHT);
  SpecialXKeys[Ord('~')] := (XK_ASCIITILDE);
  SpecialXKeys[Ord('+')] := (XK_PLUS);
  SpecialXKeys[Ord('-')] := (XK_MINUS);
  SpecialXKeys[Ord('*')] := (XK_ASTERISK);
  SpecialXKeys[Ord('/')] := (XK_SLASH);

end.
