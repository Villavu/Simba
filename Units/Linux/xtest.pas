unit XTest;

{$mode objfpc}{$H+}
{$linklib Xtst}
interface

uses
  Classes, SysUtils, x, xlib, ctypes, xinput;

{ type
      PXDevice  = Pointer; // ^XDevice ?   }

const
  X_XTestGetVersion = 0;
  X_XTestCompareCursor = 1;
  X_XTestFakeInput = 2;
  X_XTestGrabControl = 3;
  XTestNumberEvents = 0;
  XTestNumberErrors = 0;
  XTestMajorVersion = 2;
  XTestMinorVersion = 2;
  XTestExtensionName = 'XTEST';

function XTestQueryExtension(dpy:PDisplay; event_basep, error_basep, majorp, minorp:pcint):CBool; cdecl; external;
function XTestCompareCursorWithWindow(dpy:PDisplay; window:TWindow; cursor: TCursor):CBool; cdecl; external;
function XTestCompareCurrentCursorWithWindow(dpy:pDisplay; window:TWindow):CBool; cdecl; external;
function XTestFakeKeyEvent(dpy:PDisplay; keycode: cuint; is_press: CBool; delay: cuint):cint;cdecl; external;
function XTestFakeButtonEvent(dpy:PDisplay; button: cuint; is_press: CBool; delay: cuint):cint;cdecl; external;
function XTestFakeMotionEvent(dpy:PDisplay; screen, x,y: cint; delay :cuint):cint;cdecl; external;
function XTestFakeRelativeMotionEvent(dpy:PDisplay; x,y,delay: cuint):cint;cdecl; external;
function XTestFakeDeviceKeyEvent(dpy:PDisplay; dev:pXDevice; keycode:cuint; is_press:cbool;
 axes:pcint; n_axes:cint; delay:cuint):cint;cdecl; external;
function XTestFakeDeviceButtonEvent(dpy:PDisplay; dev:pXDevice; button:cuint; is_press:cbool; axes:pcint;
  n_axes:cint; delay:cuint):cint;cdecl; external;
function XTestFakeProximityEvent(dpy:PDisplay; dev:pXDevice; in_prox:cbool; axes:pcint; n_axes:cint;
  delay:cuint):cint;cdecl; external;
function XTestFakeDeviceMotionEvent(dpy:PDisplay; dev:pXDevice;
  is_relative:cbool; first_axis:cint; axes:pcint; n_axes:cint; delay:cuint):cint;cdecl; external;
function XTestGrabControl(dpy:PDisplay; impervious:cbool):cint;cdecl; external;

procedure XTestSetGContextOfGC(gc: TGC; gid:TGContext); cdecl; external;
procedure XTestSetVisualIDOfVisual(visual:PVisual; visualid: TVisualID); cdecl; external;
function XTestDiscard(dpy:pDisplay):TStatus; cdecl; external;

{ Below is old testing code }
procedure mouse_move(x,y: integer);
procedure mouse_click(left:boolean);
procedure keytest;
procedure keystest(s:string);

implementation

function thing_to_keycode(D: PDisplay; thing: string): TKeyCode;
var
   kc: TKeyCode;
   ks: TKeySym;
begin
  ks := XStringToKeysym(@thing[1]);
  if(ks = NoSymbol) then
  begin
     writeln('Can''t resolve keysym for ' + thing);
     result := thing_to_keycode(D, 'space');
     exit;
  end;
  kc := XKeysymToKeycode(D, ks);
  result := kc;
end;

procedure mouse_move(x,y: integer);
var
   d: PDisplay;
begin
  d := XOpenDisplay(nil);
  XTestFakeMotionEvent(d, -1, x, y, CurrentTime);
  XCloseDisplay(d);
end;

procedure mouse_click(left:boolean);
var
   d: PDisplay;
begin
  d := XOpenDisplay(nil);
  if left then
  begin
    XTestFakeButtonEvent(d, Button1, True, CurrentTime);
    XTestFakeButtonEvent(d, Button1, False, CurrentTime);
  end
  else
  begin
    XTestFakeButtonEvent(d, Button3, True, CurrentTime);
    XTestFakeButtonEvent(d, Button3, False, CurrentTime);
  end;
  XCloseDisplay(d);
end;

procedure send_key(D: PDisplay; c: char);
begin
  XTestFakeKeyEvent(d, thing_to_keycode(d, c), True, CurrentTime);
  XTestFakeKeyEvent(d, thing_to_keycode(d, c), False, CurrentTime);
end;

procedure send_keys(d:pdisplay; s:string);
var
   i: integer;
begin
  for i := 1 to length(s) do
  begin
    send_key(d, s[i]);
    XFlush(d);
  end;
end;

procedure keytest;
var
   d:pdisplay;
begin
  d := XOpenDisplay(nil);
  send_key(d, 'a');
  XCloseDisplay(d);
end;

procedure keystest(s:string);
var
   d:pdisplay;
begin
  d := XOpenDisplay(nil);
  send_keys(d, s);
  XCloseDisplay(d);
end;

end.

