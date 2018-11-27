unit xlibhelpers;

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  classes, sysutils,
  x, xlib, xutil;

type
  TXWindowArray = array of TWindow;

// Wrappers without display & pointers.
function _XQueryTree(Window: TWindow; out Root: TWindow; out Parent: TWindow; out Children: TXWindowArray; out Count: Int32): Boolean;
function _XGetWindowAttributes(Window: TWindow; out Struct: TXWindowAttributes): Boolean;
function _XSync(Discard: Boolean): Boolean;
function _XFlush: Boolean;
function _XKillClient(Window: TWindow): Boolean;
function _XTranslateCoordinates(Source: TWindow; Destination: TWindow; InX, InY: Int32; out OutX, OutY: Int32; out ChildReturn: TWindow): Boolean;
function _XMoveResizeWindow(Window: TWindow; X, Y, Width, Height: Int32): Boolean;
function _XQueryPointer(Window: TWindow; out Root, Child: TWindow; out RootX, RootY, WindowX, WindowY: Int32; out Mask: UInt32): Boolean;
function _XWarpPointer(Window: TWindow; Destination: TWindow; X, Y, Width, Height, DestX, DestY: Int32): Boolean;
function _XSendEvent(Window: TWindow; Propagate: Boolean; Mask: PtrUInt; Event: PXEvent): Boolean;
function _XKeyCodeToKeysym(Key: TKeyCode; Index: Int32): TKeySym;
function _XKeySymToKeyCode(Key: TKeySym): TKeyCode;
function _XQueryKeyMap(Keys: PCharArr32): Boolean;
function _XGetImage(Drawable: TDrawable; X, Y, Width, Height: Int32; Mask: PtrUInt; Format: Int32): PXImage;

// Helpers
function _XGetWindowProperty(Window: TWindow; Prop: TAtom): Int32;
function _XHasWindowProperty(Window: TWindow; Name: String): Boolean;
function _XGetActiveWindow: TWindow;
function _XSetActiveWindow(Window: TWindow): Boolean;
function _XIsWindowVaild(Window: TWindow): Boolean;
function _XGetWindowPID(Window: TWindow): UInt32;
function _XGetWindowClass(Window: TWindow): String;
function _XGetWindowTitle(Window: TWindow): String;
function _XGetRootWindow(Window: TWindow): TWindow;
function _XGetWindowVisible(Window: TWindow): Boolean;
function _XGetDesktopWindow: TWindow;
function _XGetChildren(Window: TWindow): TXWindowArray;
function _XGetKeySym(Key: String): TKeySym;
procedure _XKeyDown(Key: TKeyCode);
procedure _XKeyUp(Key: TKeyCode);

function VirtualKeyToXKeySym(Key: Word): TKeySym;

implementation

uses
  lcltype, keysym;

var
  _Display: PDisplay;

function XTestFakeKeyEvent(Display: PDisplay; KeyCode: UInt32; Is_Press: Boolean; Delay: UInt32): Int32; cdecl; external;

function _XGetKeySym(Key: String): TKeySym;
begin
  Result := XStringToKeysym(PChar(Key));
  if (Result = NoSymbol) then
    raise Exception.CreateFmt('Undefined key: `%s`', [Key]);
end;

procedure _XKeyDown(Key: TKeyCode);
begin
  XTestFakeKeyEvent(_Display, Key, True, 0);
  XFlush(_Display);
end;

procedure _XKeyUp(Key: TKeyCode);
begin
  XTestFakeKeyEvent(_Display, Key, False, 0);
  XFlush(_Display);
end;

function _XQueryTree(Window: TWindow; out Root: TWindow; out Parent: TWindow; out Children: TXWindowArray; out Count: Int32): Boolean;
var
  Data: ^TWindow;
  i: Int32;
begin
  SetLength(Children, 0);

  if (XQueryTree(_Display, Window, @Root, @Parent, @Data, @Count) <> 0) then
  begin
    if (Data <> nil) then
    begin
      SetLength(Children, Count);
      for i := 0 to High(Children) do
        Children[i] := Data[i];

      XFree(Data);
     end;

    Exit(True);
  end;

  Exit(False);
end;

function _XGetWindowAttributes(Window: TWindow; out Struct: TXWindowAttributes): Boolean;
begin
  Result := XGetWindowAttributes(_Display, Window, @Struct) <> 0;
end;

function _XSync(Discard: Boolean): Boolean;
begin
  Result := XSync(_Display, Discard) <> 0;
end;

function _XFlush: Boolean;
begin
  Result := XFlush(_Display) <> 0;
end;

function _XKillClient(Window: TWindow): Boolean;
begin
  Result := XKillClient(_Display, Window) <> 0;
end;

function _XTranslateCoordinates(Source: TWindow; Destination: TWindow; InX, InY: Int32; out OutX, OutY: Int32; out ChildReturn: TWindow): Boolean;
begin
  Result := XTranslateCoordinates(_Display, Source, Destination, InX, InY, @OutX, @OutY, @ChildReturn) <> 0;
end;

function _XMoveResizeWindow(Window: TWindow; X, Y, Width, Height: Int32): Boolean;
begin
  Result := XMoveResizeWindow(_Display, Window, X, Y, Width, Height) <> 0;
end;

function _XQueryPointer(Window: TWindow; out Root, Child: TWindow; out RootX, RootY, WindowX, WindowY: Int32; out Mask: UInt32): Boolean;
begin
  Result := XQueryPointer(_Display, Window, @Root, @Child, @RootX, @RootY, @WindowX, @WindowY, @Mask);
end;

function _XSendEvent(Window: TWindow; Propagate: Boolean; Mask: PtrUInt; Event: PXEvent): Boolean;
begin
  Result := XSendEvent(_Display, Window, Propagate, Mask, Event) = Success;
end;

function _XKeyCodeToKeysym(Key: TKeyCode; Index: Int32): TKeySym;
begin
  Result := XKeycodeToKeysym(_Display, Key, Index);
end;

function _XKeySymToKeyCode(Key: TKeySym): TKeyCode;
begin
  Result := XKeysymToKeyCode(_Display, Key);
end;

function _XQueryKeyMap(Keys: PCharArr32): Boolean;
begin
  Result := XQueryKeymap(_Display, Keys) <> 0;
end;

function _XGetImage(Drawable: TDrawable; X, Y, Width, Height: Int32; Mask: UInt64; Format: Int32): PXImage;
begin
  Result := XGetImage(_Display, Drawable, X, Y, Width, Height, Mask, Format);
end;

function _XWarpPointer(Window: TWindow; Destination: TWindow; X, Y, Width, Height, DestX, DestY: Int32): Boolean;
begin
  Result := XWarpPointer(_Display, Window, Destination, X, Y, Width, Height, DestX, DestY) <> 0;;
end;

function _XGetWindowProperty(Window: TWindow; Prop: TAtom): Int32;
var
  Atom: TAtom;
  Format, Count, Bytes: Int32;
  Data: PByte;
begin
  Result := -1;

  if XGetWindowProperty(_Display, Window, Prop, 0, 1, 0, AnyPropertyType, @Atom, @Format, @Count, @Bytes, @Data) = Success then
  begin
    if (Data <> nil) then
    begin
      Result := PInt32(Data)^;

      XFree(Data);
    end;
  end;
end;

function _XHasWindowProperty(Window: TWindow; Name: String): Boolean;
begin
  Result := _XGetWindowProperty(Window, XInternAtom(_Display, PChar(Name), False)) > 0;
end;

function _XGetActiveWindow: TWindow;
begin
  Result := _XGetWindowProperty(XDefaultRootWindow(_Display), XInternAtom(_Display, '_NET_ACTIVE_WINDOW', False));
end;

function _XSetActiveWindow(Window: TWindow): Boolean;
var
  Event: TXClientMessageEvent;
  Struct: TXWindowAttributes;
begin
  Event := Default(TXClientMessageEvent);
  Event._Type := ClientMessage;
  Event.Display := _Display;
  Event.Window := Window;
  Event.Message_Type := XInternAtom(_Display, '_NET_ACTIVE_WINDOW', False);
  Event.Format := 32;
  Event.Data.L[0] := 2;
  Event.Data.L[1] := CurrentTime;

  Result := _XGetWindowAttributes(Window, Struct) and (XSendEvent(_Display, Struct.Screen^.Root, False, SubstructureNotifyMask or SubstructureRedirectMask, @Event) = Success);
end;

function _XIsWindowVaild(Window: TWindow): Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := _XGetWindowAttributes(Window, Attributes);
end;

function _XGetWindowPID(Window: TWindow): UInt32;
begin
  Result := _XGetWindowProperty(Window, XInternAtom(_Display, PChar('_NET_WM_PID'), False));
  if (Int32(Result) = -1) then
    Result := 0;
end;

function _XGetWindowClass(Window: TWindow): String;
var
  Hint: TXClassHint;
begin
  Result := '';

  if (XGetClassHint(_Display, Window, @Hint) <> 0) then
  begin
    Result := Hint.Res_Name;

    if (Hint.Res_Name <> nil) then
      XFree(Hint.Res_Name);
    if (Hint.Res_Class <> nil) then
      XFree(Hint.Res_Class);
  end;
end;

function _XGetWindowTitle(Window: TWindow): String;
var
  Struct: TXTextProperty;
  List: PPChar;
  Count: Int32 = 0;
begin
  Result := '';

  if (XGetWMName(_Display, Window, @Struct) > 0) and (Struct.NItems > 0) then
  begin
    XUTF8TextPropertyToTextList(_Display, @Struct, @List, @Count);
    if (Count > 0) then
      Result := String(List^);

    if (List <> nil) then
      XFreeStringList(List);
    if (Struct.Value <> nil) then
      XFree(Struct.Value);
  end;
end;

function _XGetRootWindow(Window: TWindow): TWindow;
var
  Root, Parent: TWindow;
  Children: TXWindowArray;
  Count: Int32;
begin
  Result := Window;
  if _XHasWindowProperty(Window, 'WM_STATE') then
    Exit(Window);

  while (Window > 0) and _XQueryTree(Window, Root, Parent, Children, Count) do
  begin
    if (Parent > 0) and _XHasWindowProperty(Parent, 'WM_STATE') then
      Exit(Parent);

    Window := Parent;
  end;
end;

function _XGetWindowVisible(Window: TWindow): Boolean;
begin
  Result := _XGetWindowProperty(_XGetRootWindow(Window), XInternAtom(_Display, PChar('WM_STATE'), False)) = 1;
end;

function _XGetDesktopWindow: TWindow;
begin
  Result := XDefaultRootWindow(_Display);
end;

function _XGetChildren(Window: TWindow): TXWindowArray;

  procedure GetChildren(Window: TWindow);
  var
    Parent, Root: TWindow;
    Children: TXWindowArray;
    Count, i: Int32;
  begin
    if (Window > 0) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Window;

      if _XQueryTree(Window, Root, Parent, Children, Count) then
      begin
        for i := 0 to High(Children) do
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := Window;

          GetChildren(Children[i]);
        end;
      end;
    end;
  end;

begin
  GetChildren(Window);
end;

function VirtualKeyToXKeySym(Key: Word): TKeySym;
begin
  case Key of
    VK_BACK:      Result := XK_BackSpace;
    VK_TAB:       Result := XK_Tab;
    VK_CLEAR:     Result := XK_Clear;
    VK_RETURN:    Result := XK_Return;
    VK_SHIFT:     Result := XK_Shift_L;
    VK_CONTROL:   Result := XK_Control_L;
    VK_MENU:      Result := XK_Alt_R;
    VK_CAPITAL:   Result := XK_Caps_Lock;

    VK_ESCAPE:    Result := XK_Escape;
    VK_SPACE:     Result := XK_space;
    VK_PRIOR:     Result := XK_Prior;
    VK_NEXT:      Result := XK_Next;
    VK_END:       Result := XK_End;
    VK_HOME:      Result := XK_Home;
    VK_LEFT:      Result := XK_Left;
    VK_UP:        Result := XK_Up;
    VK_RIGHT:     Result := XK_Right;
    VK_DOWN:      Result := XK_Down;
    VK_SELECT:    Result := XK_Select;
    VK_PRINT:     Result := XK_Print;
    VK_EXECUTE:   Result := XK_Execute;

    VK_INSERT:    Result := XK_Insert;
    VK_DELETE:    Result := XK_Delete;
    VK_HELP:      Result := XK_Help;
    VK_0:         Result := XK_0;
    VK_1:         Result := XK_1;
    VK_2:         Result := XK_2;
    VK_3:         Result := XK_3;
    VK_4:         Result := XK_4;
    VK_5:         Result := XK_5;
    VK_6:         Result := XK_6;
    VK_7:         Result := XK_7;
    VK_8:         Result := XK_8;
    VK_9:         Result := XK_9;

    VK_A:         Result := XK_a;
    VK_B:         Result := XK_b;
    VK_C:         Result := XK_c;
    VK_D:         Result := XK_d;
    VK_E:         Result := XK_e;
    VK_F:         Result := XK_f;
    VK_G:         Result := XK_g;
    VK_H:         Result := XK_h;
    VK_I:         Result := XK_i;
    VK_J:         Result := XK_j;
    VK_K:         Result := XK_k;
    VK_L:         Result := XK_l;
    VK_M:         Result := XK_m;
    VK_N:         Result := XK_n;
    VK_O:         Result := XK_o;
    VK_P:         Result := XK_p;
    VK_Q:         Result := XK_q;
    VK_R:         Result := XK_r;
    VK_S:         Result := XK_s;
    VK_T:         Result := XK_t;
    VK_U:         Result := XK_u;
    VK_V:         Result := XK_v;
    VK_W:         Result := XK_w;
    VK_X:         Result := XK_x;
    VK_Y:         Result := XK_y;
    VK_Z:         Result := XK_z;

    VK_NUMPAD0:   Result := XK_KP_0;
    VK_NUMPAD1:   Result := XK_KP_1;
    VK_NUMPAD2:   Result := XK_KP_2;
    VK_NUMPAD3:   Result := XK_KP_3;
    VK_NUMPAD4:   Result := XK_KP_4;
    VK_NUMPAD5:   Result := XK_KP_5;
    VK_NUMPAD6:   Result := XK_KP_6;
    VK_NUMPAD7:   Result := XK_KP_7;
    VK_NUMPAD8:   Result := XK_KP_8;
    VK_NUMPAD9:   Result := XK_KP_9;
    VK_MULTIPLY:  Result := XK_KP_Multiply;
    VK_ADD:       Result := XK_KP_Add;
    VK_SEPARATOR: Result := XK_KP_Separator;
    VK_SUBTRACT:  Result := XK_KP_Subtract;
    VK_DECIMAL:   Result := XK_KP_Decimal;
    VK_DIVIDE:    Result := XK_KP_Divide;
    VK_F1:        Result := XK_F1;
    VK_F2:        Result := XK_F2;
    VK_F3:        Result := XK_F3;
    VK_F4:        Result := XK_F4;
    VK_F5:        Result := XK_F5;
    VK_F6:        Result := XK_F6;
    VK_F7:        Result := XK_F7;
    VK_F8:        Result := XK_F8;
    VK_F9:        Result := XK_F9;
    VK_F10:       Result := XK_F10;
    VK_F11:       Result := XK_F11;
    VK_F12:       Result := XK_F12;
    VK_F13:       Result := XK_F13;
    VK_F14:       Result := XK_F14;
    VK_F15:       Result := XK_F15;
    VK_F16:       Result := XK_F16;
    VK_F17:       Result := XK_F17;
    VK_F18:       Result := XK_F18;
    VK_F19:       Result := XK_F19;
    VK_F20:       Result := XK_F20;
    VK_F21:       Result := XK_F21;
    VK_F22:       Result := XK_F22;
    VK_F23:       Result := XK_F23;
    VK_F24:       Result := XK_F24;
    VK_NUMLOCK:   Result := XK_Num_Lock;
    VK_SCROLL:    Result := XK_Scroll_Lock;
  else
    Result := XK_VoidSymbol;
  end;
end;

function XSuppressError(Display: PDisplay; Event: PXErrorEvent): Int32; cdecl;
begin
  WriteLn('XError: ', Event^.Error_Code);

  Exit(0);
end;

initialization
  _Display := XOpenDisplay(nil);
  if (_Display = nil) then
    raise Exception.Create('Unable to open a display');

  XSetErrorHandler(@XSuppressError);

finalization
  XCloseDisplay(_Display);

end.

