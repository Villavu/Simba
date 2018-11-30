unit xlibhelpers;

// All simba's calls to Xlib should be handled in this file, it manages the display
// and synchronizes on Simba's mainthread. This is because lazarus can pick up a *vaild*
// x error raised by the script and will crash Simba thinking it (lazarus) caused a fatal error.

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  classes, sysutils,
  x, xlib, xutil,
  mufasatypes;

type
  TXWindowArray = array of TWindow;

function _XGetWindowProperty(Window: TWindow; Prop: TAtom): Int32;
function _XHasWindowProperty(Window: TWindow; Name: String): Boolean;
function _XGetActiveWindow: TWindow;
procedure _XSetActiveWindow(Window: TWindow);
function _XIsWindowVaild(Window: TWindow): Boolean;
function _XGetWindowPID(Window: TWindow): UInt32;
function _XGetWindowClass(Window: TWindow): String;
function _XGetWindowTitle(Window: TWindow): String;
function _XGetRootWindow(Window: TWindow): TWindow;
function _XGetWindowVisible(Window: TWindow): Boolean;
function _XGetDesktopWindow: TWindow;
function _XGetChildren(Window: TWindow): TXWindowArray;
function _XGetWindowBounds(Window: TWindow): TBox;
procedure _XSetWindowBounds(Window: TWindow; Bounds: TBox);
function _XGetWindowImage(Window: TWindow; X, Y, Width, Height: Int32): PXImage;
procedure _XHoldMouse(Window: TWindow; X, Y, Button: Int32);
procedure _XReleaseMouse(Window: TWindow; X, Y, Button: Int32);
function _XIsMouseButtonHeld(Mask: Int32): Boolean;
procedure _XMoveMouse(Window: TWindow; X, Y: Int32);
procedure _XGetMousePosition(Window: TWindow; out X, Y: Int32);
procedure _XGetKeyAndModiferCode(Key: Char; out KeyCode, ModifierCode: TKeyCode);
procedure _XKeyDown(Key: TKeyCode);
procedure _XKeyUp(Key: TKeyCode);
function _XIsKeyHeld(Key: TKeyCode): Boolean;
function _XVirtualKeyToKeyCode(Key: UInt16): TKeyCode;
procedure _XKillWindow(Window: TWindow);

implementation

uses
  lcltype, keysym;

var
  _Display: PDisplay;

function XTestFakeKeyEvent(Display: PDisplay; KeyCode: UInt32; Is_Press: Boolean; Delay: UInt32): Int32; cdecl; external;

// XVirtualKeyToKeyCode (helper)

function _XVirtualKeyToKeyCode(Key: UInt16): TKeyCode;
var
  Symbol: TKeySym;
begin
  case Key of
    VK_BACK:      Symbol := XK_BackSpace;
    VK_TAB:       Symbol := XK_Tab;
    VK_CLEAR:     Symbol := XK_Clear;
    VK_RETURN:    Symbol := XK_Return;
    VK_SHIFT:     Symbol := XK_Shift_L;
    VK_CONTROL:   Symbol := XK_Control_L;
    VK_MENU:      Symbol := XK_Alt_R;
    VK_CAPITAL:   Symbol := XK_Caps_Lock;

    VK_ESCAPE:    Symbol := XK_Escape;
    VK_SPACE:     Symbol := XK_space;
    VK_PRIOR:     Symbol := XK_Prior;
    VK_NEXT:      Symbol := XK_Next;
    VK_END:       Symbol := XK_End;
    VK_HOME:      Symbol := XK_Home;
    VK_LEFT:      Symbol := XK_Left;
    VK_UP:        Symbol := XK_Up;
    VK_RIGHT:     Symbol := XK_Right;
    VK_DOWN:      Symbol := XK_Down;
    VK_SELECT:    Symbol := XK_Select;
    VK_PRINT:     Symbol := XK_Print;
    VK_EXECUTE:   Symbol := XK_Execute;

    VK_INSERT:    Symbol := XK_Insert;
    VK_DELETE:    Symbol := XK_Delete;
    VK_HELP:      Symbol := XK_Help;
    VK_0:         Symbol := XK_0;
    VK_1:         Symbol := XK_1;
    VK_2:         Symbol := XK_2;
    VK_3:         Symbol := XK_3;
    VK_4:         Symbol := XK_4;
    VK_5:         Symbol := XK_5;
    VK_6:         Symbol := XK_6;
    VK_7:         Symbol := XK_7;
    VK_8:         Symbol := XK_8;
    VK_9:         Symbol := XK_9;

    VK_A:         Symbol := XK_A;
    VK_B:         Symbol := XK_B;
    VK_C:         Symbol := XK_C;
    VK_D:         Symbol := XK_D;
    VK_E:         Symbol := XK_E;
    VK_F:         Symbol := XK_F;
    VK_G:         Symbol := XK_G;
    VK_H:         Symbol := XK_H;
    VK_I:         Symbol := XK_I;
    VK_J:         Symbol := XK_J;
    VK_K:         Symbol := XK_K;
    VK_L:         Symbol := XK_L;
    VK_M:         Symbol := XK_M;
    VK_N:         Symbol := XK_N;
    VK_O:         Symbol := XK_O;
    VK_P:         Symbol := XK_P;
    VK_Q:         Symbol := XK_Q;
    VK_R:         Symbol := XK_R;
    VK_S:         Symbol := XK_S;
    VK_T:         Symbol := XK_T;
    VK_U:         Symbol := XK_U;
    VK_V:         Symbol := XK_V;
    VK_W:         Symbol := XK_W;
    VK_X:         Symbol := XK_X;
    VK_Y:         Symbol := XK_Y;
    VK_Z:         Symbol := XK_Z;

    VK_NUMPAD0:   Symbol := XK_KP_0;
    VK_NUMPAD1:   Symbol := XK_KP_1;
    VK_NUMPAD2:   Symbol := XK_KP_2;
    VK_NUMPAD3:   Symbol := XK_KP_3;
    VK_NUMPAD4:   Symbol := XK_KP_4;
    VK_NUMPAD5:   Symbol := XK_KP_5;
    VK_NUMPAD6:   Symbol := XK_KP_6;
    VK_NUMPAD7:   Symbol := XK_KP_7;
    VK_NUMPAD8:   Symbol := XK_KP_8;
    VK_NUMPAD9:   Symbol := XK_KP_9;
    VK_MULTIPLY:  Symbol := XK_KP_Multiply;
    VK_ADD:       Symbol := XK_KP_Add;
    VK_SEPARATOR: Symbol := XK_KP_Separator;
    VK_SUBTRACT:  Symbol := XK_KP_Subtract;
    VK_DECIMAL:   Symbol := XK_KP_Decimal;
    VK_DIVIDE:    Symbol := XK_KP_Divide;
    VK_F1:        Symbol := XK_F1;
    VK_F2:        Symbol := XK_F2;
    VK_F3:        Symbol := XK_F3;
    VK_F4:        Symbol := XK_F4;
    VK_F5:        Symbol := XK_F5;
    VK_F6:        Symbol := XK_F6;
    VK_F7:        Symbol := XK_F7;
    VK_F8:        Symbol := XK_F8;
    VK_F9:        Symbol := XK_F9;
    VK_F10:       Symbol := XK_F10;
    VK_F11:       Symbol := XK_F11;
    VK_F12:       Symbol := XK_F12;
    VK_F13:       Symbol := XK_F13;
    VK_F14:       Symbol := XK_F14;
    VK_F15:       Symbol := XK_F15;
    VK_F16:       Symbol := XK_F16;
    VK_F17:       Symbol := XK_F17;
    VK_F18:       Symbol := XK_F18;
    VK_F19:       Symbol := XK_F19;
    VK_F20:       Symbol := XK_F20;
    VK_F21:       Symbol := XK_F21;
    VK_F22:       Symbol := XK_F22;
    VK_F23:       Symbol := XK_F23;
    VK_F24:       Symbol := XK_F24;
    VK_NUMLOCK:   Symbol := XK_Num_Lock;
    VK_SCROLL:    Symbol := XK_Scroll_Lock;
  else
    Symbol := XK_VoidSymbol;
  end;

  Result := XKeySymToKeyCode(_Display, Symbol);
end;

// XKillWindow

type
  TXKillWindow = class
    Window: ^TWindow;

    procedure Execute;
  end;

procedure TXKillWindow.Execute;
begin
  XKillClient(_Display, Window^);
  XSync(_Display, False);
end;

procedure _XKillWindow(Window: TWindow);
var
  Method: TXKillWindow;
begin
  Method := TXKillWindow.Create();
  Method.Window := @Window;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// XQueryTree (helper)

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

// XGetWindowProperty (helper)

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

// XHasWindowProperty (helper)

function _XHasWindowProperty(Window: TWindow; Name: String): Boolean;
begin
  Result := _XGetWindowProperty(Window, XInternAtom(_Display, PChar(Name), False)) > 0;
end;

// KeyDown

type
  TXKeyDown = class
    Key: ^TKeyCode;

    procedure Execute;
  end;

procedure TXKeyDown.Execute;
begin
  XTestFakeKeyEvent(_Display, Key^, True, 0);
  XSync(_Display, False);
end;

procedure _XKeyDown(Key: TKeyCode);
var
  Method: TXKeyDown;
begin
  Method := TXKeyDown.Create();
  Method.Key := @Key;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// KeyUp
type
  TXKeyUp = class
    Key: ^TKeyCode;

    procedure Execute;
  end;

procedure TXKeyUp.Execute;
begin
  XTestFakeKeyEvent(_Display, Key^, False, 0);
  XSync(_Display, False);
end;

procedure _XKeyUp(Key: TKeyCode);
var
  Method: TXKeyUp;
begin
  Method := TXKeyUp.Create();
  Method.Key := @Key;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// IsKeyHeld

type
  TXIsKeyHeld = class
    Key: ^TKeyCode;
    Result: ^Boolean;

    procedure Execute;
  end;

procedure TXIsKeyHeld.Execute;
var
  Keys: array[0..31] of Byte;
begin
  XSync(_Display, False);
  XQueryKeymap(_Display, CharArr32(Keys));

  Result^ := (Keys[Key^ shr 3] shr (Key^ and $07)) and $01 > 0;
end;

function _XIsKeyHeld(Key: TKeyCode): Boolean;
var
  Method: TXIsKeyHeld;
begin
  Method := TXIsKeyHeld.Create();
  Method.Key := @Key;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetActiveWindow

type
  TXGetActiveWindow = class
    Result: ^TWindow;

    procedure Execute;
  end;

procedure TXGetActiveWindow.Execute;
begin
  XFlush(_Display);

  Result^ := _XGetWindowProperty(XDefaultRootWindow(_Display), XInternAtom(_Display, '_NET_ACTIVE_WINDOW', False));
end;

function _XGetActiveWindow: TWindow;
var
  Method: TXGetActiveWindow;
begin
  Result := 0;

  Method := TXGetActiveWindow.Create();
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// SetActiveWindow

type
  TXSetActiveWindow = class
    Window: ^TWindow;

    procedure Execute;
  end;

procedure TXSetActiveWindow.Execute;
var
  Event: TXClientMessageEvent;
  Struct: TXWindowAttributes;
begin
  Window^ := _XGetRootWindow(Window^);

  Event := Default(TXClientMessageEvent);
  Event._Type := ClientMessage;
  Event.Display := _Display;
  Event.Window := Window^;
  Event.Message_Type := XInternAtom(_Display, '_NET_ACTIVE_WINDOW', False);
  Event.Format := 32;
  Event.Data.L[0] := 2;
  Event.Data.L[1] := CurrentTime;

  if XGetWindowAttributes(_Display, Window^, @Struct) <> 0 then
    XSendEvent(_Display, Struct.Screen^.Root, False, SubstructureNotifyMask or SubstructureRedirectMask, @Event);

  XSync(_Display, False);
end;

procedure _XSetActiveWindow(Window: TWindow);
var
  Method: TXSetActiveWindow;
begin
  Method := TXSetActiveWindow.Create();
  Method.Window := @Window;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// IsWindowVaild

type
  TXIsWindowVaild = class
    Window: ^TWindow;
    Result: ^Boolean;

    procedure Execute;
  end;

procedure TXIsWindowVaild.Execute;
var
  Attributes: TXWindowAttributes;
begin
  Result^ := XGetWindowAttributes(_Display, Window^, @Attributes) <> 0;
end;

function _XIsWindowVaild(Window: TWindow): Boolean;
var
  Method: TXIsWindowVaild;
begin
  Result := False;

  Method := TXIsWindowVaild.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetWindowPID

type
  TXGetWindowPID = class
    Window: ^TWindow;
    Result: ^UInt32;

    procedure Execute;
  end;

procedure TXGetWindowPID.Execute;
begin
  Result^ := _XGetWindowProperty(Window^, XInternAtom(_Display, PChar('_NET_WM_PID'), False));
  if (Int32(Result^) = -1) then
    Result^ := 0;
end;

function _XGetWindowPID(Window: TWindow): UInt32;
var
  Method: TXGetWindowPID;
begin
  Result := 0;

  Method := TXGetWindowPID.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetWindowClass

type
  TXGetWindowClass = class
    Window: PWindow;
    Result: PString;

    procedure Execute;
  end;

procedure TXGetWindowClass.Execute;
var
  Hint: TXClassHint;
begin
  if (XGetClassHint(_Display, Window^, @Hint) <> 0) then
  begin
    Result^ := Hint.Res_Name;

    if (Hint.Res_Name <> nil) then
      XFree(Hint.Res_Name);
    if (Hint.Res_Class <> nil) then
      XFree(Hint.Res_Class);
  end;
end;

function _XGetWindowClass(Window: TWindow): String;
var
  Method: TXGetWindowClass;
begin
  Result := '';

  Method := TXGetWindowClass.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetWindowTitle

type
  TXGetWindowTitle = class
    Window: ^TWindow;
    Result: ^String;

    procedure Execute;
  end;

procedure TXGetWindowTitle.Execute;
var
  Struct: TXTextProperty;
  List: PPChar;
  Count: Int32 = 0;
begin
  if (XGetWMName(_Display, Window^, @Struct) > 0) and (Struct.NItems > 0) then
  begin
    XUTF8TextPropertyToTextList(_Display, @Struct, @List, @Count);
    if (Count > 0) then
      Result^ := String(List^);

    if (List <> nil) then
      XFreeStringList(List);
    if (Struct.Value <> nil) then
      XFree(Struct.Value);
  end;
end;

function _XGetWindowTitle(Window: TWindow): String;
var
  Method: TXGetWindowTitle;
begin
  Result := '';

  Method := TXGetWindowTitle.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetRootWindow

type
  TXGetRootWindow = class
    Window: ^TWindow;
    Result: ^TWindow;

    procedure Execute;
  end;

procedure TXGetRootWindow.Execute;
var
  Root, Parent: TWindow;
  Children: TXWindowArray;
  Count: Int32;
begin
  Result^ := Window^;
  if _XHasWindowProperty(Result^, 'WM_STATE') then
    Exit;

  while (Window^ > 0) and _XQueryTree(Window^, Root, Parent, Children, Count) do
  begin
    if (Parent > 0) and _XHasWindowProperty(Parent, 'WM_STATE') then
    begin
      Result^ := Parent;
      Exit;
    end;

    Window^ := Parent;
  end;
end;

function _XGetRootWindow(Window: TWindow): TWindow;
var
  Method: TXGetRootWindow;
begin
  Result := 0;

  Method := TXGetRootWindow.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetWindowVisble

type
  TXGetWindowVisible = class
    Window: ^TWindow;
    Result: ^Boolean;

    procedure Execute;
  end;

procedure TXGetWindowVisible.Execute;
begin
  Result^ := _XGetWindowProperty(_XGetRootWindow(Window^), XInternAtom(_Display, PChar('WM_STATE'), False)) = 1;
end;

function _XGetWindowVisible(Window: TWindow): Boolean;
var
  Method: TXGetWindowVisible;
begin
  Result := False;

  Method := TXGetWindowVisible.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetDesktopWindow

type
  TXGetDesktopWindow = class
    Result: ^TWindow;

    procedure Execute;
  end;

procedure TXGetDesktopWindow.Execute;
begin
  Result^ := XDefaultRootWindow(_Display);
end;

function _XGetDesktopWindow: TWindow;
var
  Method: TXGetDesktopWindow;
begin
  Result := 0;

  Method := TXGetDesktopWindow.Create();
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetChildren

type
  TXGetChildren = class
    Window: ^TWindow;
    Result: ^TXWindowArray;

    procedure Execute;
  end;

procedure TXGetChildren.Execute;

  procedure GetChildren(Window: TWindow);
  var
    Parent, Root: TWindow;
    Children: TXWindowArray;
    Count, i: Int32;
  begin
    if (Window > 0) then
    begin
      SetLength(Result^, Length(Result^) + 1);
      Result^[High(Result^)] := Window;

      if _XQueryTree(Window, Root, Parent, Children, Count) then
      begin
        for i := 0 to High(Children) do
        begin
          SetLength(Result^, Length(Result^) + 1);
          Result^[High(Result^)] := Window;

          GetChildren(Children[i]);
        end;
      end;
    end;
  end;

begin
  GetChildren(Window^);
end;

function _XGetChildren(Window: TWindow): TXWindowArray;
var
  Method: TXGetChildren;
begin
  SetLength(Result, 0);

  Method := TXGetChildren.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetWindowBounds

type
  TXGetWindowBounds = class
    Window: ^TWindow;
    Result: ^TBox;

    procedure Execute;
  end;

procedure TXGetWindowBounds.Execute;
var
  _: TWindow;
  X, Y: Int32;
  Attributes: TXWindowAttributes;
begin
  XSync(_Display, False);
  XGetWindowAttributes(_Display, Window^, @Attributes);
  XTranslateCoordinates(_Display, Window^, Attributes.Root, -Attributes.Border_Width, -Attributes.Border_Width, @X, @Y, @_);

  Result^.X1 := X;
  Result^.Y1 := Y;
  Result^.X2 := Result^.X1 + Attributes.Width;
  Result^.Y2 := Result^.Y1 + Attributes.Height;
end;

function _XGetWindowBounds(Window: TWindow): TBox;
var
  Method: TXGetWindowBounds;
begin
  Result := Default(TBox);

  Method := TXGetWindowBounds.Create();
  Method.Window := @Window;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetWindowImage

type
  TXGetWindowImage = class
    Window: ^TWindow;
    Result: ^PXImage;
    X: ^Int32;
    Y: ^Int32;
    Width: ^Int32;
    Height: ^Int32;

    procedure Execute;
  end;

procedure TXGetWindowImage.Execute;
begin
  Result^ := XGetImage(_Display, Window^, X^, Y^, Width^, Height^, AllPlanes, ZPixmap);
end;

function _XGetWindowImage(Window: TWindow; X, Y, Width, Height: Int32): PXImage;
var
  Method: TXGetWindowImage;
begin
  Result := nil;

  Method := TXGetWindowImage.Create();
  Method.Window := @Window;
  Method.Result := @Result;
  Method.X := @X;
  Method.Y := @Y;
  Method.Width := @Width;
  Method.Height := @Height;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// SetWindowBounds

type
  TXSetWindowBounds = class
    Window: ^TWindow;
    Bounds: ^TBox;

    procedure Execute;
  end;

procedure TXSetWindowBounds.Execute;
begin
  XMoveResizeWindow(_Display, Window^, Bounds^.X1, Bounds^.Y1, Bounds^.X2 - Bounds^.X1, Bounds^.Y2 - Bounds^.Y1);
  XSync(_Display, False);
end;

procedure _XSetWindowBounds(Window: TWindow; Bounds: TBox);
var
  Method: TXSetWindowBounds;
begin
  Method := TXSetWindowBounds.Create();
  Method.Window := @Window;
  Method.Bounds := @Bounds;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// HoldMouse

type
  TXHoldMouse = class
    Window: ^TWindow;
    X: ^Int32;
    Y: ^Int32;
    Button: ^Int32;

    procedure Execute;
  end;

procedure TXHoldMouse.Execute;
var
  Event: TXButtonPressedEvent;
begin
  Event := Default(TXButtonPressedEvent);
  Event.SubWindow := Window^;
  Event.Button := Button^;

  while (Event.SubWindow <> None) do
  begin
    Event.Window := Event.SubWindow;

    XQueryPointer(_Display, Event.Window,
                  @Event.Root, @Event.SubWindow,
                  @Event.X_Root, @Event.Y_Root,
                  @Event.X, @Event.Y,
                  @Event.State);
  end;

  Event._Type := ButtonPress;

  XSendEvent(_Display, PointerWindow, True, ButtonPressMask, @Event);
  XSync(_Display, False);
end;

procedure _XHoldMouse(Window: TWindow; X, Y, Button: Int32);
var
  Method: TXHoldMouse;
begin
  Method := TXHoldMouse.Create();
  Method.Window := @Window;
  Method.X := @X;
  Method.Y := @Y;
  Method.Button := @Button;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// Release Mouse

type
  TXReleaseMouse = class
    Window: ^TWindow;
    X: ^Int32;
    Y: ^Int32;
    Button: ^Int32;

    procedure Execute;
  end;

procedure TXReleaseMouse.Execute;
var
  Event: TXButtonReleasedEvent;
begin
  Event := Default(TXButtonReleasedEvent);
  Event.SubWindow := Window^;
  Event.Button := Button^;

  while (Event.SubWindow <> None) do
  begin
    Event.Window := Event.SubWindow;

    XQueryPointer(_Display, Event.Window,
                  @Event.Root, @Event.SubWindow,
                  @Event.X_Root, @Event.Y_Root,
                  @Event.X, @Event.Y,
                  @Event.State);
  end;

  Event._Type := ButtonRelease;

  XSendEvent(_Display, PointerWindow, True, ButtonReleaseMask, @Event);
  XSync(_Display, False);
end;

procedure _XReleaseMouse(Window: TWindow; X, Y, Button: Int32);
var
  Method: TXReleaseMouse;
begin
  Method := TXReleaseMouse.Create();
  Method.Window := @Window;
  Method.X := @X;
  Method.Y := @Y;
  Method.Button := @Button;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// IsMouseButtonHeld

type
  TXIsMouseButtonHeld = class
    Mask: ^Int32;
    Result: ^Boolean;

    procedure Execute;
  end;

procedure TXIsMouseButtonHeld.Execute;
var
  Event: TXButtonEvent;
begin
  Event := Default(TXButtonEvent);

  XQueryPointer(_Display, Event.Window,
                @Event.Root, @Event.Window,
                @Event.X_Root, @Event.Y_Root,
                @Event.X, @Event.Y,
                @Event.State);

  Result^ := ((Event.State and Mask^) > 0);
end;

function _XIsMouseButtonHeld(Mask: Int32): Boolean;
var
  Method: TXIsMouseButtonHeld;
begin
  Result := False;

  Method := TXIsMouseButtonHeld.Create();
  Method.Mask := @Mask;
  Method.Result := @Result;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// MoveMouse

type
  TXMoveMouse = class
    Window: ^TWindow;
    X: ^Int32;
    Y: ^Int32;

    procedure Execute;
  end;

procedure TXMoveMouse.Execute;
begin
  XWarpPointer(_Display, None, Window^, 0, 0, 0, 0, X^, Y^);
  XSync(_Display, False);
end;

procedure _XMoveMouse(Window: TWindow; X, Y: Int32);
var
  Method: TXMoveMouse;
begin
  Method := TXMoveMouse.Create();
  Method.Window := @Window;
  Method.X := @X;
  Method.Y := @Y;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetMousePosition

type
  TXGetMousePosition = class
    Window: ^TWindow;
    X: ^Int32;
    Y: ^Int32;

    procedure Execute;
  end;

procedure TXGetMousePosition.Execute;
var
  Event: TXButtonEvent;
begin
  Event := Default(TXButtonEvent);

  XQueryPointer(_Display, Window^,
                @Event.Root, @Event.Window,
                @Event.X_Root, @Event.Y_Root,
                X, Y,
                @Event.State);
end;

procedure _XGetMousePosition(Window: TWindow; out X, Y: Int32);
var
  Method: TXGetMousePosition;
begin
  Method := TXGetMousePosition.Create();
  Method.Window := @Window;
  Method.X := @X;
  Method.Y := @Y;

  TThread.Synchronize(nil, @Method.Execute);

  Method.Free();
end;

// GetKeyAndModifierCode (no sync needed)

procedure _XGetKeyAndModiferCode(Key: Char; out KeyCode, ModifierCode: TKeyCode);

  function GetSpecialKeySym(Key: Char): TKeySym;
  begin
    Result := 0;

    case Key of
      #9 : Result := XK_TAB;
      #10: Result := XK_RETURN;
      #32: Result := XK_SPACE;
      #34: Result := XK_QUOTEDBL;
      #39: Result := XK_APOSTROPHE;
      '!': Result := XK_EXCLAM;
      '#': Result := XK_NUMBERSIGN;
      '%': Result := XK_PERCENT;
      '$': Result := XK_DOLLAR;
      '&': Result := XK_AMPERSAND;
      '(': Result := XK_PARENLEFT;
      ')': Result := XK_PARENRIGHT;
      '=': Result := XK_EQUAL;
      ',': Result := XK_COMMA;
      '.': Result := XK_PERIOD;
      ':': Result := XK_COLON;
      ';': Result := XK_SEMICOLON;
      '<': Result := XK_LESS;
      '>': Result := XK_GREATER;
      '?': Result := XK_QUESTION;
      '@': Result := XK_AT;
      '[': Result := XK_BRACKETLEFT;
      ']': Result := XK_BRACKETRIGHT;
      '\': Result := XK_BACKSLASH;
      '^': Result := XK_ASCIICIRCUM;
      '_': Result := XK_UNDERSCORE;
      '`': Result := XK_GRAVE;
      '{': Result := XK_BRACELEFT;
      '|': Result := XK_BAR;
      '}': Result := XK_BRACERIGHT;
      '~': Result := XK_ASCIITILDE;
      '+': Result := XK_PLUS;
      '-': Result := XK_MINUS;
      '*': Result := XK_ASTERISK;
      '/': Result := XK_SLASH;
    end;
  end;

var
  Symbol: TKeySym;
  Index: Int32;
begin
  KeyCode := 0;
  ModifierCode := 0;

  if GetSpecialKeySym(Key) <> 0 then
    Symbol := GetSpecialKeySym(Key)
  else
    Symbol := XStringToKeysym(@Key);

  KeyCode := XKeySymToKeycode(_Display, Symbol);

  if (KeyCode = 0) then
    raise Exception.Create('Unknown keycode for "' + Key + '"');

  Index := 0;
  while (Index < 8) and (XKeyCodeToKeySym(_Display, KeyCode, Index) <> Symbol) do
    Inc(Index);

  if (Index <> 0) then
  begin
    Dec(Index);

    if (Index = ShiftMapIndex) then
      ModifierCode := XKeysymToKeycode(_Display, XK_Shift_L)
    else
    if (Index >= Mod1MapIndex) then
      ModifierCode := XKeysymToKeycode(_Display, XK_ISO_Level3_Shift);

    if (ModifierCode = 0) then
      raise Exception.Create('Unknown keycode for *modifier* "' + Key + '"');
  end;
end;

function XSuppressError(Display: PDisplay; Event: PXErrorEvent): Int32; cdecl;
begin
  WriteLn('Simba encountered a non fatal xlib error: ', Event^.Error_Code);

  Exit(0);
end;

initialization
  _Display := XOpenDisplay(nil);
  if (_Display = nil) then
    raise Exception.Create('Unable to open a XDisplay');

  XSetErrorHandler(@XSuppressError);

finalization
  XCloseDisplay(_Display);

end.

