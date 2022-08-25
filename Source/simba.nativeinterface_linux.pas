{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.nativeinterface_linux;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.nativeinterface;

type
  TSimbaNativeInterface_Linux = class(TSimbaNativeInterface)
  public
    procedure HoldKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0); override;
    procedure ReleaseKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0); override;

    function VirtualKeyToNativeKeyCode(VirtualKey: Integer): Integer; override;
    function GetNativeKeyCodeAndModifiers(Character: Char; out Code: Integer; out Modifiers: TShiftState): Boolean; override;

    function GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean; override;
    function GetWindowBounds(Window: TWindowHandle): TBox; override; overload;
    procedure SetWindowBounds(Window: TWindowHandle; Bounds: TBox); override;

    function GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PRGB32): Boolean; override;

    function GetMousePosition: TPoint; override;
    function GetMousePosition(Window: TWindowHandle): TPoint; override;
    procedure SetMousePosition(Window: TWindowHandle; Position: TPoint); override;
    procedure ScrollMouse(Lines: Integer); override;
    procedure HoldMouse(Button: TClickType); override;
    procedure ReleaseMouse(Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType): Boolean; override;
    function IsKeyHeld(Key: Integer): Boolean; override;

    procedure HoldKey(VirtualKey: Integer; WaitTime: Integer = 0); override;
    procedure ReleaseKey(VirtualKey: Integer; WaitTime: Integer = 0); override;

    function GetProcessMemUsage(PID: SizeUInt): Int64; override;
    function GetProcessPath(PID: SizeUInt): String; override;
    function IsProcess64Bit(PID: SizeUInt): Boolean; override;
    function IsProcessRunning(PID: SizeUInt): Boolean; override;
    procedure TerminateProcess(PID: SizeUInt); override;

    function GetWindows: TWindowHandleArray; override;
    function GetWindowChildren(Window: TWindowHandle; Recursive: Boolean): TWindowHandleArray; override;
    function GetVisibleWindows: TWindowHandleArray; override;
    function GetTopWindows: TWindowHandleArray; override;

    function GetWindowAtCursor: TWindowHandle; override;
    function GetDesktopWindow: TWindowHandle; override;
    function GetActiveWindow: TWindowHandle; override;
    function IsWindowActive(Window: TWindowHandle): Boolean; override;
    function IsWindowValid(Window: TWindowHandle): Boolean; override;
    function IsWindowVisible(Window: TWindowHandle): Boolean; override;

    function GetWindowPID(Window: TWindowHandle): Integer; override;
    function GetWindowClass(Window: TWindowHandle): WideString; override;
    function GetWindowTitle(Window: TWindowHandle): WideString; override;

    function GetRootWindow(Window: TWindowHandle): TWindowHandle; override;

    function ActivateWindow(Window: TWindowHandle): Boolean; override;

    function HighResolutionTime: Double; override;

    procedure OpenDirectory(Path: String); override;
  end;

implementation

uses
  x, xatom, keysym, baseunix, unix, linux, lcltype, lazloggerbase, ctypes,
  simba.process, simba.xlib;

const
   _SC_PAGESIZE = 30;

function sysconf(i: cint): clong; cdecl; external name 'sysconf';

function GetWindowProperty(Window: TWindow; Prop: TAtom): Int64;
var
  Atom: TAtom;
  Format, Count, Bytes: Integer;
  Data: PInt64;
begin
  Result := 0;
  if (Window = 0) then
    Exit;

  SimbaXLib.XSync(False);

  if SimbaXLib.XGetWindowProperty(Window, Prop, 0, 2, False, AnyPropertyType, @Atom, @Format, @Count, @Bytes, @Data) and (Data <> nil) then
  try
    Result := Data^;
  finally
    SimbaXLib.XFree(Data);
  end;
end;

function HasWindowProperty(Window: TWindow; Name: String): Boolean;
begin
  Result := GetWindowProperty(Window, SimbaXLib.XInternAtom(PChar(Name), False)) > 0;
end;

function TSimbaNativeInterface_Linux.VirtualKeyToNativeKeyCode(VirtualKey: Integer): Integer;
var
  Symbol: TKeySym;
begin
  case VirtualKey of
    VK_BACK:      Symbol := XK_BackSpace;
    VK_TAB:       Symbol := XK_Tab;
    VK_CLEAR:     Symbol := XK_Clear;
    VK_RETURN:    Symbol := XK_Return;
    VK_SHIFT:     Symbol := XK_Shift_L;
    VK_CONTROL:   Symbol := XK_Control_L;
    VK_MENU:      Symbol := XK_Alt_R;
    VK_CAPITAL:   Symbol := XK_Caps_Lock;

    VK_ESCAPE:    Symbol := XK_Escape;
    VK_SPACE:     Symbol := XK_Space;
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

  Result := SimbaXLib.XKeySymToKeyCode(Symbol);
end;

procedure TSimbaNativeInterface_Linux.HoldKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer);
begin
  SimbaXLib.XTestFakeKeyEvent(KeyCode, True, 0);
  SimbaXLib.XSync(False);

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

procedure TSimbaNativeInterface_Linux.ReleaseKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer);
begin
  SimbaXLib.XTestFakeKeyEvent(KeyCode, False, 0);
  SimbaXLib.XSync(False);

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

function TSimbaNativeInterface_Linux.GetNativeKeyCodeAndModifiers(Character: Char; out Code: Integer; out Modifiers: TShiftState): Boolean;

  function GetSpecialKeySym(Key: Char): TKeySym;
  begin
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
      else
        Result := 0;
    end;
  end;

var
  Symbol: TKeySym;
  Index: Integer;
begin
  Symbol := GetSpecialKeySym(Character);
  if (Symbol = NoSymbol) then
    Symbol := SimbaXLib.XStringToKeysym(PChar(String(Character)));

  Code := SimbaXLib.XKeySymToKeycode(Symbol);
  Modifiers := [];

  Result := (Code > 0);
  if Result then
  begin
    Index := 0;
    while (Index < 8) and (SimbaXLib.XKeyCodeToKeySym(Code, Index) <> Symbol) do
      Inc(Index);

    if (Index > 0) then
    begin
      Dec(Index);

      case Index of
        ControlMapIndex:
          Modifiers := [ssCtrl];
        ShiftMapIndex:
          Modifiers := [ssShift];
        Mod1MapIndex..Mod5MapIndex:
          Modifiers := [ssAlt];
      end;
    end;
  end;
end;

function TSimbaNativeInterface_Linux.GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean;
var
  Child: TWindow;
  X, Y: Integer;
  Attributes: TXWindowAttributes;
begin
  Result := SimbaXLib.XGetWindowAttributes(Window, @Attributes) and
            SimbaXLib.XTranslateCoordinates(Window, Attributes.Root, -Attributes.Border_Width, -Attributes.Border_Width, @X, @Y, @Child);

  if Result then
  begin
    Bounds.X1 := X;
    Bounds.Y1 := Y;
    Bounds.X2 := Bounds.X1 + Attributes.Width;
    Bounds.Y2 := Bounds.Y1 + Attributes.Height;
  end;
end;

function TSimbaNativeInterface_Linux.GetWindowBounds(Window: TWindowHandle): TBox;
begin
  if not GetWindowBounds(Window, Result) then
    Result := Box(0, 0, 0, 0);
end;

procedure TSimbaNativeInterface_Linux.SetWindowBounds(Window: TWindowHandle; Bounds: TBox);
begin
  SimbaXLib.XMoveResizeWindow(Window, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1);
  SimbaXLib.XSync(False);
end;

function TSimbaNativeInterface_Linux.GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PRGB32): Boolean;
var
  Image: PXImage;
begin
  Image := SimbaXLib.XGetImage(Window, X, Y, Width, Height, AllPlanes, ZPixmap);

  Result := Image <> nil;
  if Result then
  begin
    ReAllocMem(ImageData, Width * Height * SizeOf(TRGB32));
    Move(Image^.Data^, PRGB32(ImageData)^, MemSize(ImageData));

    SimbaXLib.XDestroyImage(Image);
  end;
end;

function TSimbaNativeInterface_Linux.GetMousePosition: TPoint;
var
  Event: TXButtonEvent;
begin
  SimbaXLib.XQueryPointer(None,
                          @Event.Root, @Event.Window,
                          @Event.X_Root, @Event.Y_Root,
                          @Result.X, @Result.Y,
                          @Event.State);
end;

function TSimbaNativeInterface_Linux.GetMousePosition(Window: TWindowHandle): TPoint;
var
  Event: TXButtonEvent;
begin
  SimbaXLib.XQueryPointer(Window,
                          @Event.Root, @Event.Window,
                          @Event.X_Root, @Event.Y_Root,
                          @Result.X, @Result.Y,
                          @Event.State);
end;

procedure TSimbaNativeInterface_Linux.SetMousePosition(Window: TWindowHandle; Position: TPoint);
begin
  SimbaXLib.XWarpPointer(None, Window, 0, 0, 0, 0, Position.X, Position.Y);
  SimbaXLib.XFlush();
end;

procedure TSimbaNativeInterface_Linux.ScrollMouse(Lines: Integer);
var
  Button: TClickType;
  i: Integer;
begin
  if Lines > 0 then
    Button := MOUSE_SCROLL_DOWN
  else
    Button := MOUSE_SCROLL_UP;

  for i := 1 to Abs(Lines) do
  begin
    HoldMouse(Button);
    ReleaseMouse(Button);
  end;
end;

procedure TSimbaNativeInterface_Linux.HoldMouse(Button: TClickType);
var
  Number: Integer;
begin
  case Button of
    MOUSE_LEFT:        Number := Button1;
    MOUSE_MIDDLE:      Number := Button2;
    MOUSE_RIGHT:       Number := Button3;
    MOUSE_SCROLL_DOWN: Number := Button5;
    MOUSE_SCROLL_UP:   Number := Button4;
    MOUSE_EXTRA_1:     Number := Button8;
    MOUSE_EXTRA_2:     Number := Button9;
  end;

  SimbaXLib.XTestFakeButtonEvent(Number, True, CurrentTime);
  SimbaXLib.XFlush();
end;

procedure TSimbaNativeInterface_Linux.ReleaseMouse(Button: TClickType);
var
  Number: Integer;
begin
  case Button of
    MOUSE_LEFT:        Number := Button1;
    MOUSE_MIDDLE:      Number := Button2;
    MOUSE_RIGHT:       Number := Button3;
    MOUSE_SCROLL_DOWN: Number := Button5;
    MOUSE_SCROLL_UP:   Number := Button4;
    MOUSE_EXTRA_1:     Number := Button8;
    MOUSE_EXTRA_2:     Number := Button9;
  end;

  SimbaXLib.XTestFakeButtonEvent(Number, False, CurrentTime);
  SimbaXLib.XFlush();
end;

function TSimbaNativeInterface_Linux.IsMouseButtonHeld(Button: TClickType): Boolean;
var
  Mask: Integer;
  Event: TXButtonEvent;
begin
  SimbaXLib.XSync(False);

  case Button of
    MOUSE_LEFT:        begin Event.Button := Button1; Mask := Button1Mask; end;
    MOUSE_MIDDLE:      begin Event.Button := Button2; Mask := Button2Mask; end;
    MOUSE_RIGHT:       begin Event.Button := Button3; Mask := Button3Mask; end;
    MOUSE_SCROLL_UP:   begin Event.Button := Button4; Mask := Button4Mask; end;
    MOUSE_SCROLL_DOWN: begin Event.Button := Button5; Mask := Button5Mask; end;
    MOUSE_EXTRA_1:     raise Exception.Create('IsMouseButtonHeld: MOUSE_EXTRA_1 not supported on Linux'); //begin Event.Button := Button8; Mask := Button8Mask; end;
    MOUSE_EXTRA_2:     raise Exception.Create('IsMouseButtonHeld: MOUSE_EXTRA_2 not supported on Linux'); //begin Event.Button := Button9; Mask := Button9Mask; end;
  end;

  Event := Default(TXButtonEvent);

  SimbaXLib.XQueryPointer(SimbaXLib.XDefaultRootWindow(),
                          @Event.Root, @Event.Window,
                          @Event.X_Root, @Event.Y_Root,
                          @Event.X, @Event.Y,
                          @Event.State);

  Result := ((Event.State and Mask) > 0);
end;

function TSimbaNativeInterface_Linux.IsKeyHeld(Key: Integer): Boolean;
var
  Code: TKeySym;
  Keys: CharArr32;
begin
  Code := VirtualKeyToNativeKeyCode(Key);

  SimbaXLib.XSync(False);
  SimbaXLib.XQueryKeymap(CharArr32(Keys));

  Result := (Keys[Code shr 3] shr (Code and $07)) and $01 > 0;
end;

procedure TSimbaNativeInterface_Linux.HoldKey(VirtualKey: Integer; WaitTime: Integer);
begin
  SimbaXLib.XTestFakeKeyEvent(VirtualKeyToNativeKeyCode(VirtualKey), True, 0);
  SimbaXLib.XSync(False);

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

procedure TSimbaNativeInterface_Linux.ReleaseKey(VirtualKey: Integer; WaitTime: Integer);
begin
  SimbaXLib.XTestFakeKeyEvent(VirtualKeyToNativeKeyCode(VirtualKey), False, 0);
  SimbaXLib.XSync(False);

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

function TSimbaNativeInterface_Linux.GetProcessMemUsage(PID: SizeUInt): Int64;
var
  List: TStringList;
  Resident, Shared: Int64;
begin
  Result := 0;
  if not DirectoryExists('/proc/' + IntToStr(PID)) then
    Exit;

  List := TStringList.Create();
  List.LineBreak := ' ';
  try
    List.LoadFromFile(Format('/proc/%d/statm', [PID]));

    Resident := StrToInt64(List[1]) * sysconf(_SC_PAGESIZE);
    Shared   := StrToInt64(List[2]) * sysconf(_SC_PAGESIZE);

    Result := Resident - Shared;
  finally
    List.Free();
  end;
end;

function TSimbaNativeInterface_Linux.GetProcessPath(PID: SizeUInt): String;
begin
  Result := fpReadLink('/proc/' + IntToStr(PID) + '/exe');
end;

function TSimbaNativeInterface_Linux.IsProcess64Bit(PID: SizeUInt): Boolean;
var
  Stream: TMemoryStream;
begin
  Result := False;
  if not FileExists(GetProcessPath(PID)) then
    Exit;

  Stream := TMemoryStream.Create();
  try
    Stream.LoadFromFile(Self.GetProcessPath(PID));

    Result := (Stream.ReadByte() = $7f) and
              (Stream.ReadByte() = $45) and
              (Stream.ReadByte() = $4c) and
              (Stream.ReadByte() = $46) and
              (Stream.ReadByte() = $02);
  except
  end;

  Stream.Free();
end;

function TSimbaNativeInterface_Linux.IsProcessRunning(PID: SizeUInt): Boolean;
begin
  Result := fpkill(PID, 0) <> 0;
end;

procedure TSimbaNativeInterface_Linux.TerminateProcess(PID: SizeUInt);
begin
  fpkill(PID, SIGKILL);
end;

function TSimbaNativeInterface_Linux.GetWindows: TWindowHandleArray;
begin
  Result := GetWindowChildren(GetDesktopWindow(), True);
end;

function TSimbaNativeInterface_Linux.GetWindowChildren(Window: TWindowHandle; Recursive: Boolean): TWindowHandleArray;

  procedure FindChildren(Window: TWindow);
  var
    Parent, Root: TWindow;
    Children: PWindow;
    Count, I: Integer;
  begin
    if SimbaXLib.XQueryTree(Window, @Root, @Parent, @Children, @Count) and (Children <> nil) then
    begin
      for I := 0 to Count - 1 do
      begin
        Result := Result + [Children[I]];
        if Recursive then
          FindChildren(Children[I]);
       end;

      SimbaXLib.XFree(Children);
    end;
  end;

begin
  Result := Default(TWindowHandleArray);

  FindChildren(Window);
end;

function TSimbaNativeInterface_Linux.GetVisibleWindows: TWindowHandleArray;
var
  Window: TWindowHandle;
begin
  Result := Default(TWindowHandleArray);

  for Window in GetWindows() do
    if IsWindowVisible(Window)  then
      Result := Result + [Window];
end;

function TSimbaNativeInterface_Linux.GetTopWindows: TWindowHandleArray;
var
  Window, Test: TWindowHandle;
begin
  Result := Default(TWindowHandleArray);

  for Window in GetWindowChildren(SimbaXLib.XDefaultRootWindow(), False) do
  begin
    for Test in GetWindowChildren(Window, False) do
      if IsWindowVisible(Test) then
        Result := Result + [Test];
  end;
end;

function TSimbaNativeInterface_Linux.GetWindowAtCursor: TWindowHandle;
var
  Root, Child: TWindow;
  x_root, y_root, x, y: Integer;
  mask: UInt32;
begin
  SimbaXLib.XQueryPointer(GetDesktopWindow(), @Root, @Child, @x_root, @y_root, @x, @y, @mask);

  Result := Child;

  while (Child <> 0) do
  begin
    Result := Child;

    SimbaXLib.XQueryPointer(Result, @Root, @Child, @x_root, @y_root, @x, @y, @mask);
    if (GetWindowPID(Child) = 0) then
      Break;
  end;

  // Check if first child is the client window, since that stores the caption and such...
  if (GetWindowPID(Result) = 0) then
    for Child in GetWindowChildren(Result, False) do
      if HasWindowProperty(Child, 'WM_STATE') then
        Exit(Child);
end;

function TSimbaNativeInterface_Linux.GetDesktopWindow: TWindowHandle;
begin
  Result := SimbaXLib.XDefaultRootWindow();
end;

function TSimbaNativeInterface_Linux.GetActiveWindow: TWindowHandle;
begin
  Result := GetWindowProperty(GetDesktopWindow(), SimbaXLib.XInternAtom('_NET_ACTIVE_WINDOW', False));
end;

function TSimbaNativeInterface_Linux.IsWindowActive(Window: TWindowHandle): Boolean;
begin
  Result := GetRootWindow(GetActiveWindow()) = GetRootWindow(Window);
end;

function TSimbaNativeInterface_Linux.IsWindowValid(Window: TWindowHandle): Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := SimbaXLib.XGetWindowAttributes(Window, @Attributes);
end;

function TSimbaNativeInterface_Linux.IsWindowVisible(Window: TWindowHandle): Boolean;
begin
  Result := HasWindowProperty(GetRootWindow(Window), 'WM_STATE');
end;

function TSimbaNativeInterface_Linux.GetWindowPID(Window: TWindowHandle): Integer;
begin
  Result := GetWindowProperty(GetRootWindow(Window), SimbaXLib.XInternAtom(PChar('_NET_WM_PID'), False));
end;

function TSimbaNativeInterface_Linux.GetWindowClass(Window: TWindowHandle): WideString;
var
  Hint: TXClassHint;
begin
  Result := '';
  if (Window = 0) then
    Exit;

  if (SimbaXLib.XGetClassHint(Window, @Hint) <> 0) then
  begin
    Result := StrPas(Hint.Res_Name);

    if (Hint.Res_Name <> nil) then
      SimbaXLib.XFree(Hint.Res_Name);
    if (Hint.Res_Class <> nil) then
      SimbaXLib.XFree(Hint.Res_Class);
  end;
end;

function TSimbaNativeInterface_Linux.GetWindowTitle(Window: TWindowHandle): WideString;
var
  Struct: TXTextProperty;
begin
  Result := '';
  if (Window = 0) then
    Exit;

  if (SimbaXLib.XGetWMName(Window, @Struct) <> 0) and (Struct.NItems > 0) and (Struct.Value <> nil) then
  try
    if (Struct.Encoding <> XA_STRING) then
    begin
      DebugLn('GetWindowTitle: Unexpected encoding "%d"', [Struct.Encoding]);
      Exit;
    end;

    Result := StrPas(PChar(Struct.Value));
  finally
    SimbaXLib.XFree(Struct.Value);
  end;
end;

function TSimbaNativeInterface_Linux.GetRootWindow(Window: TWindowHandle): TWindowHandle;
var
  Root, Parent: TWindow;
  Children: PWindow;
  Count: Integer;
begin
  Result := Window;
  if (Result = 0) or HasWindowProperty(Result, 'WM_STATE') then
    Exit;

  while (Window > 0) and SimbaXLib.XQueryTree(Window, @Root, @Parent, @Children, @Count) do
  begin
    if (Children <> nil) then
      SimbaXLib.XFree(Children);

    if (Parent > 0) and HasWindowProperty(Parent, 'WM_STATE') then
    begin
      Result := Parent;
      Exit;
    end;

    Window := Parent;
  end;
end;

function TSimbaNativeInterface_Linux.ActivateWindow(Window: TWindowHandle): Boolean;
var
  I: Integer;
  Event: TXClientMessageEvent;
  Struct: TXWindowAttributes;
begin
  Result := False;

  Event := Default(TXClientMessageEvent);
  Event._Type := ClientMessage;
  Event.Display := SimbaXLib.Display;
  Event.Window := GetRootWindow(Window);
  Event.Message_Type := SimbaXLib.XInternAtom('_NET_ACTIVE_WINDOW', False);
  Event.Format := 32;
  Event.Data.L[0] := 2;
  Event.Data.L[1] := CurrentTime;

  if SimbaXLib.XGetWindowAttributes(Window, @Struct) then
  begin
    SimbaXLib.XSendEvent(Struct.Screen^.Root, False, SubstructureNotifyMask or SubstructureRedirectMask, @Event);

    for I := 1 to 10 do
    begin
      if Self.IsWindowActive(Window) then
      begin
        Result := True;
        Exit;
      end;

      Sleep(100);
    end;
  end;
end;

function TSimbaNativeInterface_Linux.HighResolutionTime: Double;
var
  tp: TTimeVal;
  ts: TTimeSpec;
begin
  if clock_gettime(CLOCK_MONOTONIC, @ts) = 0 then
  begin
    Result := (Int64(ts.tv_sec) * 1000) + (ts.tv_nsec / 1000000);
    Exit;
  end;

  fpgettimeofday(@tp, nil);

  Result := (Int64(tp.tv_sec) * 1000) + (tp.tv_usec / 1000);
end;

procedure TSimbaNativeInterface_Linux.OpenDirectory(Path: String);
begin
  SimbaProcess.RunCommand('xdg-open', [Path]);
end;

end.
