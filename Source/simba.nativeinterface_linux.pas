{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.nativeinterface_linux;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.nativeinterface;

type
  TSimbaNativeInterface_Linux = class(TSimbaNativeInterface)
  public
    function GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean; override;
    function GetWindowBounds(Window: TWindowHandle): TBox; override; overload;
    procedure SetWindowBounds(Window: TWindowHandle; Bounds: TBox); override;

    function GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PColorBGRA): Boolean; override;

    function GetMousePosition: TPoint; override;
    function GetMousePosition(Window: TWindowHandle): TPoint; override;

    procedure MouseUp(Button: EMouseButton); override;
    procedure MouseDown(Button: EMouseButton); override;
    procedure MouseScroll(Scrolls: Integer); override;
    procedure MouseTeleport(RelativeWindow: TWindowHandle; P: TPoint); override;
    function MousePressed(Button: EMouseButton): Boolean; override;

    procedure KeySend(Text: PChar; TextLen: Integer; SleepTimes: PInt32); override;
    function KeyPressed(Key: EKeyCode): Boolean; override;
    procedure KeyDown(Key: EKeyCode); override;
    procedure KeyUp(Key: EKeyCode); override;

    function GetProcessStartTime(PID: SizeUInt): TDateTime; override;
    function GetProcessMemUsage(PID: SizeUInt): Int64; override;
    function GetProcessPath(PID: SizeUInt): String; override;
    function IsProcess64Bit(PID: SizeUInt): Boolean; override;
    function IsProcessRunning(PID: SizeUInt): Boolean; override;
    procedure TerminateProcess(PID: SizeUInt); override;

    function GetWindows: TWindowHandleArray; override;
    function GetWindowChildren(Window: TWindowHandle; Recursive: Boolean): TWindowHandleArray; override;
    function GetVisibleWindows: TWindowHandleArray; override;
    function GetTopWindows: TWindowHandleArray; override;

    function GetWindowAtCursor(Exclude: TWindowHandleArray): TWindowHandle; override;
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
  x, keysym, baseunix, unix, linux, lcltype, ctypes,
  simba.process, simba.xlib, simba.vartype_windowhandle, simba.vartype_box;

const
  _SC_PAGESIZE = 30;

function sysconf(i: cint): clong; cdecl; external name 'sysconf';

function VirtualKeyToNativeKeyCode(VirtualKey: EKeyCode): Integer;
var
  Symbol: TKeySym;
begin
  case VirtualKey of
    EKeyCode.BACK:      Symbol := XK_BackSpace;
    EKeyCode.TAB:       Symbol := XK_Tab;
    EKeyCode.CLEAR:     Symbol := XK_Clear;
    EKeyCode.RETURN:    Symbol := XK_Return;
    EKeyCode.SHIFT:     Symbol := XK_Shift_L;
    EKeyCode.CONTROL:   Symbol := XK_Control_L;
    EKeyCode.MENU:      Symbol := XK_Alt_R;
    EKeyCode.CAPITAL:   Symbol := XK_Caps_Lock;

    EKeyCode.ESCAPE:    Symbol := XK_Escape;
    EKeyCode.SPACE:     Symbol := XK_Space;
    EKeyCode.PRIOR:     Symbol := XK_Prior;
    EKeyCode.NEXT:      Symbol := XK_Next;
    EKeyCode.END_KEY:   Symbol := XK_End;
    EKeyCode.HOME:      Symbol := XK_Home;
    EKeyCode.LEFT:      Symbol := XK_Left;
    EKeyCode.UP:        Symbol := XK_Up;
    EKeyCode.RIGHT:     Symbol := XK_Right;
    EKeyCode.DOWN:      Symbol := XK_Down;
    EKeyCode.SELECT:    Symbol := XK_Select;
    EKeyCode.PRINT:     Symbol := XK_Print;
    EKeyCode.EXECUTE:   Symbol := XK_Execute;

    EKeyCode.INSERT:    Symbol := XK_Insert;
    EKeyCode.DELETE:    Symbol := XK_Delete;
    EKeyCode.HELP:      Symbol := XK_Help;
    EKeyCode.NUM_0:     Symbol := XK_0;
    EKeyCode.NUM_1:     Symbol := XK_1;
    EKeyCode.NUM_2:     Symbol := XK_2;
    EKeyCode.NUM_3:     Symbol := XK_3;
    EKeyCode.NUM_4:     Symbol := XK_4;
    EKeyCode.NUM_5:     Symbol := XK_5;
    EKeyCode.NUM_6:     Symbol := XK_6;
    EKeyCode.NUM_7:     Symbol := XK_7;
    EKeyCode.NUM_8:     Symbol := XK_8;
    EKeyCode.NUM_9:     Symbol := XK_9;

    EKeyCode.A:         Symbol := XK_A;
    EKeyCode.B:         Symbol := XK_B;
    EKeyCode.C:         Symbol := XK_C;
    EKeyCode.D:         Symbol := XK_D;
    EKeyCode.E:         Symbol := XK_E;
    EKeyCode.F:         Symbol := XK_F;
    EKeyCode.G:         Symbol := XK_G;
    EKeyCode.H:         Symbol := XK_H;
    EKeyCode.I:         Symbol := XK_I;
    EKeyCode.J:         Symbol := XK_J;
    EKeyCode.K:         Symbol := XK_K;
    EKeyCode.L:         Symbol := XK_L;
    EKeyCode.M:         Symbol := XK_M;
    EKeyCode.N:         Symbol := XK_N;
    EKeyCode.O:         Symbol := XK_O;
    EKeyCode.P:         Symbol := XK_P;
    EKeyCode.Q:         Symbol := XK_Q;
    EKeyCode.R:         Symbol := XK_R;
    EKeyCode.S:         Symbol := XK_S;
    EKeyCode.T:         Symbol := XK_T;
    EKeyCode.U:         Symbol := XK_U;
    EKeyCode.V:         Symbol := XK_V;
    EKeyCode.W:         Symbol := XK_W;
    EKeyCode.X:         Symbol := XK_X;
    EKeyCode.Y:         Symbol := XK_Y;
    EKeyCode.Z:         Symbol := XK_Z;

    EKeyCode.NUMPAD_0:  Symbol := XK_KP_0;
    EKeyCode.NUMPAD_1:  Symbol := XK_KP_1;
    EKeyCode.NUMPAD_2:  Symbol := XK_KP_2;
    EKeyCode.NUMPAD_3:  Symbol := XK_KP_3;
    EKeyCode.NUMPAD_4:  Symbol := XK_KP_4;
    EKeyCode.NUMPAD_5:  Symbol := XK_KP_5;
    EKeyCode.NUMPAD_6:  Symbol := XK_KP_6;
    EKeyCode.NUMPAD_7:  Symbol := XK_KP_7;
    EKeyCode.NUMPAD_8:  Symbol := XK_KP_8;
    EKeyCode.NUMPAD_9:  Symbol := XK_KP_9;
    EKeyCode.MULTIPLY:  Symbol := XK_KP_Multiply;
    EKeyCode.ADD:       Symbol := XK_KP_Add;
    EKeyCode.SEPARATOR: Symbol := XK_KP_Separator;
    EKeyCode.SUBTRACT:  Symbol := XK_KP_Subtract;
    EKeyCode.DECIMAL:   Symbol := XK_KP_Decimal;
    EKeyCode.DIVIDE:    Symbol := XK_KP_Divide;
    EKeyCode.F1:        Symbol := XK_F1;
    EKeyCode.F2:        Symbol := XK_F2;
    EKeyCode.F3:        Symbol := XK_F3;
    EKeyCode.F4:        Symbol := XK_F4;
    EKeyCode.F5:        Symbol := XK_F5;
    EKeyCode.F6:        Symbol := XK_F6;
    EKeyCode.F7:        Symbol := XK_F7;
    EKeyCode.F8:        Symbol := XK_F8;
    EKeyCode.F9:        Symbol := XK_F9;
    EKeyCode.F10:       Symbol := XK_F10;
    EKeyCode.F11:       Symbol := XK_F11;
    EKeyCode.F12:       Symbol := XK_F12;
    EKeyCode.F13:       Symbol := XK_F13;
    EKeyCode.F14:       Symbol := XK_F14;
    EKeyCode.F15:       Symbol := XK_F15;
    EKeyCode.F16:       Symbol := XK_F16;
    EKeyCode.F17:       Symbol := XK_F17;
    EKeyCode.F18:       Symbol := XK_F18;
    EKeyCode.F19:       Symbol := XK_F19;
    EKeyCode.F20:       Symbol := XK_F20;
    EKeyCode.F21:       Symbol := XK_F21;
    EKeyCode.F22:       Symbol := XK_F22;
    EKeyCode.F23:       Symbol := XK_F23;
    EKeyCode.F24:       Symbol := XK_F24;
    EKeyCode.NUMLOCK:   Symbol := XK_Num_Lock;
    EKeyCode.SCROLL:    Symbol := XK_Scroll_Lock;
  else
    Symbol := XK_VoidSymbol;
  end;

  Result := SimbaXLib.XKeySymToKeyCode(Symbol);
end;

function CharToKeySym(Key: Char): TKeySym;
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
      Result := SimbaXLib.XStringToKeysym(PChar(String(Key)));
  end;
end;

procedure GetKeyModifiers(KeySym: TKeySym; KeyCode: TKeyCode; out Shift, Ctrl, Alt: Boolean);
var
  Index: Integer;
begin
  Shift := False;
  Ctrl := False;
  Alt := False;

  if (KeyCode > 0) then
  begin
    Index := 0;
    while (Index < 8) and (SimbaXLib.XKeyCodeToKeySym(KeyCode, Index) <> KeySym) do
      Inc(Index);

    if (Index > 0) then
    begin
      Dec(Index);

      case Index of
        ControlMapIndex:            Ctrl  := True;
        ShiftMapIndex:              Shift := True;
        Mod1MapIndex..Mod5MapIndex: Alt   := True;
      end;
    end;
  end;
end;

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

function HasWindowProperty(Window: TWindow; Prop: TAtom): Boolean;
begin
  Result := GetWindowProperty(Window, Prop) > 0;
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
    Result := TBox.ZERO;
end;

procedure TSimbaNativeInterface_Linux.SetWindowBounds(Window: TWindowHandle; Bounds: TBox);
begin
  SimbaXLib.XMoveResizeWindow(Window, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1);
  SimbaXLib.XSync(False);
end;

function TSimbaNativeInterface_Linux.GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PColorBGRA): Boolean;
var
  Image: PXImage;
begin
  Image := SimbaXLib.XGetImage(Window, X, Y, Width, Height, AllPlanes, ZPixmap);

  Result := Image <> nil;
  if Result then
  begin
    ReAllocMem(ImageData, Width * Height * SizeOf(TColorBGRA));
    Move(Image^.Data^, PColorBGRA(ImageData)^, MemSize(ImageData));

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

procedure TSimbaNativeInterface_Linux.MouseTeleport(RelativeWindow: TWindowHandle; P: TPoint);
begin
  SimbaXLib.XWarpPointer(None, RelativeWindow, 0, 0, 0, 0, P.X, P.Y);
  SimbaXLib.XFlush();
end;

procedure TSimbaNativeInterface_Linux.MouseScroll(Scrolls: Integer);
var
  I: Integer;
begin
  for I := 1 to Abs(Scrolls) do
  begin
    if (Scrolls > 0) then
    begin
      MouseDown(EMouseButton.SCROLL_DOWN);
      MouseUp(EMouseButton.SCROLL_DOWN);
    end else
    begin
      MouseDown(EMouseButton.SCROLL_UP);
      MouseUp(EMouseButton.SCROLL_UP);
    end;
  end;
end;

procedure TSimbaNativeInterface_Linux.MouseDown(Button: EMouseButton);
var
  Number: Integer;
begin
  case Button of
    EMouseButton.LEFT:        Number := Button1;
    EMouseButton.MIDDLE:      Number := Button2;
    EMouseButton.RIGHT:       Number := Button3;
    EMouseButton.SCROLL_DOWN: Number := Button5;
    EMouseButton.SCROLL_UP:   Number := Button4;
  end;

  SimbaXLib.XTestFakeButtonEvent(Number, True, CurrentTime);
  SimbaXLib.XFlush();
end;

procedure TSimbaNativeInterface_Linux.MouseUp(Button: EMouseButton);
var
  Number: Integer;
begin
  case Button of
    EMouseButton.LEFT:        Number := Button1;
    EMouseButton.MIDDLE:      Number := Button2;
    EMouseButton.RIGHT:       Number := Button3;
    EMouseButton.SCROLL_DOWN: Number := Button5;
    EMouseButton.SCROLL_UP:   Number := Button4;
  end;

  SimbaXLib.XTestFakeButtonEvent(Number, False, CurrentTime);
  SimbaXLib.XFlush();
end;

function TSimbaNativeInterface_Linux.MousePressed(Button: EMouseButton): Boolean;
var
  Mask: Integer;
  Event: TXButtonEvent;
begin
  SimbaXLib.XSync(False);

  case Button of
    EMouseButton.LEFT:        begin Event.Button := Button1; Mask := Button1Mask; end;
    EMouseButton.MIDDLE:      begin Event.Button := Button2; Mask := Button2Mask; end;
    EMouseButton.RIGHT:       begin Event.Button := Button3; Mask := Button3Mask; end;
    EMouseButton.SCROLL_UP:   begin Event.Button := Button4; Mask := Button4Mask; end;
    EMouseButton.SCROLL_DOWN: begin Event.Button := Button5; Mask := Button5Mask; end;
  end;

  Event := Default(TXButtonEvent);

  SimbaXLib.XQueryPointer(SimbaXLib.XDefaultRootWindow(),
                          @Event.Root, @Event.Window,
                          @Event.X_Root, @Event.Y_Root,
                          @Event.X, @Event.Y,
                          @Event.State);

  Result := ((Event.State and Mask) > 0);
end;

procedure TSimbaNativeInterface_Linux.KeySend(Text: PChar; TextLen: Integer; SleepTimes: PInt32);
var
  ShiftDown, CtrlDown, AltDown: Boolean;

  procedure DoSleep;
  begin
    PreciseSleep(SleepTimes^);
    Inc(SleepTimes);
  end;

  procedure EnsureModifier(const Needed: Boolean; var isDown: Boolean; const KeyCode: EKeyCode);
  begin
    if (Needed = isDown) then
      Exit;
    isDown := Needed;

    if Needed then
      KeyDown(KeyCode)
    else
      KeyUp(KeyCode);

    DoSleep();
  end;

  procedure KeyEvent(KeyCode: TKeyCode; Down: Boolean);
  begin
    SimbaXLib.XTestFakeKeyEvent(KeyCode, Down, 0);
    SimbaXLib.XSync(False);

    DoSleep();
  end;

var
  I: Integer;
  KeySym: TKeySym;
  KeyCode: TKeyCode;
  Shift, Ctrl, Alt: Boolean;
begin
  ShiftDown := False;
  CtrlDown := False;
  AltDown := False;

  try
    for I := 0 to TextLen - 1 do
    begin
      KeySym := CharToKeySym(Text[I]);
      KeyCode := SimbaXLib.XKeySymToKeycode(KeySym);

      GetKeyModifiers(KeySym, KeyCode, Shift, Ctrl, Alt);

      EnsureModifier(Shift, ShiftDown, EKeyCode.SHIFT);
      EnsureModifier(Ctrl, CtrlDown, EKeyCode.CONTROL);
      EnsureModifier(Alt, AltDown, EKeyCode.MENU);

      KeyEvent(KeyCode, True);
      KeyEvent(KeyCode, False);
    end;
  finally
    EnsureModifier(False, ShiftDown, EKeyCode.SHIFT);
    EnsureModifier(False, CtrlDown, EKeyCode.CONTROL);
    EnsureModifier(False, AltDown, EKeyCode.MENU);
  end;
end;

function TSimbaNativeInterface_Linux.KeyPressed(Key: EKeyCode): Boolean;
var
  Code: TKeySym;
  Keys: CharArr32;
begin
  Code := VirtualKeyToNativeKeyCode(Key);

  SimbaXLib.XSync(False);
  SimbaXLib.XQueryKeymap(CharArr32(Keys));

  Result := (Keys[Code shr 3] shr (Code and $07)) and $01 > 0;
end;

procedure TSimbaNativeInterface_Linux.KeyDown(Key: EKeyCode);
begin
  SimbaXLib.XTestFakeKeyEvent(VirtualKeyToNativeKeyCode(Key), True, 0);
  SimbaXLib.XSync(False);
end;

procedure TSimbaNativeInterface_Linux.KeyUp(Key: EKeyCode);
begin
  SimbaXLib.XTestFakeKeyEvent(VirtualKeyToNativeKeyCode(Key), False, 0);
  SimbaXLib.XSync(False);
end;

function TSimbaNativeInterface_Linux.GetProcessStartTime(PID: SizeUInt): TDateTime;
var
  Info: stat;
begin
  Result := 0;

  if (fpstat('/proc/' + IntToStr(PID), Info) = 0) then
    Result := FileDateToDateTime(Info.st_ctime);
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

function TSimbaNativeInterface_Linux.GetWindowAtCursor(Exclude: TWindowHandleArray): TWindowHandle;

  function ShouldReturnWindow(Window: TWindow): Boolean;
  var
    B: TBox;
  begin
    B := GetWindowBounds(Window);
    Result := (not (Window in Exclude)) and
              (B.Width > 5) and (B.Height > 5) and
              (GetWindowTitle(Window) <> '') and
              (GetWindowClass(Window) <> '');
  end;

var
  Root, Child: TWindow;
  x_root, y_root, x, y: Integer;
  mask: UInt32;
  I: Integer;
  Windows: array[UInt8] of TWindow;
  WindowCount: Integer;
begin
  Result := 0;
  WindowCount := 0;

  SimbaXLib.XQueryPointer(GetDesktopWindow(), @Root, @Child, @x_root, @y_root, @x, @y, @mask);
  while SimbaXLib.XQueryPointer(Child, @Root, @Child, @x_root, @y_root, @x, @y, @mask) do
  begin
    if (Child = 0) then
      Break;
    Windows[WindowCount] := Child;
    Inc(WindowCount);
    if (WindowCount > High(Windows)) then
      Exit;
  end;

  // now go back until we find something worthwhile
  for I := WindowCount - 1 downto 0 do
    if ShouldReturnWindow(Windows[I]) then
      Exit(Windows[I])
end;

function TSimbaNativeInterface_Linux.GetDesktopWindow: TWindowHandle;
begin
  Result := SimbaXLib.XDefaultRootWindow();
end;

function TSimbaNativeInterface_Linux.GetActiveWindow: TWindowHandle;
begin
  Result := GetWindowProperty(GetDesktopWindow(), SimbaXLib._NET_ACTIVE_WINDOW);
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
  Result := HasWindowProperty(GetRootWindow(Window), SimbaXLib.WM_STATE);
end;

function TSimbaNativeInterface_Linux.GetWindowPID(Window: TWindowHandle): Integer;
begin
  Result := GetWindowProperty(GetRootWindow(Window), SimbaXLib._NET_WM_PID);
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

  if (SimbaXLib.XGetWMName(Window, @Struct) <> 0) and (Struct.Value <> nil) then
  begin
    Result := StrPas(PChar(Struct.Value));

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
  if (Result = 0) or HasWindowProperty(Result, SimbaXLib.WM_STATE) then
    Exit;

  while (Window > 0) and SimbaXLib.XQueryTree(Window, @Root, @Parent, @Children, @Count) do
  begin
    if (Children <> nil) then
      SimbaXLib.XFree(Children);

    if (Parent > 0) and HasWindowProperty(Parent, SimbaXLib.WM_STATE) then
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
  Event.Message_Type := SimbaXLib._NET_ACTIVE_WINDOW;
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
  RunProcess('xdg-open', [Path]);
end;

end.
