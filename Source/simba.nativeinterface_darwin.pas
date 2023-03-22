{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.nativeinterface_darwin;

{$i simba.inc}
{$modeswitch objectivec2}

interface

uses
  classes, sysutils, graphics, macosall,
  simba.mufasatypes, simba.nativeinterface, simba.colormath;

type
  TVirtualWindow = packed record
    Handle: CGWindowID;
    InfoIndex: Integer;
  end;
  {$IF SizeOf(TVirtualWindow) <> SizeOf(TWindowHandle)}
    {$FATAL SizeOf(TVirtualWindow) <> SizeOf(TWindowHandle)}
  {$ENDIF}

  TSimbaNativeInterface_Darwin = class(TSimbaNativeInterface)
  protected
  type
    TKeyMapItem = record
      Exists: Boolean;
      KeyCode: Integer;
      Modifiers: TShiftState;
    end;
    TKeyMap = array[#0..#255] of TKeyMapItem;
  protected
    FKeyMap: TKeyMap;
    FVirtualWindowInfo: array of record
      ClientRect: TBox;
      ClassStr: ShortString;
    end;

    function IsVirtualWindow(Window: TWindowHandle; out VirtualWindow: TVirtualWindow): Boolean;
    function GetVirtualWindowInfoIndex(ClientRect: TBox; ClassStr: ShortString): Integer;
  public
    constructor Create;

    procedure HoldKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0); override;
    procedure ReleaseKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0); override;

    function VirtualKeyToNativeKeyCode(VirtualKey: Integer): Integer; override;
    function GetNativeKeyCodeAndModifiers(Character: Char; out Code: Integer; out Modifiers: TShiftState): Boolean; override;

    function GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean; override;
    function GetWindowBounds(Window: TWindowHandle): TBox; override; overload;
    procedure SetWindowBounds(Window: TWindowHandle; Bounds: TBox); override;

    function GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PColorBGRA): Boolean; override;

    function GetMousePosition: TPoint; override;
    function GetMousePosition(Window: TWindowHandle): TPoint; override;
    procedure SetMousePosition(Window: TWindowHandle; Position: TPoint); override;
    procedure ScrollMouse(Lines: Integer); override;
    procedure HoldMouse(Button: TClickType); override;
    procedure ReleaseMouse(Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType): Boolean; override;
    function IsKeyHeld(Key: Integer): Boolean; override;

    procedure HoldKey(Key: Integer; WaitTime: Integer = 0); override;
    procedure ReleaseKey(Key: Integer; WaitTime: Integer = 0); override;

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

    function WindowHandleToStr(Window: TWindowHandle): String; override;
    function WindowHandleFromStr(Str: String): TWindowHandle; override;
  end;

implementation

uses
  baseunix, unix, lcltype, cocoaall, cocoautils,
  simba.process, simba.darwin_axui, simba.windowhandle;

type
  NSEventFix = objccategory external(NSEvent)
    function charactersByApplyingModifiers(Flags: NSEventModifierFlags): NSString; message 'charactersByApplyingModifiers:';
  end;

  TTimebaseInfoData = packed record
    numer: UInt32;
    denom: UInt32;
  end;

var
  timeInfo: TTimebaseInfoData;

function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): Int64; cdecl; external 'libc';
function mach_absolute_time: QWORD; cdecl; external 'libc';

type
  TProcTaskInfo = record
    virtual_size: uint64;
    resident_size: uint64;
    total_user: uint64;
    total_system: uint64;
    threads_user: uint64;
    threads_system: uint64;

    policy: int32;
    faults: int32;
    pageins: int32;
    cow_faults: int32;
    messages_sent: int32;
    messages_recv: int32;
    syscalls_mach: int32;
    syscalls_unix: int32;
    csw: int32;
    threadnum: int32;
    numrunning: int32;
    priority: int32;
  end;

const
  PROC_PIDTASKINFO = 4;

function proc_pidpath(pid: longint; buffer: pbyte; bufferSize: longword): longint; cdecl; external 'libproc';
function proc_pidinfo(pid: longint; flavor: longint; arg: UInt64; buffer: pointer; buffersize: longint): longint; cdecl; external 'libproc';

function TSimbaNativeInterface_Darwin.GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean;
var
  windowIds, windows: CFArrayRef;
  Rect: CGRect;
  VirtualWindow: TVirtualWindow;
  B: TBox;
begin
  if Self.IsVirtualWindow(Window, VirtualWindow) then
  begin
    Result := GetWindowBounds(VirtualWindow.Handle, B);
    if Result then
    begin
      Bounds := Self.FVirtualWindowInfo[VirtualWindow.InfoIndex].ClientRect;
      Bounds := Bounds.Offset(B.X1, B.Y1);
    end;

    Exit;
  end;

  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, Pointer(Window));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);

  Result := CFArrayGetCount(windows) <> 0;
  if Result then
  begin
    CGRectMakeWithDictionaryRepresentation(CFDictionaryGetValue(CFDictionaryRef(CFArrayGetValueAtIndex(windows, 0)), kCGWindowBounds), Rect);

    with CGRectToRect(Rect) do
    begin
      Bounds.X1 := Left;
      Bounds.Y1 := Top;
      Bounds.X2 := Right;
      Bounds.Y2 := Bottom;
    end;
  end;

  CFRelease(windowIds);
  CFRelease(windows);
end;

function TSimbaNativeInterface_Darwin.GetWindowBounds(Window: TWindowHandle): TBox;
begin
  if not GetWindowBounds(Window, Result) then
    Result := TBox.Default();
end;

procedure TSimbaNativeInterface_Darwin.SetWindowBounds(Window: TWindowHandle; Bounds: TBox);
begin
  // TODO: Requires usages of AX API.
end;

function TSimbaNativeInterface_Darwin.GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PColorBGRA): Boolean;
var
  Image: CGImageRef;
  ColorSpace: CGColorSpaceRef;
  Context: CGContextRef;
  WindowBounds: TBox;
  R: TRect;
  VirtualWindow: TVirtualWindow;
begin
  WindowBounds := GetWindowBounds(Window);
  if IsVirtualWindow(Window, VirtualWindow) then
    Window := VirtualWindow.Handle;

  R.Left   := WindowBounds.X1 + X;
  R.Top    := WindowBounds.Y1 + Y;
  R.Right  := WindowBounds.X1 + X + Width;
  R.Bottom := WindowBounds.Y1 + Y + Height;

  if Window = GetDesktopWindow() then
    Image := CGWindowListCreateImage(RectToCGRect(R), kCGWindowListOptionOnScreenOnly, kCGNullWindowID, kCGWindowImageBoundsIgnoreFraming)
  else
    Image := CGWindowListCreateImage(RectToCGRect(R), kCGWindowListOptionIncludingWindow, Window, kCGWindowImageBoundsIgnoreFraming);

  Result := (Image <> nil) and (CGImageGetWidth(Image) = Width) and (CGImageGetHeight(Image) = Height);

  if (Image <> nil) then
  begin
    if Result then
    begin
      ReAllocMem(ImageData, Width * Height * SizeOf(TColorBGRA));

      ColorSpace := CGColorSpaceCreateDeviceRGB();
      Context := CGBitmapContextCreate(ImageData, Width, Height, 8, Width * SizeOf(TColorBGRA), ColorSpace, kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little);

      CGContextDrawImage(Context, CGRectMake(0, 0, Width, Height), Image);
      CGContextRelease(Context);
      CGColorSpaceRelease(ColorSpace);
    end;

    CGImageRelease(Image);
  end;
end;

function TSimbaNativeInterface_Darwin.GetMousePosition: TPoint;
var
  Event: CGEventRef;
  Point: CGPoint;
begin
  Event := CGEventCreate(nil);
  Point := CGEventGetLocation(Event);
  CFRelease(Event);

  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;

function TSimbaNativeInterface_Darwin.GetMousePosition(Window: TWindowHandle): TPoint;
var
  event: CGEventRef;
  point: CGPoint;
begin
  event := CGEventCreate(nil);
  point := CGEventGetLocation(event);

  with GetWindowBounds(Window) do
  begin
    Result.X := Round(Point.X) - X1;
    Result.Y := Round(Point.Y) - Y1;
  end;

  CFRelease(event);
end;

procedure TSimbaNativeInterface_Darwin.SetMousePosition(Window: TWindowHandle; Position: TPoint);
begin
  with GetWindowBounds(Window) do
    CGWarpMouseCursorPosition(CGPointMake(X1 + Position.X, Y1 + Position.Y));
end;

procedure TSimbaNativeInterface_Darwin.ScrollMouse(Lines: Integer);
var
  ScrollEvent: CGEventRef;
begin
  ScrollEvent := CGEventCreateScrollWheelEvent(nil, kCGScrollEventUnitPixel, 1, -Lines * 10);
  CGEventPost(kCGHIDEventTap, ScrollEvent);
  CFRelease(ScrollEvent);
end;

const
  kCGEventRightMouseDown = 3;
  kCGEventLeftMouseDown  = 1;
  kCGEventOtherMouseDown = 25;

  kCGEventRightMouseUp = 4;
  kCGEventLeftMouseUp  = 2;
  kCGEventOtherMouseUp = 26;

  kCGEventScrollWheel  = 22;

const
  ClickTypeToMouseDownEvent: array[TClickType] of Integer = (
    kCGEventRightMouseDown,
    kCGEventLeftMouseDown,
    kCGEventOtherMouseDown,
    kCGEventOtherMouseDown,
    kCGEventOtherMouseDown,
    -1,
    -1
  );

  ClickTypeToMouseButton: array[TClickType] of Integer = (
    kCGMouseButtonRight,
    kCGMouseButtonLeft,
    kCGMouseButtonCenter,
    3,
    4,
    -1,
    -1
  );

  ClickTypeToMouseUpEvent: array[TClickType] of Integer = (
    kCGEventRightMouseUp,
    kCGEventLeftMouseUp,
    kCGEventOtherMouseUp,
    kCGEventOtherMouseUp,
    kCGEventOtherMouseUp,
    -1,
    -1
  );

procedure TSimbaNativeInterface_Darwin.HoldMouse(Button: TClickType);
var
  event: CGEventRef;
  eventType, mouseButton: Integer;
begin
  eventType := ClickTypeToMouseDownEvent[Button];
  mouseButton := ClickTypeToMouseButton[Button];

  with GetMousePosition() do
    event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(X, Y), mouseButton);

  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

procedure TSimbaNativeInterface_Darwin.ReleaseMouse(Button: TClickType);
var
  event: CGEventRef;
  eventType, mouseButton: Integer;
begin
  eventType := ClickTypeToMouseUpEvent[Button];
  mouseButton := ClickTypeToMouseButton[Button];

  with GetMousePosition() do
    event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(X, Y), mouseButton);

  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

function TSimbaNativeInterface_Darwin.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  Result := CGEventSourceButtonState(kCGEventSourceStateCombinedSessionState, ClickTypeToMouseButton[Button]) > 0;
end;

function TSimbaNativeInterface_Darwin.IsKeyHeld(Key: Integer): Boolean;
begin
  Result := CGEventSourceKeyState(kCGEventSourceStateCombinedSessionState, VirtualKeyToNativeKeyCode(Key)) <> 0;
end;

procedure TSimbaNativeInterface_Darwin.HoldKey(Key: Integer; WaitTime: Integer);
begin
  CGPostKeyboardEvent(0, VirtualKeyToNativeKeyCode(Key), 1);

  if (WaitTime > 0) then
    PreciseSleep(WaitTime);
end;

procedure TSimbaNativeInterface_Darwin.ReleaseKey(Key: Integer; WaitTime: Integer);
begin
  CGPostKeyboardEvent(0, VirtualKeyToNativeKeyCode(Key), 0);

  if (WaitTime > 0) then
    PreciseSleep(WaitTime);
end;

procedure TSimbaNativeInterface_Darwin.HoldKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer);
begin
  CGPostKeyboardEvent(0, KeyCode, 1);

  if (WaitTime > 0) then
    PreciseSleep(WaitTime);
end;

procedure TSimbaNativeInterface_Darwin.ReleaseKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer);
begin
  CGPostKeyboardEvent(0, KeyCode, 0);

  if (WaitTime > 0) then
    PreciseSleep(WaitTime);
end;

function TSimbaNativeInterface_Darwin.VirtualKeyToNativeKeyCode(VirtualKey: Integer): Integer;
begin
  case VirtualKey of
    VK_BACK:                Result := $33;
    VK_TAB:                 Result := $30;
    VK_CLEAR:               Result := $47;
    VK_RETURN:              Result := $24;
    VK_SHIFT:               Result := $38;
    VK_CONTROL:             Result := $3B;
    VK_MENU:                Result := $3A;
    VK_CAPITAL:             Result := $39;
    VK_KANA:                Result := $68;
    VK_ESCAPE:              Result := $35;
    VK_SPACE:               Result := $31;
    VK_PRIOR:               Result := $74;
    VK_NEXT:                Result := $79;
    VK_END:                 Result := $77;
    VK_HOME:                Result := $73;
    VK_LEFT:                Result := $7B;
    VK_UP:                  Result := $7E;
    VK_RIGHT:               Result := $7C;
    VK_DOWN:                Result := $7D;
    VK_DELETE:              Result := $75;
    VK_HELP:                Result := $72;
    VK_0:                   Result := $1D;
    VK_1:                   Result := $12;
    VK_2:                   Result := $13;
    VK_3:                   Result := $14;
    VK_4:                   Result := $15;
    VK_5:                   Result := $17;
    VK_6:                   Result := $16;
    VK_7:                   Result := $1A;
    VK_8:                   Result := $1C;
    VK_9:                   Result := $19;
    VK_A:                   Result := $00;
    VK_B:                   Result := $0B;
    VK_C:                   Result := $08;
    VK_D:                   Result := $02;
    VK_E:                   Result := $0E;
    VK_F:                   Result := $03;
    VK_G:                   Result := $05;
    VK_H:                   Result := $04;
    VK_I:                   Result := $22;
    VK_J:                   Result := $26;
    VK_K:                   Result := $28;
    VK_L:                   Result := $25;
    VK_M:                   Result := $2E;
    VK_N:                   Result := $2D;
    VK_O:                   Result := $1F;
    VK_P:                   Result := $23;
    VK_Q:                   Result := $0C;
    VK_R:                   Result := $0F;
    VK_S:                   Result := $01;
    VK_T:                   Result := $11;
    VK_U:                   Result := $20;
    VK_V:                   Result := $09;
    VK_W:                   Result := $0D;
    VK_X:                   Result := $07;
    VK_Y:                   Result := $10;
    VK_Z:                   Result := $06;
    VK_LWIN:                Result := $37;
    VK_RWIN:                Result := $36;
    VK_APPS:                Result := $3D;
    VK_NUMPAD0:             Result := $52;
    VK_NUMPAD1:             Result := $53;
    VK_NUMPAD2:             Result := $54;
    VK_NUMPAD3:             Result := $55;
    VK_NUMPAD4:             Result := $56;
    VK_NUMPAD5:             Result := $57;
    VK_NUMPAD6:             Result := $58;
    VK_NUMPAD7:             Result := $59;
    VK_NUMPAD8:             Result := $5B;
    VK_NUMPAD9:             Result := $5C;
    VK_MULTIPLY:            Result := $43;
    VK_ADD:                 Result := $45;
    VK_SEPARATOR:           Result := $2B; // Separator used will be VK_COMMA instead of VK_PERIOD
    VK_SUBTRACT:            Result := $4E;
    VK_DECIMAL:             Result := $41;
    VK_DIVIDE:              Result := $4B;
    VK_F1:                  Result := $7A;
    VK_F2:                  Result := $78;
    VK_F3:                  Result := $63;
    VK_F4:                  Result := $76;
    VK_F5:                  Result := $60;
    VK_F6:                  Result := $61;
    VK_F7:                  Result := $62;
    VK_F8:                  Result := $64;
    VK_F9:                  Result := $65;
    VK_F10:                 Result := $6D;
    VK_F11:                 Result := $67;
    VK_F12:                 Result := $6F;
    VK_F13:                 Result := $69;
    VK_F14:                 Result := $6B;
    VK_F15:                 Result := $71;
    VK_F16:                 Result := $6A;
    VK_F17:                 Result := $40;
    VK_F18:                 Result := $4F;
    VK_F19:                 Result := $50;
    VK_F20:                 Result := $5A;
    VK_LSHIFT:              Result := $38;
    VK_RSHIFT:              Result := $3C;
    VK_LCONTROL:            Result := $3B;
    VK_RCONTROL:            Result := $3E;
    VK_LMENU:               Result := $3A;
    VK_RMENU:               Result := $3D;
    VK_VOLUME_MUTE:         Result := $4A;
    VK_VOLUME_DOWN:         Result := $49;
    VK_VOLUME_UP:           Result := $48;
    VK_OEM_1:               Result := $29;
    VK_OEM_PLUS:            Result := $45;
    VK_OEM_COMMA:           Result := $2B;
    VK_OEM_MINUS:           Result := $1B;
    VK_OEM_PERIOD:          Result := $2F;
    VK_OEM_2:               Result := $2C; // /?
    VK_OEM_3:               Result := $32; // `~
    VK_OEM_4:               Result := $21; // [{
    VK_OEM_5:               Result := $2A; // \|
    VK_OEM_6:               Result := $1E; // ]}
    VK_OEM_7:               Result := $27; // '"
    $E1:                    Result := $18; //VK_EQUAL
    VK_OEM_102:             Result := $2A; // backslash RT-102
    $E3:                    Result := $51; //VK_KEYPAD_EQUALS
    $E4:                    Result := $3F; //VK_FUNCTION
    VK_OEM_CLEAR:           Result := $47;
  else
    Result := $FFFF;
  end;
end;

function TSimbaNativeInterface_Darwin.GetNativeKeyCodeAndModifiers(Character: Char; out Code: Integer; out Modifiers: TShiftState): Boolean;
begin
  Result := FKeyMap[Character].Exists;

  if Result then
  begin
    Code := FKeyMap[Character].KeyCode;
    Modifiers := FKeyMap[Character].Modifiers;
  end
end;

function TSimbaNativeInterface_Darwin.GetProcessMemUsage(PID: SizeUInt): Int64;
var
  info: TProcTaskInfo;
begin
  Result := 0;

  if proc_pidinfo(PID, PROC_PIDTASKINFO, 0, @info, SizeOf(TProcTaskInfo)) > 0 then
    Result := info.resident_size;
end;

function TSimbaNativeInterface_Darwin.GetProcessPath(PID: SizeUInt): String;
const
  PROC_PIDPATHINFO_MAXSIZE = 4096;
var
  Buffer: array[1..PROC_PIDPATHINFO_MAXSIZE] of Char;
  Len: Integer;
begin
  Result := '';

  Len := proc_pidpath(PID, @Buffer[1], Length(Buffer));
  if (Len > 0) then
    Result := Copy(Buffer, 1, Len);
end;

function TSimbaNativeInterface_Darwin.IsProcess64Bit(PID: SizeUInt): Boolean;
begin
  Result := True;
end;

function TSimbaNativeInterface_Darwin.IsProcessRunning(PID: SizeUInt): Boolean;
begin
  Result := fpkill(PID, 0) <> 0;
end;

procedure TSimbaNativeInterface_Darwin.TerminateProcess(PID: SizeUInt);
begin
  fpkill(PID, SIGKILL);
end;

function TSimbaNativeInterface_Darwin.HighResolutionTime: Double;
begin
  Result := Double((mach_absolute_time * timeInfo.numer) / ((1000 * 1000) * timeInfo.denom));
end;

function TSimbaNativeInterface_Darwin.GetWindows: TWindowHandleArray;
var
  Windows: CFArrayRef;
  I: Integer;
  NormalWindowLevel: Integer;
begin
  Result := Default(TWindowHandleArray);

  Windows := CGWindowListCopyWindowInfo(kCGWindowListOptionOnScreenOnly or kCGWindowListExcludeDesktopElements, kCGNullWindowID);
  NormalWindowLevel := CGWindowLevelForKey(kCGNormalWindowLevelKey);

  for I := 0 to CFArrayGetCount(Windows) - 1 do
    if NSNumber(CFDictionaryGetValue(CFArrayGetValueAtIndex(Windows, I), kCGWindowLayer)).IntValue = NormalWindowLevel then
      Result += [NSNumber(CFDictionaryGetValue(CFArrayGetValueAtIndex(Windows, I), kCGWindowNumber)).unsignedIntValue];

  CFRelease(Windows);
end;

function TSimbaNativeInterface_Darwin.GetWindowChildren(Window: TWindowHandle; Recursive: Boolean): TWindowHandleArray;
var
  I: Integer;
  VirtualWindow: ^TVirtualWindow;
  B: TBox;
  Info: TAXUIWindowInfo;
begin
  Info := AXUI_GetWindowInfo(GetWindowPID(Window));

  if (Length(Info.Children) > 0) and GetWindowBounds(Window, B) then
  begin
    SetLength(Result, Length(Info.Children));
    for I := 0 to High(Info.Children) do
    begin
      VirtualWindow := @Result[I];
      VirtualWindow^.Handle := Window;
      VirtualWindow^.InfoIndex := GetVirtualWindowInfoIndex(Info.Children[I].Bounds.Offset(-B.X1, -B.Y1), Info.Children[I].ClassName);
    end;
  end else
    SetLength(Result, 0);
end;

function TSimbaNativeInterface_Darwin.GetVisibleWindows: TWindowHandleArray;
var
  Window: TWindowHandle;
begin
  Result := Default(TWindowHandleArray);

  for Window in GetWindows() do
    if IsWindowVisible(Window)  then
      Result += [Window];
end;

function TSimbaNativeInterface_Darwin.GetTopWindows: TWindowHandleArray;
begin
  Result := GetWindows();
end;

function TSimbaNativeInterface_Darwin.GetWindowAtCursor(Exclude: TWindowHandleArray): TWindowHandle;
var
  Windows, Childs: TWindowHandleArray;
  MousePos: TPoint;
  I, J, BestIndex: Integer;
  Sum, BestSum: Single;
  B: TBox;
begin
  Result := 0;

  MousePos := GetMousePosition();
  Windows := GetTopWindows();

  for I := 0 to High(Windows) do
  begin
    if (Windows[I] in Exclude) then
      Continue;

    if GetWindowBounds(Windows[I]).Contains(MousePos.X, MousePos.Y) then
    begin
      Result := Windows[I];

      BestSum   := Single.MaxValue;
      BestIndex := -1;

      Childs := Self.GetWindowChildren(Result, True);
      for J := 0 to High(Childs) do
      begin
        B := GetWindowBounds(Childs[J]);
        if not B.Contains(MousePos) then
          Continue;

        Sum := MousePos.DistanceTo(TPoint.Create(B.X1, B.Y1)) +
               MousePos.DistanceTo(TPoint.Create(B.X2, B.Y1)) +
               MousePos.DistanceTo(TPoint.Create(B.X1, B.Y2)) +
               MousePos.DistanceTo(TPoint.Create(B.X2, B.Y2));

        if (Sum < BestSum) then
        begin
          BestSum   := Sum;
          BestIndex := J;
        end;
      end;

      if (BestIndex > -1) then
        Result := Childs[BestIndex];

      Break;
    end;
  end;
end;

function TSimbaNativeInterface_Darwin.GetDesktopWindow: TWindowHandle;
begin
  Result := 2; // kCGDesktopWindowLevelKey
end;

function TSimbaNativeInterface_Darwin.GetActiveWindow: TWindowHandle;
begin
  // TODO: Requires usage of AX API, kAXFocusedApplicationAttribute
  Result := 0;
end;

function TSimbaNativeInterface_Darwin.IsWindowActive(Window: TWindowHandle): Boolean;
begin
  Result := NSWorkspace.sharedWorkspace.frontmostApplication.processIdentifier = GetWindowPID(Window);
end;

function TSimbaNativeInterface_Darwin.IsWindowValid(Window: TWindowHandle): Boolean;
var
  windowIds, windows: CFArrayRef;
  VirtualWindow: TVirtualWindow;
begin
  if IsVirtualWindow(Window, VirtualWindow) then
    Window := VirtualWindow.Handle;

  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, Pointer(Window));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);

  Result := CFArrayGetCount(windows) <> 0;

  CFRelease(windowIds);
  CFRelease(windows);
end;

function TSimbaNativeInterface_Darwin.IsWindowVisible(Window: TWindowHandle): Boolean;
var
  windowIds, windows: CFArrayRef;
begin
  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, Pointer(Window));

  windows := CGWindowListCreateDescriptionFromArray(windowIds);
  if CFArrayGetCount(windows) <> 0 then
    Result := NSNumber(CFDictionaryGetValue(CFArrayGetValueAtIndex(windows, 0), kCGWindowIsOnScreen)).boolValue;

  CFRelease(windowIds);
  CFRelease(windows);
end;

function GetWindowDictionaryValueInt(Window: TWindowHandle; Key: CFStringRef): Integer;
var
  windowIds, windows: CFArrayRef;
  LocalPool: NSAutoReleasePool;
begin
  Result := 0;

  LocalPool := NSAutoReleasePool.alloc.init;
  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, Pointer(Window));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);
  CFRelease(windowIds);

  if CFArrayGetCount(windows) <> 0 then
    Result := NSNumber(CFDictionaryGetValue(CFArrayGetValueAtIndex(windows, 0), Key)).intValue;

  CFRelease(windows);
  LocalPool.release;
end;

function GetWindowDictionaryValueStr(Window: TWindowHandle; Key: CFStringRef): String;
var
  windowIds, windows: CFArrayRef;
  LocalPool: NSAutoReleasePool;
begin
  Result := '';

  LocalPool := NSAutoReleasePool.alloc.init;
  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, Pointer(Window));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);
  CFRelease(windowIds);

  if CFArrayGetCount(windows) <> 0 then
    Result := CFStringToString(CFDictionaryGetValue(CFArrayGetValueAtIndex(windows, 0), Key));

  CFRelease(windows);
  LocalPool.release;
end;

function TSimbaNativeInterface_Darwin.GetWindowPID(Window: TWindowHandle): Integer;
var
  VirtualWindow: TVirtualWindow;
begin
  if IsVirtualWindow(Window, VirtualWindow) then
    Window := VirtualWindow.Handle;

  Result := GetWindowDictionaryValueInt(Window, kCGWindowOwnerPID);
end;

function TSimbaNativeInterface_Darwin.GetWindowClass(Window: TWindowHandle): WideString;
var
  VirtualWindow: TVirtualWindow;
begin
  if IsVirtualWindow(Window, VirtualWindow) then
    Result := FVirtualWindowInfo[VirtualWindow.InfoIndex].ClassStr
  else
    Result := AXUI_GetWindowClass(GetWindowPID(Window));
end;

function TSimbaNativeInterface_Darwin.GetWindowTitle(Window: TWindowHandle): WideString;
begin
  Result := AXUI_GetWindowTitle(GetWindowPID(Window));
end;

function TSimbaNativeInterface_Darwin.GetRootWindow(Window: TWindowHandle): TWindowHandle;
var
  VirtualWindow: TVirtualWindow;
begin
  if IsVirtualWindow(Window, VirtualWindow) then
    Result := VirtualWindow.Handle
  else
    Result := Window;
end;

function TSimbaNativeInterface_Darwin.ActivateWindow(Window: TWindowHandle): Boolean;
begin
  Result := NSRunningApplication.runningApplicationWithProcessIdentifier(GetWindowPID(Window)).activateWithOptions(NSApplicationActivateIgnoringOtherApps);
end;

procedure TSimbaNativeInterface_Darwin.OpenDirectory(Path: String);
begin
  SimbaProcess.RunCommand('open', [Path]);
end;

function TSimbaNativeInterface_Darwin.WindowHandleToStr(Window: TWindowHandle): String;
var
  VirtualWindow: TVirtualWindow;
begin
  if Self.IsVirtualWindow(Window, VirtualWindow) then
  begin
    with Self.FVirtualWindowInfo[VirtualWindow.InfoIndex] do
      Result := 'VirtualWindow[%d, %d,%d,%d,%d, %s]'.Format([VirtualWindow.Handle, ClientRect.X1, ClientRect.Y1, ClientRect.X2, ClientRect.Y2, ClassStr]);
  end else
    Result := inherited;
end;

function TSimbaNativeInterface_Darwin.WindowHandleFromStr(Str: String): TWindowHandle;
var
  VirtualWindow: TVirtualWindow absolute Result;
  Box: TBox;
  ClassStr: String;
begin
  if Str.StartsWith('VirtualWindow') then
  begin
    SScanf(Str, 'VirtualWindow[%d, %d,%d,%d,%d, %s]', [@VirtualWindow.Handle, @Box.X1, @Box.Y1, @Box.X2, @Box.Y2, @ClassStr]);

    VirtualWindow.InfoIndex := Self.GetVirtualWindowInfoIndex(Box, ClassStr);
  end else
    Result := inherited WindowHandleFromStr(Str);
end;

function TSimbaNativeInterface_Darwin.IsVirtualWindow(Window: TWindowHandle; out VirtualWindow: TVirtualWindow): Boolean;
begin
  VirtualWindow := TVirtualWindow(Window);

  Result := VirtualWindow.InfoIndex > 0;
end;

function TSimbaNativeInterface_Darwin.GetVirtualWindowInfoIndex(ClientRect: TBox; ClassStr: ShortString): Integer;
var
  I: Integer;
begin
  Result := Length(Self.FVirtualWindowInfo);
  for I := 0 to Result - 1 do
    if (Self.FVirtualWindowInfo[I].ClientRect = ClientRect) and (Self.FVirtualWindowInfo[I].ClassStr = ClassStr) then
      Exit(I);

  SetLength(FVirtualWindowInfo, Result + 1);
  FVirtualWindowInfo[Result].ClientRect := ClientRect;
  FVirtualWindowInfo[Result].ClassStr := ClassStr;
end;

constructor TSimbaNativeInterface_Darwin.Create;

  procedure MapKey(KeyCode: Integer; KeyChar: NSString; Modifiers: TShiftState);
  var
    Str: String;
  begin
    Str := NSStringToString(keyChar);
    if (Length(Str) = 1) and (Str[1] in [#0..#255]) then
    begin
      if FKeyMap[Str[1]].Exists then
        Exit;

      FKeyMap[Str[1]].KeyCode := KeyCode;
      FKeyMap[Str[1]].Modifiers := Modifiers;
      FKeyMap[Str[1]].Exists := True;
    end;
  end;

var
  Event: NSEvent;
  KeyCode: Integer;
begin
  inherited Create();

  SetLength(FVirtualWindowInfo, 1);

  for KeyCode := 0 to 255 do
  begin
    Event := NSEvent.eventWithCGEvent(CGEventCreateKeyboardEvent(nil, KeyCode, 1));

    if (Event <> nil) then
    begin
      if (Event.Type_ = NSKeyDown) then
      begin
        MapKey(KeyCode, Event.characters, []);
        MapKey(KeyCode, NSEventFix(Event).charactersByApplyingModifiers(NSShiftKeyMask), [ssShift]);
        MapKey(KeyCode, NSEventFix(Event).charactersByApplyingModifiers(NSAlternateKeyMask), [ssAlt]);
        MapKey(KeyCode, NSEventFix(Event).charactersByApplyingModifiers(NSControlKeyMask), [ssCtrl]);
      end;

      CFRelease(Event);
    end;
  end;
end;

initialization
  timeInfo.numer := 0;
  timeInfo.denom := 0;

  mach_timebase_info(timeInfo);

end.

