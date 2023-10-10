{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}

// tccutil reset All com.villavu.simba

unit simba.nativeinterface_darwin;

{$i simba.inc}
{$MODESWITCH OBJECTIVEC2}

interface

uses
  Classes, SysUtils, Graphics, MacOSAll,
  simba.mufasatypes, simba.nativeinterface;

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
    FKeyMapBuilt: Boolean;
    FVirtualWindowInfo: array of record
      ClientRect: TBox;
      ClassStr: ShortString;
    end;

    function IsVirtualWindow(Window: TWindowHandle; out VirtualWindow: TVirtualWindow): Boolean;
    function GetVirtualWindowInfoIndex(ClientRect: TBox; ClassStr: ShortString): Integer;

    procedure BuildKeyMap;
    procedure CheckAccessibility;
  public
    constructor Create;

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

    function WindowHandleToStr(Window: TWindowHandle): String; override;
    function WindowHandleFromStr(Str: String): TWindowHandle; override;
  end;

implementation

uses
  BaseUnix, Unix, LCLType, CocoaAll, CocoaUtils, DateUtils,
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
  timeInfo: TTimebaseInfoData = (numer: 0; denom: 0);

function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): Int64; cdecl; external 'libc';
function mach_absolute_time: QWORD; cdecl; external 'libc';

type
  proc_taskinfo = record
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

  proc_bsdinfo = record
    pbi_flags: uint32;
    pbi_status: uint32;
    pbi_xstatus: uint32;
    pbi_pid: uint32;
    pbi_ppid: uint32;
    pbi_uid: int32;
    pbi_gid: int32;
    pbi_ruid: int32;
    pbi_rgid: int32;
    pbi_svuid: int32;
    pbi_svgid: int32;
    rfu_1: uint32;
    pbi_comm: array[1..16] of Char;
    pbi_name: array[1..32] of Char;
    pbi_nfiles: uint32;
    pbi_pgid: uint32;
    pbi_pjobc: uint32;
    e_tdev: uint32;
    e_tpgid: uint32;
    pbi_nice: int32;
    pbi_start_tvsec: uint64;
    pbi_start_tvusec: uint64;
  end;

const
  PROC_PIDTBSDINFO = 3;
  PROC_PIDTASKINFO = 4;

function proc_pidpath(pid: longint; buffer: pbyte; bufferSize: longword): longint; cdecl; external 'libproc';
function proc_pidinfo(pid: longint; flavor: longint; arg: UInt64; buffer: pointer; buffersize: longint): longint; cdecl; external 'libproc';

function SimbaToMacKeycode(KeyCode: EKeyCode): Integer;
begin
  case KeyCode of
    EKeyCode.BACK:                Result := $33;
    EKeyCode.TAB:                 Result := $30;
    EKeyCode.CLEAR:               Result := $47;
    EKeyCode.RETURN:              Result := $24;
    EKeyCode.SHIFT:               Result := $38;
    EKeyCode.CONTROL:             Result := $3B;
    EKeyCode.MENU:                Result := $3A;
    EKeyCode.CAPITAL:             Result := $39;
    EKeyCode.ESCAPE:              Result := $35;
    EKeyCode.SPACE:               Result := $31;
    EKeyCode.PRIOR:               Result := $74;
    EKeyCode.NEXT:                Result := $79;
    EKeyCode.END_KEY:             Result := $77;
    EKeyCode.HOME:                Result := $73;
    EKeyCode.LEFT:                Result := $7B;
    EKeyCode.UP:                  Result := $7E;
    EKeyCode.RIGHT:               Result := $7C;
    EKeyCode.DOWN:                Result := $7D;
    EKeyCode.DELETE:              Result := $75;
    EKeyCode.HELP:                Result := $72;
    EKeyCode.NUM_0:               Result := $1D;
    EKeyCode.NUM_1:               Result := $12;
    EKeyCode.NUM_2:               Result := $13;
    EKeyCode.NUM_3:               Result := $14;
    EKeyCode.NUM_4:               Result := $15;
    EKeyCode.NUM_5:               Result := $17;
    EKeyCode.NUM_6:               Result := $16;
    EKeyCode.NUM_7:               Result := $1A;
    EKeyCode.NUM_8:               Result := $1C;
    EKeyCode.NUM_9:               Result := $19;
    EKeyCode.A:                   Result := $00;
    EKeyCode.B:                   Result := $0B;
    EKeyCode.C:                   Result := $08;
    EKeyCode.D:                   Result := $02;
    EKeyCode.E:                   Result := $0E;
    EKeyCode.F:                   Result := $03;
    EKeyCode.G:                   Result := $05;
    EKeyCode.H:                   Result := $04;
    EKeyCode.I:                   Result := $22;
    EKeyCode.J:                   Result := $26;
    EKeyCode.K:                   Result := $28;
    EKeyCode.L:                   Result := $25;
    EKeyCode.M:                   Result := $2E;
    EKeyCode.N:                   Result := $2D;
    EKeyCode.O:                   Result := $1F;
    EKeyCode.P:                   Result := $23;
    EKeyCode.Q:                   Result := $0C;
    EKeyCode.R:                   Result := $0F;
    EKeyCode.S:                   Result := $01;
    EKeyCode.T:                   Result := $11;
    EKeyCode.U:                   Result := $20;
    EKeyCode.V:                   Result := $09;
    EKeyCode.W:                   Result := $0D;
    EKeyCode.X:                   Result := $07;
    EKeyCode.Y:                   Result := $10;
    EKeyCode.Z:                   Result := $06;
    EKeyCode.LWIN:                Result := $37;
    EKeyCode.RWIN:                Result := $36;
    EKeyCode.APPS:                Result := $3D;
    EKeyCode.NUMPAD_0:            Result := $52;
    EKeyCode.NUMPAD_1:            Result := $53;
    EKeyCode.NUMPAD_2:            Result := $54;
    EKeyCode.NUMPAD_3:            Result := $55;
    EKeyCode.NUMPAD_4:            Result := $56;
    EKeyCode.NUMPAD_5:            Result := $57;
    EKeyCode.NUMPAD_6:            Result := $58;
    EKeyCode.NUMPAD_7:            Result := $59;
    EKeyCode.NUMPAD_8:            Result := $5B;
    EKeyCode.NUMPAD_9:            Result := $5C;
    EKeyCode.MULTIPLY:            Result := $43;
    EKeyCode.ADD:                 Result := $45;
    EKeyCode.SEPARATOR:           Result := $2B; // Separator used will be EKeyCode.COMMA instead of EKeyCode.PERIOD
    EKeyCode.SUBTRACT:            Result := $4E;
    EKeyCode.DECIMAL:             Result := $41;
    EKeyCode.DIVIDE:              Result := $4B;
    EKeyCode.F1:                  Result := $7A;
    EKeyCode.F2:                  Result := $78;
    EKeyCode.F3:                  Result := $63;
    EKeyCode.F4:                  Result := $76;
    EKeyCode.F5:                  Result := $60;
    EKeyCode.F6:                  Result := $61;
    EKeyCode.F7:                  Result := $62;
    EKeyCode.F8:                  Result := $64;
    EKeyCode.F9:                  Result := $65;
    EKeyCode.F10:                 Result := $6D;
    EKeyCode.F11:                 Result := $67;
    EKeyCode.F12:                 Result := $6F;
    EKeyCode.F13:                 Result := $69;
    EKeyCode.F14:                 Result := $6B;
    EKeyCode.F15:                 Result := $71;
    EKeyCode.F16:                 Result := $6A;
    EKeyCode.F17:                 Result := $40;
    EKeyCode.F18:                 Result := $4F;
    EKeyCode.F19:                 Result := $50;
    EKeyCode.F20:                 Result := $5A;
    EKeyCode.LSHIFT:              Result := $38;
    EKeyCode.RSHIFT:              Result := $3C;
    EKeyCode.LCONTROL:            Result := $3B;
    EKeyCode.RCONTROL:            Result := $3E;
    EKeyCode.LMENU:               Result := $3A;
    EKeyCode.RMENU:               Result := $3D;
    EKeyCode.VOLUME_MUTE:         Result := $4A;
    EKeyCode.VOLUME_DOWN:         Result := $49;
    EKeyCode.VOLUME_UP:           Result := $48;
    EKeyCode.OEM_1:               Result := $29;
    EKeyCode.OEM_PLUS:            Result := $45;
    EKeyCode.OEM_COMMA:           Result := $2B;
    EKeyCode.OEM_MINUS:           Result := $1B;
    EKeyCode.OEM_PERIOD:          Result := $2F;
    EKeyCode.OEM_2:               Result := $2C; // /?
    EKeyCode.OEM_3:               Result := $32; // `~
    EKeyCode.OEM_4:               Result := $21; // [{
    EKeyCode.OEM_5:               Result := $2A; // \|
    EKeyCode.OEM_6:               Result := $1E; // ]}
    EKeyCode.OEM_7:               Result := $27; // '"
    EKeyCode.OEM_102:             Result := $2A; // backslash RT-102
  else
    Result := $FFFF;
  end;
end;

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
    Image := CGWindowListCreateImage(RectToCGRect(R), kCGWindowListOptionOnScreenOnly, kCGNullWindowID, kCGWindowImageBoundsIgnoreFraming or kCGWindowImageNominalResolution)
  else
    Image := CGWindowListCreateImage(RectToCGRect(R), kCGWindowListOptionIncludingWindow, Window, kCGWindowImageBoundsIgnoreFraming or kCGWindowImageNominalResolution);

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

procedure TSimbaNativeInterface_Darwin.MouseTeleport(RelativeWindow: TWindowHandle; P: TPoint);
begin
  with GetWindowBounds(RelativeWindow) do
    CGWarpMouseCursorPosition(CGPointMake(X1 + P.X, Y1 + P.Y));
end;

procedure TSimbaNativeInterface_Darwin.MouseScroll(Scrolls: Integer);
var
  ScrollEvent: CGEventRef;
begin
  CheckAccessibility();

  ScrollEvent := CGEventCreateScrollWheelEvent(nil, kCGScrollEventUnitPixel, 1, -Scrolls * 10);
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
  ClickTypeToMouseDownEvent: array[EMouseButton] of Integer = (
    kCGEventLeftMouseDown,
    kCGEventRightMouseDown,
    kCGEventOtherMouseDown,
    kCGEventOtherMouseDown,
    kCGEventOtherMouseDown
  );

  ClickTypeToMouseButton: array[EMouseButton] of Integer = (
    kCGMouseButtonLeft,
    kCGMouseButtonRight,
    kCGMouseButtonCenter,
    3,
    4
  );

  ClickTypeToMouseUpEvent: array[EMouseButton] of Integer = (
    kCGEventLeftMouseUp,
    kCGEventRightMouseUp,
    kCGEventOtherMouseUp,
    kCGEventOtherMouseUp,
    kCGEventOtherMouseUp
  );

procedure TSimbaNativeInterface_Darwin.MouseDown(Button: EMouseButton);
var
  event: CGEventRef;
  eventType, mouseButton: Integer;
begin
  CheckAccessibility();

  eventType := ClickTypeToMouseDownEvent[Button];
  mouseButton := ClickTypeToMouseButton[Button];

  with GetMousePosition() do
    event := CGEventCreateMouseEvent(nil, eventType, CGPointMake(X, Y), mouseButton);

  CGEventPost(kCGSessionEventTap, event);
  CFRelease(event);
end;

procedure TSimbaNativeInterface_Darwin.MouseUp(Button: EMouseButton);
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

function TSimbaNativeInterface_Darwin.MousePressed(Button: EMouseButton): Boolean;
begin
  Result := CGEventSourceButtonState(kCGEventSourceStateCombinedSessionState, ClickTypeToMouseButton[Button]) > 0;
end;

procedure TSimbaNativeInterface_Darwin.KeySend(Text: PChar; TextLen: Integer; SleepTimes: PInt32);
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

  procedure KeyEvent(KeyCode: Integer; Down: Boolean);
  begin
    if Down then
      CGPostKeyboardEvent(0, KeyCode, 1)
    else
      CGPostKeyboardEvent(0, KeyCode, 0);

    DoSleep();
  end;

var
  I: Integer;
  KeyCode: Integer;
  Modifiers: TShiftState;
begin
  BuildKeyMap();

  ShiftDown := False;
  CtrlDown := False;
  AltDown := False;

  try
    for I := 0 to TextLen - 1 do
    begin
      KeyCode := FKeyMap[Text[I]].KeyCode;
      Modifiers := FKeyMap[Text[I]].Modifiers;

      EnsureModifier(ssShift in Modifiers, ShiftDown, EKeyCode.SHIFT);
      EnsureModifier(ssCtrl in Modifiers, CtrlDown, EKeyCode.CONTROL);
      EnsureModifier(ssAlt in Modifiers, AltDown, EKeyCode.MENU);

      KeyEvent(KeyCode, True);
      KeyEvent(KeyCode, False);
    end;
  finally
    EnsureModifier(False, ShiftDown, EKeyCode.SHIFT);
    EnsureModifier(False, CtrlDown, EKeyCode.CONTROL);
    EnsureModifier(False, AltDown, EKeyCode.MENU);
  end;
end;

function TSimbaNativeInterface_Darwin.KeyPressed(Key: EKeyCode): Boolean;
begin
  Result := CGEventSourceKeyState(kCGEventSourceStateCombinedSessionState, SimbaToMacKeycode(Key)) <> 0;
end;

procedure TSimbaNativeInterface_Darwin.KeyDown(Key: EKeyCode);
begin
  CheckAccessibility();

  CGPostKeyboardEvent(0, SimbaToMacKeycode(Key), 1);
end;

procedure TSimbaNativeInterface_Darwin.KeyUp(Key: EKeyCode);
begin
  CGPostKeyboardEvent(0, SimbaToMacKeycode(Key), 0);
end;

function TSimbaNativeInterface_Darwin.GetProcessStartTime(PID: SizeUInt): TDateTime;
var
  info: proc_bsdinfo;
begin
  Result := 0;

  if proc_pidinfo(PID, PROC_PIDTBSDINFO, 0, @info, sizeof(proc_bsdinfo)) > 0 then
    Result := UnixToDateTime(info.pbi_start_tvsec)
end;

function TSimbaNativeInterface_Darwin.GetProcessMemUsage(PID: SizeUInt): Int64;
var
  info: proc_taskinfo;
begin
  Result := 0;

  if proc_pidinfo(PID, PROC_PIDTASKINFO, 0, @info, SizeOf(proc_taskinfo)) > 0 then
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
  Result := kCGDesktopWindowLevelKey;
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
    Result := NSNumber(CFDictionaryGetValue(CFArrayGetValueAtIndex(windows, 0), kCGWindowIsOnScreen)).boolValue
  else
    Result := False;

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

procedure TSimbaNativeInterface_Darwin.BuildKeyMap;

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
  I: Integer;
  Event: NSEvent;
  LocalPool: NSAutoReleasePool;
begin
  if FKeymapBuilt then
    Exit;
  FKeymapBuilt := True;

  CheckAccessibility();

  LocalPool := NSAutoReleasePool.alloc.init();
  for I := 0 to 255 do
  begin
    Event := NSEvent.eventWithCGEvent(CGEventCreateKeyboardEvent(nil, I, 1));

    if (Event <> nil) and (Event.Type_ = NSKeyDown) then
    begin
      MapKey(I, Event.characters, []);
      MapKey(I, NSEventFix(Event).charactersByApplyingModifiers(NSShiftKeyMask), [ssShift]);
      MapKey(I, NSEventFix(Event).charactersByApplyingModifiers(NSAlternateKeyMask), [ssAlt]);
      MapKey(I, NSEventFix(Event).charactersByApplyingModifiers(NSControlKeyMask), [ssCtrl]);
    end;
  end;
  LocalPool.release();
end;

procedure TSimbaNativeInterface_Darwin.CheckAccessibility;
begin
  if (not AXIsProcessTrusted()) then
    SimbaException('Simba needs accessbility privilege on MacOS.' + LineEnding + 'Settings > Security & Privacy Accessibility');
end;

constructor TSimbaNativeInterface_Darwin.Create;
begin
  inherited Create();

  SetLength(FVirtualWindowInfo, 1);
end;

initialization
  mach_timebase_info(timeInfo{%H-});

end.
