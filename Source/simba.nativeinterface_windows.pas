{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.nativeinterface_windows;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes, simba.nativeinterface;

type
  TSimbaNativeInterface_Windows = class(TSimbaNativeInterface)
  protected
    FDPILoaded: Boolean;
    FDWMLoaded: Boolean;
    FHasWaitableTimer: Boolean;
    FWaitableTimer: THandle;

    procedure ApplyDPI(Window: TWindowHandle; var X1, Y1, X2, Y2: Integer);
    procedure RemoveDPI(Window: TWindowHandle; var X1, Y1, X2, Y2: Integer);
  public
    constructor Create;
    destructor Destroy; override;

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
    procedure PreciseSleep(Milliseconds: UInt32); override;

    procedure OpenDirectory(Path: String); override;

    procedure PlaySound(Path: String); override;
    procedure StopSound; override;

    procedure ShowTerminal; override;
    procedure HideTerminal; override;
  end;

implementation

uses
  windows, jwapsapi, dwmapi, multimon, mmsystem,
  simba.process;

type
  MONITOR_DPI_TYPE = (
    MDT_EFFECTIVE_DPI = 0,
    MDT_ANGULAR_DPI   = 1,
    MDT_RAW_DPI       = 2
  );

  DPI_AWARENESS_CONTEXT = type TWindowHandle;

const
  DPI_AWARENESS_CONTEXT_UNAWARE                   = DPI_AWARENESS_CONTEXT(-1);
  {%H-}DPI_AWARENESS_CONTEXT_SYSTEM_AWARE         = DPI_AWARENESS_CONTEXT(-2);
  {%H-}DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE    = DPI_AWARENESS_CONTEXT(-3);
  {%H-}DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 = DPI_AWARENESS_CONTEXT(-4);
  {%H-}DPI_AWARENESS_CONTEXT_UNAWARE_GDISCALED    = DPI_AWARENESS_CONTEXT(-5);

  {%H-}MONITOR_DEFAULTTONULL    = $00000000;
  {%H-}MONITOR_DEFAULTTOPRIMARY = $00000001;
  MONITOR_DEFAULTTONEAREST = $00000002;

var
  GetWindowDpiAwarenessContext: function(Window: TWindowHandle): DPI_AWARENESS_CONTEXT; stdcall;
  AreDpiAwarenessContextsEqual: function(A: DPI_AWARENESS_CONTEXT; B: DPI_AWARENESS_CONTEXT): LongBool; stdcall; // ^___-
  GetDpiForMonitor: function(Monitor: HMONITOR; dpiType: MONITOR_DPI_TYPE; out dpiX: UINT; out dpiY: UINT): HRESULT; stdcall;

const
  CREATE_WAITABLE_TIMER_HIGH_RESOLUTION = $00000002;

  TIMER_QUERY_STATE  = $0001;
  TIMER_MODIFY_STATE = $0002;
  TIMER_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or TIMER_QUERY_STATE or TIMER_MODIFY_STATE;

var
  CreateWaitableTimerExW: function(lpTimerAttributes: LPSECURITY_ATTRIBUTES; lpTimerName: LPCWSTR; dwFlags: DWORD; dwDesiredAccess: DWORD): THANDLE; stdcall;

constructor TSimbaNativeInterface_Windows.Create;
begin
  Pointer(GetWindowDpiAwarenessContext) := GetProcAddress(LoadLibrary('user32'), 'GetWindowDpiAwarenessContext');
  Pointer(AreDpiAwarenessContextsEqual) := GetProcAddress(LoadLibrary('user32'), 'AreDpiAwarenessContextsEqual');
  Pointer(GetDpiForMonitor) := GetProcAddress(LoadLibrary('shcore'), 'GetDpiForMonitor');

  FDPILoaded := Assigned(GetWindowDpiAwarenessContext) and Assigned(AreDpiAwarenessContextsEqual) and Assigned(MonitorFromWindow) and Assigned(GetDpiForMonitor);
  FDWMLoaded := InitDwmLibrary();

  Pointer(CreateWaitableTimerExW) := GetProcAddress(LoadLibrary('kernel32'), 'CreateWaitableTimerExW');

  FHasWaitableTimer := Assigned(CreateWaitableTimerExW);
  if FHasWaitableTimer then
  begin
    FWaitableTimer := CreateWaitableTimerExW(nil, nil, CREATE_WAITABLE_TIMER_HIGH_RESOLUTION, TIMER_ALL_ACCESS);
    if (FWaitableTimer = 0) then
      FWaitableTimer := CreateWaitableTimerExW(nil, nil, 0, TIMER_ALL_ACCESS);

    FHasWaitableTimer := FWaitableTimer <> 0;
  end;
end;

destructor TSimbaNativeInterface_Windows.Destroy;
begin
  if FHasWaitableTimer then
    CloseHandle(FWaitableTimer);

  inherited Destroy();
end;

procedure TSimbaNativeInterface_Windows.ApplyDPI(Window: TWindowHandle; var X1, Y1, X2, Y2: Integer);
var
  DPI: record X, Y: UINT; end;
begin
  if FDPILoaded and AreDpiAwarenessContextsEqual(GetWindowDpiAwarenessContext(Window), DPI_AWARENESS_CONTEXT_UNAWARE) then
  begin
    GetDpiForMonitor(MonitorFromWindow(Window, MONITOR_DEFAULTTONEAREST), MDT_EFFECTIVE_DPI, DPI.X, DPI.Y);

    X1 := Round(X1 * (DPI.X / 96));
    Y1 := Round(Y1 * (DPI.Y / 96));
    X2 := Round(X2 * (DPI.X / 96));
    Y2 := Round(Y2 * (DPI.Y / 96));
  end;
end;

procedure TSimbaNativeInterface_Windows.RemoveDPI(Window: TWindowHandle; var X1, Y1, X2, Y2: Integer);
var
  DPI: record X, Y: UINT; end;
begin
  if FDPILoaded and AreDpiAwarenessContextsEqual(GetWindowDpiAwarenessContext(Window), DPI_AWARENESS_CONTEXT_UNAWARE) then
  begin
    GetDpiForMonitor(MonitorFromWindow(Window, MONITOR_DEFAULTTONEAREST), MDT_EFFECTIVE_DPI, DPI.X, DPI.Y);

    X1 := Round(X1 / (DPI.X / 96));
    Y1 := Round(Y1 / (DPI.Y / 96));
    X2 := Round(X2 / (DPI.X / 96));
    Y2 := Round(Y2 / (DPI.Y / 96));
  end;
end;

function TSimbaNativeInterface_Windows.GetWindowBounds(Window: TWindowHandle; out Bounds: TBox): Boolean;
var
  R: TRect;
begin
  if (Window = GetDesktopWindow()) then
  begin
    Bounds.X1 := GetSystemMetrics(SM_XVIRTUALSCREEN);
    Bounds.Y1 := GetSystemMetrics(SM_YVIRTUALSCREEN);
    Bounds.X2 := GetSystemMetrics(SM_XVIRTUALSCREEN) + GetSystemMetrics(SM_CXVIRTUALSCREEN);
    Bounds.Y2 := GetSystemMetrics(SM_YVIRTUALSCREEN) + GetSystemMetrics(SM_CYVIRTUALSCREEN);

    Result := True;
  end else
  begin
    if (Window = GetAncestor(Window, GA_ROOT)) and DwmCompositionEnabled then
      Result := DwmGetWindowAttribute(Window, DWMWA_EXTENDED_FRAME_BOUNDS, @R, SizeOf(TRect)) = S_OK
    else
      Result := GetWindowRect(Window, R);

    if Result then
    begin
      Bounds.X1 := R.Left;
      Bounds.Y1 := R.Top;
      Bounds.X2 := R.Right;
      Bounds.Y2 := R.Bottom;

      Self.RemoveDPI(Window, Bounds.X1, Bounds.Y1, Bounds.X2, Bounds.Y2);
    end;
  end;
end;

function TSimbaNativeInterface_Windows.GetWindowBounds(Window: TWindowHandle): TBox;
begin
  if not GetWindowBounds(Window, Result) then
    Result := Box(0, 0, 0, 0);
end;

procedure TSimbaNativeInterface_Windows.SetWindowBounds(Window: TWindowHandle; Bounds: TBox);

  procedure ApplyDWMOffset;
  var
    R: array[0..1] of TRect;
  begin
    if (Window = GetAncestor(Window, GA_ROOT)) and GetWindowRect(Window, R[0]) and
       DwmCompositionEnabled() and (DwmGetWindowAttribute(Window, DWMWA_EXTENDED_FRAME_BOUNDS, @R[1], SizeOf(TRect)) = S_OK) then
    begin
      Bounds.X1 += R[0].Left - R[1].Left;
      Bounds.Y1 += R[0].Top - R[1].Top;
      Bounds.X2 += R[0].Right - R[1].Right;
      Bounds.Y2 += R[0].Bottom - R[1].Bottom;
    end;
  end;

begin
  ApplyDWMOffset();

  SetWindowPos(Window, 0, Bounds.X1, Bounds.Y1, Bounds.Width, Bounds.Height, SWP_NOACTIVATE or SWP_NOZORDER);
end;

function GetDesktopOffset(Handle: HMONITOR; DC: HDC; Rect: PRect; Data: LPARAM): LongBool; stdcall;
begin
  with PPoint(Data)^ do
  begin
    if Rect^.Left < X then
      X := Rect^.Left;
    if Rect^.Top < Y then
      Y := Rect^.Top;
  end;

  Result := True;
end;

function TSimbaNativeInterface_Windows.GetWindowImage(Window: TWindowHandle; X, Y, Width, Height: Integer; var ImageData: PRGB32): Boolean;

  // BitBlt uses GetWindowRect area so must offset to real bounds if DwmCompositionEnabled.
  procedure ApplyRootOffset(Window: TWindowHandle; var X, Y: Integer);
  var
    R: array[0..1] of TRect;
  begin
    if DwmCompositionEnabled() and (DwmGetWindowAttribute(Window, DWMWA_EXTENDED_FRAME_BOUNDS, @R[0], SizeOf(TRect)) = S_OK) then
      if GetWindowRect(Window, R[1]) then
      begin
        Inc(X, R[0].Left - R[1].Left);
        Inc(Y, R[0].Top - R[1].Top);
      end;
  end;

  // Monitors on left of primary will be in negative coord space.
  procedure ApplyDesktopOffset(DC: HDC; var X, Y: Integer);
  var
    Offset: TPoint;
  begin
    Offset := Default(TPoint);

    if EnumDisplayMonitors(DC, nil, @GetDesktopOffset, PtrInt(@Offset)) then
    begin
      Inc(X, Offset.X);
      Inc(Y, Offset.Y);
    end;
  end;

var
  WindowDC, MemoryDC: HDC;
  MemoryBitmap: HBITMAP;
  BitmapInfo: TBitmapInfo;
  PreviousObject: HGDIOBJ;
begin
  if (Window = GetDesktopWindow()) then
  begin
    WindowDC := GetDC(GetDesktopWindow());

    ApplyDesktopOffset(WindowDC, X, Y);
  end else
  begin
    WindowDC := GetWindowDC(Window);
    if (Window = GetAncestor(Window, GA_ROOT)) then
      ApplyRootOffset(Window, X, Y);
  end;

  MemoryDC := CreateCompatibleDC(WindowDC);
  MemoryBitmap := CreateCompatibleBitmap(WindowDC, Width, Height);

  PreviousObject := SelectObject(MemoryDC, MemoryBitmap);

  Result := BitBlt(MemoryDC, 0, 0, Width, Height, WindowDC, X, Y, SRCCOPY);
  if Result then
  begin
    BitmapInfo := Default(TBitmapInfo);
    BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfo);
    BitmapInfo.bmiHeader.biWidth := Width;
    BitmapInfo.bmiHeader.biHeight := -Height;
    BitmapInfo.bmiHeader.biPlanes := 1;
    BitmapInfo.bmiHeader.biBitCount := BitSizeOf(TRGB32);
    BitmapInfo.bmiHeader.biCompression := BI_RGB;

    GetDIBits(MemoryDC, MemoryBitmap, 0, Height, ReAllocMem(ImageData, Width * Height * SizeOf(TRGB32)), BitmapInfo, DIB_RGB_COLORS);
  end;

  SelectObject(MemoryDC, PreviousObject);

  DeleteDC(MemoryDC);
  DeleteObject(MemoryBitmap);

  ReleaseDC(Window, WindowDC);
end;

function TSimbaNativeInterface_Windows.GetMousePosition: TPoint;
var
  _: Integer;
begin
  if not GetCursorPos(Result) then
    Exit(Default(TPoint));

  Self.RemoveDPI(GetDesktopWindow(), _, _, Result.X, Result.Y);
end;

function TSimbaNativeInterface_Windows.GetMousePosition(Window: TWindowHandle): TPoint;
var
  Bounds: TBox;
  _: Integer;
begin
  if not GetCursorPos(Result) then
    Exit(Default(TPoint));

  Self.GetWindowBounds(Window, Bounds);
  Self.RemoveDPI(Window, _, _, Result.X, Result.Y);

  Result.X := Result.X - Bounds.X1;
  Result.Y := Result.Y - Bounds.Y1;
end;

procedure TSimbaNativeInterface_Windows.SetMousePosition(Window: TWindowHandle; Position: TPoint);
var
  Bounds: TBox;
begin
  if Self.GetWindowBounds(Window, Bounds) then
  begin
    Self.ApplyDPI(Window, Bounds.X1, Bounds.Y1, Position.X, Position.Y);

    SetCursorPos(Bounds.X1 + Position.X, Bounds.Y1 + Position.Y);
  end;
end;

procedure TSimbaNativeInterface_Windows.ScrollMouse(Lines: Integer);
var
  Input: TInput;
  P: TPoint;
begin
  GetCursorPos(P); // unscaled on purpose

  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := P.X;
  Input.mi.dy := P.Y;
  Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_WHEEL;
  Input.mi.mouseData := -Lines * WHEEL_DELTA;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TSimbaNativeInterface_Windows.HoldMouse(Button: TClickType);
var
  Input: TInput;
  P: TPoint;
begin
  GetCursorPos(P); // unscaled on purpose

  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := P.X;
  Input.mi.dy := P.Y;

  case Button of
    MOUSE_LEFT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN;
    MOUSE_MIDDLE: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN;
    MOUSE_RIGHT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN;
    MOUSE_EXTRA_1:
      begin
        Input.mi.mouseData := XBUTTON1;
        Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_XDOWN;
      end;
    MOUSE_EXTRA_2:
      begin
        Input.mi.mouseData := XBUTTON2;
        Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_XDOWN;
      end;
  end;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TSimbaNativeInterface_Windows.ReleaseMouse(Button: TClickType);
var
  Input: TInput;
  P: TPoint;
begin
  GetCursorPos(P); // unscaled on purpose

  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := P.X;
  Input.mi.dy := P.Y;

  case Button of
    MOUSE_LEFT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP;
    MOUSE_MIDDLE: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP;
    MOUSE_RIGHT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP;
    MOUSE_EXTRA_1:
      begin
        Input.mi.mouseData := XBUTTON1;
        Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_XUP;
      end;
    MOUSE_EXTRA_2:
      begin
        Input.mi.mouseData := XBUTTON2;
        Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_XUP;
      end;
  end;

  SendInput(1, @Input, SizeOf(Input));
end;

function TSimbaNativeInterface_Windows.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  case Button of
    MOUSE_LEFT:    Result := (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0);
    MOUSE_MIDDLE:  Result := (GetAsyncKeyState(VK_MBUTTON) and $8000 <> 0);
    MOUSE_RIGHT:   Result := (GetAsyncKeyState(VK_RBUTTON) and $8000 <> 0);
    MOUSE_EXTRA_1: Result := (GetAsyncKeyState(VK_XBUTTON1) and $8000 <> 0);
    MOUSE_EXTRA_2: Result := (GetAsyncKeyState(VK_XBUTTON2) and $8000 <> 0);
  end;
end;

function TSimbaNativeInterface_Windows.IsKeyHeld(Key: Integer): Boolean;
begin
  Result := (GetAsyncKeyState(Key) and $8000 <> 0); //only check if high-order bit is set
end;

procedure TSimbaNativeInterface_Windows.HoldKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0);
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.dwFlags := KEYEVENTF_SCANCODE;
  Input.ki.wScan := MapVirtualKey(KeyCode, 0);

  SendInput(1, @Input, SizeOf(Input));

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

procedure TSimbaNativeInterface_Windows.ReleaseKeyNativeKeyCode(KeyCode: Integer; WaitTime: Integer = 0);
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.dwFlags := KEYEVENTF_KEYUP or KEYEVENTF_SCANCODE;
  Input.ki.wScan := MapVirtualKey(KeyCode, 0);

  SendInput(1, @Input, SizeOf(Input));

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

function TSimbaNativeInterface_Windows.VirtualKeyToNativeKeyCode(VirtualKey: Integer): Integer;
begin
  Result := VirtualKey;
end;

function TSimbaNativeInterface_Windows.GetNativeKeyCodeAndModifiers(Character: Char; out Code: Integer; out Modifiers: TShiftState): Boolean;
begin
  Result := True;

  Code := VKKeyScan(Character) and $FF;
  Modifiers := TShiftState(VKKeyScan(Character) shr 8 and $FF);
end;

procedure TSimbaNativeInterface_Windows.HoldKey(VirtualKey: Integer; WaitTime: Integer);
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.dwFlags := 0;
  Input.ki.wVk := VirtualKey;

  SendInput(1, @Input, SizeOf(Input));

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

procedure TSimbaNativeInterface_Windows.ReleaseKey(VirtualKey: Integer; WaitTime: Integer);
var
  Input: TInput;
begin
  Input := Default(TInput);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.dwFlags := KEYEVENTF_KEYUP;
  Input.ki.wVk := VirtualKey;

  SendInput(1, @Input, SizeOf(Input));

  if (WaitTime > 0) then
    Sleep(WaitTime);
end;

function TSimbaNativeInterface_Windows.GetProcessMemUsage(PID: SizeUInt): Int64;
var
  Handle: THandle;
  pmcEx: PROCESS_MEMORY_COUNTERS_Ex;
  pmc: PROCESS_MEMORY_COUNTERS absolute pmcEx;
begin
  Result := 0;

  Handle := OpenProcess(PROCESS_VM_READ or PROCESS_QUERY_INFORMATION, False, PID);
  if (Handle = 0) then
    Exit;

  pmcEx := Default(PROCESS_MEMORY_COUNTERS_Ex);
  pmcEx.cb := SizeOf(PROCESS_MEMORY_COUNTERS_Ex);

  if GetProcessMemoryInfo(Handle, pmc, SizeOf(pmcEx)) then
    Result := pmcEx.PrivateUsage;

  CloseHandle(Handle);
end;

function TSimbaNativeInterface_Windows.GetProcessPath(PID: SizeUInt): String;
var
  Buffer: array[1..MAX_PATH] of Char;
  BufferSize: UInt32 = MAX_PATH;
  Handle: THandle;
begin
  Result := '';

  Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
  if (Handle > 0) then
  begin
    if QueryFullProcessImageNameA(Handle, 0, @Buffer[1], @BufferSize) then
      Result := Copy(Buffer, 1, BufferSize);

    CloseHandle(Handle);
  end;
end;

function TSimbaNativeInterface_Windows.IsProcess64Bit(PID: SizeUInt): Boolean;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  Handle: THandle;
  SystemInfo: TSystemInfo;
  Wow64Process: LongBool;
begin
  Result := False;

  GetNativeSystemInfo(@SystemInfo);

  if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
  begin
    Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
    if (Handle = 0) then
      Exit;

    Result := IsWow64Process(Handle, @Wow64Process) and (not Wow64Process);

    CloseHandle(Handle);
  end;
end;

function TSimbaNativeInterface_Windows.IsProcessRunning(PID: SizeUInt): Boolean;
var
  Handle: THandle;
  ExitCode: UInt32;
begin
  Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
  if (Handle = 0) then
    Exit(False);

  Result := GetExitCodeProcess(Handle, ExitCode) and (ExitCode = STILL_ACTIVE);

  CloseHandle(Handle);
end;

procedure TSimbaNativeInterface_Windows.TerminateProcess(PID: SizeUInt);
var
  Handle: THandle;
begin
  Handle := OpenProcess(PROCESS_TERMINATE, False, PID);
  if (Handle = 0) then
    Exit;

  Windows.TerminateProcess(Handle, 0);

  CloseHandle(Handle);
end;

type
  PEnumWindowData = ^TEnumWindowData;
  TEnumWindowData = record
    Windows: TWindowHandleArray;
    Recursive: Boolean;
    Parent: TWindowHandle;
  end;

function GetWindowsCallback(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if Recursive or (not Recursive and (GetAncestor(Window, GA_PARENT) = Parent)) then
      Windows += [Window];
  end;
end;

function GetTopWindowsCallback(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if IsWindowVisible(Window) and (not IsIconic(Window)) and (GetAncestor(Window, GA_ROOT) = Window) then
      Windows += [Window];
  end;
end;

function GetChildrenCallback(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if Recursive or (not Recursive and (GetAncestor(Window, GA_PARENT) = Parent)) then
      Windows += [Window];
  end;
end;

function GetVisibleWindowsCallback(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if IsWindowVisible(Window) and (not IsIconic(Window)) then
    begin
      Windows += [Window];

      EnumChildWindows(Window, @GetChildrenCallback, Data);
    end;
  end;
end;

function TSimbaNativeInterface_Windows.GetWindows: TWindowHandleArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);
  Data.Recursive := True;

  EnumWindows(@GetWindowsCallback, PtrUInt(@Data));

  Result := Data.Windows;
end;

function TSimbaNativeInterface_Windows.GetWindowChildren(Window: TWindowHandle; Recursive: Boolean): TWindowHandleArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);
  Data.Parent := Window;
  Data.Recursive := Recursive;

  EnumChildWindows(Window, @GetChildrenCallback, PtrUInt(@Data));

  Result := Data.Windows;
end;

function TSimbaNativeInterface_Windows.GetVisibleWindows: TWindowHandleArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);
  Data.Recursive := True;

  EnumWindows(@GetVisibleWindowsCallback, PtrUInt(@Data));

  Result := Data.Windows;
end;

function TSimbaNativeInterface_Windows.GetTopWindows: TWindowHandleArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);

  EnumWindows(@GetTopWindowsCallback, PtrUInt(@Data));

  Result := Data.Windows;
end;

function TSimbaNativeInterface_Windows.GetWindowAtCursor: TWindowHandle;
var
  Cursor: TPoint;
begin
  Result := 0;
  if GetCursorPos(Cursor) then
    Result := WindowFromPoint(Cursor);
end;

function TSimbaNativeInterface_Windows.GetDesktopWindow: TWindowHandle;
begin
  Result := Windows.GetDesktopWindow();
end;

function TSimbaNativeInterface_Windows.GetActiveWindow: TWindowHandle;
begin
  Result := GetForegroundWindow();
end;

function TSimbaNativeInterface_Windows.IsWindowActive(Window: TWindowHandle): Boolean;
begin
  Result := (GetForegroundWindow() = Self.GetRootWindow(Window)) or (Window = GetDesktopWindow());
end;

function TSimbaNativeInterface_Windows.IsWindowValid(Window: TWindowHandle): Boolean;
begin
  Result := IsWindow(Window);
end;

function TSimbaNativeInterface_Windows.IsWindowVisible(Window: TWindowHandle): Boolean;
begin
  Result := IsWindowVisible(Window) and (not IsIconic(Window))
end;

function TSimbaNativeInterface_Windows.GetWindowPID(Window: TWindowHandle): Integer;
begin
  GetWindowThreadProcessId(Window, @Result);
end;

function TSimbaNativeInterface_Windows.GetWindowClass(Window: TWindowHandle): WideString;
begin
  SetLength(Result, 255);
  SetLength(Result, GetClassNameW(Window, PWideChar(Result), Length(Result)));
end;

function TSimbaNativeInterface_Windows.GetWindowTitle(Window: TWindowHandle): WideString;
begin
  SetLength(Result, 255);
  SetLength(Result, GetWindowTextW(Window, PWideChar(Result), Length(Result)));
end;

function TSimbaNativeInterface_Windows.GetRootWindow(Window: TWindowHandle): TWindowHandle;
begin
  Result := GetAncestor(Window, GA_ROOT);
end;

function TSimbaNativeInterface_Windows.ActivateWindow(Window: TWindowHandle): Boolean;
var
  CurrentThread, TargetThread, PID: UInt32;
  RootWindow: TWindowHandle;
  I: Integer;
begin
  if GetActiveWindow() = Window then
    Exit(True);

  RootWindow := GetRootWindow(Window);
  CurrentThread := GetWindowThreadProcessID(GetForegroundWindow(), PID);
  TargetThread := GetCurrentThreadID();

  if (CurrentThread <> TargetThread) then
    AttachThreadInput(CurrentThread, TargetThread, True);

  BringWindowToTop(RootWindow);
  ShowWindow(RootWindow, SW_SHOW);

  if (CurrentThread <> TargetThread) then
    AttachThreadInput(CurrentThread, TargetThread, False);

  for I := 1 to 10 do
  begin
    if Self.IsWindowActive(Window) then
      Exit(True);

    Sleep(100);
  end;

  Result := False;
end;

function TSimbaNativeInterface_Windows.HighResolutionTime: Double;
var
  Frequency, Count: Int64;
begin
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(Count);

  Result := Count / Frequency * 1000;
end;

procedure TSimbaNativeInterface_Windows.PreciseSleep(Milliseconds: UInt32);
var
  Time: Int64;
begin
  if FHasWaitableTimer then
  begin
    Time := -Round((Milliseconds - 0.25) * 1000) * 10;

    if SetWaitableTimer(FWaitableTimer, Time, 0, nil, nil, False) then
    begin
      WaitForSingleObject(FWaitableTimer, INFINITE);
      Exit;
    end;
  end;

  inherited PreciseSleep(Milliseconds); // Fallback to normal sleep
end;

procedure TSimbaNativeInterface_Windows.OpenDirectory(Path: String);
begin
  SimbaProcess.RunCommand('explorer.exe', ['/root,"' + Path + '"']);
end;

procedure TSimbaNativeInterface_Windows.PlaySound(Path: String);
begin
  sndPlaySound(PChar(Path), SND_ASYNC or SND_NODEFAULT);
end;

procedure TSimbaNativeInterface_Windows.StopSound;
begin
  sndPlaySound(nil, 0);
end;

procedure TSimbaNativeInterface_Windows.ShowTerminal;
var
  PID: UInt32;
begin
  GetWindowThreadProcessId(GetConsoleWindow(), PID);
  if (PID = GetCurrentProcessID()) then
    ShowWindow(GetConsoleWindow(), SW_SHOWNORMAL);
end;

procedure TSimbaNativeInterface_Windows.HideTerminal;
var
  PID: UInt32;
begin
  GetWindowThreadProcessId(GetConsoleWindow(), PID);
  if (PID = GetCurrentProcessID()) then
    ShowWindow(GetConsoleWindow(), SW_HIDE);
end;

end.

