unit simba.windows_helpers;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  classes, sysutils, windows, dwmapi, multimon,
  simba.mufasatypes;

type
  TSimbaWindowsHelpers = record
  private
    procedure ApplyDPI(Window: HWND; var X1, Y1, X2, Y2: Int32);
    procedure RemoveDPI(Window: HWND; var X1, Y1, X2, Y2: Int32);
  public
    function IsScaledAndDPIAware(Window: HWND): Boolean;
    function GetWindowImage(Window: HWND; WindowDC, BufferDC: HDC; X, Y, Width, Height: Int32): Boolean;
    function GetWindowBounds(Window: HWND; out Bounds: TBox): Boolean;
    function GetMousePosition(Window: HWND): TPoint;
    procedure SetMousePosition(Window: HWND; Position: TPoint);
  end;

var
  SimbaWindowsHelpers: TSimbaWindowsHelpers;

implementation

type
  MONITOR_DPI_TYPE = (
    MDT_EFFECTIVE_DPI = 0,
    MDT_ANGULAR_DPI   = 1,
    MDT_RAW_DPI       = 2
  );

  DPI_AWARENESS_CONTEXT = type THandle;

const
  DPI_AWARENESS_CONTEXT_UNAWARE              = DPI_AWARENESS_CONTEXT(-1);
  DPI_AWARENESS_CONTEXT_SYSTEM_AWARE         = DPI_AWARENESS_CONTEXT(-2);
  DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE    = DPI_AWARENESS_CONTEXT(-3);
  DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2 = DPI_AWARENESS_CONTEXT(-4);
  DPI_AWARENESS_CONTEXT_UNAWARE_GDISCALED    = DPI_AWARENESS_CONTEXT(-5);

  MONITOR_DEFAULTTONULL    = $00000000;
  MONITOR_DEFAULTTOPRIMARY = $00000001;
  MONITOR_DEFAULTTONEAREST = $00000002;

var
  GetWindowDpiAwarenessContext: function(Window: HWND): DPI_AWARENESS_CONTEXT; stdcall;
  AreDpiAwarenessContextsEqual: function(A: DPI_AWARENESS_CONTEXT; B: DPI_AWARENESS_CONTEXT): LongBool; stdcall; // ^___-
  GetDpiForMonitor: function(Monitor: HMONITOR; dpiType: MONITOR_DPI_TYPE; out dpiX: UINT; out dpiY: UINT): HRESULT; stdcall;

  DPIEnabled: Boolean;

procedure TSimbaWindowsHelpers.ApplyDPI(Window: HWND; var X1, Y1, X2, Y2: Int32);
var
  DPI: record X, Y: UINT; end;
begin
  if DPIEnabled and AreDpiAwarenessContextsEqual(GetWindowDpiAwarenessContext(Window), DPI_AWARENESS_CONTEXT_UNAWARE) then
  begin
    GetDpiForMonitor(MonitorFromWindow(Window, MONITOR_DEFAULTTONEAREST), MDT_EFFECTIVE_DPI, DPI.X, DPI.Y);

    X1 := Round(X1 * (DPI.X / 96));
    Y1 := Round(Y1 * (DPI.Y / 96));
    X2 := Round(X2 * (DPI.X / 96));
    Y2 := Round(Y2 * (DPI.Y / 96));
  end;
end;

procedure TSimbaWindowsHelpers.RemoveDPI(Window: HWND; var X1, Y1, X2, Y2: Int32);
var
  DPI: record X, Y: UINT; end;
begin
  if DPIEnabled and AreDpiAwarenessContextsEqual(GetWindowDpiAwarenessContext(Window), DPI_AWARENESS_CONTEXT_UNAWARE) then
  begin
    GetDpiForMonitor(MonitorFromWindow(Window, MONITOR_DEFAULTTONEAREST), MDT_EFFECTIVE_DPI, DPI.X, DPI.Y);

    X1 := Round(X1 / (DPI.X / 96));
    Y1 := Round(Y1 / (DPI.Y / 96));
    X2 := Round(X2 / (DPI.X / 96));
    Y2 := Round(Y2 / (DPI.Y / 96));
  end;
end;

function TSimbaWindowsHelpers.IsScaledAndDPIAware(Window: HWND): Boolean;
var
  DPI: record X, Y: UINT; end;
begin
  Result := DPIEnabled and (not AreDpiAwarenessContextsEqual(GetWindowDpiAwarenessContext(Window), DPI_AWARENESS_CONTEXT_UNAWARE)) and
                           (GetDpiForMonitor(MonitorFromWindow(Window, MONITOR_DEFAULTTONEAREST), MDT_EFFECTIVE_DPI, DPI.X, DPI.Y) = S_OK) and ((DPI.X <> 96) or (DPI.Y <> 96));
end;

function TSimbaWindowsHelpers.GetWindowBounds(Window: HWND; out Bounds: TBox): Boolean;
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

function TSimbaWindowsHelpers.GetMousePosition(Window: HWND): TPoint;
var
  Bounds: TBox;
  _: Int32;
begin
  if not GetCursorPos(Result) then
    Exit(Default(TPoint));

  Self.GetWindowBounds(Window, Bounds);
  Self.RemoveDPI(Window, _, _, Result.X, Result.Y);

  Result.X := Result.X - Bounds.X1;
  Result.Y := Result.Y - Bounds.Y1;
end;

procedure TSimbaWindowsHelpers.SetMousePosition(Window: HWND; Position: TPoint);
var
  Bounds: TBox;
begin
  if Self.GetWindowBounds(Window, Bounds) then
  begin
    Self.ApplyDPI(Window, Bounds.X1, Bounds.Y1, Position.X, Position.Y);

    SetCursorPos(Bounds.X1 + Position.X, Bounds.Y1 + Position.Y);
  end;
end;

function GetDesktopOffset(Handle: THandle; DC: HDC; Rect: PRect; Data: LPARAM): LongBool; stdcall;
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

function TSimbaWindowsHelpers.GetWindowImage(Window: HWND; WindowDC, BufferDC: HDC; X, Y, Width, Height: Int32): Boolean;

  // BitBlt uses GetWindowRect area so must offset to real bounds if DwmCompositionEnabled.
  procedure ApplyRootOffset(Window: HWND; var X, Y: Int32);
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
  procedure ApplyDesktopOffset(DC: HDC; var X, Y: Int32);
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

begin
  if (Window = GetAncestor(Window, GA_ROOT)) then
    ApplyRootOffset(Window, X, Y);
  if (Window = GetDesktopWindow()) then
    ApplyDesktopOffset(WindowDC, X, Y);

  Result := BitBlt(BufferDC, 0, 0, Width, Height, WindowDC, X, Y, SRCCOPY);
end;

procedure InitDPI;
begin
  Pointer(GetWindowDpiAwarenessContext) := GetProcAddress(LoadLibrary('user32'), 'GetWindowDpiAwarenessContext');
  Pointer(AreDpiAwarenessContextsEqual) := GetProcAddress(LoadLibrary('user32'), 'AreDpiAwarenessContextsEqual');
  Pointer(GetDpiForMonitor) := GetProcAddress(LoadLibrary('shcore'), 'GetDpiForMonitor');

  DPIEnabled := Assigned(GetWindowDpiAwarenessContext) and Assigned(AreDpiAwarenessContextsEqual) and Assigned(MonitorFromWindow) and Assigned(GetDpiForMonitor);
end;

initialization
  InitDwmLibrary();
  InitDPI();

end.

