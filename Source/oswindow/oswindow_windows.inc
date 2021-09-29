{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{%MainUnit simba.oswindow}

uses
  windows, dwmapi;

function TOSWindow_Helper.IsValid: Boolean;
begin
  Result := IsWindow(Self);
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := (GetForegroundWindow() = Self.GetRootWindow()) or (Self = GetDesktopWindow());
end;

function TOSWindow_Helper.IsActive(Time: Int32): Boolean;
var
  T: UInt64;
begin
  T := GetTickCount64() + Time;
  while (T > GetTickCount64()) do
  begin
    if Self.IsActive() then
      Exit(True);

    Sleep(25);
  end;

  Exit(False);
end;

function TOSWindow_Helper.IsVisible: Boolean;
begin
  Result := IsWindowVisible(Self);
end;

function TOSWindow_Helper.GetPID: UInt32;
begin
  GetWindowThreadProcessId(Self, @Result);
end;

function TOSWindow_Helper.GetRootWindow: TOSWindow;
begin
  Result := GetAncestor(Self, GA_ROOT);
end;

function TOSWindow_Helper.GetClassName: WideString;
begin
  SetLength(Result, 255);
  SetLength(Result, GetClassNameW(Self, PWideChar(Result), Length(Result)));
end;

function TOSWindow_Helper.GetTitle: WideString;
begin
  SetLength(Result, 255);
  SetLength(Result, GetWindowTextW(Self, PWideChar(Result), Length(Result)));
end;

function TOSWindow_Helper.GetBounds(out Bounds: TBox): Boolean;
var
  R: TRect;
begin
  if (Self = GetDesktopWindow()) then
  begin
    Bounds.X1 := GetSystemMetrics(SM_XVIRTUALSCREEN);
    Bounds.Y1 := GetSystemMetrics(SM_YVIRTUALSCREEN);
    Bounds.X2 := GetSystemMetrics(SM_CXVIRTUALSCREEN);
    Bounds.Y2 := GetSystemMetrics(SM_CYVIRTUALSCREEN);

    if Bounds.X1 < 0 then
      Bounds.X2 += Bounds.X1;
    if Bounds.Y1 < 0 then
      Bounds.Y2 += Bounds.Y1;

    Exit(True);
  end else
  begin
    if (Self = Self.GetRootWindow()) and DwmCompositionEnabled then
      Result := DwmGetWindowAttribute(Self, DWMWA_EXTENDED_FRAME_BOUNDS, @R, SizeOf(TRect)) = S_OK
    else
      Result := GetWindowRect(Self, R);
  end;

  if Result then
  begin
    Bounds.X1 := R.Left;
    Bounds.Y1 := R.Top;
    Bounds.X2 := R.Right;
    Bounds.Y2 := R.Bottom;
  end;
end;

function TOSWindow_Helper.GetBounds: TBox;
begin
  if Self.GetBounds(Result) then
    Exit;

  Result.X1 := -1;
  Result.Y1 := -1;
  Result.X2 := -1;
  Result.Y2 := -1;
end;

type
  PEnumWindowData = ^TEnumWindowData;
  TEnumWindowData = record
    Windows: TOSWindowArray;
    Recursive: Boolean;
    Parent: TOSWindow;
  end;

function __GetChildren(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if Recursive or (not Recursive and (GetAncestor(Window, GA_PARENT) = Parent)) then
      Windows += [Window];
  end;
end;

function TOSWindow_Helper.GetChildren(Recursive: Boolean): TOSWindowArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);
  Data.Parent := Self;
  Data.Recursive := Recursive;

  EnumChildWindows(Self, @__GetChildren, PtrUInt(@Data));

  Result := Data.Windows;
end;

procedure TOSWindow_Helper.SetBounds(Bounds: TBox);
begin
  SetWindowPos(Self, 0, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1, SWP_NOACTIVATE or SWP_NOZORDER);
end;

function TOSWindow_Helper.Activate: Boolean;
var
  Current, Thread, PID: UInt32;
  Root: TOSWindow;
begin
  if Self.IsActive() then
    Exit(True);

  Root := GetRootWindow();
  Current := GetWindowThreadProcessID(GetForegroundWindow(), PID);
  Thread := GetCurrentThreadID();

  if (Current <> Thread) then
    AttachThreadInput(Current, Thread, True);

  BringWindowToTop(Root);
  ShowWindow(Root, SW_SHOW);

  if (Current <> Thread) then
    AttachThreadInput(Current, Thread, False);

  Result := Self.IsActive(500);
end;

procedure TOSWindow_Helper.Kill;
var
  Handle: THandle;
begin
  Handle := OpenProcess(PROCESS_TERMINATE, False, Self.GetPID());
  TerminateProcess(Handle, 0);
  CloseHandle(Handle);
end;

function TOSWindow_Helper.GetRelativeCursorPos: TPoint;
begin
  Result := Default(TPoint);

  if GetCursorPos(Result) then
    with Self.GetBounds() do
    begin
      Result.X := Result.X - X1;
      Result.Y := Result.Y - Y1;
    end;
end;

function __GetVisibleWindows(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if IsWindowVisible(Window) and (not IsIconic(Window)) then
    begin
      Windows += [Window];

      EnumChildWindows(Window, @__GetChildren, Data);
    end;
  end;
end;

function GetVisibleWindows: TOSWindowArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);
  Data.Recursive := True;

  EnumWindows(@__GetVisibleWindows, PtrUInt(@Data));

  Result := Data.Windows;
end;

function __GetWindows(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    Windows += [Window];

    EnumChildWindows(Window, @__GetChildren, Data);
  end;
end;

function GetWindows: TOSWindowArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);
  Data.Recursive := True;

  EnumWindows(@__GetWindows, PtrUInt(@Data));

  Result := Data.Windows;
end;

function GetActiveWindow: TOSWindow;
begin
  Result := GetForegroundWindow();
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := Windows.GetDesktopWindow();
end;

function GetWindowAtCursor: TOSWindow;
var
  Cursor: TPoint;
begin
  Result := 0;

  if GetCursorPos(Cursor) then
    Result := WindowFromPoint(Cursor);
end;

function __GetTopWindows(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
begin
  Result := True;

  with PEnumWindowData(Data)^ do
  begin
    if IsWindowVisible(Window) and (not IsIconic(Window)) and (GetAncestor(Window, GA_ROOT) = Window) then
      Windows += [Window];
  end;
end;

function GetTopWindows: TOSWindowArray;
var
  Data: TEnumWindowData;
begin
  Data := Default(TEnumWindowData);

  EnumWindows(@__GetTopWindows, PtrUInt(@Data));

  Result := Data.Windows;
end;
