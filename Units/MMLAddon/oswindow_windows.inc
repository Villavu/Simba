{%MainUnit oswindow}

uses
  windows;

function TOSWindow_Helper.IsVaild: Boolean;
begin
  Result := IsWindow(Self);
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := GetForegroundWindow() = GetAncestor(Self, GA_ROOT);
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

    Sleep(5);
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

function TOSWindow_Helper.GetClassName: String;
var
  Buffer: array[0..255] of Char;
begin
  Windows.GetClassName(Self, @Buffer[0], Length(Buffer));

  Result := String(Buffer);
end;

function TOSWindow_Helper.GetTitle: String;
var
  Buffer: array[0..255] of Char;
begin
  GetWindowText(Self, @Buffer[0], Length(Buffer));

  Result := String(Buffer);
end;

function TOSWindow_Helper.GetBounds: TBox;
var
  R: TRect;
begin
  R := Default(TRect);

  GetWindowRect(Self, R);

  Result.X1 := R.Left;
  Result.Y1 := R.Top;
  Result.X2 := R.Right;
  Result.Y2 := R.Bottom;
end;

function __GetChildren(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
var
  Arr: ^TOSWindowArray absolute Data;
begin
  SetLength(Arr^, Length(Arr^) + 1);
  Arr^[High(Arr^)] := Window;

  Exit(True);
end;

function TOSWindow_Helper.GetChildren: TOSWindowArray;
begin
  EnumChildWindows(Self, @__GetChildren, LParam(@Result));
end;

procedure TOSWindow_Helper.SetBounds(Bounds: TBox);
begin
  SetWindowPos(Self, 0, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1, SWP_NOACTIVATE or SWP_NOZORDER);
end;

function TOSWindow_Helper.Activate: Boolean;
var
  Current, Thread, PID: UInt32;
begin
  if (GetForegroundWindow() <> Self) then
  begin
    Current := GetWindowThreadProcessID(GetForegroundWindow(), PID);
    Thread := GetCurrentThreadID;

    if (Current <> Thread) then
      AttachThreadInput(Current, Thread, True);

    BringWindowToTop(Self);
    ShowWindow(Self, SW_SHOW);

    if (Current <> Thread) then
      AttachThreadInput(Current, Thread, False);
  end;

  Result := Self.IsActive(1000);
end;

procedure TOSWindow_Helper.Kill;
var
  Handle: THandle;
begin
  Handle := OpenProcess(PROCESS_TERMINATE, False, Self.GetPID());
  TerminateProcess(Handle, 0);
  CloseHandle(Handle);
end;

function __GetVisibleWindows(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
var
  Arr: ^TOSWindowArray absolute Data;
begin
  if IsWindowVisible(Window) then
  begin
    EnumChildWindows(Window, @__GetChildren, Data);

    SetLength(Arr^, Length(Arr^) + 1);
    Arr^[High(Arr^)] := Window;
  end;

  Exit(True);
end;

function GetVisibleWindows: TOSWindowArray;
begin
  EnumWindows(@__GetVisibleWindows, LParam(@Result));
end;

function __GetWindows(Window: HWND; Data: LPARAM): WINBOOL; stdcall;
var
  Arr: ^TOSWindowArray absolute Data;
begin
  SetLength(Arr^, Length(Arr^) + 1);
  Arr^[High(Arr^)] := Window;

  EnumChildWindows(Window, @__GetChildren, Data);

  Exit(True);
end;

function GetWindows: TOSWindowArray;
begin
  EnumWindows(@__GetWindows, LParam(@Result));
end;

function GetActiveWindow: TOSWindow;
begin
  Result := GetForegroundWindow();
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := Windows.GetDesktopWindow();
end;
