{%MainUnit oswindow}

uses
  x, xlib, xlibhelpers;

function TOSWindow_Helper.IsVaild: Boolean;
begin
  Result := _XIsWindowVaild(Self);
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := _XGetRootWindow(_XGetActiveWindow()) = _XGetRootWindow(Self);
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

    _XFlush();

    Sleep(5);
  end;

  Exit(False);
end;

function TOSWindow_Helper.IsVisible: Boolean;
begin
  Result := _XGetWindowVisible(Self);
end;

function TOSWindow_Helper.GetPID: UInt32;
begin
  Result := _XGetWindowPID(Self);
end;

function TOSWindow_Helper.GetRootWindow: TOSWindow;
begin
  Result := _XGetRootWindow(Self);
end;

function TOSWindow_Helper.GetClassName: String;
begin
  Result := _XGetWindowClass(Self);
end;

function TOSWindow_Helper.GetTitle: String;
begin
  Result := _XGetWindowTitle(Self);
end;

function TOSWindow_Helper.GetBounds: TBox;
var
  Attributes: TXWindowAttributes;
  _: TWindow;
begin
  _XSync(False);
  _XGetWindowAttributes(Self, Attributes);
  _XTranslateCoordinates(Self, Attributes.Root, -Attributes.Border_Width, -Attributes.Border_Width, Result.X1, Result.Y1, _);

  Result.X2 := Result.X1 + Attributes.Width;
  Result.Y2 := Result.Y1 + Attributes.Height;
end;

function TOSWindow_Helper.GetChildren: TOSWindowArray;
begin
  Result := _XGetChildren(Self);
end;

procedure TOSWindow_Helper.SetBounds(Box: TBox);
begin
  _XMoveResizeWindow(Self, Box.X1, Box.Y1, Box.X2 - Box.X1, Box.Y2 - Box.Y1);
  _XSync(False);
end;

function TOSWindow_Helper.Activate: Boolean;
begin
  _XSetActiveWindow(_XGetRootWindow(Self));
  _XSync(False);

  Result := Self.IsActive(1000);
end;

procedure TOSWindow_Helper.Kill;
begin
  _XKillClient(Self);
  _XSync(False);
end;

function GetVisibleWindows: TOSWindowArray;
var
  Window: TWindow;
begin
  SetLength(Result, 0);

  for Window in _XGetChildren(_XGetDesktopWindow()) do
    if _XGetWindowVisible(Window) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Window;
    end;
end;

function GetWindows: TOSWindowArray;
begin
  Result := _XGetChildren(_XGetDesktopWindow());
end;

function GetActiveWindow: TOSWindow;
begin
  Result := _XGetActiveWindow();
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := _XGetDesktopWindow();
end;
