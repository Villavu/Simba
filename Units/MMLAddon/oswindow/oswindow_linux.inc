{%MainUnit simba.oswindow}

uses
  x, baseunix,
  simba.xlib, simba.xlib_helpers;

function TOSWindow_Helper.IsValid: Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := XGetWindowAttributes(DefaultDisplay, Self, @Attributes) <> 0;
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := XGetRootWindow(DefaultDisplay, XGetActiveWindow(DefaultDisplay)) = XGetRootWindow(DefaultDisplay, Self);
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
  Result := XHasWindowProperty(DefaultDisplay, XGetRootWindow(DefaultDisplay, Self), 'WM_STATE');
end;

function TOSWindow_Helper.GetPID: UInt32;
begin
  Result := XGetWindowProperty(DefaultDisplay, XGetRootWindow(DefaultDisplay, Self), XInternAtom(DefaultDisplay, PChar('_NET_WM_PID'), False));
end;

function TOSWindow_Helper.GetRootWindow: TOSWindow;
begin
  Result := XGetRootWindow(DefaultDisplay, Self);
end;

function TOSWindow_Helper.GetClassName: WideString;
begin
  Result := UTF8Decode(XGetWindowClass(DefaultDisplay, Self));
end;

function TOSWindow_Helper.GetTitle: WideString;
begin
  Result := UTF8Decode(XGetWindowTitle(DefaultDisplay, Self));
end;

function TOSWindow_Helper.GetBounds(out Bounds: TBox): Boolean;
var
  Child: TWindow;
  X, Y: Int32;
  Attributes: TXWindowAttributes;
begin
  Result := (XGetWindowAttributes(DefaultDisplay, Self, @Attributes) <> 0) and
            (XTranslateCoordinates(DefaultDisplay, Self, Attributes.Root, -Attributes.Border_Width, -Attributes.Border_Width, @X, @Y, @Child) <> 0);

  if Result then
  begin
    Bounds.X1 := X;
    Bounds.Y1 := Y;
    Bounds.X2 := Bounds.X1 + Attributes.Width;
    Bounds.Y2 := Bounds.Y1 + Attributes.Height;
  end;
end;

function TOSWindow_Helper.GetBounds: TBox;
begin
  if (not GetBounds(Result)) then
  begin
    Result.X1 := -1;
    Result.Y1 := -1;
    Result.X2 := -1;
    Result.Y2 := -1;
  end;
end;

function TOSWindow_Helper.GetChildren(Recursive: Boolean): TOSWindowArray;
begin
  Result := XGetChildren(DefaultDisplay, Self, Recursive);
end;

procedure TOSWindow_Helper.SetBounds(Bounds: TBox);
begin
  XMoveResizeWindow(DefaultDisplay, Self, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1);
  XSync(DefaultDisplay, 0);
end;

function TOSWindow_Helper.Activate: Boolean;
begin
  XSetActiveWindow(DefaultDisplay, Self.GetRootWindow());

  Result := Self.IsActive(1000);
end;

procedure TOSWindow_Helper.Kill;
begin
  fpkill(Self.GetPID(), SIGTERM);
end;

function GetWindows: TOSWindowArray;
var
  Window: TOSWindow;
begin
  Window := XDefaultRootWindow(DefaultDisplay);

  Result := Window.GetChildren();
end;

function GetVisibleWindows: TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in GetWindows() do
    if Window.IsVisible() then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Window;
    end;
end;

function GetActiveWindow: TOSWindow;
begin
  Result := XGetActiveWindow(DefaultDisplay);
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := XDefaultRootWindow(DefaultDisplay);
end;

function GetTopWindows: TOSWindowArray;

  function IsVisible(var Window: TOSWindow): Boolean;
  var
    Windows: TOSWindowArray;
    i: Int32;
  begin
    Result := Window.IsVisible();

    if (not Result) then
    begin
      Windows := XGetChildren(DefaultDisplay, Window, False);

      for i := High(Windows) downto 0 do
      begin
        Window := Windows[i];
        if Window.IsVisible() then
          Exit(True);
      end;
    end;
  end;

var
  Windows: TOSWindowArray;
  i: Int32;
begin
  SetLength(Result, 0);

  Windows := XGetChildren(DefaultDisplay, GetDesktopWindow, FAlse);
  for i := High(Windows) downto 0 do
    if IsVisible(Windows[i]) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Windows[i];
    end;
end;
