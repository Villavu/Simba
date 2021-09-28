{%MainUnit simba.oswindow}

uses
  x, baseunix,
  simba.xlib, simba.xlib_helpers, simba.xlib_display;

function TOSWindow_Helper.IsValid: Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := XGetWindowAttributes(GetDisplay(), Self, @Attributes) <> 0;
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := XGetRootWindow(GetDisplay(), XGetActiveWindow(GetDisplay())) = XGetRootWindow(GetDisplay(), Self);
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
  Result := XHasWindowProperty(GetDisplay(), XGetRootWindow(GetDisplay(), Self), 'WM_STATE');
end;

function TOSWindow_Helper.GetPID: UInt32;
begin
  Result := XGetWindowProperty(GetDisplay(), XGetRootWindow(GetDisplay(), Self), XInternAtom(GetDisplay(), PChar('_NET_WM_PID'), TBool(False)));
end;

function TOSWindow_Helper.GetRootWindow: TOSWindow;
begin
  Result := XGetRootWindow(GetDisplay(), Self);
end;

function TOSWindow_Helper.GetClassName: WideString;
begin
  Result := XGetWindowClass(GetDisplay(), Self);
end;

function TOSWindow_Helper.GetTitle: WideString;
begin
  Result := XGetWindowTitle(GetDisplay(), Self);
end;

function TOSWindow_Helper.GetBounds(out Bounds: TBox): Boolean;
var
  Child: TWindow;
  X, Y: Int32;
  Attributes: TXWindowAttributes;
begin
  Result := (XGetWindowAttributes(GetDisplay(), Self, @Attributes) <> 0) and
            (XTranslateCoordinates(GetDisplay(), Self, Attributes.Root, -Attributes.Border_Width, -Attributes.Border_Width, @X, @Y, @Child) <> 0);

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
  Result := XGetChildren(GetDisplay(), Self, Recursive);
end;

procedure TOSWindow_Helper.SetBounds(Bounds: TBox);
begin
  XMoveResizeWindow(GetDisplay(), Self, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1);
  XSync(GetDisplay(), 0);
end;

function TOSWindow_Helper.Activate: Boolean;
begin
  XSetActiveWindow(GetDisplay(), Self.GetRootWindow());

  Result := Self.IsActive(1000);
end;

procedure TOSWindow_Helper.Kill;
begin
  fpkill(Self.GetPID(), SIGKILL);
end;

function GetWindows: TOSWindowArray;
var
  Window: TOSWindow;
begin
  Window := XDefaultRootWindow(GetDisplay());

  Result := Window.GetChildren();
end;

function GetVisibleWindows: TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in GetWindows() do
    if Window.IsVisible() then
      Result += [Window];
end;

function GetActiveWindow: TOSWindow;
begin
  Result := XGetActiveWindow(GetDisplay());
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := XDefaultRootWindow(GetDisplay());
end;

function TOSWindow_Helper.GetRelativeCursorPos: TPoint;
var
  Root: TWindow;
  x_root, y_root: Int32;
  mask: UInt32;
begin
  Result := Default(TPoint);

  XQueryPointer(GetDisplay(), Self, @Root, @Result, @x_root, @y_root, @Result.X, @Result.Y, @mask);
end;

function GetWindowAtCursor: TOSWindow;
var
  Root, Child: TWindow;
  x_root, y_root, x, y: Int32;
  mask: UInt32;
begin
  XQueryPointer(GetDisplay(), XDefaultRootWindow(GetDisplay()), @Root, @Result, @x_root, @y_root, @x, @y, @mask);

  Child := Result;

  while (Child <> 0) do
  begin
    Result := Child;

    XQueryPointer(GetDisplay(), Result, @Root, @Child, @x_root, @y_root, @x, @y, @mask);
  end;
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
      Windows := XGetChildren(GetDisplay(), Window, False);

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

  Windows := XGetChildren(GetDisplay(), GetDesktopWindow, False);
  for i := High(Windows) downto 0 do
    if IsVisible(Windows[i]) then
      Result += [Windows[i]];
end;
