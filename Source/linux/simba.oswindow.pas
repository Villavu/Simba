{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Linux implementation of TOSWindow
}
unit simba.oswindow;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.platformhelpers, simba.mufasatypes;

{$i oswindow_header.inc}

implementation

uses
  regexpr, x,
  simba.xlib;

function TOSWindow_Helper.IsValid: Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := SimbaXLib.XGetWindowAttributes(Self, @Attributes) <> 0;
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := SimbaXLib.XGetRootWindow(SimbaXLib.XGetActiveWindow()) = SimbaXLib.XGetRootWindow(Self);
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
  Result := SimbaXLib.XHasWindowProperty(SimbaXLib.XGetRootWindow(Self), 'WM_STATE');
end;

function TOSWindow_Helper.GetPID: UInt32;
begin
  Result := SimbaXLib.XGetWindowProperty(SimbaXLib.XGetRootWindow(Self), SimbaXLib.XInternAtom(PChar('_NET_WM_PID'), TBool(False)));
end;

function TOSWindow_Helper.GetRootWindow: TOSWindow;
begin
  Result := SimbaXLib.XGetRootWindow(Self);
end;

function TOSWindow_Helper.GetClassName: WideString;
begin
  Result := SimbaXLib.XGetWindowClass(Self);
end;

function TOSWindow_Helper.GetTitle: WideString;
begin
  Result := SimbaXLib.XGetWindowTitle(Self);
end;

function TOSWindow_Helper.GetBounds(out Bounds: TBox): Boolean;
var
  Child: TWindow;
  X, Y: Int32;
  Attributes: TXWindowAttributes;
begin
  Result := (SimbaXLib.XGetWindowAttributes(Self, @Attributes) <> 0) and
            (SimbaXLib.XTranslateCoordinates(Self, Attributes.Root, -Attributes.Border_Width, -Attributes.Border_Width, @X, @Y, @Child) <> 0);

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
  Result := SimbaXLib.XGetChildren(Self, Recursive);
end;

procedure TOSWindow_Helper.SetBounds(Bounds: TBox);
begin
  SimbaXLib.XMoveResizeWindow(Self, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1);
  SimbaXLib.XSync(0);
end;

function TOSWindow_Helper.Activate: Boolean;
begin
  SimbaXLib.XSetActiveWindow(Self.GetRootWindow());

  Result := Self.IsActive(1000);
end;

procedure TOSWindow_Helper.Kill;
begin
  SimbaPlatformHelpers.TerminateProcess(Self.GetPID());
end;

function GetWindows: TOSWindowArray;
var
  Window: TOSWindow;
begin
  Window := SimbaXLib.XDefaultRootWindow();

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
  Result := SimbaXLib.XGetActiveWindow();
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := SimbaXLib.XDefaultRootWindow();
end;

function TOSWindow_Helper.GetRelativeCursorPos: TPoint;
var
  Root: TWindow;
  x_root, y_root: Int32;
  mask: UInt32;
begin
  Result := Default(TPoint);

  SimbaXLib.XQueryPointer(Self, @Root, @Result, @x_root, @y_root, @Result.X, @Result.Y, @mask);
end;

function GetWindowAtCursor: TOSWindow;
var
  Root, Child: TWindow;
  x_root, y_root, x, y: Int32;
  mask: UInt32;
begin
  SimbaXLib.XQueryPointer(SimbaXLib.XDefaultRootWindow(), @Root, @Result, @x_root, @y_root, @x, @y, @mask);

  Child := Result;

  while (Child <> 0) do
  begin
    Result := Child;

    SimbaXLib.XQueryPointer(Result, @Root, @Child, @x_root, @y_root, @x, @y, @mask);
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
      Windows := SimbaXLib.XGetChildren(Window, False);

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

  Windows := SimbaXLib.XGetChildren(GetDesktopWindow, False);
  for i := High(Windows) downto 0 do
    if IsVisible(Windows[i]) then
      Result += [Windows[i]];
end;

{$i oswindow_body.inc}

end.

