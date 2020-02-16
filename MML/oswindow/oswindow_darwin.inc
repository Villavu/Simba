{%MainUnit simba.oswindow}

uses
  baseunix, CocoaAll,
  macosall, simba.darwin_inputhelpers;

function TOSWindow_Helper.IsValid: Boolean;
var
  windowIds, windows: CFArrayRef;
begin
  windowIds := CFArrayCreateMutable(nil, 1, nil);
  CFArrayAppendValue(windowIds, UnivPtr(Self));
  windows := CGWindowListCreateDescriptionFromArray(windowIds);
  CFRelease(windowIds);
  Result := CFArrayGetCount(windows) <> 0;
  CFRelease(windows);
end;

function TOSWindow_Helper.IsActive: Boolean;
begin
  Result := isWindowActive(Self);
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
  Result := GetWindowInfo(Self).onScreen; //No other way unless we check if any windows are on top of it..
end;

function TOSWindow_Helper.GetPID: UInt32;
begin
  Result := GetWindowInfo(Self).ownerPid;
end;

function TOSWindow_Helper.GetRootWindow: TOSWindow;
begin
  Result := Self;   //Not sure..
end;

function TOSWindow_Helper.GetClassName: WideString;
begin
  Result := ''; //No such thing on MacOS..
end;

function TOSWindow_Helper.GetTitle: WideString;
begin
  Result := GetWindowInfo(Self).title;
end;

function TOSWindow_Helper.GetBounds(out Bounds: TBox): Boolean;
var
  info: TWindowInfo;
begin
  info := GetWindowInfo(Self);
  if info.id <> high(CGWindowID) then
  begin
    Bounds.X1 := info.x;
    Bounds.Y1 := info.y;
    Bounds.X2 := info.x + info.width;
    Bounds.Y2 := info.y + info.height;
    Exit(True);
  end;

  Bounds.X1 := -1;
  Bounds.Y1 := -1;
  Bounds.X2 := -1;
  Bounds.Y2 := -1;
  Exit(False);
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
  Result := GetChildWindows(Self);
end;

procedure TOSWindow_Helper.SetBounds(Bounds: TBox);
begin
  SetWindowBoundsAX(Self, Bounds.X1, Bounds.Y1, Bounds.X2 - Bounds.X1, Bounds.Y2 - Bounds.Y1);
end;

function TOSWindow_Helper.Activate: Boolean;
begin
  SetWindowActive(Self);
  Result := Self.IsActive(1000);
end;

procedure TOSWindow_Helper.Kill;
begin
  fpkill(Self.GetPID(), SIGTERM);
end;

function GetWindows: TOSWindowArray;
begin
  Result := GetOnScreenWindows();
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
  Result := GetActiveWindowAX();
end;

function GetDesktopWindow: TOSWindow;
begin
  Result := 2; //Always 2.. but we can get it with APis if needed..
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
      Windows := GetChildWindows(Window);
      for i := 0 to High(Windows) do
      begin
        Window := Windows[i];
        if Window.IsVisible() then
          Exit(True);
      end;
    end;
  end;
var
  Windows: TWindowArray;
  i: Int32;
begin
  SetLength(Result, 0);
  Windows := GetOnScreenWindows();

  for i := 0 to High(Windows) do
    if IsVisible(TOSWindow(Windows[i])) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := TOSWindow(Windows[i]);
    end;
end;

function GetWindowAtCursor: TOSWindow;
var
  Desktop: TBox;
  Windows: TOSWindowArray;
  Child: TOSWindow;
  X, Y: Int32;
  B: TBox;
  I: Int32;
begin
  Result := 0;

  GetCursorPos(X, Y);

  Desktop := GetDesktopWindow().GetBounds();
  Windows := GetTopWindows();

  for I := 1 to High(Windows) do
  begin
    B := Windows[I].GetBounds();
    if (B.X1 = Desktop.X1) and (B.Y1 = Desktop.Y1) and (B.X2 = Desktop.X2) and (B.Y2 = Desktop.Y2) then
      Continue;

    if (X > B.X1) and (Y > B.Y1) and (X < B.X2) and (Y < B.Y2) then
    begin
      Result := Windows[I];
      for Child in Result.GetChildren() do
      begin
        B := Child.GetBounds();
        if (X > B.X1) and (Y > B.Y1) and (X < B.X2) and (Y < B.Y2) then
          Result := Child;
      end;

      Break;
    end;
  end;
end;
