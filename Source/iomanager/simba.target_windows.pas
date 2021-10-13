{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
{$i simba.inc}

unit simba.target_windows;

interface

uses
  classes, sysutils, windows,
  simba.mufasatypes, simba.oswindow, simba.target;

type
  TWindowBuffer = class
  protected
    FDC: HDC;
    FBitmap: HBitmap;
    FWidth, FHeight: Int32;
    FData: PRGB32;
    FInfo: TBitmapInfo;
  public
    property DC: HDC read FDC;
    property Data: PRGB32 read FData;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;

    constructor Create(AWidth, AHeight: Int32);
    destructor Destroy; override;
  end;

  TWindowTarget = class(TTarget)
  protected
    FWindow: TOSWindow;
    FAutoFocus: Boolean;
    FDC: HDC;
    FBuffer: TWindowBuffer;

    function GetHandle: PtrUInt; override;
    procedure SetHandle(Value: PtrUInt); override;

    function GetAutoFocus: Boolean; override;
    procedure SetAutoFocus(Value: Boolean); override;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;

    function TargetValid: Boolean; override;

    procedure ActivateClient; override;
    procedure GetMousePosition(out X, Y: Int32); override;
    procedure MoveMouse(X, Y: Int32); override;
    procedure ScrollMouse(X, Y, Lines: Int32); override;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType): Boolean; override;

    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); override;
    procedure HoldKey(Key: Int32); override;
    procedure ReleaseKey(Key: Int32); override;
    function IsKeyHeld(Key: Int32): Boolean; override;
    function GetKeyCode(Character: Char): Int32; override;

    constructor Create;
    constructor Create(Target: TOSWindow);
    destructor Destroy; override;
  end;

implementation

uses
  simba.platformhelpers;

constructor TWindowBuffer.Create(AWidth, AHeight: Int32);
begin
  FWidth := AWidth;
  FHeight := AHeight;

  if (FWidth > 0) and (FHeight > 0) then
  begin
    FInfo := Default(TBitmapInfo);
    FInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    FInfo.bmiHeader.biWidth := FWidth;
    FInfo.bmiHeader.biHeight := -FHeight;
    FInfo.bmiHeader.biPlanes := 1;
    FInfo.bmiHeader.biBitCount := 32;
    FInfo.bmiHeader.biCompression := BI_RGB;

    FDC := CreateCompatibleDC(0);
    FBitmap := CreateDIBSection(FDC, FInfo, DIB_RGB_COLORS, FData, 0, 0);

    SelectObject(FDC, FBitmap);
  end;
end;

destructor TWindowBuffer.Destroy;
begin
  if (FBitmap > 0) then
    DeleteObject(FBitmap);
  if (FDC > 0) then
    DeleteDC(FDC);

  inherited Destroy();
end;

function TWindowTarget.GetHandle: PtrUInt;
begin
  Result := FWindow;
end;

procedure TWindowTarget.SetHandle(Value: PtrUInt);
begin
  if (FDC > 0) then
    ReleaseDC(FWindow, FDC);
  if (Value = 0) then
    Value := GetDesktopWindow();

  FWindow := Value;

  if (FWindow = GetDesktopWindow()) then
    FDC := GetDC(FWindow)
  else
    FDC := GetWindowDC(FWindow);
end;

function TWindowTarget.GetAutoFocus: Boolean;
begin
  Result := FAutoFocus;
end;

procedure TWindowTarget.SetAutoFocus(Value: Boolean);
begin
  FAutoFocus := Value;
end;

function TWindowTarget.TargetValid: Boolean;
begin
  Result := FWindow.IsValid();
end;

procedure TWindowTarget.ActivateClient;
begin
  FWindow.Activate();
end;

procedure TWindowTarget.GetTargetBounds(out Bounds: TBox);
var
  Attempts: Int32;
begin
  for Attempts := 1 to 5 do
  begin
    if SimbaPlatformHelpers.GetWindowBounds(FWindow, Bounds) then
      Exit;

    Self.InvalidTarget();
  end;

  raise Exception.CreateFmt('Invalid window handle: %d', [FWindow]);
end;

function TWindowTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
begin
  Result := NullReturnData;

  if FAutoFocus then
    ActivateClient();

  GetTargetBounds(Bounds);

  if (FBuffer.Width <> Bounds.Width - 1) or (FBuffer.Height <> Bounds.Height - 1) then
  begin
    FBuffer.Free();
    FBuffer := TWindowBuffer.Create(Bounds.Width - 1, Bounds.Height - 1);
  end;

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    if SimbaPlatformHelpers.GetWindowImage(FWindow, FDC, FBuffer.DC, X, Y, Width, Height) then
    begin
      Result.Ptr := FBuffer.Data;
      Result.IncPtrWith := FBuffer.Width - Width;
      Result.RowLen := FBuffer.Width;
    end;
  end;
end;

function TWindowTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Bounds: TBox;
  Buffer: TWindowBuffer;
begin
  Result := nil;

  if FAutoFocus then
    ActivateClient();

  GetTargetBounds(Bounds);

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    Buffer := TWindowBuffer.Create(Width, Height);

    if SimbaPlatformHelpers.GetWindowImage(FWindow, FDC, FBuffer.DC, X, Y, Width, Height) then
    begin
      Result := GetMem(Width * Height * 4);

      Move(Buffer.Data^, Result^, Width * Height * 4);
    end;

    Buffer.Free();
  end;
end;

procedure TWindowTarget.GetMousePosition(out X, Y: Int32);
var
  Position: TPoint;
begin
  Position := SimbaPlatformHelpers.GetMousePosition(FWindow);

  X := Position.X;
  Y := Position.Y;

  if FMouseClientAreaSet then
  begin
    X := X - FMouseClientArea.X1;
    Y := Y - FMouseClientArea.Y1;
  end;
end;

procedure TWindowTarget.MoveMouse(X, Y: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  SimbaPlatformHelpers.SetMousePosition(FWindow, TPoint.Create(X, Y));
end;

procedure TWindowTarget.ScrollMouse(X, Y, Lines: Int32);
var
  Input: TInput;
  RealMousePos: TPoint;
begin
  MoveMouse(X, Y);

  GetCursorPos(RealMousePos); // unscaled etc

  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := RealMousePos.X;
  Input.mi.dy := RealMousePos.Y;
  Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_WHEEL;
  Input.mi.mouseData := -Lines * WHEEL_DELTA;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWindowTarget.HoldMouse(X, Y: Int32; Button: TClickType);
var
  Input: TInput;
  RealMousePos: TPoint;
begin
  MoveMouse(X, Y);

  GetCursorPos(RealMousePos); // unscaled etc

  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := RealMousePos.X;
  Input.mi.dy := RealMousePos.Y;

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

procedure TWindowTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
var
  Input: TInput;
  RealMousePos: TPoint;
begin
  MoveMouse(X, Y);

  GetCursorPos(RealMousePos); // unscaled etc

  Input := Default(TInput);
  Input._Type := INPUT_MOUSE;
  Input.mi.dx := RealMousePos.X;
  Input.mi.dy := RealMousePos.Y;

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

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  case Button of
    MOUSE_LEFT:    Result := (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0);
    MOUSE_MIDDLE:  Result := (GetAsyncKeyState(VK_MBUTTON) and $8000 <> 0);
    MOUSE_RIGHT:   Result := (GetAsyncKeyState(VK_RBUTTON) and $8000 <> 0);
    MOUSE_EXTRA_1: Result := (GetAsyncKeyState(VK_XBUTTON1) and $8000 <> 0);
    MOUSE_EXTRA_2: Result := (GetAsyncKeyState(VK_XBUTTON2) and $8000 <> 0);
  end;
end;

procedure TWindowTarget.SendString(Text: String; KeyWait, KeyModWait: Int32);
var
  i: Int32;
  Code: Byte;
  ScanCode, VK: Word;
  Modifiers: TShiftState;
begin
  if FAutoFocus then
    ActivateClient();

  for i := 1 to Length(Text) do
  begin
    VK := VKKeyScan(Text[i]);
    Modifiers := TShiftState(VK shr 8 and $FF);
    Code := VK and $FF;
    ScanCode := MapVirtualKey(Code, 0);
    if (ScanCode = 0) then
      raise Exception.Create('Unknown key code: ' + Text[i]);

    if (Modifiers <> []) then
    begin
      if ssShift in Modifiers then Keybd_Event(VK_SHIFT,   $2A, 0, 0);
      if ssCtrl  in Modifiers then Keybd_Event(VK_CONTROL, $2A, 0, 0);
      if ssAlt   in Modifiers then Keybd_Event(VK_MENU,    $2A, 0, 0);

      Sleep(KeyModWait div 2);
    end;

    Keybd_Event(Code, ScanCode, 0, 0);

    if (KeyWait <> 0) then
      Sleep(KeyWait);

    Keybd_Event(Code, ScanCode, KEYEVENTF_KEYUP, 0);

    if (Modifiers <> []) then
    begin
      Sleep(KeyModWait div 2);

      if ssAlt   in Modifiers then Keybd_Event(VK_MENU,    $2A, KEYEVENTF_KEYUP, 0);
      if ssCtrl  in Modifiers then Keybd_Event(VK_CONTROL, $2A, KEYEVENTF_KEYUP, 0);
      if ssShift in Modifiers then Keybd_Event(VK_SHIFT,   $2A, KEYEVENTF_KEYUP, 0);
    end;
  end;
end;

procedure TWindowTarget.HoldKey(Key: Int32);
var
  Input: TInput;
begin
  if FAutoFocus then
    ActivateClient();

  Input := Default(TInput);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.dwFlags := 0;
  Input.ki.wVk := Key;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWindowTarget.ReleaseKey(Key: Int32);
var
  Input: TInput;
begin
  if FAutoFocus then
    ActivateClient();

  Input := Default(TInput);
  Input._Type := INPUT_KEYBOARD;
  Input.ki.dwFlags := KEYEVENTF_KEYUP;
  Input.ki.wVk := Key;

  SendInput(1, @Input, SizeOf(Input));
end;

function TWindowTarget.IsKeyHeld(Key: Int32): Boolean;
begin
  Result := (GetAsyncKeyState(Key) and $8000 <> 0); //only check if high-order bit is set
end;

function TWindowTarget.GetKeyCode(Character: Char): Int32;
begin
  Result := VKKeyScan(Character) and $FF;
end;

constructor TWindowTarget.Create;
begin
  inherited Create();

  FBuffer := TWindowBuffer.Create(0, 0);
end;

constructor TWindowTarget.Create(Target: TOSWindow);
begin
  Create();

  Handle := Target;
end;

destructor TWindowTarget.Destroy;
begin
  FBuffer.Free();

  inherited Destroy();
end;

end.

