{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

     Windows OS specific implementation for Mufasa Macro Library
}

{$mode objfpc}{$H+}

unit simba.target_windows;

interface

uses
  classes, sysutils, jwawinuser, windows,
  mufasatypes, simba.oswindow, simba.target;

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
    FIsDesktop: Boolean;
    FIsRoot: Boolean;
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
    procedure ScrollMouse(X, Y, Lines : Int32); override;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType) : Boolean; override;

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
  dwmapi;

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

  FWindow := Value;
  FIsDesktop := FWindow = GetDesktopWindow();
  FIsRoot := FWindow = FWindow.GetRootWindow();

  if FIsDesktop then
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
  if FIsDesktop then
  begin
    Bounds.X1 := GetSystemMetrics(SM_XVIRTUALSCREEN);
    Bounds.Y1 := GetSystemMetrics(SM_YVIRTUALSCREEN);
    Bounds.X2 := GetSystemMetrics(SM_XVIRTUALSCREEN) + GetSystemMetrics(SM_CXVIRTUALSCREEN);
    Bounds.Y2 := GetSystemMetrics(SM_YVIRTUALSCREEN) + GetSystemMetrics(SM_CYVIRTUALSCREEN);

    Exit;
  end;

  for Attempts := 1 to 5 do
  begin
    if FWindow.GetBounds(Bounds) then
      Exit;

    Self.InvalidTarget();
  end;

  raise Exception.CreateFmt('Invalid window handle: %d', [FWindow]);
end;

function __GetDesktopOffset(Handle: THandle; DC: HDC; Rect: PRect; Data: LParam): LongBool; stdcall;
begin
  with PPoint(Data)^ do
  begin
    if Rect^.Left < X then
      X := Rect^.Left;
    if Rect^.Top < Y then
      Y := Rect^.Top;
  end;
end;

// If using multi monitors with monitor on the left of the primary it will be in the negative values.
procedure GetDesktopOffset(DC: HWND; var X, Y: Int32);
var
  Offset: TPoint;
begin
  Offset := Default(TPoint);

  if EnumDisplayMonitors(DC, nil, @__GetDesktopOffset, PtrInt(@Offset)) then
  begin
    Inc(X, Offset.X);
    Inc(Y, Offset.Y);
  end;
end;

procedure GetRootOffset(Window: TOSWindow; var X, Y: Int32);
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
    // BitBlt uses GetWindowRect area so must offset to real bounds if DwmCompositionEnabled.
    if FIsRoot then
      GetRootOffset(FWindow, X, Y);

    // Multi monitor support.
    if FIsDesktop then
      GetDesktopOffset(FDC, X, Y);

    if BitBlt(FBuffer.DC, 0, 0, Width, Height, FDC, X, Y, SRCCOPY) then
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

    // BitBlt uses GetWindowRect area so must offset to real bounds if DwmCompositionEnabled.
    if FIsRoot then
      GetRootOffset(FWindow, X, Y);

    // Multi monitor support.
    if FIsDesktop then
      GetDesktopOffset(FDC, X, Y);

    if BitBlt(Buffer.DC, 0, 0, Width, Height, FDC, X, Y, SRCCOPY) then
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
  Bounds: TBox;
begin
  GetCursorPos(Position);

  if FIsDesktop then
  begin
    X := Position.X;
    Y := Position.Y;
  end else
  begin
    GetTargetBounds(Bounds);

    X := Position.X - Bounds.X1;
    Y := Position.Y - Bounds.Y1;
  end;

  if FMouseClientAreaSet then
  begin
    X := X - FMouseClientArea.X1;
    Y := Y - FMouseClientArea.Y1;
  end;
end;

procedure TWindowTarget.MoveMouse(X, Y: Int32);
var
  Bounds: TBox;
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  if FIsDesktop then
    SetCursorPos(X, Y)
  else
  begin
    GetTargetBounds(Bounds);
    SetCursorPos(Bounds.X1 + X, Bounds.Y1 + Y);
  end;
end;

procedure TWindowTarget.ScrollMouse(X, Y, Lines: Int32);
const
  MOUSEEVENTF_WHEEL = $800;
var
  Input: TInput;
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  Input := Default(TInput);
  Input.type_ := INPUT_MOUSE;
  Input.mi.dx := Bounds.X1 + X;
  Input.mi.dy := Bounds.Y1 + Y;
  Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_WHEEL;
  Input.mi.mouseData := Lines * WHEEL_DELTA;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWindowTarget.HoldMouse(X, Y: Int32; Button: TClickType);
var
  Input: TInput;
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  Input := Default(TInput);
  Input.type_ := INPUT_MOUSE;
  Input.mi.dx := Bounds.X1 + X;
  Input.mi.dy := Bounds.Y1 + Y;

  case Button of
    MOUSE_LEFT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN;
    MOUSE_MIDDLE: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN;
    MOUSE_RIGHT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN;
  end;

  SendInput(1, @Input, SizeOf(Input));
end;

procedure TWindowTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
var
  Input: TInput;
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  Input := Default(TInput);
  Input.type_ := INPUT_MOUSE;
  Input.mi.dx := Bounds.X1 + X;
  Input.mi.dy := Bounds.Y1 + Y;

  case Button of
    MOUSE_LEFT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP;
    MOUSE_MIDDLE: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP;
    MOUSE_RIGHT: Input.mi.dwFlags := MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP;
  end;

  SendInput(1, @Input, SizeOf(Input));
end;

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  case Button of
    MOUSE_LEFT: Result := (GetAsyncKeyState(VK_LBUTTON) and $8000 <> 0);
    MOUSE_MIDDLE: Result := (GetAsyncKeyState(VK_MBUTTON) and $8000 <> 0);
    MOUSE_RIGHT: Result := (GetAsyncKeyState(VK_RBUTTON) and $8000 <> 0);
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
      if ssALT   in Modifiers then Keybd_Event(VK_MENU,    $2A, 0, 0);

      Sleep(KeyModWait div 2);
    end;

    Keybd_Event(Code, ScanCode, 0, 0);

    if (KeyWait <> 0) then
      Sleep(KeyWait);

    Keybd_Event(Code, ScanCode, KEYEVENTF_KEYUP, 0);

    if (Modifiers <> []) then
    begin
      Sleep(KeyModWait div 2);

      if ssALT   in Modifiers then Keybd_Event(VK_MENU,    $2A, KEYEVENTF_KEYUP, 0);
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
  Input.type_ := INPUT_KEYBOARD;
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
  Input.type_ := INPUT_KEYBOARD;
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

