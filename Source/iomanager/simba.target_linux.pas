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

      Linux OS specific implementation for Mufasa Macro Library
}
{$mode objfpc}{$H+}

unit simba.target_linux;

interface

uses
  classes, sysutils,
  simba.xlib, simba.xlib_helpers, simba.xlib_display, simba.target, simba.oswindow, simba.mufasatypes;

type
  TWindowTarget = class(TTarget)
  protected
    FWindow: TOSWindow;
    FAutoFocus: Boolean;
    FImage: PXImage;

    function GetHandle: PtrUInt; override;
    procedure SetHandle(Value: PtrUInt); override;

    function GetAutoFocus: Boolean; override;
    procedure SetAutoFocus(Value: Boolean); override;

    procedure GetTargetBounds(out Bounds: TBox); override;
  public
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    procedure FreeReturnData; override;

    function TargetValid: Boolean; override;

    procedure ActivateClient; override;
    procedure GetMousePosition(out X, Y: Int32); override;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); override;
    procedure MoveMouse(X, Y: Int32); override;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override;
    function IsMouseButtonHeld(Button: TClickType): Boolean;override;

    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); override;
    procedure HoldKey(Key: Int32); override;
    procedure ReleaseKey(Key: Int32); override;
    function IsKeyHeld(Key: Int32): Boolean; override;
    function GetKeyCode(Character: Char): Int32; override;

    constructor Create(Target: TOSWindow);
    destructor Destroy; override;
  end;

implementation

uses
  x;

const
  Button8 = 8;
  Button9 = 9;

const
  Button8Mask = 1 << 15;
  Button9Mask = 1 << 16;

function TWindowTarget.GetHandle: PtrUInt;
begin
  Result := FWindow;
end;

procedure TWindowTarget.SetHandle(Value: PtrUInt);
begin
  FWindow := Value;
  if (FWindow = 0) then
    FWindow := GetDesktopWindow();
end;

function TWindowTarget.GetAutoFocus: Boolean;
begin
  Result := FAutoFocus;
end;

procedure TWindowTarget.SetAutoFocus(Value: Boolean);
begin
  FAutoFocus := Value;
end;

procedure TWindowTarget.GetTargetBounds(out Bounds: TBox);
var
  Attempts: Int32;
begin
  for Attempts := 1 to 5 do
  begin
    if FWindow.GetBounds(Bounds) then
      Exit;

    Self.InvalidTarget();
  end;

  raise Exception.CreateFmt('Invalid window handle: %d', [FWindow]);
end;

constructor TWindowTarget.Create(Target: TOSWindow);
begin
  inherited Create();

  Handle := Target;
end;

destructor TWindowTarget.Destroy;
begin
  inherited Destroy();
end;

function TWindowTarget.TargetValid: Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := XGetWindowAttributes(GetDisplay(), FWindow, @Attributes) <> 0;
end;

procedure TWindowTarget.ActivateClient;
begin
  FWindow.Activate();
end;

function TWindowTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
var
  Bounds: TBox;
  Image: PXImage;
begin
  Result := nil;

  if FAutoFocus then
    ActivateClient();

  GetTargetBounds(Bounds);
  
  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    Image := XGetImage(GetDisplay(), FWindow, X, Y, Width, Height, AllPlanes, ZPixmap);

    if (Image <> nil) then
    begin
      try
        Result := GetMem(Width * Height * SizeOf(TRGB32));

        Move(Image^.Data^, Result^, Width * Height * SizeOf(TRGB32));
      finally
        XDestroyImage(Image);
      end;
    end;
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

  ImageClientAreaOffset(X, Y);

  if Bounds.Contains(Bounds.X1 + X, Bounds.Y1 + Y, Width, Height) then
  begin
    if (FImage <> nil) then
      raise Exception.Create('FreeReturnData has not been called');

    FImage := XGetImage(GetDisplay(), FWindow, X, Y, Width, Height, AllPlanes, ZPixmap);

    if (FImage <> nil) then
    begin
      Result.Ptr := PRGB32(FImage^.Data);
      Result.IncPtrWith := 0;
      Result.RowLen := Width;
    end;
  end;
end;

procedure TWindowTarget.FreeReturnData;
begin
  if (FImage <> nil) then
    XDestroyImage(FImage);

  FImage := nil;
end;

procedure TWindowTarget.GetMousePosition(out X, Y: Int32);
var
  Event: TXButtonEvent;
begin
  XQueryPointer(GetDisplay(), FWindow,
                @Event.Root, @Event.Window,
                @Event.X_Root, @Event.Y_Root,
                @X, @Y,
                @Event.State);

  MouseClientAreaOffset(X, Y);
end;

procedure TWindowTarget.ScrollMouse(X, Y: Int32; Lines: Int32);
var
  Button: TClickType;
  i: Int32;
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  if Lines > 0 then
    Button := MOUSE_SCROLL_DOWN
  else
    Button := MOUSE_SCROLL_UP;

  for i := 1 to Abs(Lines) do
  begin
    HoldMouse(X, Y, Button);
    ReleaseMouse(X, Y, Button);
  end;
end;

procedure TWindowTarget.MoveMouse(X, Y: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  MouseClientAreaOffset(X, Y);

  XWarpPointer(GetDisplay(), None, FWindow, 0, 0, 0, 0, X, Y);
  XFlush(GetDisplay());
end;

procedure TWindowTarget.HoldMouse(X, Y: Int32; Button: TClickType);
var
  Number: Int32;
begin
  if FAutoFocus then
    ActivateClient();

  case Button of
    MOUSE_LEFT:        Number := Button1;
    MOUSE_MIDDLE:      Number := Button2;
    MOUSE_RIGHT:       Number := Button3;
    MOUSE_SCROLL_DOWN: Number := Button5;
    MOUSE_SCROLL_UP:   Number := Button4;
    MOUSE_EXTRA_1:     Number := Button8;
    MOUSE_EXTRA_2:     Number := Button9;
  end;

  XTestFakeButtonEvent(GetDisplay(), Number, TBool(True), CurrentTime);
  XFlush(GetDisplay());
end;

procedure TWindowTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
var
  Number: Int32;
begin
  if FAutoFocus then
    ActivateClient();

  case Button of
    MOUSE_LEFT:        Number := Button1;
    MOUSE_MIDDLE:      Number := Button2;
    MOUSE_RIGHT:       Number := Button3;
    MOUSE_SCROLL_DOWN: Number := Button5;
    MOUSE_SCROLL_UP:   Number := Button4;
    MOUSE_EXTRA_1:     Number := Button8;
    MOUSE_EXTRA_2:     Number := Button9;
  end;

  XTestFakeButtonEvent(GetDisplay(), Number, TBool(False), CurrentTime);
  XFlush(GetDisplay());
end;

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
var
  Mask: Int32;
  Event: TXButtonEvent;
begin
  XSync(GetDisplay(), 0);

  case Button of
    MOUSE_LEFT:        begin Event.Button := Button1; Mask := Button1Mask; end;
    MOUSE_MIDDLE:      begin Event.Button := Button2; Mask := Button2Mask; end;
    MOUSE_RIGHT:       begin Event.Button := Button3; Mask := Button3Mask; end;
    MOUSE_SCROLL_UP:   begin Event.Button := Button4; Mask := Button4Mask; end;
    MOUSE_SCROLL_DOWN: begin Event.Button := Button5; Mask := Button5Mask; end;
    MOUSE_EXTRA_1:     raise Exception.Create('Xlib doesn''t support this?'); //begin Event.Button := Button8; Mask := Button8Mask; end;
    MOUSE_EXTRA_2:     raise Exception.Create('Xlib doesn''t support this?'); //begin Event.Button := Button9; Mask := Button9Mask; end;
  end;

  Event := Default(TXButtonEvent);

  XQueryPointer(GetDisplay(), FWindow,
                @Event.Root, @Event.Window,
                @Event.X_Root, @Event.Y_Root,
                @Event.X, @Event.Y,
                @Event.State);

  Result := ((Event.State and Mask) > 0);
end;

procedure TWindowTarget.SendString(Text: String; KeyWait, KeyModWait: Int32);

  procedure KeyEvent(KeyCode: UInt32; Press: Boolean);
  begin
    XTestFakeKeyEvent(GetDisplay(), KeyCode, TBool(Press), 0);
    XSync(GetDisplay(), 0);
  end;

var
  i: Int32;
  Key, Modifier: TKeyCode;
begin
  if FAutoFocus then
    ActivateClient();

  for i := 1 to Length(Text) do
  begin
    XGetKeyCode(GetDisplay(), Text[i], Key, Modifier);

    if (Modifier <> 0) then
    begin
      KeyEvent(Modifier, True);
      if KeyModWait > 0 then
        Sleep(KeyModWait div 2);
    end;

    KeyEvent(Key, True);
    Sleep(KeyWait);
    KeyEvent(Key, False);

    if (Modifier <> 0) then
    begin
      KeyEvent(Modifier, False);
      if KeyModWait > 0 then
        Sleep(KeyModWait div 2);
    end;
  end;
end;

procedure TWindowTarget.HoldKey(Key: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  XTestFakeKeyEvent(GetDisplay(), XGetKeyCode(GetDisplay(), Key), TBool(True), 0);
  XSync(GetDisplay(), 0);
end;

procedure TWindowTarget.ReleaseKey(Key: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  XTestFakeKeyEvent(GetDisplay(), XGetKeyCode(GetDisplay(), Key), TBool(False), 0);
  XSync(GetDisplay(), 0);
end;

function TWindowTarget.IsKeyHeld(Key: Int32): Boolean;
var
  Code: TKeySym;
  Keys: CharArr32;
begin
  Code := XGetKeyCode(GetDisplay(), Key);

  XSync(GetDisplay(), 0);
  XQueryKeymap(GetDisplay(), CharArr32(Keys));

  Result := (Keys[Code shr 3] shr (Code and $07)) and $01 > 0;
end;

function TWindowTarget.GetKeyCode(Character: Char): Int32;
var
  KeyCode, Modifier: TKeyCode;
begin
  XGetKeyCode(GetDisplay(), Character, KeyCode, Modifier);

  Result := KeyCode;
end;

end.
