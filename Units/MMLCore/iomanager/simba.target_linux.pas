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
  simba.xlib, simba.xlib_helpers, simba.target, simba.oswindow, mufasatypes;

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
  public
    procedure GetTargetBounds(out Bounds: TBox);
    procedure GetTargetDimensions(out Width, Height: Int32); override;
    procedure GetTargetPosition(out Left, Top: Int32); override;
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

function TWindowTarget.GetHandle: PtrUInt;
begin
  Result := FWindow;
end;

procedure TWindowTarget.SetHandle(Value: PtrUInt);
begin
  FWindow := Value;
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
  if FImageClientAreaSet then
  begin
    Bounds := FImageClientArea;

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

constructor TWindowTarget.Create(Target: TOSWindow);
begin
  inherited Create();

  Handle := Target;
end;

destructor TWindowTarget.Destroy;
begin
  inherited Destroy();
end;

procedure TWindowTarget.GetTargetDimensions(out Width, Height: Int32);
var
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  Width := Bounds.X2 - Bounds.X1;
  Height := Bounds.Y2 - Bounds.Y1;
end;

procedure TWindowTarget.GetTargetPosition(out Left, Top: Int32);
var
  Bounds: TBox;
begin
  GetTargetBounds(Bounds);

  Left := Bounds.X1;
  Top := Bounds.Y1;
end;

function TWindowTarget.TargetValid: Boolean;
var
  Attributes: TXWindowAttributes;
begin
  Result := XGetWindowAttributes(DefaultDisplay, FWindow, @Attributes) <> 0;
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
  ImageClientAreaOffset(X, Y);

  Bounds := GetClientBounds();

  if Bounds.Contains(X, Y, Width, Height) then
  begin
    Image := XGetImage(DefaultDisplay, FWindow, X, Y, Width, Height, AllPlanes, ZPixmap);

    if (Image <> nil) then
    begin
      try
        Result := GetMem(Width * Height * SizeOf(TRGB32));

        Move(Image^.Data^, Result^, Width * Height * SizeOf(TRGB32));
      finally
        XDestroyImage(Image);
      end;
    end else
      Result := nil;
  end else
    Result := nil;
end;

function TWindowTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
var
  Bounds: TBox;
begin
  if (FImage <> nil) then
    raise Exception.Create('FreeReturnData has not been called');

  ImageClientAreaOffset(X, Y);

  Bounds := GetClientBounds();

  if Bounds.Contains(X, Y, Width, Height) then
  begin
    FImage := XGetImage(DefaultDisplay, FWindow, X, Y, Width, Height, AllPlanes, ZPixmap);

    if (FImage <> nil) then
    begin
      Result.Ptr := PRGB32(FImage^.Data);
      Result.IncPtrWith := 0;
      Result.RowLen := Width;
    end else
      Result := NullReturnData;
  end else
    Result := NullReturnData;
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
  XQueryPointer(DefaultDisplay, FWindow,
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
    Button := mouse_ScrollDown
  else
    Button := mouse_ScrollUp;

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

  XWarpPointer(DefaultDisplay, None, FWindow, 0, 0, 0, 0, X, Y);
  XFlush(DefaultDisplay);
end;

procedure TWindowTarget.HoldMouse(X, Y: Int32; Button: TClickType);
var
  Event: TXButtonEvent;
begin
  if FAutoFocus then
    ActivateClient();

  Event := Default(TXButtonEvent);
  Event.SubWindow := FWindow;

  case Button of
    mouse_Left:       Event.Button := Button1;
    mouse_Middle:     Event.Button := Button2;
    mouse_Right:      Event.Button := Button3;
    mouse_ScrollDown: Event.Button := Button5;
    mouse_ScrollUp:   Event.Button := Button4;
  end;

  while (Event.SubWindow <> None) do
  begin
    Event.Window := Event.SubWindow;

    XQueryPointer(DefaultDisplay, Event.Window,
                  @Event.Root, @Event.SubWindow,
                  @Event.X_Root, @Event.Y_Root,
                  @Event.X, @Event.Y,
                  @Event.State);
  end;

  Event._Type := ButtonPress;

  XSendEvent(DefaultDisplay, PointerWindow, True, ButtonPressMask, @Event);
  XFlush(DefaultDisplay);
end;

procedure TWindowTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
var
  Event: TXButtonEvent;
begin
  if FAutoFocus then
    ActivateClient();

  Event := Default(TXButtonEvent);
  Event.SubWindow := FWindow;

  case Button of
    mouse_Left:       Event.Button := Button1;
    mouse_Middle:     Event.Button := Button2;
    mouse_Right:      Event.Button := Button3;
    mouse_ScrollDown: Event.Button := Button5;
    mouse_ScrollUp:   Event.Button := Button4;
  end;

  while (Event.SubWindow <> None) do
  begin
    Event.Window := Event.SubWindow;

    XQueryPointer(DefaultDisplay, Event.Window,
                  @Event.Root, @Event.SubWindow,
                  @Event.X_Root, @Event.Y_Root,
                  @Event.X, @Event.Y,
                  @Event.State);
  end;

  Event._Type := ButtonRelease;

  XSendEvent(DefaultDisplay, PointerWindow, True, ButtonPressMask, @Event);
  XFlush(DefaultDisplay);
end;

function TWindowTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
var
  Mask: Int32;
  Event: TXButtonEvent;
begin
  XSync(DefaultDisplay, 0);

  case Button of
    mouse_Left:       Mask := Button1Mask;
    mouse_Middle:     Mask := Button2Mask;
    mouse_Right:      Mask := Button3Mask;
    mouse_ScrollDown: Mask := Button4Mask;
    mouse_ScrollUp:   Mask := Button5Mask;
  end;

  Event := Default(TXButtonEvent);

  XQueryPointer(DefaultDisplay, Event.Window,
                @Event.Root, @Event.Window,
                @Event.X_Root, @Event.Y_Root,
                @Event.X, @Event.Y,
                @Event.State);

  Result := ((Event.State and Mask) > 0);
end;

procedure TWindowTarget.SendString(Text: String; KeyWait, KeyModWait: Int32);
var
  i: Int32;
  Key, Modifier: TKeyCode;
begin
  if FAutoFocus then
    ActivateClient();

  for i := 1 to Length(Text) do
  begin
    XGetKeyCode(DefaultDisplay, Text[i], Key, Modifier);

    if (Modifier <> 0) then
    begin
      XTestFakeKeyEvent(DefaultDisplay, Modifier, True, 0);
      XSync(DefaultDisplay, 0);
      if KeyModWait > 0 then
        Sleep(KeyModWait div 2);
    end;

    XTestFakeKeyEvent(DefaultDisplay, Key, True, 0);
    XSync(DefaultDisplay, 0);

    Sleep(KeyWait);

    XTestFakeKeyEvent(DefaultDisplay, Key, False, 0);
    XSync(DefaultDisplay, 0);

    if (Modifier <> 0) then
    begin
      XTestFakeKeyEvent(DefaultDisplay, Modifier, False, 0);
      XSync(DefaultDisplay, 0);
      if KeyModWait > 0 then
        Sleep(KeyModWait div 2);
    end;
  end;
end;

procedure TWindowTarget.HoldKey(Key: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  XTestFakeKeyEvent(DefaultDisplay, XGetKeyCode(DefaultDisplay, Key), True, 0);
  XFlush(DefaultDisplay);
end;

procedure TWindowTarget.ReleaseKey(Key: Int32);
begin
  if FAutoFocus then
    ActivateClient();

  XTestFakeKeyEvent(DefaultDisplay, XGetKeyCode(DefaultDisplay, Key), False, 0);
  XFlush(DefaultDisplay);
end;

function TWindowTarget.IsKeyHeld(Key: Int32): Boolean;
var
  Code: TKeySym;
  Keys: CharArr32;
begin
  Code := XGetKeyCode(DefaultDisplay, Key);

  XSync(DefaultDisplay, 0);
  XQueryKeymap(DefaultDisplay, CharArr32(Keys));

  Result := (Keys[Code shr 3] shr (Code and $07)) and $01 > 0;
end;

function TWindowTarget.GetKeyCode(Character: Char): Int32;
var
  KeyCode, Modifier: TKeyCode;
begin
  XGetKeyCode(DefaultDisplay, Character, KeyCode, Modifier);

  Result := KeyCode;
end;

end.
