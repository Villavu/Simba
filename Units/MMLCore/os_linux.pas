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

unit os_linux;

interface

uses
  classes, sysutils, lcltype,
  x, xlib, xutil, xlibhelpers,
  mufasatypes, iomanager;

type
  TWindow = class(TWindow_Abstract)
  public
    procedure GetTargetDimensions(out w, h: integer); override;
    procedure GetTargetPosition(out left, top: integer); override;
    function ReturnData(xs, ys, width, height: Integer): TRetData; override;
    procedure FreeReturnData; override;

    function TargetValid: boolean; override;

    function MouseSetClientArea(x1, y1, x2, y2: integer): boolean; override;
    procedure MouseResetClientArea; override;
    function ImageSetClientArea(x1, y1, x2, y2: integer): boolean; override;
    procedure ImageResetClientArea; override;

    procedure ActivateClient; override;
    procedure GetMousePosition(out x,y: integer); override;
    procedure ScrollMouse(x, y: integer; Lines: integer); override;
    procedure MoveMouse(x,y: integer); override;
    procedure HoldMouse(x,y: integer; button: TClickType); override;
    procedure ReleaseMouse(x,y: integer; button: TClickType); override;
    function  IsMouseButtonHeld( button : TClickType) : boolean;override;

    procedure SendString(str: string; keywait, keymodwait: integer); override;
    procedure HoldKey(key: integer); override;
    procedure ReleaseKey(key: integer); override;
    function IsKeyHeld(key: integer): boolean; override;
    function GetKeyCode(C: Char) : integer;override;

    function GetHandle: PtrUInt; override;
  private
    Window: HWND;
    Buffer: PXImage;

    { (Forced) Client Area }
    mx1, my1, mx2, my2: integer;
    ix1, iy1, ix2, iy2: integer;
    mcaset, icaset: Boolean;

    procedure MouseApplyAreaOffset(var x, y: integer);
    procedure ImageApplyAreaOffset(var x, y: integer);
  public
    constructor Create(AWindow: HWND);
    destructor Destroy; override;
  end;

  TIOManager = class(TIOManager_Abstract)
  public
    function GetProcesses: TSysProcArr; override;
    function SetTarget(Handle: HWND): Int32; overload;

    procedure SetTargetEx(Proc: TSysProc); override;
    procedure SetDesktop; override;
  end;

implementation

constructor TWindow.Create(AWindow: HWND);
begin
  inherited Create();

  Self.Window := AWindow;
end;

destructor TWindow.Destroy;
begin
  Self.FreeReturnData();

  inherited Destroy();
end;

function TWindow.GetHandle: PtrUInt;
begin
  Result := Self.Window;
end;

procedure TWindow.GetTargetDimensions(out w, h: integer);
var
  Bounds: TBox;
begin
  if icaset then
  begin
    W := ix2 - ix1;
    H := iy2 - iy1;
  end else
  begin
    Bounds := _XGetWindowBounds(Self.Window);

    W := Bounds.X2 - Bounds.X1;
    H := Bounds.Y2 - Bounds.Y1;
  end;
end;

procedure TWindow.GetTargetPosition(out left, top: integer);
var
  Bounds: TBox;
begin
  Bounds := _XGetWindowBounds(Self.Window);

  Left := Bounds.X1;
  Top := Bounds.Y1;
end;

function TWindow.TargetValid: boolean;
begin
  Result := _XIsWindowVaild(Self.Window);
end;

{ SetClientArea allows you to use a part of the actual client as a virtual
  client. Im other words, all mouse,find functions will be relative to the
  virtual client.

  XXX:
  I realise I can simply add a[x,y]1 to all of the functions rather than check
  for caset (since they're 0 if it's not set), but I figured it would be more
  clear this way.
}
function TWindow.MouseSetClientArea(x1, y1, x2, y2: integer): boolean;
var w, h: integer;
begin
  { TODO: What if the client resizes (shrinks) and our ``area'' is too large? }
  GetTargetDimensions(w, h);
  if ((x2 - x1) > w) or ((y2 - y1) > h) then
    exit(False);
  if (x1 < 0) or (y1 < 0) then
    exit(False);

  mx1 := x1; my1 := y1; mx2 := x2; my2 := y2;
  mcaset := True;
end;

procedure TWindow.MouseResetClientArea;
begin
  mx1 := 0; my1 := 0; mx2 := 0; my2 := 0;
  mcaset := False;
end;

function TWindow.ImageSetClientArea(x1, y1, x2, y2: integer): boolean;
var w, h: integer;
begin
  { TODO: What if the client resizes (shrinks) and our ``area'' is too large? }
  GetTargetDimensions(w, h);
  if ((x2 - x1) > w) or ((y2 - y1) > h) then
    exit(False);
  if (x1 < 0) or (y1 < 0) then
    exit(False);

  ix1 := x1; iy1 := y1; ix2 := x2; iy2 := y2;
  icaset := True;
end;

procedure TWindow.ImageResetClientArea;
begin
  ix1 := 0; iy1 := 0; ix2 := 0; iy2 := 0;
  icaset := False;
end;

procedure TWindow.MouseApplyAreaOffset(var x, y: integer);
begin
  if mcaset then
  begin
    x := x + mx1;
    y := y + my1;
  end;
end;

procedure TWindow.ImageApplyAreaOffset(var x, y: integer);
begin
  if icaset then
  begin
    x := x + ix1;
    y := y + iy1;
  end;
end;

procedure TWindow.ActivateClient;
begin
  _XSetActiveWindow(Self.Window);
end;

function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;
var
  w, h: integer;
begin
  GetTargetDimensions(w,h);
  if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) or (width < 0) or (height < 0) then
    raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);

  ImageApplyAreaOffset(xs, ys);

  Result.Ptr := nil;
  Result.IncPtrWith := 0;

  if (Self.Buffer <> nil) then
    raise Exception.Create('TIOManager.FreeReturnData must be called for each TIOManager.ReturnData call');

  Self.Buffer := _XGetWindowImage(Self.Window, xs, ys, width, height);
  if (Self.Buffer = nil) then
    raise Exception.Create('TIOManager.ReturnData: XGetImage returned nothing!');

  Result.Ptr := PRGB32(buffer^.data);
  Result.IncPtrWith := 0;
  Result.RowLen := width;
end;

procedure TWindow.FreeReturnData;
begin
  if (Self.Buffer <> nil) then
    XDestroyImage(Self.Buffer);

  Self.Buffer := nil;
end;

procedure TWindow.GetMousePosition(out x, y: integer);
begin
  _XGetMousePosition(Self.Window, X, Y);

  X := X - mx1;
  Y := Y - my1;
end;

procedure TWindow.ScrollMouse(x, y: integer; Lines: integer);
var
  Button: TClickType;
  i: Int32;
begin
  MouseApplyAreaOffset(x, y);

  if Lines > 0 then
    Button := mouse_ScrollDown
  else
  if Lines < 0 then
    Button := mouse_ScrollUp;

  for i := 1 to Abs(Lines) do
  begin
    HoldMouse(x, y, Button);
    ReleaseMouse(x, y, Button);
  end;
end;

procedure TWindow.MoveMouse(x,y: integer);
begin
  MouseApplyAreaOffset(x, y);

  _XMoveMouse(Self.Window, X, Y);
end;

procedure TWindow.HoldMouse(x, y: integer; button: TClickType);
var
  Number: Int32;
begin
  case Button of
    mouse_Left:       Number := Button1;
    mouse_Middle:     Number := Button2;
    mouse_Right:      Number := Button3;
    mouse_ScrollDown: Number := Button5;
    mouse_ScrollUp:   Number := Button4;
  end;

  _XHoldMouse(Self.Window, X, Y, Number);
end;

procedure TWindow.ReleaseMouse(x,y: integer; button: TClickType);
var
  Number: Int32;
begin
  case Button of
    mouse_Left:       Number := Button1;
    mouse_Middle:     Number := Button2;
    mouse_Right:      Number := Button3;
    mouse_ScrollDown: Number := Button5;
    mouse_ScrollUp:   Number := Button4;
  end;

  _XReleaseMouse(Self.Window, X, Y, Number);
end;

function TWindow.IsMouseButtonHeld(button: TClickType): boolean;
var
  Mask: Int32;
begin
  case Button of
    mouse_Left:       Mask := Button1Mask;
    mouse_Middle:     Mask := Button2Mask;
    mouse_Right:      Mask := Button3Mask;
    mouse_ScrollDown: Mask := Button4Mask;
    mouse_ScrollUp:   Mask := Button5Mask;
  end;

  Result := _XIsMouseButtonHeld(Mask);
end;

procedure TWindow.SendString(str: string; keywait, keymodwait: integer);
var
  i: Int32;
  Key, Modifer: TKeyCode;
begin
  for i := 1 to Length(Str) do
  begin
    _XGetKeyAndModiferCode(Str[i], Key, Modifer);

    if (Modifer <> 0) then
    begin
      _XKeyDown(Modifer);

      Sleep(KeyModWait div 2);
    end;

    _XKeyDown(Key);

    if (KeyWait > 0) then
      Sleep(KeyWait);

    _XKeyUp(Key);

    if (Modifer <> 0) then
    begin
      _XKeyUp(Modifer);

      Sleep(KeyModWait div 2);
    end;
  end;
end;

procedure TWindow.HoldKey(key: integer);
begin
  _XKeyDown(_XVirtualKeyToKeyCode(Key));
end;

procedure TWindow.ReleaseKey(key: integer);
begin
  _XKeyUp(_XVirtualKeyToKeyCode(Key));
end;

function TWindow.IsKeyHeld(key: integer): boolean;
begin
  Result := _XIsKeyHeld(_XVirtualKeyToKeyCode(Key));
end;

function TWindow.GetKeyCode(C: Char): integer;
var
  Key, Modifier: TKeyCode;
begin
  _XGetKeyAndModiferCode(C, Key, Modifier);

  Result := Key;
end;

procedure TIOManager.SetDesktop;
begin
  SetBothTargets(TWindow.Create(_XGetDesktopWindow()));
end;

function TIOManager.SetTarget(Handle: HWND): Int32;
begin
  Result := SetBothTargets(TWindow.Create(Handle));
end;

function TIOManager.GetProcesses: TSysProcArr;
begin
  raise Exception.Create('GetProcesses: Not implemented on linux');
end;

procedure TIOManager.SetTargetEx(Proc: TSysProc);
begin
  raise Exception.Create('SetTargetEx: Not implemented on linux');
end;

end.
